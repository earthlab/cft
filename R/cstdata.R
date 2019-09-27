#' Get climate future scenarios for the Contiguous United States using the
#' Multivariate Adaptive Constructed Analogs (MACA) technique.
#'
#' This package retrieves daily gridded data sets of General Climate Models (GCM) clipped to
#' specified National Parks. Each of these data sets represent a single GCM, climate variable,
#' and Representative Concentration Pathway (RCP) from 1950 to 2099. The 1950 to 2005 portion
#' of this time period represent historical data while the 2006 to 2099 portion represents
#' modeled data. These can be stored as NetCDF files either locally or on an Amazon Web Service
#' S3 bucket. The original data sets may be found at
#' \url{ http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_CMIP5_aggregated_macav2_catalog.html}
#'
#'
#' Production Notes:
#' The use of reticulate may enable us to use Zarr arrays, which are accessible directly
#' from an s3 bucket.
#'
#' There is also a non-aggregated catalog:
#' \url{http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_CMIP5_macav2_catalog2.html}
#' These files come in 5 year chunks but have daily windspeed (was) and monthly potential
#' evapotranspiration (PotEvap) in addition to the other variables. Consider this data portal
#' for a full list of url queries: \url{https://climate.northwestknowledge.net/MACA/data_portal.php}

library(aws.s3)
library(glue)
library(progress)
library(raster)
library(reticulate)  # <-------------------------------------------------------------------- Read: https://rstudio.github.io/reticulate/articles/package.html
reticulate::use_condaenv("dict")
xr <- reticulate::import("xarray")

cstdata <- function(parkname="Yellowstone National Park"){
  # Initialize AWS access
  # creds <- readRDS("~/.aws/credentials.RDS")
  # Sys.setenv("AWS_ACCESS_KEY_ID" = creds['key'],
  #            "AWS_SECRET_ACCESS_KEY" = creds['skey'],
  #            "AWS_DEFAULT_REGION" = creds['region'])

  # Make sure we have the national park shapefile
  if (!file.exists("data/shapefiles/nps_boundary.shp")){
    get_park_boundaries()
  }

  # Get the boundaries of the chosen national park
  parks <- rgdal::readOGR("data/shapefiles/nps_boundary.shp")
  aoi <- parks[grepl(parkname, parks$UNIT_NAME),]

  # Make sure we a park-specific destination folder
  location <- gsub(" ", "_", tolower(aoi$UNIT_NAME))
  print(paste("Retrieving climate data for", location))
  dst_folder <- file.path('data', 'netcdfs', location)
  if (!dir.exists(dst_folder)) dir.create(dst_folder, recursive = TRUE)

  # Instatiate reference objects
  grid = Grid_Reference()
  arg_ref = Argument_Reference()

  # Get the extent coordinates
  aoi <- sp::spTransform(aoi, grid$crs)
  lonmin <- aoi@bbox[1,1]
  lonmax <- aoi@bbox[1,2]
  latmin <- aoi@bbox[2,1]
  latmax <- aoi@bbox[2,2]

  # Now use these from the lat/lons of the full grid to get index positions
  lonmindiffs <- abs(grid$lons - lonmin)
  lonmaxdiffs <- abs(grid$lons - lonmax)
  latmindiffs <- abs(grid$lats - latmin)
  latmaxdiffs <- abs(grid$lats - latmax)
  x1 <- match(lonmindiffs[lonmindiffs == min(lonmindiffs)], lonmindiffs)
  x2 <- match(lonmaxdiffs[lonmaxdiffs == min(lonmaxdiffs)], lonmaxdiffs)
  y1 <- match(latmindiffs[latmindiffs == min(latmindiffs)], latmindiffs)
  y2 <- match(latmaxdiffs[latmaxdiffs == min(latmaxdiffs)], latmaxdiffs)

  # Now rasterize shape to get coordinates within boundaries (check output, move to function)
  ny <- (y2 - y1) + 1
  nx <- (x2 - x1) + 1
  # aoilats <- sapply(0:(ny - 1), function(x) latmin + x*grid$resolution)
  # aoilons <- sapply(0:(nx - 1), function(x) lonmin + x*grid$resolution)
  r <- raster::raster(ncol=nx, nrow=ny)
  raster::extent(r) <- raster::extent(aoi)
  r <- rasterize(aoi, r, 'UNIT_CODE')  # Not generalizable
  mask <- r * 0 + 1

  # Get some time information
  ntime_hist <- grid$ntime_hist
  ntime_model <- grid$ntime_model
  historical_years <- "1950_2005"
  model_years <- "2006_2099"

  # Now we can get the historical and model years together
  urlbase = "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_macav2metdata"
  hurls <- c()
  murls <- c()
  for (model in arg_ref$models){
    args = arg_ref$get_args(model)
    variables <- arg_ref$variables
    params <- args$parameters
    scenarios <- args$scenarios
    ensemble <- args$ensemble
    for (param in params){
      for (scenario in scenarios){
        hattachment <- paste(c(urlbase, param, model, ensemble, "historical",
                               historical_years, "CONUS_daily.nc?"), collapse = "_")
        mattachment <- paste(c(urlbase, param, model, ensemble, scenario,
                               model_years, "CONUS_daily.nc?"), collapse = "_")
        var <- variables[param]
        hquery <- paste0(var, glue::glue("[{0}:{1}:{ntime_hist}][{y1}:{1}:{y2}][{x1}:{1}:{x2}]"))
        mquery <- paste0(var, glue::glue("[{0}:{1}:{ntime_model}][{y1}:{1}:{y2}][{x1}:{1}:{x2}]"))
        hurl <- paste0(hattachment, hquery)
        murl <- paste0(mattachment, mquery)
        hurls <- append(hurls, hurl)
        murls <- append(murls, murl)
      }
    }
  }

  # Let's have the historical and modeled urls in pairs
  purls <- lapply(1:length(hurls), function(i) c(hurls[i], murls[i]))

  # # If we want to parallelize, we could group the pairs further
  # `%dopar%` <- foreach::`%dopar%`
  # ncores <- parallel::detectCores() - 1
  # doParallel::registerDoParallel(ncores)
  # purls2 <- split(purls, ceiling(seq_along(purls)/ncores))
  # Now the urls are in groups of 7 (on my computer) that may be dl'ed at once

  # Let's leave parallelization aside for now, merge and download each pair
  pb <- progress_bar$new(total = length(purls))
  pb$tick(0)
  for (purl in purls){
    # Get the local destination file
    content <- purl[2]  # 2 for the modeled set, it has more information
    query <- basename(content)
    cutoff <- regexpr("CONUS", query)[1] - 12  # to get to the scenario
    file_name <- paste0(substr(query, 1, cutoff), ".nc") # <--------------------------------- Incorporate the naming convention Imtiaz requested
    dst <-file.path(dst_folder, file_name)

    if (!file.exists(dst)) {
      # Save a local file
      ds <- xr$open_mfdataset(purl, concat_dim="time")

      # add coordinates
      # ds <- ds$assign_coords(lat = c('lat' = as.matrix(aoilats)),
      #                        lon = c('lon' = as.matrix(aoilons)))

      # Mask by boundary ...
      dsmask <- xr$DataArray(as.matrix(mask))
      dsmask <- dsmask$fillna(0)
      ds <- ds$where(dsmask$data == 1)

      # Test if anything was masked
      # ds$air_temperature[[1]]$values

      # Add location attribute (The park name) ...
      # Add time attribute (Daily, 1950 - 2099, historical until 2006, modeled after) ...

      # Save to local file
      ds$to_netcdf(dst)

      # Put the file in the bucket
      # bucket_name <- "cstdata-test"
      # object <- file.path(location, file_name)
      # put_folder(location, bucket_name)
      # put_object(file = dst, object = object, bucket = bucket_name)
    }
    pb$tick()
  }
}


get_park_boundaries <- function(){
  # create directory if not present
  if (!file.exists('data/shapefiles')) {
    dir.create(file.path('data', 'shapefiles'), recursive = TRUE, showWarnings = FALSE)
  }

  # Download if not already preset
  nps_boundary <- "data/shapefiles/nps_boundary.shp"
  file <- "data/shapefiles/nps_boundary.zip"
  if(!file.exists(nps_boundary)){
    url <- "https://irma.nps.gov/DataStore/DownloadFile/627620"
    download.file(url = url, destfile = file, method = "curl")  # Check this, not stable yet
    unzip(file, exdir = "data/shapefiles")
  }
}


# Reference Classes
Grid_Reference <- setRefClass(
  "reference_grid",

  fields = list(
    crs = "character",
    extent = "list",
    resolution = "numeric",
    lats = "numeric",
    lons = "numeric",
    ntime_hist = "numeric",
    ntime_model = "numeric"
  ),

  methods = list(
    initialize = function(crs = "+proj=longlat +a=6378137 +f=0.00335281066474748 +pm=0 +no_defs",
                          extent = list("latmin" = 25.0631, "latmax" = 49.3960,
                                        "lonmin" = -124.7722, "lonmax" = -67.0648),
                          resolution = 0.04166575,
                          nlat = 585,
                          nlon = 1386,
                          ntime_hist = 20453,
                          ntime_model = 34332){
      crs <<- crs
      resolution <<- resolution
      extent <<- extent
      lats <<- sapply(0:(nlat - 1), function(x) extent["latmin"][[1]] + x*resolution)
      lons <<- sapply(0:(nlon - 1), function(x) extent["lonmin"][[1]] + x*resolution)
      ntime_hist <<- ntime_hist
      ntime_model <<- ntime_model
    }
  )
)


Argument_Reference <- setRefClass(
  "maca_options",

  fields = list(
    models = "character",
    parameters = "character",
    scenarios = "character",
    variables = "list",
    units = "list"),

  methods = list(
    initialize = function(
      models = c("bcc-csm1-1", "bcc-csm1-1-m", "BNU-ESM", "CanESM2", "CCSM4", "CNRM-CM5",
                 "CSIRO-Mk3-6-0", "GFDL-ESM2M", "GFDL-ESM2G", "HadGEM2-ES365",
                 "HadGEM2-CC365", "inmcm4", "IPSL-CM5A-LR", "IPSL-CM5A-MR", "IPSL-CM5B-LR",
                 "MIROC5", "MIROC-ESM", "MIROC-ESM-CHEM", "MRI-CGCM3", "NorESM1-M"),
      parameters = c("tasmin", "tasmax", "rhsmin", "rhsmax", "pr", "rsds", "uas", "vas",
                     "huss", "vpd"),
      scenarios = c("rcp45", "rcp85"),
      variables = list("tasmin" = "air_temperature",
                      "tasmax" = "air_temperature",
                      "rhsmin" = "relative_humidity",
                      "rhsmax" = "relative_humidity",
                      "pr" = "precipitation",
                      "rsds" = "surface_downwelling_shortwave_flux_in_air",
                      "uas" = "eastward_wind",
                      "vas" = "northward_wind",
                      "huss" = "specific_humidity",
                      "vpd" = "vpd"),
      units = list("air_temperature" = "K",
                   "relative_humidity" = "%",
                   "precipitation" = "mm",
                   "surface_downwelling_shortwave_flux_in_air" = "W m-2",
                   "eastward_wind" = "m s-1",
                   "northward_wind" = "m s-1",
                   "specific_humidity" = "kg kg-1",
                   "vpd" = "kPa")) {
      models <<- models
      parameters <<- parameters
      scenarios <<- scenarios
      variables <<- variables
      units <<- units
    },

    get_args = function(model){
      args <- list()
      for (m in models){
        args[[m]] <- list("parameters" = parameters, "scenarios" = scenarios, "ensemble" =  "r1i1p1")
      }

      # CCSM4 does not have relative humidity and uses ensemble "r6i1p1" (so far the only differences observed)
      args[["CCSM4"]]$parameters = c("tasmin", "tasmax", "pr", "rsds", "uas", "vas", "huss", "vpd")
      args[["CCSM4"]]$ensemble = "r6i1p1"

      return(args[[model]])
    },

    get_query = function(model, aoi) {
      print("Maybe we could build the url queries here?")
    }
  )
)
