#' Get climate future scenarios for the Contiguous United States using the
#' Multivariate Adaptive Constructed Analogs (MACA) technique.
#'
#' This function will take an area of interest (with extent coordinates or as a shapefile),
#' and return a spatial of subset of retrieves NetCDFs files and
#'
#' ...
#'
#' This function reticulates Python's Xarray and netCDF4 packages to do the job.
#' It might not be pure R code, but it is much more stable and faster than anything
#' I could manage in R.
#'
#' This strategy may also enable us to use Zarr arrays, which are accessible directly
#' from an s3 bucket, according to Will.
#'
#' We are using this catalog:
#' http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_CMIP5_aggregated_macav2_catalog.html
#'
#' There is also the non-aggregated catalog:
#' http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_CMIP5_macav2_catalog2.html
#' These files come in 5 year chunks but have windspeed (was) and potential evapotranspiration (PotEvap)
#' in addition to the other variables. This could be done with url list files from here:
#' https://climate.northwestknowledge.net/MACA/data_portal.php
#'
#' We also have mean vapor pressure deficit (vpd) in both data sets.

library(aws.s3)
library(glue)
library(progress)
library(reticulate)  # <-------------------------------------------------------------------- Read: https://rstudio.github.io/reticulate/articles/package.html
reticulate::use_condaenv("dict")
xr <- reticulate::import("xarray")

get_cstdata <- function(parkname="Yellowstone National Park"){
  # Park Shapefile and Area of Interest
  parks <- rgdal::readOGR("data/shapefiles/nps_boundary.shp")
  aoi <- parks[grepl(parkname, parks$UNIT_NAME),]

  # First we need a destination folder
  location <- gsub(" ", "_", tolower(aoi$UNIT_NAME))
  print(paste("Retrieving climate data for", location))
  dst_folder <- file.path('data', 'netcdfs', location)
  if (!dir.exists(dst_folder)) dir.create(dst_folder, recursive = TRUE)

  # Reference Objects
  grid = Grid_Reference()
  arg_ref = Argument_Reference()

  # Get the extent coordinates
  lonmin <- aoi@bbox[1,1]
  lonmax <- aoi@bbox[1,2]
  latmin <- aoi@bbox[2,1]
  latmax <- aoi@bbox[2,2]

  # Now use these from the lat/lons of the full grid to get index positions
  lonmindiffs <- abs(grid$lons - lonmin)
  lonmaxdiffs <- abs(grid$lons - lonmax)
  latmindiffs <- abs(grid$lats - latmin)
  latmaxdiffs <- abs(grid$lats - latmax)
  lon1 <- match(lonmindiffs[lonmindiffs == min(lonmindiffs)], lonmindiffs)
  lon2 <- match(lonmaxdiffs[lonmaxdiffs == min(lonmaxdiffs)], lonmaxdiffs)
  lat1 <- match(latmindiffs[latmindiffs == min(latmindiffs)], latmindiffs)
  lat2 <- match(latmaxdiffs[latmaxdiffs == min(latmaxdiffs)], latmaxdiffs)

  # Get some time information
  ntime_hist <- grid$ntime_hist
  ntime_model <- grid$ntime_model
  historical_years <- "1950_2005"
  model_years <- "2006_2099"
  # h1 <- strsplit(historical_years, "_")[[1]][1]
  # h2 <- strsplit(historical_years, "_")[[1]][2]
  # m1 <- strsplit(model_years, "_")[[1]][1]
  # m2 <- strsplit(model_years, "_")[[1]][2]
  # hd1 <- as.Date(paste(c(h1, "01", "01"), collapse = "-"))
  # hd2 <- as.Date(paste(c(h2, "12", "31"), collapse = "-"))
  # md1 <- as.Date(paste(c(m1, "01", "01"), collapse = "-"))
  # md2 <- as.Date(paste(c(m2, "12", "31"), collapse = "-"))
  # historical_dates <- seq(hd1, hd2, "days")
  # model_dates <- seq(md1, md2, "days")
  # ntime_hist <- length(historical_dates)

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
        hquery <- paste0(var, glue("[{0}:{1}:{ntime_hist}][{lat1}:{1}:{lat2}][{lon1}:{1}:{lon2}]"))
        mquery <- paste0(var, glue("[{0}:{1}:{ntime_model}][{lat1}:{1}:{lat2}][{lon1}:{1}:{lon2}]"))
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
  # # Finish getURL to accept this

  # Let's leave parallelization aside for now, merge and download each pair
  pb <- progress_bar$new(total = length(purls))
  pb$tick(0)
  for (purl in purls){
    # Get the local destination file
    content <- purl[2]  # 2 for the modeled set, it has more information
    query <- basename(content)
    cutoff <- regexpr("CONUS", query)[1] - 12  # to get to the scenario
    file_name <- paste0(substr(query, 1, cutoff), ".nc")
    dst <-file.path(dst_folder, file_name)
    pb$tick()

    if (!file.exists(dst)) {
      # Save a local file - (replace with getURL for more options)
      ds <- xr$open_mfdataset(purl, concat_dim="time")
      ds$to_netcdf(dst)

      # Send to s3 bucket
      creds <- readRDS("~/.aws/credentials.RDS")
      Sys.setenv("AWS_ACCESS_KEY_ID" = creds['key'],
                 "AWS_SECRET_ACCESS_KEY" = creds['skey'],
                 "AWS_DEFAULT_REGION" = creds['region'])

      # Put the file in the bucket
      bucket_name <- "cstdata-test"
      object <- file.path(location, file_name)
      put_folder(location, bucket_name)
      put_object(file = dst, object = object, bucket = bucket_name)
    }
  }
}



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
    initialize = function(models = cst_models, parameters = cst_params, scenarios = cst_scenarios,
                          variables = cst_variables, units = cst_units) {
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
      print("Got Query")
    }
  )
)

