#' Get climate future scenarios for the Contiguous United States using the
#' Multivariate Adaptive Constructed Analogs (MACA) technique.
#'
#' This package retrieves daily gridded data sets of General Climate Models
#' (GCM) clipped to specified National Parks. Each of these data sets represent
#' a single GCM, climate variable and Representative Concentration Pathway (RCP)
#' from 1950 to 2099. The 1950 to 2005 portion of this time period represents
#' historical data while the 2006 to 2099 portion represents modeled data. These
#' can be stored as NetCDF files either locally or on an Amazon Web Service S3
#' bucket. The original data sets may be found at
#' \url{ http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_
#' CMIP5_aggregated_macav2_catalog.html}
#'
#' Production Notes:
#' - The use of reticulate may enable us to use Zarr arrays, which are accessible
#'   directly from an s3 bucket.
#'
#' - There is also a non-aggregated catalog:
#'   \url{http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_CMIP5_
#'   macav2_catalog2.html}
#'   These files come in 5 year chunks but have daily windspeed (was) and monthly
#'   potential evapotranspiration (PotEvap) in addition to the other variables.
#'   Imtiaz would like the monthl PotEvap.
#' 
#' - The Vapor Pressure Deficit products in the aggregated catalog has an issue
#'   with fill values. I've seen this before and it is easily fixed by appending
#'   "#fillmistmatch" to the end of the query, which I will do when I get the
#'   computer back from a test run.
#
#' - This currently saves everything to disk first, which can be a problem. The
#'   alternative is to save each file to a tempfile and overwrite. We'll have to
#'   be careful when parallelizing. Now, Imtiaz has expressed interest in the
#'   ability to save locally. For small parks this is fine, so perhaps an option.
#
#' - For Death Valley using 7 cores with 15.6 GB of RAM and 2GB of SWAP, it
#'   crashed about 275 files in. 
#'   
#' @param parkname The name of the national park for which to download data 
#' (character), e.g., "Acadia National Park".
#' @importFrom methods new

cstdata <- function(parkname="Acadia National Park"){
  # Initialize AWS access
  creds <- readRDS("~/.aws/credentials.RDS")
  Sys.setenv("AWS_ACCESS_KEY_ID" = creds['key'],
             "AWS_SECRET_ACCESS_KEY" = creds['skey'],
             "AWS_DEFAULT_REGION" = creds['region'])

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

  # Generate reference objects
  grid = Grid_Reference()
  arg_ref = Argument_Reference()

  # Get relative index positions to full grid  # <------------------------------ This can be wrapped in a function, but I currently some of the parts generated here.
  aoi <- sp::spTransform(aoi, grid$crs)
  lonmin <- aoi@bbox[1,1]
  lonmax <- aoi@bbox[1,2]
  latmin <- aoi@bbox[2,1]
  latmax <- aoi@bbox[2,2]
  lonmindiffs <- abs(grid$lons - lonmin)
  lonmaxdiffs <- abs(grid$lons - lonmax)
  latmindiffs <- abs(grid$lats - latmin)
  latmaxdiffs <- abs(grid$lats - latmax)
  x1 <- match(lonmindiffs[lonmindiffs == min(lonmindiffs)], lonmindiffs)
  x2 <- match(lonmaxdiffs[lonmaxdiffs == min(lonmaxdiffs)], lonmaxdiffs)
  y1 <- match(latmindiffs[latmindiffs == min(latmindiffs)], latmindiffs)
  y2 <- match(latmaxdiffs[latmaxdiffs == min(latmaxdiffs)], latmaxdiffs)
  ny <- (y2 - y1) + 1
  nx <- (x2 - x1) + 1
  aoilats <- sapply(1:ny, function(x) latmin + x*grid$resolution)
  aoilons <- sapply(1:nx, function(x) lonmin + x*grid$resolution)
  resolution <- grid$resolution

  # Now create a mask for later
  r <- raster::raster(ncols=length(aoilons), nrows=length(aoilats))
  raster::extent(r) <- raster::extent(aoi)
  r <- raster::rasterize(aoi, r, 'UNIT_CODE')  # <------------------------------ Not generalizable
  mask_grid <- r * 0 + 1
  mask_mat <- as.matrix(mask_grid)

  # Get some time information
  ntime_hist <- grid$ntime_hist
  ntime_model <- grid$ntime_model

  # Now we can get the historical and model years together
  urlbase = paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/",
                   "agg_macav2metdata")
  queries <- list()
  for (model in arg_ref$models){
    args = arg_ref$get_args(model)
    params <- args$parameters
    scenarios <- args$scenarios
    ensemble <- args$ensemble
    variables <- arg_ref$variables
    for (param in params){
      for (scenario in scenarios){
        file_name <- paste(c(param, location, model, ensemble, scenario,
                            "macav2metdata_1950_2099_daily.nc"),
                           collapse = "_")
        hattachment <- paste(c(urlbase, param, model, ensemble, "historical",
                               "1950_2005", "CONUS_daily.nc?"),
                             collapse = "_")
        mattachment <- paste(c(urlbase, param, model, ensemble, scenario,
                               "2006_2099", "CONUS_daily.nc?"), collapse = "_")
        var <- variables[param]
        hquery <- paste0(var, glue::glue("[{0}:{1}:{ntime_hist}][{y1}:{1}:{y2}]
                                         [{x1}:{1}:{x2}]#fillmismatch"))
        mquery <- paste0(var, glue::glue("[{0}:{1}:{ntime_model}][{y1}:{1}:{y2}]
                                         [{x1}:{1}:{x2}]#fillmismatch"))
        hurl <- paste0(hattachment, hquery)
        murl <- paste0(mattachment, mquery)
        purl <- c(hurl, murl)
        queries[[length(queries)+1]] <- list(purl, file_name)
      }
    }
  }

  # If we want to parallelize, we could group the pairs further
  ncores <- parallel::detectCores() / 2
  gqueries <- split(queries, ceiling(seq_along(queries)/ncores))
  `%dopar%` <- foreach::`%dopar%`
  cl <- parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl)
  parallel::clusterExport(cl, "retrieve_subset", envir = environment())

  # With parallelization
  for (gq in gqueries) {
      t <- foreach::foreach(i=1:length(gq)) %dopar% {
            retrieve_subset(gq[[i]], dst_folder, parkname, aoilats, aoilons,
                            mask_mat, latmin, latmax, lonmin, lonmax,
                            resolution)
      }
  }
  parallel::stopCluster(cl)
}


get_park_boundaries <- function(dir = "data") {
  # Create directory if not present
  prefix <- file.path(dir, "shapefiles")
  dir.create(prefix, recursive = TRUE, showWarnings = FALSE)

  # Download
  nps_boundary <- file.path(prefix, "nps_boundary.shp")
  file <- file.path(prefix, "nps_boundary.zip")
  url <- "https://irma.nps.gov/DataStore/DownloadFile/627620"
  utils::download.file(url = url, destfile = file, method = "curl")
  utils::unzip(file, exdir = prefix)
}


retrieve_subset <- function(query, dst_folder, parkname, aoilats, aoilons,  # <- Simplify inputs somehow
                            mask_mat, latmin, latmax, lonmin, lonmax,
                            resolution){
  xr <- reticulate::import("xarray")

  # Get the local destination file
  file_name <- query[[2]]
  dst <- file.path(dst_folder, file_name)
  location <- gsub(" ", "_", tolower(parkname))
  
  if (!file.exists(dst)) {
    # Combine historical and modeled url queries
    purl <- query[[1]]

    # Save a local file
    ds <- xr$open_mfdataset(purl, concat_dim="time")

    # add coordinate
    ds <- ds$assign_coords(lat = c('lat' = as.matrix(aoilats)),
                           lon = c('lon' = as.matrix(aoilons)))

    # Mask by boundary
    dsmask <- xr$DataArray(mask_mat)
    dsmask <- dsmask$fillna(0)
    ds <- ds$where(dsmask$data == 1)

    # Update Attributes  <------------------------------------------------------ Standards: https://www.unidata.ucar.edu/software/netcdf-java/current/metadata/DataDiscoveryAttConvention.html
    print("Retrieving old attributes...")
    attrs1 <- ds$attrs
    print("Creating list of new attributes...")
    attrs2 <- list(
      "title" = attrs1$title,
      "id" = attrs1$id,
      "naming_authority" = attrs1$naming_authority,
      "comment" = paste0("Subsetted to ", parkname, "by the North ",
                         "Central Climate Adaption Science Center"),
      "description" = attrs1$description,
      "keywords" = attrs1$keywords,
      "cdm_data_type" = attrs1$cdm_data_type,
      "Metadata_Conventions" = attrs1$Metadata_Conventions,
      "standard_name_vocabulary" = attrs1$standard_name_vocabulary,
      "geospatial_lat_min" = latmin,
      "geospatial_lat_max" = latmax,
      "geospatial_lon_min" = lonmin,
      "geospatial_lon_max" = lonmax,
      "geospatial_lat_units" = attrs1$geospatial_lat_units,
      "geospatial_lon_units" = attrs1$geospatial_lon_units,
      "geospatial_lat_resolution" = resolution,
      "geospatial_lon_resolution" = resolution,
      "geospatial_vertical_min" = '0',
      "geospatial_vertical_min" = '0',
      "geospatial_vertical_resolution" = '0',
      "geospatial_vertical_positive" = '0',
      "time_coverage_start" = "1950-01-01T00:0",
      "time_coverage_end" = "2099-12-31T00:0",
      "time_coverage_duration" = "P150Y",
      "time_coverage_resolution" = "P1D",
      "date_created" = attrs1$date_created,
      "date_issued" = attrs1$date_issued,
      "creator_name" = attrs1$creator_name,
      "creator_url" = attrs1$creator_url,
      "creater_email" = attrs1$creator_email,
      "institution" = attrs1$institution,
      "processing_level" = attrs1$processing_level,
      "contributor_name" = attrs1$contributor_name,
      "contributor_role" = attrs1$contributor_role,
      "publisher_name" = attrs1$publisher_name,
      "publisher_url" = attrs1$publisher_url,
      "license" = attrs1$license,
      "coordinate_system" = attrs1$coordinate_system
    )
    print("Assigning new attributes...")
    ds$attrs <- attrs2
    
    # Save to local file
    start <- Sys.time()
    print("Saving file locally...")
    ds$to_netcdf(dst)
    end <- Sys.time()
    print(end - start)
    
    # Put the file in the bucket
    print("Saving file to cloud...")
    bucket_name <- "cstdata-test"
    object <- file.path(location, file_name)
    aws.s3::put_folder(location, bucket_name)
    aws.s3::put_object(file = dst, object = object, bucket = bucket_name)
  }
}


# Reference Classes
Grid_Reference <- methods::setRefClass(
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
    initialize = function(crs = paste0("+proj=longlat +a=6378137 ",
                                       "+f=0.00335281066474748 +pm=0 +no_defs"),
                          extent = list("latmin" = 25.0631,
                                        "latmax" = 49.3960,
                                        "lonmin" = -124.7722,
                                        "lonmax" = -67.0648),
                          resolution = 0.04166575,
                          nlat = 585,
                          nlon = 1386,
                          ntime_hist = 20453,
                          ntime_model = 34332){
      crs <<- crs
      resolution <<- resolution
      extent <<- extent
      lats <<- sapply(1:(nlat),
                      function(x) extent["latmin"][[1]] + x*resolution)
      lons <<- sapply(1:(nlon),
                      function(x) extent["lonmin"][[1]] + x*resolution)
      ntime_hist <<- ntime_hist
      ntime_model <<- ntime_model
    }
  )
)


Argument_Reference <- methods::setRefClass(
  "argument_options",

  fields = list(
    models = "character",
    parameters = "character",
    scenarios = "character",
    variables = "list",
    units = "list"),

  methods = list(
    initialize = function(
      models = c("bcc-csm1-1", "bcc-csm1-1-m", "BNU-ESM", "CanESM2", "CCSM4",
                 "CNRM-CM5", "CSIRO-Mk3-6-0", "GFDL-ESM2M", "GFDL-ESM2G",
                 "HadGEM2-ES365", "HadGEM2-CC365", "inmcm4", "IPSL-CM5A-LR",
                 "IPSL-CM5A-MR", "IPSL-CM5B-LR", "MIROC5", "MIROC-ESM",
                 "MIROC-ESM-CHEM", "MRI-CGCM3", "NorESM1-M"),
      parameters = c("tasmin", "tasmax", "rhsmin", "rhsmax", "pr", "rsds",
                     "uas", "vas", "huss", "vpd"),
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
                   "specific_humidity" = "kg kg-1"
                   # "vpd" = "kPa"  # vpd breaks
                   )) {
      models <<- models
      parameters <<- parameters
      scenarios <<- scenarios
      variables <<- variables
      units <<- units
    },

    get_args = function(model){
      args <- list()
      for (m in models){
        args[[m]] <- list("parameters" = parameters, "scenarios" = scenarios,
                          "ensemble" =  "r1i1p1")
      }

      # Exceptions
      param_red <-  c("tasmin", "tasmax", "pr", "rsds", "uas", "vas", "huss")
      args[["NorESM1-M"]]$parameters = param_red
      args[["CCSM4"]]$parameters <- param_red
      args[["CCSM4"]]$ensemble <- "r6i1p1"

      return(args[[model]])
    }
  )
)

