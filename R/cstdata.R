#' Climate Scenario Toolkit Data
#' 
#' Retrieves subsetted data of climate future scenarios for National Parks in
#' the Contiguous United States. This data is downscaled using the Multivariate
#' Adaptive Constructed Analogs (MACA) technique.
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
#'
#' - The use of reticulate may enable us to use Zarr arrays, which are
#'   accessible directly from an s3 bucket.
#'
#' - This currently saves everything to disk first, which can be a problem. The
#'   alternative is to save each file to a tempfile and overwrite. We'll have to
#'   be careful when parallelizing. However, there has been interest in the
#'   ability to save locally, and for small parks this is fine, so let's an
#'   build in an option to save locally and to the s3 bucket.
#' 
#' @param parkname The name of the U.S. National Park for which to download data
#' , e.g., "Yellowstone National Park". (character)
#' @param start_year The first year of the desired period. (integer)
#' @param end_year The last year of the desired period. (integer)
#' @param store_locally If `TRUE` this function will store the results in a
#' local directory as NetCDF files. This options may be set to `FALSE` to save
#' disk space, but the `store_remotely` option must be set to `TRUE` in this
#' case. (boolean)
#' @param local_dir The local directory in which to save files if
#' `store_locally` is set to `TRUE`. (character)
#' @param store_remotely If `TRUE` this function will store the results in an
#' Amazon Web Services S3 bucket. This will require the user to store an S3
#' credentials file on their local machine. (boolean)
#' @param aws_config_dir The local directory in which to save the configuration
#' file needed for storing data in an AWS S3 bucket. If the file is not yet
#' present in this directory, the user will be prompted for the information
#' needed to build the file. (character)
#'
#' @importFrom methods new


cstdata <- function(parkname="Acadia National Park", start_year = 1950,
                    end_year = 2099, store_locally = TRUE,
                    local_dir = "cstdata", store_remotely = TRUE,
                    aws_config_dir = "~/.aws"){
  '
  parkname="Theodore Roosevelt National Park"
  start_year = 1990
  end_year = 2000
  store_locally = TRUE
  local_dir = "cstdata"
  store_remotely = TRUE
  aws_config_dir = "~/.aws"
  '
  
  # Make sure user is choosing to store somewhere
  if (store_locally == FALSE & store_remotely == FALSE) {
    msg <- "Choose to store either locally, remotely, or both"
    stop(msg)
  }
  
  # Set up AWS access
  bucket = "na"
  if (store_remotely == TRUE) {
    bucket <- config_aws(aws_config_dir)
  }
  
  # Make sure we have the national park shapefile
  get_park_boundaries()
  
  # Get the boundaries of the chosen national park
  parks <- rgdal::readOGR("data/shapefiles/nps_boundary.shp")
  aoi <- parks[grepl(parkname, parks$UNIT_NAME),]
  
  # Make sure we a park-specific destination folder  
  location <- gsub(" ", "_", tolower(aoi$UNIT_NAME))
  print(paste("Retrieving climate data for", location))
  
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
  mask_mat <- methods::as(mask_grid, "matrix")
  
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
                             "macav2metdata", as.character(start_year),
                             as.character(end_year), "daily.nc"),
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
  parallel::clusterExport(cl, c("retrieve_subset", "filter_years"),
                          envir = environment())
  
  # With parallelization
  for (gq in gqueries) {
    t <- foreach::foreach(i=1:length(gq)) %dopar% {  # <---------------------- Build note: "no visible binding for global variable ‘i’"  
      retrieve_subset(gq[[i]], start_year, end_year, parkname, aoilats, aoilons,
                      mask_mat, latmin, latmax, lonmin, lonmax, resolution,
                      bucket, local_dir, store_locally = store_locally,
                      store_remotely = store_remotely)
    }
  }
  parallel::stopCluster(cl)
}


config_aws <- function(aws_config_dir){
  aws_config_file <- file.path(aws_config_dir, "cstdata_config.RDS")
  
  # Build configuration file if needed
  if (!file.exists(aws_config_file)) {
    print("Build AWS Configuration File\n")
    bucket <- readline("s3 bucket name: ")
    key <- readline("aws key: ")
    skey <- readline("aws secret key: ")
    region <- readline("aws region: ")
    creds <- c("bucket" = bucket, "key" = key, "skey" = skey, "region" = region)
    saveRDS(creds, aws_config_file)
  }
  
  # Initialize AWS access
  creds <- readRDS(aws_config_file)
  Sys.setenv("AWS_ACCESS_KEY_ID" = creds['key'],
             "AWS_SECRET_ACCESS_KEY" = creds['skey'],
             "AWS_DEFAULT_REGION" = creds['region'])
  bucket_name <- creds['bucket']
  
  return(bucket_name)
}


filter_years <- function(start_year = 1950, end_year = 2099, 
                         available_start = 1950, available_end = 2099) {
  # Turn these into strings, then dates
  h1 <- as.Date(glue::glue("{available_start}-01-01"))
  h2 <- as.Date(glue::glue("{available_end}-12-31"))
  d1 <- as.Date(glue::glue("{start_year}-01-01"))
  d2 <- as.Date(glue::glue("{end_year}-12-31"))
  
  # If the end year is before the start year, go ahead and switch them
  if (d2 < d1) {
    tmp1 <- d1
    d1 <- d2
    d2 <- tmp1
    rm(tmp1)
  }
  
  # Now, if they asked for more than is available, truncate
  if (d1 < h1) d1 = h1
  if (d2 > h2) d2 = h2
  
  # Return the difference between the target and start dates
  t1 <- as.integer(d1 - h1)
  t2 <- as.integer(d2 - h1)
  
  # Return these as a pair
  return(c(t1, t2))
}


get_park_boundaries <- function(dir = "data") {
  # Create directory if not present
  prefix <- file.path(dir, "shapefiles")
  dir.create(prefix, recursive = TRUE, showWarnings = FALSE)
  nps_boundary <- file.path(prefix, "nps_boundary.shp")
  
  # Download if needed
  if (!file.exists(nps_boundary)){
    file <- file.path(prefix, "nps_boundary.zip")
    url <- "https://irma.nps.gov/DataStore/DownloadFile/629794"  # <------------ This url is not consistent!
    utils::download.file(url = url, destfile = file, method = "curl")
    utils::unzip(file, exdir = prefix)
  }
}


retrieve_subset <- function(query, start_year, end_year, parkname, aoilats,
                            aoilons, mask_mat, latmin, latmax, lonmin, lonmax,
                            resolution, bucket, local_dir, store_locally = TRUE,
                            store_remotely = TRUE){
  xr <- reticulate::import("xarray")
  
  # Get the destination file
  location <- gsub(" ", "_", tolower(parkname))
  local_park_dir <- file.path(local_dir, location)
  if (store_locally == TRUE) {
    dst_folder <- local_park_dir
    if (!dir.exists(dst_folder)){
      dir.create(dst_folder, recursive = TRUE)
    }
    file_name <- query[[2]]
    store_name <- query[[2]]
    dst <- file.path(dst_folder, file_name)
  } else {
    store_name <- query[[2]]
    dst <- tempfile(fileext = '.nc')
    file_name <- basename(dst)
    dst_folder <- dirname(dst)
  }

  if (!file.exists(dst)) {
    # Get the combined historical and modeled url query
    purl <- query[[1]]
    
    # Save a local file
    ds <- xr$open_mfdataset(purl, concat_dim="time")
    
    # Filter dates
    didx <- filter_years(start_year, end_year)
    ds <- ds$sel(time = c(didx[[1]]: didx[[-1]]))
    
    # add coordinate
    ds <- ds$assign_coords(lat = c('lat' = as.matrix(aoilats)),
                           lon = c('lon' = as.matrix(aoilons)))
    
    # Mask by boundary
    dsmask <- xr$DataArray(mask_mat)
    dsmask <- dsmask$fillna(0)
    ds <- ds$where(dsmask$data == 1)
    
    # Update Attributes  <------------------------------------------------------ Standards: https://www.unidata.ucar.edu/software/netcdf-java/current/metadata/DataDiscoveryAttConvention.html
    summary = paste0(
      "This archive contains daily downscaled meteorological and hydrological ",
      "projections for the Conterminous United States at 1/24-deg resolution ",
      "utilizing the Multivariate Adaptive Constructed Analogs (MACA, ",
      "Abatzoglou, 2012) statistical downscaling method with the METDATA ",
      "(Abatzoglou,2013) training dataset. The downscaled meteorological ",
      "variables are maximum/minimum temperature(tasmax/tasmin), ",
      "maximum/minimum relative humidity(rhsmax/rhsmin) precipitation ",
      "amount(pr), downward shortwave solar radiation(rsds), eastward ",
      "wind(uas), northward wind(vas), and specific humidity(huss). The ",
      "downscaling is based on the 365-day model outputs from different ",
      "global climate models (GCMs) from Phase 5 of the Coupled Model ",
      "Inter-comparison Project (CMIP3) utlizing the historical (1950-2005) ",
      "and future RCP4.5/8.5(2006-2099) scenarios. Leap days have been added ",
      "to the dataset from the average values between Feb 28 and Mar 1 in ",
      "order to aid modellers. Daily vapor pressure deficit is calculate from ",
      "downscaled daily minimum and maximum temperatures for saturated vapor ",
      "pressure and daily dew point temperature for actual vapor pressure. ",
      "The dew point temperature is estimated by converting downscaled daily ",
      "mean specific humidity to partial pressure of moisture in the ",
      "atmosphere and estimating pressure from elevation using the barometric ",
      "formula.")
    print("Retrieving old attributes...")
    attrs1 <- ds$attrs
    print("Creating list of new attributes...")
    attrs2 <- list(
      "title" = attrs1$title,
      "author" = "John Abatzoglou-University of Idaho, jabatzoglou@uidaho.edu",
      "comment" = paste0("Subsetted to ", parkname, "by the North Central ",
                         "Climate Adaption Science Center"),
      "summary" = summary,
      "coordinate_system" = attrs1$coordinate_system,
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
      "time_coverage_start" = glue::glue("{start_year}-01-01T00:0"),
      "time_coverage_end" = glue::glue("{end_year}-12-31T00:0"),
      "time_coverage_duration" = glue::glue("P{end_year - start_year + 1}Y"),
      "time_coverage_resolution" = "P1D"
      # "id" = attrs1$id,  # <-------------------------------------------------- Old attributes, these have changed since vapor pressure deficit came out recently
      # "naming_authority" = attrs1$naming_authority,
      # "description" = attrs1$description,
      # "keywords" = attrs1$keywords,
      # "cdm_data_type" = attrs1$cdm_data_type,
      # "Metadata_Conventions" = attrs1$Metadata_Conventions,
      # "standard_name_vocabulary" = attrs1$standard_name_vocabulary,
      # "date_created" = attrs1$date_created,
      # "date_issued" = attrs1$date_issued,
      # "creator_name" = attrs1$creator_name,
      # "creator_url" = attrs1$creator_url,
      # "creater_email" = attrs1$creator_email,
      # "institution" = attrs1$institution,
      # "processing_level" = attrs1$processing_level,
      # "contributor_name" = attrs1$contributor_name,
      # "contributor_role" = attrs1$contributor_role,
      # "publisher_name" = attrs1$publisher_name,
      # "publisher_url" = attrs1$publisher_url,
      # "license" = attrs1$license,
    )
    print("Assigning new attributes...")
    ds$attrs <- attrs2
    
    # Save to local file
    ds$to_netcdf(dst)
    
    # Put the file in the bucket
    if (store_remotely == TRUE) {
      object <- file.path(location, store_name)
      aws.s3::put_folder(location, bucket)
      aws.s3::put_object(file = dst, object = object, bucket = bucket)
    }
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
                   "specific_humidity" = "kg kg-1",
                   "vpd" = "kPa"
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

