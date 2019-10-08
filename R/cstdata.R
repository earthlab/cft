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
#' configuration file on their local machine. (boolean)
#' @param aws_config_dir The local directory in which to save the configuration
#' file needed for storing data in an AWS S3 bucket. If the file is not yet
#' present in this directory, the user will be prompted for the information
#' needed to build the file. (character)
#'
#' @importFrom methods new


# Main Function
cstdata <- function(parkname="Acadia National Park", start_year = 1950,
                    end_year = 2099, store_locally = TRUE,
                    local_dir = "cstdata", store_remotely = TRUE,
                    aws_config_dir = "~/.aws"){

  # Make sure user is choosing to store somewhere
  if (store_locally == FALSE & store_remotely == FALSE) {
    msg <- "Choose to store either locally, remotely, or both"
    stop(msg)
  }

  # If that works, tell them whats happening  <--------------------------------- Tell them installation folder, too
  print(paste("Retrieving climate data for", parkname))

  # Set up AWS access
  if (store_remotely == TRUE) {
    bucket <- config_aws(aws_config_dir)
  } else{
    bucket <- "na"
  }

  # Generate reference objects
  grid_ref <- Grid_Reference()
  arg_ref <- Argument_Reference()

  # Get national park area of interest
  aoi <- get_park_boundaries(parkname)

  # Match coordinate systems
  aoi <- sp::spTransform(aoi, grid_ref$crs)

  # Get geographic information about the aoi
  aoi_info <- get_aoi_info(aoi, grid_ref)

  # Now we can get the historical and model years together
  gqueries <- get_grouped_queries(aoi, start_year, end_year, arg_ref, grid_ref)

  # Setup parallelization
  ncores <- parallel::detectCores() / 2
  `%dopar%` <- foreach::`%dopar%`
  cl <- parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl)
  parallel::clusterExport(cl, c("retrieve_subset", "filter_years"),
                          envir = environment())

  # Retrieve Subsets
  pb <- utils::txtProgressBar(min = 0, max = length(gqueries), style = 3)
  utils::setTxtProgressBar(pb, 0) 
  for (g in seq(length(gqueries))) {
    gq <- gqueries[[g]]
    t <- foreach::foreach(i=1:length(gq)) %dopar% {  # <------------------------ Build note: "no visible binding for global variable ‘i’"  
      retrieve_subset(gq[[i]], start_year, end_year, parkname, aoi_info,
                      bucket, local_dir, store_locally = store_locally,
                      store_remotely = store_remotely)
    }
    utils::setTxtProgressBar(pb, g) 
  }

  # Done
  close(pb)
  parallel::stopCluster(cl)
}


# Sub functions
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

  # Done.
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


get_aoi_indexes <- function(aoi, grid_ref){
  lonmin <- aoi@bbox[1,1]
  lonmax <- aoi@bbox[1,2]
  latmin <- aoi@bbox[2,1]
  latmax <- aoi@bbox[2,2]
  lonmindiffs <- abs(grid_ref$lons - lonmin)
  lonmaxdiffs <- abs(grid_ref$lons - lonmax)
  latmindiffs <- abs(grid_ref$lats - latmin)
  latmaxdiffs <- abs(grid_ref$lats - latmax)
  x1 <- match(lonmindiffs[lonmindiffs == min(lonmindiffs)], lonmindiffs)
  x2 <- match(lonmaxdiffs[lonmaxdiffs == min(lonmaxdiffs)], lonmaxdiffs)
  y1 <- match(latmindiffs[latmindiffs == min(latmindiffs)], latmindiffs)
  y2 <- match(latmaxdiffs[latmaxdiffs == min(latmaxdiffs)], latmaxdiffs)
  index_pos <- list("y1" = y1, "y2" = y2, "x1" = x1, "x2" = x2)
  return(index_pos)
}


get_aoi_info <- function(aoi, grid_ref) {
  # Get relative index positions to full grid
  index_pos <- get_aoi_indexes(aoi, grid_ref)
  y1 <- index_pos[["y1"]]
  y2 <- index_pos[["y2"]]
  x1 <- index_pos[["x1"]]
  x2 <- index_pos[["x2"]]

  # Get list of lats and lons
  res <- grid_ref$resolution
  ny <- (y2 - y1)
  nx <- (x2 - x1)
  latmin <- aoi@bbox[2,1]
  lonmin <- aoi@bbox[1,1]
  aoilats <- sapply(0:ny, function(x) latmin + x*res)
  aoilons <- sapply(0:nx, function(x) lonmin + x*res)

  # Now create a mask as a matrix
  r <- raster::raster(ncols=length(aoilons), nrows=length(aoilats))
  raster::extent(r) <- raster::extent(aoi)
  r <- raster::rasterize(aoi, r, 'UNIT_CODE')  # <------------------------------ Not generalizable
  mask_grid <- r * 0 + 1
  mask_mat <- methods::as(mask_grid, "matrix")

  # Package all of this into one object
  aoi_info <- list("aoilats" = aoilats,
                   "aoilons" = aoilons,
                   "mask_mat" = mask_mat, 
                   "resolution" = res)

  # Done.
  return(aoi_info)
}


get_grouped_queries <- function(aoi, start_year, end_year, arg_ref, grid_ref){
  # We are building url queries from this base
  urlbase = paste0("http://thredds.northwestknowledge.net:8080/thredds/",
                   "dodsC/agg_macav2metdata")

  # This will be the folder name for this park
  location <- gsub(" ", "_", tolower(aoi$UNIT_NAME))

  # Get time information from our grid reference
  ntime_hist <- grid_ref$ntime_hist
  ntime_model <- grid_ref$ntime_model

  # Get relative index positions to full grid
  index_pos <- get_aoi_indexes(aoi, grid_ref)
  y1 <- index_pos[["y1"]]
  y2 <- index_pos[["y2"]]
  x1 <- index_pos[["x1"]]
  x2 <- index_pos[["x2"]]

  # Build a list of vectors with historical and modeled queries and a filename
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
        q1 <- paste0("[{0}:{1}:{ntime_hist}][{y1}:{1}:{y2}][{x1}:{1}:{x2}]",
                     "#fillmismatch")
        q2 <- paste0("[{0}:{1}:{ntime_model}][{y1}:{1}:{y2}][{x1}:{1}:{x2}]",
                     "#fillmismatch")
        hquery <- paste0(var, glue::glue(q1))
        mquery <- paste0(var, glue::glue(q2))
        hurl <- paste0(hattachment, hquery)
        murl <- paste0(mattachment, mquery)
        purl <- c(hurl, murl)
        queries[[length(queries)+1]] <- list(purl, file_name)
      }
    }
  }

  # Group these queries into groups based on the number of physical cpus
  ncores <- parallel::detectCores() / 2
  gqueries <- split(queries, ceiling(seq_along(queries)/ncores))

  # Done.
  return(gqueries)  
}


get_park_boundaries <- function(parkname, dir = "data") {
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

  # Get the boundaries of the chosen national park
  parks <- rgdal::readOGR(nps_boundary)
  aoi <- parks[grepl(parkname, parks$UNIT_NAME),]

  return(aoi)
}


retrieve_subset <- function(query, start_year, end_year, parkname, aoi_info,
                            bucket, local_dir, store_locally = TRUE,
                            store_remotely = TRUE){
  # Load xarray
  xr <- reticulate::import("xarray")

  # Unpack aoi info
  aoilats <- aoi_info[["aoilats"]]
  aoilons <- aoi_info[["aoilons"]]
  mask_mat <- aoi_info[["mask_mat"]]
  resolution <- aoi_info[["resolution"]]

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

  # These data sets are assumed to be consistent
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
      "geospatial_lat_min" = min(aoilats),
      "geospatial_lat_max" = max(aoilats),
      "geospatial_lon_min" = min(aoilons),
      "geospatial_lon_max" = max(aoilons),
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