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
#' - This command apparently install python packages automatically through
#'   conda: "reticulate::conda_install(envname, packages)"
#' - The R Packages book suggests that we don't put all of these functions into
#'   file (why not?). It looks like we should split them up and use
#'   devtools::load_all() instead.
#' - Where should the default local data storage go? Tmp?
#' - The builds work find with reticulate, but we will need to take a few more
#'   steps:
#'    1) I've not had luck with python 2, though this will more often be the default
#'       I had to create a .Renviron file with the line:
#'      
#'           RETICULATE_PYTHON="/usr/bin/python3"
#'
#'       This won't do, though, its always different
#'
#'    2) In addition to xarray, I needed to install netcdf4, dask, and toolz
#'    3) We could better ensure that this functions by using a conda environment,
#'       but that might require extra doing on the users part.
#'    4) Xarray 0.14 will change the behaviour of open_mfdataset, 0.13 is giving 
#'       deprecation warnings. Perhaps we pin xarray to 0.12 (same behavior,
#'       no warnings). 
#'
#' @param shp_path A path to a shapefile with which to clip the resulting data
#'  sets. If this option is used, leave the national_park argument empty or set
#'  to NA. (character)
#' @param area_name If a shapefile path is provided, provide a name to use as a
#'  reference to the location. This will be used in file names and directories. 
#'  (character)' 
#' @param national_park The name of a national park (e.g., "Yellowstone National
#'  Park". The user may use this option in place of a shapefile path. In this
#'  case, leave the shp_path argument empty or set to NA. (character)
#' @param start_year The first year of the desired period. (integer)
#' @param end_year The last year of the desired period. (integer)
#' @param store_locally If `TRUE` this function will store the results in a
#'  local directory as NetCDF files. This options may be set to `FALSE` to save
#'  disk space, but the `store_remotely` option must be set to `TRUE` in this
#'  case. (logical)
#' @param local_dir The local directory in which to save files if
#' `store_locally` is set to `TRUE`. (character)
#' @param store_remotely If `TRUE` this function will store the results in an
#' Amazon Web Services S3 bucket. This will require the user to store an S3
#' configuration file on their local machine. (logical)
#' @param aws_config_dir The local directory in which to save the configuration
#' file needed for storing data in an AWS S3 bucket. If the file is not yet
#' present in this directory, the user will be prompted for the information
#' needed to build the file. (character)
#' @param verbose Print verbose output. (logical)
#' 
#' @importFrom methods new
#' 
#' @export
cstdata <- function(shp_path = NA, area_name = NA, national_park = NA,
                    start_year = 1950, end_year = 2099,
                    store_locally = TRUE, local_dir = tempdir(),
                    store_remotely = TRUE, aws_config_dir = "~/.aws",
                    verbose = TRUE) {
  '
  shp_path = "/home/travis/Desktop/yellowstone/yellowstone.shp"
  area_name = "Yellowstone National Park"
  national_park = NA
  start_year = 1995
  end_year = 2000
  store_locally = TRUE
  local_dir = tempdir()
  store_remotely = TRUE
  aws_config_dir = "~/.aws"
  verbose = TRUE

  cstdata(shp_path = shp_path, area_name = "test_area", start_year = start_year, end_year = end_year)
  '
  # Make sure user is providing some kind of location information
  if (is.na(shp_path) & is.na(national_park)) {
    msg <- paste("Please provide either a shapefile path or the name of",
                 "a nation park ('Name National Park').")
    stop(msg)
  }

  # Make sure user is not providing too much location information
  if (!is.na(shp_path) & !is.na(national_park)) {
    msg <- paste("Please provide either a shapefile path or the name of",
                 "a nation park ('Name National Park'), but not both.")
    stop(msg)
  }

  # If a shapefile path is provided, make sure it comes with an area name
  if (!is.na(shp_path) & is.na(area_name)) {
    msg <- paste("Please provide a the name you would like to use to reference",
                 "the location of the shapefile provided. This will be used",
                 "for both file names and directories.")
    stop(msg)
  }

  # Make sure user is choosing to store somewhere
  if (!store_locally & !store_remotely) {
    msg <- paste("Please set the store_locally and/or the store_remotely", 
                 "arguments equal to TRUE.")
    stop(msg)
  }

  # Create the target folder
  location_folder <- gsub(" ", "_", tolower(area_name))
  location_dir <- file.path(local_dir, location_folder)
  if (!dir.exists(location_dir)) dir.create(location_dir)
  location_dir = normalizePath(location_dir)

  # Set up AWS access
  if (store_remotely) {
    aws_creds <- config_aws(aws_config_dir)
    bucket = aws_creds["bucket"]
    region = aws_creds["region"]
    aws_url <- paste0("https://s3.console.aws.amazon.com/s3/buckets/", bucket,
                      "/", location_folder, "/?region=", region, "&tab=overview")
  } else {
    aws_creds <- NA
    aws_url <- NA
  }

  # Generate reference objects
  grid_ref <- Grid_Reference()
  arg_ref <- Argument_Reference()

  # Get national park area of interest
  if (!is.na(national_park)) {
    aoi <- get_park_boundaries(national_park)
    area_name <- national_park
  } else {
    aoi <- rgdal::readOGR(shp_path, verbose = FALSE) 
  }

  # Match coordinate systems
  aoi <- sp::spTransform(aoi, grid_ref$crs)

  # Get geographic information about the aoi
  aoi_info <- get_aoi_info(aoi, grid_ref)

  # Build url queries and group by number of cpus
  ncores <- parallel::detectCores() / 2
  grouped_queries <- get_grouped_queries(aoi, location_dir, start_year,
                                         end_year, arg_ref, grid_ref, ncores)


  # Setup parallelization
  cl <- parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl)
  parallel::clusterExport(cl, c("retrieve_subset", "filter_years"),
                          envir = environment())

  # If all that works, signal that the process is starting
  if (verbose) {
    print(paste("Retrieving climate data for", area_name))
    print(paste("Saving local files to", location_dir))
    if (store_remotely) print(paste("Saving remote files to", aws_url))
  }

  # Retrieve subsets from grouped queries and create file reference data frame
  pbapply::pboptions(type="none")
  file_references <- data.frame("local_file" = character(0),
                                "local_path" = character(0),
                                "aws_url" = character(0),
                                stringsAsFactors = FALSE)
  pb <- utils::txtProgressBar(min = 1, max = length(grouped_queries), style = 3)
  for (i in seq_along(grouped_queries)) {

    # Putting progress bar here to avoid delayed ticks
    utils::setTxtProgressBar(pb, i)

    # Retrieve, subset, and write the files
    refs <- pbapply::pblapply(grouped_queries[[i]],
                              FUN = retrieve_subset, 
                              start_year = start_year, 
                              end_year = end_year,
                              aoi_info = aoi_info,
                              location_dir = location_dir, 
                              aws_creds = aws_creds,
                              store_locally = store_locally,
                              store_remotely = store_remotely,
                              cl = cl)

    # Add the file reference outputs to data frame
    refdf <- do.call(rbind.data.frame, refs)
    file_references <- rbind(file_references, refdf)
  }

  # Close cluster
  parallel::stopCluster(cl)

  # Reset index of file reference data frame
  rownames(file_references) <- seq(nrow(file_references))

  # Return file references
  return(file_references)
}


# Sub functions
config_aws <- function(aws_config_dir) {
  aws_config_file <- file.path(aws_config_dir, "cstdata_config.RDS")

  # Build configuration file if needed
  if (!file.exists(aws_config_file)) {
    if (!dir.exists(aws_config_dir)) {
      dir.create(aws_config_dir)
    }
    print("Build AWS Configuration File\n")
    bucket <- readline("s3 bucket name: ")
    key <- readline("aws key: ")
    skey <- readline("aws secret key: ")
    region <- readline("aws region: ")
    aws_creds <- c("bucket" = bucket, "key" = key, "skey" = skey,
                   "region" = region)
    print(paste("Saving configuration file to", aws_config_file))
    saveRDS(aws_creds, aws_config_file)
  }

  # Initialize AWS access
  aws_creds <- readRDS(aws_config_file)
  Sys.setenv("AWS_ACCESS_KEY_ID" = aws_creds["key"],
             "AWS_SECRET_ACCESS_KEY" = aws_creds["skey"],
             "AWS_DEFAULT_REGION" = aws_creds["region"])

  # Return the credentials
  return(aws_creds)
}


filter_years <- function(start_year = 1950, end_year = 2099,
                         available_start = 1950, available_end = 2099) {
  # Turn these into strings, then dates
  available1 <- as.Date(glue::glue("{available_start}-01-01"))
  available2 <- as.Date(glue::glue("{available_end}-12-31"))
  date1 <- as.Date(glue::glue("{start_year}-01-01"))
  date2 <- as.Date(glue::glue("{end_year}-12-31"))

  # Check that the end year is after the start year
  if (date2 < date1) stop("end_year is set before start_year")

  # Check that the requested years are available
  if (date1 < available1) {
    stop(paste0("start_year is unavailable. Please choose a year between ",
                 available_start, " and ", available_end, "."))
  }
  if (date2 > available2) {
    stop(paste0("end_year is unavailable. Please choose a year between ",
                available_start, " and ", available_end, "."))
  }

  # Return the difference in days between the first available and chosen dates
  day1 <- as.integer(date1 - available1)
  day2 <- as.integer(date2 - available1)

  # Return these as a pair
  return(c(day1, day2))
}


get_aoi_indexes <- function(aoi, grid_ref) {

  # Extract the bounding box of the area of interest
  lonmin <- aoi@bbox[1, 1]
  lonmax <- aoi@bbox[1, 2]
  latmin <- aoi@bbox[2, 1]
  latmax <- aoi@bbox[2, 2]

  # Calculate differences between bounding box and target grid coordinates
  lonmindiffs <- abs(grid_ref$lons - lonmin)
  lonmaxdiffs <- abs(grid_ref$lons - lonmax)
  latmindiffs <- abs(grid_ref$lats - latmin)
  latmaxdiffs <- abs(grid_ref$lats - latmax)

  # Find the index positions of the closest grid coordinates to the aoi extent
  x1 <- match(lonmindiffs[lonmindiffs == min(lonmindiffs)], lonmindiffs)
  x2 <- match(lonmaxdiffs[lonmaxdiffs == min(lonmaxdiffs)], lonmaxdiffs)
  y1 <- match(latmindiffs[latmindiffs == min(latmindiffs)], latmindiffs)
  y2 <- match(latmaxdiffs[latmaxdiffs == min(latmaxdiffs)], latmaxdiffs)

  # Create a list with each required grid index position
  index_pos <- list("y1" = y1, "y2" = y2, "x1" = x1, "x2" = x2)

  # Return list  
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
  latmin <- aoi@bbox[2, 1]
  lonmin <- aoi@bbox[1, 1]
  aoilats <- sapply(0:ny, function(x) latmin + x * res)
  aoilons <- sapply(0:nx, function(x) lonmin + x * res)

  # Now create a mask as a matrix
  r <- raster::raster(ncols = length(aoilons), nrows = length(aoilats))
  raster::extent(r) <- raster::extent(aoi)

  # TODO: write tests for this function.
  r <- raster::rasterize(aoi, r)
  mask_grid <- r * 0 + 1
  mask_matrix <- methods::as(mask_grid, "matrix")

  # Package all of this into one object
  aoi_info <- list("aoilats" = aoilats,
                   "aoilons" = aoilons,
                   "mask_matrix" = mask_matrix,
                   "resolution" = res)

  # Done.
  return(aoi_info)
}


get_grouped_queries <- function(aoi, location_dir, start_year, end_year,
                                arg_ref, grid_ref, ncores) {

  # We are building url queries from this base
  urlbase <- paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/",
                    "agg_macav2metdata")

  # This will be the folder name for this park
  location <- basename(location_dir)

  # Get time information from our grid reference
  ntime_hist <- grid_ref$ntime_hist
  ntime_model <- grid_ref$ntime_model

  # Get relative index positions to full grid
  index_pos <- get_aoi_indexes(aoi, grid_ref)
  y1 <- index_pos[["y1"]]
  y2 <- index_pos[["y2"]]
  x1 <- index_pos[["x1"]]
  x2 <- index_pos[["x2"]]

  # Build a list of lists with historical/future model queries and a file name
  queries <- list()
  for (model in arg_ref$models) {
    args <- arg_ref$get_args(model)
    variables <- arg_ref$variables
    params <- args$parameters
    scenarios <- args$scenarios
    ensemble <- args$ensemble
    for (param in params) {
      for (scenario in scenarios) {

        # Get internal variable name
        var <- variables[param]

        # Build local file name
        file_name <- paste(c(param, location, model, ensemble, scenario,
                             "macav2metdata", as.character(start_year),
                             as.character(end_year), "daily.nc"),
                           collapse = "_")

        # Build remote historical and future file names
        historical <- paste(c(urlbase, param, model, ensemble, "historical",
                              "1950_2005", "CONUS_daily.nc"), collapse = "_")
        future <- paste(c(urlbase, param, model, ensemble, scenario,
                          "2006_2099", "CONUS_daily.nc"), collapse = "_")

        # Build the temporal and spatial subsets
        historical_subset <- glue::glue(paste0("?{var}[{0}:{1}:{ntime_hist}]",
                                               "[{y1}:{1}:{y2}][{x1}:{1}:{x2}]",
                                               "#fillmismatch"))
        future_subset <- glue::glue(paste0("?{var}[{0}:{1}:{ntime_model}]",
                                           "[{y1}:{1}:{y2}][{x1}:{1}:{x2}]",
                                           "#fillmismatch"))

        # Combine everything into a query package and add to query list
        historical_url <- paste0(historical, historical_subset)
        future_url <- paste0(future, future_subset)
        paired_url <- c(historical_url, future_url)
        queries[[length(queries) + 1]] <- list(paired_url, file_name)
      }
    }
  }

  # Group these queries based on the number of physical cpus
  grouped_queries <- split(queries, ceiling(seq_along(queries) / ncores))

  # Done.
  return(grouped_queries)
}


get_park_boundaries <- function(parkname, dir = tempdir()) {

  # Create directory if not present
  prefix <- file.path(dir, "shapefiles")
  dir.create(prefix, recursive = TRUE, showWarnings = FALSE)
  nps_boundary <- file.path(prefix, "nps_boundary.shp")

  # Download if needed
  if (!file.exists(nps_boundary)) {
    file <- file.path(prefix, "nps_boundary.zip")
    url <- "https://irma.nps.gov/DataStore/DownloadFile/629794"  # <------------ This is updated every few years. The link is generated with javascript and it would be a a bit messy for me to extract new urls from this. Checked all over for a cleaner retrievable reference, even their api didn't appear to have it. Perhaps the js is the way to go.
    utils::download.file(url = url, destfile = file, method = "curl")
    utils::unzip(file, exdir = prefix)
  }

  # Get the boundaries of the chosen national park
  parks <- rgdal::readOGR(nps_boundary, verbose = FALSE)
  aoi <- parks[grepl(parkname, parks$UNIT_NAME), ]

  return(aoi)
}


retrieve_subset <- function(query, start_year, end_year, aoi_info, location_dir,
                            aws_creds, store_locally = TRUE, 
                            store_remotely = TRUE) {

  # Load xarray
  xr <- reticulate::import("xarray")

  # Unpack aoi info
  aoilats <- aoi_info[["aoilats"]]
  aoilons <- aoi_info[["aoilons"]]
  mask_matrix <- aoi_info[["mask_matrix"]]
  resolution <- aoi_info[["resolution"]]

  # Get the destination file
  if (store_locally == TRUE) {
    dst_folder <- location_dir
    if (!dir.exists(dst_folder)) {
      dir.create(dst_folder, recursive = TRUE)
    }
    file_name <- query[[2]]
    store_name <- query[[2]]
    dst <- file.path(dst_folder, file_name)
  } else {
    store_name <- query[[2]]
    dst <- tempfile(fileext = ".nc")
    file_name <- basename(dst)
    dst_folder <- dirname(dst)
  }

  # Retrieve subset and save file locally
  if (!file.exists(dst)) {

    # Get the combined historical and modeled url query
    purl <- query[[1]]

    # Save a local file
    ds <- xr$open_mfdataset(purl, concat_dim = "time")

    # Filter dates
    didx <- filter_years(start_year, end_year)
    stopifnot(length(didx) == 2)
    ds <- ds$sel(time = c(didx[[1]]: didx[[2]]))

    # add coordinate
    ds <- ds$assign_coords(lat = c("lat" = as.matrix(aoilats)),
                           lon = c("lon" = as.matrix(aoilons)))

    # Mask by boundary
    dsmask <- xr$DataArray(mask_matrix)
    dsmask <- dsmask$fillna(0)
    ds <- ds$where(dsmask$data == 1)

    # Update Attributes  <------------------------------------------------------ Standards: https://www.unidata.ucar.edu/software/netcdf-java/current/metadata/DataDiscoveryAttConvention.html
    area_name <- tools::toTitleCase(gsub("_", " ", basename(location_dir)))
    summary <- paste0(
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
    attrs1 <- ds$attrs
    attrs2 <- list(
      "title" = attrs1$title,
      "author" = "John Abatzoglou-University of Idaho, jabatzoglou@uidaho.edu",
      "comment" = paste0("Subsetted to ", area_name, "by the North Central ",
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
      "geospatial_vertical_min" = "0",
      "geospatial_vertical_min" = "0",
      "geospatial_vertical_resolution" = "0",
      "geospatial_vertical_positive" = "0",
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
      # "license" = attrs1$license
    )
    ds$attrs <- attrs2

    # Save to local file
    ds$to_netcdf(dst)
  }

  # Put the file in the bucket
  if (store_remotely == TRUE) {
    bucket <- aws_creds["bucket"]
    region <- aws_creds["region"]
    location_folder <- basename(location_dir)
    object <- file.path(location_folder, store_name)
    aws_url <- paste0("https://s3.console.aws.amazon.com/s3/object/", bucket,
                      "/", location_folder, "/", store_name, "?region=", region,
                      "&tab=overview")
    if (!aws.s3::head_object(object, bucket, silent = TRUE, verbose = FALSE)) {
      aws.s3::put_folder(location_folder, bucket)
      aws.s3::put_object(file = dst, object = object, bucket = bucket)
    }
  } else {
      aws_url <- "NA"
  }

  # Keep track of file information
  file_dir <- normalizePath(dst_folder)
  file_name <- basename(dst)
  file_path <- file.path(file_dir, file_name)
  reference <- list("local_file" = file_name, "local_path" = file_path,
                    "aws_url" = aws_url)
  return(reference)
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
                          ntime_model = 34332) {
      crs <<- crs
      resolution <<- resolution
      extent <<- extent
      lats <<- sapply(1:(nlat),
                      function(x) extent["latmin"][[1]] + x * resolution)
      lons <<- sapply(1:(nlon),
                      function(x) extent["lonmin"][[1]] + x * resolution)
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

    get_args = function(model) {
      args <- list()
      for (m in models) {
        args[[m]] <- list("parameters" = parameters, "scenarios" = scenarios,
                          "ensemble" =  "r1i1p1")
      }

      # Exceptions
      param_red <-  c("tasmin", "tasmax", "pr", "rsds", "uas", "vas", "huss")
      args[["NorESM1-M"]]$parameters <- param_red
      args[["CCSM4"]]$parameters <- param_red
      args[["CCSM4"]]$ensemble <- "r6i1p1"

      return(args[[model]])
    }
  )
)
