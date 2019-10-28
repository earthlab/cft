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
#' - Xarray 0.14 will change the behaviour of open_mfdataset, 0.13 is giving 
#'   deprecation warnings. Perhaps we pin xarray to 0.12 (same behavior,
#'   no warnings). 
#'
#' @param shp_path A path to a shapefile with which to clip the resulting data
#'  sets. This path may be to a local .shp file or a url to a zipped remote
#'  file. If this option is used, leave the national_park argument empty or set
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
#' @importFrom methods new
#' 
#' @export
cstdata <- function(shp_path = NA, area_name = NA, national_park = NA,
                    start_year = 1950, end_year = 2099, store_locally = TRUE,
                    local_dir = tempdir(), store_remotely = FALSE,
                    aws_config_dir = "~/.aws", verbose = TRUE) {
  '
  shp_path = "/home/travis/Desktop/yellowstone/yellowstone.shp"
  shp_path = "https://www2.census.gov/geo/tiger/TIGER2016/COUSUB/tl_2016_08_cousub.zip"
  shp_path = "/home/travis/Downloads/eGRID2016 Subregions.shp"
  area_name = "Yellowstone National Park"
  national_park = "Yosemite National Park
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
    msg <- paste("No location data/AOI data were provided.",
                 "Please provide either a shapefile path or the name of",
                 "a national park (e.g., 'Yosemite National Park').")
    stop(msg)
  }

  # Make sure user is not providing too much location information
  if (!is.na(shp_path) & !is.na(national_park)) {
    msg <- paste("Both a shapefile and a national park were provided.",
                 "Please provide either a shapefile path or the name of",
                 "a nation park ('Name National Park'), but not both.")
    stop(msg)
  }

  # If a shapefile path is provided, make sure it comes with an area name
  if (!is.na(shp_path) & is.na(area_name)) {
    msg <- paste("Please provide the name you would like to use to reference",
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

  # If a national park is provided without an area_name, default to park name
  if (!is.na(national_park) & is.na(area_name)) {
    area_name = national_park
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
                      "/", location_folder, "/?region=", region,
                      "&tab=overview")
  } else {
    aws_creds <- NA
    aws_url <- NA
  }

  # Generate reference objects
  grid_ref <- Grid_Reference()
  arg_ref <- Argument_Reference()

  # Get national park area of interest
  if (verbose) print("Retrieving all US National Park Boundaries")
  if (!is.na(national_park)) {
    aoi <- get_park_boundaries(national_park)
    area_name <- national_park
  } else {
    aoi <- get_shapefile(shp_path, dir_loc = local_dir) 
  }

  # Match coordinate systems
  aoi <- sp::spTransform(aoi, grid_ref$crs)

  # Get geographic information about the aoi
  aoi_info <- get_aoi_info(aoi, grid_ref)

  # Build url queries and group by number of cpus
  queries <- get_queries(aoi, location_dir, start_year, end_year, arg_ref,
                         grid_ref)

  # Setup parallelization
  pbapply::pboptions(use_lb = TRUE)
  ncores <- parallel::detectCores() / 2
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
  file_references <- data.frame("local_file" = character(0),
                                "local_path" = character(0),
                                "aws_url" = character(0),
                                stringsAsFactors = FALSE)

  # Retrieve, subset, and write the files
  refs <- pbapply::pblapply(queries,
                            FUN = retrieve_subset,
                            start_year = start_year,
                            end_year = end_year,
                            aoi_info = aoi_info,
                            location_dir = location_dir,
                            aws_creds = aws_creds,
                            store_locally = store_locally,
                            store_remotely = store_remotely,
                            cl = cl)

  # Create a data frame from the file references
  file_references <- do.call(rbind.data.frame, refs)

  # Close cluster
  parallel::stopCluster(cl)

  # Reset index of file reference data frame
  rownames(file_references) <- seq(nrow(file_references))

  # Return file references
  return(file_references)
}
