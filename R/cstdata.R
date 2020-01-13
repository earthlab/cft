#' Climate Scenario Toolkit Data
#' 
#' Retrieves subsetted data of climate future scenarios within specified National
#' Parks or shapefiles in the Contiguous United States. This data is downscaled
#' using the Multivariate Adaptive Constructed Analogs (MACA) technique.
#'
#' This package retrieves daily gridded data sets of General Circulation Model
#' (GCM) runs clipped to specified areas of interest and returns a data frame of the
#' file names and they're storage paths. Each of these data sets represent
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
#' - RCurl::url_exists works in linux and osx, but sometimes fails in windows.
#'   It apparently needs proxy settings in these cases. Try setting proxy
#'   options next chance on a windows machine.
#'
#' @param shp_path A path to a shapefile with which to clip the resulting data
#'  sets. This path may be to a local .shp file or a url to a zipped remote
#'  file. If this option is used, leave the `park` argument empty or set
#'  to NA. (character)
#' @param area_name If a shapefile path is used, provide a name to use as a
#'  reference to the location. This will be used in file names, attributes, and
#'  directories. (character)' 
#' @param park The name of a national park (e.g., "Yellowstone National
#'  Park", "Yellowstone Park", "Yellowstone", "yellowstone", etc.). The user may
#'  use this option in place of a shapefile path. In this case, leave the
#'  shp_path argument empty or set to NA. (character)
#' @param models A list of global circulation models to download. If left empty
#' all available models will be downloaded. A list of available of models is
#' available under cstdata::argument_reference$models. (vector)
#' @param parameters A list of climate parameters to download. If left empty
#' all available parameters will be downloaded. A list of available of models is
#' available under cstdata::argument_reference$parameters. (vector)
#' @param scenarios A list of representative concentration pathways (rcps) to
#' download. If left empty all available rcps will be downloaded. A list of
#' available of rcps is available under cstdata::argument_reference$scenarios.
#' (vector)
#' @param years The first and last years of the desired period. (vector)
#' @param store_locally If `TRUE` this function will store the results in a
#'  local directory as NetCDF files. This option may be set to `FALSE` to save
#'  disk space, but the `store_remotely` option must be set to `TRUE` in this
#'  case. (logical)
#' @param local_dir The local directory in which to save files if
#' `store_locally` is set to `TRUE`. (character)
#' @param s3_bucket Optional parameter (character) for an Amazon Web Services
#' S3 bucket to store climate data.
#' @param verbose Print verbose output. (logical)
#' @param ncores The number of cpus to use, overrides the default of half the
#' number of detected cpus. (numeric)
#' 
#' @examples 
#' \dontrun{
#' d <- cstdata(park = "Acadia National Park", parameters = "pr", 
#'              years = c(2020, 2021), models = "CCSM4", scenarios = "rcp85")
#' }
#' 
#' @importFrom methods new
#' 
#' @export
cstdata <- function(shp_path = NA, area_name = NA, park = NA, models = NA,
                    parameters = NA, scenarios = NA, years = c(1950, 2099),
                    store_locally = TRUE, local_dir = tempdir(),
                    s3_bucket = NA, verbose = TRUE, ncores = NA) {

  # Make sure user is providing some kind of location information
  if (is.na(shp_path) & is.na(park)) {
    msg <- paste("No location data/AOI data were provided.",
                 "Please provide either a shapefile path or the name of",
                 "a national park (e.g., 'Yosemite National Park').")
    stop(msg)
  }

  # Make sure user is not providing too much location information
  if (!is.na(shp_path) & !is.na(park)) {
    msg <- paste("Both a shapefile and a national park were provided.",
                 "Please provide either a shapefile path or the name of",
                 "a national park ('Name National Park'), but not both.")
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
  if (!store_locally & is.na(s3_bucket)) {
    msg <- paste("Please set the store_locally and/or the s3_bucket", 
                 "arguments equal to TRUE.")
    stop(msg)
  }

  # If a national park is provided without an area_name, default to park name
  if (!is.na(park) & is.na(area_name)) {
    area_name = park
  }

  # Get national park or area of interest
  if (verbose) print("Retrieving Area of Interest Boundaries")
  aoi <- get_aoi(park, shp_path, area_name, local_dir)

  # Create the target folder
  area_name <- gsub(" ", "_", tolower(area_name))
  location_dir <- file.path(local_dir, area_name)
  if (!dir.exists(location_dir)) dir.create(location_dir, recursive = TRUE)
  location_dir = normalizePath(location_dir)

  # Set up AWS access
  if (!is.na(s3_bucket)) {
    aws_url <- paste0("https://s3.console.aws.amazon.com/s3/buckets/", 
                      s3_bucket,
                      "/", area_name, 
                      "/?region=", Sys.getenv("AWS_DEFAULT_REGION"),
                      "&tab=overview")
  } else {
    aws_url <- NA
  }

  # Generate reference objects
  grid_ref <- Grid_Reference()
  arg_ref <- Argument_Reference()

  # Match coordinate systems
  aoi <- sp::spTransform(aoi, grid_ref$crs)

  # Get geographic information about the aoi
  aoi_info <- get_aoi_info(aoi, grid_ref)

  # Build url queries, filenames, and dataset elements
  queries <- get_queries(aoi, area_name, years, models, parameters, scenarios,
                         arg_ref, grid_ref)

  # Setup parallelization
  if (is.na(ncores)) ncores <- get_ncores()
  pbapply::pboptions(use_lb = TRUE)
  cl <- parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl)
  parallel::clusterExport(cl, c("retrieve_subset", "filter_years"),
                          envir = environment())

  # If all that works, signal that the process is starting
  if (verbose) {
    print(paste("Retrieving climate data for", area_name))
    print(paste("Saving local files to", location_dir))
    if (!is.na(s3_bucket)) print(paste("Saving remote files to", aws_url))
  }

  # Retrieve subsets from grouped queries and create file reference data frame
  file_references <- data.frame("local_file" = character(0),
                                "local_path" = character(0),
                                "aws_url" = character(0),
                                stringsAsFactors = FALSE)

  # Retrieve, subset, and write the files
  refs <- pbapply::pblapply(queries,
                            FUN = retrieve_subset,
                            years = years,
                            aoi_info = aoi_info,
                            area_name = area_name,
                            local_dir = location_dir,
                            store_locally = store_locally,
                            s3_bucket = s3_bucket,
                            cl = cl)


  # Create a data frame from the file references
  file_references <- data.frame(do.call(rbind, refs), stringsAsFactors = FALSE)

  # Close cluster  # <--------------------------------------------------------- Close in exception
  parallel::stopCluster(cl)

  # Reset index of file reference data frame
  rownames(file_references) <- seq(nrow(file_references))

  # Return file references
  return(file_references)
}
