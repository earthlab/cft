#' Climate Futures Toolbox gridMET Data
#' 
#' Retrieves subsetted data of historical climate variables within National
#' Parks or shapefiles in the Contiguous United States from the gridMET
#' dataset.
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
#' @param parameters A list of climate parameters to download. If left empty
#' all available parameters will be downloaded. A list of available of models is
#' available under cft::maca_reference$parameters. (vector)
#' @param years The first and last years of the desired period. (vector)
#' @param local_dir The local directory in which to save files. By default, 
#' files are saved in a temporary directory (as per CRAN guidelines), and are
#' lost after your R session ends. Specify a path to a local directory with 
#' this argument to retain files and avoid duplicate downloads
#' in subsequent R sessions.  (character)
#' @param verbose Print verbose output. (logical)
#' @param ncores The number of cpus to use, which defaults to 1. (numeric)
#' 
#' @return A tibble containing information about climate data files. 
#' 
#' @examples 
#' \dontrun{
#' d <- get_gridmet(park = "Acadia National Park",
#'                  parameters = "pr", 
#'                  years = c(1979, 2020),
#'                  ncores = parallel::detectCores())
#' }
#' 
#' @importFrom methods new
#' 
#' @export
get_gridmet <- function(shp_path, 
                        area_name, 
                        park, 
                        parameters = gridmet_reference$parameters,
                        years = c(1979, 2020),
                        local_dir = tempdir(),
                        verbose = TRUE, 
                        ncores = 1) {

  # Make sure user is providing some kind of location information
  if (missing(shp_path) & missing(park)) {
    msg <- paste("No location data/AOI data were provided.",
                 "Please provide either a shapefile path or the name of",
                 "a national park (e.g., 'Yosemite National Park').")
    stop(msg)
  }

  # Make sure user is not providing too much location information
  if (!missing(shp_path) & !missing(park)) {
    msg <- paste("Both a shapefile and a national park were provided.",
                 "Please provide either a shapefile path or the name of",
                 "a national park ('Name National Park'), but not both.")
    stop(msg)
  }

  # If a shapefile path is provided, make sure it comes with an area name
  if (!missing(shp_path) & missing(area_name)) {
    msg <- paste("Please provide the name you would like to use to reference",
                 "the location of the shapefile provided. This will be used",
                 "for both file names and directories.")
    stop(msg)
  }

  if (!missing(park) & missing(area_name)) {
    area_name <- park
  }

  # Get national park or area of interest
  if (verbose) print("Retrieving area of interest boundaries...")
  if (missing(park)) {
    park <- NA
  }
  aoi <- get_aoi(park, shp_path, area_name, local_dir)

  # Create the target folder
  area_name <- gsub(" ", "_", tolower(area_name))
  location_dir <- file.path(local_dir, area_name)
  if (!dir.exists(location_dir)) dir.create(location_dir, recursive = TRUE)
  location_dir <- normalizePath(location_dir)
  
  # Generate reference objects
  arg_ref <- get_reference("gridmet")

  # Match coordinate systems
  aoi <- sp::spTransform(aoi, grid_reference$crs)

  # Get geographic information about the aoi
  if (verbose) print("Building area of interest grid...")
  aoi_info <- get_aoi_info(aoi, local_dir, area_name)

  # Build url queries, filenames, and dataset elements
  queries <- get_gridmet_queries(aoi, area_name, years, parameters)

  # Setup parallelization
  pbapply::pboptions(use_lb = TRUE)
  cl <- parallel::makeCluster(ncores)
  on.exit(parallel::stopCluster(cl))
  parallel::clusterExport(cl, c("retrieve_subset", "filter_years"),
                          envir = environment())

  # If all that works, signal that the process is starting
  if (verbose) {
    print(paste("Retrieving climate data for", area_name))
    print(paste("Saving local files to", location_dir))
  }

  # Retrieve, subset, and write the files
  refs <- pbapply::pblapply(queries,
                            FUN = retrieve_subset,
                            years = years,
                            aoi_info = aoi_info,
                            area_name = area_name,
                            local_dir = location_dir,
                            arg_ref = arg_ref,
                            cl = cl)

  # Create a data frame from the file references
  file_references <- data.frame(do.call(rbind, refs), stringsAsFactors = FALSE)
  file_references <- tibble::as_tibble(lapply(file_references, unlist))
  file_references$parameter_long <- unlist(lapply(
    file_references$parameter, 
    FUN = function(x) maca_reference$variables[x]
  ))
  return(file_references)
}
