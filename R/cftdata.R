#' Climate Futures Toolbox Data
#' 
#' Retrieves subsetted data of climate future scenarios within National
#' Parks or shapefiles in the Contiguous United States. This data is downscaled
#' using the Multivariate Adaptive Constructed Analogs (MACA) technique.
#'
#' This package retrieves daily gridded data sets of General Circulation Model
#' (GCM) runs clipped to areas of interest and returns a data frame of the
#' file names and they're storage paths. Each of these data sets represent
#' a single GCM, climate variable and Representative Concentration Pathway (RCP)
#' from 1950 to 2099. The 1950 to 2005 portion of this time period represents
#' historical data while the 2006 to 2099 portion represents modeled data. 
#' The original data sets may be found at
#' \url{http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_CMIP5_aggregated_macav2_catalog.html}
#'
#' @param aoi A Spatial object representing an area of interest. Could be a 
#' SpatialPolygonsDataFrame, SpatialLinesDataFrame, or SpatialPointsDataFrame. 
#' (Spatial)
#' @param area_name A name to use in file names, attributes, and
#'  directories. (character)
#' @param models A list of global circulation models to download. If left empty
#' all available models will be downloaded. A list of available of models is
#' available under cft::argument_reference$models. (vector)
#' @param parameters A list of climate parameters to download. If left empty
#' all available parameters will be downloaded. A list of available of models is
#' available under cft::argument_reference$parameters. (vector)
#' @param scenarios A list of representative concentration pathways (rcps) to
#' download. If left empty all available rcps will be downloaded. A list of
#' available of rcps is available under cft::argument_reference$scenarios.
#' (vector)
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
#' d <- cftdata(park = "Acadia National Park", parameters = "pr", 
#'              years = c(2020, 2021), models = "CCSM4", scenarios = "rcp85", 
#'              ncores = parallel::detectCores())
#' }
#' 
#' @importFrom methods new
#' 
#' @export
cftdata <- function(aoi, 
                    area_name,
                    models = argument_reference$models,
                    parameters = argument_reference$parameters, 
                    scenarios = argument_reference$scenarios, 
                    years = c(1950, 2099),
                    local_dir = tempdir(),
                    verbose = TRUE, 
                    ncores = 1) {
  
  # Create the target folder
  area_name <- gsub(" ", "_", tolower(area_name))
  location_dir <- file.path(local_dir, area_name)
  if (!dir.exists(location_dir)) dir.create(location_dir, recursive = TRUE)
  location_dir <- normalizePath(location_dir)

  # Generate reference objects
  grid_ref <- Grid_Reference()
  arg_ref <- Argument_Reference()

  # Match coordinate systems
  aoi <- sp::spTransform(aoi, grid_ref$crs)

  # Get geographic information about the aoi
  if (verbose) print("Building area of interest grid...")
  aoi_info <- get_aoi_info(aoi, grid_ref)

  # Build url queries, filenames, and dataset elements
  queries <- get_queries(aoi, area_name, years, models, parameters, scenarios,
                         arg_ref, grid_ref)

  # Setup parallelization
  pbapply::pboptions(use_lb = TRUE)
  cl <- parallel::makeCluster(ncores)
  on.exit(parallel::stopCluster(cl))
  parallel::clusterExport(cl, "retrieve_subset", envir = environment())

  # If all that works, signal that the process is starting
  if (verbose) {
    print(paste("Retrieving climate data for", area_name))
    print(paste("Saving local files to", location_dir))
  }

  # Retrieve subsets from grouped queries and create file reference data frame
  file_references <- data.frame("local_file" = character(0),
                                "local_path" = character(0),
                                stringsAsFactors = FALSE)

  # Retrieve, subset, and write the files
  refs <- pbapply::pblapply(queries,
                            FUN = retrieve_subset,
                            years = years,
                            aoi_info = aoi_info,
                            area_name = area_name,
                            local_dir = location_dir,
                            cl = cl)


  # Create a data frame from the file references
  file_references <- data.frame(do.call(rbind, refs), stringsAsFactors = FALSE)
  
  file_references <- tibble::as_tibble(lapply(file_references, unlist))
  file_references$parameter_long <- unlist(lapply(
    file_references$parameter, 
    FUN = function(x) argument_reference$variables[x]
  ))
  return(file_references)
}
