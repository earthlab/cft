#' Climate Futures Toolbox Data
#'
#' Retrieves subsets of historical or projected climate datsets.
#' Available datasets include a Multivariate Adaptive Constructed Analogs (MACA)
#' dataset and the Gridded Surface Meteorological Dataset (gridMET).
#'
#' This package retrieves daily gridded data sets of General Circulation Model  # We need to rephrase this
#' (GCM) runs clipped to areas of interest and returns a data frame of the
#' file names and they're storage paths. Each of these data sets represent
#' a single GCM, climate variable and Representative Concentration Pathway (RCP)
#' from 1950 to 2099. The 1950 to 2005 portion of this time period represents
#' historical data while the 2006 to 2099 portion represents modeled data.
#' The original data sets may be found at
#' \url{http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_CMIP5_aggregated_macav2_catalog.html}
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
#' @param dataset The name of the data source from which to retrieve data. Defaults
#' to "maca". (character)
#'  \itemize{
#'    \item "maca" = Multivariate Adaptive Constructed Analogs
#'    \item "gridmet" = Gridded Surface Meteorological Dataset
#'  }
#' @param models A list of global circulation models to download. Only available
#'  for GCMs. If left empty all available models will be downloaded. A list of
#'  currently available models is available under cft::get_reference("maca")$models.
#'  (vector)
#' @param parameters A list of climate parameters to download. If left empty
#'  all available parameters will be downloaded. A list of available of models is
#'  available under cft::get_reference(<dataset>)$parameters. (vector)
#' @param scenarios A list of representative concentration pathways (rcps) to
#'  download. If left empty all available rcps will be downloaded. A list of
#'  available of rcps is available under cft::get_reference(<dataset>)$scenarios.
#'  (vector)
#' @param years The first and last years of the desired period. (vector)
#' @param project_dir The local directory in which to save files. By default,
#'  files are saved in a temporary directory (as per CRAN guidelines), and are
#'  lost after your R session ends. Specify a path to a local directory with
#'  this argument to retain files and avoid duplicate downloads
#'  in subsequent R sessions.  (character)
#' @param verbose Print verbose output. (logical)
#' @param ncores The number of cpus to use, which defaults to 1. (numeric)
#'
#' @return A tibble containing information about climate data files.
#'
#' @examples
#' \donttest{
#' d <- cftdata(park = "Acadia National Park", parameters = "pr", dataset = "maca",
#'              years = c(2020, 2021), models = "CCSM4", scenarios = "rcp85",
#'              ncores = parallel::detectCores())
#' }
#'
#' @importFrom methods new

#' @export
cftdata <- function(shp_path,
                    area_name,
                    park,
                    dataset = "maca",
                    models = get_reference("maca")$models,
                    parameters = get_reference("maca")$parameters,
                    scenarios = get_reference("maca")$scenarios,
                    years = c(1950, 2099),
                    project_dir = tempdir(),
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
  if (missing(park)) {
    park <- NA
  }

  # Retrieve and initialize a dataset generator object
  DATASETS = list("maca" = Maca, "gridmet" = GridMet)
  ds <- DATASETS[[dataset]]$new(project_dir, verbose = verbose)

  # Set the area of interest information
  ds$set_aoi(shp_path, area_name, park)

  # And run the thing
  ds$get_subset(models, parameters, scenarios, years, ncores)

  # Return file reference
  return(ds$file_references)
  
}
