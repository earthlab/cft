
get_shapefile <- function(path, shp_name = NA, local_dir = tempdir()) {

  # Specify or infer the shapefile name
  if (is.na(shp_name)) shp_name <- tools::file_path_sans_ext(basename(path))

  # Create a path within chosen directory for this shapefile
  shp_folder <- file.path(local_dir, "shapefiles", shp_name)

  # Check if this is a url or local path
  if (RCurl::url.exists(path)) {

    # Only run if no files exist
    exp_file = c(list.files(shp_folder, full.names = TRUE, pattern = "\\.shp$"))
    if (length(exp_file) == 0) {
      dir.create(shp_folder, recursive = TRUE, showWarnings = FALSE)
  
      # Create a file path for the zipped folder
      zip_path <- file.path(shp_folder, basename(path))
  
      # Download and unzip
      utils::download.file(url = path, destfile = zip_path, method = "curl")
      utils::unzip(zip_path, exdir = shp_folder)
      file.remove(zip_path)

      # Retrieve the .shp file from the new folder
      shapefile <- list.files(shp_folder, pattern = "\\.shp$", full.names = TRUE)
      aoi <- rgdal::readOGR(shapefile, verbose = FALSE)
    }
  } else {
    # Retrieve the .shp file 
    aoi <- rgdal::readOGR(path, verbose = FALSE)   
  }

  return(aoi)
}


get_park_boundaries <- function(national_park, local_dir = tempdir()) {
  "
  The url updated every few years. The link is generated with javascript and
  it would be a a bit messy for me to extract new urls from this. Checked all 
  over for a cleaner retrievable reference, even their api didn't appear to
  have it. Perhaps the js is the way to go.
  "
  # Download and return park object
  parks <- get_shapefile(path = nps_boundary_url(),
                         shp_name = "nps_boundary",
                         local_dir = local_dir)

  # Get the boundaries of the chosen national park
  aoi <- parks[grepl(national_park, parks$UNIT_NAME), ]

  return(aoi)
}
