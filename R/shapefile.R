
get_shapefile <- function(path, shp_name = NA, dir_loc = tempdir()) {

  # Specify or infer the shapefile name
  if (is.na(shp_name)) shp_name <- tools::file_path_sans_ext(basename(path))

  # Create a path within chosen directory for this shapefile
  dir_loc <- file.path(dir_loc, shp_name)

  # Check if this is a url or local path
  if (RCurl::url.exists(path)) {

    # Only run if folder doesn't exist
    if (!dir.exists(dir_loc)) {
      dir.create(dir_loc, recursive = TRUE, showWarnings = FALSE)
  
      # Create a file path for the zipped folder
      zip_path <- file.path(dir_loc, basename(path))
  
      # Download and unzip
      utils::download.file(url = path, destfile = zip_path, method = "curl")
      utils::unzip(zip_path, exdir = dir_loc)

      # Retrieve the .shp file from the new folder
      shapefile <- list.files(dir_loc, pattern = "\\.shp$", full.names = TRUE)
      aoi <- rgdal::readOGR(shapefile, verbose = FALSE)
    }
  } else {
    # Retrieve the .shp file 
    aoi <- rgdal::readOGR(path, verbose = FALSE)   
  }

  return(aoi)
}


get_park_boundaries <- function(parkname, dir_loc = tempdir()) {
  "
  The url updated every few years. The link is generated with javascript and
  it would be a a bit messy for me to extract new urls from this. Checked all 
  over for a cleaner retrievable reference, even their api didn't appear to
  have it. Perhaps the js is the way to go.
  "
  # Download and return park object
  parks <- get_shapefile(nps_boundary_url(),
                              shp_name = "nps_boundary",
                              dir_loc = dir_loc)

  # Get the boundaries of the chosen national park
  aoi <- parks[grepl(parkname, parks$UNIT_NAME), ]

  return(aoi)
}
