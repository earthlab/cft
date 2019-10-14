
download_shapefile <- function(url, shp_name = NA, dir_loc = tempdir()) {

  # Specify or infer the shapefile name
  if (is.na(shp_name)) shp_name <- tools::file_path_sans_ext(basename(url))

  # Create a folder within chosen directory for shapefiles
  dir_loc <- file.path(dir_loc, shp_name)

  # Only run if file doesn't exist
  if (!dir.exists(dir_loc)) {
    dir.create(dir_loc, recursive = TRUE, showWarnings = FALSE)

    # Create a file path for the zipped folder
    zip_path <- file.path(dir_loc, basename(url))

    # Download and unzip
    utils::download.file(url = url, destfile = zip_path, method = "curl")
    utils::unzip(zip_path, exdir = dir_loc)
  }

  # Retrieve the .shp file from the new folder
  shapefile <- list.files(dir_loc, pattern = "\\.shp$", full.names = TRUE)
  aoi <- rgdal::readOGR(shapefile, verbose = FALSE)

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
  parks <- download_shapefile(nps_boundary_url(),
                              shp_name = "nps_boundary",
                              dir_loc = dir_loc)

  # Get the boundaries of the chosen national park
  aoi <- parks[grepl(parkname, parks$UNIT_NAME), ]

  return(aoi)
}
