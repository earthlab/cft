
get_shapefile <- function(path, shp_name = NA, local_dir = tempdir()) {

  if (is.na(shp_name)) shp_name <- tools::file_path_sans_ext(basename(path))

  # Create a path within chosen directory for this shapefile
  local_dir <- file.path(local_dir, "shapefiles")
  shp_folder <- file.path(local_dir, shp_name)

  # Check if this is a url or local path
  if (RCurl::url.exists(path)) {
    expected_file = list.files(shp_folder, 
                               full.names = TRUE, 
                               pattern = "\\.shp$")
    if (length(expected_file) == 0) {
      # download shapefile if it doesn't exist
      dir.create(shp_folder, recursive = TRUE, showWarnings = FALSE)
      zip_path <- file.path(shp_folder, "temp.zip")
      utils::download.file(url = path, destfile = zip_path, method = "curl")
      utils::unzip(zip_path, exdir = shp_folder)
      file.remove(zip_path)
    }
    path <- list.files(shp_folder, pattern = "\\.shp$", full.names = TRUE)
  }
  aoi <- rgdal::readOGR(path, verbose = FALSE)   
  aoi
}

check_parkname <- function(parkname) {
  
  # Start in lower case
  parkname = tolower(parkname)

  # If there is more than one space, reduce to one
  parkname <- gsub("\\s+", " ", parkname)

  # If 'national park' is in there somewhere, assume it's correct
  if ( grepl("national park", parkname) ) {
    

  # If just 'park' is in there, fix it
  } else if (grepl("park", parkname)) {
    parkname <- gsub("park", "", parkname)
    parkname <- paste(parkname, "national park")

  # If no reference to parks, fix that too
  } else {
    parkname <- paste(parkname, "national park")
  }

  # Now reduce multi-spaces again and trim the ends
  parkname <- gsub("\\s+", " ", parkname)
  parkname <- trimws(parkname)

  # And return to title case to match the shapefile format  
  parkname <- tools::toTitleCase(parkname)

  return(parkname)
}


get_park_boundaries <- function(parkname, local_dir = tempdir()) {
  local_path <- file.path(local_dir, "shapefiles", "nps_boundary",
                          "nps_boundary.shp")

  if (!file.exists(local_path)) {
    path <- nps_boundary_url()
  } else {
    path <- local_path
  }
  
  parks <- get_shapefile(path = path,
                         shp_name = "nps_boundary",
                         local_dir = local_dir)

  if (!parkname %in% parks$UNIT_NAME) {
    stop(
        paste0(
          "The requested park (", parkname, ") is not contained in the ", 
          "national park boundary data. Is the park name spelled correctly?"))
  }

    aoi <- parks[grepl(parkname, parks$UNIT_NAME), ]

  return(aoi)
}
