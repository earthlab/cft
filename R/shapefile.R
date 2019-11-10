
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
      zip_path <- file.path(shp_folder, "temp.zip")
  
      # Download and unzip
      utils::download.file(url = path, destfile = zip_path, method = "libcurl")
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


get_park_boundaries <- function(park, local_dir = tempdir()) {
  "
  The url updated every few years. The link is generated with javascript and
  it would be a a bit messy for me to extract new urls from this. Checked all 
  over for a cleaner retrievable reference, even their api didn't appear to
  have it. Perhaps the js is the way to go.
  "
  # This would be the local path if it exists yet
  local_path <- file.path(local_dir, "shapefiles", "nps_boundary",
                          "nps_boundary.shp")

  # Check if the local file exists yet
  if (!file.exists(local_path)) {
    path <- nps_boundary_url()
  } else {
    path <- local_path
  }
  
  # Download and return park object
  parks <- get_shapefile(path = path,
                         shp_name = "nps_boundary",
                         local_dir = local_dir)

  # Get the boundaries of the chosen national park
  aoi <- parks[grepl(park, parks$UNIT_NAME), ]

  return(aoi)
}
