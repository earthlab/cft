get_aoi <- function(park, shp_path, area_name, local_dir) {

  # Choose which to area of interest to return.
  if ( !is.na(park) ) {
    aoi <- get_park_boundaries(park, local_dir)
  } else {
    aoi <- get_shapefile(shp_path, area_name, local_dir) 
  }
  return(aoi)
}

get_shapefile <- function(shp_path, area_name = NA, local_dir = tempdir()) {

  if (is.na(area_name)) area_name <- tools::file_path_sans_ext(basename(shp_path))
  local_dir = normalizePath(local_dir)

  # Check if this is a url or local path
  if ( grepl("www.|http:|https:", shp_path) ) {
  
    # Create a local path within chosen local directory for this shapefile
    shp_dir <- file.path(local_dir, "shapefiles")
    shp_folder <- file.path(shp_dir, area_name)
    if ( !httr::http_error(shp_path) ) {
      expected_file <- list.files(shp_folder, full.names = TRUE, 
                                  pattern = "\\.shp$")
      if (length(expected_file) == 0) {

        # Download shapefile if it doesn't exist
        dir.create(shp_folder, recursive = TRUE, showWarnings = FALSE)
        zip_path <- file.path(shp_folder, "temp.zip")
        utils::download.file(url = shp_path, destfile = zip_path, mode = "wb")
        utils::unzip(zip_path, exdir = shp_folder)
        file.remove(zip_path)
      }

      # Now get whatever the shapefile is path from the folder
      path <- list.files(shp_folder, pattern = "\\.shp$", full.names = TRUE)
      print(path)
    }
  } else {
  
    # Just use the given shapefile path
    path <- shp_path
  }

  # Now read locally
  tryCatch({
    aoi <- rgdal::readOGR(path, verbose = FALSE)
  }, error = function(e) {
    stop(paste0("Cannot read ", shp_path))
  })

  return(aoi)
}

suggest_parkname <- function(parkname, available_names) {

  # Make everything lowercase
  names <- tolower(available_names)
  parkname <- tolower(parkname)
  
  # Remove potentially shared words
  parkname <- gsub("national", "", parkname)
  parkname <- gsub("park", "", parkname)
  parkname <- gsub("memorial", "", parkname)
  parkname <- gsub("monument", "", parkname)
  names <- gsub("national", "", names)
  names <- gsub("park", "", names)
  names <- gsub("memorial", "", names)
  names <- gsub("monument", "", names)

  string_similarity <-stringdist::stringsim(names, parkname)
  sorted_string_similarity <- sort(string_similarity, decreasing = TRUE)
  top_five <- sorted_string_similarity[1:5]
  indices <- match(top_five, string_similarity)

  # Use the index position to find the character string
  suggestions <- as.character(available_names[indices])

  return(suggestions)
}


get_park_boundaries <- function(parkname, local_dir = tempdir()) {
  local_path <- file.path(local_dir, "shapefiles", "nps_boundary",
                          "nps_boundary.shp")

  # Check if the National Park Shapefile is stored locally
  if (!file.exists(local_path)) {
    shp_path <- nps_boundary_url()
  } else {
    shp_path <- local_path
  }
  
  # Get the National Park Boundary
  parks <- get_shapefile(shp_path = shp_path,
                         area_name = "nps_boundary",
                         local_dir = local_dir)

  
  # Check that the supplied name is available
  avail_names <- parks$UNIT_NAME
  if (!parkname %in% avail_names) {
    
    # If not, it could be a spelling error. Make suggestions.
    best_matches <- suggest_parkname(parkname, avail_names)
    best_matches <- paste(best_matches, collapse = "\n")
    msg <- paste("The requested park (", parkname, ") is not contained in",
                 "the national park boundary data. Here are the top five",
                 "matches:\n", best_matches, "\n")
    stop(msg)
  }

  aoi <- parks[grepl(parkname, parks$UNIT_NAME), ]

  return(aoi)
}
