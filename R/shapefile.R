get_aoi <- function(park, shp_path, area_name, local_dir) {
  # Choose which to area of interest to return.
  if (!is.na(park)) {
    aoi <- get_park_boundaries(park, local_dir = local_dir)
  } else {
    aoi <- rgdal::readOGR(shp_path, verbose = FALSE)
  }
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
  local_prefix <- file.path(local_dir, "shapefiles", "nps_boundary")
  local_path <- file.path(local_prefix, "nps_boundary.shp")

  # Check if the National Park Shapefile is stored locally
  if (!file.exists(local_path)) {
    dir.create(local_prefix, showWarnings = FALSE, recursive = TRUE)
    zip_path <- file.path(local_prefix, "nps.zip")
    utils::download.file(nps_boundary_url(), destfile = zip_path)
    utils::unzip(zip_path, exdir = local_prefix)
    if(!file.exists(local_path)) {
      stop(
        paste(
          "Expected file", local_path, "to exist, but it was not found.", 
          "The files in that directory include: ", 
          paste(list.files(path=local_prefix, full.names = TRUE), 
                collapse = ", ")
        )
      )
    }
  }
  
  parks <- rgdal::readOGR(local_path, verbose = FALSE)
  
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
