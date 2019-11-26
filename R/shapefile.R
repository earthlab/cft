
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

  # Get pair-wise string similarity score with each available name
  scores <-sapply(as.character(names),
                  FUN = stringdist::stringsim,
                  b = parkname)
  
  # Find the index positions of the top five highest scores
  sorted_scores <- sort(scores, decreasing = TRUE)
  top_five <- sorted_scores[1:5]
  indices <- match(top_five, scores)

  # Use the index position to find the character string
  suggestions <- as.character(available_names[indices])

  return(suggestions)
}


get_park_boundaries <- function(parkname, local_dir = tempdir()) {
  local_path <- file.path(local_dir, "shapefiles", "nps_boundary",
                          "nps_boundary.shp")

  # Check if the National Park Shapefile is stored locally
  if (!file.exists(local_path)) {
    path <- nps_boundary_url()
  } else {
    path <- local_path
  }
  
  # Get the National Park Boundary
  parks <- get_shapefile(path = path,
                         shp_name = "nps_boundary",
                         local_dir = local_dir)

  
  # Check that the supplied name is available
  avail_names <- parks$UNIT_NAME
  if (!parkname %in% avail_names) {
    
    # If not, it could be a spelling error. Make suggestions.
    best_matches <- suggest_parkname(parkname, avail_names)
    best_matches <- paste(best_matches, collapse = "\n")
    msg <- paste0("The requested park (", parkname, ") is not contained in ",
                 "the national park boundary data. Here are the top five ",
                 "matches:\n", best_matches, "\n")
    stop(cat(msg))
  }

  aoi <- parks[grepl(parkname, parks$UNIT_NAME), ]

  return(aoi)
}
