#' Retrieve park boundary shapefiles from the Integrated Resource
#' Management Applications (IRMA) Portal
#'
#' This will provide a method to download all US National Park
#' boundary shapefiles and extract individual boundaries.
#'
#'IRMA: https://irma.nps.gov/Portal

# create directory if not present
if (!file.exists('data/shapefiles')) {
  dir.create(file.path('data', 'shapefiles'), recursive = TRUE, showWarnings = FALSE)
}

# Download if not already preset
nps_boundary <- "data/shapefiles/nps_boundary.shp"
file <- "data/shapefiles/nps_boundary.zip"
if(!file.exists(nps_boundary)){
  url <- "https://irma.nps.gov/DataStore/DownloadFile/627620"
  download.file(url = url, destfile = file, method = "curl")  # Check this, not stable yet
  unzip(file, exdir = "data/shapefiles")
}

# Let's also get the lower 48 states
cb_2016_us_state_20mp <- "data/shapefiles/cb_2016_us_state_20m.shp"
file <- "data/shapefiles/cb_2016_us_state_20m.zip"
if(!file.exists(cb_2016_us_state_20mp)){
  url <- "http://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_state_20m.zip"
  download.file(url = url, destfile = file)
  unzip(file, exdir = "data/shapefiles")
}
