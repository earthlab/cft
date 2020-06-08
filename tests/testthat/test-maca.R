
test_that("Maca data generator class works :)", {
  
  # Arguments
  park <- "Acadia National Park"
  years <- c(2004, 2005)
  models <- "bcc-csm1-1"
  parameters <- "pr"
  scenarios <- "rcp45"
  local_dir <- "."
  ncores <- 2

  # Initiate an object
  maca <- Maca$new(local_dir)

  # Get/Set the Area of Interest
  maca$set_aoi(park, shp_path, area_name, verbose=TRUE)
})