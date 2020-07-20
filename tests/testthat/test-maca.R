
test_that("Maca data generator class works :)", {
  
  # Arguments
  park <- "Acadia National Park"
  years <- c(2004, 2005)
  models <- "bcc-csm1-1"
  parameters <- "pr"
  scenarios <- "rcp45"
  local_dir <- "test_project"
  ncores <- 2

  # Initiate an object
  maca <- Maca$new(local_dir)

  # Set/Get the Area of Interest
  maca$set_aoi(shp_path, park, area_name = "acadia_national_park")

})