
test_that("Maca data generator class works :)", {
  
  # Arguments
  park <- "Acadia National Park"
  years <- c(2004, 2005)
  models <- "bcc-csm1-1"
  parameters <- "pr"
  scenarios <- "rcp45"
  project_dir <- "."
  ncores <- 2

  # Initiate an object
  maca <- Maca$new(project_dir)

  # Set/Get the Area of Interest
  maca$set_aoi(park, area_name = "acadia_national_park")

})