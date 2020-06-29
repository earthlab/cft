
test_that("GridMet data generator class works :)", {
  
  # Arguments
  park <- "Acadia National Park"
  years <- c(2004, 2005)
  parameters <- "pr"
  project_dir <- "test_project"
  ncores <- 2

  # Initiate an object
  gridmet <- GridMet$new(project_dir)

  # Set/Get the Area of Interest
  gridmet$set_aoi(shp_path, park, area_name = "acadia_national_park")

})
