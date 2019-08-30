#' Just testing the getData function
library(aws.s3)
library(climateR)
source('R/getScenario.R')
source('R/makeNC.R')

main <- function(park = "Death Valley", startDate = "1950-01-01", endDate = "2099-12-31",
                 method = "maca", param = "tmax", model = "CCSM4", scenario = "rcp45",
                 plotSample = FALSE){
  '
  park = "Death Valley"
  method = "maca"
  param = "tmax"
  model = "CCSM4"
  scenario = "rcp85"
  startDate = "1950-01-01"
  endDate =  "2099-12-31"
  timeRes = "daily"
  plotsample = TRUE
  '
  # Subsetting by National Parks
  if (!exists("parks")){
    parks <- rgdal::readOGR("data/shapefiles/nps_boundary.shp")
  }

  # Query data and save to disk (returning data may not be necessary, perhaps return directory)
  AOI <- parks[grepl(park, parks$UNIT_NAME),]
  folder <- getScenario(AOI,  method = method, param = param, model = model, scenario = scenario,
                        startDate = startDate, endDate = endDate, timeRes = timeRes, plotsample = FALSE)

  # Merge into one file
  files <- file.path(folder, "????????_????????.nc")
  call <- paste0("ncrcat ", files, " ", folder, ".nc")
  system(call)
}
