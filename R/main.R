#' This will get all data for any one set of parameters, stack them, and save to s3
library(aws.s3)
library(climateR)
library(readtext)
source('R/getScenario.R')
source('R/getScenario2.R')
source('R/makeNC.R')

main <- function(location = "Death Valley", startDate = "1950-01-01", endDate = "2099-12-31",
                 method = "maca", param = "prcp", model = "CCSM4", scenario = "rcp45",
                 timeRes = "daily", year_range = 5, plotSample = FALSE){
  '
  location = "Wind Cave"
  method = "maca"
  param = "tmin"
  model = "CCSM4"
  scenario = "rcp45"
  startDate = "1950-01-01"
  endDate =  "2099-12-31"
  timeRes = "daily"
  year_range = 4
  plotsample = FALSE
  '
  # Subsetting by National Parks
  if (!exists("parks")){
    parks <- rgdal::readOGR("data/shapefiles/nps_boundary.shp", verbose = FALSE)
  }

  # Get the perimeter and signal that it's working
  AOI <- parks[grepl(tolower(location), tolower(parks$UNIT_NAME)),]  # <----------------There will cometimes be two (e.g. "X park" and "X preserve") so fix that
  loc <- as.character(AOI$UNIT_NAME)
  print(paste0("Retrieving requested data for ", loc))

  # Query data and save to disk and return directory
  folder <- getScenario(AOI, method = method, param = param, model = model,
                        scenario = scenario, startDate = startDate, endDate = endDate,
                        timeRes = timeRes, year_range = year_range, plotsample = FALSE)

  # folder <- getScenario2(AOI, method = method, param = param, model = model,
  #                        scenario = scenario, startDate = startDate, endDate = endDate,
  #                        timeRes = timeRes, year_range = year_range, plotsample = FALSE)

  # Merge into one file - might need to sort first when downloaded in parallel
  files <- file.path(folder, "????????_????????.nc")  # <------------------------------- Silly syntax, but this works rather quickly
  call <- paste0("ncrcat --hst ", files, " ", folder, ".nc")
  system(call)
  outfile <- paste0(folder, '.nc')

  # Push file to s3
  creds <- readtext("~/.aws/credentials.txt")[[2]]
  creds = strsplit(creds, "\n")
  key = substr(creds[[1]][2], 14, 33)
  skey = substr(creds[[1]][3], 14, 53)
  region = substr(creds[[1]][4], 11, 19)

  # Sign in to the account
  Sys.setenv("AWS_ACCESS_KEY_ID" = key,
             "AWS_SECRET_ACCESS_KEY" = skey,
             "AWS_DEFAULT_REGION" = region)

  # Put the file in the bucket
  bucket_name <- "cstdata-test"
  location_folder <- gsub(" ", "_", tolower(as.character(AOI$UNIT_NAME)))
  object <- file.path(location_folder, basename(outfile))
  put_folder(location_folder, bucket_name)
  put_object(file = outfile, object = object, bucket = bucket_name)

  # Example retrieval from bucket
  # aws.s3::save_object(object, bucket_name, file = "/home/travis/Desktop/test.nc")
}

