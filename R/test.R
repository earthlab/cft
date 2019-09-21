#' Just testing the getData function
library(aws.s3)
library(climateR)
source('R/getData.R')
source('R/makeNC.R')

# We want all dates, ultimately
parks <- rgdal::readOGR("data/shapefiles/nps_boundary.shp")
method = "maca"
param = "tmax"
model = "CCSM4"
scenario = "rcp85"
startDate = "1950-01-01"
endDate =  "2099-12-31"
timeRes = "daily"
plotsample = TRUE

# Sample dataset query and save to disk
print(parks$UNIT_NAME)
AOI <- getAOI(state = "CO")
AOI <- parks[grepl("Death", parks$UNIT_NAME),]
d <- getData(AOI,  method = method, param = param, model = model, scenario = scenario,
             startDate =startDate, endDate = endDate, timeRes = timeRes)
avg <- mean(raster::cellStats(d, stat='mean', na.rm=TRUE, asSample=TRUE))
f <- (avg - 273.15) * 9/5 + 32
f <- round(f, 2)
print(paste("Average max temperature :", f, "farenheit"))
