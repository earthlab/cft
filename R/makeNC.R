#' Raster object to netcdf4 file
library(aws.s3)
library(climateR)
library(ncdf4)
library(raster)
source('R/getData.R')



# netcdf method
toNC <- function(rasterbrick, savepath='data/rasters/tests/temp3.nc', method = "maca",
                 model = "CCSM4", param = "tmax", location = "Death Valley National Park",
                 scenario = "rcp85", naval = -9999){
  '
  parks <- rgdal::readOGR("data/shapefiles/nps_boundary.shp")
  method = "maca"
  param = "tmax"
  model = "CCSM4"
  scenario = "rcp85"
  startDate = "2019-08-14"
  endDate =  "2020-08-14"
  timeRes = "daily"
  plotsample = TRUE
  savepath="data/rasters/tests/temp.nc"

  # Sample dataset query
  AOI <- parks[grepl("Death", parks$UNIT_NAME),]
  rasterbrick <- getData(AOI,  method = method, param = param, model = model,
                         scenario = scenario, startDate =startDate,
                         endDate = endDate, timeRes = timeRes)
  '
  # Make sure the target folder exists
  if (!file.exists('data/rasters/tests/')){
    dir.create(file.path('data', 'rasters', 'tests'), recursive = TRUE,
               showWarnings = FALSE)
  }

  # Extract coordinates from rasterbrick
  r <- rasterbrick
  r[is.na(r)] <- naval
  lons <- unique(values(init(r, "x")))
  lats <- unique(values(init(r, "y")))
  nlon <- length(lons)
  nlat <- length(lats)

  # Extract dates
  date_strings <- gsub("X", "", names(r))
  dates <- lapply(date_strings, function(x) as.Date(x, format = "%Y.%m.%d"))
  base_date = as.Date("1950-01-01")
  days <- unlist(lapply(dates, function(x) as.integer(x - base_date)))

  # Extract data arrays
  data <- as.matrix(r)

  # Extract attributes
  paramdf <- data.frame(param_meta[[method]])
  modeldf <- data.frame(model_meta[[method]])
  units <- paramdf$units[paramdf$common.name == param]
  longname <- paramdf$description[paramdf$common.name == param]
  model_agency <- modeldf$agency[modeldf$model == model]

  # Create dimensions
  dlon <- ncdim_def('longitude', 'degrees_east', lons)
  dlat <- ncdim_def('latitude', 'degrees_north', lats)
  dtime <- ncdim_def('time', 'days since 1950-01-01', days, unlim = TRUE,
                     longname = "Days since 1950-01-01")

  # Create variables
  varout <- ncvar_def(name = param, units = units, dim = list(dlon, dlat, dtime),
                     longname = longname, missval = naval, compression = 9)

  # Add variables to the file
  ncout <- nc_create(savepath, varout, force_v4 = FALSE, verbose = FALSE)

  # Add attributes to the file
  ncatt_put(ncout, 0, "Title", "Title describing parameters here")
  ncatt_put(ncout, 0, "Location", location)
  ncatt_put(ncout, 0, "GCM_Model", paste0(model, ": ", model_agency))
  ncatt_put(ncout, 0, "Downscaling_Method", method)
  ncatt_put(ncout, 0, "Climate_Scenario", scenario)
  ncatt_put(ncout, 0, "Created_On", as.character(Sys.time()))
  ncatt_put(ncout, 0, "References", "References and acknowledgements here?")

  # Finally, write the file
  ncvar_put(ncout, varout, data)
  nc_close(ncout)

  # Let the user know where to find their object
  print(paste("File saved to:",  savepath))
}



# toNC(rasterbrick, savepath = 'data/rasters/tests/temp3.nc', method = "maca",
#      model = "CCSM4", param = "tmax", location = "Death Valley National Park",
#      scenario = "rcp85", naval = -9999)
