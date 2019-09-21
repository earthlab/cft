getScenario2 <- function(AOI, method = "maca", param = "tmax",
                         model = "CCSM4", scenario = "rcp85", startDate = "1950-01-01",
                         endDate = "2099-12-31", timeRes = "daily", year_range = 5,
                         plotsample = TRUE){
  '
  parks <- rgdal::readOGR("data/shapefiles/nps_boundary.shp")
  AOI <- parks[grepl("Death Valley", parks$UNIT_NAME),]
  method = "maca"
  param = "tmin"
  model = "CCSM4"
  scenario = "rcp85"
  startDate = "2018-10-29"
  endDate = "NULL"
  timeRes = "daily"
  year_range = 5
  '
  require(aws.s3)
  require(climateR)
  require(leaflet)
  require(sf)
  require(sp)
  require(stringr)
  source("R/parScenario.R")

  # Start the timer
  start <- Sys.time()

  # Attributes
  pmeta <- data.frame(climateR::param_meta[[method]])
  longname <- pmeta$description[pmeta$common.name == param]
  varunit <- pmeta$units[pmeta$common.name == param]
  location <- as.character(AOI$UNIT_NAME)
  locale <- gsub(" ", "_", tolower(location))

  # AOI can come in many ways
  AOI <- tryCatch({
    AOI <- sf:::as_Spatial(AOI)
    },
    error = function(err){
        return(AOI)
    })

  # Get download methods for each data set
  datasets <- ls("package:climateR")
  datasets <- datasets[grepl('get', datasets)]
  cnames <- gsub("get", "", datasets)
  cnames <- lapply(cnames, FUN = function(x) tolower(x))
  names(datasets) <- cnames
  if (is.null(endDate)) endDate <- 'NULL'

  # We can't retrieve the whole file at once, it breaks (it breaks a lot anyway)
  days <- year_range * 365 + 1
  date_range <- seq(as.Date(startDate), as.Date(endDate), "days")
  date_chunks <- split(date_range, ceiling(seq_along(date_range)/days))

  # Each cpu should be able to handle one of these date chunks at a time
  ncpu <- detectCores() - 1
  core_chunks <- split(date_chunks, ceiling(seq_along(date_chunks)/ncpu))

  # Iterate through each date chunk and save to file
  pb <- progress::progress_bar$new(total = length(date_chunks))
  pb$tick(0)
  cl <- makeCluster(ncpu)
  clusterEvalQ(cl, library(climateR))
  for (cc in core_chunks){
    # Is this really faster? I don't think its working
    parScenario(AOI, method, param, model, scenario, timeRes, location, cc, cl)

    # Advance progress bar
    pb$tick()
  }

  # Don't need this anymore
  stopCluster(cl)

  # End timer
  end <- Sys.time()
  print(paste(round((end - start), 2), "minutes"))

  # Should work
  return(folder)
}
