#' Wrapper for climateR get functions
#' Will gather data from any dataset, GCM, parameter, scenario, time resolution, and time period.
#' I suppose, we could also clip here, since the AOI argument only clips by the bounding box.
#'
#' Things to do:
#' 1) Create a simple park query, maybe with an id, or pattern recognition for names
#' 2) Translate the raster package nc functions directly into the base nc functions
#'     to be able to write more attributes
#' 3) Split the time range up into the most manageable chunks
#' 4) There is a raster package function called getData, so change the name of this


# This to get the data
getData <- function(AOI, method = "maca", param = "tmax", model = "CCSM4", scenario = "rcp85",
                    startDate = "1950-01-01", endDate = "2099-12-31", timeRes = "daily",
                    plotsample = TRUE){
  '
  AOI = AOI <- getAOI(state = "RI")
  parks <- rgdal::readOGR("data/shapefiles/nps_boundary.shp")
  AOI <- parks[grepl("Death Valley", parks$UNIT_NAME),]
  method = "maca"
  param = "tmax"
  model = "CCSM4"
  scenario = "rcp85"
  startDate = "2018-10-29"
  endDate = "NULL"
  timeRes = "daily"
  '
  require(aws.s3)
  require(climateR)
  require(leaflet)
  require(readtext)
  require(sf)
  require(sp)
  require(stringr)

  # Attributes
  pmeta = data.frame(climateR::param_meta[[method]])
  longname = pmeta$description[pmeta$common.name == param]
  varunit = pmeta$units[pmeta$common.name == param]

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

  # We can't retrieve the whole file at once, it seems
  date_range <- seq(as.Date(startDate), as.Date(endDate), "days")
  date_chunks <- split(date_range, ceiling(seq_along(date_range)/1826))

  # Iterate through each date chunk and save to file
  pb <- progress::progress_bar$new(total = length(date_chunks))
  for (dc in date_chunks){
    # Date portion range
    startd <- as.character(dc[[1]])
    endd <- as.character(tail(dc, n=1))

    # Get arguments for each method
    arguments <- c(AOI, method, param, model, scenario, startd, endd, timeRes)
    names(arguments) <- c("AOI", "method", "param", "model", "scenario", "startDate",
                          "endDate", "timeRes")
    argument_dictionary <- lapply(datasets, formalArgs)
  
    # Get arguments associated with the chosen method
    method_args <- argument_dictionary[method]
    args <- lapply(method_args, function(k) arguments[k])[[1]]
    if (args['endDate'] == 'NULL') args <- args[!grepl('endDate', names(args))]
    
    # Use these arguments for the method call
    data_full <- do.call(datasets[[method]], args = args)
    data <- raster::mask(data_full[[1]], AOI)
  
    # Create file name
    d1 <- gsub("-", "", startd)
    d2 <- gsub("-", "", endd)
    if (d2 == "NULL") {
      filename <- paste(c( method,model, scenario, param, d1), collapse = "_")
    } else {
      filename <- paste(c( method,model, scenario, param, d1, d2), collapse = "_")
    }
    filename <- paste(c(filename, 'nc'), collapse = ".")
    filename <- file.path("data", "rasters", "tests", filename)
  
    # Create a local file
    toNC(data, savepath = filename, method = method, model = model, param = param,
         location = "Death Valley National Park", scenario = scenario, naval = -9999)

    # Advance progress bar
    pb$tick()
  }


  # ############# Construction Zone ###############
  # # Push file to s3 #<---------------------------------------------------------------------------------------- wait until merging all date together?
  # # https://cran.r-project.org/web/packages/aws.s3/readme/README.html
  # # How to establish credentials without making them explicit?
  # creds <- readtext("~/../.aws/credentials.txt")[[2]]
  # creds = strsplit(creds, "\n")
  # key = substr(creds[[1]][2], 14, 33)
  # skey = substr(creds[[1]][3], 14, 53)
  # region = substr(creds[[1]][4], 11, 19)
  # 
  # # Something like this?
  # Sys.setenv("AWS_ACCESS_KEY_ID" = key,
  #            "AWS_SECRET_ACCESS_KEY" = skey,
  #            "AWS_DEFAULT_REGION" = region)
  # 
  # # Put the file in the bucket
  # bucket_name <- "cstdata-test"
  # aws.s3::put_object(filename, "test.nc", bucket_name)
  # 
  # # Retrieve from bucket
  # aws.s3::save_object("test.nc", bucket_name, file = "data/test.nc")
  # 
  # ############## Construction Zone ###############

  # Plot
  if (plotsample) {
    r <- raster::calc(data, fun = function(x) mean(x))
    title = paste("mean ", param[[1]], '<br>', '<br>', startDate, '<br>to<br>', endDate)
    pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), raster::values(r),
                        na.color = "transparent")
    plot <- leaflet(AOI) %>%
      addTiles() %>%
      addRasterImage(r, colors = pal, layerId = "raster") %>%
      leaflet.opacity::addOpacitySlider(layerId = "raster") %>%
      addPolygons(color = "#444444", weight = 5, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0,
                  highlightOptions = highlightOptions(color = "#0f7321", weight = 5,
                                                      bringToFront = TRUE)) %>%
      addLegend(pal = pal, values = raster::values(r), title = title)
    print(plot)
  }

  # Should work
  return(data)
}


# And this to check what doesn't work
checkData <- function(AOI, method = "maca", param = "tmax", model = "CCSM4", scenario = "rcp85",
                      startDate = "2018-10-29", endDate = NULL, timeRes = "daily", checks = NULL){
  '
  method = "terraclim"
  AOI = getAOI(state = "RI")
  param = "rhmin"
  scenario = "rcp85"
  model = "CCSM4"
  startDate = "2018-10-29"
  '
  if ( is.null(checks) ){
    checks <- data.frame(method = character(), parameter = character(), model = character(),
                         scenario = character(), success = numeric())
  }

  checks <- tryCatch({
    sink("file")
    data <- getData(AOI = AOI, method = method, param = param, model = model, scenario = scenario,
                    startDate = startDate, endDate = "NULL", timeRes = "daily", plotsample = FALSE)
    sink()
    rm(test)
    row <- data.frame(method = method, parameter = param, model = model,
                      scenario = scenario, success = 1)
    checks <- rbind(checks, row)
  },
  error = function(err){
    print(err)
    row <- data.frame(method = method, parameter = param, model = model,
                      scenario = scenario, success = 0)
    checks <- rbind(checks, row)
    return(checks)
  })
  return(checks)
}


