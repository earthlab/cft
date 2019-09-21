#'Apply same strategy as climateR, but just for MACA and with more control on our part.
#'
#' One of the reasons to split this out is to allow us to parallelize more efficiently.
#' He only ever parallelizes two data sources at once: historical and future.
library(magrittr)
library(RNetCDF)
library(glue)
source('R/parameters.R')
source("R/getSubset.R")

# Sample aoi
parks <- rgdal::readOGR("data/shapefiles/nps_boundary.shp")
aoi <- parks[grepl("Yellowstone National Park", parks$UNIT_NAME),]

getPark <- function(aoi, historical_years = "1950_2005", model_years = "2006_2099"){
  # Get historical dates
  h1 <- strsplit(historical_years, "_")[[1]][1]
  h2 <- strsplit(historical_years, "_")[[1]][2]
  m1 <- strsplit(model_years, "_")[[1]][1]
  m2 <- strsplit(model_years, "_")[[1]][2]
  hd1 <- as.Date(paste(c(h1, "01", "01"), collapse = "-"))
  hd2 <- as.Date(paste(c(h2, "12", "31"), collapse = "-"))
  md1 <- as.Date(paste(c(m1, "01", "01"), collapse = "-"))
  md2 <- as.Date(paste(c(m2, "12", "31"), collapse = "-"))
  historical_dates <- seq(hd1, hd2, "days")
  model_dates <- seq(md1, md2, "days")

  # Get the extent coordinates
  lonmin <- aoi@bbox[1,1]
  lonmax <- aoi@bbox[1,2]
  latmin <- aoi@bbox[2,1]
  latmax <- aoi@bbox[2,2]

  # Now use these from the lat/lons of the full grid to get index positions
  lonmindiffs <- abs(lons - lonmin)
  lonmaxdiffs <- abs(lons - lonmax)
  latmindiffs <- abs(lats - latmin)
  latmaxdiffs <- abs(lats - latmax)
  lon1 <- match(lonmindiffs[lonmindiffs == min(lonmindiffs)], lonmindiffs)
  lon2 <- match(lonmaxdiffs[lonmaxdiffs == min(lonmaxdiffs)], lonmaxdiffs)
  lat1 <- match(latmindiffs[latmindiffs == min(latmindiffs)], latmindiffs)
  lat2 <- match(latmaxdiffs[latmaxdiffs == min(latmaxdiffs)], latmaxdiffs)

  # Now we can build the urls, starting with history of course
  urlbase = "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_macav2metdata"
  hurls <- c()
  for (p in seq(length(params))){
    for (m in seq(length(models))){
      for (s in seq(length(scenarios))){
        attachment <- paste(c(base, params[p], models[m], ensembles[m], "historical",
                              historical_years, "CONUS_daily.nc?"), collapse = "_")
        var <- variables[p]
        query <- paste0(var, glue("[{0}:{1}:{ntime_hist}][{lat1}:{1}:{lat2}][{lon1}:{1}:{lon2}]"))
        url <- paste0(attachment, query)
        hurls <- append(hurls, url)
      }
    }
  }

  # And then the "future"
  furls = c()
  for (p in seq(length(params))){
    for (m in seq(length(models))){
      for (s in seq(length(scenarios))){
        attachment <- paste(c(base, params[p], models[m], ensembles[m], scenarios[s],
                              model_years, "CONUS_daily.nc?"), collapse = "_")
        var <- variables[p]
        query <- paste0(var, glue("[{0}:{1}:{ntime_future}][{lat1}:{1}:{lat2}][{lon1}:{1}:{lon2}]"))
        url <- paste0(attachment, query)
        furls <- append(furls, url)
      }
    }
  }

  # Now, let's parallelize a little more than climateR does
  `%dopar%` <- foreach::`%dopar%`
  ncores <- parallel::detectCores() - 1
  doParallel::registerDoParallel(ncores)

  # Now I want them in groups of ncores
  hurls2 <- split(hurls, ceiling(seq_along(hurls)/ncores))
  furls2 <- split(furls, ceiling(seq_along(furls)/ncores))

  # Okay, now we need a function that can take the results of these and save to file in s3 (check if these need to be transposed).
  for (chunk in seq(length(hurls2))){
    foreach::foreach(i = 1:length(hurls2[[chunk]])) %dopar% {
      hurl <- hurls2[[chunk]][i]
      furl <- furls2[[chunk]][i]
      tryCatch({
        # Try to download and stack the subsets
        hbrick <- getSubset(hurl, aoi, historical_dates)
        mbrick <- getSubset(furl, aoi, model_dates)
        dates <- c(historical_dates, model_dates)
        bricks <- list(hbrick, mbrick)
        names(bricks) <- dates
        brick <- raster::stack(bricks)  # <------------------------------------------------------- This is very slow, I see why he did what he did.

      }, error = function(e){
        # Wait a few seconds and try again
        Sys.sleep(3)
      })

    }
  }
}
