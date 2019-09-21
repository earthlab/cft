  #' Reticulating Python's Xarray and netCDF4 packages to do the job.
  #' It might not be pure R code, but it is much more stable and faster than anything
  #' I could manage in R.
  #'
  #' This strategy may also enable us to use Zarr arrays, which are accessible directly
  #' from an s3 bucket, according to Will.
  #'
  #' We are using this catalog:
  #' http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_CMIP5_aggregated_macav2_catalog.html
  #'
  #' There is also the non-aggregated catalog:
  #' http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_CMIP5_macav2_catalog2.html
  #' These files come in 5 year chunks but have windspeed (was) and potential evapotranspiration (PotEvap)
  #' in addition to the other variables. This could be done with url list files from here:
  #' https://climate.northwestknowledge.net/MACA/data_portal.php
  #'
  #' We also have mean vapor pressure deficit (vpd) in both data sets.

  library(aws.s3)
  library(glue)
  library(progress)
  library(reticulate)
  source('R/parameters.R')  # imports arguments (url pieces)
  use_condaenv("dict")
  xr <- import("xarray")
  nc <- import("netCDF4")

  # Sample aoi
  parks <- rgdal::readOGR("data/shapefiles/nps_boundary.shp")
  aoi <- parks[grepl("Yellowstone National Park", parks$UNIT_NAME),]

  getParkpy <- function(aoi, historical_years = "1950_2005", model_years = "2006_2099"){  # <- I think it would be best to require a path to a shapefile to get the aoi, though that would require the park managers to subset their own files.
    # First we need a destination folder
    location <- gsub(" ", "_", tolower(aoi$UNIT_NAME))  # <----------------------------------- This won't be consistent
    print(paste("Retrieving climate data for", location))
    dst_folder <- file.path('data', 'netcdfs', location) # <---------------------------------- Could, optionally, be a temp file
    if (!dir.exists(dst_folder)) dir.create(dst_folder, recursive = TRUE)

    # Now we can get the historical and model years together
    urlbase = "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_macav2metdata"
    hurls <- c()
    for (model in models){
      params <- arguments[[model]]$parameters
      scenarios <- arguments[[model]]$scenarios
      ensemble <- arguments[[model]]$ensemble
      for (param in params){
        for (scenario in scenarios){  # No scenarios, but we need repeats when pairing with modeled data sets
          attachment <- paste(c(urlbase, param, model, ensemble, "historical",
                                historical_years, "CONUS_daily.nc?"), collapse = "_")
          var <- variables[param]
          query <- paste0(var, glue("[{0}:{1}:{ntime_hist}][{lat1}:{1}:{lat2}][{lon1}:{1}:{lon2}]"))
          url <- paste0(attachment, query)
          hurls <- append(hurls, url)
        }
      }
    }

    # And then the "future"
    murls = c()
    for (model in models){
      params <- arguments[[model]]$parameters
      scenarios <- arguments[[model]]$scenarios
      ensemble <- arguments[[model]]$ensemble
      for (param in params){
        for (scenario in scenarios){  # No scenarios, but we need repeats when pairing with modeled data sets
          attachment <- paste(c(urlbase, param, model, ensemble, scenario,
                                model_years, "CONUS_daily.nc?"), collapse = "_")
          var <- variables[param]
          query <- paste0(var, glue("[{0}:{1}:{ntime_hist}][{lat1}:{1}:{lat2}][{lon1}:{1}:{lon2}]"))
          url <- paste0(attachment, query)
          murls <- append(murls, url)
        }
      }
    }

    # Let's have the historical and modeled urls in pairs
    purls <- lapply(1:length(hurls), function(i) c(hurls[i], murls[i]))

    # # If we want to parallelize, we could group the pairs further
    # `%dopar%` <- foreach::`%dopar%`
    # ncores <- parallel::detectCores() - 1
    # doParallel::registerDoParallel(ncores)
    # purls2 <- split(purls, ceiling(seq_along(purls)/ncores))
    # # Finish getURL to accept this

    # Let's base the grid (used for subset queries) of a sample file
    grid =

    # Get the extent coordinates  # <--------------------------------------------------------- Here we could distinguish between simple extent coordinates and a spatial object
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

    # Let's leave parallelization aside for now, merge and download each pair
    pb <- progress_bar$new(total = length(purls))
    pb$tick(0)
    for (purl in purls){
      # Get the local destination file
      content <- purl[2]  # 2 for the modeled set, it has more information
      query <- basename(content)
      cutoff <- regexpr("CONUS", query)[1] - 12  # to get to the scenario
      file_name <- paste0(substr(query, 1, cutoff), ".nc")
      dst <-file.path(dst_folder, file_name)
      pb$tick()

      if (!file.exists(dst)) {
        # Save a local file - (replace with getURL for more options)
        ds <- xr$open_mfdataset(purl, concat_dim="time")
        ds$to_netcdf(dst) # <---------------------------------------------------------------------------- Local storage could be optional, alternatively we could save to a single temp file and send that to the bucket

        # Send to s3 bucket ...  # <--------------------------------------------------------------------- How to make this as easy as possible?
        creds <- readRDS("~/.aws/credentials.RDS")
        Sys.setenv("AWS_ACCESS_KEY_ID" = creds['key'],
                   "AWS_SECRET_ACCESS_KEY" = creds['skey'],
                   "AWS_DEFAULT_REGION" = creds['region'])

        # Put the file in the bucket
        bucket_name <- "cstdata-test"
        object <- file.path(location, file_name)
        put_folder(location, bucket_name)
        put_object(file = dst, object = object, bucket = bucket_name)
      }
    }
  }


#   getURL <- function(purl, dst){
#       # Option 1: xarray
#       ds <- xr$open_mfdataset(purl)
#       ds$to_netcdf(dst)
#
#       # Option #2: netCDF4 (less abstraction, may be more consistent, gives more control over file structure...might also be slower)
#       # ds <- nc$MFDataset(purl, aggdim='time')
#
#       # # Now save to file using
#       # nco = nc$Dataset(dst, 'w')
#       # nco$createDimension('time', ds$dimensions$time$dimtotlen)
#       # nco$createDimension('lat', ds$dimensions$lat$size)
#       # nco$createDimension('lon', ds$dimensions$lon$size)
#
#       # # So far so good, but...
#       # lats = nco$createVariable('lats', 'd', ('lat'))
#       # lons = nco$createVariable('lons', 'd', ('lon'))
#       # var = nco$createVariable('huss', 'd', c('time', 'lat', 'lon'), zlib=TRUE)
#
#       # Below has not been translated yet. Indexing is different.
#       # lats[] = ds['lat']
#       # lons[] = ds['lon'][lonidx1:lonidx2]
#       # var[] = ?
#       # nco.close()
#   }
