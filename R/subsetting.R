
get_aoi_indexes <- function(aoi, grid_ref) {
  
  # Extract the bounding box of the area of interest
  lonmin <- aoi@bbox[1, 1]
  lonmax <- aoi@bbox[1, 2]
  latmin <- aoi@bbox[2, 1]
  latmax <- aoi@bbox[2, 2]
  
  # Calculate differences between bounding box and target grid coordinates
  lonmindiffs <- abs(grid_ref$lons - lonmin)
  lonmaxdiffs <- abs(grid_ref$lons - lonmax)
  latmindiffs <- abs(grid_ref$lats - latmin)
  latmaxdiffs <- abs(grid_ref$lats - latmax)
  
  # Find the index positions of the closest grid coordinates to the aoi extent
  x1 <- match(lonmindiffs[lonmindiffs == min(lonmindiffs)], lonmindiffs)
  x2 <- match(lonmaxdiffs[lonmaxdiffs == min(lonmaxdiffs)], lonmaxdiffs)
  y1 <- match(latmindiffs[latmindiffs == min(latmindiffs)], latmindiffs)
  y2 <- match(latmaxdiffs[latmaxdiffs == min(latmaxdiffs)], latmaxdiffs)
  
  # Create a list with each required grid index position
  index_pos <- list("y1" = y1, "y2" = y2, "x1" = x1, "x2" = x2)
  
  # Return list  
  return(index_pos)
}


get_aoi_info <- function(aoi, grid_ref) {
  # Get relative index positions to full grid
  # slight buffering of extent allows us to handle points
  if (class(aoi) == "SpatialPointsDataFrame") {
    orig_crs <- raster::projection(aoi)
    # buffering is only possible in projected coordinate systems
    proj_aoi <- sp::spTransform(aoi, sp::CRS("+init=epsg:5070"))
    extended_aoi <- rgeos::gBuffer(proj_aoi, width=.1)
    extended_aoi <- sp::spTransform(extended_aoi, orig_crs)
  } else {
    extended_aoi = aoi
  }
  index_pos <- get_aoi_indexes(extended_aoi, grid_ref)
  y1 <- index_pos[["y1"]]
  y2 <- index_pos[["y2"]]
  x1 <- index_pos[["x1"]]
  x2 <- index_pos[["x2"]]
  
  # Get list of lats and lons
  res <- grid_ref$resolution
  ny <- (y2 - y1)
  nx <- (x2 - x1)
  latmin <- extended_aoi@bbox[2, 1]
  lonmin <- extended_aoi@bbox[1, 1]
  aoilats <- latmin + (0:ny) * res
  aoilons <- lonmin + (0:nx) * res

  # Now create a mask as a matrix
  r <- raster::raster(ncols = length(aoilons), nrows = length(aoilats))
  raster::extent(r) <- raster::extent(extended_aoi)
  r <- raster::rasterize(aoi, r, field = 1)
  mask_grid <- r * 0 + 1
  mask_matrix <- methods::as(mask_grid, "matrix")
  
  # Package all of this into one object
  aoi_info <- list("aoilats" = aoilats,
                   "aoilons" = aoilons,
                   "mask_matrix" = mask_matrix,
                   "resolution" = res)
  
  # Done.
  return(aoi_info)
}


get_queries <- function(aoi, area_name, years, models, parameters, scenarios,
                        arg_ref, grid_ref) {
  
  # We are building url queries from this base
  urlbase <- paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/",
                    "agg_macav2metdata")
  
  # Split year range up
  start_year <- years[1]
  end_year <- years[2]
  
  # Get time information from our grid reference
  ntime_hist <- grid_ref$ntime_historical
  ntime_model <- grid_ref$ntime_model
  
  # Get relative index positions to full grid
  index_pos <- get_aoi_indexes(aoi, grid_ref)
  y1 <- index_pos[["y1"]]
  y2 <- index_pos[["y2"]]
  x1 <- index_pos[["x1"]]
  x2 <- index_pos[["x2"]]
  
  # Get relative time indices for query
  # if necessary, generate historical and future queries
  hist_dates_avail <- seq.Date(as.Date("1950-01-01"), 
                               as.Date("2005-12-31"), 
                               by = 1)
  future_dates_avail <- seq.Date(as.Date("2006-01-01"), 
                                 as.Date("2099-12-31"), 
                                 by = 1)
  dates_requested <- seq.Date(as.Date(paste0(start_year, "-01-01")), 
                              as.Date(paste0(end_year, "-12-31")), 
                              by = 1)
  is_historical <- any(years < 2006)
  historical_time_indices <- which(hist_dates_avail %in% dates_requested)

  is_future <- any(years > 2005)
  future_time_indices <- which(future_dates_avail %in% dates_requested)

  # Build a list of lists with historical/future model queries and a file name
  queries <- list()
  for (model in models) {
    
    # Available arguments for this model
    args <- arg_ref$get_args(model)
    avail_params <- args$parameters
    avail_scenarios <- args$scenarios

    # Available requested arguments
    params <- lapply(parameters, FUN = function(x) if (x %in% avail_params) x)
    rcps <- lapply(scenarios, FUN = function(x) if (x %in% avail_scenarios) x)
    params <- params[params %in% avail_params]
    rcps <- rcps[rcps %in% avail_scenarios]

    # Variable reference
    variables <- arg_ref$variables
    
    # Only one model run for each
    ensemble <- args$ensemble
    
    # Loop through all of the available arguments and build queries
    for (param in params) {
      for (rcp in rcps) {
        
        # Get internal variable name
        var <- variables[param]
        
        # Build local file name
        file_name <- paste(c(param, area_name, model, ensemble, rcp,
                             "macav2metdata", as.character(start_year),
                             as.character(end_year), "daily.tif"),
                           collapse = "_")
        
        # Build the temporal and spatial subsets
        if (is_historical) {
          historical <- paste(c(urlbase, param, model, ensemble, "historical",
                                "1950_2005", "CONUS_daily.nc"), collapse = "_")
          historical_subset <- glue::glue(paste0("?{var}[{min(historical_time_indices)-1}:1:{max(historical_time_indices)-1}]",
                                                 "[{y1}:1:{y2}][{x1}:1:{x2}]",
                                                 "#fillmismatch"))
          historical_url <- paste0(historical, historical_subset)
        } else {
          historical_url <- NA
        }
        
        if (is_future) {
          future <- paste(c(urlbase, param, model, ensemble, rcp,
                            "2006_2099", "CONUS_daily.nc"), collapse = "_")
          future_subset <- glue::glue(paste0("?{var}[{min(future_time_indices)-1}:1:{max(future_time_indices)-1}]",
                                             "[{y1}:1:{y2}][{x1}:1:{x2}]",
                                             "#fillmismatch"))
          future_url <- paste0(future, future_subset)
        } else {
          future_url <- NA
        }

        # For further reference, create a vector of data set elements
        elements <- c("model" = model,
                      "parameter" = param,
                      "rcp" = rcp,
                      "ensemble" = ensemble,
                      "year1" = as.numeric(years[1]),
                      "year2" = as.numeric(years[2]),
                      "area_name" = area_name,
                      "units" = unname(arg_ref$units[unlist(var)]),
                      "full_varname" = unname(arg_ref$labels[unlist(param)]),
                      "internal_varname" = unname(var))
  
        # Combine everything into a query package and add to query list
        paired_url <- c(historical_url, future_url)
        queries[[length(queries) + 1]] <- list(
          paired_url = paired_url, 
          file_name = file_name, 
          elements = elements,
          dates = list(historical = hist_dates_avail[historical_time_indices],
                       future = future_dates_avail[future_time_indices]))
      }
    }
  }
  return(queries)
}


retrieve_subset <- function(query, years, aoi_info, area_name, local_dir) {
  start_year <- years[1]
  end_year <- years[2]
  
  # Unpack aoi info
  aoilats <- aoi_info[["aoilats"]]
  aoilons <- aoi_info[["aoilons"]]
  mask_matrix <- t(aoi_info[["mask_matrix"]])
  resolution <- aoi_info[["resolution"]]

  dir.create(local_dir, recursive = TRUE, showWarnings = FALSE)
  file_name <- query[[2]]
  store_name <- query[[2]]
  destination_file <- file.path(local_dir, file_name)

  # Get data set attributes
  elements <- query[[3]]

  # Retrieve subset and save file locally
  if (!file.exists(destination_file)) {

    # Get the combined historical and modeled url query
    url_pair <- query[[1]]
    
    # generate raster data for queries and stack it together
    any_historical <- !is.na(url_pair[1])
    if (any_historical) {
      nc <- RNetCDF::open.nc(url_pair[1])
      historical_array <- RNetCDF::read.nc(nc)[[1]]
      RNetCDF::close.nc(nc)
    } else {
      historical_array <- NA
    }
    
    any_future <- !is.na(url_pair[2])
    if (any_future) {
      nc <- RNetCDF::open.nc(url_pair[2])
      future_array <- RNetCDF::read.nc(nc)[[1]]
      RNetCDF::close.nc(nc)
    } else {
      future_array <- NA
    }
    
    ndate <- length(unlist(query$dates))
    if (any_historical & any_future) {
      stopifnot(dim(historical_array)[1:2] == dim(future_array)[1:2])
      stopifnot(ndate == (dim(historical_array)[3] + dim(future_array)[3]))
      final_array <- array(c(historical_array, future_array), 
                         dim = c(dim(future_array)[1:2], 
                                 ndate))
    }
    if (any_historical & !any_future) {
      final_array <- historical_array
    }
    if (!any_historical & any_future) {
      final_array <- future_array
    } 

    # the following is necessary to retain 1-pixel dimensionality
    # otherwise these dims won't be represented
    expected_dims <- c(length(aoilons), 
                       length(aoilats), 
                       ndate)
    if (length(dim(final_array)) < 3) {
      final_array <- array(c(final_array), dim = expected_dims)
    }
    stopifnot(all(dim(final_array) == expected_dims))
    
    for (t in 1:ndate) {
      # one dim needs to be reversed
      final_array[, , t] <- final_array[, dim(final_array)[2]:1, t]
      final_array[, , t][is.na(mask_matrix)] <- NA 
    }

    r <- raster::t(raster::brick(final_array, 
                                 # note x and y are transposed
                                 xmn = min(aoilats) - resolution/2, 
                                 xmx = max(aoilats) + resolution/2, 
                                 ymn = min(aoilons) - resolution/2, 
                                 ymx = max(aoilons) + resolution/2, 
                                 crs = "+init=epsg:4326"))
    raster::writeRaster(r, destination_file)
  }

  # Keep track of file information
  file_dir <- normalizePath(local_dir)
  file_name <- basename(destination_file)
  file_path <- file.path(file_dir, file_name)
  reference <- c(list("local_file" = file_name, "local_path" = file_path), 
                 elements)
  return(reference)
}
