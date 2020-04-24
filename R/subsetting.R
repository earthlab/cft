filter_years <- function(start_year = 1950, end_year = 2099,
                         available_start = 1950, available_end = 2099) {
  
  # Turn these into strings, then dates
  available1 <- as.Date(glue::glue("{available_start}-01-01"))
  available2 <- as.Date(glue::glue("{available_end}-12-31"))
  date1 <- as.Date(glue::glue("{start_year}-01-01"))
  date2 <- as.Date(glue::glue("{end_year}-12-31"))
  
  # Check that the end year is after the start year
  if (date2 < date1) stop("end_year is set before start_year")
  
  # Check that the requested years are available
  if (date1 < available1) {
    stop(paste0("start_year is unavailable. Please choose a year between ",
                available_start, " and ", available_end, "."))
  }
  if (date2 > available2) {
    stop(paste0("end_year is unavailable. Please choose a year between ",
                available_start, " and ", available_end, "."))
  }
  
  # Return the difference in days between the first available and chosen dates
  day1 <- as.integer(date1 - available1)
  day2 <- as.integer(date2 - available1)
  
  # Return these as a range
  return(seq(day1, day2))
}


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
  index_pos <- get_aoi_indexes(aoi, grid_ref)
  y1 <- index_pos[["y1"]]
  y2 <- index_pos[["y2"]]
  x1 <- index_pos[["x1"]]
  x2 <- index_pos[["x2"]]
  
  # Get list of lats and lons
  res <- grid_ref$resolution
  ny <- (y2 - y1)
  nx <- (x2 - x1)
  latmin <- aoi@bbox[2, 1]
  lonmin <- aoi@bbox[1, 1]
  aoilats <- latmin + (0:ny) * res
  aoilons <- lonmin + (0:nx) * res

  # Now create a mask as a matrix
  r <- raster::raster(ncols = length(aoilons), nrows = length(aoilats))
  raster::extent(r) <- raster::extent(aoi)
  r1 <- raster::rasterize(aoi, r)
  r2 <- raster::rasterize(methods::as(aoi, "SpatialLines"), r)
  r <- raster::cover(r1, r2)
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
                             as.character(end_year), "daily.nc"),
                           collapse = "_")
        
        # Build remote historical and future file names
        historical <- paste(c(urlbase, param, model, ensemble, "historical",
                              "1950_2005", "CONUS_daily.nc"), collapse = "_")
        future <- paste(c(urlbase, param, model, ensemble, rcp,
                          "2006_2099", "CONUS_daily.nc"), collapse = "_")
        
        # Build the temporal and spatial subsets
        historical_subset <- glue::glue(paste0("?{var}[{0}:{1}:{ntime_hist}]",
                                               "[{y1}:{1}:{y2}][{x1}:{1}:{x2}]",
                                               "#fillmismatch"))
        future_subset <- glue::glue(paste0("?{var}[{0}:{1}:{ntime_model}]",
                                           "[{y1}:{1}:{y2}][{x1}:{1}:{x2}]",
                                           "#fillmismatch"))

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
        historical_url <- paste0(historical, historical_subset)
        future_url <- paste0(future, future_subset)
        paired_url <- c(historical_url, future_url)
        queries[[length(queries) + 1]] <- list(paired_url, file_name, elements)
      }
    }
  }
  
  # Done.
  return(queries)
}


retrieve_subset <- function(query, years, aoi_info, area_name, local_dir) {

  # Load python dependencies
  reticulate::use_condaenv('cft', required = TRUE) 
  xr <- reticulate::import("xarray")
  np <- reticulate::import("numpy", convert = FALSE)
  
  # Split year range up
  start_year <- years[1]
  end_year <- years[2]
  
  # Unpack aoi info
  aoilats <- aoi_info[["aoilats"]]
  aoilons <- aoi_info[["aoilons"]]
  mask_matrix <- aoi_info[["mask_matrix"]]
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

    # Save a local file
    ds <- xr$open_mfdataset(url_pair, concat_dim = "time", combine = "nested")

    # Filter dates
    days <- filter_years(start_year, end_year)
    ds <- ds$sel(time = c(days[[1]]: days[[length(days)]]))

    # Create coordinate vectors
    lats <- np$asarray(c(as.matrix(aoilats)), dtype = np$float32)
    lons <- np$asarray(c(as.matrix(aoilons)), dtype = np$float32)
    times <- np$asarray(days, np$int32)
    
    # For a single point
    if (length(lats) == 1) lats = c(lats)
    if (length(lons) == 1) lons = c(lons)
    
    # Assign coordinates
    ds <- ds$assign_coords(lon = lons,
                           lat = lats,
                           time = times)
    ds <- ds$sortby('lat', ascending=FALSE)

    # Mask by boundary
    dsmask <- xr$DataArray(mask_matrix)
    dsmask <- dsmask$fillna(0)
    ds <- ds$where(dsmask$data == 1)

    # Update Attributes
    summary <- paste0(
      "This archive contains daily downscaled meteorological and hydrological ",
      "projections for the Conterminous United States at 1/24-deg resolution ",
      "utilizing the Multivariate Adaptive Constructed Analogs (MACA, ",
      "Abatzoglou, 2012) statistical downscaling method with the METDATA ",
      "(Abatzoglou,2013) training dataset. The downscaled meteorological ",
      "variables are maximum/minimum temperature(tasmax/tasmin), ",
      "maximum/minimum relative humidity(rhsmax/rhsmin) precipitation ",
      "amount(pr), downward shortwave solar radiation(rsds), eastward ",
      "wind(uas), northward wind(vas), and specific humidity(huss). The ",
      "downscaling is based on the 365-day model outputs from different ",
      "global climate models (GCMs) from Phase 5 of the Coupled Model ",
      "Inter-comparison Project (CMIP3) utlizing the historical (1950-2005) ",
      "and future RCP4.5/8.5(2006-2099) scenarios. Leap days have been added ",
      "to the dataset from the average values between Feb 28 and Mar 1 in ",
      "order to aid modellers. Daily vapor pressure deficit is calculate from ",
      "downscaled daily minimum and maximum temperatures for saturated vapor ",
      "pressure and daily dew point temperature for actual vapor pressure. ",
      "The dew point temperature is estimated by converting downscaled daily ",
      "mean specific humidity to partial pressure of moisture in the ",
      "atmosphere and estimating pressure from elevation using the barometric ",
      "formula.")
    attrs1 <- ds$attrs

    # Vapor pressure is missing title information
    if ( is.null(attrs1$title) ) {
      attrs1$title <- paste("Downscaled daily meteorological data of",
                            elements["parameter"], "from", elements["model"],
                            "using the run", elements["ensemble"])
    }
    attrs2 <- list(
      "title" = attrs1$title,
      "author" = "John Abatzoglou-University of Idaho, jabatzoglou@uidaho.edu",
      "comment" = paste0("Subsetted to ", area_name, "by the North Central ",
                         "Climate Adaption Science Center"),
      "summary" = summary,
      "ensemble" = unname(elements["ensemble"]),
      "model" = unname(elements["model"]),
      "rcp" = unname(elements["rcp"]),
      "coordinate_system" = attrs1$coordinate_system,
      "geospatial_lat_min" = min(aoilats),
      "geospatial_lat_max" = max(aoilats),
      "geospatial_lon_min" = min(aoilons),
      "geospatial_lon_max" = max(aoilons),
      "geospatial_lat_units" = attrs1$geospatial_lat_units,
      "geospatial_lon_units" = attrs1$geospatial_lon_units,
      "geospatial_lat_resolution" = resolution,
      "geospatial_lon_resolution" = resolution,
      "geospatial_vertical_min" = "0",
      "geospatial_vertical_min" = "0",
      "geospatial_vertical_resolution" = "0",
      "geospatial_vertical_positive" = "0",
      "time_coverage_start" = glue::glue("{start_year}-01-01T00:0"),
      "time_coverage_end" = glue::glue("{end_year}-12-31T00:0"),
      "time_coverage_duration" = glue::glue("P{end_year - start_year + 1}Y"),
      "time_coverage_resolution" = "P1D",
      "time_units" = "days since 1950-01-01"
    )
    ds$attrs <- attrs2

    # Fix the variable attributes (why is grid_mapping a variable attribute?)
    internal_varname <- elements[["internal_varname"]]
    var_attrs <- ds[internal_varname]$attrs
    if ("grid_mapping" %in% names(var_attrs)) {
      var_attrs["grid_mapping"] <- NULL
    }

    # Assign attributes - reticulate doesn't handle this well
    x <- tryCatch({
      ds[internal_varname]$attrs <- var_attrs
    }, error=function(e){})
  
    x <- tryCatch({
      ds$time$attrs <- list("long_name" = "time", "units" = "days since 1950-01-01", "calendar" = "gregorian")
    }, error=function(e){})

    x <- tryCatch({
      ds$lon$attrs <- list("long_name" = "longitude" , "units" = "degrees_east", "standard_name" = "longitude")
    }, error=function(e){})

    x <- tryCatch({
      ds$lat$attrs = list("long_name" = "latitude" , "units" = "degrees_north", "standard_name" = "latitude")
    }, error=function(e){})

    # Save to local file
    ds$to_netcdf(destination_file)
  }

  # Keep track of file information
  file_dir <- normalizePath(local_dir)
  file_name <- basename(destination_file)
  file_path <- file.path(file_dir, file_name)
  reference <- c(list("local_file" = file_name, "local_path" = file_path), 
                 elements)
  return(reference)
}
