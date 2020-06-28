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


get_aoi_info <- function(aoi, project_dir, area_name, grid_ref) {
  
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
  mask_path <- file.path(project_dir, "rasters", paste0(area_name, ".tif"))
  dir.create(dirname(mask_path), showWarnings = FALSE)
  if ( !file.exists(mask_path) ) {
    # Rasterize
    r <- raster::raster(ncols = length(aoilons), nrows = length(aoilats))
    raster::extent(r) <- raster::extent(aoi)
    r1 <- raster::rasterize(aoi, r)
    r2 <- raster::rasterize(methods::as(aoi, "SpatialLines"), r)
    r <- raster::cover(r1, r2)
    
    # Create binary mask
    mask_grid <- r * 0 + 1
    
    # Write as raster for later
    raster::dataType(mask_grid) <- "INT1U"
    raster::writeRaster(mask_grid, mask_path, "GTiff")
    
  } else {
    # Read mask as raster
    mask_grid <- raster::raster(mask_path)
  }
  
  # Convert to a matrix
  mask_matrix <- methods::as(mask_grid, "matrix")
  
  # Package all of this into one object
  aoi_info <- list("aoilats" = aoilats,
                   "aoilons" = aoilons,
                   "mask_matrix" = mask_matrix,
                   "resolution" = res)

  # Done.
  return(aoi_info)
}


retrieve_subset <- function(query, years, aoi_info, area_name, area_dir, arg_ref) {
  
  # Load python dependencies
  reticulate::use_condaenv("cft", required = TRUE) 
  xr <- reticulate::import("xarray")
  np <- reticulate::import("numpy", convert = FALSE)
  
  # Which dataset are we working with?
  dataset <- class(arg_ref)[[1]]
  
  # Split year range up
  start_year <- years[1]
  end_year <- years[2]
  
  # Unpack aoi info
  aoilats <- aoi_info[["aoilats"]]
  aoilons <- aoi_info[["aoilons"]]
  mask_matrix <- aoi_info[["mask_matrix"]]
  resolution <- aoi_info[["resolution"]]
  
  # Put path information together
  dir.create(area_dir, recursive = TRUE, showWarnings = FALSE)
  file_name <- query[[2]]
  store_name <- query[[2]]
  destination_file <- file.path(area_dir, file_name)
  
  # Get data set attributes
  elements <- query[[3]]
  param <- elements$parameter
  time_dim <- arg_ref$time_dim
  
  # Retrieve subset and save file locally
  if ( !file.exists(destination_file) ) {
    
    # Get the combined historical and modeled url query
    urls <- query[[1]]
    
    # Save a local file
    ds <- xr$open_mfdataset(urls, concat_dim = time_dim, combine = "nested")
    
    # Filter MACA dates, reformat GridMET dates
    dataset <- tolower(strsplit(class(arg_ref), "_")[[1]][1])
    dates <- filter_years(start_year, end_year)
    if (dataset == "maca") {
      ds <- ds$sel(time = c(dates[[1]]: dates[[length(dates)]]))
    }
    
    # Create coordinate vectors
    lats <- np$asarray(c(as.matrix(aoilats)), dtype = np$float32)
    lons <- np$asarray(c(as.matrix(aoilons)), dtype = np$float32)
    times <- np$asarray(dates, np$int32)
    
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

    #     # Fix the variable attributes (why is grid_mapping a variable attribute?)
    #     internal_varname <- elements[["internal_varname"]]
    #     var_attrs <- ds[internal_varname]$attrs
    #     if ("grid_mapping" %in% names(var_attrs)) {
    #       var_attrs["grid_mapping"] <- NULL
    #     }
    # 
    #     # Assign attributes - reticulate doesn't handle this well
    #     x <- tryCatch({
    #       ds[internal_varname]$attrs <- var_attrs
    #     }, error=function(e){})
    #   
    #     x <- tryCatch({
    #       ds$time$attrs <- list("long_name" = "time", "units" = arg_ref$units[["time"]], "calendar" = "gregorian")
    #     }, error=function(e){})
    # 
    #     x <- tryCatch({
    #       ds$lon$attrs <- list("long_name" = "longitude" , "units" = "degrees_east", "standard_name" = "longitude")
    #     }, error=function(e){})
    # 
    #     x <- tryCatch({
    #       ds$lat$attrs = list("long_name" = "latitude" , "units" = "degrees_north", "standard_name" = "latitude")
    #     }, error=function(e){})
    
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