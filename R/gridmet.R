fillin <- function () {
  
  # Load cft conda environment
  reticulate::use_condaenv("cft", required = TRUE)
  xr <- reticulate::import("xarray")
  
  # Sample query for future get_gridmet function
  local_dir <- "~/cft_test"
  years = c(1979, 1985)
  parameters = c("pr", "rmax")
  area_name = "Yellowstone National Park"
  aoi = get_park_boundaries(area_name, local_dir = local_dir)
  index_pos <- get_aoi_indexes(aoi, grid_reference)
  y1 <- index_pos[["y1"]]
  y2 <- index_pos[["y2"]]
  x1 <- index_pos[["x1"]]
  x2 <- index_pos[["x2"]]
  
  # Different base url from maca of course
  urlbase <- "http://thredds.northwestknowledge.net:8080/thredds/dodsC/MET"
  queries <- list()
  for (p in names(parameters)) {
    for (y in years[1]: years[2]) {
      y <- as.character(y)
      ntime <- year_days(y, p)
      filename <- paste(paste(p, y, sep = "_"), "nc", sep = ".")
      parbase <- file.path(urlbase, p, filename)
      var <- parameters[p]
      query <- glue::glue(paste0("?{var}[{0}:{1}:{ntime}][{y1}:{1}:{y2}][{x1}:{1}:{x2}],",
                                "day[0:1:{ntime}],lat[0:1:584],lon[0:1:1385]",
                                "#fillmismatch"))
      url <- paste0(parbase, query)
  
      # ...
      # queries[[length(queries) + 1]] <- list(paired_url, file_name, elements)
      queries[[length(queries) + 1]] <- url
    }
  }

  # This call will be slightly different too since we don't have a multi file call
  query = queries[[1]]
  ds <- xr$open_dataset(query)
  # ds$to_netcdf("~/cft_test/gridmet_test.nc")
}


# New Functions for subsetting
year_days <- function(year, parameter) {
  # Two scenarios, this year or previous years
  today <- Sys.Date()
  year <- as.character(year)

  if (year == format(today, "%Y")) {
    # There is a several day lag for new daily data
    ndays <- current_year_days(year, parameter)

  } else {
    # Only one possible number for past years
    last_day <- as.Date(paste(year, "12", "31", sep = "-"))
    first_day <- as.Date(paste(year, "01", "01", sep = "-"))
    days <- seq(first_day, last_day, by = "1 day")
    ndays <- length(days)
  }

  return(ndays)
}


current_year_days <- function(year, parameter) {
  # Load cft conda environment
  reticulate::use_condaenv("cft", required = TRUE)
  xr <- reticulate::import("xarray")
  
  # How many days are available in current year for this parameter?
  query <- glue::glue(paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/",
                             "MET/{parameter}/{parameter}_{year}.nc?#fillmismatch"))
  ds <- xr$open_dataset(query)
  ndays <- length(ds$day$data)
  return(ndays)
}
