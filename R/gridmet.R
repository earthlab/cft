# Scrap material for gridmet
reticulate::use_condaenv("cft")
xr <- reticulate::import("xarray")

# Different list of parameters and internal variable names
translations = c("pr" = "pr",
                 "rhsmax" = "rmax",
                 "rhsmin" = "rmin",
                 "sph" = "huss",
                 "srad" = NULL,
                 "th" = NULL,
                 "tmmn" = "tasmin",
                 "tmmx" = "tasmax",
                 "vs" = NULL,
                 "bi" = NULL,
                 "fm100" = NULL,
                 "fm1000" = NULL,
                 "erc" = NULL,
                 "pdsi" = NULL,
                 "etr" = NULL,
                 "pet" = NULL,
                 "vpd" = "vpd")

parameters = c("pr" = "precipitation_amount",
               "rmax" = "relative_humidity",
               "rmin" = "relative_humidity",
               "sph" = "specific_humidity",
               "srad" = "surface_downwelling_shortwave_flux_in_air",
               "th" = "wind_from_direction",
               "tmmn" = "air_temperature",
               "tmmx" = "air_temperature",
               "vs" = "wind_speed",
               "bi" = "burning_index_g",
               "fm100" = "dead_fuel_moisture_100hr",
               "fm1000" = "dead_fuel_moisture_1000hr",
               "erc" = "energy_release_component-g",
               "pdsi" = "palmer_drought_severity_index",
               "etr" = "potential_evaporation",
               "pet" = "potential_evaporation",
               "vpd" = "mean_vapor_pressure_deficit")

labels = c("pr" = "Precipitation",
           "rmax" =  "Maximum Relative Humidity",
           "rmin" =  "Minimum Relative Humidity",
           "sph" = "specific_humidity",
           "srad" = "surface_downwelling_shortwave_flux_in_air",
           "th" = "wind_from_direction",
           "tmmn" = "Minimum Temperature",
           "tmmx" = "Maximum Temperature",
           "vs" = "wind_speed",
           "bi" = "burning_index_g",
           "fm100" = "dead_fuel_moisture_100hr",
           "fm1000" = "dead_fuel_moisture_1000hr",
           "erc" = "energy_release_component-g",
           "pdsi" = "palmer_drought_severity_index",
           "etr" = "potential_evapotranspiration",
           "pet" = "potential_evaporation",
           "vpd" = "mean_vapor_pressure_deficit") 

labels = list("pr" = "Daily Accumulated Precipitation",
              "rmax" = "Daily Maximum Relative Humidity",
              "rmin" = "Daily Minimum Relative Humidity",
              "sph" = "Daily Mean Specific Humidity",
              "srad" = "Daily Mean Downward Shortwave Radiation At Surface",
              "th" = "Daily Mean Wind Direction",
              "tmmn" = "Daily Minimum Temperature",
              "tmmx" = "Daily Maximum Temperature",
              "vs" = "Daily Mean Wind Speed",
              "bi" = "Burning Index",
              "fm100" = "100 Hour Fuel Moisture",
              "fm1000" = "1000 Hour Fuel Moisture",
              "erc" = "Energy Release Component",
              "pdsi" = "Palmer Drought Severity Index",
              "etr" = "Daily Reference Evapotranspiration (alfalfa)",
              "pet" = "Daily Reference Evapotranspiration (short Grass)",
              "vpd" = "Mean Vapor Presure Deficit")

units = list("pr" = "mm",
             "rmax" = "%",
             "rmin" = "%",
             "sph" = "kg/kg",
             "srad" = "W m-2",
             "th" = "Degrees Clockwise from north",
             "tmmn" = "K",
             "tmmx" = "K",
             "vs" = "m/s",
             "bi" = "Unitless",
             "fm100" = "Percent",
             "fm1000" = "Percent",
             "erc" = "Unitless",
             "pdsi" = "unitless",
             "etr" = "mm",
             "pet" = "mm",
             "vpd" = "kPa")


# Sample query for future get_gridmet function
local_dir <- "~/cft_test"
years = c(1979, 1985)
parameters = c("pr", "rmax")
area_name = "Yellowstone National Park"

grid_ref = cft::grid_reference
aoi = get_park_boundaries(area_name, local_dir = local_dir)
index_pos <- get_aoi_indexes(aoi, grid_ref)
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


# This call will be slightly different too since we'll have a multifile call
query = queries[[1]]
ds <- xr$open_dataset(query)
ds$to_netcdf("~/cft_test/gridmet_test.nc")






# New Functions
year_days <- function(year, parameter) {

  # Two scenarios, this year or previous years
  today <- Sys.Date()
  year <- as.character(year)

  if (year == format(today, "%Y")) {

    # There is a several day lag for new daily data
    ndays <- current_year_days(year, parameter)

  } else {

    # Only one possible number for past years
    last_day = as.Date(paste(year, "12", "31", sep = "-"))
    first_day <- as.Date(paste(year, "01", "01", sep = "-"))
    days <- seq(first_day, last_day, by="1 day")
    ndays <- length(days)
  }

  return(ndays)
}


current_year_days <- function(year, parameter) {

  # How many days are available in current year
  query <- glue::glue(paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/",
                             "MET/{parameter}/{parameter}_{year}.nc?#fillmismatch"))
  ds <- xr$open_dataset(query)
  ndays = length(ds$day$data)
  return(ndays)
}
