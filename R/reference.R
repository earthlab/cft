GridMET_Reference <- methods::setRefClass(
  "GridMET_Reference",

  fields = list(
    parameters = "character",
    variables = "list",
    labels = "list",
    units = "list",
    time_dim = "character"
    ),

  methods = list(
    initialize = function(
      parameters = c("pr", "rmax", "rmin", "sph", "srad", "th", "tmmn", "tmmx", "vs","bi",
                     "fm100", "fm1000", "erc", "pdsi", "etr", "pet", "vpd"),
      variables = list("pr" = "precipitation_amount",
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
                       "etr" = "potential_evapotranspiration",
                       "pet" = "potential_evapotranspiration",
                       "vpd" = "mean_vapor_pressure_deficit"),
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
                    "vpd" = "Mean Vapor Presure Deficit"),
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
                   "vpd" = "kPa",
                   "time" = "days since 1900-01-01"),
      time_dim = "day"
    ) {
      parameters <<- parameters
      variables <<- variables
      labels <<- labels
      units <<- units
      time_dim <<- time_dim
    }
  )
)


MACA_Reference <- methods::setRefClass(
  "MACA_Reference",
  
  fields = list(
    models = "character",
    parameters = "character",
    scenarios = "character",
    variables = "list",
    labels = "list",
    units = "list",
    time_dim = "character"
    ),
  
  methods = list(
    initialize = function(
      models = c("bcc-csm1-1", "bcc-csm1-1-m", "BNU-ESM", "CanESM2", "CCSM4",
                 "CNRM-CM5", "CSIRO-Mk3-6-0", "GFDL-ESM2M", "GFDL-ESM2G",
                 "HadGEM2-ES365", "HadGEM2-CC365", "inmcm4", "IPSL-CM5A-LR",
                 "IPSL-CM5A-MR", "IPSL-CM5B-LR", "MIROC5", "MIROC-ESM",
                 "MIROC-ESM-CHEM", "MRI-CGCM3", "NorESM1-M", "GridMET"),
      parameters = c("tasmin", "tasmax", "rhsmin", "rhsmax", "pr", "rsds",
                     "uas", "vas", "huss", "vpd"),
      scenarios = c("rcp45", "rcp85"),
      variables = list("tasmin" = "air_temperature",
                       "tasmax" = "air_temperature",
                       "rhsmin" = "relative_humidity",
                       "rhsmax" = "relative_humidity",
                       "pr" = "precipitation",
                       "rsds" = "surface_downwelling_shortwave_flux_in_air",
                       "uas" = "eastward_wind",
                       "vas" = "northward_wind",
                       "huss" = "specific_humidity",
                       "vpd" = "vpd"),
      labels = list("tasmin" = "Minimum Temperature",
                    "tasmax" = "Maximum Temperature",
                    "rhsmin" = "Minimum Relative Humidity",
                    "rhsmax" = "Maximum Relative Humidity",
                    "pr" = "Precipitation",
                    "rsds" = "Surface Downswelling Shortwave Flux",
                    "uas" = "Eastward Wind",
                    "vas" = "Northward Wind",
                    "huss" = "Specific Humidity",
                    "vpd" = "Vapor Pressure Deficit",
                    "rcp45" = "Representative Concentration Pathway 4.5",
                    "rcp85" = "Representative Concentration Pathway 8.5",
                    "bcc-csm1-1" = "Beijing Climate Center - Climate System Model 1.1",
                    "bcc-csm1-1m" = "Beijing Climate Center - Climate System Model 1.1 Moderate Resolution",
                    "BNU-ESM" = "Beijing Normal University - Earth System Model",
                    "CanESM2" = "Canadian Earth System Model 2",
                    "CCSM4" = "Community Climate System Model 4",
                    "CNRM-CM5" = "Centre National de Recherches M\U00E9t\U00E9orologiques - Climate Model 5",
                    "CSIRO-Mk3-6-0" = "Commonwealth Scientific and Industrial Research Organisation - Mk3.6.0",
                    "GFDL-ESM2M" = "Geophysical Fluid Dynamics Laboratory - Earth System Model 2 Modular Ocean",  
                    "GFDL-ESM2G" = "Geophysical Fluid Dynamics Laboratory - Earth System Model 2 Generalized Ocean Layer Dynamics",
                    "HadGEM2-ES365" = "Hadley Global Environment Model 2 - Earth System 365 (day)",
                    "HadGEM2-CC365" = "Hadley Global Environment Model 2 - Climate Chemistry 365 (day) ", 
                    "inmcm4" = "Institute of Numerical Mathematics Climate Model 4",
                    "IPSL-CM5A-LR" = "Institut Pierre Simon Laplace (IPSL) - Climate Model 5A - Low Resolution",
                    "IPSL-CM5A-MR" = "Institut Pierre Simon Laplace (IPSL) - Climate Model 5A - Medium Resolution",
                    "IPSL-CM5B-LR" = "Institut Pierre Simon Laplace (IPSL) - Climate Model 5B - Low Resolution",
                    "MIROC5" = "Model for Interdisciplinary Research On Climate 5",      
                    "MIROC-ESM" = "Model for Interdisciplinary Research On Climate - Earth System Model",
                    "MIROC-ESM-CHEM" = "Model for Interdisciplinary Research On Climate - Earth System Model - Chemistry",
                    "MRI-CGCM3" = "Meteorological Research Institute - Coupled Global Climate Model 3",
                    "NorESM1-M" = "Norwegian Earth System Model 1 - Medium Resolution"),
      units = list("air_temperature" = "K",
                   "relative_humidity" = "%",
                   "precipitation" = "mm",
                   "surface_downwelling_shortwave_flux_in_air" = "W m-2",
                   "eastward_wind" = "m s-1",
                   "northward_wind" = "m s-1",
                   "specific_humidity" = "kg kg-1",
                   "vpd" = "kPa",
                   "time" = "days since 1950-01-01"),
      time_dim = "time"
      ) {
      models <<- models
      parameters <<- parameters
      scenarios <<- scenarios
      variables <<- variables
      labels <<- labels
      units <<- units
      time_dim <<- time_dim
    },
  
    get_args = function(model) {
      args <- list()
      for (m in models) {
        args[[m]] <- list("parameters" = parameters, "scenarios" = scenarios,
                          "ensemble" =  "r1i1p1")
      }
      
      # Exceptions
      param_red <-  c("tasmin", "tasmax", "pr", "rsds", "uas", "vas", "huss")
      args[["NorESM1-M"]]$parameters <- param_red
      args[["CCSM4"]]$parameters <- param_red
      args[["CCSM4"]]$ensemble <- "r6i1p1"
      
      return(args[[model]])
    }
  )
)


Grid_Reference <- methods::setRefClass(
  "Grid_Reference",
  
  fields = list(
    crs = "character",
    extent = "list",
    resolution = "numeric",
    lats = "numeric",
    lons = "numeric",
    ntime_historical = "numeric",
    ntime_model = "numeric"
  ),
  
  methods = list(
    initialize = function(crs = paste0("+proj=longlat +a=6378137 ",
                                       "+f=0.00335281066474748 +pm=0 +no_defs"),
                          extent = list("latmin" = 25.0631,
                                        "latmax" = 49.3960,
                                        "lonmin" = -124.7722,
                                        "lonmax" = -67.0648),
                          resolution = 0.04166575,
                          nlat = 585,
                          nlon = 1386,
                          ntime_historical = 20453,
                          ntime_model = 34332) {
      crs <<- crs
      resolution <<- resolution
      extent <<- extent
      lats <<- extent$latmin + (1:nlat) * resolution
      lons <<- extent$lonmin + (1:nlon) * resolution
      ntime_historical <<- ntime_historical
      ntime_model <<- ntime_model
    }
  )
)

# Inititalize each
maca_reference <- MACA_Reference()$initFields()
gridmet_reference <- GridMET_Reference()$initFields()
grid_reference <- Grid_Reference()$initFields()


#' Return a reference object for the arguments associated with a data set used in cft.
#'
#' @param dataset A data set from which to download data. Currently available data
#' sets include 'MACA' and 'GridMET'. (character)
#' @return A reference class object containing available input parameters for `get_gridmet()`
#' or `get_maca()` and reference information about each.
#' 
#' @examples 
#' \dontrun{
#' file_ref <- get_reference("maca")
#' print(file_ref$parameters)
#' }
#' 
#' @export
get_reference <- function(dataset) {
  dataset <- tolower(dataset)
  ref <- DATASETS[[dataset]]
  return(ref)            
}


DATASETS <- list("maca" = maca_reference,
                 "gridmet" = gridmet_reference
)
