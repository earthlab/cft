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
    initialize = function(crs = "+proj=longlat +datum=WGS84",
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


Argument_Reference <- methods::setRefClass(
  "Argument_Reference",
  
  fields = list(
    models = "character",
    parameters = "character",
    scenarios = "character",
    variables = "list",
    labels = "list",
    units = "list"),
  
  methods = list(
    initialize = function(
      models = c("bcc-csm1-1", "bcc-csm1-1-m", "BNU-ESM", "CanESM2", "CCSM4",
                 "CNRM-CM5", "CSIRO-Mk3-6-0", "GFDL-ESM2M", "GFDL-ESM2G",
                 "HadGEM2-ES365", "HadGEM2-CC365", "inmcm4", "IPSL-CM5A-LR",
                 "IPSL-CM5A-MR", "IPSL-CM5B-LR", "MIROC5", "MIROC-ESM",
                 "MIROC-ESM-CHEM", "MRI-CGCM3", "NorESM1-M"),
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
                   "vpd" = "kPa")
      ) {
      models <<- models
      parameters <<- parameters
      scenarios <<- scenarios
      variables <<- variables
      labels <<- labels
      units <<- units
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


#' @title Attributes
#' @param argument_reference object containing information about input models 
#' and parameters for cft.
#' @export
argument_reference <- Argument_Reference()$initFields()

#' @title Attributes
#' @param grid_reference Reference object containing geographical coordinate
#' information of the full grid in which the data is provided.
#' @export
grid_reference <- Grid_Reference()$initFields()
