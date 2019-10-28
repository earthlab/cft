#' @import methods

Grid_Reference <- methods::setRefClass(
  "Grid_Reference",
  
  fields = list(
    crs = "character",
    extent = "list",
    resolution = "numeric",
    lats = "numeric",
    lons = "numeric",
    ntime_hist = "numeric",
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
                          ntime_hist = 20453,
                          ntime_model = 34332) {
      crs <<- crs
      resolution <<- resolution
      extent <<- extent
      lats <<- sapply(1:(nlat),
                      function(x) extent["latmin"][[1]] + x * resolution)
      lons <<- sapply(1:(nlon),
                      function(x) extent["lonmin"][[1]] + x * resolution)
      ntime_hist <<- ntime_hist
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
      units = list("air_temperature" = "K",
                   "relative_humidity" = "%",
                   "precipitation" = "mm",
                   "surface_downwelling_shortwave_flux_in_air" = "W m-2",
                   "eastward_wind" = "m s-1",
                   "northward_wind" = "m s-1",
                   "specific_humidity" = "kg kg-1",
                   "vpd" = "kPa"
      )) {
      models <<- models
      parameters <<- parameters
      scenarios <<- scenarios
      variables <<- variables
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


#' @export 
grid_reference = Grid_Reference()

#' @export
argument_reference = Argument_Reference()