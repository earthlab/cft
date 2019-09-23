#' URL parameter equivalents, with attributes
models1 <- c("bcc-csm1-1", "bcc-csm1-1-m", "BNU-ESM", "CanESM2", "CCSM4", "CNRM-CM5", "CSIRO-Mk3-6-0",
             "GFDL-ESM2M", "GFDL-ESM2G", "HadGEM2-ES365", "HadGEM2-CC365", "inmcm4", "IPSL-CM5A-LR",
             "IPSL-CM5A-MR", "IPSL-CM5B-LR", "MIROC5", "MIROC-ESM", "MIROC-ESM-CHEM", "MRI-CGCM3",
             "NorESM1-M")
params1 <- c("tasmin", "tasmax", "rhsmin", "rhsmax", "pr", "rsds", "uas", "vas", "huss")
scenarios1 <- c("rcp45", "rcp85")
variables1 <- list("tasmin" = "air_temperature",
                   "tasmax" = "air_temperature",
                   "rhsmin" = "relative_humidity",
                   "rhsmax" = "relative_humidity",
                   "pr" = "precipitation",
                   "rsds" = "surface_downwelling_shortwave_flux_in_air",
                   "uas" = "eastward_wind",
                   "vas" = "northward_wind",
                   "huss" = "specific_humidity")
units1 <- list("air_temperature" = "K",  "relative_humidity" = "%", "precipitation" = "mm",
               "surface_downwelling_shortwave_flux_in_air" = "W m-2", "eastward_wind" = "m s-1",
               "northward_wind" = "m s-1", "specific_humidity" = "kg kg-1")


# Something like a python class structure?
Argument_Builder <- setRefClass(
  "maca_parameters",

  fields = list(
    models = "character",
    params = "character",
    scenarios = "character",
    variables = "list",
    units = "list"),

  methods = list(
    initialize = function(models = models1, params = params1, scenarios = scenarios1,
                          variables = variables1, units = units1) {
      models <<- models
      params <<- params
      scenarios <<- scenarios
      variables <<- variables
      units <<- units
      },

    get_args = function(model){
      args <- list()
      for (m in models){
        args[[m]] <- list("parameters" = params1, "scenarios" = scenarios1, "ensemble" =  "r1i1p1")
      }

      # CCSM4 does not have relative humidity and uses ensemble "r6i1p1" (so far the only differences observed)
      args[["CCSM4"]]$parameters = c("tasmin", "tasmax", "pr", "rsds", "uas", "vas", "huss")
      args[["CCSM4"]]$ensemble = "r6i1p1"

      print("ARGUMENTS!")
      return(args[model])
    }
  )
)


arg_builder <- Argument_Builder()
arg_builder$get_args("CCSM4")










# The full grid for spatial subsetting <------------------------------------------------------------- check precision
conus_grid <- function(ncpointer){
  # Use NC object itself for the full grid. This way, in case the extent ever changes, it will adapt

}
proj <- "+proj=longlat +a=6378137 +f=0.00335281066474748 +pm=0 +no_defs"
extent <- c("latmin" = 25.0631, "latmax" = 49.3960, "lonmin" = -124.7722, "lonmax" = -67.0648)
resolution <- 0.04166575
nlat <- 584  # -1 for everything
nlon <- 1385
ntime_hist <- 20453
ntime_future <- 34332
lats <- sapply(0:(nlat), function(x) extent["latmin"][[1]] + x*resolution)
lons <- sapply(0:(nlon), function(x) extent["lonmin"][[1]] + x*resolution)

