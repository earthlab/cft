#' @title Available data
#'
#' @description Using the default web link, this package retrieves daily gridded data sets of General Circulation Model
#' (GCM) runs clipped to areas of interest and returns a data frame of the
#' file names and they're storage paths. Each of these data sets represent
#' a single GCM, climate variable and Representative Concentration Pathway (RCP)
#' from 1950 to 2099. The 1950 to 2005 portion of this time period represents
#' historical data while the 2006 to 2099 portion represents modeled data. 
#' The original data sets may be found at
#' \url{http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_CMIP5_aggregated_macav2_catalog.html}
#'
#' @param web_link A web link to the api data source you want to read (character)
#' @param verbose Should the api outputs be combined with internal metadata? (logical)
#'
#' @importFrom magrittr %>%
#' @return Data.frame of requested data
#' @export
available_data <- function(web_link = "https://cida.usgs.gov/thredds/dodsC/macav2metdata_daily_future",
verbose = TRUE){
  require(tidync, quietly = TRUE)
  
  message('Trying to connect to the USGS.gov API')
  
  src <- tidync::tidync(web_link)
  
  message('Reading results')
  
  variable_names <- src$variable %>% dplyr::select(name) 
  variable_names <- variable_names[-1:-4,]
  
  available_times <- src %>% activate("D3") %>% hyper_tibble()
  
  colnames(variable_names) <- "Available variable"
  colnames(available_times) <- "Available times"
  available_times$`Available times` <- as.numeric(available_times$`Available times`)
  if(verbose==FALSE){
    return(list(
      "variable_names" = variable_names, 
      "available_times" = available_times,
      "src" = src
    ))  
  }else{
    message('Converting into an R data.table')
    
    mydates <- seq.Date(as.Date("2006-01-01"), 
                        as.Date("2099-12-31"), 
                        by = 1)
    
    myjulian <- julian(mydates, origin=as.Date("1900-01-01"))
    time_library <- as.data.frame(cbind("Available times" = myjulian, dates = as.character(julian2date(myjulian))))
    time_library$`Available times` <- as.numeric(time_library$`Available times`)
    
    available_times <- as.data.frame(available_times) %>% left_join(time_library, by = "Available times")
    
    available_times$`Available times` <- as.numeric(available_times$`Available times`)
    
    
    scenarios <- as.data.frame(matrix(c("rcp45", "rcp85", "RCP 4.5", "RCP 8.5"), 2,2, byrow=FALSE))
    colnames(scenarios) <- c("scenario_abbreviation","scenario_name")
    
    
    
    labels <- as.data.frame(matrix(c("tasmin" , "Minimum Temperature",
                                     "tasmax" , "Maximum Temperature",
                                     "rhsmin" , "Minimum Relative Humidity",
                                     "rhsmax" , "Maximum Relative Humidity",
                                     "pr" , "Precipitation",
                                     "rsds" , "Surface Downswelling Shortwave Flux",
                                     "uas" , "Eastward Wind",
                                     "vas" , "Northward Wind",
                                     "huss" , "Specific Humidity",
                                     "vpd" , "Vapor Pressure Deficit",
                                     "rcp45" , "Representative Concentration Pathway 4.5",
                                     "rcp85" , "Representative Concentration Pathway 8.5",
                                     "bcc-csm1-1" , "Beijing Climate Center - Climate System Model 1.1",
                                     "bcc-csm1-1m" , "Beijing Climate Center - Climate System Model 1.1 Moderate Resolution",
                                     "BNU-ESM" , "Beijing Normal University - Earth System Model",
                                     "CanESM2" , "Canadian Earth System Model 2",
                                     "CCSM4" , "Community Climate System Model 4",
                                     "CNRM-CM5" , "Centre National de Recherches M\U00E9t\U00E9orologiques - Climate Model 5",
                                     "CSIRO-Mk3-6-0" , "Commonwealth Scientific and Industrial Research Organisation - Mk3.6.0",
                                     "GFDL-ESM2M" , "Geophysical Fluid Dynamics Laboratory - Earth System Model 2 Modular Ocean",  
                                     "GFDL-ESM2G" , "Geophysical Fluid Dynamics Laboratory - Earth System Model 2 Generalized Ocean Layer Dynamics",
                                     "HadGEM2-ES365" , "Hadley Global Environment Model 2 - Earth System 365 (day)",
                                     "HadGEM2-CC365" , "Hadley Global Environment Model 2 - Climate Chemistry 365 (day) ", 
                                     "inmcm4" , "Institute of Numerical Mathematics Climate Model 4",
                                     "IPSL-CM5A-LR" , "Institut Pierre Simon Laplace (IPSL) - Climate Model 5A - Low Resolution",
                                     "IPSL-CM5A-MR" , "Institut Pierre Simon Laplace (IPSL) - Climate Model 5A - Medium Resolution",
                                     "IPSL-CM5B-LR" , "Institut Pierre Simon Laplace (IPSL) - Climate Model 5B - Low Resolution",
                                     "MIROC5" , "Model for Interdisciplinary Research On Climate 5",      
                                     "MIROC-ESM" , "Model for Interdisciplinary Research On Climate - Earth System Model",
                                     "MIROC-ESM-CHEM" , "Model for Interdisciplinary Research On Climate - Earth System Model - Chemistry",
                                     "MRI-CGCM3" , "Meteorological Research Institute - Coupled Global Climate Model 3",
                                     "NorESM1-M" , "Norwegian Earth System Model 1 - Medium Resolution"), 32, 2, byrow=TRUE))
    
    labels <- as.data.frame(cbind(labels[,1], labels[,1], labels[,2]))
    colnames(labels) <- c("variable_abbreviation","model_abbreviation","name")
    
    units <- as.data.frame(matrix(c("Minimum Temperature" , "K",
                                    "Maximum Temperature" , "K",
                                    "Minimum Relative Humidity" , "%",
                                    "Maximum Relative Humidity" , "%",
                                    "Precipitation" , "mm",
                                    "Surface Downswelling Shortwave Flux" , "W m-2",
                                    "Eastward Wind" , "m s-1",
                                    "Northward Wind" , "m s-1",
                                    "Specific Humidity" , "kg kg-1",
                                    "Vapor Pressure Deficit" , "kPa"), 10,2, byrow=TRUE))
    colnames(units) <- c("Variable","Units")
    
    
    
    variable_names_verbose  <- variable_names %>% lubridate::separate("Available variable", "_",
                                                           into = c("variable_abbreviation", "model_abbreviation", "ensemble","scenario_abbreviation"), 
                                                           remove = FALSE)
    
    variable_names_verbose <- variable_names_verbose  %>% 
      left_join(labels %>% dplyr::select("variable_abbreviation","name"), by=c("variable_abbreviation")) %>% 
      left_join(labels %>% dplyr::select("model_abbreviation","name"), by=c("model_abbreviation")) %>% 
      left_join(scenarios, by=c("scenario_abbreviation")) 
    
    
    
    
    
    names(variable_names_verbose) <- c("Available variable", "Variable abbreviation",
                                       "Model abbreviation",
                                       "Model ensemble type (only CCSM4 relevant)",
                                       "Scenario abbreviation",
                                       "Variable",
                                       "Model",
                                       "Scenario")
    
    variable_names_verbose <- variable_names_verbose  %>% 
      left_join(units, by=c("Variable"))
    
    
    variable_names_verbose <- variable_names_verbose %>% dplyr::select("Available variable",
                                                                "Variable",
                                                                "Units",
                                                                "Model",
                                                                "Model ensemble type (only CCSM4 relevant)",
                                                                "Scenario",
                                                                "Variable abbreviation",
                                                                "Model abbreviation",
                                                                "Scenario abbreviation"
    )
    
    
    
    variable_names <- variable_names_verbose
    
    return(list(
      "variable_names" = variable_names, 
      "available_times" = available_times,
      "src" = src
    ))   
  }}