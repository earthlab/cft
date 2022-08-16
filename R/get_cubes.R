#' @title Get cubes
#'
#' @description This function downloads a data cube of MACA data and a data cube of gridMET data and returns a list containing both data cubes, where the names of the data cubes in the list are MACA and gridMET, respectively.
#' In order to obtain MACA data, this function uses CFT's available_data function and to obtain gridMET data, the function uses CFT's read_gridmet_parallel function.   
#'
#' @param bb The bounding box for the requested spatial extent for both cubes, created using the st_bbox function
#' @param maca_vars A vector containing the requested MACA variables as strings
#' @param maca_models A vector containing the requested MACA models as strings
#' @param maca_rcps A vector containing the requested MACA emission scenarios as strings
#' @param gm_vars A vector containing the requested gridMET variables as strings of the variable abbreviations
#' @param start_date A string containing the requested start date for the data in both cubes, in YYYY-MM-DD format
#' @param end_date A string containing the requested end date for the data in both cubes, in YYYY-MM-DD format
#'
#' @importFrom magrittr %>%
#' @return List containing a Data.frame of MACA date, named MACA, and a Data.frame of gridMET data, named gridMET
#' @export
get_cubes <- function(bb, maca_vars, maca_models, maca_rcps, gm_vars, start_date, end_date){
  require(cft)
  
  # define the reference date for both MACA and gridMET data
  ref_date <- as.Date("1900-01-01")
  
  # convert the start and end dates for MACA and gridMET data into date format
  start <- as.Date(start_date)
  end <- as.Date(end_date)
  
  # find the number of days from the reference date to the start and end dates, respectively
  start_time <- length(seq(from = ref_date, to = start, by = 'day'))-1
  end_time <- length(seq(from = ref_date, to = end, by = 'day'))-1
  
  # find the starting and ending year that are requested for gridMET data and create a vector containing all the years between the starting and ending year
  start_year <- year(start)
  end_year <- year(end)
  years <- seq(start_year, end_year, by=1)
  
  # obtain the available data from MACA using CFT
  inputs <- cft::available_data()
  
  # select the available variables from MACA that include the requested variables, models, and RCP scenarios
  input_variables <- inputs$variable_names %>% 
    filter(Variable %in% maca_vars) %>%
    filter(Model %in% maca_models) %>%
    filter(Scenario %in% maca_rcps) %>%
    pull("Available variable")
  
  # obtain the desired MACA data in the requested spatial region and the requested time frame, then convert the data to an SF object
  maca_data <- inputs$src %>%
    hyper_filter(lat = lat <= c(bb[4]+0.05) & lat >= c(bb[2]-0.05)) %>%
    hyper_filter(lon = lon <= c(bb[3]+0.05) & lon >= c(bb[1]-0.05)) %>%
    hyper_filter(time = time <= end_time & time >= start_time) %>%
    hyper_tibble(select_var = input_variables) #%>%
    #st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant")
  
  maca_data <- as.data.frame(maca_data)
  
  # use the read_gridmet_parallel function to obtain gridMET data in the requested spatial region, over the requested time frame, and for the requested gridMET variables
  # this returns a regular data frame containing the requested gridMET data
  gm_data <- read_gridmet_parallel(bb, gm_vars, years, start_time, end_time)
  
  # create a list containing the two cubes whose names are MACA and gridMET respectively
  cubes <- list("MACA" = maca_data, "gridMET" = gm_data)
  
  # return the list containing both data cubes
  return(cubes)
}