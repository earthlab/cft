#' @title Single point firehose
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
single_point_firehose <- function(input_variables, lat, lon,
                            web_link = "https://cida.usgs.gov/thredds/dodsC/macav2metdata_daily_future",
                           verbose = TRUE){
  require(tidync, quietly = TRUE)
  #require(RCurl)
  #require(curl)
  require(future)
  require(furrr)
  
  require(rlist)

  require(plyr)
  require(pipeR)
  require(cft)
  
  #doParallel::registerDoParallel(cores = 3)
  
  n_cores <- availableCores() - 1
  plan(multisession, workers = n_cores)
  
  
  
  message('Trying to connect to the USGS.gov API')
  
  src <- tidync::tidync(web_link)
  
  
  
  new_lat <- lat
  new_lon <- lon
  
  OISST_load <- function(column, input_variables, new_lat, new_lon){
    
    tryCatch({
      web_link = "https://cida.usgs.gov/thredds/dodsC/macav2metdata_daily_future"
      src <- tidync::tidync(web_link)
      
      Pulled_data_single_space_single_timepoint <- inputs$src %>% 
        hyper_filter(lat = lat == c(new_lat)) %>% 
        hyper_filter(lon = lon == c(new_lon)) %>%
        #hyper_filter(time = times$`Available times` ==  44558) %>% 
        hyper_tibble(select_var = input_variables[column]) %>%
        as.data.frame() 
      
      return(Pulled_data_single_space_single_timepoint[,c(1,4)])
    },
    error = function(e){
      Pulled_data_single_space_single_timepoint <-     
        data.frame( matrix(rep(NA, 34333) ))
      colnames(Pulled_data_single_space_single_timepoint) <- input_variables[column]
      return(Pulled_data_single_space_single_timepoint)
    })
  }
  
  
  
  length(input_variables)
  start_time <- Sys.time()
  
  mp <- future_map(1:181 ,OISST_load, .progress=TRUE, input_variables= input_variables,
                   new_lat = new_lat, new_lon = new_lon)
  mp
  
  end_time <- Sys.time()
  print(end_time - start_time)
  
  need_rerun <- mp %>%
    list.filter(ncol(.) == 1) %>%
    list.cbind() 
  #need_rerun
  rr <- future_map(which( input_variables %in% colnames(need_rerun) ),OISST_load, .progress=TRUE, input_variables= input_variables,
                   new_lat = new_lat, new_lon = new_lon)
  
  
  
  mps <- mp %>>% list.filter( nrow(.) == 34333 & ncol(.) == 2) %>>% list.cbind()
  rrs <- rr %>>% list.filter( nrow(.) == 34333& ncol(.) == 2) %>>% list.cbind()
  
  if(length(rrs) == 0){
    first_pass <- mps %>% as.data.frame()
  }else{
    first_pass <- cbind(mps, rrs) %>% as.data.frame()
  }
  
  
  
  all(first_pass[,which(names(first_pass) == "time")])
  all(first_pass[,which(names(first_pass) == "lon")])
  all(first_pass[,which(names(first_pass) == "lat")])
  
  first_pass_clean <- first_pass[ , !duplicated(colnames(first_pass))] 
  
  
  odd_balls <- mp %>>% list.filter( nrow(.) != 34333 & ncol(.) == 2) 
  
  dfs <- list(first_pass_clean)
  outs <- join_all(append(dfs, odd_balls)) 
  
  lat_lon <- data.frame(cbind( rep(as.numeric(new_lon), nrow(outs)), rep(as.numeric(new_lat), nrow(outs))))
  colnames(lat_lon) <- c("lon", "lat")
  outs_spatial <- cbind(outs,lat_lon)
  
  
  pulled_data_sf <- st_as_sf(outs_spatial, coords = c("lon", "lat"), crs = "WGS84", agr = "constant")
  
  
  return(pulled_data_sf)
}

