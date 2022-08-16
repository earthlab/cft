#' @title Read gridMET parallel
#'
#' @description This function downloads large quantities of gridMET data into memory by reading and combining the data from a variety of gridMET data files into a single data frame using
#' parallel computing.  The function takes a bounding box for the spatial extent over which the user would like gridMET data, a vector of variable abbreviations for which the user would 
#' like gridMET data, a vector of years over which the user would like gridMET data, the start time for which the user would like gridMET data, and the end time for which the user would
#' like gridMET data.  Note that the start time and end time are expressed as the number of days between 1900-01-01 and the start date or end date, respectively.  
#'
#' @param bb A bounding box for the spatial extent for which the user would like gridMET data
#' @param gm_vars A vector of variable abbreviations for which you would like gridMET data
#' @param years A vector of years for which you would like gridMET data
#' @param start_time The start time for which you would like gridMET data, expressed as the number of days between 1900-01-01 and your desired start date
#' @param end_time The end time for which you would like gridMET data, expressed as the number of days between 1900-01-01 and your desired end date
#'
#' @importFrom magrittr %>%
#' @importFrom doParallel
#' @importFrom foreach
#' @importFrom dplyr
#' @return Data.frame containing the desired gridMET data with one column per gridMET variable as well as columns for latitude, longitude, and time
#' @export
read_gridmet_parallel <- function(bb, gm_vars, years, start_time, end_time){
  require(doParallel)
  require(foreach)
  require(dplyr)
  
  # find the number of years for which gridMET data is requested
  len_yrs <- length(years)
  
  urls <- c()
  data_ids <- c()
  
  # iterate over the requested gridMET variables
  for (i in 1:length(gm_vars)){
    # find the current gridMET variable
    var <- gm_vars[i]
    
    # iterate over the requested gridMET years
    for (j in 1:length(years)){
      # find the current gridMET year
      year <- years[j]
      
      # using the current gridMET variable and year, find the url of the nc file containing the requested gridMET data
      url <- paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/MET/", var, "/", var, "_", year, ".nc#fillmismatch")
      
      # add the url for the nc file containing the current gridMET variable at the current year to the vector of gridMET urls
      urls <- c(urls, url)
      
      # create a unique id for the current gridMET data
      data_id <- paste(var, year, sep='_')
      
      # add the current data id to the vector of data ids for the requested gridMET data
      data_ids <- c(data_ids, data_id)
    }
  }
  
  max_len <- 0

  # determine the number of available cores for parallel computing, saving one to run the operating system
  no_cores  <- parallel::detectCores() - 1
  # set up parallel computing using all but one of the cores available
  doParallel::registerDoParallel(no_cores)
  
  # use parallel computing to obtain data about each of the requested gridMET variables for each of the requested years
  gm_data <- foreach::foreach(i = 1:length(urls)) %dopar% {
    # select the url for the current gridMET nc file
    file_url <- urls[i]
    
    # use tidync to open the current gridMET nc file
    file <- tidync(file_url)
    
    # filter the data in the current gridMET file to select only latitudes and longitudes within the requested bounding box and between the starting and ending time for the current year and convert it into an SF object using the WGS84 projection
    data <- file %>% 
      hyper_filter(lat = lat <= c(bb[4]+0.05) & lat >= c(bb[2]-0.05)) %>%
      hyper_filter(lon = lon <= c(bb[3]+0.05) & lon >= c(bb[1]-0.05)) %>%
      hyper_filter(day = day <= end_time & day >= start_time) %>%
      hyper_tibble() %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant")
    
    # extract the data from the SF object into a data frame with a lon and lat column for longitude and latitude data, respectively
    data <- tidyr::extract(data, geometry, into = c('lon', 'lat'), '\\((.*),(.*)\\)')
    # remove the unnecessary geometry columns from the data frame
    data <- subset(data, select=-geometry)
    data <- subset(data, select=-geometry)
    # reorder the columns so that longitude data is first, followed by latitude data, then the gridMET variable data, and finally day data
    data <- dplyr::select(data,3,4,1,2)
    
    # this code finds the maximum length of all of the gridMET data frames
    if (length(data$lon) > max_len){
      max_len <- length(data$lon)
    }
    
    # save the data frame to the list of data frames for the gridMET variables and years
    list(data)
  }
  
  # since we are doing synchronous parallel callbacks, the order of data frames in the list from parallel computing matches the order of data ids we found earlier
  names(gm_data) <- data_ids
  
  # create two new data frames for combining the data from the list into a data frame
  data <- data.frame()
  tmp <- data.frame()
  
  # iterate over the data frames in the list of gridMET data
  for (i in 1:length(gm_data)){
    # convert the current gridMET data into a data frame
    dat <- as.data.frame(gm_data[[i]])
    
    # find the length of the current data frame
    cur_len <- length(dat$lon)
    
    # if the length of the current data frame is smaller than the maximum data frame length, backfill the vectors in the data frame with NAs
    if (cur_len < max_len){
      length(dat$lon) <- max_len
      length(dat$lat) <- max_len
      length(dat$day) <- max_len
      length(dat[,3]) <- max_len
    }
    
    # this code handles combining all of the data frames into a single data frame of gridMET data
    if (i == 1){
      # for the first data frame, simply replace data with the data frame
      tmp <- dat
      data <- tmp
      tmp <- data.frame()
    } else if ((i-1) %% len_yrs == 0){
      tmp <- dplyr::select(dat, 3)
    } else if (i > 1 && i < len_yrs+1){
      tmp <- as.data.frame(rbind(data, dat))
      data <- tmp
      tmp <- data.frame()
    } else {
      new_var <- dplyr::select(dat, 3)
      tmp <- as.data.frame(rbind(tmp, new_var))
      if (i %% len_yrs == 0){
        data <- as.data.frame(cbind(data, tmp))
        tmp <- data.frame()
      }
    }
  }
  
  # move the day column to be the last column in the data frame
  data <- data %>% relocate(day, .after=last_col())
  
  # return the data frame containing the gridMET data
  return(data)
}