read_gridmet_parallel <- function(bb, gm_vars, years, start_time, end_time){
  len_yrs <- length(years)
  urls <- c()
  data_ids <- c()
  
  for (i in 1:length(gm_vars)){
    var <- gm_vars[i]
    
    #params <- c(params, rep(var, length(years)))
    
    for (j in 1:length(years)){
      year <- years[j]
      
      url <- paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/MET/", var, "/", var, "_", year, ".nc#fillmismatch")
      
      urls <- c(urls, url)
      
      data_id <- paste(var, year, sep='_')
      data_ids <- c(data_ids, data_id)
    }
  }
  
  data_order <- c()
  max_len <- 0
  
  #`%dopar%` <- foreach::`%dopar%
  no_cores  <- parallel::detectCores() - 1
  doParallel::registerDoParallel(no_cores)
  
  gm_data <- foreach::foreach(i = 1:length(urls)) %dopar% {
    file_url <- urls[i]
    
    file <- tidync(file_url)
    
    data <- file %>% 
      hyper_filter(lat = lat <= c(bb[4]+0.05) & lat >= c(bb[2]-0.05)) %>%
      hyper_filter(lon = lon <= c(bb[3]+0.05) & lon >= c(bb[1]-0.05)) %>%
      hyper_filter(day = day <= end_time & day >= start_time) %>%
      hyper_tibble() %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant")
    
    data <- tidyr::extract(data, geometry, into = c('lon', 'lat'), '\\((.*),(.*)\\)')
    data <- subset(data, select=-geometry)
    data <- subset(data, select=-geometry)
    data <- dplyr::select(data,3,4,1,2)
    
    if (length(data$lon) > max_len){
      max_len <- length(data$lon)
    }
    
    data_order <- c(data_order, file_url)
    list(data)
  }
  
  names(gm_data) <- data_ids
  
  data <- data.frame()
  
  tmp <- data.frame()
  for (i in 1:length(gm_data)){
    
    dat <- as.data.frame(gm_data[[i]])
    cur_len <- length(dat$lon)
    
    if (cur_len < max_len){
      length(dat$lon) <- max_len
      length(dat$lat) <- max_len
      length(dat$day) <- max_len
      length(dat[,3]) <- max_len
    }
    
    if (i == 1){
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
  
  data <- data %>% relocate(day, .after=last_col())
  
  gm_rast <- terra::rast(data, crs='WGS84')
  
  return(gm_rast)
}