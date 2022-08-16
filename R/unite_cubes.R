#' @title Unite cubes
#'
#' @description This function combines two spatiotemporal data cubes which each have a single dependent variable and latitude, longitude, and time data.
#' The function first selects the data where the temporal data overlaps between the two cubes and then converts the temporal data to be at a common temporal resolution which is selected by the user.  
#' Then function allows the user to select the spatial resolution and spatial extent they would like to have in the final data cube and performs kriging to obtain predictions of the dependent variable 
#' from each cube using the spatial dependency formula var ~ lon + lat to indicate that the dependent variable depends only on latitude and longitude.
#'
#' @param cube1 A spatiotemporal data cube with one dependent variable and some number of independent variables stored as a data frame (Data.frame)
#' @param cube2 A spatiotemporal data cube with one dependent variable and some number of independent variables stored as a data frame (Data.frame)
#'
#' @importFrom magrittr %>%
#' @importFrom cft
#' @importFrom sf
#' @importFrom sp
#' @importFrom gstat
#' @importFrom dplyr
#' @return Data.frame containing the latitude and longitude coordinates of the combined cube, the time values for the combined cube, and the predicted values and variance for the dependent variables from both cubes from kriging 
#' @export
unite_cubes <- function(cube1, cube2){
  require(dplyr)
  require(cft)
  require(sf)
  require(sp)
  require(gstat)
  
  # define vectors containing the possible column names for the columns containing latitude, longitude, and time data, respectively
  ynames <- c('lat', 'latitude', 'Lat', 'Latitude', 'y')
  xnames <- c('lon', 'long', 'longitude', 'Lon', 'Long', 'Longitude', 'x')
  tnames <- c('time', 'day', 'hour', 'month', 'year', 'date')
    
  # create a sequence of values corresponding to the indices of all the columns in cube 1
  c1_indx <- seq(1, length(colnames(cube1)))
    
  # find the indices of the columns containing longitude, latitude, and time data in cube 1
  c1_xname <- which(colnames(cube1) %in% xnames)
  c1_yname <- which(colnames(cube1) %in% ynames)
  c1_tname <- which(colnames(cube1) %in% tnames)
    
  # find the index of the column containing the dependent variable data in cube 1 by selecting the remaining column index/indices from cube 1
  c1_vname <- c1_indx[-c(c1_xname, c1_yname, c1_tname)]
  
  # select the name of the dependent variable in cube 1 (or the dependent variable and independent variables in cube 1)
  c1_varname <- colnames(cube1)[c1_vname]
    
  # create a sequence of values corresponding to the indices of all the columns in cube 2
  c2_indx <- seq(1, length(colnames(cube2)))
    
  # find the indices of the columns containing longitude, latitude, and time data in cube 2
  c2_xname <- which(colnames(cube2) %in% xnames)
  c2_yname <- which(colnames(cube2) %in% ynames)
  c2_tname <- which(colnames(cube2) %in% tnames)
  
  # find the index of the column containing the dependent variable data in cube 2 by selecting the remaining column index/indices from cube 2
  c2_vname <- c2_indx[-c(c2_xname, c2_yname, c2_tname)]
    
  # select the name of the dependent variable in cube 2 (or the dependent variable and the independent variables in cube 2)
  c2_varname <- colnames(cube2)[c2_vname]
    
  # convert the longitude and latitude data in cube 1 to numeric values
  cube1[,c1_xname] <- as.numeric(cube1[,c1_xname])
  cube1[,c1_yname] <- as.numeric(cube1[,c1_yname])
    
  # convert the dependent and independent variable data in cube 1 to numeric values
  for (i in 1:length(c1_vname)){
    cube1[,c1_vname[i]] <- as.numeric(cube1[,c1_vname[i]])
  }
    
  # convert the longitude and latitude data in cube 2 to numeric values
  cube2[,c2_xname] <- as.numeric(cube2[,c2_xname])
  cube2[,c2_yname] <- as.numeric(cube2[,c2_yname])
    
  # convert the dependent and independent variable data in cube 2 to numeric values
  for (i in 1:length(c2_vname)){
    cube2[,c2_vname[i]] <- as.numeric(cube2[,c2_vname[i]])
  }
    
  # ask the user for the reference date for the time data in cube 1 and cube 2
  c1_ref_date <- readline(prompt = "Enter the reference date for Cube 1 in YYYY-MM-DD format: ")
  c2_ref_date <- readline(prompt = "Enter the reference date for Cube 2 in YYYY-MM-DD format: ")
    
  # try to convert the time data in cube 1 to a POSIX and then to a Date and stop execution with an error if that is not possible
  tryCatch({
    # this code will convert time stored as a Date or as a string in Date format to a POSIX and then to a Date
    new_times <- as.Date(as.POSIXct(cube1[,c1_tname], origin = c1_ref_date))
      
    # some time date is stored as days since last millennium, so this code converts numeric time data to a POSIX and then to a Date
    # note that the above code will convert numeric time data, but it does so by miliseconds so the starting and ending time is the same
    if (min(new_times) == max(new_times)){
      new_times <- as.Date(as.POSIXct(cube1[,c1_tname]*86400, origin = c1_ref_date))
    }
      
    # replace the existing time data with the new Date time data
    cube1[,c1_tname] <- new_times
  },
  error = function(e){
    # return an error and stop execution if the time data in Cube 1 cannot be converted to a Date using the above methods
    stop('Error: The values in the time column of Cube 1 cannot be converted to dates')
  })
    
  # try to convert the time data in cube 2 to a POSIX and then to a Date and stop execution with an error if that is not possible
  tryCatch({
    # this code will convert time stored as a Date or as a string in Date format to a POSIX and then to a Date
    new_times <- as.Date(as.POSIXct(cube2[,c2_tname], origin = c2_ref_date))
    
    # some time date is stored as days since last millennium, so this code converts numeric time data to a POSIX and then to a Date
    # note that the above code will convert numeric time data, but it does so by miliseconds so the starting and ending time is the same
    if (min(new_times) == max(new_times)){
      new_times <- as.Date(as.POSIXct(cube2[,c2_tname]*86400, origin = c2_ref_date))
    }
      
    # replace the existing time data with the new Date time data
    cube2[,c2_tname] <- new_times
  },
  error = function(e){
    # return an error and stop execution if the time data in Cube 1 cannot be converted to a Date using the above methods
    stop('Error: The values in the time column of Cube 2 cannot be converted to dates')
  })
    
  # stop function execution and return an error if the temporal extents of the two cubes do not overlap
  if (max(cube1[,c1_tname]) < min(cube2[,c2_tname]) | max(cube2[,c2_tname]) < min(cube1[,c1_tname])){
    stop('Error: The temporal extents of the two cubes do not overlap.')
  }
    
  # find the minimum and maximum shared time for the two cubes
  min_shared <- max(min(cube1[,c1_tname]), min(cube2[,c2_tname]))
  max_shared <- min(max(cube1[,c1_tname]), max(cube2[,c2_tname]))
    
  # find the indices in each cube where the two cubes share time values
  cube1_intt <- which((cube1[,c1_tname] >= min_shared) & (cube1[,c1_tname] <= max_shared))
  cube2_intt <- which((cube2[,c2_tname] >= min_shared) & (cube2[,c2_tname] <= max_shared))
  
  # subset the two cubes to include only the data where the two cubes share time values
  cube1 <- cube1[cube1_intt,]
  cube2 <- cube2[cube2_intt,]
    
  # find the unique time values in each cube
  cube1_unique <- unique(cube1[,c1_tname])
  cube2_unique <- unique(cube2[,c2_tname])
    
  # find the temporal resolution in days of the time data in each cube
  cube1_tres <- difftime(cube1_unique[2], cube1_unique[1], units = 'days')
  cube2_tres <- difftime(cube2_unique[2], cube2_unique[1], units = 'days')
    
  tres_decision <- ""
    
  # if the temporal resolutions of the two cubes do not match, ask the user whether they want to use the coarser or finer temporal resolution
  if (cube1_tres != cube2_tres){
    tres_decision <- readline(prompt = "The temporal resolutions of the cubes do not match.  Would you for the final cube to use the coarser or finer temporal resolution?  Enter c for coarser resolution and f for finer resolution.")
  }
    
  # convert the temporal resolutions of both cubes to a numeric number of days
  cube1_tres <- as.numeric(cube1_tres)
  cube2_tres <- as.numeric(cube2_tres)
    
  # convert the cube with coarser temporal resolution to have the same temporal resolution as the finer temporal resolution cube
  if (tres_decision == 'f'){
    # find the smaller temporal resolution between the two cubes
    min_tres <- min(cube1_tres, cube2_tres)
      
    # set a string value that corresponds to the finer temporal resolution of the two cubes (days, weeks, months, or years)
    if (min_tres == 1){
      inc <- 'days'
    }
    else if (min_tres == 7){
      inc <- 'weeks'
    }
    else if (any(min_tres %in% c(30, 31))){
      inc <- 'months'
    }
    else if (any(min_tres %in% c(365, 366))){
      inc <- 'years'
    }
      
    # this code handles the case where cube 1 has finer temporal resolution and we need to update cube 2 such that it has the same temporal resolution as cube 1
    if (min_tres == cube1_tres){
      # make a new data frame for the new version of cube 2 with finer temporal resolution
      new_cube2 <- data.frame()
        
      # create a sequence from the smallest unique time value in cube 2 to the maximum unique time value in cube 2 using the increment corresponding to the temporal resolution of cube 1
      times <- seq(cube2_unique[1], cube2_unique[length(cube2_unique)], inc)
        
      # restrict the new time values for cube 2 to time values that are shared between cube 1 and cube 2 (to eliminate time values that are missing from cube 1)
      times <- times[which(times %in% cube1_unique)]
      
      # select the unique latitude values from cube 2
      lats <- unique(cube2[,c2_yname])
        
      # select the unique longitude values from cube 2
      lons <- unique(cube2[,c2_xname])
      
      # find the number of lat/long coordinates there are in cube 2 since this is the number of times that each new time value in cube 2 will need to be repeated
      reps <- length(lats)*length(lons)
      
      # create a new cube 2 containing all unique combinations of longitude, latitude, and time values for cube 2
      new_cube2 <- expand.grid(lons, lats, times) 
      
      # create a new vector for the values in the new version of cube 2
      vals <- c()
        
      # iterate over all of the variables in cube 2
      for (j in 1:length(c2_vname)){
        # find the value of the dependent variable in cube 2 that corresponds to the current time
        time_val <- cube2[which(cube2[,c2_tname] %in% times)[1], c2_vname[j]]
          
        # repeat the value from cube 2 the number of times that the time value will appear in the new cube 2
        vals <- rep(time_val, reps)
          
        # add the vector of repeated values of the current variable from cube 2 to the new cube 2
        new_cube2[,j+3] <- vals
      }
        
      # set the column names of the new version of cube 2 to be lon, lat, time, and the names of the other variables in cube 2
      colnames(new_cube2) <- c('lon', 'lat', 'time', c2_varname)
        
      # replace cube 2 with the new version of cube 2 with finer temporal resolution
      cube2 <- new_cube2
      
      # find the indices where the time values in cube 1 match the time values in cube 2
      c1_tinds <- which(cube1[,c1_tname] %in% cube2[,3])
        
      # subset cube 1 so that it only contains indices where the time values in cube 1 and cube 2 match
      cube1 <- cube1[c1_tinds,]
    }
    # this code handles the case where cube 2 has finer temporal resolution and we need to update cube 1 such that it has the same temporal resolution as cube 2
    if (min_tres == cube2_tres){
      # make a new data frame for the new version of cube 1 with finer temporal resolution
      new_cube1 <- data.frame()
        
      # create a sequence from the smallest unique time value in cube 1 to the maximum unique time value in cube 1 using the increment corresponding to the temporal resolution of cube 2
      times <- seq(cube1_unique[1], cube1_unique[length(cube1_unique)], inc)
        
      # restrict the new time values for cube 1 to time values that are shared between cube 1 and cube 2 (to eliminate time values that are missing from cube 2)
      times <- times[which(times %in% cube2_unique)]
        
      # select the unique latitude values from cube 1
      lats <- unique(cube1[,c1_yname])
        
      # select the unique longitude values from cube 1
      lons <- unique(cube1[,c1_xname])
        
      # find the number of lat/long coordinates there are in cube 1 since this is the number of times that each new time value in cube 1 will need to be repeated
      reps <- length(lats)*length(lons)
        
      # create a new cube 1 containing all unique combinations of longitude, latitude, and time values for cube 1
      new_cube1 <- expand.grid(lons, lats, times)
        
      # create a new vector for the values in the new version of cube 1
      vals <- c()
        
      # iterate over all of the variables in cube 1
      for (j in 1:length(c1_vname)){
        # find the value of the current variable in cube 1 at the current new time value for cube 1
        time_val <- cube1[which(cube1[,c1_tname] %in% times)[1], c1_vname[j]]
          
        # repeat the value of the variable the number of times that the time value will appear in the new cube 1
        vals <- rep(time_val, reps)
          
        # add the vector of repeated values of the current variable from cube 1 to the new cube 1
        new_cube1[,j+3] <- vals
      }
        
      # set the column names of the new version of cube 2 to be lon, lat, time, and the names of the other variables in cube 2
      colnames(new_cube1) <- c('lon', 'lat', 'time', c1_varname)
        
      # replace cube 2 with the new version of cube 2 with finer temporal resolution
      cube1 <- new_cube1
        
      # find the indices where the time values in cube 1 match the time values in cube 2
      c2_tinds <- which(cube2[,c2_tname] %in% cube1[,3])
        
      # subset cube 1 so that it only contains indices where the time values in cube 1 and cube 2 match
      cube2 <- cube2[c2_tinds,]
    }
  }else{
    # find the indices where the time values in cube 1 match the time values in cube 2
    c1_tinds <- which(cube1[,c1_tname] %in% cube2[,c2_tname])
    # find the indices where the time values in cube 2 match the time values in cube 1
    c2_tinds <- which(cube2[,c2_tname] %in% cube1[,c1_tname])
    
    # subset cube 1 and cube 2 such that the time values in both cubes are shared between those cubes
    cube1 <- cube1[c1_tinds,]
    cube2 <- cube2[c2_tinds,]
      
    # rename the latitude, longitude, and time columns in cube 1
    colnames(cube1)[c1_yname] <- 'lat'
    colnames(cube1)[c1_xname] <- 'lon'
    colnames(cube1)[c1_tname] <- 'time'
      
    # rename the latitude, longitude, and time columns in cube 2
    colnames(cube2)[c2_yname] <- 'lat'
    colnames(cube2)[c2_xname] <- 'lon'
    colnames(cube2)[c2_tname] <- 'time'
  }
    
  # convert both cubes to SF objects using longitude and latitude as the coordinates and the WGS84 projection - this is done to reproject the coordinates into a standard projection
  cube1_sf <- st_as_sf(x = cube1, coords = c('lon', 'lat'), crs = '+proj=longlat +datum=WGS84')
  cube2_sf <- st_as_sf(x = cube2, coords = c('lon', 'lat'), crs = '+proj=longlat +datum=WGS84')
    
  # convert both cubes back to data frames, extracting the geometry coordinates into lon and lat columns
  cube1_tmp <- as.data.frame(tidyr::extract(cube1_sf, geometry, into = c('lon', 'lat'), '\\((.*),(.*)\\)'))
  cube2_tmp <- as.data.frame(tidyr::extract(cube2_sf, geometry, into = c('lon', 'lat'), '\\((.*),(.*)\\)'))
    
  # remove the unnecessary geometry columns from cube 1
  cube1_tmp <- cube1_tmp %>% dplyr::select(!c('geometry'))
  cube1_tmp <- cube1_tmp %>% dplyr::select(!c('geometry'))
    
  # find the indices of the longitude, latitude, time, and dependent/independent variable columns in cube 1, as well as the name of the dependent/independent variables in cube 1
  # this is done because converting to an SF object and then back to a data frame shifts the order of the columns
  c1_ind <- seq(1, length(colnames(cube1_tmp)))
  c1_xindx <- which(colnames(cube1_tmp) %in% xnames)
  c1_yindx <- which(colnames(cube1_tmp) %in% ynames)
  c1_tindx <- which(colnames(cube1_tmp) %in% tnames)
  c1_vindx <- c1_ind[-c(c1_xindx, c1_yindx, c1_tindx)]
  c1_varname <- colnames(cube1_tmp)[c1_vindx]
  
  # remove the unnecessary geometry columns from cube 2
  cube2_tmp <- cube2_tmp %>% dplyr::select(!c('geometry'))
  cube2_tmp <- cube2_tmp %>% dplyr::select(!c('geometry'))
    
  # find the indices of the longitude, latitude, time, and dependent/independent variable columns in cube 2, as well as the name of the dependent/independent variables in cube 2
  # this is done because converting to an SF object and then back to a data frame shifts the order of the columns
  c2_ind <- seq(1, length(colnames(cube2_tmp)))
  c2_xindx <- which(colnames(cube2_tmp) %in% xnames)
  c2_yindx <- which(colnames(cube2_tmp) %in% ynames)
  c2_tindx <- which(colnames(cube2_tmp) %in% tnames)
  c2_vindx <- c2_ind[-c(c2_xindx, c2_yindx, c2_tindx)]
  c2_varname <- colnames(cube2_tmp)[c2_vindx]
    
  # reorder the columns in cube 1 such that longitude is first, latitude is second, time is third, and the dependent/independent variable(s) are last, then rename the columns in cube 1 accordingly
  cube1_tmp <- dplyr::select(cube1_tmp, all_of(c(c1_xindx, c1_yindx, c1_tindx, c1_vindx)))
  colnames(cube1_tmp) <- c('lon', 'lat', 'time', c1_varname)
    
  # convert the longitude and latitude columns of cube 1 to numeric values (they get switched to characters during the conversion from an SF object back to a data frame)
  cube1_tmp$lon <- as.numeric(cube1_tmp$lon)
  cube1_tmp$lat <- as.numeric(cube1_tmp$lat)
    
  # reorder the columns in cube 2 such that longitude is first, latitude is second, time is third, and the dependent/independent variable(s) are last, then rename the columns in cube 2 accordingly
  cube2_tmp <- dplyr::select(cube2_tmp, all_of(c(c2_xindx, c2_yindx, c2_tindx, c2_vindx)))
  colnames(cube2_tmp) <- c('lon', 'lat', 'time', c2_varname)
    
  # convert the longitude and latitude columns of cube 1 to numeric values (they get switched to characters during the conversion from an SF object back to a data frame)
  cube2_tmp$lon <- as.numeric(cube2_tmp$lon)
  cube2_tmp$lat <- as.numeric(cube2_tmp$lat)
    
  # remove NA values from both cubes
  cube1_tmp <- na.omit(cube1_tmp)
  cube2_tmp <- na.omit(cube2_tmp)
    
  # find the minimum and maximum longitude values in cube 1
  c1_xmin <- min(cube1_tmp$lon)
  c1_xmax <- max(cube1_tmp$lon)
    
  # find the minimum and maximum latitude values in cube 1
  c1_ymin <- min(cube1_tmp$lat)
  c1_ymax <- max(cube1_tmp$lat)
    
  # find the minimum and maximum longitude values in cube 2
  c2_xmin <- min(cube2_tmp$lon)
  c2_xmax <- max(cube2_tmp$lon)
    
  # find the minimum and maximum latitude values in cube 2
  c2_ymin <- min(cube2_tmp$lat)
  c2_ymax <- max(cube2_tmp$lat)
    
  # find the longitudinal and latitudinal resolution of cube 1
  c1_xres <- resolution(cube1_tmp$lon)
  c1_yres <- resolution(cube1_tmp$lat)
    
  # find the longitudinal and latitudinal resolution of cube 2
  c2_xres <- resolution(cube2_tmp$lon)
  c2_yres <- resolution(cube2_tmp$lat)
    
  # find the minimum longitudinal and latitudinal resolution of the two cubes
  min_xres <- min(c1_xres, c2_xres)
  min_yres <- min(c1_yres, c2_yres)
    
  # find the maximum longitudinal and latitudinal resolution of the two cubes
  max_xres <- max(c1_xres, c2_xres)
  max_yres <- max(c1_yres, c2_yres)
    
  # find the minimum and maximum longitude and latitude values that combine to form the lat/long coordinates of the union of the spatial extents of the two cubes
  union_xmin <- min(c1_xmin, c2_xmin)
  union_xmax <- max(c1_xmax, c2_xmax)
  union_ymin <- min(c1_ymin, c2_ymin)
  union_ymax <- max(c1_ymax, c2_ymax)
  
  # find the minimum and maximum longitude and latitude values that combine to form the lat/long coordinates of the intersection of the spatial extents of the two cubes
  intersect_xmin <- max(c1_xmin, c2_xmin)
  intersect_xmax <- min(c1_xmax, c2_xmax)
  intersect_ymin <- max(c1_ymin, c2_ymin)
  intersect_ymax <- min(c1_ymax, c2_ymax)
    
  # determine the number of data points from each cube that would be lost if the spatial extents of both cubes were restricted to the intersection of the spatial extents of both cubes
  c1_subset_len <- length(cube1_tmp[(cube1_tmp$lon < intersect_xmin) & (cube1_tmp$lon > intersect_xmax) & (cube1_tmp$lat < intersect_ymin) & (cube1_tmp$lat > intersect_ymax),1])
  c2_subset_len <- length(cube2_tmp[(cube2_tmp$lon < intersect_xmin) & (cube2_tmp$lon > intersect_xmax) & (cube2_tmp$lat < intersect_ymin) & (cube2_tmp$lat > intersect_ymax),1])
    
  # determine the fraction of data points in each cube that would be lost by restricting the spatial extents of both cubes to the intersection of the spatial extents of both cubes
  c1_lost_data <- c1_subset_len/(length(cube1_tmp$lon))
  c2_lost_data <- c2_subset_len/(length(cube2_tmp$lon))
    
  extent_decision = ''
  resolution_decision = ''
  
  # if the spatial extents and resolutions of the two cubes perfectly match, then just combine the dependent variables from both cubes with the lon/lat/time data from one cube and return the combined cube
  if (c1_xmin == c2_xmin && c1_xmax == c2_xmax && c1_ymin == c2_ymin && c1_ymax == c2_ymax && c1_xres == c2_xres && c1_yres == c2_yres){
    combined_cube <- data.frame(cube1_tmp$lon, cube1_tmp$lat, cube1_tmp[,c1_vindx], cube2_tmp[,c2_vindx])
    colnames(combined_cube) <- c(colnames(cube1), c2_varname)
    return(combined_cube)
  }
    
  # if the spatial extents of the two cubes do not match, inform the user and let them choose between the intersection or the union of the spatial extents for the combined cube
  if (c1_xmin != c2_xmin || c1_xmax != c2_xmax || c1_ymin != c2_ymin || c1_ymax != c2_ymax){
    extent_decision <- readline(prompt = paste0('The spatial extents of the two cubes do not match.  Would you like to perform kriging on the intersection of the cubes or on the union of the cubes?  Enter i for intersection and u for union.  Note that performing kriging on the intersection will result in a loss of ', 100*c1_lost_data, '% of the data in Cube 1 and ', 100*c2_lost_data, '% of the data in Cube 2 but will produce more accurate results than performing unbounded kriging on the union of the cubes.'))
  }
    
  # if the spatial resolutions of the two cubes do not match, inform the user and let them choose between the coarser and finer spatial resolutions for the combined cube
  if (c1_xres != c2_xres || c1_yres != c2_yres){
    resolution_decision <- readline(prompt = 'The spatial resolutions of the two cubes do not match.  Would you like to perform kriging so that the resulting cube has the same resolution as the finer resolution cube or the coarser resolution cube?  Enter f for finer resolution and c for coarser resolution')
  }
    
  # the following code generates the vectors of longitude and latitude coordinates where we want kriging predictions of the dependent variables for different user selections of spatial extent and spatial resolutions
  # generate the vectors for the intersection and coarser resolution
  if (extent_decision == 'i' && resolution_decision == 'c'){
    krige_lons <- seq(intersect_xmin, intersect_xmax, by = max_xres)
    krige_lats <- seq(intersect_ymin, intersect_ymax, by = max_yres)
  }
  # generate the vectors for the intersection and finer resolution
  if (extent_decision == 'i' && resolution_decision == 'f'){
    krige_lons <- seq(intersect_xmin, intersect_xmax, by = min_xres)
    krige_lats <- seq(intersect_ymin, intersect_ymax, by = min_yres)
  }
  # generate the vectors for the union and coarser resolution
  if (extent_decision == 'u' && resolution_decision == 'c'){
    krige_lons <- seq(union_xmin, union_xmax, by = max_xres)
    krige_lats <- seq(union_ymin, union_ymax, by = max_yres)
  }
  # generate the vectors for the union and finer resolution
  if (extent_decision == 'u' && resolution_decision == 'f'){
    krige_lons <- seq(union_xmin, union_xmax, by = min_xres)
    krige_lats <- seq(union_ymin, union_ymax, by = min_yres)
  }
  # generate the vectors for the intersection with matching resolution
  if (extent_decision == 'i' && resolution_decision == ''){
    krige_lons <- seq(intersect_xmin, intersect_xmax, by = c1_xres)
    krige_lats <- seq(intersect_ymin, intersect_ymax, by = c1_yres)
  }
  # generate the vectors for the union and matching resolution
  if (extent_decision == 'u' && resolution_decision == ''){
    krige_lons <- seq(union_xmin, union_xmax, by = c1_xres)
    krige_lats <- seq(union_ymin, union_ymax, by = c1_yres)
  }
  # generate the vectors for matching extent and coarser resolution
  if (extent_decision == '' && resolution_decision == 'c'){
    krige_lons <- seq(c1_xmin, c1_xmax, by = min_xres)
    krige_lats <- seq(c1_ymin, c1_ymax, by = min_yres)
  }
  # generate the vectors for matching extent and finer resolution
  if (extent_decision == '' && resolution_decision == 'f'){
    krige_lons <- seq(c1_xmin, c1_xmax, by = max_xres)
    krige_lats <- seq(c1_ymin, c1_ymax, by = max_yres)
  }
    
  # determine the number of rows that will be in the combined cube
  n <- length(krige_lons)*length(krige_lats)*length(unique(cube1_tmp$time))
  
  # determine the number of rows that will be in one time slice of the combined cube
  m <- length(krige_lons)*length(krige_lats)
    
  # if there are multiple dependent/independent variables in cube 1, ask the user to enter a spatial dependency formula to use when kriging cube 1, then determine the name and index of the dependent variable in the cube
  if (length(c1_vindx) > 1){
    c1_formula <- readline(prompt = 'Enter the spatial dependency formula that you would like to use during kriging for Cube 1.  Note that variable names must be exactly the same as they appear in the data frame column names and the formula should be entered without spaces.  This formula should use all of the variables in Cube 1, but does not have to include latitude or longitude: ')
    c1_dep_var <- str_split(c1_formula, '~')[[1]][1]
    c1_dep_indx <- which(colnames(cube1_tmp) %in% c1_dep_var)
    #c1_ind_indx <- c1_vindx[-c(c1_dep_indx)]
    
    c1_formula <- as.formula(c1_formula)
  }
    
  # if there are multiple dependent/independent variables in cube 2, ask the user to enter a spatial dependency formula to use when kriging cube 2, then determine the name and index of the dependent variable in the cube
  if (length(c2_vindx) > 1){
    c2_formula <- readline(prompt = 'Enter the spatial dependency formula that you would like to use during kriging for Cube 2.  Note that variable names must be exactly the same as they appear in the data frame column names and the formula should be
                           entered without spaces.  This formula should use all of the variables in Cube 1, but does not have to include latitude or longitude: ')
    c2_dep_var <- str_split(c2_formula, '~')[[1]][1]
    c2_dep_indx <- which(colnames(cube2_tmp) %in% c2_dep_var)
    #c2_ind_indx <- c2_vindx[-c(c2_dep_indx)]
      
    c2_formula <- as.formula(c2_formula)
  }
    
  # create two matrices full of NA values that have the same number of rows as the combined cube will, each with 2 columns which will contain the values from kriging each cube
  krige_c1 <- matrix(NA, nrow = n, ncol = 2)
  krige_c2 <- matrix(NA, nrow = n, ncol = 2)
    
  # keep track of the current row value in each matrix of kriging values
  c1_indx <- 1
  c2_indx <- 1
  
  # create vectors that will hold the column names for each of the above matrices
  c1_names <- c()
  c2_names <- c()
  
  # iterate over all of the time slices in each cube and perform kriging on both cubes for each time slice
  for (t in 1:length(unique(cube1_tmp$time))){
    # select the current time value
    time <- unique(cube1_tmp$time)[t]
    
    # subset cube 1 and cube 2 to just the data points at the current time value
    c1 <- cube1_tmp[cube1_tmp$time == time,]
    c2 <- cube2_tmp[cube2_tmp$time == time,]
      
    # this code handles the case where cube 1 only has a dependent variable
    if (length(c1_vindx) == 1){
      # remove time data from cube 1 b/c the gstat::krige function does not allow extra columns in the data frame that are not used in the spatial dependency formula or contain locations for the spatial data
      tmp <- dplyr::select(c1, -c('time'))
      # rename the dependent variable to var so that we know what name to use in the defualt spatial dependency formula for kriging
      colnames(tmp) <- c('lon', 'lat', 'var')
      # convert the cube 1 data to a spatial points data frame using longitude and latitude data as coordinates and the WGS84 projection
      coordinates(tmp) <- c('lon', 'lat')
      proj4string(tmp) <- crs('+proj=longlat +datum=WGS84')
        
      # use the krige_cubes function to perform kriging on the data in cube 1 using the default spatial dependency formula var ~ lon + lat and store the resulting data frame of kriging values
      c1_krige <- krige_cubes(tmp, krige_lons, krige_lats, c1_varname)
        
      # if we are in the first time slice for cube 1, add the names from the kriging cube to the list of column names for cube 1
      if (t == 1){
        c1_names <- c(c1_names, colnames(c1_krige))
      }
        
      # convert the data frame of kriging values to a matrix
      c1_krige <- as.matrix(c1_krige)
        
      # iterate over the rows in the kriging values matrix and add each row of kriging values to the next row in the matrix of kriging values for all time slices in cube 1, updating the index of the next row each time
      for (j in 1:dim(c1_krige)[1]){
        krige_c1[c1_indx,] <- c1_krige[j,]
        c1_indx <- c1_indx + 1
      }
        
      # this code was an attempt at doing all of this work in a single larger matrix, but returns errors when run
      #krige_c1[((t-1)*m)+1:t*m,] <- as.matrix(c1_krige)
    }
    # this code handles the case where cube 1 contains a dependent variable and one or more indpendent variables
    else{
      # remove time data from cube 1 b/c the gstat::krige function does not allow extra columns in the data frame that are not used in the spatial dependency formula or contain locations for the spatial data
      tmp <- dplyr::select(c1, -c('time'))
      # convert the cube 1 data to a spatial points data frame using longitude and latitude data as coordinates and the WGS84 projection
      coordinates(tmp) <- c('lon', 'lat')
      proj4string(tmp) <- crs('+proj=longlat +datum=WGS84')
        
      # use the krige_cubes function to perform kriging on the data in cube 1 using the given spatial dependency formula and store the resulting data frame of kriging values
      c1_krige <- krige_cubes(tmp, krige_lons, krige_lats, c1_dep_var, c1_formula)
      
      # if we are in the first time slice for cube 1, add the names from the kriging cube to the list of column names for cube 1
      if (t == 1){
        c1_names <- c(c1_names, colnames(c1_krige))
      }
        
      # convert the data frame of kriging values to a matrix
      c1_krige <- as.matrix(c1_krige)
        
      # iterate over the rows in the kriging values matrix and add each row of kriging values to the next row in the matrix of kriging values for all time slices in cube 1, updating the index of the next row each time
      for (j in 1:dim(c1_krige)[1]){
        krige_c1[c1_indx,] <- c1_krige[j,]
        c1_indx <- c1_indx + 1
      }
        
      # this code was an attempt at doing all of this work in a single larger matrix, but returns errors when run
      #krige_c1[((t-1)*m)+1:t*m,1:2] <- as.matrix(c1_krige) 
        
      # this code attempted to also krige the ancillary data using the default spatial dependency formula, but it's broken
      #for (i in 1:length(c1_ind_indx)){
      #c1_ind_var <- colnames(cube1_tmp)[c1_ind_indx[i]]
      #tmp <- dplyr::select(c1, c('lon', 'lat', c1_ind_var))
      #coordinates(tmp) <- c('lon', 'lat')
      #proj4string(tmp) <- crs('+proj=longlat +datum=WGS84')
      #c1_krige <- krige_cubes(tmp, krge_lons, krige_lats, c1_ind_var)
      
      #c1_names <- c(c1_names, colnames(c1_krige))
      
      #krige_c1[((t-1)*m)+1:t*m,(2*i)+1:2*(i+1)] <- as.matrix(c1_krige) 
      #}
    }
      
    # this code handles the case where cube 2 only has a dependent variable
    if (length(c2_vindx) == 1){
      # rename the dependent variable in cube 2 to be var so that we know what name to use in the spatial dependency formula for kriging
      colnames(c2) <- c('lon', 'lat', 'time', 'var')
      # remove time data from cube 1 b/c the gstat::krige function does not allow extra columns in the data frame that are not used in the spatial dependency formula or contain locations for the spatial data
      tmp <- dplyr::select(c2, -c('time'))
      # convert the cube 1 data to a spatial points data frame using longitude and latitude data as coordinates and the WGS84 projection
      coordinates(tmp) <- c('lon', 'lat')
      proj4string(tmp) <- crs('+proj=longlat +datum=WGS84')
        
      # use the krige_cubes function to perform kriging on the data in cube 2 using the default spatial dependency formula var ~ lon + lat and store the resulting data frame of kriging values
      c2_krige <- krige_cubes(tmp, krige_lons, krige_lats, c2_varname)
        
      # if we are in the first time slice for cube 2, add the names from the kriging cube to the list of column names for cube 2
      if (t == 1){
        c2_names <- c(c2_names, colnames(c2_krige))
      }
        
      # convert the data frame of kriging values to a matrix
      c2_krige <- as.matrix(c2_krige)
      
      # iterate over the rows in the kriging values matrix and add each row of kriging values to the next row in the matrix of kriging values for all time slices in cube 2, updating the index of the next row each time
      for (j in 1:dim(c2_krige)[1]){
        krige_c2[c2_indx,] <- c2_krige[j,]
        c2_indx <- c2_indx + 1
      }
        
      # this code was an attempt at doing all of this work in a single larger matrix, but returns errors when run
      #krige_c2[((t-1)*m)+1:t*m,] <- as.matrix(c2_krige)
    }
    # this code handles the case where cube 2 has a dependent variable and one or more independent variables
    else{
      # remove time data from cube 1 b/c the gstat::krige function does not allow extra columns in the data frame that are not used in the spatial dependency formula or contain locations for the spatial data
      tmp <- dplyr::select(c2, -c('time'))
      # convert the cube 1 data to a spatial points data frame using longitude and latitude data as coordinates and the WGS84 projection
      coordinates(tmp) <- c('lon', 'lat')
      proj4string(tmp) <- crs('+proj=longlat +datum=WGS84')
        
      # use the krige_cubes function to perform kriging on the data in cube 2 using the given spatial dependency formula and store the resulting data frame of kriging values
      c2_krige <- krige_cubes(tmp, krige_lons, krige_lats, c2_dep_var, c2_formula)
      
      # if we are in the first time slice for cube 2, add the names from the kriging cube to the list of column names for cube 2
      if (t == 1){
        c2_names <- c(c2_names, colnames(c2_krige))
      }
        
      # convert the data frame of kriging values to a matrix
      c2_krige <- as.matrix(c2_krige)
        
      # iterate over the rows in the kriging values matrix and add each row of kriging values to the next row in the matrix of kriging values for all time slices in cube 1, updating the index of the next row each time
      for (j in 1:dim(c2_krige)[1]){
        krige_c2[c2_indx,] <- c2_krige[j,]
        c2_indx <- c2_indx + 1
      }
        
      # this code was an attempt at doing all of this work in a single larger matrix, but returns errors when run
      #krige_c2[((t-1)*m)+1:t*m,1:2] <- as.matrix(c2_krige) 
        
      # this code attempted to also krige the ancillary data using the default spatial dependency formula, but it's broken
      #for (i in 1:length(c2_ind_indx)){
      #c2_ind_var <- colnames(cube2_tmp)[c2_ind_indx[i]]
      #tmp <- dplyr::select(c2, c('lon', 'lat', c2_ind_var))
      #coordinates(tmp) <- c('lon', 'lat')
      #proj4string(tmp) <- crs('+proj=longlat +datum=WGS84')
      #c2_krige <- krige_cubes(tmp, krge_lons, krige_lats, c2_ind_var)
        
      #c2_names <- c(c2_names, colnames(c2_krige))
        
      #krige_c2[((t-1)*m)+1:t*m,(2*i)+1:2*(i+1)] <- as.matrix(c2_krige) 
      #}
    }
  }
    
  # combine the unique long/lat/time coordinates in the combined cube with the kriging values in cube 1 and the kriging values in cube 2 to create the combined cube
  combined_cube <- as.data.frame(cbind(expand.grid(krige_lons, krige_lats, unique(cube1_tmp$time)), krige_c1, krige_c2))
    
  # set the column names of the combined cube
  colnames(combined_cube) <- c('lon', 'lat', 'time', c1_names, c2_names)
  
  # return the combined cube to the user
  return(combined_cube)
}