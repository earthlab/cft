#' @title Unite spatial cubes
#'
#' @description This function combines two spatial data cubes which each have a single dependent variable and latitude and longitude data.
#' The function allows the user to select the spatial resolution and spatial extent they would like to have in the final data cube and performs kriging to obtain predictions of the dependent variable 
#' from each cube using the spatial dependency formula var ~ lon + lat to indicate that the dependent variable depends only on latitude and longitude.
#'
#' @param cube1 A spatial data cube with one dependent variable and some number of independent variables stored as a data frame (Data.frame)
#' @param cube2 A spatial data cube with one dependent variable and some number of independent variables stored as a data frame (Data.frame)
#'
#' @importFrom magrittr %>%
#' @importFrom cft
#' @importFrom sf
#' @importFrom sp
#' @importFrom gstat
#' @importFrom dplyr
#' @return Data.frame containing the latitude and longitude coordinates of the combined cube and the predicted values and variance for the dependent variables from both cubes from kriging 
#' @export
unite_cubes <- function(cube1, cube2){
  require(dplyr)
  require(cft)
  require(sf)
  require(sp)
  require(gstat)
  
  # define vectors containing the possible column names for the columns containing latitude and longitude data
  ynames <- c('lat', 'latitude', 'Lat', 'Latitude', 'y')
  xnames <- c('lon', 'long', 'longitude', 'Lon', 'Long', 'Longitude', 'x')
  
  # create a sequence of values corresponding to the indices of all the columns in cube 1
  c1_indx <- seq(1, length(colnames(cube1)))
  
  # find the indices of the columns containing longitude and latitude data in cube 1
  c1_xname <- which(colnames(cube1) %in% xnames)
  c1_yname <- which(colnames(cube1) %in% ynames)
  
  # find the index of the column containing the dependent variable data in cube 1 by selecting the remaining column index/indices from cube 1
  c1_vname <- c1_indx[-c(c1_xname, c1_yname)]
  
  # select the name of the dependent variable in cube 1 (or the dependent variable and independent variables in cube 1)
  c1_varname <- colnames(cube1)[c1_vname]
  
  # create a sequence of values corresponding to the indices of all the columns in cube 2
  c2_indx <- seq(1, length(colnames(cube2)))
  
  # find the indices of the columns containing longitude and latitude data in cube 2
  c2_xname <- which(colnames(cube2) %in% xnames)
  c2_yname <- which(colnames(cube2) %in% ynames)
  
  # find the index of the column containing the dependent variable data in cube 2 by selecting the remaining column index/indices from cube 2
  c2_vname <- c2_indx[-c(c2_xname, c2_yname)]
  
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
  
  # rename the longitude and latitude columns of cube 1
  colnames(cube1[c1_xname]) <- 'lon'
  colnames(cube1[c1_yname]) <- 'lat'
  
  # rename the longitude and latitude columns of cube 2
  colnames(cube2[c2_xname]) <- 'lon'
  colnames(cube2[c2_yname]) <- 'lat'
  
  # convert both cubes to SF objects using longitude and latitude as the coordinates and the WGS84 projection - this is done to reproject the coordinates into a standard projection
  cube1_sf <- st_as_sf(x = cube1, coords = c('lon', 'lat'), crs = '+proj=longlat +datum=WGS84')
  cube2_sf <- st_as_sf(x = cube2, coords = c('lon', 'lat'), crs = '+proj=longlat +datum=WGS84')
  
  # convert both cubes back to data frames, extracting the geometry coordinates into lon and lat columns
  cube1_tmp <- as.data.frame(tidyr::extract(cube1_sf, geometry, into = c('lon', 'lat'), '\\((.*),(.*)\\)'))
  cube2_tmp <- as.data.frame(tidyr::extract(cube2_sf, geometry, into = c('lon', 'lat'), '\\((.*),(.*)\\)'))
  
  # remove the unnecessary geometry columns from cube 1
  cube1_tmp <- cube1_tmp %>% dplyr::select(!c('geometry'))
  cube1_tmp <- cube1_tmp %>% dplyr::select(!c('geometry'))
  
  # find the indices of the longitude, latitude, and dependent/independent variable columns in cube 1, as well as the name of the dependent/independent variables in cube 1
  # this is done because converting to an SF object and then back to a data frame shifts the order of the columns
  c1_ind <- seq(1, length(colnames(cube1_tmp)))
  c1_xindx <- which(colnames(cube1_tmp) %in% xnames)
  c1_yindx <- which(colnames(cube1_tmp) %in% ynames)
  c1_vindx <- c1_ind[-c(c1_xindx, c1_yindx)]
  c1_varname <- colnames(cube1_tmp)[c1_vindx]
  
  # remove the unnecessary geometry columns from cube 2
  cube2_tmp <- cube2_tmp %>% dplyr::select(!c('geometry'))
  cube2_tmp <- cube2_tmp %>% dplyr::select(!c('geometry'))
  
  # find the indices of the longitude, latitude, and dependent/independent variable columns in cube 2, as well as the name of the dependent/independent variables in cube 2
  # this is done because converting to an SF object and then back to a data frame shifts the order of the columns
  c2_ind <- seq(1, length(colnames(cube2_tmp)))
  c2_xindx <- which(colnames(cube2_tmp) %in% xnames)
  c2_yindx <- which(colnames(cube2_tmp) %in% ynames)
  c2_vindx <- c2_ind[-c(c2_xindx, c2_yindx)]
  c2_varname <- colnames(cube2_tmp)[c2_vindx]
  
  # reorder the columns in cube 1 such that longitude is first, latitude is second, and the dependent/independent variable(s) are last, then rename the columns in cube 1 accordingly
  cube1_tmp <- dplyr::select(cube1_tmp, all_of(c(c1_xindx, c1_yindx, c1_vindx)))
  colnames(cube1_tmp) <- c('lon', 'lat', c1_varname)
  
  # convert the longitude and latitude columns of cube 1 to numeric values (they get switched to characters during the conversion from an SF object back to a data frame)
  cube1_tmp$lon <- as.numeric(cube1_tmp$lon)
  cube1_tmp$lat <- as.numeric(cube1_tmp$lat)
  
  # reorder the columns in cube 2 such that longitude is first, latitude is second, and the dependent/independent variable(s) are last, then rename the columns in cube 2 accordingly
  cube2_tmp <- dplyr::select(cube2_tmp, all_of(c(c2_xindx, c2_yindx, c2_vindx)))
  colnames(cube2_tmp) <- c('lon', 'lat', c2_varname)
  
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
  
  # if the spatial extents and resolutions of the two cubes perfectly match, then just combine the dependent variables from both cubes with the lon/lat data from one cube and return the combined cube
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
  n <- length(krige_lons)*length(krige_lats)
  
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
    
   # this code handles the case where cube 1 only has a dependent variable
   if (length(c1_vindx) == 1){
     c1 <- cube1_tmp
     # rename the dependent variable to var so that we know what name to use in the defualt spatial dependency formula for kriging
     colnames(c1) <- c('lon', 'lat', 'var')
     # convert the cube 1 data to a spatial points data frame using longitude and latitude data as coordinates and the WGS84 projection
     coordinates(c1) <- c('lon', 'lat')
     proj4string(c1) <- crs('+proj=longlat +datum=WGS84')
      
     # use the krige_cubes function to perform kriging on the data in cube 1 using the default spatial dependency formula var ~ lon + lat and store the resulting data frame of kriging values
     krige_c1<- krige_cubes(c1, krige_lons, krige_lats, c1_varname)
      
     c1_names <- c(c1_names, colnames(c1_krige))
   }
   # this code handles the case where cube 1 contains a dependent variable and one or more indpendent variables
   else{
     c1 <- cube1_tmp
     # convert the cube 1 data to a spatial points data frame using longitude and latitude data as coordinates and the WGS84 projection
     coordinates(c1) <- c('lon', 'lat')
     proj4string(c1) <- crs('+proj=longlat +datum=WGS84')
      
     # use the krige_cubes function to perform kriging on the data in cube 1 using the given spatial dependency formula and store the resulting data frame of kriging values
     krige_c1 <- krige_cubes(c1, krige_lons, krige_lats, c1_dep_var, c1_formula)
      
     c1_names <- c(c1_names, colnames(c1_krige))
      
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
     c2 <- cube2_tmp
     # rename the dependent variable in cube 2 to be var so that we know what name to use in the spatial dependency formula for kriging
     colnames(c2) <- c('lon', 'lat', 'var')
     # convert the cube c data to a spatial points data frame using longitude and latitude data as coordinates and the WGS84 projection
     coordinates(c2) <- c('lon', 'lat')
     proj4string(c2) <- crs('+proj=longlat +datum=WGS84')
      
     # use the krige_cubes function to perform kriging on the data in cube 2 using the default spatial dependency formula var ~ lon + lat and store the resulting data frame of kriging values
     krige_c2 <- krige_cubes(c2, krige_lons, krige_lats, c2_varname)
      
     c2_names <- c(c2_names, colnames(c2_krige))
      
     # this code was an attempt at doing all of this work in a single larger matrix, but returns errors when run
     #krige_c2[((t-1)*m)+1:t*m,] <- as.matrix(c2_krige)
   }
   # this code handles the case where cube 2 has a dependent variable and one or more independent variables
   else{
     c2 <- cube2_tmp
     # convert the cube 1 data to a spatial points data frame using longitude and latitude data as coordinates and the WGS84 projection
     coordinates(c2) <- c('lon', 'lat')
     proj4string(c2) <- crs('+proj=longlat +datum=WGS84')
      
     # use the krige_cubes function to perform kriging on the data in cube 2 using the given spatial dependency formula and store the resulting data frame of kriging values
     krige_c2 <- krige_cubes(c2, krige_lons, krige_lats, c2_dep_var, c2_formula)
      
     c2_names <- c(c2_names, colnames(c2_krige))
      
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
  
  # combine the unique long/lat/time coordinates in the combined cube with the kriging values in cube 1 and the kriging values in cube 2 to create the combined cube
  combined_cube <- as.data.frame(cbind(expand.grid(krige_lons, krige_lats), krige_c1, krige_c2))
  
  # set the column names of the combined cube
  colnames(combined_cube) <- c('lon', 'lat', c1_names, c2_names)
  
  # return the combined cube to the user
  return(combined_cube)
}