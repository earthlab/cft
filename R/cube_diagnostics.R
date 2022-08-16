#' @title Cube diagnostics
#'
#' @description This function takes two spatiotemporal data cubes and runs diagnostic checks on the cubes to ensure that they can be combined using the unite_cubes function in the CFT package.  This function will stop execution and return an error
#' if the given data cubes cannot be combined using the CFT unite_cubes function and will return a data frame containing relevant diagnostic information about the two data cubes if the two cubes can be combined using the CFT unite_cubes function.
#'
#' @param cube1 A spatiotemporal data cube with one dependent variable and some number of independent variables (Data.frame)
#' @param cube2 A spatiotemporal data cube with one dependent variable and some number of independent variables (Data.frame)
#'
#' @importFrom magrittr %>%
#' @importFrom sf
#' @importFrom sp
#' @importFrom dplyr
#' @return Data.frame containing diagnostic information about the two data cubes
#' @export
cube_diagnostics <- function(cube1, cube2){
  require(sf)
  require(sp)
  require(dplyr)
  
  # define a vector of possible column names containing latitude data
  ynames <- c('lat', 'latitude', 'Lat', 'Latitude', 'y')
  # define a vector of possible column names containing longitude data
  xnames <- c('lon', 'long', 'longitude', 'Lon', 'Long', 'Longitude', 'x')
  # define a vector of possible column names containing time data
  tnames <- c('time', 'day', 'hour', 'month', 'year', 'date')
    
  # check if cube 1 is formatted as a data frame and if not, stop execution with an error
  if (is.data.frame(cube1) == FALSE){
    stop('Error: Cube 1 is not a dataframe.  Please convert Cube 1 to a dataframe and try again.')
  }
  # check if cube 2 is formatted as a data frame and if not, stop execution with an error
  if (is.data.frame(cube2) == FALSE){
    stop('Error: Cube 2 is not a dataframe.  Please convert Cube 2 to a dataframe and try again.')
  }
  # check if cube 1 is empty and if so, stop execution with an error
  if (nrow(cube1) == 0 || ncol(cube1) == 0){
    stop('Error: Cube 1 is empty.  Add data to Cube 1 and try again.')
  }
  # check if cube 2 is empty and if so, stop execution with an error
  if (nrow(cube2) == 0 || ncol(cube2) == 0){
    stop('Error: Cube 2 is empty.  Add data to Cube 2 and try again.')
  }
  #if (ncol(cube1) > 4){
    #stop('Error: Cube 1 contains more than 1 variable.  You will need to create separate data cubes (with lat/long/time data) for each variable in Cube 1 and then try again using each subcube separately.')
  #}
  #if (ncol(cube2) > 4){
    #stop('Error: Cube 2 contains more than 1 variable.  You will need to create separate data cubes (with lat/long/time data) for each variable in Cube 1 and then try again using each subcube separately.')
  #}
  # check if cube 1 contains latitude data with one of the previously specified column names and if not, stop execution with an error
  if (any(c('lat', 'latitude', 'Lat', 'Latitude', 'y') %in% colnames(cube1)) == FALSE){
    stop("Error: Cube 1 does not contain latitude data.  Your cube must contain spatial data and your latitude data must be in a column labeled 'lat', 'latitude', 'Lat', or 'Latitude'.")
  }
  # check if cube 1 contains longitude data with one of the previously specified column names and if not, stop execution with an error
  if (any(c('lon', 'long', 'longitude', 'Lon', 'Long', 'Longitude', 'x') %in% colnames(cube1)) == FALSE){
    stop("Error: Cube 1 does not contain longitude data.  Your cube must contain spatial data and your longitude data must be in a column labeled 'lon', 'long', 'longitude', 'Lon', 'Long', or 'Longitude'.")
  }
  # check if cube 1 contains time data with one of the previously specified column names and if not, stop execution with an error
  if (any(c('time', 'day', 'hour', 'month', 'year', 'date') %in% colnames(cube1)) == FALSE){
    stop("Error: Cube 1 does not contain time data.  Your cube must contain temporal data and your time data must be in a column labeled 'time', 'day', 'hour', 'month', 'year', or 'date'.")
  }
  # check if cube 2 contains time data with one of the previously specified column names and if not, stop execution with an error
  if (any(c('time', 'day', 'hour', 'month', 'year', 'date') %in% colnames(cube2)) == FALSE){
    stop("Error: Cube 2 does not contain time data.  Your cube must contain temporal data and your time data must be in a column labeled 'time', 'day', 'hour', 'month', 'year', or 'date'.")
  }
  # check if cube 2 contains latitude data with one of the previously specified column names and if not, stop execution with an error
  if (any(c('lat', 'latitude', 'Lat', 'Latitude', 'y') %in% colnames(cube2)) == FALSE){
    stop("Error: Cube 2 does not contain latitude data.  Your cube must contain spatial data and your latitude data must be in a column labeled 'lat', 'latitude', 'Lat', or 'Latitude'.")
  }
  # check if cube 2 contains longitude data with one of the previously specified column names and if not, stop execution with an error
  if (any(c('lon', 'long', 'longitude', 'Lon', 'Long', 'Longitude', 'x') %in% colnames(cube2)) == FALSE){
    stop("Error: Cube 2 does not contain longitude data.  Your cube must contain spatial data and your longitude data must be in a column labeled 'lon', 'long', 'longitude', 'Lon', 'Long', or 'Longitude'.")
  }
  
  # find the index of the column containing longitude data in cube 1
  c1_xname <- which(colnames(cube1) %in% xnames)
  # find the index of the column containing latitude data in cube 1
  c1_yname <- which(colnames(cube1) %in% ynames)
  # find the index of the column containing time data in cube 1
  c1_tname <- which(colnames(cube1) %in% tnames)
  
  # find the index of the column containing longitude data in cube 2
  c2_xname <- which(colnames(cube2) %in% xnames)
  # find the index of the column containing latitude data in cube 2
  c2_yname <- which(colnames(cube2) %in% ynames)
  # find the index of the column containing time data in cube 2
  c2_tname <- which(colnames(cube2) %in% tnames)
  
  # Dimension Check
  # find the x dimension of cube 1, which corresponds to the length of unique longitude values in cube 1
  c1_xdim <- length(unique(cube1[,c1_xname]))
  # find the y dimension of cube 1, which corresponds to the length of unique latitude values in cube 1
  c1_ydim <- length(unique(cube1[,c1_yname]))
  # find the z dimension of cube 1, which corresponds to the length of unique time values in cube 1
  c1_zdim <- length(unique(cube1[,c1_tname]))
  
  # find the x dimension of cube 2, which corresponds to the length of unique longitude values in cube 2
  c2_xdim <- length(unique(cube2[,c2_xname]))
  # find the y dimension of cube 2, which corresponds to the length of unique latitude values in cube 2
  c2_ydim <- length(unique(cube2[,c2_yname]))
  # find the z dimension of cube 2, which corresponds to the length of unique time values in cube 2
  c2_zdim <- length(unique(cube2[,c2_tname]))
  
  # check if the x dimension of cube 1 and cube 2 are equal
  xdim_check <- c1_xdim == c2_xdim
  # check if the y dimension of cube 1 and cube 2 are equal
  ydim_check <- c1_ydim == c2_ydim
  # check if the z dimension of cube 1 and cube 2 are equal
  zdim_check <- c1_zdim == c2_zdim
  
  # find the difference between the x dimension of the smaller cube and the x dimension of the larger cube
  xdim_dif <- abs(c1_xdim - c2_xdim)
  # find the difference between the y dimension of the smaller cube and the y dimension of the larger cube
  ydim_dif <- abs(c1_ydim - c2_ydim)
  # find the difference between the z dimension of the smaller cube and the z dimension of the larger cube
  zdim_dif <- abs(c1_zdim - c2_zdim)
  
  # use the difference between the x dimension of cube 1 and the x dimension of cube 2 to determine which cube has the larger x dimension
  if (c1_xdim - c2_xdim > 0){
    xdim_larger <- 'Cube 1'
  } else{
    xdim_larger <- 'Cube 2'
  }
  
  # use the difference between the y dimension of cube 1 and the y dimension of cube 2 to determine which cube has the larger y dimension
  if (c1_ydim - c2_ydim > 0){
    ydim_larger <- 'Cube 1'
  } else{
    ydim_larger <- 'Cube 2'
  }
  
  # use the difference between the z dimension of cube 1 and the z dimension of cube 2 to determine which cube has the larger z dimension
  if (c1_zdim - c2_zdim > 0){
    zdim_larger <- 'Cube 1'
  } else{
    zdim_larger <- 'Cube 2'
  }
  
  # Resolution Check
  # find the longitudinal resolution of cube 1
  c1_xres <- resolution(cube1[,c1_xname])
  # find the latitudinal resolution of cube 1
  c1_yres <- resolution(cube1[,c1_yname])
  # find the temporal resolution of cube 1 
  c1_tres <- resolution(cube1[,c1_tname])
  
  # find the longitude resolution of cube 2
  c2_xres <- resolution(cube2[,c2_xname])
  # find the latitude resolution of cube 2
  c2_yres <- resolution(cube2[,c2_yname])
  # find the temporal resolution of cube 2
  c2_tres <- resolution(cube2[,c2_tname])
  
  # check whether the longitudinal resolution of cube 1 is the same as the longitudinal resolution of cube 2
  xres_check <- c1_xres == c2_xres
  # check whether the latitudinal resolution of cube 1 is the same as the latitudinal resolution of cube 2
  yres_check <- c1_yres == c2_yres
  # check whether the temporal resolution of cube 1 is the same as the temporal resolution of cube 2
  tres_check <- c1_tres == c2_tres
  
  # find the difference between the longitudinal resolution of the larger cube and the longitudinal resolution of the smaller cube
  xres_dif <- abs(c1_xres - c2_xres)
  # find the difference between the latitudinal resolution of the larger cube and the latitudinal resolution of the smaller cube 
  yres_dif <- abs(c1_yres - c2_yres)
  # find the difference between the temporal resolution of the larger cube and the temporal resolution of the smaller cube
  tres_dif <- abs(c1_tres - c2_tres)
  
  # use the difference between the longitudinal resolution of cube 1 and the longitudinal resolution of cube 2 to determine which cube has the larger longitudinal resolution
  if (c1_xres - c2_xres > 0){
    xres_larger <- 'Cube 1'
  } else{
    xres_larger <- 'Cube 2'
  }
  
  # use the difference between the latitudinal resolution of cube 1 and the latitudinal resolution of cube 2 to determine which cube has the larger latitudinal resolution
  if (c1_yres - c2_yres > 0){
    yres_larger <- 'Cube 1'
  } else{
    yres_larger <- 'Cube 2'
  }
    
  # use the difference between the temporal resolution of cube 1 and the temporal resolution of cube 2 to determine which cube has the larger temporal resolution
  if (c1_tres - c2_tres > 0){
    tres_larger <- 'Cube 1'
  } else{
    tres_larger <- 'Cube 2'
  }
  
  # Data Types Check
  # create a data frame of NA values with 1 row and the same number of columns as cube 1 to hold the data type of each column in cube 1
  c1_dtypes <- data.frame(matrix(NA, nrow = 1, ncol = length(colnames(cube1))))
  
  # iterate over the columns in cube 1, adding the data type of each column in cube 1 to its respective column in the new data frame
  for (i in 1:length(colnames(cube1))){
    c1_dtypes[1,i] <- typeof(cube1[,i])
  }
  
  # set the column names of the data frame to be the column names of cube 1
  colnames(c1_dtypes) <- colnames(cube1)
  
  # create a data frame of NA values with 1 row and the same number of columns as cube 2 to hold the data type of each column in cube 2
  c2_dtypes <- data.frame(matrix(NA, nrow = 1, ncol = length(colnames(cube2))))
  
  # iterate over the columns in cube 2, adding the data type of each column in cube 2 to its respective column in the new data frame
  for (j in 1:length(colnames(cube2))){
    c2_dtypes[1,j] <- typeof(cube2[,j])
  }
  
  # se the column names of the data frame to the column names of cube 2
  colnames(c2_dtypes) <- colnames(cube2)
  
  # Convert to Spatial Objects - this is done to reproject the data to a standard long/lat projection
  # convert cube 1 to an SF object using the longitude and latitude columns as the coordinates and a long/lat WGS84 projection
  cube1_sf <- st_as_sf(x = cube1, coords = c(colnames(cube1)[c1_xname], colnames(cube1)[c1_yname]), crs = '+proj=longlat +datum=WGS84')
  # convert cube 2 to an SF object using the longitude and latitude columns as the coordinates and a long/lat WGS84 projection
  cube2_sf <- st_as_sf(x = cube2, coords = c(colnames(cube2)[c2_xname], colnames(cube2)[c2_yname]), crs = '+proj=longlat +datum=WGS84')
  
  # convert cube 1 back into a data frame putting the points from the SF geometry into two columns named lon and lat for longitude and latitude values, respectively
  cube1 <- tidyr::extract(cube1_sf, geometry, into = c('lon', 'lat'), '\\((.*),(.*)\\)')
  # convert cube 2 back into a data frame putting the points from the SF geometry into two columns named lon and lat for longitude and latitude values, respectively
  cube2 <- tidyr::extract(cube2_sf, geometry, into = c('lon', 'lat'), '\\((.*),(.*)\\)')
  
  
  # Extent Check
  # find the minimum and maximum longitude value in cube 1
  c1_xmin <- min(cube1$lon)
  c1_xmax <- max(cube1$lon)
  # find the minimum and maximum latitude value in cube 1
  c1_ymin <- min(cube1$lat)
  c1_ymax <- max(cube1$lat)
  
  # find the minimum and maximum longitude value in cube 2
  c2_xmin <- min(cube2$lon)
  c2_xmax <- max(cube2$lon)
  # find the minimum and maximum latitude value in cube 2
  c2_ymin <- min(cube2$lat)
  c2_ymax <- max(cube2$lat)
  
  # check whether the minimum longitude value in cube 1 is equal to the minimum longitude value in cube 2
  xmin_check <- c1_xmin == c2_xmin
  # check whether the maximum longitude value in cube 1 is equal to the maximum longitude value in cube 2
  xmax_check <- c1_xmax == c2_xmax
  # check whether the minimum latitude value in cube 1 is equal to the minimum latitude value in cube 2
  ymin_check <- c1_ymin == c2_ymin
  # check whether the maximum latitude value in cube 1 is equal to the maximum latitude value in cube 2
  ymax_check <- c1_ymax == c2_ymax
  
  # find the difference between the minimum longitude value of the larger cube and the minimum longitude value of the smaller cube
  xmin_dif <- abs(c1_xmin - c2_xmin)
  # find the difference between the maximum longitude value of the larger cube and the maximum longitude value of the smaller cube
  xmax_dif <- abs(c1_xmax - c2_xmax)
  # find the difference between the minimum latitude value of the larger cube and the minimum latitude value of the smaller cube
  ymin_dif <- abs(c1_ymin - c2_ymin)
  # find the difference between the maximum latitude value of the larger cube and the maximum latitude value of the smaller cube 
  ymax_dif <- abs(c1_ymax - c2_ymax)
  
  # determine which cube has a smaller minimum longitude value
  if (c1_xmin < c2_xmin){
    xmin_larger <- 'Cube 1'
  } else{
    xmin_larger <- 'Cube 2'
  }
  
  # determine which cube has a larger maximum longitude value
  if (c1_xmax > c2_xmax){
    xmax_larger <- 'Cube 1'
  } else{
    xmax_larger <- 'Cube 2'
  }
  
  # determine which cube has a smaller minimum latitude value
  if (c1_ymin < c2_ymin){
    ymin_larger <- 'Cube 1'
  } else{
    ymin_larger <- 'Cube 2'
  }
  
  # determine which cube has a larger maximum latitude value
  if (c1_ymax > c2_ymax){
    ymax_larger <- 'Cube 1'
  } else{
    ymax_larger <- 'Cube 2'
  }
    
  # create a vector of row names for the data frame of diagnostic information about the two data cubes 
  description <- c('x Dimension', 'y Dimension', 'Time', 'x Resolution', 'y Resolution', 'Temporal Resolution', 'x Min', 'x Max', 'y Min', 'y Max')
    
  # create a vector containing boolean values for all of the checks performed above
  check_vals <- c(xdim_check, ydim_check, zdim_check, xres_check, yres_check, tres_check, xmin_check, xmax_check, ymin_check, ymax_check)
    
  # create a vector containing the values of the dimensions, resolutions, and min and max values for cube 1
  cube1_vals <- c(c1_xdim, c1_ydim, c1_zdim, c1_xres, c1_yres, c1_tres, c1_xmin, c1_xmax, c1_ymin, c1_ymax)
    
  # create a vector containing the values of the dimensions, resolutions, and min and max values for cube 2
  cube2_vals <- c(c2_xdim, c2_ydim, c2_zdim, c2_xres, c2_yres, c2_tres, c2_xmin, c2_xmax, c2_ymin, c2_ymax)
    
  # create a vector containing the absolute value of the difference of each dimension, resolution, and min and max values between the two cubes
  dif_vals <- c(xdim_dif, ydim_dif, zdim_dif, xres_dif, yres_dif, tres_dif, xmin_dif, xmax_dif, ymin_dif, ymax_dif)
    
  # create a vector containing strings identifying which cube has the larger dimension, resolution, and min and max value
  larger_vals <- c(xdim_larger, ydim_larger, zdim_larger, xres_larger, yres_larger, tres_larger, xmin_larger, xmax_larger, ymin_larger, ymax_larger)
    
  # create a data frame of diagnostic values for the two data cubes containing the vectors defined above
  diagnostics <- data.frame(description, check_vals, cube1_vals, cube2_vals, dif_vals, larger_vals)
    
  # set the column names for the data frame of diagnostic values for the two data cubes
  colnames(diagnostics) <- c('Description', 'Check', 'Cube 1 Value', 'Cube 2 Value', 'Difference', 'Larger Cube')
    
  # create a list containing the data frame with diagnostic information about the two data cubes, the data frame of data types for the columns in cube 1, and the data frame of data types for the columns in cube 2 
  vals <- list(diag = diagnostics, c1 = c1_dtypes, c2 = c2_dtypes)
    
  # return the list containing the return values
  return(vals)

}