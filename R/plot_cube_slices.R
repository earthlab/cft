#' @title Plot Cube Slices
#'
#' @description This function creates a 2D plot of a single time slice from two cubes to show how the spatial extents and spatial resolutions of those two cubes differ.  The slice from the first cube is shown in blue, with blue lines connecting
#' each of the blue points in the slice from that cube.  The slice from the second cube is shown in red, with red lines connecting each of the red points in the slice from that cube.  This function returns a ggplot object containing the 2D plot. 
#'
#' @param cube1 A spatiotemporal data cube with one dependent variable and some number of independent variables stored as a data frame (Data.frame)
#' @param cube2 A spatiotemporal data cube with one dependent variable and some number of independent variables stored as a data frame (Data.frame)
#'
#' @importFrom magrittr %>%
#' @importFrom tidyverse
#' @return A ggplot object containing the 2D plot of a single time slice from both cubes
#' @export
plot_cube_slices <- function(cube1, cube2){
  require(tidyverse)
  
  # define a vector of possible names for the latitude and longitude data columns in each cube
  ynames <- c('lat', 'latitude', 'Lat', 'Latitude', 'y')
  xnames <- c('lon', 'long', 'longitude', 'Lon', 'Long', 'Longitude', 'x')
  
  # find the indices of the longitude and latitude data columns in cube 1
  c1_xname <- which(colnames(cube1) %in% xnames)
  c1_yname <- which(colnames(cube1) %in% ynames)
  
  # find the indices of the longitude and latitude data columns in cube 2
  c2_xname <- which(colnames(cube2) %in% xnames)
  c2_yname <- which(colnames(cube2) %in% ynames)
  
  # rename the longitude column in cube 1 to lon and rename the latitude column in cube 1 to lat, to make plotting easier
  colnames(cube1)[c1_xname] <- 'lon'
  colnames(cube1)[c1_yname] <- 'lat'
  
  # rename the longitude column in cube 2 to lon and rename the latitude column in cube 2 to lat, to make plotting easier
  colnames(cube2)[c2_xname] <- 'lon'
  colnames(cube2)[c2_yname] <- 'lat'

  # create the 2D plot of a single time slice from each cube by first creating lines between all of the data points in each time slice using geom_segment, then plotting the data points in each time slice using geom_point,
  # adding a filled rectangle over the points and line segments to show the spatial extent of each time slice using annotate, and using coord_quickmap to display the points using their lat/long coordinates
  cube_plot <- ggplot() + 
    geom_segment(data = cube1, aes(lon, lat, xend = max(lon), yend = lat), color = 'blue', size=.25) +
    geom_segment(data = cube1, aes(lon, lat, xend = lon, yend = max(lat)), color = 'blue', size=.25) +
    geom_segment(data = cube2, aes(lon, lat, xend = max(lon), yend = lat), color = 'red', size=.25) + 
    geom_segment(data = cube2, aes(lon, lat, xend = lon, yend = max(lat)), color = 'red', size=.25) + 
    geom_point(data = cube1, aes(lon, lat), color = 'blue', size = .8, show.legend = FALSE) +
    geom_point(data = cube2, aes(lon, lat), color = 'red', size = .8, show.legend = FALSE) +
    annotate(geom = 'rect', xmin = min(cube1$lon), xmax = max(cube1$lon), ymin = min(cube1$lat), ymax = max(cube1$lat),fill = 'blue', colour = 'blue', alpha = 0.3) +
    annotate(geom = 'rect', xmin = min(cube2$lon), xmax = max(cube2$lon), ymin = min(cube2$lat), ymax = max(cube2$lat), fill = 'red', colour = 'red', alpha = 0.3) +
    coord_quickmap()

  # return the 2D plot of both cube slices
  return(cube_plot)
}

