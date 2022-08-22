#' @title Plot Cubes 3D
#'
#' @description This function produces a 3D plot using RGL that shows the spatial and temporal extents of two spatiotemporal data cubes each containing a single dependent variable and some number of independent variables.  
#'  
#'
#' @param cube1 A spatiotemporal data cube with one dependent variable and some number of independent variables stored as a data frame (Data.frame)
#' @param cube2 A spatiotemporal data cube with one dependent variable and some number of independent variables stored as a data frame (Data.frame)
#'
#' @importFrom magrittr %>%
#' @importFrom rgl
#' @return rgl widget containing the 3d plot of the two cubes
#' @export
plot_cubes_3d <- function(cube1, cube2){
  require(rgl)
  
  # define a vector of possible names for the latitude, longitude, and time data columns in each cube
  ynames <- c('lat', 'latitude', 'Lat', 'Latitude', 'y')
  xnames <- c('lon', 'long', 'longitude', 'Lon', 'Long', 'Longitude', 'x')
  tnames <- c('time', 'day', 'hour', 'month', 'year', 'date')

  # find the indices of the columns containing longitude, latitude, and time data in cube 1, respectively
  c1_xname <- which(colnames(cube1) %in% xnames)
  c1_yname <- which(colnames(cube1) %in% ynames)
  c1_tname <- which(colnames(cube1) %in% tnames)

  # find the indices of the columns containing longitude, latitude, and time data in cube 2, respectively
  c2_xname <- which(colnames(cube2) %in% xnames)
  c2_yname <- which(colnames(cube2) %in% ynames)
  c2_tname <- which(colnames(cube2) %in% tnames)
  
  # find the minimum and maximum longitude, latitude, and time values in cube 1
  c1_xmin <- min(cube1[,c1_xname])
  c1_xmax <- max(cube1[,c1_xname])
  c1_ymin <- min(cube1[,c1_yname])
  c1_ymax <- max(cube1[,c1_yname])
  c1_tmin <- min(cube1[,c1_tname])
  c1_tmax <- max(cube1[,c1_tname])
  
  # find the minimum and maximum longitude, latitude, and time values in cube 2
  c2_xmin <- min(cube2[,c2_xname])
  c2_xmax <- max(cube2[,c2_xname])
  c2_ymin <- min(cube2[,c2_yname])
  c2_ymax <- max(cube2[,c2_yname])
  c2_tmin <- min(cube2[,c2_tname])
  c2_tmax <- max(cube2[,c2_tname])
  
  # set RGL options so the 3D plot appears and has a white background to make it easier to see the cubes
  options(rgl.printRglwidget = TRUE)
  rgl.bg(color = "white")

  # create a 3D cube plot for cube 1 by first creating a unit cube that is medium slate blue in color and fairly transparent
  c3d <- cube3d(color="mediumslateblue", alpha=0.3)
  
  # create vectors that define the vertices of cube 1
  c1x = c(c1_xmin, c1_xmax, c1_xmin, c1_xmax, c1_xmin, c1_xmax, c1_xmin, c1_xmax)
  c1y = c(c1_ymin, c1_ymin, c1_ymax, c1_ymax, c1_ymin, c1_ymin, c1_ymax, c1_ymax)
  c1z = c(c1_tmin, c1_tmin, c1_tmin, c1_tmin, c1_tmax, c1_tmax, c1_tmax, c1_tmax)

  # change the vertices of the 3D unit cube to match the vertices of cube 1
  c3d$vb[1,] <- c1x
  c3d$vb[2,] <- c1y
  c3d$vb[3,] <- c1z

  # create a 3D cube plot for cube 2 by first creating a unit cube that is medium spring green in color and fairly transparent
  c3d2 <- cube3d(color="mediumspringgreen", alpha=0.3)
  
  # create vectors that define the vertices of cube 2
  c2x = c(c2_xmin, c2_xmax, c2_xmin, c2_xmax, c2_xmin, c2_xmax, c2_xmin, c2_xmax)
  c2y = c(c2_ymin, c2_ymin, c2_ymax, c2_ymax, c2_ymin, c2_ymin, c2_ymax, c2_ymax)
  c2z = c(c2_tmin, c2_tmin, c2_tmin, c2_tmin, c2_tmax, c2_tmax, c2_tmax, c2_tmax)

  # change the vertices of the 3D unit cube to match the vertices of cube 2
  c3d2$vb[1,] <- c2x
  c3d2$vb[2,] <- c2y
  c3d2$vb[3,] <- c2z

  # plot the 3D cube for cube 1
  fig <- plot3d(c3d, box = FALSE)
  
  # add the 3D cube for cube 2 to the 3D plot
  fig <- shade3d(c3d2)
  
  # return the 3D plot of the two cubes
  return(fig)
}