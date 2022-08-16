#' @title Krige cubes
#'
#' @description This function performs kriging using the data in a spatial data cube with one dependent variable and some number of independent variables using a user-specified formula that defines the spatial relationship between
#' the dependent variable and all of the independent variables in the data cube, a vector of longitude values, and a vector of latitude values where predictions of the dependent variable are requested.  
#' The default spatial formula is that the dependent variable depends only on longitude and latitude, i.e. var ~ lon + lat.  As arguments, this function takes the spatial data cube, a vector of longitude values, a vector of latitude values,
#' a string defining the name of the dependent variable, and an optional formula for the spatial dependency of the dependent variable on the independent variables.  Note that if independent variables are included in the data cube, all of those
#' independent variables must be included in the formula defined by the user.  Latitude and longitude values do not need to be included in the spatial dependency formula for the dependent variable.  This function returns a data frame containing
#' the predictions of the dependent variable from kriging at the latitude and longitude locations passed into the function as well as the variance of those predictions.  
#'
#' @param cube A data cube with longitude data, latitude data, one dependent variable and some number of independent variables stored as a spatial points data frame where longitude and latitude are the coordinates (Data.frame)
#' @param krige_lons A vector of longitude values at which the user would like predictions for the dependent variable
#' @param krige_lats A vector of latitude values at which the user would like predictions for the dependent variable
#' @param varname A string containing the name of the dependent variable
#' @param formula A formula that defines the spatial dependency of the dependent variable on all of the independent variables in the data cube; the default is that the dependent variable depends only on longitude and latitude
#'
#' @importFrom magrittr %>%
#' @importFrom gstat
#' @importFrom sf
#' @importFrom sp
#' @return Data.frame containing the predictions of the dependent variable and the variance of those predictions from kriging
#' @export
krige_cubes <- function(cube, krige_lons, krige_lats, varname, formula = as.formula('var ~ lon + lat')){
  require(gstat)
  require(sf)
  require(sp)
  
  # check whether or not kriging needs to be performed and if so, perform kriging
  if (length(krige_lons) > 0){
      # create a spatial points data frame containing the locations where we want predictions of the dependent variable from kriging
      points <- expand.grid(krige_lons, krige_lats)
      colnames(points) <- c("lon", "lat")
      coordinates(points) <- c("lon", "lat")
      proj4string(points) <- crs("+proj=longlat + datum=WGS84")
    
      # use the provided spatial dependency formula and the data in the original cube to create a variogram
      cube_vgm <- variogram(formula, cube, width=0.1)
    
      # fit the variogram to the data by selecting the optimal variogram model from the options Gaussian, Spherical, Mattern, or Exponential
      vgm_fit <- fit.variogram(cube_vgm, vgm(c("Gau", "Sph", "Mat", "Exp")), fit.kappa = TRUE);
    
      # use the provided spatial dependency formula, the data cube, the data frame of locations where we want values from kriging, and the variogram that was previously fit to perform kriging
      cube_krige <- krige(formula, cube, points, vgm_fit)
    
      # obtain the predictions of the dependent variable from kriging
      preds <- cube_krige@data$var1.pred
    
      # obtain the variance for the predictions of the dependent variable from kriging
      vars <- cube_krige@data$var1.var
        
      # create a data frame containing the predictions of the dependent variable from kriging and the variance of the predictions from kriging
      kriged_cube <- data.frame(preds, vars)
    
      # set the column names of the kriged cube to the variable name given by the user and the kriging variance of that variable
      colnames(kriged_cube) <- c(varname, paste(varname, 'krige variance', sep = '_'))
  }
  # if kriging does not need to be performed, simply select the relevant entries in the cube and return them
  if (length(krige_lons) == 0){
      # define a filter which will select the entries in the data cube where the longitude and latitude match a longitude and latitude location where the dependent variable is requested
      filter <- which((cube$lon %in% krige_lons) & (cube$lat %in% krige_lats))
    
      # select the values of the dependent variable at the requested locations from the original data cube
      kriged_cube <- cube[filter,3]
    
      # create a second column for the data frame that is filled with NA, taking the place of the kriging variance
      kriged_cube[,2] <- rep(NA, length(kriged_cube[,1]))
        
      # set the column names to be the variable name given by the user and the kriging variance of that variable
      colnames(kriged_cube) <- c(varname, paste(varname, "krige_variance", sep = '_'))
  }
    
  # return the kriged cube
  return(kriged_cube)
}