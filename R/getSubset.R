getSubset <- function(url, aoi, dates){
  # We need dates, daily, from start to finish
  d2 <- lapply(strsplit(url, ''), function(x) which(x == "_CONUS"))

  # We also need the variable name, use the url for consistency
  v1 <- lapply(strsplit(url, ''), function(x) which(x == "?"))
  v2 <- lapply(strsplit(url, ''), function(x) which(x == "[")[1])
  var <- substr(url, as.integer(v1) + 1, as.integer(v2) - 1)

  # This returns a 3 dimensional matrix relatively quickly
  mtrcs <- RNetCDF::open.nc(url) %>% RNetCDF::var.get.nc(var, unpack = T)

  # This is, however, transposed and reversed
  mtrcs2 = array(0, dim = c(dim(mtrcs)[2], dim(mtrcs)[1],dim(mtrcs)[3]))
  for(j in 1:dim(mtrcs)[3]){
    mtrcs2[,,j] = apply(t(mtrcs[,,j]), 2, rev)
  }

  # Now to make it a raster object
  brick = raster::brick(mtrcs2)
  raster::crs(brick) = '+proj=longlat +a=6378137 +f=0.00335281066474748 +pm=0 +no_defs'
  raster::extent(brick) = aoi@bbox
  # names(brick) <- dates

  # And return the raster object
  return(brick)
}
