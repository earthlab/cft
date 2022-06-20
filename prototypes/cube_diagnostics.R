cube_diagnostics <- function(cube1, cube2){
  # Dimension Check
  c1_xdim <- length(unique(cube1$lon))
  c1_ydim <- length(unique(cube1$lat))
  c1_zdim <- dim(cube1)[2]
  
  c2_xdim <- length(unique(cube2$lon))
  c2_ydim <- length(unique(cube2$lat))
  c2_zdim <- dim(cube2)[2]
  
  xdim_check <- c1_xdim == c2_xdim
  ydim_check <- c1_ydim == c2_ydim
  zdim_check <- c1_zdim == c2_zdim
  
  xdim_dif <- abs(c1_xdim - c2_xdim)
  ydim_dif <- abs(c1_ydim - c2_ydim)
  zdim_dif <- abs(c1_zdim - c2_zdim)
  
  if (c1_xdim - c2_xdim > 0){
    xdim_larger <- 'Cube 1'
  } else{
    xdim_larger <- 'Cube 2'
  }
  
  if (c1_ydim - c2_ydim > 0){
    ydim_larger <- 'Cube 1'
  } else{
    ydim_larger <- 'Cube 2'
  }
  
  if (c1_zdim - c2_zdim > 0){
    zdim_larger <- 'Cube 1'
  } else{
    zdim_larger <- 'Cube 2'
  }
  
  # Resolution Check 
  c1_xres <- resolution(cube1$lon)
  c1_yres <- resolution(cube1$lat)
  c1_tres <- resolution(cube1$time)
  
  c2_xres <- resolution(cube2$lon)
  c2_yres <- resolution(cube2$lat)
  c2_tres <- resolution(cube2$time)
  
  xres_check <- c1_xres == c2_xres
  yres_check <- c1_yres == c2_yres
  tres_check <- c1_tres == c2_tres
  
  xres_dif <- abs(c1_xres - c2_xres)
  yres_dif <- abs(c1_yres - c2_yres)
  tres_dif <- abs(c1_tres - c2_tres)
  
  if (c1_xres - c2_xres > 0){
    xres_larger <- 'Cube 1'
  } else{
    xres_larger <- 'Cube 2'
  }
  
  if (c1_yres - c2_yres > 0){
    yres_larger <- 'Cube 1'
  } else{
    yres_larger <- 'Cube 2'
  }
  
  if (c1_tres - c2_tres > 0){
    tres_larger <- 'Cube 1'
  } else{
    tres_larger <- 'Cube 2'
  }
  
  # Data Types Check\
  c1_dtypes <- data.frame(matrix(NA, nrow = 1, ncol = length(colnames(cube1))))
  
  for (i in 1:length(colnames(cube1))){
    c1_dtypes[1,i] <- typeof(cube1[,i])
  }
  
  colnames(c1_dtypes) <- colnames(cube1)
  
  c2_dtypes <- data.frame(matrix(NA, nrow = 1, ncol = length(colnames(cube2))))
  
  for (j in 1:length(colnames(cube2))){
    c2_dtypes[1,j] <- typeof(cube2[,j])
  }
  
  colnames(c2_dtypes) <- colnames(cube2)
  
  # Convert to Spatial Objects
  cube1_sf <- st_as_sf(x = cube1, coords = c('lon', 'lat'), crs = '+proj=longlat +datum=WGS84')
  cube2_sf <- st_as_sf(x = cube2, coords = c('lon', 'lat'), crs = '+proj=longlat +datum=WGS84')
  
  crs_val <- '+proj=longlat +datum=WGS84'
  
  # Spatial Projection Check
  c1_proj <- st_crs(cube1_sf)
  c2_proj <- st_crs(cube2_sf)
  
  proj_check <- c1_proj == c2_proj
  
  # Longlat Check
  c1_ll <- st_is_longlat(cube1_sf)
  c2_ll <- st_is_longlat(cube2_sf)
  
  # Extent Check 
  c1_ext <- st_bbox(cube1_sf)
  c2_ext <- st_bbox(cube2_sf)
  
  c1_xmin <- c1_ext$xmin
  c1_xmax <- c1_ext$xmax
  c1_ymin <- c1_ext$ymin
  c1_ymax <- c1_ext$ymax
  
  c2_xmin <- c2_ext$xmin
  c2_xmax <- c2_ext$xmax
  c2_ymin <- c2_ext$ymin
  c2_ymax <- c2_ext$ymax
  
  xmin_check <- c1_xmin == c2_xmin
  xmax_check <- c1_xmax == c2_xmax
  ymin_check <- c1_ymin == c2_ymin
  ymax_check <- c1_ymax == c2_ymax
  
  xmin_dif <- abs(c1_xmin - c2_xmin)
  xmax_dif <- abs(c1_xmax - c2_xmax)
  ymin_dif <- abs(c1_ymin - c2_ymin)
  ymax_dif <- abs(c1_ymax - c2_ymax)
  
  if (c1_xmin < c2_xmin){
    xmin_larger <- 'Cube 1'
  } else{
    xmin_larger <- 'Cube 2'
  }
  
  if (c1_xmax > c2_xmax){
    xmax_larger <- 'Cube 1'
  } else{
    xmax_larger <- 'Cube 2'
  }
  
  if (c1_ymin < c2_ymin){
    ymin_larger <- 'Cube 1'
  } else{
    ymin_larger <- 'Cube 2'
  }
  
  if (c1_ymax > c2_ymax){
    ymax_larger <- 'Cube 1'
  } else{
    ymax_larger <- 'Cube 2'
  }
  
  
  
  # Spatial Projection Check
  #spat_proj_check <- crs(cube1, proj=TRUE) == crs(cube2, proj=TRUE)
  
  # Extent Check
  #extent_check <- ext(cube1) == ext(cube2)
  #xmin_check <- ext(cube1)$xmin[[1]] == ext(cube2)$xmin[[1]]
  #xmax_check <- ext(cube1)$xmax[[1]] == ext(cube2)$xmax[[1]]
  #ymin_check <- ext(cube1)$ymin[[1]] == ext(cube2)$ymin[[1]]
  #ymax_check <- ext(cube1)$ymax[[1]] == ext(cube2)$ymax[[1]]
  
  # Resolution Check
  #xres_check <- xres(cube1) == xres(cube2)
  #yres_check <- yres(cube1) == yres(cube2)
  
  # Dimension Check
  #rows_check <- nrow(cube1) == nrow(cube2)
  #cols_check <- ncol(cube1) == nrow(cube2)
  #cells_check <- ncell(cube1) == ncell(cube2)
  #layer_check <- nlyr(cube1) == nlyr(cube2)
  
  # Data Type Check
  #cube1_dtypes <- c()
  #cube2_dtypes <- c()
  
  #cube1_names <- c()
  #cube2_names <- c()
  
  #for (i in 1:nlyr(cube1)){
  #name <- names(cube1)[i]
  #cube1_names <- c(cube1_names, paste('Cube 1', name, 'Data Type', sep=" "))
  #cube1_dtypes <- c(cube1_dtypes, typeof(values(cube1[[i]])))
  #}
  
  #for (j in 1:nlyr(cube2)){
  #name <- names(cube2)[i]
  #cube2_names <- c(cube2_names, paste('Cube 2', name, 'Date Type', sep=' '))
  #cube2_dtypes <- c(cube2_dtypes, typeof(values(cube2[[i]])))
  #}
  
  #check <- c('Spatial Projection Check', 'Extent Check', 'xmin Check', 'xmax Check', 'ymin Check', 'ymax Check', 'x Resolution Check', 'y Resolution Check', 'Rows Check', 'Columns Check', 'Cells Check', 'Layers Check', cube1_names, cube2_names)
  #check_values <- c(spat_proj_check, extent_check, xmin_check, xmax_check, ymin_check, ymax_check, xres_check, yres_check, rows_check, cols_check, cells_check, layer_check, cube1_dtypes, cube2_dtypes)
  #quantities <- c('Spatial Projection', 'Extent', 'xmin', 'xmax', 'ymin', 'ymax', 'x Resolution', 'y Resolution', 'Number of Rows', 'Number of Columns', 'Number of Cells', 'Number of Layers', rep(NA, length(cube1_names)+length(cube2_names)))
  #cube1_values <- c(crs(cube1, proj=TRUE), ext(cube1), ext(cube1)$xmin[[1]], ext(cube1)$xmax[[1]], ext(cube1)$ymin[[1]], ext(cube1)$ymax[[1]], xres(cube1), yres(cube1), nrow(cube1), ncol(cube1), ncell(cube1), nlyr(cube1), rep(NA, length(cube1_names)+length(cube2_names)))
  #cube2_values <- c(crs(cube2, proj=TRUE), ext(cube2), ext(cube2)$xmin[[1]], ext(cube2)$xmax[[1]], ext(cube2)$ymin[[1]], ext(cube2)$ymax[[1]], xres(cube2), yres(cube2), nrow(cube2), ncol(cube2), ncell(cube2), nlyr(cube2), rep(NA, length(cube1_names)+length(cube2_names)))
  
  #check <- c('Spatial Projection Check', 'Extent Check', 'xmin Check', 'xmax Check', 'ymin Check', 'ymax Check', 'x Resolution Check', 'y Resolution Check', 'Rows Check', 'Columns Check', 'Cells Check', 'Layers Check')
  #check_values <- c(spat_proj_check, extent_check, xmin_check, xmax_check, ymin_check, ymax_check, xres_check, yres_check, rows_check, cols_check, cells_check, layer_check)
  #quantities <- c('Spatial Projection', 'Extent', 'xmin', 'xmax', 'ymin', 'ymax', 'x Resolution', 'y Resolution', 'Number of Rows', 'Number of Columns', 'Number of Cells', 'Number of Layers')
  #cube1_values <- c(crs(cube1, proj=TRUE), NA, ext(cube1)$xmin[[1]], ext(cube1)$xmax[[1]], ext(cube1)$ymin[[1]], ext(cube1)$ymax[[1]], xres(cube1), yres(cube1), nrow(cube1), ncol(cube1), ncell(cube1), nlyr(cube1))
  #cube2_values <- c(crs(cube2, proj=TRUE), NA, ext(cube2)$xmin[[1]], ext(cube2)$xmax[[1]], ext(cube2)$ymin[[1]], ext(cube2)$ymax[[1]], xres(cube2), yres(cube2), nrow(cube2), ncol(cube2), ncell(cube2), nlyr(cube2))
  
  description <- c('x Dimension', 'y Dimension', 'z Dimension', 'x Resolution', 'y Resolution', 'Temporal Resolution', 'Spatial Projection', 'x Min', 'x Max', 'y Min', 'y Max')
  
  check_vals <- c(xdim_check, ydim_check, zdim_check, xres_check, yres_check, tres_check, proj_check, xmin_check, xmax_check, ymin_check, ymax_check)
  
  cube1_vals <- c(c1_xdim, c1_ydim, c1_zdim, c1_xres, c1_yres, c1_tres, NA, c1_xmin, c1_xmax, c1_ymin, c1_ymax)
  
  cube2_vals <- c(c2_xdim, c2_ydim, c2_zdim, c2_xres, c2_yres, c2_tres, NA, c2_xmin, c2_xmax, c2_ymin, c2_ymax)
  
  dif_vals <- c(xdim_dif, ydim_dif, zdim_dif, xres_dif, yres_dif, tres_dif, NA, xmin_dif, xmax_dif, ymin_dif, ymax_dif)
  
  larger_vals <- c(xdim_larger, ydim_larger, zdim_larger, xres_larger, yres_larger, tres_larger, NA, xmin_larger, xmax_larger, ymin_larger, ymax_larger)
  
  #diagnostics <- data.frame(check, check_values, quantities, cube1_values, cube2_values)
  
  diagnostics <- data.frame(description, check_vals, cube1_vals, cube2_vals, dif_vals, larger_vals)
  
  colnames(diagnostics) <- c('Description', 'Check', 'Cube 1 Value', 'Cube 2 Value', 'Difference', 'Larger Cube')
  
  vals <- list(diag = diagnostics, c1 = c1_dtypes, c2 = c2_dtypes)
  
  return(vals)
}