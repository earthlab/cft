`%dopar%` <- foreach::`%dopar%`
no_cores <- parallel::detectCores() - 1
doParallel::registerDoParallel(no_cores)

var = foreach::foreach(i = 1:length(urls)) %dopar% {
  RNetCDF::open.nc(urls[i]) %>% RNetCDF::var.get.nc(params[i],
                                                    unpack = T)
}

b = list()
for (i in 1:length(var)) {
  v = var[[i]] * scale_factor
  if (!is.na(no_data)) {
    if (no_data > 0) {
      v[v > no_data] = NA
    }
    else {
      v[v < no_data] = NA
    }
  }
  if (length(dim(v)[3]) == 0 | is.na(dim(v)[3])) {
    var2 = .orient(v, fun = fun)
    b1 = raster::raster(var2)
    raster::crs(b1) = g$proj
    raster::extent(b1) = g$e
  }
  else {
    var2 = array(0, dim = c(dim(v)[2], dim(v)[1],
                            dim(v)[3]))
    for (j in 1:dim(v)[3]) {
      var2[, , j] = .orient(v[, , j], fun = fun)
    }
    b1 = raster::brick(var2)
    raster::crs(b1) = g$proj
    raster::extent(b1) = g$e
  }
  b1[b1 > 1e+05] = NA
  b[[i]] = b1
}
names(b) = names
keys = unique(names(b))
b = sapply(keys, function(name) {
  stack(b[grep(name, names(b))])
})
for (i in 1:length(b)) {
  names(b[[i]]) = unique(date.names)
}
