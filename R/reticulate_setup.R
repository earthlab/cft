# We need these for xarray to work properly
deps <- c("dask", "netcdf4", "toolz", "xarray")

# If needed install these to the default reticualte virtual env
for (dep in deps) {
  if (!reticulate::py_module_available(dep)) {
    reticulate::py_install(dep, method = "auto", conda = "auto")
  }
}
