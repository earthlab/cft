# We need these for xarray to work properly
deps <- c("dask", "netcdf4", "toolz", "xarray")

# Tell reticulate to use a virtualenv
reticulate::use_virtualenv()

# If needed install these to the default reticulate virtual env
if (!Sys.info()['sysname'] == "Windows"){
  if (!reticulate::py_module_available(dep)) {
    for (dep in deps) {
      # Conda not required here
      reticulate::py_install(dep, method = "auto", conda = "auto")
    }
  }
} else {
    # Surely, conda isn't absolutely necessary for windows?
    reticulate::conda_install(envname = "r-reticulate",
                              packages = deps)
}
