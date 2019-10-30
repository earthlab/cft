#' Install python dependencies
#'
#' Installs python dependencies of cstdata.
#' 
#' @param conda Whether to install dependencies with conda (logical).
#' 
#' @export
install_py_deps <- function(conda = TRUE) {
  if (conda) {
    deps <- c("dask", "netcdf4", "toolz", "xarray=0.12.3")
    reticulate::conda_install(envname = "r-reticulate", packages = deps)
  } else {
    deps <- c("dask", "netcdf4", "toolz", "xarray==0.12.3")
    reticulate::py_install(deps, method = "auto", conda = "auto")
  }
}
