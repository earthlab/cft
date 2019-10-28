#' Install python dependencies
#'
#' Installs python dependencies of cstdata.
#' 
#' @param conda Whether to install dependencies with conda (logical).
#' 
#' @export
install_py_deps <- function(conda = TRUE) {
  deps <- c("dask", "netcdf4", "toolz", "xarray")
  if (conda) {
    reticulate::conda_install(envname = "r-reticulate", packages = deps)
  } else {
    reticulate::py_install(deps, method = "auto", conda = "auto")
  }
}
