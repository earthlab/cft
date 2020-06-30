#' Install python dependencies
#'
#' Installs python dependencies of cft.
#' 
#' The implementation and interface are adapted from the 
#' `tensorflow::install_tensorflow()` function.
#' 
#' @param conda Path to conda executable (or "auto" to find conda using the
#'   PATH and other conventional install locations).
#'   
#' @param method Installation method. By default, "auto" automatically finds a
#'   method that will work in the local environment. Change the default to force
#'   a specific installation method. Note that the "virtualenv" method is not
#'   available on Windows. Note also
#'   that since this command runs without privilege the "system" method is
#'   available only on Windows.
#'
#' @param envname Name of Python environment to install within
#'
#' @param restart_session Restart R session after installing (note this will
#'   only occur within RStudio).
#'   
#' @param ... other arguments passed to [reticulate::conda_install()] or
#'   [reticulate::virtualenv_install()].
#' 
#' @export
install_py_deps <- function(method = "conda",
                            conda = "auto",
                            envname = "cft",
                            restart_session = TRUE,
                            ...) {
  method <- match.arg(method)
  
  reticulate::conda_install(
    packages = c("dask", "netcdf4", "toolz", "xarray"), 
    envname = envname, 
    conda = conda, 
    pip = FALSE, 
    forge = TRUE,
    ...
  )
  
  
  cat("\nInstallation complete.\n\n")
  
  if (restart_session && rstudioapi::hasFun("restartSession"))
    rstudioapi::restartSession()
  
  invisible(NULL)
}
