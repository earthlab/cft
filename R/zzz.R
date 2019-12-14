nps_boundary_url <- function() {
  # return default nps boundary url
  "https://irma.nps.gov/DataStore/DownloadFile/630692"
}

get_ncores <- function() {
  # Checkout these posts: https://stackoverflow.com/questions/28954991/whether-to-use-the-detectcores-function-in-r-to-specify-the-number-of-cores-for
  # Considering the above posts, consider the future::availableCores()
  # Testing/checks may impose a limit on cpu usage
  limit <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")

  # If so, use only 2 cores (if available)
  if (nzchar(limit) && limit == "TRUE") {
    ncores <- min(c(2L, parallel::detectCores()))

  # Otherwise, use half number of all available cpus
  } else {
    ncores <- parallel::detectCores() / 2
  }
  return(ncores)
}

.onAttach <- function(libname, pkgname){
  # To default to function inputs from user
  options(cstdata.connection = stdin())
}
