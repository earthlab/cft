nps_boundary_url <- function() {
  # return default nps boundary url
  "https://irma.nps.gov/DataStore/DownloadFile/633334"
}

get_ncores <- function() {
  # Using half the number of cores severly restricts my personal machine, which
  # has four physical cpus and plenty of memory. Is there a way to account for 
  # RAM to allow computers to use more of their computing power?

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
