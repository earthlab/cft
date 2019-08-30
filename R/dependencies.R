#' Install and load packages, what is the convention in R?

# Installer
loadInstall <- function(packages){
  new_packages = packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages) > 0)
    install.packages(new_packages, dependencies = TRUE)
}

# cran packages (sometimes requires some user input in linux)
packages <- c('aws.s3', 'devtools', 'leaflet', 'leaflet.opacity', 'lubridate', 'lwgeom',
              'ncdf4', 'progress', 'rasterVis', 'readtext', 'reprex', 'rgdal', 'rgeos',
              'sf', 'tidync')
loadInstall(packages)

# not-cran packages
devtools::install_github("mikejohnson51/climateR", force=TRUE)
