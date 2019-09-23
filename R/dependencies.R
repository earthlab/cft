#' Install and load packages, what is the convention in R?
# How to manage system-level dependencies? On a fresh Ubuntu installation:
# aws.s3:
#   libcurl4-openssl-dev, libssl-dev
# rgdal:
#   gdal ... needed to PKG_CONFIG_PATH,
# sf:
#   libudunits2-dev



# Installer
loadInstall <- function(packages){
  new_packages = packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages) > 0)
    install.packages(new_packages, dependencies = TRUE)
}

# cran packages
packages <- c('aws.s3', 'devtools', 'glue', 'leaflet', 'leaflet.opacity', 'lubridate', 'lwgeom',
              'ncdf4', 'progress', 'rasterVis', 'reprex', 'rgdal', 'rgeos', 'sf', 'tidync')
loadInstall(packages)

# not-cran packages
devtools::install_github("mikejohnson51/climateR", force=TRUE)
