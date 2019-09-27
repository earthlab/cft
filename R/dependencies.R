#' Install and load packages, what is the convention in R?

# Installer
loadInstall <- function(packages){
  new_packages = packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages) > 0)
    install.packages(new_packages, dependencies = TRUE)
}

# cran packages
packages <- c('aws.s3', 'devtools', 'glue', 'progress', 'reticulate', 'rgdal')
loadInstall(packages)
