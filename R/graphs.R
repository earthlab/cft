scatter <- function() {
  #' Just a sample to practice this

  # Sample set of model outputs
  file_refs <- cstdata::cstdata(park = "Acadia National Park",
                                years = c(2050, 2055),
                                local_dir = "~/cstdata_test")

  # List all of the files in this directory
  pr_files <- list.files("~/cstdata_test/acadia_national_park/",
                         pattern = "*pr_*", full.names = TRUE)
  t_files <- list.files("~/cstdata_test/acadia_national_park/",
                         pattern = "*tasmax_*", full.names = TRUE)

  # Open each data set in this folder
  pncs <- lapply(pr_files, FUN = ncdf4::nc_open)
  tncs <- lapply(t_files, FUN = ncdf4::nc_open)

  # And we would scatter the mean max temperature with the mean precipitation
  pmeans <- c()
  for ( nc in pncs ) {
    model <- ncdf4::ncatt_get(nc, 0)$model
    pmean <-  mean(ncdf4::ncvar_get(nc, "precipitation"), na.rm=TRUE)
    pmeans[model] <- pmean
  }
  
  tmeans <- c()
  for ( nc in tncs ) {
    model <- ncdf4::ncatt_get(nc, 0)$model
    tmean <-  mean(ncdf4::ncvar_get(nc, "air_temperature"), na.rm=TRUE)
    tmeans[model] <- tmean
  }

  # Create a data frame out of this
  df <- dplyr::tibble(models = names(tmeans), precip = pmeans,
                      maxtemp = tmeans)

  # plotly scatterplot?
  plotly::plot_ly(df, x = ~precip, y = ~maxtemp, text = ~models,
                  mode = "markers", type = "scatter")

}
