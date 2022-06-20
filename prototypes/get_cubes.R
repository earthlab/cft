get_cubes <- function(bb, maca_vars, maca_models, maca_rcps, gm_vars, start_date, end_date){
  
  ref_date <- as.Date("1900-01-01")
  
  start <- as.Date(start_date)
  end <- as.Date(end_date)
  
  start_time <- length(seq(from = ref_date, to = start, by = 'day'))-1
  end_time <- length(seq(from = ref_date, to = end, by = 'day'))-1
  
  start_year <- year(start)
  end_year <- year(end)
  years <- seq(start_year, end_year, by=1)
  
  inputs <- cft::available_data()
  
  input_variables <- inputs$variable_names %>% 
    filter(Variable %in% maca_vars) %>%
    filter(Model %in% maca_models) %>%
    filter(Scenario %in% maca_rcps) %>%
    pull("Available variable")
  
  maca_data <- inputs$src %>%
    hyper_filter(lat = lat <= c(bb[4]+0.05) & lat >= c(bb[2]-0.05)) %>%
    hyper_filter(lon = lon <= c(bb[3]+0.05) & lon >= c(bb[1]-0.05)) %>%
    hyper_filter(time = time <= end_time & time >= start_time) %>%
    hyper_tibble(select_var = input_variables) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant")
  
  maca_data <- st_rasterize(maca_data)
  
  maca_rast <- terra::rast(maca_data)
  
  gm_rast <- read_gridmet(bb, gm_vars, years, start_time, end_time)
  
  cubes <- list("MACA" = maca_rast, "gridMET" = gm_rast)
  
  return(cubes)
}