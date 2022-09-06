library(shiny)
library(cft)
library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(ggplot2)
library(doParallel)
library(foreach)

read_gridmet_parallel <- function(bb, gm_vars, years, start_time, end_time){
  
  len_yrs <- length(years)
  urls <- c()
  data_ids <- c()
  
  for (i in 1:length(gm_vars)){
    var <- gm_vars[i]
    
    for (j in 1:length(years)){
      year <- years[j]
      
      url <- paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/MET/", var, "/", var, "_", year, ".nc#fillmismatch")
      
      urls <- c(urls, url)
      
      data_id <- paste(var, year, sep='_')
      data_ids <- c(data_ids, data_id)
    }
  }
  
  data_order <- c()
  max_len <- 0
  
  no_cores  <- parallel::detectCores() - 1
  doParallel::registerDoParallel(no_cores)
  
  gm_data <- foreach::foreach(i = 1:length(urls)) %dopar% {
    file_url <- urls[i]
    
    file <- tidync(file_url)
    
    data <- file %>% 
      hyper_filter(lat = lat <= c(bb[4]+0.05) & lat >= c(bb[2]-0.05)) %>%
      hyper_filter(lon = lon <= c(bb[3]+0.05) & lon >= c(bb[1]-0.05)) %>%
      hyper_filter(day = day <= end_time & day >= start_time) %>%
      hyper_tibble() %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant")
    
    data <- tidyr::extract(data, geometry, into = c('lon', 'lat'), '\\((.*),(.*)\\)')
    data <- subset(data, select=-geometry)
    data <- subset(data, select=-geometry)
    data <- dplyr::select(data,3,4,1,2)
    
    if (length(data$lon) > max_len){
      max_len <- length(data$lon)
    }
    
    data_order <- c(data_order, file_url)
    list(data)
  }
  
  names(gm_data) <- data_ids
  
  data <- data.frame()
  
  tmp <- data.frame()
  for (i in 1:length(gm_data)){
    
    dat <- as.data.frame(gm_data[[i]])
    cur_len <- length(dat$lon)
    
    if (cur_len < max_len){
      length(dat$lon) <- max_len
      length(dat$lat) <- max_len
      length(dat$day) <- max_len
      length(dat[,3]) <- max_len
    }
    
    if (i == 1){
      tmp <- dat
      data <- tmp
      tmp <- data.frame()
    } else if ((i-1) %% len_yrs == 0){
      tmp <- dplyr::select(dat, 3)
    } else if (i > 1 && i < len_yrs+1){
      tmp <- as.data.frame(rbind(data, dat))
      data <- tmp
      tmp <- data.frame()
    } else {
      new_var <- dplyr::select(dat, 3)
      tmp <- as.data.frame(rbind(tmp, new_var))
      if (i %% len_yrs == 0){
        data <- as.data.frame(cbind(data, tmp))
        tmp <- data.frame()
      }
    }
  }
  
  data <- data %>% relocate(day, .after=last_col())
  
  return(data)
}

ui <- fluidPage(
  titlePanel('Satellite Data Downloader'),
  
  radioButtons("data_choice", "Select a dataset:", choices = list("MACA" = 1, "gridMET" = 2, "NASA Land Processes Data" = 3), selected = 1),
  
  #selectInput("var", "Select a variable", choices = NULL)
  
  #actionButton("maca", "MACA Climate Model Data"),
  
  #actionButton("gm", "gridMET Data"),
  
  #actionButton("lp", "NASA Land Processes Data"),
  
  #actionButton("ornl", "Oak Ridge National Lab Data"),
  
  #actionButton("po", "NASA Physical Oceanography Data"),
  
  #actionButton("ob", "NASA Ocean Biogeochemistry Data"),
  
  #uiOutput("LP"),
  
  #uiOutput("user"),
  
  #uiOutput("pass"),
  
  #uiOutput('lp_submit'),
  
  uiOutput("var"),
  
  uiOutput("model"),
  
  uiOutput("rcp"),
  
  #uiOutput('products'),
  
  #uiOutput('layers'),
  
  uiOutput("dates"),
  
  sliderInput("min_lon", "Select a minimum longitude value:", value = 0, min = -180, max = 180, step = 0.1),
  
  uiOutput("max_lon"),
  
  sliderInput('min_lat', 'Select a minimum latitude value:', value = 0, min = -90, max = 90, step = 0.1),
  
  uiOutput("max_lat"),
  
  uiOutput('submit'),
  
  dataTableOutput("data")
)

server <- function(input, output){
  
     output$var <- renderUI({
       if (input$data_choice == 1){
         selectInput("var", "Select a variable",
                     choices = list("Specific Humidity" = 1, "Precipitation" = 2, "Maximum Relative Humidity" = 3, "Minimum Relative Humidity" = 4, 
                                    "Surface Downswelling Shortwave Flux" = 5, "Maximum Temperature" = 6, "Minimum Temperature" = 7, 
                                    "Eastward Wind" = 8, "Northward Wind" = 9), selected = 1)
       }
       
       else if (input$data_choice == 2){
         selectInput("var", "Select a variable",
                     choices = list("Maximum Temperature" = 1, "Minimum Temperature" = 2, "Precipitation" = 3, "Downward Surface Shortwave Radiation" = 4, "Wind Speed" = 5, "Wind Direction" = 6, 
                     "Maximum Relative Humidity" = 7, "Minimum Relative Humidity" = 8, "Specific Humidity" = 9, "Burning Index" = 10, "Fuel Moisture 100-hour" = 11, 
                     "Fuel Moisture 1000-hour" = 12, "Energy Release Component" = 13, "Reference Evapotranspiration Alfalfa" = 14, "Reference Evapotranspiration Grass" = 15, 
                     "Vapor Pressure Deficit" = 16), selected = 1)
       }
       
       # else if (input$data_choice == 3){
       #   selectInput("var", "Select a variable", choices = list("UN-adjusted Population Count" = 1, "UN-adjusted Population Density" = 2, "Land Cover Type" = 3, "Land Cover Dynamics" = 4,
       #    "Leaf Area Index (LAI) and Fraction of Photosynthetically Active Radiation (FPAR)" = 5, "Bidirectional Reflectance Distribution Function (BRDF) and Albedo" = 6,
       #    "Burned Area (fire)" = 7, "Surface Reflectance Bands 1-7" = 8, "Snow Cover" = 9, "Land Surface Temperature & Emissivity" = 10,
       #    "Vegetation Indices (NDVI & EVI)" = 11, "Thermal Anomalies and Fire" = 12, "Evapotranspiration" = 13, "Net Evapotranspiration" = 14,
       #    "Gross Primary Productivity (GPP)" = 15, "Net Primary Production (NPP)" = 16, "Temperature and Emissivity" = 17, "Vegetation Continuous Fields" = 18,
       #    "Land/Water Mask" = 19, "Ocean Reflectance Bands 8-16" = 20, "Thermal Bands and Albedo" = 21, "Elevation" = 22, "Water Bodies Database Attributes" = 23,
       #    "Water Bodies Database Elevation" = 24, "Surface Reflectance" = 25, "Global Land Surface Phenology (GLSP)" = 26, "BRDF-Albedo Model Parameters" = 27,
       #    "BRDF-Albedo Quality" = 28, "Albedo" = 29, "Nadir BRDF-Adjusted Reflectance" = 30, "Daily Surface Weather Data for North America" = 31), selected = 1)
       # }
       })
     
     output$model <- renderUI({
       if (input$data_choice == 1){
         selectInput("model", "Select a model", 
                     choices = list("Beijing Normal University - Earth System Model" = 1, "Community Climate System Model 4" = 2, "Centre National de Recherches Meteorologiques - Climate Model 5" = 3, 
                                    "Commonwealth Scientific and Industrial Research Organisation - Mk3.6.0" = 4, "Canadian Earth System Model 2" = 5, "Geophysical Fluid Dynamics Laboratory - Earth System Model 2 Generalized Ocean Layer Dynamics" = 6, 
                                    "Geophysical Fluid Dynamics Laboratory - Earth System Model 2 Modular Ocean" = 7, "Hadley Global Enviornment Model 2 - Climate Chemistry 365 (day)" = 8, "Hadley Global Environment Model 2 - Earth System 365 (day)" = 9, 
                                    "Institut Pierre Simon Laplace (IPSL) - Climate Model 5A - Low Resolution" = 10, "Institut Pierre Simon Laplace (IPSL) - Climate Model 5A - Medium Resolution" = 11, "Institut Pierre Simon Laplace (IPSL) - Climate Model 5B - Low Resolution" = 12,
                                    "Model for Interdisciplinary Research On Climate - Earth System Model - Chemistry" = 13, "Model for Interdisciplinary Research On Climate - Earth System Model" = 14, "Model for Interdisciplinary Research On Climate 5" = 15,
                                    "Meteorological Research Institute - Coupled Global Climate Model 3" = 16, "Norwegian Earth System Model 1 - Medium Resolution" = 17, "Beijing Climate Center - Climate System Model 1.1" = 18, "Institute of Numerical Mathematics Climate Model 4"  = 19), selected = 1)
       }
     })
     
     output$rcp <- renderUI({
       if (input$data_choice == 1){
         selectInput("rcp", "Select an emissions scenario", choices = list("RCP 4.5" = 1, "RCP 8.5" = 2), selected = 1)
       }
     })
                 
     # output$LP <- renderUI({
     #   if (input$data_choice == 3){
     #     p("In order to access data from the LP DAAC, you must enter your NASA EarthData login credentials below.  If you do not have an EarthData account, you can create one", a("here", href = "https://urs.earthdata.nasa.gov/users/new"))
     #   }
     # })
     # 
     # output$user <- renderUI({
     #   if (input$data_choice == 3){
     #      textInput("ed_user", label="Enter your EarthData username")
     #   }
     # })
     # 
     # output$pass <- renderUI({
     #   if (input$data_choice == 3){
     #     passwordInput("ed_pass", label="Enter your EarthData password") 
     #   }
     # })
     # 
     # output$lp_submit <- renderUI({
     #   if (input$data_choice == 3){
     #     actionButton('lp_submit', 'Submit username and password')
     #   }
     # })
     # 
     # output$products <- renderUI({
     #   if (input$data_choice == 3){
     #     radioButtons('user_products', 'Select products:', choices = NULL)
     #   }
     # })
     # 
     # output$layers <- renderUI({
     #   if (input$data_choice == 3){
     #     radioButtons('user_layers', 'Select layers:', choices = NULL)
     #   }
     # })
     
     # observeEvent('submit', {
     #   lp_vars = data.frame(c("UN-adjusted Population Count", "UN-adjusted Population Density", "Land Cover Type", "Land Cover Dynamics",
     #                          "Leaf Area Index (LAI) and Fraction of Photosynthetically Active Radiation (FPAR)", "Bidirectional Reflectance Distribution Function (BRDF) and Albedo",
     #                          "Burned Area (fire)", "Surface Reflectance Bands 1-7", "Snow Cover", "Land Surface Temperature & Emissivity",
     #                          "Vegetation Indices (NDVI & EVI)", "Thermal Anomalies and Fire", "Evapotranspiration", "Net Evapotranspiration",
     #                          "Gross Primary Productivity (GPP)", "Net Primary Production (NPP)", "Temperature and Emissivity", "Vegetation Continuous Fields",
     #                          "Land/Water Mask", "Ocean Reflectance Bands 8-16", "Thermal Bands and Albedo", "Elevation", "Water Bodies Database Attributes",
     #                          "Water Bodies Database Elevation", "Surface Reflectance", "Global Land Surface Phenology (GLSP)", "BRDF-Albedo Model Parameters",
     #                          "BRDF-Albedo Quality", "Albedo", "Nadir BRDF-Adjusted Reflectance", "Daily Surface Weather Data for North America"))
     #   
     #   var <- lp_vars[input$var,]
     # 
     #   secret <- jsonlite::base64_enc(paste(input$ed_user, input$ed_pass, sep=":"))
     #   
     #   API_URL <- "https://appeears.earthdatacloud.nasa.gov/api/"
     #   
     #   response <- httr::POST(paste0(API_URL, "login"),
     #                          add_headers("Authorization" = paste("Basic", gsub("\n", "", secret)),
     #                                      "Content-Type" = "application/x-www-form-urlencoded;charset=UTF-8"),
     #                          body = "grant_type=client_credentials")
     #   
     #   response_content <- content(response)
     #   
     #   token_response <- toJSON(response_content, auto_unbox = TRUE)
     #   
     #   remove(secret, response)
     #   
     #   prods_req <- GET(paste0(API_URL, "product"))
     #   
     #   prods_content <- content(prods_req)
     #   
     #   all_Prods <- toJSON(prods_content, auto_unbox = TRUE)
     #   
     #   remove(prods_req, prods_content)
     #   
     #   divided_products <- split(fromJSON(all_Prods), seq(nrow(fromJSON(all_Prods))))
     #   
     #   products <- setNames(divided_products, fromJSON(all_Prods)$ProductAndVersion)
     #   
     #   #prods <- matrix(nrow = length(products), ncol = 2)
     #   
     #   #i <- 1
     #   
     #   options <- list()
     #   
     #   for (p in products){
     #     
     #     #prods[i] <- c(p$ProductAndVersion, p$Description)
     #     
     #     if (grepl(var, p$Description)){
     #       options <- append(options, p$ProductAndVersion)
     #     }
     #     
     #     #i <- i + 1
     #   }
     #   
     #   #prods <- as.data.frame(prods)
     #   
     #   #colnames(prods) <- c('Product', 'Description')
     #   
     #   #indices <- which(grepl(input$var, prods$Description))
     #   
     #   #user_prods <- prods[indices,1]
     #   
     #   updateRadioButtons('product_input', 'Select LP Products:', choices = options)
     # })
     # 
     # observeEvent('submit', {
     #   secret <- jsonlite::base64_enc(paste(input$ed_user, input$ed_pass, sep=":"))
     #   
     #   API_URL <- "https://appeears.earthdatacloud.nasa.gov/api/"
     #   
     #   response <- httr::POST(paste0(API_URL, "login"),
     #                          add_headers("Authorization" = paste("Basic", gsub("\n", "", secret)),
     #                                      "Content-Type" = "application/x-www-form-urlencoded;charset=UTF-8"),
     #                          body = "grant_type=client_credentials")
     #   
     #   response_content <- content(response)
     #   
     #   token_response <- toJSON(response_content, auto_unbox = TRUE)
     #   
     #   remove(secret, response)
     #   
     #   layer_options <- c()
     #   
     #   for (i in 1:length(input$user_prods)){
     #     req <- GET(paste0(API_URL, "product/", input$user_prods[i]))
     #     
     #     p_content <- content(req)
     #     
     #     response <- toJSON(p_content, auto_unbox = TRUE)
     #     
     #     remove(req, p_content)
     #     
     #     layer_options <- append(layer_options, names(fromJSON(response)))
     #   }
     #   
     #   updateRadioButtons('layer_input', 'Select product layers:', choices = layer_options)
     # })
     
     output$date_button <- renderUI({
       checkboxInput("date_button", "Click the button below to get the entire MACA record")
     })
     
     output$dates <- renderUI({
       if (input$data_choice == 1){
         dateRangeInput("dates", "Enter a range of dates", min = '1950-01-01', max = '2100-12-31')
       }
       
       else if (input$data_choice == 2){
         dateRangeInput("dates", "Enter a range of dates", min = '1979-01-01', max = Sys.Date())
       }
       
       # else if (input$data_choice == 3){
       #   dateRangeInput("dates", "Enter a range of dates")
       # }
     })
     
     output$max_lon <- renderUI({
       sliderInput("max_lon", "Select a maximum longitude value:", value = input$min_lon, min = input$min_lon, max = 180, step = 0.1)
     })
     
     output$max_lat <- renderUI({
       sliderInput("max_lat", "Select a maximum latitude value:", value = input$min_lat, min = input$min_lat, max = 90, step = 0.1)
     })
     
     output$submit <- renderUI({
       if (input$data_choice == 1){
         actionButton("maca_submit", "Download")
       }
       else if (input$data_choice == 2){
         actionButton('gm_submit', "Download")
       }
       # else if (input$data_choice == 3){
       #   actionButton('lp_submit', "Download")
       # }
     })
     
     observeEvent(input$maca_submit, {
       withProgress(message = 'Downloading MACA Data', {
        maca_vars = data.frame(c("Specific Humidity", "Precipitation", "Maximum Relative Humidity", "Minimum Relative Humidity", 
                      "Surface Downswelling Shortwave Flux", "Maximum Temperature", "Minimum Temperature", 
                      "Eastward Wind", "Northward Wind"))
       
         maca_models = data.frame(c("Beijing Normal University - Earth System Model", "Community Climate System Model 4", "Centre National de Recherches Meteorologiques - Climate Model 5", 
                      "Commonwealth Scientific and Industrial Research Organisation - Mk3.6.0", "Canadian Earth System Model 2", "Geophysical Fluid Dynamics Laboratory - Earth System Model 2 Generalized Ocean Layer Dynamics", 
                      "Geophysical Fluid Dynamics Laboratory - Earth System Model 2 Modular Ocean", "Hadley Global Enviornment Model 2 - Climate Chemistry 365 (day)", "Hadley Global Environment Model 2 - Earth System 365 (day)", 
                      "Institut Pierre Simon Laplace (IPSL) - Climate Model 5A - Low Resolution", "Institut Pierre Simon Laplace (IPSL) - Climate Model 5A - Medium Resolution", "Institut Pierre Simon Laplace (IPSL) - Climate Model 5B - Low Resolution",
                      "Model for Interdisciplinary Research On Climate - Earth System Model - Chemistry", "Model for Interdisciplinary Research On Climate - Earth System Model", "Model for Interdisciplinary Research On Climate 5",
                      "Meteorological Research Institute - Coupled Global Climate Model 3", "Norwegian Earth System Model 1 - Medium Resolution", "Beijing Climate Center - Climate System Model 1.1", "Institute of Numerical Mathematics Climate Model 4"))
       
        maca_rcps = data.frame(c("RCP 4.5", "RCP 8.5"))
       
        inputs <- cft::available_data()
       
        ref_date <- as.Date("1900-01-01")
       
        start <- as.Date(input$dates[1])
        end <- as.Date(input$dates[2])
       
        start_time <- length(seq(from = ref_date, to = start, by = 'day'))-1
        end_time <- length(seq(from = ref_date, to = end, by = 'day'))-1
       
        vars <- maca_vars[input$var,]
        models <- maca_models[input$model,]
        rcps <- maca_rcps[input$rcp,]
       
        input_variables <- inputs$variable_names %>% 
          filter(Variable %in% vars) %>%
          filter(Model %in% models) %>%
          filter(Scenario %in% rcps) %>%
          pull("Available variable")
       
        data <- inputs$src %>%
          hyper_filter(lat = lat <= c(input$max_lat+0.05) & lat >= c(input$min_lat-0.05)) %>%
          hyper_filter(lon = lon <= c(input$max_lon+0.05) & lon >= c(input$min_lon-0.05)) %>%
          hyper_filter(time = time <= end_time & time >= start_time) %>%
          hyper_tibble(select_var = input_variables)
       })
       
       output$data <- renderDataTable(data)
       
       write.csv(data, file = 'maca.csv')
     })
     
     observeEvent(input$gm_submit, {
       gm_vars = data.frame(c("Maximum Temperature", "Minimum Temperature", "Precipitation", "Downward Surface Shortwave Radiation", "Wind Speed", "Wind Direction", 
                      "Maximum Relative Humidity", "Minimum Relative Humidity", "Specific Humidity", "Burning Index", "Fuel Moisture 100-hour", 
                      "Fuel Moisture 1000-hour", "Energy Release Component", "Reference Evapotranspiration Alfalfa", "Reference Evapotranspiration Grass", 
                      "Vapor Pressure Deficit"))
       
       gm_short_vars <- data.frame(c('tmmx', 'tmmn', 'pr', 'srad', 'vs', 'th', 'rmax', 'rmin', 'sph', 'bi', 'fm100', 'fm1000', 'erc', 'etr', 'pet', 'vpd'))
       
       gm_names <- data.frame(c('max_air_temperature', 'min_air_temperature', 'precipitation', 'downward_surface_shortwave_radiation', 'wind_speed', 'wind_direction', 
                                'max_relative_humidity', 'min_relative_humidity', 'specific_humidity', 'burning_index', 'fuel_moisture_100hr', 'fuel_moisture_1000_hr', 'energy_release_component',
                                'evapotranspiration_alfalfa', 'evapotranspiration_grass', 'vapor_pressure_deficit'))
       
       ref_date <- as.Date("1900-01-01")
       
       start <- input$dates[1]
       end <- input$dates[2]
       
       start_time <- length(seq(from = ref_date, to = start, by = 'day'))-1
       end_time <- length(seq(from = ref_date, to = end, by = 'day'))-1
       
       start_year <- year(start)
       end_year <- year(end)
       years <- seq(start_year, end_year, by=1)
       
       bb <- c(input$min_lon, input$min_lat, input$max_lon, input$max_lat)
       
       vars <- gm_short_vars[input$var,]
       
       data <- read_gridmet_parallel(bb, vars, years, start_time, end_time)
       
       colnames(data)[3] <- gm_names[input$var,]
       
       output$data <- renderDataTable(data)
       
       write.csv(data, file = 'gm.csv')
     })
     
     # observeEvent(input$lp_submit, {
     #   lp_vars = data.frame(c("UN-adjusted Population Count", "UN-adjusted Population Density", "Land Cover Type", "Land Cover Dynamics",
     #                  "Leaf Area Index (LAI) and Fraction of Photosynthetically Active Radiation (FPAR)", "Bidirectional Reflectance Distribution Function (BRDF) and Albedo",
     #                  "Burned Area (fire)", "Surface Reflectance Bands 1-7", "Snow Cover", "Land Surface Temperature & Emissivity",
     #                  "Vegetation Indices (NDVI & EVI)", "Thermal Anomalies and Fire", "Evapotranspiration", "Net Evapotranspiration",
     #                  "Gross Primary Productivity (GPP)", "Net Primary Production (NPP)", "Temperature and Emissivity", "Vegetation Continuous Fields",
     #                  "Land/Water Mask", "Ocean Reflectance Bands 8-16", "Thermal Bands and Albedo", "Elevation", "Water Bodies Database Attributes",
     #                  "Water Bodies Database Elevation", "Surface Reflectance", "Global Land Surface Phenology (GLSP)", "BRDF-Albedo Model Parameters",
     #                  "BRDF-Albedo Quality", "Albedo", "Nadir BRDF-Adjusted Reflectance", "Daily Surface Weather Data for North America"))
     #   
     #   secret <- jsonlite::base64_enc(paste(input$ed_user, input$ed_pass, sep=":"))
     #   
     #   API_URL <- "https://appeears.earthdatacloud.nasa.gov/api/"
     #   
     #   response <- httr::POST(paste0(API_URL, "login"),
     #                          add_headers("Authorization" = paste("Basic", gsub("\n", "", secret)),
     #                                      "Content-Type" = "application/x-www-form-urlencoded;charset=UTF-8"),
     #                          body = "grant_type=client_credentials")
     #   
     #   response_content <- content(response)
     #   
     #   token_response <- toJSON(response_content, auto_unbox = TRUE)
     #   
     #   remove(secret, response)
     #   
     #   prods_req <- GET(paste0(API_URL, "product"))
     #   
     #   prods_content <- content(prods_req)
     #   
     #   all_Prods <- toJSON(prods_content, auto_unbox = TRUE)
     #   
     #   remove(prods_req, prods_content)
     #   
     #   divided_products <- split(fromJSON(all_Prods), seq(nrow(fromJSON(all_Prods))))
     #   
     #   products <- setNames(divided_products, fromJSON(all_Prods)$ProductAndVersion)
     #   
     #   prods <- matrix(nrow = length(products), ncol = 2)
     #   
     #   for (i in 1:length(products)){
     #     p <- products[i]
     #     
     #     prods[i] <- c(p$ProductAndVersion, p$Description)
     #   }
     #   
     #   prods <- as.data.frame(prods)
     #   
     #   colnames(prods) <- c('Product', 'Description')
     #   
     #   indices <- which(grepl(input$var, prods$Description))
     #   
     #   user_prods <- prods[indices,1]
     #   
     #   
     #   
     #   layers <- c()
     #   
     #   for (p in user_prods){
     #     req <- GET(paste0(API_URL, "product/", p))
     #     
     #     p_content <- content(req)
     #     
     #     response <- toJSON(p_content, auto_unbox = TRUE)
     #     
     #     remove(req, p_content)
     #     
     #     layers <- c(layers, names(fromJSON(response)))
     #   }
     #   
     #   desired_layers <- input$user_layers
     #   
     #   desired_prods <- input$user_prods
     #   
     #   layers <- data.frame(product = desired_prods, layer = desired_layers)
     #   
     #   startDate <- input$dates[1]
     #   
     #   endDate <- input$dates[2]
     #   
     #   taskName <- 'LP Download'
     #   
     #   taskType <- 'point'
     #   
     #   date <- data.frame(startDate = startDate, endDate = endDate)
     #   
     #   task_info <- list(date, layers)
     #   
     #   names(task_info) <- c('dates', 'layers')
     #   
     #   task <- list(task_info, taskName, taskType)
     #   
     #   names(task) <- c('params', 'task_name', 'task_type')
     #   
     #   remove(date, layers, bbox, task_info)
     #   
     #   task_json <- toJSON(task, auto_unbox = TRUE)
     #   
     #   token <- paste("Bearer", fromJSON(token_response)$token)
     #   
     #   response <- POST(paste0(API_URL, "task"),
     #                    body = task_json,
     #                    encode = "json",
     #                    add_headers(Authorization = token, "Content-Type" = "application/json"))
     #   
     #   task_content <- content(response)
     #   
     #   task_response <- prettify(toJSON(task_content, auto_unbox = TRUE))
     #   
     #   remove(response, task_content)
     #   
     #   task_id <- fromJSON(task_response)$task_id
     #   
     #   response <- GET(paste0(API_URL, "bundle/", task_id), add_headers(Authorization = token))
     #   
     #   response_content <- content(response)
     #   
     #   bundle_response <- toJSON(response_content, auto_unbox = TRUE)
     #   
     #   bundle <- fromJSON(bundle_response)$files
     #   
     #   for (id in bundle$file_id){
     #     filename <- bundle[bundle$file_id == id,]$file_name
     #     
     #     filepath <- paste(outDir, filename, sep="/")
     #     
     #     suppressWarnings(dir.create(dirname(filepath)))
     #     
     #     response <- GET(paste0(API_URL, "bundle/", task_id, "/", id),
     #                     write_disk(filepath, overwrite = TRUE),
     #                     progress(),
     #                     add_headers(Authorization = token))
     #   }
     #   
     #   files <- list.files(outDir, '\.csv')
     #   
     #   df <- read_csv(paste0(outDir, "/", files), show_col_types = FALSE)
     #   
     # })
}

shinyApp(ui = ui, server = server)