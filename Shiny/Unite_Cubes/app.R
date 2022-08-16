library(shiny)
library(tidync)
library(RNetCDF)
library(ncdf4)
library(ncmeta)
library(cft)
library(usmap)
library(ggplot2)
library(tibble)
library(foreach)
library(doParallel)
library(sp)
library(sf)
library(rgl)
library(gstat)

krige_cubes <- function(cube, krige_lons, krige_lats, varname, formula = as.formula('var ~ lon + lat')){
  # check whether or not kriging needs to be performed and if so, perform kriging
  if (length(krige_lons) > 0){
    # create a spatial points data frame containing the locations where we want predictions of the dependent variable from kriging
    points <- expand.grid(krige_lons, krige_lats)
    colnames(points) <- c("lon", "lat")
    coordinates(points) <- c("lon", "lat")
    proj4string(points) <- crs("+proj=longlat + datum=WGS84")
    
    # use the provided spatial dependency formula and the data in the original cube to create a variogram
    cube_vgm <- variogram(formula, cube, width=0.1)
    
    # fit the variogram to the data by selecting the optimal variogram model from the options Gaussian, Spherical, Mattern, or Exponential
    vgm_fit <- fit.variogram(cube_vgm, vgm(c("Gau", "Sph", "Mat", "Exp")), fit.kappa = TRUE);
    
    # use the provided spatial dependency formula, the data cube, the data frame of locations where we want values from kriging, and the variogram that was previously fit to perform kriging
    cube_krige <- krige(formula, cube, points, vgm_fit)
    
    # obtain the predictions of the dependent variable from kriging
    preds <- cube_krige@data$var1.pred
    
    # obtain the variance for the predictions of the dependent variable from kriging
    vars <- cube_krige@data$var1.var
    
    # create a data frame containing the predictions of the dependent variable from kriging and the variance of the predictions from kriging
    kriged_cube <- data.frame(preds, vars)
    
    # set the column names of the kriged cube to the variable name given by the user and the kriging variance of that variable
    colnames(kriged_cube) <- c(varname, paste(varname, 'krige variance', sep = '_'))
  }
  # if kriging does not need to be performed, simply select the relevant entries in the cube and return them
  if (length(krige_lons) == 0){
    # define a filter which will select the entries in the data cube where the longitude and latitude match a longitude and latitude location where the dependent variable is requested
    filter <- which((cube$lon %in% krige_lons) & (cube$lat %in% krige_lats))
    
    # select the values of the dependent variable at the requested locations from the original data cube
    kriged_cube <- cube[filter,3]
    
    # create a second column for the data frame that is filled with NA, taking the place of the kriging variance
    kriged_cube[,2] <- rep(NA, length(kriged_cube[,1]))
    
    # set the column names to be the variable name given by the user and the kriging variance of that variable
    colnames(kriged_cube) <- c(varname, paste(varname, "krige_variance", sep = '_'))
  }
  
  # return the kriged cube
  return(kriged_cube)
}

ui <- fluidPage(
  titlePanel("Uniting Cubes User Interface"),
  
  h1('This Shiny App will help you interactively perform kriging to combine two spatiotemporal data cubes of your choosing!'),
  
  fileInput('cube1', "Select the CSV file containing the data for Cube 1", accept = '.csv'),
  
  fileInput('cube2', "Select the CSV file containing the data for Cube 2", accept = '.csv'),
  
  dateInput("c1_ref_date", "Select the reference date for Cube 1"),
  
  dateInput("c2_ref_date", "Select the reference date for Cube 2"),
  
  actionButton('user_vals', 'Submit Selections'),
  
  #dataTableOutput("cube1"),
  
  #dataTableOutput('cube2'),
  
  uiOutput("tres_message"),
  
  uiOutput('tres_decision'),
  
  uiOutput('time_done'),
  
  #uiOutput('continue_button'),
  
  h2('This visualization shows the two cubes in 3D.'),
  
  p('If you only see one cube, then both cubes have the same shape.  Note that this visualization is interactive, so you can click and drag to move the cubes around and see them from different angles.'),
  
  rglwidgetOutput("cube3d", width = 800, height = 600),
  
  h2('This visualization shows the spatial extents and spatial resolutions of one time slice from both cubes.'),
  
  p('The data from cube 1 is shown in blue and the data from cube 2 is shown in red.'),
  
  plotOutput("cube_plot"),
  
  uiOutput('user_message'),
  
  uiOutput('buttons'),
  
  h2('Histograms from the combined cube'),
  
  plotOutput('c1_krigehist'),
  
  plotOutput('c1_varhist'),
  
  plotOutput('c2_krigehist'),
  
  plotOutput('c2_varhist'),
  
  h2('Here is the combined cube!'),
  
  dataTableOutput("cube"),
  
  h2('Would you like to save the combined cube in a file?'),
  
  actionButton('save_yes', 'Yes'),
  
  actionButton('save_no', 'No'),
  
  uiOutput('file_path'),
  
  uiOutput('file_name'),
  
  uiOutput('file')
)

server <- function(input, output){
  observeEvent(input$user_vals, {
    cube1_file <- input$cube1
    cube2_file <- input$cube2
    
    req(cube1_file)
    req(cube2_file)
    
    cube1_ext <- tools::file_ext(cube1_file$datapath)
    cube2_ext <- tools::file_ext(cube2_file$datapath)

    validate(need(cube1_ext == "csv", "Please upload a csv file"))
    validate(need(cube2_ext == "csv", "Please upload a csv file"))
    
    cube1 <- read.csv(cube1_file$datapath)
    cube2 <- read.csv(cube2_file$datapath)
    
    #output$cube1 <- renderDataTable(cube1)
    
    #output$cube2 <- renderDataTable(cube2)
    
    #print('names')
    ynames <- c('lat', 'latitude', 'Lat', 'Latitude', 'y')
    xnames <- c('lon', 'long', 'longitude', 'Lon', 'Long', 'Longitude', 'x')
    tnames <- c('time', 'day', 'hour', 'month', 'year', 'date')
    
    #print('c1 vals')
    c1_indx <- seq(1, length(colnames(cube1)))
    c1_xname <- which(colnames(cube1) %in% xnames)
    c1_yname <- which(colnames(cube1) %in% ynames)
    c1_tname <- which(colnames(cube1) %in% tnames)
    c1_vname <- c1_indx[-c(c1_xname, c1_yname, c1_tname)]
    c1_varname <- colnames(cube1)[c1_vname]
    
    #print('c2 vals')
    c2_indx <- seq(1, length(colnames(cube2)))
    c2_xname <- which(colnames(cube2) %in% xnames)
    c2_yname <- which(colnames(cube2) %in% ynames)
    c2_tname <- which(colnames(cube2) %in% tnames)
    c2_vname <- c2_indx[-c(c2_xname, c2_yname, c2_tname)]
    c2_varname <- colnames(cube2)[c2_vname]
    
    #print('convert vals to numeric')
    cube1[,c1_xname] <- as.numeric(cube1[,c1_xname])
    cube1[,c1_yname] <- as.numeric(cube1[,c1_yname])
    
    for (i in 1:length(c1_vname)){
      cube1[,c1_vname[i]] <- as.numeric(cube1[,c1_vname[i]])
    }
    
    cube2[,c2_xname] <- as.numeric(cube2[,c2_xname])
    cube2[,c2_yname] <- as.numeric(cube2[,c2_yname])
    
    for (i in 1:length(c2_vname)){
      cube2[,c2_vname[i]] <- as.numeric(cube2[,c2_vname[i]])
    }
    
    #print('reference dates')
    c1_ref_date <- input$c1_ref_date
    
    c2_ref_date <- input$c2_ref_date
    
    #print('convert to POSIX')
    tryCatch({
      new_times <- as.Date(as.POSIXct(cube1[,c1_tname], origin = c1_ref_date))
      
      if (min(new_times) == max(new_times)){
        new_times <- as.Date(as.POSIXct(cube1[,c1_tname]*86400, origin = c1_ref_date))
      }
      
      cube1[,c1_tname] <- new_times
    },
    error = function(e){
      stop('Error: The values in the time column of Cube 1 cannot be converted to dates')
    })
    
    tryCatch({
      new_times <- as.Date(as.POSIXct(cube2[,c2_tname], origin = c2_ref_date))
      
      if (min(new_times) == max(new_times)){
        new_times <- as.Date(as.POSIXct(cube2[,c2_tname]*86400, origin = c2_ref_date))
      }
      
      cube2[,c2_tname] <- new_times
    },
    error = function(e){
      stop('Error: The values in the time column of Cube 2 cannot be converted to dates')
    })
    
    if (max(cube1[,c1_tname]) < min(cube2[,c2_tname]) | max(cube2[,c2_tname]) < min(cube1[,c1_tname])){
      stop('Error: The temporal extents of the two cubes do not overlap.')
    }
    
    #print('time set-up')
    min_shared <- max(min(cube1[,c1_tname]), min(cube2[,c2_tname]))
    max_shared <- min(max(cube1[,c1_tname]), max(cube2[,c2_tname]))

    cube1_intt <- which((cube1[,c1_tname] >= min_shared) & (cube1[,c1_tname] <= max_shared))
    cube2_intt <- which((cube2[,c2_tname] >= min_shared) & (cube2[,c2_tname] <= max_shared))

    cube1 <- cube1[cube1_intt,]
    cube2 <- cube2[cube2_intt,]

    cube1_unique <- unique(cube1[,c1_tname])
    cube2_unique <- unique(cube2[,c2_tname])

    cube1_tres <- difftime(cube1_unique[2], cube1_unique[1], units = 'days')
    cube2_tres <- difftime(cube2_unique[2], cube2_unique[1], units = 'days')
    
    time_finished <- FALSE

    if (cube1_tres == cube2_tres){
      output$tres_message <- renderUI({
        h4(paste0('The temporal resolutions of the cubes match!'))
      })

      c1_tinds <- which(cube1[,c1_tname] %in% cube2[,c2_tname])
      c2_tinds <- which(cube2[,c2_tname] %in% cube1[,c1_tname])

      cube1 <- cube1[c1_tinds,]
      cube2 <- cube2[c2_tinds,]

      colnames(cube1)[c1_yname] <- 'lat'
      colnames(cube1)[c1_xname] <- 'lon'
      colnames(cube1)[c1_tname] <- 'time'

      colnames(cube2)[c2_yname] <- 'lat'
      colnames(cube2)[c2_xname] <- 'lon'
      colnames(cube2)[c2_tname] <- 'time'
      
      time_finished <- TRUE

      output$time_done <- renderUI({
        h4('Finished Combining the Time Data!')
      })
      
      #output$continue_button <- renderUI({
        #actionButton('continue', 'Continue')
      #})
      
      withProgress(message = 'Combining Cubes', {
        cube1_sf <- st_as_sf(x = cube1, coords = c('lon', 'lat'), crs = '+proj=longlat +datum=WGS84')
        cube2_sf <- st_as_sf(x = cube2, coords = c('lon', 'lat'), crs = '+proj=longlat +datum=WGS84')
        
        cube1_tmp <- as.data.frame(tidyr::extract(cube1_sf, geometry, into = c('lon', 'lat'), '\\((.*),(.*)\\)'))
        cube2_tmp <- as.data.frame(tidyr::extract(cube2_sf, geometry, into = c('lon', 'lat'), '\\((.*),(.*)\\)'))
        
        cube1_tmp <- cube1_tmp %>% dplyr::select(!c('geometry'))
        cube1_tmp <- cube1_tmp %>% dplyr::select(!c('geometry'))
        
        c1_ind <- seq(1, length(colnames(cube1_tmp)))
        c1_xindx <- which(colnames(cube1_tmp) %in% xnames)
        c1_yindx <- which(colnames(cube1_tmp) %in% ynames)
        c1_tindx <- which(colnames(cube1_tmp) %in% tnames)
        c1_vindx <- c1_ind[-c(c1_xindx, c1_yindx, c1_tindx)]
        c1_varname <- colnames(cube1_tmp)[c1_vindx]
        
        cube2_tmp <- cube2_tmp %>% dplyr::select(!c('geometry'))
        cube2_tmp <- cube2_tmp %>% dplyr::select(!c('geometry'))
        
        c2_ind <- seq(1, length(colnames(cube2_tmp)))
        c2_xindx <- which(colnames(cube2_tmp) %in% xnames)
        c2_yindx <- which(colnames(cube2_tmp) %in% ynames)
        c2_tindx <- which(colnames(cube2_tmp) %in% tnames)
        c2_vindx <- c2_ind[-c(c2_xindx, c2_yindx, c2_tindx)]
        c2_varname <- colnames(cube2_tmp)[c2_vindx]
        
        cube1_tmp <- dplyr::select(cube1_tmp, all_of(c(c1_xindx, c1_yindx, c1_tindx, c1_vindx)))
        colnames(cube1_tmp) <- c('lon', 'lat', 'time', c1_varname)
        cube1_tmp$lon <- as.numeric(cube1_tmp$lon)
        cube1_tmp$lat <- as.numeric(cube1_tmp$lat)
        
        cube2_tmp <- dplyr::select(cube2_tmp, all_of(c(c2_xindx, c2_yindx, c2_tindx, c2_vindx)))
        colnames(cube2_tmp) <- c('lon', 'lat', 'time', c2_varname)
        cube2_tmp$lon <- as.numeric(cube2_tmp$lon)
        cube2_tmp$lat <- as.numeric(cube2_tmp$lat)
        
        cube1_tmp <- na.omit(cube1_tmp)
        cube2_tmp <- na.omit(cube2_tmp)
        
        c1_xmin <- min(cube1_tmp$lon)
        c1_xmax <- max(cube1_tmp$lon)
        
        c1_ymin <- min(cube1_tmp$lat)
        c1_ymax <- max(cube1_tmp$lat)
        
        c1_tmin <- min(cube1_tmp$time)
        c1_tmax <- max(cube1_tmp$time)
        
        c2_xmin <- min(cube2_tmp$lon)
        c2_xmax <- max(cube2_tmp$lon)
        
        c2_ymin <- min(cube2_tmp$lat)
        c2_ymax <- max(cube2_tmp$lat)
        
        c2_tmin <- min(cube2_tmp$time)
        c2_tmax <- max(cube2_tmp$time)
        
        output$cube3d <- renderRglwidget({
          options(rgl.printRglwidget = TRUE)
          rgl.bg(color = "white")
          
          c3d <- cube3d(color="mediumslateblue", alpha=0.25)
          c1x = c(c1_xmin, c1_xmax, c1_xmin, c1_xmax, c1_xmin, c1_xmax, c1_xmin, c1_xmax)
          c1y = c(c1_ymin, c1_ymin, c1_ymax, c1_ymax, c1_ymin, c1_ymin, c1_ymax, c1_ymax)
          c1z = c(c1_tmin, c1_tmin, c1_tmin, c1_tmin, c1_tmax, c1_tmax, c1_tmax, c1_tmax)
          
          c3d$vb[1,] <- c1x
          c3d$vb[2,] <- c1y
          c3d$vb[3,] <- c1z
          
          c3d2 <- cube3d(color="mediumspringgreen", alpha=0.5)
          c2x = c(c2_xmin, c2_xmax, c2_xmin, c2_xmax, c2_xmin, c2_xmax, c2_xmin, c2_xmax)
          c2y = c(c2_ymin, c2_ymin, c2_ymax, c2_ymax, c2_ymin, c2_ymin, c2_ymax, c2_ymax)
          c2z = c(c2_tmin, c2_tmin, c2_tmin, c2_tmin, c2_tmax, c2_tmax, c2_tmax, c2_tmax)
          
          c3d2$vb[1,] <- c2x
          c3d2$vb[2,] <- c2y
          c3d2$vb[3,] <- c2z
          
          fig <- plot3d(c3d, box = FALSE)
          fig <- shade3d(c3d2)
          
          rglwidget()
        })
        
        output$cube_plot <- renderPlot({
          cube_plot <- ggplot() +
            geom_segment(data = cube1_tmp, aes(lon, lat, xend = max(lon), yend = lat), color = 'blue', size=.25) +
            geom_segment(data = cube1_tmp, aes(lon, lat, xend = lon, yend = max(lat)), color = 'blue', size=.25) +
            geom_segment(data = cube2_tmp, aes(lon, lat, xend = max(lon), yend = lat), color = 'red', size=.25) +
            geom_segment(data = cube2_tmp, aes(lon, lat, xend = lon, yend = max(lat)), color = 'red', size=.25) +
            geom_point(data = cube1_tmp, aes(lon, lat), color = 'blue', size = .8, show.legend = FALSE) +
            geom_point(data = cube2_tmp, aes(lon, lat), color = 'red', size = .8, show.legend = FALSE) +
            annotate(geom = 'rect', xmin = min(cube1_tmp$lon), xmax = max(cube1_tmp$lon), ymin = min(cube1_tmp$lat), ymax = max(cube1_tmp$lat),fill = 'blue', colour = 'blue', alpha = 0.3) +
            annotate(geom = 'rect', xmin = min(cube2_tmp$lon), xmax = max(cube2_tmp$lon), ymin = min(cube2_tmp$lat), ymax = max(cube2_tmp$lat), fill = 'red', colour = 'red', alpha = 0.3) +
            coord_quickmap()
          
          cube_plot
        })
        
        c1_xres <- resolution(cube1_tmp$lon)
        c1_yres <- resolution(cube1_tmp$lat)
        
        c2_xres <- resolution(cube2_tmp$lon)
        c2_yres <- resolution(cube2_tmp$lat)
        
        min_xres <- min(c1_xres, c2_xres)
        min_yres <- min(c1_yres, c2_yres)
        
        max_xres <- max(c1_xres, c2_xres)
        max_yres <- max(c1_yres, c2_yres)
        
        union_xmin <- min(c1_xmin, c2_xmin)
        union_xmax <- max(c1_xmax, c2_xmax)
        union_ymin <- min(c1_ymin, c2_ymin)
        union_ymax <- max(c1_ymax, c2_ymax)
        
        intersect_xmin <- max(c1_xmin, c2_xmin)
        intersect_xmax <- min(c1_xmax, c2_xmax)
        intersect_ymin <- max(c1_ymin, c2_ymin)
        intersect_ymax <- min(c1_ymax, c2_ymax)
        
        c1_subset_len <- length(cube1_tmp[(cube1_tmp$lon < intersect_xmin) & (cube1_tmp$lon > intersect_xmax) & (cube1_tmp$lat < intersect_ymin) & (cube1_tmp$lat > intersect_ymax),1])
        c2_subset_len <- length(cube2_tmp[(cube2_tmp$lon < intersect_xmin) & (cube2_tmp$lon > intersect_xmax) & (cube2_tmp$lat < intersect_ymin) & (cube2_tmp$lat > intersect_ymax),1])
        
        c1_lost_data <- c1_subset_len/(length(cube1_tmp$lon))
        c2_lost_data <- c2_subset_len/(length(cube2_tmp$lon))
        
        if (c1_xmin == c2_xmin && c1_xmax == c2_xmax && c1_ymin == c2_ymin && c1_ymax == c2_ymax && c1_xres == c2_xres && c1_yres == c2_yres){
          combined_cube <- data.frame(dplyr::select(cube1, c(c1_xname, c1_yname, c1_tname, c1_vname)), dplyr::select(cube2, c(c2_vname)))
          output$cube <- renderDataTable(combined_cube)
          
          output$c1_krigehist <- renderPlot({hist(combined_cube[,4], col = 'blue', main = paste0('Histogram of Kriged Values of ', colnames(combined_cube)[4]))})
          
          output$c2_krigehist <- renderPlot({hist(combined_cube[,5], col = 'green', main = paste0('Histogram of Kriged Values of ', colnames(combined_cube)[5]))})
          
          
          observeEvent(input$save_yes, {
            output$save_filepath <- renderUI({
              textInput('file_path', 'Enter the full path to the directory where you would like to store your file, i.e. /users/JohnDoe/Downloads/')
            })
            
            output$save_filename <- renderUI({
              textInput('file_name', 'Enter the name you would like to give your file, ending in .csv, i.e. data.csv')
            })
            
            output$submit_file <- renderUI({
              actionButton('file', 'Submit file path and file name!')
            })
            
            observeEvent(input$file, {
              filename <- paste0(input$file_path, input$file_name)
              
              write.csv(combined_cube, filename, row.names = FALSE)
            })
          }, once = TRUE)
          
          return(combined_cube)
        }
        
        else if ((c1_xmin != c2_xmin || c1_xmax != c2_xmax || c1_ymin != c2_ymin || c1_ymax != c2_ymax) && (c1_xres != c2_xres || c1_yres != c2_yres)){
          output$user_message <- renderUI({
            h4(paste0('The cubes have different spatial extents and different spatial resolutions.  You can select to perform kriging to the intersection of the cubes at the coarser resolution, to the intersection of the cubes at the finer resolution, to the union of the cubes
              with the coarser resolution, or to the union of the cubes with the finer resolution.  Note that performing kriging on the intersection will result in a loss of ', 100*c1_lost_data, '% of the data in Cube 1 and ', 100*c2_lost_data, '% of the data in Cube 2.  However,
              performing kriging to the intersection and the coarser resolution will result in more accurate predictions from kriging than performing kriging to either the finer resoution or the union of the cubes.'))
          })
          output$buttons <- renderUI({
            fluidRow(column(3, actionButton('int_coarse', 'Intersection and Coarser Resolution')), column(3, actionButton('int_fine', 'Intersection and Finer Resolution')), column(3, actionButton('union_coarse', 'Union and Coarser Resolution')),
                     column(3, actionButton('union_fine', 'Union and Finer Resolution')))
          })
        }
        
        else if (c1_xres != c2_xres || c1_yres != c2_yres){
          output$user_message <- renderUI({
            h4('The cubes have different spatial resolutions.  You can either perform kriging to the coarser resolution or to the finer resolution.  Note that kriging to the coarser resolution might result in the loss of data, but will produce more
                   accurate predictions from kriging.')
          })
          output$buttons <- renderUI({
            fluidRow(column(3, actionButton('coarse', 'Coarser Resolution')), column(3, 'fine', 'Finer Resolution'))
          })
        }
        
        else if (c1_xmin != c2_xmin || c1_xmax != c2_xmax || c1_ymin != c2_ymin || c1_ymax != c2_ymax){
          output$user_message <- renderUI({
            h4(paste0('The cubes have different spatial extents.  You can select to either perform kriging to the intersection of the cubes or to the union of the cubes.  Note that performing kriging to the intersection of the cubes will result in a loss of ',
                      100*c1_lost_data, '% of the data in Cube 1 and ', 100*c2_lost_data, '% of the data in Cube 2 but will result in more accurate predictions than kriging to the union of the cubes.'))
          })
          output$buttons <- renderUI({
            fluidRow(column(3, actionButton('int', 'Intersection')), column(3, actionButton('union', 'Union')))
          })
        }
        
        krige_lons <- c()
        krige_lats <- c()
        
        observeEvent(input$int_coarse, {
          krige_lons <- seq(intersect_xmin, intersect_xmax, by = max_xres)
          krige_lats <- seq(intersect_ymin, intersect_ymax, by = max_yres)
        }, once = TRUE)
        
        observeEvent(input$int_fine, {
          krige_lons <- seq(intersect_xmin, intersect_xmax, by = min_xres)
          krige_lats <- seq(intersect_ymin, intersect_ymax, by = min_yres)
        }, once = TRUE)
        
        observeEvent(input$union_coarse, {
          krige_lons <- seq(union_xmin, union_xmax, by = max_xres)
          krige_lats <- seq(union_ymin, union_ymax, by = max_yres)
        }, once = TRUE)
        
        observeEvent(input$union_fine, {
          krige_lons <- seq(union_xmin, union_xmax, by = min_xres)
          krige_lats <- seq(union_ymin, union_ymax, by = min_yres)
        }, once = TRUE)
        
        observeEvent(input$coarse, {
          krige_lons <- seq(c1_xmin, c1_xmax, by = min_xres)
          krige_lats <- seq(c1_ymin, c1_ymax, by = min_yres)
        }, once = TRUE)
        
        observeEvent(input$fine, {
          krige_lons <- seq(c1_xmin, c1_xmax, by = max_xres)
          krige_lats <- seq(c1_ymin, c1_ymax, by = max_yres)
        }, once = TRUE)
        
        observeEvent(input$int, {
          krige_lons <- seq(intersect_xmin, intersect_xmax, by = c1_xres)
          krige_lats <- seq(intersect_ymin, intersect_ymax, by = c1_yres)
        }, once = TRUE)
        
        observeEvent(input$union, {
          krige_lons <- seq(union_xmin, union_xmax, by = c1_xres)
          krige_lats <- seq(union_ymin, union_ymax, by = c1_yres)
        }, once = TRUE)
        
        n <- length(krige_lons)*length(krige_lats)*length(unique(cube1_tmp$time))
        
        m <- length(krige_lons)*length(krige_lats)
        
        krige_c1 <- matrix(NA, nrow = n, ncol = 2)
        krige_c2 <- matrix(NA, nrow = n, ncol = 2)
        
        c1_indx <- 1
        c2_indx <- 1
        
        c1_names <- c()
        c2_names <- c()
        
        for (t in 1:length(unique(cube1_tmp$time))){
          time <- unique(cube1_tmp$time)[1]
          c1 <- cube1_tmp[cube1_tmp$time == time,]
          c2 <- cube2_tmp[cube2_tmp$time == time,]
          
          tmp <- dplyr::select(c1, -c('time'))
          colnames(tmp) <- c('lon', 'lat', 'var')
          coordinates(tmp) <- c('lon', 'lat')
          proj4string(tmp) <- crs('+proj=longlat +datum=WGS84')
          
          c1_krige <- krige_cubes(tmp, krige_lons, krige_lats, c1_varname)
          
          if (t == 1){
            c1_names <- c(c1_names, colnames(c1_krige))
          }
          
          c1_krige <- as.matrix(c1_krige)
          
          for (j in 1:dim(c1_krige)[1]){
            krige_c1[c1_indx,] <- c1_krige[j,]
            c1_indx <- c1_indx + 1
          }
          
          colnames(c2) <- c('lon', 'lat', 'time', 'var')
          tmp <- dplyr::select(c2, -c('time'))
          coordinates(tmp) <- c('lon', 'lat')
          proj4string(tmp) <- crs('+proj=longlat +datum=WGS84')
          
          c2_krige <- krige_cubes(tmp, krige_lons, krige_lats, c2_varname)
          
          if (t == 1){
            c2_names <- c(c2_names, colnames(c2_krige))
          }
          
          c2_krige <- as.matrix(c2_krige)
          
          for (j in 1:dim(c2_krige)[1]){
            krige_c2[c2_indx,] <- c2_krige[j,]
            c2_indx <- c2_indx + 1
          }
          
        }
        
        combined_cube <- as.data.frame(cbind(expand.grid(krige_lons, krige_lats, unique(cube1_tmp$time)), krige_c1, krige_c2))
        
        colnames(combined_cube) <- c('lon', 'lat', 'time', c1_names, c2_names)
        
        output$c1_krigehist <- renderPlot({hist(combined_cube[,4], col = 'blue', main = paste0('Histogram of Kriged Values of ', colnames(combined_cube)[4]))})
        
        output$c1_varhist <- renderPlot({hist(combined_cube[,5], col = 'green', main = paste0('Histogram of Kriged Variance of ', colnames(combined_cube)[5]))})
        
        output$c2_krigehist <- renderPlot({hist(combined_cube[,6], col = 'red', main = paste0('Histogram of Kriged Values of ', colnames(combined_cube)[6]))})
        
        output$c2_varhist <- renderPlot({hist(combined_cube[,7], col = 'purple', main = paste0('Histogram of Kriged Variance of ', colnames(combined_cube)[7]))})
        
        output$cube <- renderDataTable(combined_cube)
        
        observeEvent(input$save_yes, {
          output$save_filepath <- renderUI({
            textInput('file_path', 'Enter the full path to the directory where you would like to store your file, i.e. /users/JohnDoe/Downloads/')
          })
          
          output$save_filename <- renderUI({
            textInput('file_name', 'Enter the name you would like to give your file, ending in .csv, i.e. data.csv')
          })
          
          output$submit_file <- renderUI({
            actionButton('file', 'Submit file path and file name!')
          })
          
          observeEvent(input$file, {
            filename <- paste0(input$file_path, input$file_name)
            
            write.csv(combined_cube, filename, row.names = FALSE)
          })
        }, once = TRUE)
      })
    }

    if (cube1_tres != cube2_tres){
      output$tres_message <- renderUI({
        h4(paste0('The temporal resolutions of the cubes do not match.  Would you like the final cube to have finer or coarser temporal resolution?'))
      })
      output$tres_decision <- renderUI({
        fluidRow(column(3, actionButton('tres_fine', 'Finer Resolution')), column(3, actionButton('tres_coarse', 'Coarser Resolution')))
      })
    }

    cube1_tres <- as.numeric(cube1_tres)
    cube2_tres <- as.numeric(cube2_tres)

    observeEvent(input$tres_fine, {
      #print('Combining with Finer Resolution')
      
      min_tres <- min(cube1_tres, cube2_tres)

      if (min_tres == 1){
        inc <- 'days'
      }
      else if (min_tres == 7){
        inc <- 'weeks'
      }
      else if (any(min_tres %in% c(30, 31))){
        inc <- 'months'
      }
      else if (any(min_tres %in% c(365, 366))){
        inc <- 'years'
      }

      if (min_tres == cube1_tres){
        new_cube2 <- data.frame()

        times <- seq(cube2_unique[1], cube2_unique[length(cube2_unique)], inc)

        times <- times[which(times %in% cube1_unique)]

        lats <- unique(cube2[,c2_yname])

        lons <- unique(cube2[,c2_xname])

        reps <- length(lats)*length(lons)

        new_cube2 <- expand.grid(lons, lats, times)

        for (j in 1:length(c2_vname)){
          time_val <- cube2[which(cube2[,c2_tname] %in% times)[1], c2_vname[j]]

          vals <- rep(time_val, reps)

          new_cube2[,j+3] <- vals
        }

        colnames(new_cube2) <- c('lon', 'lat', 'time', c2_varname)

        cube2 <- new_cube2

        c1_tinds <- which(cube1[,c1_tname] %in% cube2[,3])

        cube1 <- cube1[c1_tinds,]
      }
      if (min_tres == cube2_tres){
        new_cube1 <- data.frame()

        times <- seq(cube1_unique[1], cube1_unique[length(cube1_unique)], inc)

        times <- times[which(times %in% cube2_unique)]

        lats <- unique(cube1[,c1_yname])

        lons <- unique(cube1[,c1_xname])

        reps <- length(lats)*length(lons)

        new_cube1 <- expand.grid(lons, lats, times)

        for (j in 1:length(c1_vname)){
          time_val <- cube1[which(cube1[,c1_tname] %in% times)[1], c1_vname[j]]

          vals <- rep(time_val, reps)

          new_cube1[,j+3] <- vals
        }

        colnames(new_cube1) <- c('lon', 'lat', 'time', c1_varname)

        cube1 <- new_cube1

        c2_tinds <- which(cube2[,c2_tname] %in% cube1[,3])

        cube2 <- cube2[c2_tinds,]
      }

      output$time_done <- renderUI({
        h4('Finished Combining the Time Data!')
      })
      
      #output$continue_button <- renderUI({
        #actionButton('continue', 'Continue')
      #})
      
      withProgress(message = 'Combining Cubes', {
        cube1_sf <- st_as_sf(x = cube1, coords = c('lon', 'lat'), crs = '+proj=longlat +datum=WGS84')
        cube2_sf <- st_as_sf(x = cube2, coords = c('lon', 'lat'), crs = '+proj=longlat +datum=WGS84')
        
        cube1_tmp <- as.data.frame(tidyr::extract(cube1_sf, geometry, into = c('lon', 'lat'), '\\((.*),(.*)\\)'))
        cube2_tmp <- as.data.frame(tidyr::extract(cube2_sf, geometry, into = c('lon', 'lat'), '\\((.*),(.*)\\)'))
        
        cube1_tmp <- cube1_tmp %>% dplyr::select(!c('geometry'))
        cube1_tmp <- cube1_tmp %>% dplyr::select(!c('geometry'))
        
        c1_ind <- seq(1, length(colnames(cube1_tmp)))
        c1_xindx <- which(colnames(cube1_tmp) %in% xnames)
        c1_yindx <- which(colnames(cube1_tmp) %in% ynames)
        c1_tindx <- which(colnames(cube1_tmp) %in% tnames)
        c1_vindx <- c1_ind[-c(c1_xindx, c1_yindx, c1_tindx)]
        c1_varname <- colnames(cube1_tmp)[c1_vindx]
        
        cube2_tmp <- cube2_tmp %>% dplyr::select(!c('geometry'))
        cube2_tmp <- cube2_tmp %>% dplyr::select(!c('geometry'))
        
        c2_ind <- seq(1, length(colnames(cube2_tmp)))
        c2_xindx <- which(colnames(cube2_tmp) %in% xnames)
        c2_yindx <- which(colnames(cube2_tmp) %in% ynames)
        c2_tindx <- which(colnames(cube2_tmp) %in% tnames)
        c2_vindx <- c2_ind[-c(c2_xindx, c2_yindx, c2_tindx)]
        c2_varname <- colnames(cube2_tmp)[c2_vindx]
        
        cube1_tmp <- dplyr::select(cube1_tmp, all_of(c(c1_xindx, c1_yindx, c1_tindx, c1_vindx)))
        colnames(cube1_tmp) <- c('lon', 'lat', 'time', c1_varname)
        cube1_tmp$lon <- as.numeric(cube1_tmp$lon)
        cube1_tmp$lat <- as.numeric(cube1_tmp$lat)
        
        cube2_tmp <- dplyr::select(cube2_tmp, all_of(c(c2_xindx, c2_yindx, c2_tindx, c2_vindx)))
        colnames(cube2_tmp) <- c('lon', 'lat', 'time', c2_varname)
        cube2_tmp$lon <- as.numeric(cube2_tmp$lon)
        cube2_tmp$lat <- as.numeric(cube2_tmp$lat)
        
        cube1_tmp <- na.omit(cube1_tmp)
        cube2_tmp <- na.omit(cube2_tmp)
        
        c1_xmin <- min(cube1_tmp$lon)
        c1_xmax <- max(cube1_tmp$lon)
        
        c1_ymin <- min(cube1_tmp$lat)
        c1_ymax <- max(cube1_tmp$lat)
        
        c1_tmin <- min(cube1_tmp$time)
        c1_tmax <- max(cube1_tmp$time)
        
        c2_xmin <- min(cube2_tmp$lon)
        c2_xmax <- max(cube2_tmp$lon)
        
        c2_ymin <- min(cube2_tmp$lat)
        c2_ymax <- max(cube2_tmp$lat)
        
        c2_tmin <- min(cube2_tmp$time)
        c2_tmax <- max(cube2_tmp$time)
        
        output$cube3d <- renderRglwidget({
          options(rgl.printRglwidget = TRUE)
          rgl.bg(color = "white")
          
          c3d <- cube3d(color="mediumslateblue", alpha=0.25)
          c1x = c(c1_xmin, c1_xmax, c1_xmin, c1_xmax, c1_xmin, c1_xmax, c1_xmin, c1_xmax)
          c1y = c(c1_ymin, c1_ymin, c1_ymax, c1_ymax, c1_ymin, c1_ymin, c1_ymax, c1_ymax)
          c1z = c(c1_tmin, c1_tmin, c1_tmin, c1_tmin, c1_tmax, c1_tmax, c1_tmax, c1_tmax)
          
          c3d$vb[1,] <- c1x
          c3d$vb[2,] <- c1y
          c3d$vb[3,] <- c1z
          
          c3d2 <- cube3d(color="mediumspringgreen", alpha=0.5)
          c2x = c(c2_xmin, c2_xmax, c2_xmin, c2_xmax, c2_xmin, c2_xmax, c2_xmin, c2_xmax)
          c2y = c(c2_ymin, c2_ymin, c2_ymax, c2_ymax, c2_ymin, c2_ymin, c2_ymax, c2_ymax)
          c2z = c(c2_tmin, c2_tmin, c2_tmin, c2_tmin, c2_tmax, c2_tmax, c2_tmax, c2_tmax)
          
          c3d2$vb[1,] <- c2x
          c3d2$vb[2,] <- c2y
          c3d2$vb[3,] <- c2z
          
          fig <- plot3d(c3d, box = FALSE)
          fig <- shade3d(c3d2)
          
          rglwidget()
        })
        
        output$cube_plot <- renderPlot({
          cube_plot <- ggplot() +
            geom_segment(data = cube1_tmp, aes(lon, lat, xend = max(lon), yend = lat), color = 'blue', size=.25) +
            geom_segment(data = cube1_tmp, aes(lon, lat, xend = lon, yend = max(lat)), color = 'blue', size=.25) +
            geom_segment(data = cube2_tmp, aes(lon, lat, xend = max(lon), yend = lat), color = 'red', size=.25) +
            geom_segment(data = cube2_tmp, aes(lon, lat, xend = lon, yend = max(lat)), color = 'red', size=.25) +
            geom_point(data = cube1_tmp, aes(lon, lat), color = 'blue', size = .8, show.legend = FALSE) +
            geom_point(data = cube2_tmp, aes(lon, lat), color = 'red', size = .8, show.legend = FALSE) +
            annotate(geom = 'rect', xmin = min(cube1_tmp$lon), xmax = max(cube1_tmp$lon), ymin = min(cube1_tmp$lat), ymax = max(cube1_tmp$lat),fill = 'blue', colour = 'blue', alpha = 0.3) +
            annotate(geom = 'rect', xmin = min(cube2_tmp$lon), xmax = max(cube2_tmp$lon), ymin = min(cube2_tmp$lat), ymax = max(cube2_tmp$lat), fill = 'red', colour = 'red', alpha = 0.3) +
            coord_quickmap()
          
          cube_plot
        })
        
        c1_xres <- resolution(cube1_tmp$lon)
        c1_yres <- resolution(cube1_tmp$lat)
        
        c2_xres <- resolution(cube2_tmp$lon)
        c2_yres <- resolution(cube2_tmp$lat)
        
        min_xres <- min(c1_xres, c2_xres)
        min_yres <- min(c1_yres, c2_yres)
        
        max_xres <- max(c1_xres, c2_xres)
        max_yres <- max(c1_yres, c2_yres)
        
        union_xmin <- min(c1_xmin, c2_xmin)
        union_xmax <- max(c1_xmax, c2_xmax)
        union_ymin <- min(c1_ymin, c2_ymin)
        union_ymax <- max(c1_ymax, c2_ymax)
        
        intersect_xmin <- max(c1_xmin, c2_xmin)
        intersect_xmax <- min(c1_xmax, c2_xmax)
        intersect_ymin <- max(c1_ymin, c2_ymin)
        intersect_ymax <- min(c1_ymax, c2_ymax)
        
        c1_subset_len <- length(cube1_tmp[(cube1_tmp$lon < intersect_xmin) & (cube1_tmp$lon > intersect_xmax) & (cube1_tmp$lat < intersect_ymin) & (cube1_tmp$lat > intersect_ymax),1])
        c2_subset_len <- length(cube2_tmp[(cube2_tmp$lon < intersect_xmin) & (cube2_tmp$lon > intersect_xmax) & (cube2_tmp$lat < intersect_ymin) & (cube2_tmp$lat > intersect_ymax),1])
        
        c1_lost_data <- c1_subset_len/(length(cube1_tmp$lon))
        c2_lost_data <- c2_subset_len/(length(cube2_tmp$lon))
        
        if (c1_xmin == c2_xmin && c1_xmax == c2_xmax && c1_ymin == c2_ymin && c1_ymax == c2_ymax && c1_xres == c2_xres && c1_yres == c2_yres){
          combined_cube <- data.frame(dplyr::select(cube1, c(c1_xname, c1_yname, c1_tname, c1_vname)), dplyr::select(cube2, c(c2_vname)))
          output$cube <- renderDataTable(combined_cube)
          
          output$c1_krigehist <- renderPlot({hist(combined_cube[,4], col = 'blue', main = paste0('Histogram of Kriged Values of ', colnames(combined_cube)[4]))})
          
          output$c2_krigehist <- renderPlot({hist(combined_cube[,5], col = 'green', main = paste0('Histogram of Kriged Values of ', colnames(combined_cube)[5]))})
          
          observeEvent(input$save_yes, {
            output$save_filepath <- renderUI({
              textInput('file_path', 'Enter the full path to the directory where you would like to store your file, i.e. /users/JohnDoe/Downloads/')
            })
            
            output$save_filename <- renderUI({
              textInput('file_name', 'Enter the name you would like to give your file, ending in .csv, i.e. data.csv')
            })
            
            output$submit_file <- renderUI({
              actionButton('file', 'Submit file path and file name!')
            })
            
            observeEvent(input$file, {
              filename <- paste0(input$file_path, input$file_name)
              
              write.csv(combined_cube, filename, row.names = FALSE)
            })
          }, once = TRUE)
          
          return(combined_cube)
        }
        
        else if ((c1_xmin != c2_xmin || c1_xmax != c2_xmax || c1_ymin != c2_ymin || c1_ymax != c2_ymax) && (c1_xres != c2_xres || c1_yres != c2_yres)){
          output$user_message <- renderUI({
            h4(paste0('The cubes have different spatial extents and different spatial resolutions.  You can select to perform kriging to the intersection of the cubes at the coarser resolution, to the intersection of the cubes at the finer resolution, to the union of the cubes
              with the coarser resolution, or to the union of the cubes with the finer resolution.  Note that performing kriging on the intersection will result in a loss of ', 100*c1_lost_data, '% of the data in Cube 1 and ', 100*c2_lost_data, '% of the data in Cube 2.  However,
              performing kriging to the intersection and the coarser resolution will result in more accurate predictions from kriging than performing kriging to either the finer resoution or the union of the cubes.'))
          })
          output$buttons <- renderUI({
            fluidRow(column(3, actionButton('int_coarse', 'Intersection and Coarser Resolution')), column(3, actionButton('int_fine', 'Intersection and Finer Resolution')), column(3, actionButton('union_coarse', 'Union and Coarser Resolution')),
                     column(3, actionButton('union_fine', 'Union and Finer Resolution')))
          })
        }
        
        else if (c1_xres != c2_xres || c1_yres != c2_yres){
          output$user_message <- renderUI({
            h4('The cubes have different spatial resolutions.  You can either perform kriging to the coarser resolution or to the finer resolution.  Note that kriging to the coarser resolution might result in the loss of data, but will produce more
                   accurate predictions from kriging.')
          })
          output$buttons <- renderUI({
            fluidRow(column(3, actionButton('coarse', 'Coarser Resolution')), column(3, 'fine', 'Finer Resolution'))
          })
        }
        
        else if (c1_xmin != c2_xmin || c1_xmax != c2_xmax || c1_ymin != c2_ymin || c1_ymax != c2_ymax){
          output$user_message <- renderUI({
            h4(paste0('The cubes have different spatial extents.  You can select to either perform kriging to the intersection of the cubes or to the union of the cubes.  Note that performing kriging to the intersection of the cubes will result in a loss of ',
                      100*c1_lost_data, '% of the data in Cube 1 and ', 100*c2_lost_data, '% of the data in Cube 2 but will result in more accurate predictions than kriging to the union of the cubes.'))
          })
          output$buttons <- renderUI({
            fluidRow(column(3, actionButton('int', 'Intersection')), column(3, actionButton('union', 'Union')))
          })
        }
        
        krige_lons <- c()
        krige_lats <- c()
        
        observeEvent(input$int_coarse, {
          krige_lons <- seq(intersect_xmin, intersect_xmax, by = max_xres)
          krige_lats <- seq(intersect_ymin, intersect_ymax, by = max_yres)
        }, once = TRUE)
        
        observeEvent(input$int_fine, {
          krige_lons <- seq(intersect_xmin, intersect_xmax, by = min_xres)
          krige_lats <- seq(intersect_ymin, intersect_ymax, by = min_yres)
        }, once = TRUE)
        
        observeEvent(input$union_coarse, {
          krige_lons <- seq(union_xmin, union_xmax, by = max_xres)
          krige_lats <- seq(union_ymin, union_ymax, by = max_yres)
        }, once = TRUE)
        
        observeEvent(input$union_fine, {
          krige_lons <- seq(union_xmin, union_xmax, by = min_xres)
          krige_lats <- seq(union_ymin, union_ymax, by = min_yres)
        }, once = TRUE)
        
        observeEvent(input$coarse, {
          krige_lons <- seq(c1_xmin, c1_xmax, by = min_xres)
          krige_lats <- seq(c1_ymin, c1_ymax, by = min_yres)
        }, once = TRUE)
        
        observeEvent(input$fine, {
          krige_lons <- seq(c1_xmin, c1_xmax, by = max_xres)
          krige_lats <- seq(c1_ymin, c1_ymax, by = max_yres)
        }, once = TRUE)
        
        observeEvent(input$int, {
          krige_lons <- seq(intersect_xmin, intersect_xmax, by = c1_xres)
          krige_lats <- seq(intersect_ymin, intersect_ymax, by = c1_yres)
        }, once = TRUE)
        
        observeEvent(input$union, {
          krige_lons <- seq(union_xmin, union_xmax, by = c1_xres)
          krige_lats <- seq(union_ymin, union_ymax, by = c1_yres)
        }, once = TRUE)
        
        n <- length(krige_lons)*length(krige_lats)*length(unique(cube1_tmp$time))
        
        m <- length(krige_lons)*length(krige_lats)
        
        krige_c1 <- matrix(NA, nrow = n, ncol = 2)
        krige_c2 <- matrix(NA, nrow = n, ncol = 2)
        
        c1_indx <- 1
        c2_indx <- 1
        
        c1_names <- c()
        c2_names <- c()
        
        for (t in 1:length(unique(cube1_tmp$time))){
          time <- unique(cube1_tmp$time)[1]
          c1 <- cube1_tmp[cube1_tmp$time == time,]
          c2 <- cube2_tmp[cube2_tmp$time == time,]
          
          tmp <- dplyr::select(c1, -c('time'))
          colnames(tmp) <- c('lon', 'lat', 'var')
          coordinates(tmp) <- c('lon', 'lat')
          proj4string(tmp) <- crs('+proj=longlat +datum=WGS84')
          
          c1_krige <- krige_cubes(tmp, krige_lons, krige_lats, c1_varname)
          
          if (t == 1){
            c1_names <- c(c1_names, colnames(c1_krige))
          }
          
          c1_krige <- as.matrix(c1_krige)
          
          for (j in 1:dim(c1_krige)[1]){
            krige_c1[c1_indx,] <- c1_krige[j,]
            c1_indx <- c1_indx + 1
          }
          
          colnames(c2) <- c('lon', 'lat', 'time', 'var')
          tmp <- dplyr::select(c2, -c('time'))
          coordinates(tmp) <- c('lon', 'lat')
          proj4string(tmp) <- crs('+proj=longlat +datum=WGS84')
          
          c2_krige <- krige_cubes(tmp, krige_lons, krige_lats, c2_varname)
          
          if (t == 1){
            c2_names <- c(c2_names, colnames(c2_krige))
          }
          
          c2_krige <- as.matrix(c2_krige)
          
          for (j in 1:dim(c2_krige)[1]){
            krige_c2[c2_indx,] <- c2_krige[j,]
            c2_indx <- c2_indx + 1
          }
          
        }
        
        combined_cube <- as.data.frame(cbind(expand.grid(krige_lons, krige_lats, unique(cube1_tmp$time)), krige_c1, krige_c2))
        
        colnames(combined_cube) <- c('lon', 'lat', 'time', c1_names, c2_names)
        
        output$c1_krigehist <- renderPlot({hist(combined_cube[,4], col = 'blue', main = paste0('Histogram of Kriged Values of ', colnames(combined_cube)[4]))})
        
        output$c1_varhist <- renderPlot({hist(combined_cube[,5], col = 'green', main = paste0('Histogram of Kriged Variance of ', colnames(combined_cube)[5]))})
        
        output$c2_krigehist <- renderPlot({hist(combined_cube[,6], col = 'red', main = paste0('Histogram of Kriged Values of ', colnames(combined_cube)[6]))})
        
        output$c2_varhist <- renderPlot({hist(combined_cube[,7], col = 'purple', main = paste0('Histogram of Kriged Variance of ', colnames(combined_cube)[7]))})
        
        output$cube <- renderDataTable(combined_cube)
        
        observeEvent(input$save_yes, {
          output$save_filepath <- renderUI({
            textInput('file_path', 'Enter the full path to the directory where you would like to store your file, i.e. /users/JohnDoe/Downloads/')
          })
          
          output$save_filename <- renderUI({
            textInput('file_name', 'Enter the name you would like to give your file, ending in .csv, i.e. data.csv')
          })
          
          output$submit_file <- renderUI({
            actionButton('file', 'Submit file path and file name!')
          })
          
          observeEvent(input$file, {
            filename <- paste0(input$file_path, input$file_name)
            
            write.csv(combined_cube, filename, row.names = FALSE)
          })
        }, once = TRUE)
      })
    }, once = TRUE)

    observeEvent(input$tres_coarse, {
      #print('Combining with Coarser Resolution')
      
      c1_tinds <- which(cube1[,c1_tname] %in% cube2[,c2_tname])
      c2_tinds <- which(cube2[,c2_tname] %in% cube1[,c1_tname])

      cube1 <- cube1[c1_tinds,]
      cube2 <- cube2[c2_tinds,]

      colnames(cube1)[c1_yname] <- 'lat'
      colnames(cube1)[c1_xname] <- 'lon'
      colnames(cube1)[c1_tname] <- 'time'

      colnames(cube2)[c2_yname] <- 'lat'
      colnames(cube2)[c2_xname] <- 'lon'
      colnames(cube2)[c2_tname] <- 'time'
      
      output$time_done <- renderUI({
        h4('Finished Combining the Time Data!')
      })
      
      #output$continue_button <- renderUI({
        #actionButton('continue', 'Continue')
      #})
      
      withProgress(message = 'Combining Cubes', {
        cube1_sf <- st_as_sf(x = cube1, coords = c('lon', 'lat'), crs = '+proj=longlat +datum=WGS84')
        cube2_sf <- st_as_sf(x = cube2, coords = c('lon', 'lat'), crs = '+proj=longlat +datum=WGS84')
        
        cube1_tmp <- as.data.frame(tidyr::extract(cube1_sf, geometry, into = c('lon', 'lat'), '\\((.*),(.*)\\)'))
        cube2_tmp <- as.data.frame(tidyr::extract(cube2_sf, geometry, into = c('lon', 'lat'), '\\((.*),(.*)\\)'))
        
        cube1_tmp <- cube1_tmp %>% dplyr::select(!c('geometry'))
        cube1_tmp <- cube1_tmp %>% dplyr::select(!c('geometry'))
        
        c1_ind <- seq(1, length(colnames(cube1_tmp)))
        c1_xindx <- which(colnames(cube1_tmp) %in% xnames)
        c1_yindx <- which(colnames(cube1_tmp) %in% ynames)
        c1_tindx <- which(colnames(cube1_tmp) %in% tnames)
        c1_vindx <- c1_ind[-c(c1_xindx, c1_yindx, c1_tindx)]
        c1_varname <- colnames(cube1_tmp)[c1_vindx]
        
        cube2_tmp <- cube2_tmp %>% dplyr::select(!c('geometry'))
        cube2_tmp <- cube2_tmp %>% dplyr::select(!c('geometry'))
        
        c2_ind <- seq(1, length(colnames(cube2_tmp)))
        c2_xindx <- which(colnames(cube2_tmp) %in% xnames)
        c2_yindx <- which(colnames(cube2_tmp) %in% ynames)
        c2_tindx <- which(colnames(cube2_tmp) %in% tnames)
        c2_vindx <- c2_ind[-c(c2_xindx, c2_yindx, c2_tindx)]
        c2_varname <- colnames(cube2_tmp)[c2_vindx]
        
        cube1_tmp <- dplyr::select(cube1_tmp, all_of(c(c1_xindx, c1_yindx, c1_tindx, c1_vindx)))
        colnames(cube1_tmp) <- c('lon', 'lat', 'time', c1_varname)
        cube1_tmp$lon <- as.numeric(cube1_tmp$lon)
        cube1_tmp$lat <- as.numeric(cube1_tmp$lat)
        
        cube2_tmp <- dplyr::select(cube2_tmp, all_of(c(c2_xindx, c2_yindx, c2_tindx, c2_vindx)))
        colnames(cube2_tmp) <- c('lon', 'lat', 'time', c2_varname)
        cube2_tmp$lon <- as.numeric(cube2_tmp$lon)
        cube2_tmp$lat <- as.numeric(cube2_tmp$lat)
        
        cube1_tmp <- na.omit(cube1_tmp)
        cube2_tmp <- na.omit(cube2_tmp)
        
        c1_xmin <- min(cube1_tmp$lon)
        c1_xmax <- max(cube1_tmp$lon)
        
        c1_ymin <- min(cube1_tmp$lat)
        c1_ymax <- max(cube1_tmp$lat)
        
        c1_tmin <- min(cube1_tmp$time)
        c1_tmax <- max(cube1_tmp$time)
        
        c2_xmin <- min(cube2_tmp$lon)
        c2_xmax <- max(cube2_tmp$lon)
        
        c2_ymin <- min(cube2_tmp$lat)
        c2_ymax <- max(cube2_tmp$lat)
        
        c2_tmin <- min(cube2_tmp$time)
        c2_tmax <- max(cube2_tmp$time)
        
        output$cube3d <- renderRglwidget({
          options(rgl.printRglwidget = TRUE)
          rgl.bg(color = "white")
          
          c3d <- cube3d(color="mediumslateblue", alpha=0.25)
          c1x = c(c1_xmin, c1_xmax, c1_xmin, c1_xmax, c1_xmin, c1_xmax, c1_xmin, c1_xmax)
          c1y = c(c1_ymin, c1_ymin, c1_ymax, c1_ymax, c1_ymin, c1_ymin, c1_ymax, c1_ymax)
          c1z = c(c1_tmin, c1_tmin, c1_tmin, c1_tmin, c1_tmax, c1_tmax, c1_tmax, c1_tmax)
          
          c3d$vb[1,] <- c1x
          c3d$vb[2,] <- c1y
          c3d$vb[3,] <- c1z
          
          c3d2 <- cube3d(color="mediumspringgreen", alpha=0.5)
          c2x = c(c2_xmin, c2_xmax, c2_xmin, c2_xmax, c2_xmin, c2_xmax, c2_xmin, c2_xmax)
          c2y = c(c2_ymin, c2_ymin, c2_ymax, c2_ymax, c2_ymin, c2_ymin, c2_ymax, c2_ymax)
          c2z = c(c2_tmin, c2_tmin, c2_tmin, c2_tmin, c2_tmax, c2_tmax, c2_tmax, c2_tmax)
          
          c3d2$vb[1,] <- c2x
          c3d2$vb[2,] <- c2y
          c3d2$vb[3,] <- c2z
          
          fig <- plot3d(c3d, box = FALSE)
          fig <- shade3d(c3d2)
          
          rglwidget()
        })
        
        output$cube_plot <- renderPlot({
          cube_plot <- ggplot() +
            geom_segment(data = cube1_tmp, aes(lon, lat, xend = max(lon), yend = lat), color = 'blue', size=.25) +
            geom_segment(data = cube1_tmp, aes(lon, lat, xend = lon, yend = max(lat)), color = 'blue', size=.25) +
            geom_segment(data = cube2_tmp, aes(lon, lat, xend = max(lon), yend = lat), color = 'red', size=.25) +
            geom_segment(data = cube2_tmp, aes(lon, lat, xend = lon, yend = max(lat)), color = 'red', size=.25) +
            geom_point(data = cube1_tmp, aes(lon, lat), color = 'blue', size = .8, show.legend = FALSE) +
            geom_point(data = cube2_tmp, aes(lon, lat), color = 'red', size = .8, show.legend = FALSE) +
            annotate(geom = 'rect', xmin = min(cube1_tmp$lon), xmax = max(cube1_tmp$lon), ymin = min(cube1_tmp$lat), ymax = max(cube1_tmp$lat),fill = 'blue', colour = 'blue', alpha = 0.3) +
            annotate(geom = 'rect', xmin = min(cube2_tmp$lon), xmax = max(cube2_tmp$lon), ymin = min(cube2_tmp$lat), ymax = max(cube2_tmp$lat), fill = 'red', colour = 'red', alpha = 0.3) +
            coord_quickmap()
          
          cube_plot
        })
        
        c1_xres <- resolution(cube1_tmp$lon)
        c1_yres <- resolution(cube1_tmp$lat)
        
        c2_xres <- resolution(cube2_tmp$lon)
        c2_yres <- resolution(cube2_tmp$lat)
        
        min_xres <- min(c1_xres, c2_xres)
        min_yres <- min(c1_yres, c2_yres)
        
        max_xres <- max(c1_xres, c2_xres)
        max_yres <- max(c1_yres, c2_yres)
        
        union_xmin <- min(c1_xmin, c2_xmin)
        union_xmax <- max(c1_xmax, c2_xmax)
        union_ymin <- min(c1_ymin, c2_ymin)
        union_ymax <- max(c1_ymax, c2_ymax)
        
        intersect_xmin <- max(c1_xmin, c2_xmin)
        intersect_xmax <- min(c1_xmax, c2_xmax)
        intersect_ymin <- max(c1_ymin, c2_ymin)
        intersect_ymax <- min(c1_ymax, c2_ymax)
        
        c1_subset_len <- length(cube1_tmp[(cube1_tmp$lon < intersect_xmin) & (cube1_tmp$lon > intersect_xmax) & (cube1_tmp$lat < intersect_ymin) & (cube1_tmp$lat > intersect_ymax),1])
        c2_subset_len <- length(cube2_tmp[(cube2_tmp$lon < intersect_xmin) & (cube2_tmp$lon > intersect_xmax) & (cube2_tmp$lat < intersect_ymin) & (cube2_tmp$lat > intersect_ymax),1])
        
        c1_lost_data <- c1_subset_len/(length(cube1_tmp$lon))
        c2_lost_data <- c2_subset_len/(length(cube2_tmp$lon))
        
        if (c1_xmin == c2_xmin && c1_xmax == c2_xmax && c1_ymin == c2_ymin && c1_ymax == c2_ymax && c1_xres == c2_xres && c1_yres == c2_yres){
          combined_cube <- data.frame(dplyr::select(cube1, c(c1_xname, c1_yname, c1_tname, c1_vname)), dplyr::select(cube2, c(c2_vname)))
          output$cube <- renderDataTable(combined_cube)
          
          output$c1_krigehist <- renderPlot({hist(combined_cube[,4], col = 'blue', main = paste0('Histogram of Kriged Values of ', colnames(combined_cube)[4]))})
          
          output$c2_krigehist <- renderPlot({hist(combined_cube[,5], col = 'green', main = paste0('Histogram of Kriged Variance of ', colnames(combined_cube)[5]))})
          
          observeEvent(input$save_yes, {
            output$save_filepath <- renderUI({
              textInput('file_path', 'Enter the full path to the directory where you would like to store your file, i.e. /users/JohnDoe/Downloads/')
            })
            
            output$save_filename <- renderUI({
              textInput('file_name', 'Enter the name you would like to give your file, ending in .csv, i.e. data.csv')
            })
            
            output$submit_file <- renderUI({
              actionButton('file', 'Submit file path and file name!')
            })
            
            observeEvent(input$file, {
              filename <- paste0(input$file_path, input$file_name)
              
              write.csv(combined_cube, filename, row.names = FALSE)
            })
          }, once = TRUE)
          
          return(combined_cube)
        }
        
        else if ((c1_xmin != c2_xmin || c1_xmax != c2_xmax || c1_ymin != c2_ymin || c1_ymax != c2_ymax) && (c1_xres != c2_xres || c1_yres != c2_yres)){
          output$user_message <- renderUI({
            h4(paste0('The cubes have different spatial extents and different spatial resolutions.  You can select to perform kriging to the intersection of the cubes at the coarser resolution, to the intersection of the cubes at the finer resolution, to the union of the cubes
              with the coarser resolution, or to the union of the cubes with the finer resolution.  Note that performing kriging on the intersection will result in a loss of ', 100*c1_lost_data, '% of the data in Cube 1 and ', 100*c2_lost_data, '% of the data in Cube 2.  However,
              performing kriging to the intersection and the coarser resolution will result in more accurate predictions from kriging than performing kriging to either the finer resoution or the union of the cubes.'))
          })
          output$buttons <- renderUI({
            fluidRow(column(3, actionButton('int_coarse', 'Intersection and Coarser Resolution')), column(3, actionButton('int_fine', 'Intersection and Finer Resolution')), column(3, actionButton('union_coarse', 'Union and Coarser Resolution')),
                     column(3, actionButton('union_fine', 'Union and Finer Resolution')))
          })
        }
        
        else if (c1_xres != c2_xres || c1_yres != c2_yres){
          output$user_message <- renderUI({
            h4('The cubes have different spatial resolutions.  You can either perform kriging to the coarser resolution or to the finer resolution.  Note that kriging to the coarser resolution might result in the loss of data, but will produce more
                   accurate predictions from kriging.')
          })
          output$buttons <- renderUI({
            fluidRow(column(3, actionButton('coarse', 'Coarser Resolution')), column(3, 'fine', 'Finer Resolution'))
          })
        }
        
        else if (c1_xmin != c2_xmin || c1_xmax != c2_xmax || c1_ymin != c2_ymin || c1_ymax != c2_ymax){
          output$user_message <- renderUI({
            h4(paste0('The cubes have different spatial extents.  You can select to either perform kriging to the intersection of the cubes or to the union of the cubes.  Note that performing kriging to the intersection of the cubes will result in a loss of ',
                      100*c1_lost_data, '% of the data in Cube 1 and ', 100*c2_lost_data, '% of the data in Cube 2 but will result in more accurate predictions than kriging to the union of the cubes.'))
          })
          output$buttons <- renderUI({
            fluidRow(column(3, actionButton('int', 'Intersection')), column(3, actionButton('union', 'Union')))
          })
        }
        
        krige_lons <- c()
        krige_lats <- c()
        
        observeEvent(input$int_coarse, {
          krige_lons <- seq(intersect_xmin, intersect_xmax, by = max_xres)
          krige_lats <- seq(intersect_ymin, intersect_ymax, by = max_yres)
        }, once = TRUE)
        
        observeEvent(input$int_fine, {
          krige_lons <- seq(intersect_xmin, intersect_xmax, by = min_xres)
          krige_lats <- seq(intersect_ymin, intersect_ymax, by = min_yres)
        }, once = TRUE)
        
        observeEvent(input$union_coarse, {
          krige_lons <- seq(union_xmin, union_xmax, by = max_xres)
          krige_lats <- seq(union_ymin, union_ymax, by = max_yres)
        }, once = TRUE)
        
        observeEvent(input$union_fine, {
          krige_lons <- seq(union_xmin, union_xmax, by = min_xres)
          krige_lats <- seq(union_ymin, union_ymax, by = min_yres)
        }, once = TRUE)
        
        observeEvent(input$coarse, {
          krige_lons <- seq(c1_xmin, c1_xmax, by = min_xres)
          krige_lats <- seq(c1_ymin, c1_ymax, by = min_yres)
        }, once = TRUE)
        
        observeEvent(input$fine, {
          krige_lons <- seq(c1_xmin, c1_xmax, by = max_xres)
          krige_lats <- seq(c1_ymin, c1_ymax, by = max_yres)
        }, once = TRUE)
        
        observeEvent(input$int, {
          krige_lons <- seq(intersect_xmin, intersect_xmax, by = c1_xres)
          krige_lats <- seq(intersect_ymin, intersect_ymax, by = c1_yres)
        }, once = TRUE)
        
        observeEvent(input$union, {
          krige_lons <- seq(union_xmin, union_xmax, by = c1_xres)
          krige_lats <- seq(union_ymin, union_ymax, by = c1_yres)
        }, once = TRUE)
        
        n <- length(krige_lons)*length(krige_lats)*length(unique(cube1_tmp$time))
        
        m <- length(krige_lons)*length(krige_lats)
        
        krige_c1 <- matrix(NA, nrow = n, ncol = 2)
        krige_c2 <- matrix(NA, nrow = n, ncol = 2)
        
        c1_indx <- 1
        c2_indx <- 1
        
        c1_names <- c()
        c2_names <- c()
        
        for (t in 1:length(unique(cube1_tmp$time))){
          time <- unique(cube1_tmp$time)[1]
          c1 <- cube1_tmp[cube1_tmp$time == time,]
          c2 <- cube2_tmp[cube2_tmp$time == time,]
          
          tmp <- dplyr::select(c1, -c('time'))
          colnames(tmp) <- c('lon', 'lat', 'var')
          coordinates(tmp) <- c('lon', 'lat')
          proj4string(tmp) <- crs('+proj=longlat +datum=WGS84')
          
          c1_krige <- krige_cubes(tmp, krige_lons, krige_lats, c1_varname)
          
          if (t == 1){
            c1_names <- c(c1_names, colnames(c1_krige))
          }
          
          c1_krige <- as.matrix(c1_krige)
          
          for (j in 1:dim(c1_krige)[1]){
            krige_c1[c1_indx,] <- c1_krige[j,]
            c1_indx <- c1_indx + 1
          }
          
          colnames(c2) <- c('lon', 'lat', 'time', 'var')
          tmp <- dplyr::select(c2, -c('time'))
          coordinates(tmp) <- c('lon', 'lat')
          proj4string(tmp) <- crs('+proj=longlat +datum=WGS84')
          
          c2_krige <- krige_cubes(tmp, krige_lons, krige_lats, c2_varname)
          
          if (t == 1){
            c2_names <- c(c2_names, colnames(c2_krige))
          }
          
          c2_krige <- as.matrix(c2_krige)
          
          for (j in 1:dim(c2_krige)[1]){
            krige_c2[c2_indx,] <- c2_krige[j,]
            c2_indx <- c2_indx + 1
          }
          
        }
        
        combined_cube <- as.data.frame(cbind(expand.grid(krige_lons, krige_lats, unique(cube1_tmp$time)), krige_c1, krige_c2))
        
        colnames(combined_cube) <- c('lon', 'lat', 'time', c1_names, c2_names)
        
        output$c1_krigehist <- renderPlot({hist(combined_cube[,4], col = 'blue', main = paste0('Histogram of Kriged Values of ', colnames(combined_cube)[4]))})
        
        output$c1_varhist <- renderPlot({hist(combined_cube[,5], col = 'green', main = paste0('Histogram of Kriged Variance of ', colnames(combined_cube)[5]))})
        
        output$c2_krigehist <- renderPlot({hist(combined_cube[,6], col = 'red', main = paste0('Histogram of Kriged Values of ', colnames(combined_cube)[6]))})
        
        output$c2_varhist <- renderPlot({hist(combined_cube[,7], col = 'purple', main = paste0('Histogram of Kriged Variance of ', colnames(combined_cube)[7]))})
        
        output$cube <- renderDataTable(combined_cube)
        
        observeEvent(input$save_yes, {
          output$save_filepath <- renderUI({
            textInput('file_path', 'Enter the full path to the directory where you would like to store your file, i.e. /users/JohnDoe/Downloads/')
          })
          
          output$save_filename <- renderUI({
            textInput('file_name', 'Enter the name you would like to give your file, ending in .csv, i.e. data.csv')
          })
          
          output$submit_file <- renderUI({
            actionButton('file', 'Submit file path and file name!')
          })
          
          observeEvent(input$file, {
            filename <- paste0(input$file_path, input$file_name)
            
            write.csv(combined_cube, filename, row.names = FALSE)
          })
        }, once = TRUE)
      })
    }, once = TRUE)
    
    # observeEvent(input$continue, {
    #   withProgress(message = 'Combining Cubes', {
    #     cube1_sf <- st_as_sf(x = cube1, coords = c('lon', 'lat'), crs = '+proj=longlat +datum=WGS84')
    #     cube2_sf <- st_as_sf(x = cube2, coords = c('lon', 'lat'), crs = '+proj=longlat +datum=WGS84')
    #     
    #     cube1_tmp <- as.data.frame(tidyr::extract(cube1_sf, geometry, into = c('lon', 'lat'), '\\((.*),(.*)\\)'))
    #     cube2_tmp <- as.data.frame(tidyr::extract(cube2_sf, geometry, into = c('lon', 'lat'), '\\((.*),(.*)\\)'))
    #     
    #     cube1_tmp <- cube1_tmp %>% dplyr::select(!c('geometry'))
    #     cube1_tmp <- cube1_tmp %>% dplyr::select(!c('geometry'))
    #     
    #     c1_ind <- seq(1, length(colnames(cube1_tmp)))
    #     c1_xindx <- which(colnames(cube1_tmp) %in% xnames)
    #     c1_yindx <- which(colnames(cube1_tmp) %in% ynames)
    #     c1_tindx <- which(colnames(cube1_tmp) %in% tnames)
    #     c1_vindx <- c1_ind[-c(c1_xindx, c1_yindx, c1_tindx)]
    #     c1_varname <- colnames(cube1_tmp)[c1_vindx]
    #     
    #     cube2_tmp <- cube2_tmp %>% dplyr::select(!c('geometry'))
    #     cube2_tmp <- cube2_tmp %>% dplyr::select(!c('geometry'))
    #     
    #     c2_ind <- seq(1, length(colnames(cube2_tmp)))
    #     c2_xindx <- which(colnames(cube2_tmp) %in% xnames)
    #     c2_yindx <- which(colnames(cube2_tmp) %in% ynames)
    #     c2_tindx <- which(colnames(cube2_tmp) %in% tnames)
    #     c2_vindx <- c2_ind[-c(c2_xindx, c2_yindx, c2_tindx)]
    #     c2_varname <- colnames(cube2_tmp)[c2_vindx]
    #     
    #     cube1_tmp <- dplyr::select(cube1_tmp, all_of(c(c1_xindx, c1_yindx, c1_tindx, c1_vindx)))
    #     colnames(cube1_tmp) <- c('lon', 'lat', 'time', c1_varname)
    #     cube1_tmp$lon <- as.numeric(cube1_tmp$lon)
    #     cube1_tmp$lat <- as.numeric(cube1_tmp$lat)
    #     
    #     cube2_tmp <- dplyr::select(cube2_tmp, all_of(c(c2_xindx, c2_yindx, c2_tindx, c2_vindx)))
    #     colnames(cube2_tmp) <- c('lon', 'lat', 'time', c2_varname)
    #     cube2_tmp$lon <- as.numeric(cube2_tmp$lon)
    #     cube2_tmp$lat <- as.numeric(cube2_tmp$lat)
    #     
    #     cube1_tmp <- na.omit(cube1_tmp)
    #     cube2_tmp <- na.omit(cube2_tmp)
    #     
    #     c1_xmin <- min(cube1_tmp$lon)
    #     c1_xmax <- max(cube1_tmp$lon)
    #     
    #     c1_ymin <- min(cube1_tmp$lat)
    #     c1_ymax <- max(cube1_tmp$lat)
    #     
    #     c1_tmin <- min(cube1_tmp$time)
    #     c1_tmax <- max(cube1_tmp$time)
    #     
    #     c2_xmin <- min(cube2_tmp$lon)
    #     c2_xmax <- max(cube2_tmp$lon)
    #     
    #     c2_ymin <- min(cube2_tmp$lat)
    #     c2_ymax <- max(cube2_tmp$lat)
    #     
    #     c2_tmin <- min(cube2_tmp$time)
    #     c2_tmax <- max(cube2_tmp$time)
    #     
    #     output$cube3d <- renderRglwidget({
    #       options(rgl.printRglwidget = TRUE)
    #       rgl.bg(color = "white")
    #       
    #       c3d <- cube3d(color="mediumslateblue", alpha=0.25)
    #       c1x = c(c1_xmin, c1_xmax, c1_xmin, c1_xmax, c1_xmin, c1_xmax, c1_xmin, c1_xmax)
    #       c1y = c(c1_ymin, c1_ymin, c1_ymax, c1_ymax, c1_ymin, c1_ymin, c1_ymax, c1_ymax)
    #       c1z = c(c1_tmin, c1_tmin, c1_tmin, c1_tmin, c1_tmax, c1_tmax, c1_tmax, c1_tmax)
    #       
    #       c3d$vb[1,] <- c1x
    #       c3d$vb[2,] <- c1y
    #       c3d$vb[3,] <- c1z
    #       
    #       c3d2 <- cube3d(color="mediumspringgreen", alpha=0.5)
    #       c2x = c(c2_xmin, c2_xmax, c2_xmin, c2_xmax, c2_xmin, c2_xmax, c2_xmin, c2_xmax)
    #       c2y = c(c2_ymin, c2_ymin, c2_ymax, c2_ymax, c2_ymin, c2_ymin, c2_ymax, c2_ymax)
    #       c2z = c(c2_tmin, c2_tmin, c2_tmin, c2_tmin, c2_tmax, c2_tmax, c2_tmax, c2_tmax)
    #       
    #       c3d2$vb[1,] <- c2x
    #       c3d2$vb[2,] <- c2y
    #       c3d2$vb[3,] <- c2z
    #       
    #       fig <- plot3d(c3d, box = FALSE)
    #       fig <- shade3d(c3d2)
    #       
    #       rglwidget()
    #     })
    #     
    #     output$cube_plot <- renderPlot({
    #       cube_plot <- ggplot() +
    #         geom_segment(data = cube1_tmp, aes(lon, lat, xend = max(lon), yend = lat), color = 'blue', size=.25) +
    #         geom_segment(data = cube1_tmp, aes(lon, lat, xend = lon, yend = max(lat)), color = 'blue', size=.25) +
    #         geom_segment(data = cube2_tmp, aes(lon, lat, xend = max(lon), yend = lat), color = 'red', size=.25) +
    #         geom_segment(data = cube2_tmp, aes(lon, lat, xend = lon, yend = max(lat)), color = 'red', size=.25) +
    #         geom_point(data = cube1_tmp, aes(lon, lat), color = 'blue', size = .8, show.legend = FALSE) +
    #         geom_point(data = cube2_tmp, aes(lon, lat), color = 'red', size = .8, show.legend = FALSE) +
    #         annotate(geom = 'rect', xmin = min(cube1_tmp$lon), xmax = max(cube1_tmp$lon), ymin = min(cube1_tmp$lat), ymax = max(cube1_tmp$lat),fill = 'blue', colour = 'blue', alpha = 0.3) +
    #         annotate(geom = 'rect', xmin = min(cube2_tmp$lon), xmax = max(cube2_tmp$lon), ymin = min(cube2_tmp$lat), ymax = max(cube2_tmp$lat), fill = 'red', colour = 'red', alpha = 0.3) +
    #         coord_quickmap()
    #       
    #       cube_plot
    #     })
    #     
    #     c1_xres <- resolution(cube1_tmp$lon)
    #     c1_yres <- resolution(cube1_tmp$lat)
    #     
    #     c2_xres <- resolution(cube2_tmp$lon)
    #     c2_yres <- resolution(cube2_tmp$lat)
    #     
    #     min_xres <- min(c1_xres, c2_xres)
    #     min_yres <- min(c1_yres, c2_yres)
    #     
    #     max_xres <- max(c1_xres, c2_xres)
    #     max_yres <- max(c1_yres, c2_yres)
    #     
    #     union_xmin <- min(c1_xmin, c2_xmin)
    #     union_xmax <- max(c1_xmax, c2_xmax)
    #     union_ymin <- min(c1_ymin, c2_ymin)
    #     union_ymax <- max(c1_ymax, c2_ymax)
    #     
    #     intersect_xmin <- max(c1_xmin, c2_xmin)
    #     intersect_xmax <- min(c1_xmax, c2_xmax)
    #     intersect_ymin <- max(c1_ymin, c2_ymin)
    #     intersect_ymax <- min(c1_ymax, c2_ymax)
    #     
    #     c1_subset_len <- length(cube1_tmp[(cube1_tmp$lon < intersect_xmin) & (cube1_tmp$lon > intersect_xmax) & (cube1_tmp$lat < intersect_ymin) & (cube1_tmp$lat > intersect_ymax),1])
    #     c2_subset_len <- length(cube2_tmp[(cube2_tmp$lon < intersect_xmin) & (cube2_tmp$lon > intersect_xmax) & (cube2_tmp$lat < intersect_ymin) & (cube2_tmp$lat > intersect_ymax),1])
    #     
    #     c1_lost_data <- c1_subset_len/(length(cube1_tmp$lon))
    #     c2_lost_data <- c2_subset_len/(length(cube2_tmp$lon))
    #     
    #     if (c1_xmin == c2_xmin && c1_xmax == c2_xmax && c1_ymin == c2_ymin && c1_ymax == c2_ymax && c1_xres == c2_xres && c1_yres == c2_yres){
    #       combined_cube <- data.frame(dplyr::select(cube1, c(c1_xname, c1_yname, c1_vname)), dplyr::select(cube2, c(c2_vname)))
    #       output$cube <- renderDataTable(combined_cube)
    #       
    #       observeEvent(input$save_yes, {
    #         output$save_filepath <- renderUI({
    #           textInput('file_path', 'Enter the full path to the directory where you would like to store your file, i.e. /users/JohnDoe/Downloads/')
    #         })
    #         
    #         output$save_filename <- renderUI({
    #           textInput('file_name', 'Enter the name you would like to give your file, ending in .csv, i.e. data.csv')
    #         })
    #         
    #         output$submit_file <- renderUI({
    #           actionButton('file', 'Submit file path and file name!')
    #         })
    #         
    #         observeEvent(input$file, {
    #           filename <- paste0(input$file_path, input$file_name)
    #           
    #           write.csv(combined_cube, filename, row.names = FALSE)
    #         })
    #       }, once = TRUE)
    #       
    #       return(combined_cube)
    #     }
    #     
    #     else if ((c1_xmin != c2_xmin || c1_xmax != c2_xmax || c1_ymin != c2_ymin || c1_ymax != c2_ymax) && (c1_xres != c2_xres || c1_yres != c2_yres)){
    #       output$user_message <- renderUI({
    #         h4(paste0('The cubes have different spatial extents and different spatial resolutions.  You can select to perform kriging to the intersection of the cubes at the coarser resolution, to the intersection of the cubes at the finer resolution, to the union of the cubes
    #           with the coarser resolution, or to the union of the cubes with the finer resolution.  Note that performing kriging on the intersection will result in a loss of ', 100*c1_lost_data, '% of the data in Cube 1 and ', 100*c2_lost_data, '% of the data in Cube 2.  However,
    #           performing kriging to the intersection and the coarser resolution will result in more accurate predictions from kriging than performing kriging to either the finer resoution or the union of the cubes.'))
    #       })
    #       output$buttons <- renderUI({
    #         fluidRow(column(3, actionButton('int_coarse', 'Intersection and Coarser Resolution')), column(3, actionButton('int_fine', 'Intersection and Finer Resolution')), column(3, actionButton('union_coarse', 'Union and Coarser Resolution')),
    #                  column(3, actionButton('union_fine', 'Union and Finer Resolution')))
    #       })
    #     }
    #     
    #     else if (c1_xres != c2_xres || c1_yres != c2_yres){
    #       output$user_message <- renderUI({
    #         h4('The cubes have different spatial resolutions.  You can either perform kriging to the coarser resolution or to the finer resolution.  Note that kriging to the coarser resolution might result in the loss of data, but will produce more
    #                accurate predictions from kriging.')
    #       })
    #       output$buttons <- renderUI({
    #         fluidRow(column(3, actionButton('coarse', 'Coarser Resolution')), column(3, 'fine', 'Finer Resolution'))
    #       })
    #     }
    #     
    #     else if (c1_xmin != c2_xmin || c1_xmax != c2_xmax || c1_ymin != c2_ymin || c1_ymax != c2_ymax){
    #       output$user_message <- renderUI({
    #         h4(paste0('The cubes have different spatial extents.  You can select to either perform kriging to the intersection of the cubes or to the union of the cubes.  Note that performing kriging to the intersection of the cubes will result in a loss of ',
    #                   100*c1_lost_data, '% of the data in Cube 1 and ', 100*c2_lost_data, '% of the data in Cube 2 but will result in more accurate predictions than kriging to the union of the cubes.'))
    #       })
    #       output$buttons <- renderUI({
    #         fluidRow(column(3, actionButton('int', 'Intersection')), column(3, actionButton('union', 'Union')))
    #       })
    #     }
    #     
    #     krige_lons <- c()
    #     krige_lats <- c()
    #     
    #     observeEvent(input$int_coarse, {
    #       krige_lons <- seq(intersect_xmin, intersect_xmax, by = max_xres)
    #       krige_lats <- seq(intersect_ymin, intersect_ymax, by = max_yres)
    #     }, once = TRUE)
    #     
    #     observeEvent(input$int_fine, {
    #       krige_lons <- seq(intersect_xmin, intersect_xmax, by = min_xres)
    #       krige_lats <- seq(intersect_ymin, intersect_ymax, by = min_yres)
    #     }, once = TRUE)
    #     
    #     observeEvent(input$union_coarse, {
    #       krige_lons <- seq(union_xmin, union_xmax, by = max_xres)
    #       krige_lats <- seq(union_ymin, union_ymax, by = max_yres)
    #     }, once = TRUE)
    #     
    #     observeEvent(input$union_fine, {
    #       krige_lons <- seq(union_xmin, union_xmax, by = min_xres)
    #       krige_lats <- seq(union_ymin, union_ymax, by = min_yres)
    #     }, once = TRUE)
    #     
    #     observeEvent(input$coarse, {
    #       krige_lons <- seq(c1_xmin, c1_xmax, by = min_xres)
    #       krige_lats <- seq(c1_ymin, c1_ymax, by = min_yres)
    #     }, once = TRUE)
    #     
    #     observeEvent(input$fine, {
    #       krige_lons <- seq(c1_xmin, c1_xmax, by = max_xres)
    #       krige_lats <- seq(c1_ymin, c1_ymax, by = max_yres)
    #     }, once = TRUE)
    #     
    #     observeEvent(input$int, {
    #       krige_lons <- seq(intersect_xmin, intersect_xmax, by = c1_xres)
    #       krige_lats <- seq(intersect_ymin, intersect_ymax, by = c1_yres)
    #     }, once = TRUE)
    #     
    #     observeEvent(input$union, {
    #       krige_lons <- seq(union_xmin, union_xmax, by = c1_xres)
    #       krige_lats <- seq(union_ymin, union_ymax, by = c1_yres)
    #     }, once = TRUE)
    #     
    #     n <- length(krige_lons)*length(krige_lats)*length(unique(cube1_tmp$time))
    #     
    #     m <- length(krige_lons)*length(krige_lats)
    #     
    #     krige_c1 <- matrix(NA, nrow = n, ncol = 2)
    #     krige_c2 <- matrix(NA, nrow = n, ncol = 2)
    #     
    #     c1_indx <- 1
    #     c2_indx <- 1
    #     
    #     c1_names <- c()
    #     c2_names <- c()
    #     
    #     for (t in 1:length(unique(cube1_tmp$time))){
    #       time <- unique(cube1_tmp$time)[1]
    #       c1 <- cube1_tmp[cube1_tmp$time == time,]
    #       c2 <- cube2_tmp[cube2_tmp$time == time,]
    #       
    #       tmp <- dplyr::select(c1, -c('time'))
    #       colnames(tmp) <- c('lon', 'lat', 'var')
    #       coordinates(tmp) <- c('lon', 'lat')
    #       proj4string(tmp) <- crs('+proj=longlat +datum=WGS84')
    #       
    #       c1_krige <- krige_cubes(tmp, krige_lons, krige_lats, c1_varname)
    #       
    #       if (t == 1){
    #         c1_names <- c(c1_names, colnames(c1_krige))
    #       }
    #       
    #       c1_krige <- as.matrix(c1_krige)
    #       
    #       for (j in 1:dim(c1_krige)[1]){
    #         krige_c1[c1_indx,] <- c1_krige[j,]
    #         c1_indx <- c1_indx + 1
    #       }
    #       
    #       colnames(c2) <- c('lon', 'lat', 'time', 'var')
    #       tmp <- dplyr::select(c2, -c('time'))
    #       coordinates(tmp) <- c('lon', 'lat')
    #       proj4string(tmp) <- crs('+proj=longlat +datum=WGS84')
    #       
    #       c2_krige <- krige_cubes(tmp, krige_lons, krige_lats, c2_varname)
    #       
    #       if (t == 1){
    #         c2_names <- c(c2_names, colnames(c2_krige))
    #       }
    #       
    #       c2_krige <- as.matrix(c2_krige)
    #       
    #       for (j in 1:dim(c2_krige)[1]){
    #         krige_c2[c2_indx,] <- c2_krige[j,]
    #         c2_indx <- c2_indx + 1
    #       }
    #       
    #     }
    #     
    #     combined_cube <- as.data.frame(cbind(expand.grid(krige_lons, krige_lats, unique(cube1_tmp$time)), krige_c1, krige_c2))
    #     
    #     colnames(combined_cube) <- c('lon', 'lat', 'time', c1_names, c2_names)
    #     
    #     output$c1_krigehist <- renderPlot({hist(combined[,4], col = 'blue', main = paste0('Histogram of Kriged Values of ', colnames(combined)[4]))})
    #     
    #     output$c1_varhist <- renderPlot({hist(combined[,5], col = 'green', main = paste0('Histogram of Kriged Variance of ', colnames(combined)[5]))})
    #     
    #     output$c2_krigehist <- renderPlot({hist(combined[,6], col = 'red', main = paste0('Histogram of Kriged Values of ', colnames(combined)[6]))})
    #     
    #     output$c2_varhist <- renderPlot({hist(combined[,7], col = 'purple', main = paste0('Histogram of Kriged Variance of ', colnames(combined)[7]))})
    #     
    #     output$cube <- renderDataTable(combined_cube)
    #     
    #     observeEvent(input$save_yes, {
    #       output$save_filepath <- renderUI({
    #         textInput('file_path', 'Enter the full path to the directory where you would like to store your file, i.e. /users/JohnDoe/Downloads/')
    #       })
    #       
    #       output$save_filename <- renderUI({
    #         textInput('file_name', 'Enter the name you would like to give your file, ending in .csv, i.e. data.csv')
    #       })
    #       
    #       output$submit_file <- renderUI({
    #         actionButton('file', 'Submit file path and file name!')
    #       })
    #       
    #       observeEvent(input$file, {
    #         filename <- paste0(input$file_path, input$file_name)
    #         
    #         write.csv(combined_cube, filename, row.names = FALSE)
    #       })
    #     }, once = TRUE)
    #   })
    # }, once = TRUE)

    # cube1_sf <- st_as_sf(x = cube1, coords = c('lon', 'lat'), crs = '+proj=longlat +datum=WGS84')
    # cube2_sf <- st_as_sf(x = cube2, coords = c('lon', 'lat'), crs = '+proj=longlat +datum=WGS84')
    # 
    # cube1_tmp <- as.data.frame(tidyr::extract(cube1_sf, geometry, into = c('lon', 'lat'), '\\((.*),(.*)\\)'))
    # cube2_tmp <- as.data.frame(tidyr::extract(cube2_sf, geometry, into = c('lon', 'lat'), '\\((.*),(.*)\\)'))
  #      
  #   cube1_tmp <- cube1_tmp %>% dplyr::select(!c('geometry'))
  #   cube1_tmp <- cube1_tmp %>% dplyr::select(!c('geometry'))
  # 
  #   c1_ind <- seq(1, length(colnames(cube1_tmp)))
  #   c1_xindx <- which(colnames(cube1_tmp) %in% xnames)
  #   c1_yindx <- which(colnames(cube1_tmp) %in% ynames)
  #   c1_tindx <- which(colnames(cube1_tmp) %in% tnames)
  #   c1_vindx <- c1_ind[-c(c1_xindx, c1_yindx, c1_tindx)]
  #   c1_varname <- colnames(cube1_tmp)[c1_vindx]
  # 
  #   cube2_tmp <- cube2_tmp %>% dplyr::select(!c('geometry'))
  #   cube2_tmp <- cube2_tmp %>% dplyr::select(!c('geometry'))
  # 
  #   c2_ind <- seq(1, length(colnames(cube2_tmp)))
  #   c2_xindx <- which(colnames(cube2_tmp) %in% xnames)
  #   c2_yindx <- which(colnames(cube2_tmp) %in% ynames)
  #   c2_tindx <- which(colnames(cube2_tmp) %in% tnames)
  #   c2_vindx <- c2_ind[-c(c2_xindx, c2_yindx, c2_tindx)]
  #   c2_varname <- colnames(cube2_tmp)[c2_vindx]
  # 
  #   cube1_tmp <- dplyr::select(cube1_tmp, all_of(c(c1_xindx, c1_yindx, c1_tindx, c1_vindx)))
  #   colnames(cube1_tmp) <- c('lon', 'lat', 'time', c1_varname)
  #   cube1_tmp$lon <- as.numeric(cube1_tmp$lon)
  #   cube1_tmp$lat <- as.numeric(cube1_tmp$lat)
  # 
  #   cube2_tmp <- dplyr::select(cube2_tmp, all_of(c(c2_xindx, c2_yindx, c2_tindx, c2_vindx)))
  #   colnames(cube2_tmp) <- c('lon', 'lat', 'time', c2_varname)
  #   cube2_tmp$lon <- as.numeric(cube2_tmp$lon)
  #   cube2_tmp$lat <- as.numeric(cube2_tmp$lat)
  # 
  #   cube1_tmp <- na.omit(cube1_tmp)
  #   cube2_tmp <- na.omit(cube2_tmp)
  # 
  #   c1_xmin <- min(cube1_tmp$lon)
  #   c1_xmax <- max(cube1_tmp$lon)
  # 
  #   c1_ymin <- min(cube1_tmp$lat)
  #   c1_ymax <- max(cube1_tmp$lat)
  # 
  #   c1_tmin <- min(cube1_tmp$time)
  #   c1_tmax <- max(cube1_tmp$time)
  # 
  #   c2_xmin <- min(cube2_tmp$lon)
  #   c2_xmax <- max(cube2_tmp$lon)
  # 
  #   c2_ymin <- min(cube2_tmp$lat)
  #   c2_ymax <- max(cube2_tmp$lat)
  # 
  #   c2_tmin <- min(cube2_tmp$time)
  #   c2_tmax <- max(cube2_tmp$time)
  # 
  #   output$cube3d <- renderRglwidget({
  #     options(rgl.printRglwidget = TRUE)
  #     rgl.bg(color = "white")
  # 
  #     c3d <- cube3d(color="mediumslateblue", alpha=0.25)
  #     c1x = c(c1_xmin, c1_xmax, c1_xmin, c1_xmax, c1_xmin, c1_xmax, c1_xmin, c1_xmax)
  #     c1y = c(c1_ymin, c1_ymin, c1_ymax, c1_ymax, c1_ymin, c1_ymin, c1_ymax, c1_ymax)
  #     c1z = c(c1_tmin, c1_tmin, c1_tmin, c1_tmin, c1_tmax, c1_tmax, c1_tmax, c1_tmax)
  # 
  #     c3d$vb[1,] <- c1x
  #     c3d$vb[2,] <- c1y
  #     c3d$vb[3,] <- c1z
  # 
  #     c3d2 <- cube3d(color="mediumspringgreen", alpha=0.5)
  #     c2x = c(c2_xmin, c2_xmax, c2_xmin, c2_xmax, c2_xmin, c2_xmax, c2_xmin, c2_xmax)
  #     c2y = c(c2_ymin, c2_ymin, c2_ymax, c2_ymax, c2_ymin, c2_ymin, c2_ymax, c2_ymax)
  #     c2z = c(c2_tmin, c2_tmin, c2_tmin, c2_tmin, c2_tmax, c2_tmax, c2_tmax, c2_tmax)
  # 
  #     c3d2$vb[1,] <- c2x
  #     c3d2$vb[2,] <- c2y
  #     c3d2$vb[3,] <- c2z
  # 
  #     fig <- plot3d(c3d, box = FALSE)
  #     fig <- shade3d(c3d2)
  # 
  #     rglwidget()
  #   })
  # 
  #   output$cube_plot <- renderPlot({
  #     cube_plot <- ggplot() +
  #       geom_segment(data = cube1_tmp, aes(lon, lat, xend = max(lon), yend = lat), color = 'blue', size=.25) +
  #       geom_segment(data = cube1_tmp, aes(lon, lat, xend = lon, yend = max(lat)), color = 'blue', size=.25) +
  #       geom_segment(data = cube2_tmp, aes(lon, lat, xend = max(lon), yend = lat), color = 'red', size=.25) +
  #       geom_segment(data = cube2_tmp, aes(lon, lat, xend = lon, yend = max(lat)), color = 'red', size=.25) +
  #       geom_point(data = cube1_tmp, aes(lon, lat), color = 'blue', size = .8, show.legend = FALSE) +
  #       geom_point(data = cube2_tmp, aes(lon, lat), color = 'red', size = .8, show.legend = FALSE) +
  #       annotate(geom = 'rect', xmin = min(cube1_tmp$lon), xmax = max(cube1_tmp$lon), ymin = min(cube1_tmp$lat), ymax = max(cube1_tmp$lat),fill = 'blue', colour = 'blue', alpha = 0.3) +
  #       annotate(geom = 'rect', xmin = min(cube2_tmp$lon), xmax = max(cube2_tmp$lon), ymin = min(cube2_tmp$lat), ymax = max(cube2_tmp$lat), fill = 'red', colour = 'red', alpha = 0.3) +
  #       coord_quickmap()
  # 
  #     cube_plot
  #   })
  # 
  #   c1_xres <- resolution(cube1_tmp$lon)
  #   c1_yres <- resolution(cube1_tmp$lat)
  # 
  #   c2_xres <- resolution(cube2_tmp$lon)
  #   c2_yres <- resolution(cube2_tmp$lat)
  # 
  #   min_xres <- min(c1_xres, c2_xres)
  #   min_yres <- min(c1_yres, c2_yres)
  # 
  #   max_xres <- max(c1_xres, c2_xres)
  #   max_yres <- max(c1_yres, c2_yres)
  # 
  #   union_xmin <- min(c1_xmin, c2_xmin)
  #   union_xmax <- max(c1_xmax, c2_xmax)
  #   union_ymin <- min(c1_ymin, c2_ymin)
  #   union_ymax <- max(c1_ymax, c2_ymax)
  # 
  #   intersect_xmin <- max(c1_xmin, c2_xmin)
  #   intersect_xmax <- min(c1_xmax, c2_xmax)
  #   intersect_ymin <- max(c1_ymin, c2_ymin)
  #   intersect_ymax <- min(c1_ymax, c2_ymax)
  # 
  #   c1_subset_len <- length(cube1_tmp[(cube1_tmp$lon < intersect_xmin) & (cube1_tmp$lon > intersect_xmax) & (cube1_tmp$lat < intersect_ymin) & (cube1_tmp$lat > intersect_ymax),1])
  #   c2_subset_len <- length(cube2_tmp[(cube2_tmp$lon < intersect_xmin) & (cube2_tmp$lon > intersect_xmax) & (cube2_tmp$lat < intersect_ymin) & (cube2_tmp$lat > intersect_ymax),1])
  # 
  #   c1_lost_data <- c1_subset_len/(length(cube1_tmp$lon))
  #   c2_lost_data <- c2_subset_len/(length(cube2_tmp$lon))
  # 
  #   if (c1_xmin == c2_xmin && c1_xmax == c2_xmax && c1_ymin == c2_ymin && c1_ymax == c2_ymax && c1_xres == c2_xres && c1_yres == c2_yres){
  #     combined_cube <- data.frame(dplyr::select(cube1, c(c1_xname, c1_yname, c1_vname)), dplyr::select(cube2, c(c2_vname)))
  #     output$cube <- renderDataTable(combined_cube)
  #     
  #     observeEvent(input$save_yes, {
  #       output$save_filepath <- renderUI({
  #         textInput('file_path', 'Enter the full path to the directory where you would like to store your file, i.e. /users/JohnDoe/Downloads/')
  #       })
  #       
  #       output$save_filename <- renderUI({
  #         textInput('file_name', 'Enter the name you would like to give your file, ending in .csv, i.e. data.csv')
  #       })
  #       
  #       output$submit_file <- renderUI({
  #         actionButton('file', 'Submit file path and file name!')
  #       })
  #       
  #       observeEvent(input$file, {
  #         filename <- paste0(input$file_path, input$file_name)
  #         
  #         write.csv(combined_cube, filename, row.names = FALSE)
  #       })
  #     }, once = TRUE)
  # 
  #     return(combined_cube)
  #   }
  # 
  #   else if ((c1_xmin != c2_xmin || c1_xmax != c2_xmax || c1_ymin != c2_ymin || c1_ymax != c2_ymax) && (c1_xres != c2_xres || c1_yres != c2_yres)){
  #     output$user_message <- renderUI({
  #       h4(paste0('The cubes have different spatial extents and different spatial resolutions.  You can select to perform kriging to the intersection of the cubes at the coarser resolution, to the intersection of the cubes at the finer resolution, to the union of the cubes
  #         with the coarser resolution, or to the union of the cubes with the finer resolution.  Note that performing kriging on the intersection will result in a loss of ', 100*c1_lost_data, '% of the data in Cube 1 and ', 100*c2_lost_data, '% of the data in Cube 2.  However,
  #         performing kriging to the intersection and the coarser resolution will result in more accurate predictions from kriging than performing kriging to either the finer resoution or the union of the cubes.'))
  #     })
  #     output$buttons <- renderUI({
  #       fluidRow(column(3, actionButton('int_coarse', 'Intersection and Coarser Resolution')), column(3, actionButton('int_fine', 'Intersection and Finer Resolution')), column(3, actionButton('union_coarse', 'Union and Coarser Resolution')),
  #                column(3, actionButton('union_fine', 'Union and Finer Resolution')))
  #     })
  #   }
  # 
  #   else if (c1_xres != c2_xres || c1_yres != c2_yres){
  #     output$user_message <- renderUI({
  #       h4('The cubes have different spatial resolutions.  You can either perform kriging to the coarser resolution or to the finer resolution.  Note that kriging to the coarser resolution might result in the loss of data, but will produce more
  #              accurate predictions from kriging.')
  #     })
  #     output$buttons <- renderUI({
  #       fluidRow(column(3, actionButton('coarse', 'Coarser Resolution')), column(3, 'fine', 'Finer Resolution'))
  #     })
  #   }
  # 
  #   else if (c1_xmin != c2_xmin || c1_xmax != c2_xmax || c1_ymin != c2_ymin || c1_ymax != c2_ymax){
  #     output$user_message <- renderUI({
  #       h4(paste0('The cubes have different spatial extents.  You can select to either perform kriging to the intersection of the cubes or to the union of the cubes.  Note that performing kriging to the intersection of the cubes will result in a loss of ',
  #                 100*c1_lost_data, '% of the data in Cube 1 and ', 100*c2_lost_data, '% of the data in Cube 2 but will result in more accurate predictions than kriging to the union of the cubes.'))
  #     })
  #     output$buttons <- renderUI({
  #       fluidRow(column(3, actionButton('int', 'Intersection')), column(3, actionButton('union', 'Union')))
  #     })
  #   }
  # 
  #   krige_lons <- c()
  #   krige_lats <- c()
  # 
  #   observeEvent(input$int_coarse, {
  #     krige_lons <- seq(intersect_xmin, intersect_xmax, by = max_xres)
  #     krige_lats <- seq(intersect_ymin, intersect_ymax, by = max_yres)
  #   }, once = TRUE)
  # 
  #   observeEvent(input$int_fine, {
  #     krige_lons <- seq(intersect_xmin, intersect_xmax, by = min_xres)
  #     krige_lats <- seq(intersect_ymin, intersect_ymax, by = min_yres)
  #   }, once = TRUE)
  # 
  #   observeEvent(input$union_coarse, {
  #     krige_lons <- seq(union_xmin, union_xmax, by = max_xres)
  #     krige_lats <- seq(union_ymin, union_ymax, by = max_yres)
  #   }, once = TRUE)
  # 
  #   observeEvent(input$union_fine, {
  #     krige_lons <- seq(union_xmin, union_xmax, by = min_xres)
  #     krige_lats <- seq(union_ymin, union_ymax, by = min_yres)
  #   }, once = TRUE)
  # 
  #   observeEvent(input$coarse, {
  #     krige_lons <- seq(c1_xmin, c1_xmax, by = min_xres)
  #     krige_lats <- seq(c1_ymin, c1_ymax, by = min_yres)
  #   }, once = TRUE)
  # 
  #   observeEvent(input$fine, {
  #     krige_lons <- seq(c1_xmin, c1_xmax, by = max_xres)
  #     krige_lats <- seq(c1_ymin, c1_ymax, by = max_yres)
  #   }, once = TRUE)
  # 
  #   observeEvent(input$int, {
  #     krige_lons <- seq(intersect_xmin, intersect_xmax, by = c1_xres)
  #     krige_lats <- seq(intersect_ymin, intersect_ymax, by = c1_yres)
  #   }, once = TRUE)
  # 
  #   observeEvent(input$union, {
  #     krige_lons <- seq(union_xmin, union_xmax, by = c1_xres)
  #     krige_lats <- seq(union_ymin, union_ymax, by = c1_yres)
  #   }, once = TRUE)
  # 
  #   n <- length(krige_lons)*length(krige_lats)*length(unique(cube1_tmp$time))
  # 
  #   m <- length(krige_lons)*length(krige_lats)
  # 
  #   krige_c1 <- matrix(NA, nrow = n, ncol = 2)
  #   krige_c2 <- matrix(NA, nrow = n, ncol = 2)
  # 
  #   c1_indx <- 1
  #   c2_indx <- 1
  # 
  #   c1_names <- c()
  #   c2_names <- c()
  # 
  #   for (t in 1:length(unique(cube1_tmp$time))){
  #     time <- unique(cube1_tmp$time)[1]
  #     c1 <- cube1_tmp[cube1_tmp$time == time,]
  #     c2 <- cube2_tmp[cube2_tmp$time == time,]
  # 
  #     tmp <- dplyr::select(c1, -c('time'))
  #     colnames(tmp) <- c('lon', 'lat', 'var')
  #     coordinates(tmp) <- c('lon', 'lat')
  #     proj4string(tmp) <- crs('+proj=longlat +datum=WGS84')
  # 
  #     c1_krige <- krige_cubes(tmp, krige_lons, krige_lats, c1_varname)
  # 
  #     if (t == 1){
  #       c1_names <- c(c1_names, colnames(c1_krige))
  #     }
  # 
  #     c1_krige <- as.matrix(c1_krige)
  # 
  #     for (j in 1:dim(c1_krige)[1]){
  #       krige_c1[c1_indx,] <- c1_krige[j,]
  #       c1_indx <- c1_indx + 1
  #     }
  # 
  #     colnames(c2) <- c('lon', 'lat', 'time', 'var')
  #     tmp <- dplyr::select(c2, -c('time'))
  #     coordinates(tmp) <- c('lon', 'lat')
  #     proj4string(tmp) <- crs('+proj=longlat +datum=WGS84')
  # 
  #     c2_krige <- krige_cubes(tmp, krige_lons, krige_lats, c2_varname)
  # 
  #     if (t == 1){
  #       c2_names <- c(c2_names, colnames(c2_krige))
  #     }
  # 
  #     c2_krige <- as.matrix(c2_krige)
  # 
  #     for (j in 1:dim(c2_krige)[1]){
  #       krige_c2[c2_indx,] <- c2_krige[j,]
  #       c2_indx <- c2_indx + 1
  #     }
  # 
  #   }
  # 
  #   combined_cube <- as.data.frame(cbind(expand.grid(krige_lons, krige_lats, unique(cube1_tmp$time)), krige_c1, krige_c2))
  # 
  #   colnames(combined_cube) <- c('lon', 'lat', 'time', c1_names, c2_names)
  # 
  #   output$c1_krigehist <- renderPlot({hist(combined[,4], col = 'blue', main = paste0('Histogram of Kriged Values of ', colnames(combined)[4]))})
  # 
  #   output$c1_varhist <- renderPlot({hist(combined[,5], col = 'green', main = paste0('Histogram of Kriged Variance of ', colnames(combined)[5]))})
  # 
  #   output$c2_krigehist <- renderPlot({hist(combined[,6], col = 'red', main = paste0('Histogram of Kriged Values of ', colnames(combined)[6]))})
  # 
  #   output$c2_varhist <- renderPlot({hist(combined[,7], col = 'purple', main = paste0('Histogram of Kriged Variance of ', colnames(combined)[7]))})
  # 
  #   output$cube <- renderDataTable(combined_cube)
  # 
  #   observeEvent(input$save_yes, {
  #     output$save_filepath <- renderUI({
  #     textInput('file_path', 'Enter the full path to the directory where you would like to store your file, i.e. /users/JohnDoe/Downloads/')
  #   })
  # 
  #     output$save_filename <- renderUI({
  #       textInput('file_name', 'Enter the name you would like to give your file, ending in .csv, i.e. data.csv')
  #     })
  # 
  #     output$submit_file <- renderUI({
  #       actionButton('file', 'Submit file path and file name!')
  #     })
  # 
  #     observeEvent(input$file, {
  #       filename <- paste0(input$file_path, input$file_name)
  # 
  #       write.csv(combined_cube, filename, row.names = FALSE)
  #     })
  #   }, once = TRUE)
  }, once = TRUE)
}

shinyApp(ui = ui, server = server)