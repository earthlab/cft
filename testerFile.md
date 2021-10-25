# Welcome to the Climate Futures Toolbox

The purpose of this package is to make it easy to download and process
climate data from the … database.

``` r
library(tidyverse)
library(tidync)
library(osmdata)
library(ggthemes)
library(sf)
library(epitools)
library(stars)
```

# Use read-only mode to find available data without initiating a full download.

``` r
available_data <- function(web_link = "https://cida.usgs.gov/thredds/dodsC/macav2metdata_daily_future",
                           verbose = TRUE){
  
  message('Trying to connect to the USGS.gov API')
  
  src <- tidync(web_link)

  message('Reading results')
  
variable_names <- src$variable %>% dplyr::select(name) 
variable_names <- variable_names[-1:-4,]

available_times <- src %>% activate("D3") %>% hyper_tibble()

colnames(variable_names) <- "Available variable"
colnames(available_times) <- "Available times"
available_times$`Available times` <- as.numeric(available_times$`Available times`)
if(verbose==FALSE){
return(list(
"variable_names" = variable_names, 
"available_times" = available_times,
"src" = src
))  
}else{
  message('Converting into an R data.table')
  
mydates <- seq.Date(as.Date("2006-01-01"), 
                                 as.Date("2099-12-31"), 
                                 by = 1)

myjulian <- julian(mydates, origin=as.Date("1900-01-01"))
time_library <- as.data.frame(cbind("Available times" = myjulian, dates = as.character(julian2date(myjulian))))
time_library$`Available times` <- as.numeric(time_library$`Available times`)

available_times <- as.data.frame(available_times) %>% left_join(time_library, by = "Available times")

available_times$`Available times` <- as.numeric(available_times$`Available times`)

      scenarios <- as.data.frame(matrix(c("rcp45", "rcp85", "RCP 4.5", "RCP 8.5"), 2,2, byrow=FALSE))
      colnames(scenarios) <- c("scenario_abbreviation","scenario_name")
      
     
      
      labels <- as.data.frame(matrix(c("tasmin" , "Minimum Temperature",
                    "tasmax" , "Maximum Temperature",
                    "rhsmin" , "Minimum Relative Humidity",
                    "rhsmax" , "Maximum Relative Humidity",
                    "pr" , "Precipitation",
                    "rsds" , "Surface Downswelling Shortwave Flux",
                    "uas" , "Eastward Wind",
                    "vas" , "Northward Wind",
                    "huss" , "Specific Humidity",
                    "vpd" , "Vapor Pressure Deficit",
                    "rcp45" , "Representative Concentration Pathway 4.5",
                    "rcp85" , "Representative Concentration Pathway 8.5",
                    "bcc-csm1-1" , "Beijing Climate Center - Climate System Model 1.1",
                    "bcc-csm1-1m" , "Beijing Climate Center - Climate System Model 1.1 Moderate Resolution",
                    "BNU-ESM" , "Beijing Normal University - Earth System Model",
                    "CanESM2" , "Canadian Earth System Model 2",
                    "CCSM4" , "Community Climate System Model 4",
                    "CNRM-CM5" , "Centre National de Recherches M\U00E9t\U00E9orologiques - Climate Model 5",
                    "CSIRO-Mk3-6-0" , "Commonwealth Scientific and Industrial Research Organisation - Mk3.6.0",
                    "GFDL-ESM2M" , "Geophysical Fluid Dynamics Laboratory - Earth System Model 2 Modular Ocean",  
                    "GFDL-ESM2G" , "Geophysical Fluid Dynamics Laboratory - Earth System Model 2 Generalized Ocean Layer Dynamics",
                    "HadGEM2-ES365" , "Hadley Global Environment Model 2 - Earth System 365 (day)",
                    "HadGEM2-CC365" , "Hadley Global Environment Model 2 - Climate Chemistry 365 (day) ", 
                    "inmcm4" , "Institute of Numerical Mathematics Climate Model 4",
                    "IPSL-CM5A-LR" , "Institut Pierre Simon Laplace (IPSL) - Climate Model 5A - Low Resolution",
                    "IPSL-CM5A-MR" , "Institut Pierre Simon Laplace (IPSL) - Climate Model 5A - Medium Resolution",
                    "IPSL-CM5B-LR" , "Institut Pierre Simon Laplace (IPSL) - Climate Model 5B - Low Resolution",
                    "MIROC5" , "Model for Interdisciplinary Research On Climate 5",      
                    "MIROC-ESM" , "Model for Interdisciplinary Research On Climate - Earth System Model",
                    "MIROC-ESM-CHEM" , "Model for Interdisciplinary Research On Climate - Earth System Model - Chemistry",
                    "MRI-CGCM3" , "Meteorological Research Institute - Coupled Global Climate Model 3",
                    "NorESM1-M" , "Norwegian Earth System Model 1 - Medium Resolution"), 32, 2, byrow=TRUE))
      
      labels <- as.data.frame(cbind(labels[,1], labels[,1], labels[,2]))
      colnames(labels) <- c("variable_abbreviation","model_abbreviation","name")
      
      units <- as.data.frame(matrix(c("Minimum Temperature" , "K",
                                      "Maximum Temperature" , "K",
                   "Minimum Relative Humidity" , "%",
                   "Maximum Relative Humidity" , "%",
                   "Precipitation" , "mm",
                   "Surface Downswelling Shortwave Flux" , "W m-2",
                   "Eastward Wind" , "m s-1",
                   "Northward Wind" , "m s-1",
                   "Specific Humidity" , "kg kg-1",
                   "Vapor Pressure Deficit" , "kPa"), 10,2, byrow=TRUE))
      colnames(units) <- c("Variable","Units")
      
      
      
variable_names_verbose  <- variable_names %>% separate("Available variable", "_",
                into = c("variable_abbreviation", "model_abbreviation", "ensemble","scenario_abbreviation"), 
                remove = FALSE)

variable_names_verbose <- variable_names_verbose  %>% 
  left_join(labels %>% select("variable_abbreviation","name"), by=c("variable_abbreviation")) %>% 
  left_join(labels %>% select("model_abbreviation","name"), by=c("model_abbreviation")) %>% 
  left_join(scenarios, by=c("scenario_abbreviation")) 





names(variable_names_verbose) <- c("Available variable", "Variable abbreviation",
                                                            "Model abbreviation",
                                                            "Model ensemble type (only CCSM4 relevant)",
                                                            "Scenario abbreviation",
                                                            "Variable",
                                                            "Model",
                                                            "Scenario")

variable_names_verbose <- variable_names_verbose  %>% 
  left_join(units, by=c("Variable"))


variable_names_verbose <- variable_names_verbose %>% select("Available variable",
                                                            "Variable",
                                                            "Units",
                                                            "Model",
                                                            "Model ensemble type (only CCSM4 relevant)",
                                                            "Scenario",
                                                            "Variable abbreviation",
                                                            "Model abbreviation",
                                                            "Scenario abbreviation"
                                                            )



variable_names <- variable_names_verbose

return(list(
"variable_names" = variable_names, 
"available_times" = available_times,
"src" = src
))   
}}

inputs <- available_data()
```

    ## Trying to connect to the USGS.gov API

    ## not a file: 
    ## ' https://cida.usgs.gov/thredds/dodsC/macav2metdata_daily_future '
    ## 
    ## ... attempting remote connection

    ## Connection succeeded.

    ## Reading results

    ## Converting into an R data.table

# Filter the results from available_data() to specify which data to actually download.

## Filter time

``` r
# Year 2034
time_min <- 72048
time_max <- 73048

input_times <- inputs$available_times %>% add_column(index = 0) 
input_times[which(inputs$available_times[,1] > time_min & inputs$available_times[,1] < time_max ),3] <- 1

tail(input_times)
```

    ##       Available times      dates index
    ## 34328           73043 2099-12-26     1
    ## 34329           73044 2099-12-27     1
    ## 34330           73045 2099-12-28     1
    ## 34331           73046 2099-12-29     1
    ## 34332           73047 2099-12-30     1
    ## 34333           73048 2099-12-31     0

## Filter variable names

``` r
input_variables <- inputs$variable_names %>% 
  filter(Variable == "Specific Humidity") %>% 
  filter(Model == c("Beijing Normal University - Earth System Model", "Hadley Global Environment Model 2 - Earth System 365 (day)")) %>%
  filter(Scenario == c("RCP 4.5", "RCP 8.5")) %>% 
  pull("Available variable")

input_variables
```

    ## [1] "huss_BNU-ESM_r1i1p1_rcp45"       "huss_HadGEM2-ES365_r1i1p1_rcp85"

# Establish area of interst (AOI) by bounding box

``` r
bb <- getbb("yellowstone")
bb_manual <- bb
bb_manual[1,1] <- -111.15594815937659
bb_manual[1,2] <- -109.8305463801207
bb_manual[2,1] <- 44.12354048271325
bb_manual[2,2] <- 45.11911641599412

my_boundary <- opq(bb_manual) %>% 
  add_osm_feature(key = "boundary", value = "national_park") %>% 
osmdata_sf() 

my_boundary
```

    ## Object of class 'osmdata' with:
    ##                  $bbox : 44.1235404827133,-111.155948159377,45.1191164159941,-109.830546380121
    ##         $overpass_call : The call submitted to the overpass API
    ##                  $meta : metadata including timestamp and version numbers
    ##            $osm_points : 'sf' Simple Features Collection with 31397 points
    ##             $osm_lines : 'sf' Simple Features Collection with 224 linestrings
    ##          $osm_polygons : 'sf' Simple Features Collection with 11 polygons
    ##        $osm_multilines : NULL
    ##     $osm_multipolygons : 'sf' Simple Features Collection with 5 multipolygons

``` r
boundaries <- my_boundary$osm_multipolygons
pulled_bb <- st_bbox(boundaries)
pulled_bb
```

    ##       xmin       ymin       xmax       ymax 
    ## -113.27781   41.90143 -108.69849   45.10896

``` r
basemap <- ggplot(data = boundaries) +
  geom_sf(fill = "cornflowerblue") +
  geom_sf_text(aes(label = boundaries$name)) 

basemap
```

![](testerFile_files/figure-markdown_github/plot%20of%20area%20of%20interest-1.png)

# Download data by AOI, filtered times, and filtered variable list

``` r
Pulled_data <- inputs$src %>% 
  hyper_filter(lat = lat <= c(pulled_bb[4]+0.05) & lat >= c(pulled_bb[2]-0.05)) %>% 
  hyper_filter(lon = lon <= c(pulled_bb[3]+0.05) & lon >= c(pulled_bb[1]-0.05)) %>% 
  hyper_filter(time =  input_times[,3] == 1) %>% 
  hyper_tibble(select_var = input_variables
    ) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant")

head(Pulled_data)
```

    ## Simple feature collection with 6 features and 3 fields
    ## Attribute-geometry relationship: 3 constant, 0 aggregate, 0 identity
    ## Geometry type: POINT
    ## Dimension:     XY
    ## Bounding box:  xmin: -113.314 ymin: 41.85448 xmax: -113.1057 ymax: 41.85448
    ## Geodetic CRS:  WGS 84
    ## # A tibble: 6 × 4
    ##   `huss_BNU-ESM_r1i1p1_rcp45` `huss_HadGEM2-ES365_r1i1p1_rcp85`  time             geometry
    ##                         <dbl>                             <dbl> <dbl>          <POINT [°]>
    ## 1                     0.00491                           0.00431 72049  (-113.314 41.85448)
    ## 2                     0.00488                           0.00430 72049 (-113.2723 41.85448)
    ## 3                     0.00510                           0.00457 72049 (-113.2307 41.85448)
    ## 4                     0.00521                           0.00464 72049  (-113.189 41.85448)
    ## 5                     0.00536                           0.00473 72049 (-113.1473 41.85448)
    ## 6                     0.00545                           0.00491 72049 (-113.1057 41.85448)

``` r
check_filter <- Pulled_data %>% filter(time == min(Pulled_data$time))

ggplot() +
  geom_sf(data = boundaries, fill = "cornflowerblue") +
 geom_sf(data = check_filter, color = "red", size=0.1) +
  coord_sf(crs = 4326) 
```

![](testerFile_files/figure-markdown_github/check%20pulled%20data-1.png)

# Melt downloaded points into a raster before aggretation

``` r
rast <- st_rasterize(Pulled_data)
plot(rast)
```

![](testerFile_files/figure-markdown_github/rasterize%20with%20stars-1.png)

#Aggregate downloaded data to different spatial objects

## Aggregate to polygon (faster method)

``` r
extracted <- st_extract(rast, boundaries$geometry) %>% st_as_sf()

ggplot(data=extracted) +
  geom_sf(aes(fill = huss_BNU.ESM_r1i1p1_rcp45)) +
  scale_fill_continuous(low="thistle2", high="darkred", 
                       guide="colorbar",na.value="white")+
  coord_sf(crs = 4326)
```

![](testerFile_files/figure-markdown_github/aggregate%20to%20polygon-1.png)

``` r
cube <- src_slc %>% hyper_tbl_cube(select_var = c("huss_BNU-ESM_r1i1p1_rcp45"))
cube
```

## Clip raster with polygon (slower method)

``` r
small_pulled <- Pulled_data %>%
  filter(time == 72049)
intersection <- st_intersection(x = small_pulled, y = boundaries$geometry)

names(intersection)[1:2] <- c("Humidity","b")
```

``` r
ggplot() +
  geom_sf(data = intersection, aes(color=Humidity)) +
  scale_color_continuous(low="thistle2", high="darkred", 
                       guide="colorbar",na.value="white")+
  geom_sf(data = boundaries, fill = NA, color = "white") +
  theme_tufte()+
  labs(title = "YELLOWSTONE NATIONAL PARK", subtitle = "Temperture in 2050")
```

![](testerFile_files/figure-markdown_github/plot%20of%20raster%20mask-1.png)

## Aggregate to River segment

``` r
river <- opq(bb_manual) %>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf() 
river
```

    ## Object of class 'osmdata' with:
    ##                  $bbox : 44.1235404827133,-111.155948159377,45.1191164159941,-109.830546380121
    ##         $overpass_call : The call submitted to the overpass API
    ##                  $meta : metadata including timestamp and version numbers
    ##            $osm_points : 'sf' Simple Features Collection with 34174 points
    ##             $osm_lines : 'sf' Simple Features Collection with 512 linestrings
    ##          $osm_polygons : 'sf' Simple Features Collection with 0 polygons
    ##        $osm_multilines : 'sf' Simple Features Collection with 16 multilinestrings
    ##     $osm_multipolygons : NULL

``` r
river_sub <- st_buffer(river$osm_lines, 2200)
extracted_river <- st_extract(rast,  river_sub$geometry ) %>% st_as_sf()
head(extracted_river)
```

    ## Simple feature collection with 6 features and 3 fields
    ## Geometry type: POLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: -112.9075 ymin: 42.75654 xmax: -110.8015 ymax: 44.68225
    ## Geodetic CRS:  WGS 84
    ##   huss_BNU.ESM_r1i1p1_rcp45 huss_HadGEM2.ES365_r1i1p1_rcp85  time                       geometry
    ## 1               0.003031481                     0.004334444 73047 POLYGON ((-112.8874 42.7605...
    ## 2               0.002355000                     0.003540000 73047 POLYGON ((-110.9755 44.6740...
    ## 3               0.002347500                     0.003532500 73047 POLYGON ((-110.9755 44.6740...
    ## 4               0.002360000                     0.003520000 73047 POLYGON ((-110.9809 44.6787...
    ## 5               0.002350000                     0.003490000 73047 POLYGON ((-110.8453 44.6551...
    ## 6               0.002395000                     0.003525000 73047 POLYGON ((-110.8306 44.5611...

``` r
#colnames(extracted_river)[1] <- "var1"
```

``` r
ggplot(data=extracted_river) +
  geom_sf(aes(fill = huss_BNU.ESM_r1i1p1_rcp45), size=0) +
   coord_sf(crs = 4326, xlim = c(pulled_bb[1], pulled_bb[3]), 
           ylim = c(pulled_bb[2], pulled_bb[4]),
           expand = FALSE) +
  scale_fill_continuous(low="thistle2", high="darkred", 
                       guide="colorbar",na.value="white")+
  labs(title = "Rivers of Yellowstone",
       subtitle = "Projected humidity in 2040", 
       caption = "Data Source: Climate Futures...") + 
  theme_tufte()
```

![](testerFile_files/figure-markdown_github/plot%20river%20aggregation-1.png)

## Aggregate to road segment

``` r
roads <- opq(bb_manual) %>%
  add_osm_feature(key = 'highway', value = 'primary') %>%
  add_osm_feature(key = 'highway', value = 'secondary') %>%
  osmdata_sf() 
roads_sub <- st_buffer(roads$osm_lines, 2200)
extracted_roads <- st_extract(rast,  roads_sub$geometry ) %>% st_as_sf()
extracted_roads
```

    ## Simple feature collection with 345 features and 3 fields
    ## Geometry type: POLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: -111.2232 ymin: 44.0906 xmax: -109.7656 ymax: 45.27416
    ## Geodetic CRS:  WGS 84
    ## First 10 features:
    ##    huss_BNU.ESM_r1i1p1_rcp45 huss_HadGEM2.ES365_r1i1p1_rcp85  time                       geometry
    ## 1                0.002387500                       0.0035525 73047 POLYGON ((-110.9855 44.6672...
    ## 2                0.002405000                       0.0036600 73047 POLYGON ((-111.2083 44.6620...
    ## 3                0.002350000                       0.0036100 73047 POLYGON ((-111.1227 44.6451...
    ## 4                0.002286667                       0.0035550 73047 POLYGON ((-111.1332 45.0682...
    ## 5                0.002350000                       0.0036100 73047 POLYGON ((-111.1275 44.6695...
    ## 6                0.002370000                       0.0036700 73047 POLYGON ((-111.029 44.85203...
    ## 7                0.002350000                       0.0036100 73047 POLYGON ((-111.125 44.66158...
    ## 8                0.002335000                       0.0036500 73047 POLYGON ((-111.1376 44.8132...
    ## 9                0.002350000                       0.0036850 73047 POLYGON ((-111.0784 44.8061...
    ## 10               0.002350000                       0.0036100 73047 POLYGON ((-111.1275 44.6592...

``` r
ggplot(data=extracted_roads) +
  geom_sf(aes(fill = huss_BNU.ESM_r1i1p1_rcp45), size=0) +
   coord_sf(crs = 4326) +
  scale_fill_continuous(low="thistle2", high="darkred", 
                       guide="colorbar",na.value="white")+
  labs(title = "Roads of Yellowstone",
       subtitle = "Projected humidity in 2040", 
       caption = "Data Source: Climate Futures...") + 
  theme_tufte()
```

![](testerFile_files/figure-markdown_github/plot%20road%20aggregation-1.png)
