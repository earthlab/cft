# Welcome to the Climate Futures Toolbox

The purpose of this package is to make it easy to download and process
climate data from the … database.

``` r
library(devtools)
install_github("earthlab/cft")
```

``` r
library(cft)
ls(pos="package:cft")
```

    ## [1] "available_data"

``` r
?available_data
```

# Use read-only mode to find available data without initiating a full download.

``` r
inputs <- cft::available_data()
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
  filter(Variable == "Precipitation") %>% 
  filter(Model == c("Beijing Normal University - Earth System Model", "Hadley Global Environment Model 2 - Earth System 365 (day)")) %>%
  filter(Scenario == c( "RCP 8.5")) %>% 
  pull("Available variable")

input_variables
```

    ## [1] "pr_HadGEM2-ES365_r1i1p1_rcp85"

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
    ##            $osm_points : 'sf' Simple Features Collection with 31372 points
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

![](README_files/figure-gfm/plot%20of%20area%20of%20interest-1.png)<!-- -->

# Download data by AOI, filtered times, and filtered variable list

``` r
Pulled_data <- inputs$src %>% 
  hyper_filter(lat = lat <= c(pulled_bb[4]+0.05) & lat >= c(pulled_bb[2]-0.05)) %>% 
  hyper_filter(lon = lon <= c(pulled_bb[3]+0.05) & lon >= c(pulled_bb[1]-0.05)) %>% 
  hyper_filter(time =  input_times[,3] == 1) %>% 
  hyper_tibble(select_var = input_variables
    ) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant")
## should time be in here?
head(Pulled_data)
```

    ## Simple feature collection with 6 features and 2 fields
    ## Attribute-geometry relationship: 2 constant, 0 aggregate, 0 identity
    ## Geometry type: POINT
    ## Dimension:     XY
    ## Bounding box:  xmin: -113.314 ymin: 41.85448 xmax: -113.1057 ymax: 41.85448
    ## Geodetic CRS:  WGS 84
    ## # A tibble: 6 × 3
    ##   `pr_HadGEM2-ES365_r1i1p1_rcp85`  time             geometry
    ##                             <dbl> <dbl>          <POINT [°]>
    ## 1                            8.80 72049  (-113.314 41.85448)
    ## 2                            6.00 72049 (-113.2723 41.85448)
    ## 3                            4.80 72049 (-113.2307 41.85448)
    ## 4                            3.10 72049  (-113.189 41.85448)
    ## 5                            3.40 72049 (-113.1473 41.85448)
    ## 6                            3.10 72049 (-113.1057 41.85448)

``` r
check_filter <- Pulled_data %>% filter(time == min(Pulled_data$time))

ggplot() +
  geom_sf(data = boundaries, fill = "cornflowerblue") +
 geom_sf(data = check_filter, color = "red", size=0.5) +
  coord_sf(crs = 4326) 
```

![](README_files/figure-gfm/check%20pulled%20data-1.png)<!-- -->

# Melt downloaded points into a raster before aggretation

``` r
rast <- st_rasterize(Pulled_data) 
plot(rast)
```

![](README_files/figure-gfm/rasterize%20with%20stars-1.png)<!-- -->

``` r
#Pulled_data %>% as.data.frame() %>% brick()
```

# GridMET data

``` r
param_meta$gridmet
```

    ##       common.name   call                                     description timestep                        units
    ## 1            prcp     pr                            precipitation_amount    daily                           mm
    ## 2           rhmax   rmax                 daily_maximum_relative_humidity    daily                      Percent
    ## 3           rhmin   rmin                 daily_minimum_relative_humidity    daily                      Percent
    ## 4            shum    sph                    daily_mean_specific_humidity    daily                        kg/kg
    ## 5            srad   srad       daily_mean_shortwave_radiation_at_surface    daily                        W/m^2
    ## 6        wind_dir     th                       daily_mean_wind_direction    daily Degrees Clockwise from north
    ## 7            tmin   tmmn                       daily_minimum_temperature    daily                         degK
    ## 8            tmax   tmmx                       daily_maximum_temperature    daily                         degK
    ## 9        wind_vel     vs                           daily_mean_wind_speed    daily                          m/s
    ## 10     burn_index     bi                      daily_mean_burning_index_g    daily                     Unitless
    ## 11     fmoist_100  fm100                        dead_fuel_moisture_100hr    daily                      Percent
    ## 12    fmoist_1000 fm1000                       dead_fuel_moisture_1000hr    daily                      Percent
    ## 13 energy_release    erc           daily_mean_energy_release_component-g    daily                     Unitless
    ## 14         palmer   pdsi        daily_mean_palmer_drought_severity_index   pentad                     Unitless
    ## 15    pet_alfalfa    etr daily_mean_reference_evapotranspiration_alfalfa    daily                           mm
    ## 16      pet_grass    pet   daily_mean_reference_evapotranspiration_grass    daily                           mm
    ## 17            vpd    vpd               daily_mean_vapor_pressure_deficit    daily                          kPa

``` r
subed_times <- input_times %>% filter(index == 1) 
GM <- getGridMET(st_as_sfc(st_bbox(boundaries)), "tmax", startDate = "1997-04-06", endDate = "1999-12-30")
SM_stars <- GM$gridmet_tmax %>% brick() %>%  st_as_stars()
#st_set_dimensions(SM_stars, 3, values = X1997.04.06, names = "tmax")

ggplot() +
  geom_sf(data = SM_stars, fill = "cornflowerblue") +
 geom_sf(data = check_filter, color = "red", size=0.5) +
  coord_sf(crs = 4326) 
```

``` r
st_get_dimension_values(rast)
#combo <- st_extract(rast, SM_stars) %>% st_as_sf()
combo <- c(SM_stars,rast , along_crs=TRUE, along=c(1,2))
class(combo) 
class(SM_stars)
plot(combo$X2000.01.01)
```

``` r
plot(combo$X2000.01.01)

#extracted_GridMET <- st_extract(rast, SM_stars) %>% st_as_sf()
ggplot(data=combo) +
  geom_sf(aes(fill = X2000.01.01)) +
  scale_fill_continuous(low="thistle2", high="darkred", 
                       guide="colorbar",na.value="white")+
  coord_sf(crs = 4326)
```

#Aggregate downloaded data to different spatial objects

## Aggregate to polygon (faster method)

``` r
extracted <- st_extract(rast, boundaries$geometry) %>% st_as_sf()
names(extracted)[1] <- "nn"
ggplot(data=extracted) +
  geom_sf(aes(fill = nn)) +
  scale_fill_continuous(low="thistle2", high="darkred", 
                       guide="colorbar",na.value="white")+
  coord_sf(crs = 4326)
```

![](README_files/figure-gfm/aggregate%20to%20polygon-1.png)<!-- -->

``` r
cube <- src_slc %>% hyper_tbl_cube(select_var = c("pr_HadGEM2-ES365_r1i1p1_rcp85"))
cube
```

## Clip raster with polygon (slower method)

``` r
small_pulled <- Pulled_data %>%
  filter(time == 72049)
intersection <- st_intersection(x = small_pulled, y = boundaries$geometry)

names(intersection)[1:2] <- c("Precipitation","b")
```

``` r
library(ggthemes)
ggplot() +
  geom_sf(data = intersection, aes(color=Precipitation)) +
  scale_color_continuous(low="thistle2", high="darkred", 
                       guide="colorbar",na.value="white")+
  geom_sf(data = boundaries, fill = NA, color = "white") +
  theme_tufte()+
  labs(title = "YELLOWSTONE NATIONAL PARK", subtitle = "Temperture in 2050")
```

![](README_files/figure-gfm/plot%20of%20raster%20mask-1.png)<!-- -->

## Aggregate to River segment

``` r
river <- opq(bb_manual) %>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf() 
```

    ## Request failed [504]. Retrying in 1.8 seconds...

``` r
river
```

    ## Object of class 'osmdata' with:
    ##                  $bbox : 44.1235404827133,-111.155948159377,45.1191164159941,-109.830546380121
    ##         $overpass_call : The call submitted to the overpass API
    ##                  $meta : metadata including timestamp and version numbers
    ##            $osm_points : 'sf' Simple Features Collection with 34185 points
    ##             $osm_lines : 'sf' Simple Features Collection with 512 linestrings
    ##          $osm_polygons : 'sf' Simple Features Collection with 0 polygons
    ##        $osm_multilines : 'sf' Simple Features Collection with 16 multilinestrings
    ##     $osm_multipolygons : NULL

``` r
river_sub <- st_buffer(river$osm_lines, 2200)
extracted_river <- st_extract(rast,  river_sub$geometry ) %>% st_as_sf()
head(extracted_river)
```

    ## Simple feature collection with 6 features and 2 fields
    ## Geometry type: POLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: -112.9075 ymin: 42.75654 xmax: -110.8015 ymax: 44.68225
    ## Geodetic CRS:  WGS 84
    ##   pr_HadGEM2.ES365_r1i1p1_rcp85  time                       geometry
    ## 1                    0.04814815 73047 POLYGON ((-112.8874 42.7605...
    ## 2                    2.60000002 73047 POLYGON ((-110.9755 44.6740...
    ## 3                    2.60000002 73047 POLYGON ((-110.9755 44.6740...
    ## 4                    2.40000010 73047 POLYGON ((-110.9809 44.6787...
    ## 5                    2.39999998 73047 POLYGON ((-110.8453 44.6551...
    ## 6                    2.35000002 73047 POLYGON ((-110.8306 44.5611...

``` r
#colnames(extracted_river)[1] <- "var1"
```

``` r
ggplot(data=extracted_river) +
  geom_sf(aes(fill = pr_HadGEM2.ES365_r1i1p1_rcp85), size=0) +
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

![](README_files/figure-gfm/plot%20river%20aggregation-1.png)<!-- -->

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

    ## Simple feature collection with 256 features and 2 fields
    ## Geometry type: POLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: -111.1182 ymin: 44.11161 xmax: -109.9742 ymax: 45.04956
    ## Geodetic CRS:  WGS 84
    ## First 10 features:
    ##    pr_HadGEM2.ES365_r1i1p1_rcp85  time                       geometry
    ## 1                          2.125 73047 POLYGON ((-110.9855 44.6672...
    ## 2                          0.500 73047 POLYGON ((-110.711 44.99508...
    ## 3                          0.500 73047 POLYGON ((-110.7109 44.9949...
    ## 4                          2.500 73047 POLYGON ((-110.8482 44.6642...
    ## 5                          2.150 73047 POLYGON ((-110.3904 44.5473...
    ## 6                          1.500 73047 POLYGON ((-110.835 44.44695...
    ## 7                          1.500 73047 POLYGON ((-110.8194 44.4478...
    ## 8                          1.500 73047 POLYGON ((-110.8194 44.4478...
    ## 9                             NA    NA POLYGON ((-110.704 44.94797...
    ## 10                         0.500 73047 POLYGON ((-110.7044 44.9503...

``` r
ggplot(data=extracted_roads) +
  geom_sf(aes(fill = pr_HadGEM2.ES365_r1i1p1_rcp85), size=0) +
   coord_sf(crs = 4326) +
  scale_fill_continuous(low="thistle2", high="darkred", 
                       guide="colorbar",na.value="white")+
  labs(title = "Roads of Yellowstone",
       subtitle = "Projected humidity in 2040", 
       caption = "Data Source: Climate Futures...") + 
  theme_tufte()
```

![](README_files/figure-gfm/plot%20road%20aggregation-1.png)<!-- -->
