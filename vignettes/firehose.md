---
title: "R Notebook"
output: html_notebook
---



```r
library(cft)
```


```r
n_cores <- availableCores() - 1
plan(multiprocess, workers = n_cores)
```


```r
web_link = "https://cida.usgs.gov/thredds/dodsC/macav2metdata_daily_future"
src <- tidync::tidync(web_link)
```

```
## not a file: 
## ' https://cida.usgs.gov/thredds/dodsC/macav2metdata_daily_future '
## 
## ... attempting remote connection
```

```
## Connection succeeded.
```


```r
inputs <- cft::available_data()
```

```
## Trying to connect to the USGS.gov API
```

```
## not a file: 
## ' https://cida.usgs.gov/thredds/dodsC/macav2metdata_daily_future '
## 
## ... attempting remote connection
```

```
## Connection succeeded.
```

```
## Reading results
```

```
## Converting into an R data.table
```

```r
inputs[[1]]
```

```
## # A tibble: 350 × 9
##    `Available variable`       Variable Units Model `Model ensembl…` Scenario `Variable abbr…` `Model abbrevi…` `Scenario abbr…`
##    <chr>                      <chr>    <chr> <chr> <chr>            <chr>    <chr>            <chr>            <chr>           
##  1 huss_BNU-ESM_r1i1p1_rcp45  Specifi… kg k… Beij… r1i1p1           RCP 4.5  huss             BNU-ESM          rcp45           
##  2 huss_BNU-ESM_r1i1p1_rcp85  Specifi… kg k… Beij… r1i1p1           RCP 8.5  huss             BNU-ESM          rcp85           
##  3 huss_CCSM4_r6i1p1_rcp45    Specifi… kg k… Comm… r6i1p1           RCP 4.5  huss             CCSM4            rcp45           
##  4 huss_CCSM4_r6i1p1_rcp85    Specifi… kg k… Comm… r6i1p1           RCP 8.5  huss             CCSM4            rcp85           
##  5 huss_CNRM-CM5_r1i1p1_rcp45 Specifi… kg k… Cent… r1i1p1           RCP 4.5  huss             CNRM-CM5         rcp45           
##  6 huss_CNRM-CM5_r1i1p1_rcp85 Specifi… kg k… Cent… r1i1p1           RCP 8.5  huss             CNRM-CM5         rcp85           
##  7 huss_CSIRO-Mk3-6-0_r1i1p1… Specifi… kg k… Comm… r1i1p1           RCP 4.5  huss             CSIRO-Mk3-6-0    rcp45           
##  8 huss_CSIRO-Mk3-6-0_r1i1p1… Specifi… kg k… Comm… r1i1p1           RCP 8.5  huss             CSIRO-Mk3-6-0    rcp85           
##  9 huss_CanESM2_r1i1p1_rcp45  Specifi… kg k… Cana… r1i1p1           RCP 4.5  huss             CanESM2          rcp45           
## 10 huss_CanESM2_r1i1p1_rcp85  Specifi… kg k… Cana… r1i1p1           RCP 8.5  huss             CanESM2          rcp85           
## # … with 340 more rows
```



```r
input_variables <- inputs$variable_names %>% 
  filter(Variable %in% c("Maximum Relative Humidity", 
                       "Maximum Temperature", 
                       "Minimum Relative Humidity",          
                       "Minimum Temperature",                 
                       "Precipitation")) %>% 
  filter(Scenario %in% c( "RCP 8.5", "RCP 4.5")) %>% 
  filter(Model %in% c(
    "Beijing Climate Center - Climate System Model 1.1",
    "Beijing Normal University - Earth System Model",
    "Canadian Earth System Model 2",                                                                
  "Centre National de Recherches Météorologiques - Climate Model 5",                              
  "Commonwealth Scientific and Industrial Research Organisation - Mk3.6.0",                       
  "Community Climate System Model 4",                                                             
  "Geophysical Fluid Dynamics Laboratory - Earth System Model 2 Generalized Ocean Layer Dynamics",
  "Geophysical Fluid Dynamics Laboratory - Earth System Model 2 Modular Ocean",                   
  "Hadley Global Environment Model 2 - Climate Chemistry 365 (day) ",                             
 "Hadley Global Environment Model 2 - Earth System 365 (day)",                                   
 "Institut Pierre Simon Laplace (IPSL) - Climate Model 5A - Low Resolution",                     
 "Institut Pierre Simon Laplace (IPSL) - Climate Model 5A - Medium Resolution",                  
 "Institut Pierre Simon Laplace (IPSL) - Climate Model 5B - Low Resolution",                     
 "Institute of Numerical Mathematics Climate Model 4",                                           
 "Meteorological Research Institute - Coupled Global Climate Model 3",                           
 "Model for Interdisciplinary Research On Climate - Earth System Model",                         
 "Model for Interdisciplinary Research On Climate - Earth System Model - Chemistry",             
 "Model for Interdisciplinary Research On Climate 5",                                            
 "Norwegian Earth System Model 1 - Medium Resolution"  )) %>%
  
  pull("Available variable")

input_variables
```

```
##   [1] "pr_BNU-ESM_r1i1p1_rcp45"            "pr_BNU-ESM_r1i1p1_rcp85"            "pr_CCSM4_r6i1p1_rcp45"             
##   [4] "pr_CCSM4_r6i1p1_rcp85"              "pr_CNRM-CM5_r1i1p1_rcp45"           "pr_CNRM-CM5_r1i1p1_rcp85"          
##   [7] "pr_CSIRO-Mk3-6-0_r1i1p1_rcp45"      "pr_CSIRO-Mk3-6-0_r1i1p1_rcp85"      "pr_CanESM2_r1i1p1_rcp45"           
##  [10] "pr_CanESM2_r1i1p1_rcp85"            "pr_GFDL-ESM2G_r1i1p1_rcp45"         "pr_GFDL-ESM2G_r1i1p1_rcp85"        
##  [13] "pr_GFDL-ESM2M_r1i1p1_rcp45"         "pr_GFDL-ESM2M_r1i1p1_rcp85"         "pr_HadGEM2-CC365_r1i1p1_rcp45"     
##  [16] "pr_HadGEM2-CC365_r1i1p1_rcp85"      "pr_HadGEM2-ES365_r1i1p1_rcp45"      "pr_HadGEM2-ES365_r1i1p1_rcp85"     
##  [19] "pr_IPSL-CM5A-LR_r1i1p1_rcp45"       "pr_IPSL-CM5A-LR_r1i1p1_rcp85"       "pr_IPSL-CM5A-MR_r1i1p1_rcp45"      
##  [22] "pr_IPSL-CM5A-MR_r1i1p1_rcp85"       "pr_IPSL-CM5B-LR_r1i1p1_rcp45"       "pr_IPSL-CM5B-LR_r1i1p1_rcp85"      
##  [25] "pr_MIROC-ESM-CHEM_r1i1p1_rcp45"     "pr_MIROC-ESM-CHEM_r1i1p1_rcp85"     "pr_MIROC-ESM_r1i1p1_rcp85"         
##  [28] "pr_MIROC-ESM_r1i1p1_rcp45"          "pr_MIROC5_r1i1p1_rcp45"             "pr_MIROC5_r1i1p1_rcp85"            
##  [31] "pr_MRI-CGCM3_r1i1p1_rcp45"          "pr_MRI-CGCM3_r1i1p1_rcp85"          "pr_NorESM1-M_r1i1p1_rcp45"         
##  [34] "pr_NorESM1-M_r1i1p1_rcp85"          "pr_bcc-csm1-1_r1i1p1_rcp45"         "pr_bcc-csm1-1_r1i1p1_rcp85"        
##  [37] "pr_inmcm4_r1i1p1_rcp45"             "pr_inmcm4_r1i1p1_rcp85"             "rhsmax_BNU-ESM_r1i1p1_rcp45"       
##  [40] "rhsmax_BNU-ESM_r1i1p1_rcp85"        "rhsmax_CNRM-CM5_r1i1p1_rcp45"       "rhsmax_CNRM-CM5_r1i1p1_rcp85"      
##  [43] "rhsmax_CSIRO-Mk3-6-0_r1i1p1_rcp45"  "rhsmax_CSIRO-Mk3-6-0_r1i1p1_rcp85"  "rhsmax_CanESM2_r1i1p1_rcp45"       
##  [46] "rhsmax_CanESM2_r1i1p1_rcp85"        "rhsmax_GFDL-ESM2G_r1i1p1_rcp45"     "rhsmax_GFDL-ESM2G_r1i1p1_rcp85"    
##  [49] "rhsmax_GFDL-ESM2M_r1i1p1_rcp45"     "rhsmax_HadGEM2-CC365_r1i1p1_rcp45"  "rhsmax_HadGEM2-CC365_r1i1p1_rcp85" 
##  [52] "rhsmax_HadGEM2-ES365_r1i1p1_rcp45"  "rhsmax_HadGEM2-ES365_r1i1p1_rcp85"  "rhsmax_IPSL-CM5A-LR_r1i1p1_rcp45"  
##  [55] "rhsmax_IPSL-CM5A-LR_r1i1p1_rcp85"   "rhsmax_IPSL-CM5A-MR_r1i1p1_rcp45"   "rhsmax_IPSL-CM5A-MR_r1i1p1_rcp85"  
##  [58] "rhsmax_IPSL-CM5B-LR_r1i1p1_rcp45"   "rhsmax_IPSL-CM5B-LR_r1i1p1_rcp85"   "rhsmax_MIROC-ESM-CHEM_r1i1p1_rcp45"
##  [61] "rhsmax_MIROC-ESM-CHEM_r1i1p1_rcp85" "rhsmax_MIROC-ESM_r1i1p1_rcp45"      "rhsmax_MIROC-ESM_r1i1p1_rcp85"     
##  [64] "rhsmax_MIROC5_r1i1p1_rcp45"         "rhsmax_MIROC5_r1i1p1_rcp85"         "rhsmax_MRI-CGCM3_r1i1p1_rcp45"     
##  [67] "rhsmax_MRI-CGCM3_r1i1p1_rcp85"      "rhsmax_bcc-csm1-1_r1i1p1_rcp45"     "rhsmax_bcc-csm1-1_r1i1p1_rcp85"    
##  [70] "rhsmax_inmcm4_r1i1p1_rcp45"         "rhsmax_inmcm4_r1i1p1_rcp85"         "rhsmin_BNU-ESM_r1i1p1_rcp45"       
##  [73] "rhsmin_BNU-ESM_r1i1p1_rcp85"        "rhsmin_CNRM-CM5_r1i1p1_rcp45"       "rhsmin_CNRM-CM5_r1i1p1_rcp85"      
##  [76] "rhsmin_CSIRO-Mk3-6-0_r1i1p1_rcp45"  "rhsmin_CSIRO-Mk3-6-0_r1i1p1_rcp85"  "rhsmin_CanESM2_r1i1p1_rcp45"       
##  [79] "rhsmin_CanESM2_r1i1p1_rcp85"        "rhsmin_GFDL-ESM2G_r1i1p1_rcp45"     "rhsmin_GFDL-ESM2G_r1i1p1_rcp85"    
##  [82] "rhsmin_GFDL-ESM2M_r1i1p1_rcp45"     "rhsmin_GFDL-ESM2M_r1i1p1_rcp85"     "rhsmin_HadGEM2-CC365_r1i1p1_rcp45" 
##  [85] "rhsmin_HadGEM2-CC365_r1i1p1_rcp85"  "rhsmin_HadGEM2-ES365_r1i1p1_rcp45"  "rhsmin_HadGEM2-ES365_r1i1p1_rcp85" 
##  [88] "rhsmin_IPSL-CM5A-LR_r1i1p1_rcp45"   "rhsmin_IPSL-CM5A-LR_r1i1p1_rcp85"   "rhsmin_IPSL-CM5A-MR_r1i1p1_rcp45"  
##  [91] "rhsmin_IPSL-CM5A-MR_r1i1p1_rcp85"   "rhsmin_IPSL-CM5B-LR_r1i1p1_rcp45"   "rhsmin_IPSL-CM5B-LR_r1i1p1_rcp85"  
##  [94] "rhsmin_MIROC-ESM-CHEM_r1i1p1_rcp45" "rhsmin_MIROC-ESM-CHEM_r1i1p1_rcp85" "rhsmin_MIROC-ESM_r1i1p1_rcp45"     
##  [97] "rhsmin_MIROC-ESM_r1i1p1_rcp85"      "rhsmin_MIROC5_r1i1p1_rcp45"         "rhsmin_MIROC5_r1i1p1_rcp85"        
## [100] "rhsmin_MRI-CGCM3_r1i1p1_rcp45"      "rhsmin_MRI-CGCM3_r1i1p1_rcp85"      "rhsmin_bcc-csm1-1_r1i1p1_rcp45"    
## [103] "rhsmin_bcc-csm1-1_r1i1p1_rcp85"     "rhsmin_inmcm4_r1i1p1_rcp45"         "rhsmin_inmcm4_r1i1p1_rcp85"        
## [106] "tasmax_BNU-ESM_r1i1p1_rcp45"        "tasmax_BNU-ESM_r1i1p1_rcp85"        "tasmax_CCSM4_r6i1p1_rcp45"         
## [109] "tasmax_CCSM4_r6i1p1_rcp85"          "tasmax_CNRM-CM5_r1i1p1_rcp45"       "tasmax_CNRM-CM5_r1i1p1_rcp85"      
## [112] "tasmax_CSIRO-Mk3-6-0_r1i1p1_rcp45"  "tasmax_CSIRO-Mk3-6-0_r1i1p1_rcp85"  "tasmax_CanESM2_r1i1p1_rcp45"       
## [115] "tasmax_CanESM2_r1i1p1_rcp85"        "tasmax_GFDL-ESM2G_r1i1p1_rcp45"     "tasmax_GFDL-ESM2G_r1i1p1_rcp85"    
## [118] "tasmax_GFDL-ESM2M_r1i1p1_rcp45"     "tasmax_GFDL-ESM2M_r1i1p1_rcp85"     "tasmax_HadGEM2-CC365_r1i1p1_rcp45" 
## [121] "tasmax_HadGEM2-CC365_r1i1p1_rcp85"  "tasmax_HadGEM2-ES365_r1i1p1_rcp45"  "tasmax_HadGEM2-ES365_r1i1p1_rcp85" 
## [124] "tasmax_IPSL-CM5A-LR_r1i1p1_rcp45"   "tasmax_IPSL-CM5A-LR_r1i1p1_rcp85"   "tasmax_IPSL-CM5A-MR_r1i1p1_rcp45"  
## [127] "tasmax_IPSL-CM5A-MR_r1i1p1_rcp85"   "tasmax_IPSL-CM5B-LR_r1i1p1_rcp45"   "tasmax_IPSL-CM5B-LR_r1i1p1_rcp85"  
## [130] "tasmax_MIROC-ESM-CHEM_r1i1p1_rcp45" "tasmax_MIROC-ESM-CHEM_r1i1p1_rcp85" "tasmax_MIROC-ESM_r1i1p1_rcp45"     
## [133] "tasmax_MIROC-ESM_r1i1p1_rcp85"      "tasmax_MIROC5_r1i1p1_rcp45"         "tasmax_MIROC5_r1i1p1_rcp85"        
## [136] "tasmax_MRI-CGCM3_r1i1p1_rcp45"      "tasmax_MRI-CGCM3_r1i1p1_rcp85"      "tasmax_NorESM1-M_r1i1p1_rcp45"     
## [139] "tasmax_NorESM1-M_r1i1p1_rcp85"      "tasmax_bcc-csm1-1_r1i1p1_rcp45"     "tasmax_bcc-csm1-1_r1i1p1_rcp85"    
## [142] "tasmax_inmcm4_r1i1p1_rcp45"         "tasmax_inmcm4_r1i1p1_rcp85"         "tasmin_BNU-ESM_r1i1p1_rcp45"       
## [145] "tasmin_BNU-ESM_r1i1p1_rcp85"        "tasmin_CCSM4_r6i1p1_rcp45"          "tasmin_CCSM4_r6i1p1_rcp85"         
## [148] "tasmin_CNRM-CM5_r1i1p1_rcp45"       "tasmin_CNRM-CM5_r1i1p1_rcp85"       "tasmin_CSIRO-Mk3-6-0_r1i1p1_rcp45" 
## [151] "tasmin_CSIRO-Mk3-6-0_r1i1p1_rcp85"  "tasmin_CanESM2_r1i1p1_rcp45"        "tasmin_CanESM2_r1i1p1_rcp85"       
## [154] "tasmin_GFDL-ESM2G_r1i1p1_rcp45"     "tasmin_GFDL-ESM2G_r1i1p1_rcp85"     "tasmin_GFDL-ESM2M_r1i1p1_rcp45"    
## [157] "tasmin_GFDL-ESM2M_r1i1p1_rcp85"     "tasmin_HadGEM2-CC365_r1i1p1_rcp45"  "tasmin_HadGEM2-CC365_r1i1p1_rcp85" 
## [160] "tasmin_HadGEM2-ES365_r1i1p1_rcp45"  "tasmin_HadGEM2-ES365_r1i1p1_rcp85"  "tasmin_IPSL-CM5A-LR_r1i1p1_rcp45"  
## [163] "tasmin_IPSL-CM5A-LR_r1i1p1_rcp85"   "tasmin_IPSL-CM5A-MR_r1i1p1_rcp45"   "tasmin_IPSL-CM5A-MR_r1i1p1_rcp85"  
## [166] "tasmin_IPSL-CM5B-LR_r1i1p1_rcp45"   "tasmin_IPSL-CM5B-LR_r1i1p1_rcp85"   "tasmin_MIROC-ESM-CHEM_r1i1p1_rcp45"
## [169] "tasmin_MIROC-ESM-CHEM_r1i1p1_rcp85" "tasmin_MIROC-ESM_r1i1p1_rcp45"      "tasmin_MIROC-ESM_r1i1p1_rcp85"     
## [172] "tasmin_MIROC5_r1i1p1_rcp45"         "tasmin_MIROC5_r1i1p1_rcp85"         "tasmin_MRI-CGCM3_r1i1p1_rcp45"     
## [175] "tasmin_MRI-CGCM3_r1i1p1_rcp85"      "tasmin_NorESM1-M_r1i1p1_rcp45"      "tasmin_NorESM1-M_r1i1p1_rcp85"     
## [178] "tasmin_bcc-csm1-1_r1i1p1_rcp45"     "tasmin_bcc-csm1-1_r1i1p1_rcp85"     "tasmin_inmcm4_r1i1p1_rcp45"        
## [181] "tasmin_inmcm4_r1i1p1_rcp85"
```





```r
aoi_name <- "colorado"
bb <- getbb(aoi_name)
my_boundary <- opq(bb) %>%
  add_osm_feature(key = "boundary", value = "national_park") %>%
  osmdata_sf()
```

```
## Error in check_for_error(doc): General overpass server error; returned:
## The data included in this document is from www.openstreetmap.org. The data is made available under ODbL. runtime error: Query timed out in "query" at line 5 after 26 seconds.
```

```r
my_boundary
```

```
## Object of class 'osmdata' with:
##                  $bbox : 36.992426,-109.0601879,41.0034002,-102.041585
##         $overpass_call : The call submitted to the overpass API
##                  $meta : metadata including timestamp and version numbers
##            $osm_points : 'sf' Simple Features Collection with 39511 points
##             $osm_lines : 'sf' Simple Features Collection with 276 linestrings
##          $osm_polygons : 'sf' Simple Features Collection with 38 polygons
##        $osm_multilines : NULL
##     $osm_multipolygons : 'sf' Simple Features Collection with 21 multipolygons
```

```r
#boundaries <- my_boundary$osm_polygons[1,]
boundaries <- my_boundary$osm_multipolygons[1,] #change to multipolygons
pulled_bb <-  st_bbox(boundaries)
pulled_bb
```

```
##       xmin       ymin       xmax       ymax 
## -105.91371   40.15777 -105.49358   40.55379
```

```r
pt <- st_coordinates(st_centroid(boundaries))
```

```
## Warning in st_centroid.sf(boundaries): st_centroid assumes attributes are constant over geometries of x
```

```r
lat_pt <- pt[1,2]
lon_pt <- pt[1,1]

lons <- src %>% activate("D2") %>% hyper_tibble()
lats <- src %>% activate("D1") %>% hyper_tibble()

new_lon <- lons[which(abs(lons-lon_pt)==min(abs(lons-lon_pt))),]
new_lat <- lats[which(abs(lats-lat_pt)==min(abs(lats-lat_pt))),]


chosen_pt <- st_as_sf(cbind(new_lon,new_lat), coords = c("lon", "lat"), crs = "WGS84", agr = "constant")

ggplot() +
  geom_sf(data = boundaries, fill = "cornflowerblue") +
  geom_sf(data = st_centroid(boundaries), color = "red", size=0.5) +
  geom_sf(data = chosen_pt, color = "green", size=0.5)
```

```
## Warning in st_centroid.sf(boundaries): st_centroid assumes attributes are constant over geometries of x
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)



```r
out <- single_point_firehose(input_variables, new_lat, new_lon )
out
```



```r
ggplot() +
  geom_sf(data = boundaries, fill = "cornflowerblue") +
 geom_sf(data = pulled_data_sf, color = "red", size=0.5) +
  coord_sf(crs = 4326) 
```

```
## Error in fortify(data): object 'pulled_data_sf' not found
```

