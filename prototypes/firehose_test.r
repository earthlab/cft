library(cft)
library(doParallel)
library(foreach)

# set the web link
src <- tidync::tidync("https://cida.usgs.gov/thredds/dodsC/macav2metdata_daily_future")

# set up the area of interest and bounding box, then obtain the relevant OSM data
aoi_name <- "colorado"
bb <- getbb(aoi_name)
my_boundary <- opq(bb) %>%
  add_osm_feature(key = "boundary", value = "national_park") %>%
  osmdata_sf()

my_boundary
my_boundary$osm_multipolygons

boundaries <- my_boundary$osm_multipolygons[1,] 

pulled_bb <-  st_bbox(boundaries)

# get the coordinates of the center point of the desired boundary
pt <- st_coordinates(st_centroid(boundaries))

# select the latitude and longitude coordinates from the centroid
lat_pt <- pt[1,2]
lon_pt <- pt[1,1]

# activate the longitude and latitude dimensions of the nc file and extract their data
lons <- src %>% activate("D2") %>% hyper_tibble()
lats <- src %>% activate("D1") %>% hyper_tibble()

# find the closest longitude and closest latitude points in the nc file to the desired lat/long coordinates
known_lon <- lons[which(abs(lons-lon_pt)==min(abs(lons-lon_pt))),]
known_lat <- lats[which(abs(lats-lat_pt)==min(abs(lats-lat_pt))),]

# obtain MACA data using the available_data function
inputs <- cft::available_data()

# select the available variables 
vars <- inputs$variable_name$`Available variable`

# this is old code that tried to do double parallel - I don't think this will ever work, but I'm leaving it here in case it is helpful
#ncores <- detectCores() - 2

#registerDoParallel(ncores)

# set the beginning index
start_indx <- 1

# create a vector of index values going from 1 to the number of indices in the list of available MACA variables, counting by 10
indices <- seq(start_indx, length(vars), by = 10)

# create an empty list to hold the MACA data
maca_data <- list()

# this is more old code that tried to do double parallel
#maca_data <- foreach(i = 1:length(indices)) %dopar% {
    #if (i < length(indices)){
        #data <- single_point_firehose(vars[indices[i]:indices[i+1]], known_lat, known_lon)  
    #}
    #else{
        #data <- single_point_firehose(vars[indices[i]:length(vars)], known_lat, known_lon)
    #}
    #list(data)
#}

# this is the sharding code
# start by iterating over the indices
for (i in 1:length(indices)){
    # if we aren't going from the last index to the end of the list of available variables, we will go between the current index and the next index, meaning 10 variables
    # at the desired lat/long using single_point_firehose
    if (i < length(indices)){
        data <- single_point_firehose(vars[indices[i]:indices[i+1]], known_lat, known_lon)  
    }
    # otherwise go from the current index to the end of the list of available variables at the desired lat/long point
    else{
        data <- single_point_firehose(vars[indices[i]:length(vars)], known_lat, known_lon)
    }
  # add the MACA data from the current iteration to the list of MACA data
    maca_data <- append(maca_data, data)
}

# print the list of MACA data
print(maca_data)