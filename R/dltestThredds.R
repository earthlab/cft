#' brief attempt to simplify opendap subsetting

library(RNetCDF)
library(thredds)
library(tidync)
library(tidyverse)

# Function parameters
location = "Wind Cave National Park"
method = "maca"
param = "tmin"
model = "CCSM4"
scenario = "rcp45"
startDate = "1950-01-01"
endDate =  "2099-12-31"
timeRes = "daily"
year_range = 4
plotsample = FALSE



# Park Shapefile for area of interest
if (!exists("parks")){
  parks <- rgdal::readOGR("data/shapefiles/nps_boundary.shp",
                          verbose = FALSE)
}
aoi <- parks[grepl(tolower(location), tolower(parks$UNIT_NAME)),]  # <----------------There will cometimes be two (e.g. "X park" and "X preserve") so fix that
bb <- aoi@bbox

# Can I get a pointer to a multifile dataset (a la xarray.open_mfdataset())
# test <- RNetCDF::open.nc(urls)
# test <- tidync(urls[1])
# No (at least not with windows)

# List of urls for a sample set of parameters
urls <- read.table('data/macav2metdata_urls.txt')
urls <- lapply(urls, as.character)[[1]]
urls <- urls[c(grep('historical', urls), grep(scenario, urls))]
urls <- unlist(lapply(urls, function(x) gsub('fileServer', 'dodsC', x)))

# Experimenting with this THREDDS package
bbox = c()
bbox[['xmin']] = bb[1,1]
bbox[['xmax']] = bb[1,2]
bbox[['ymin']] = bb[2,1]
bbox[['ymax']] = bb[2,2]
tds_ncss_download(ncss_url = urls[1], vars = 'specific_humidity', bbox = bbox, overwrite = TRUE, out_file = "data/thredds.nc")


# Now get lat/lon index positions of bounding box
# Get bounding box of aoi
minlon = bb[1, 1] + 360
maxlon = bb[1, 2] + 360
minlat = bb[2, 1]
maxlat = bb[2, 2]

# Get lons and lats of dataset
# lons = huss.lon.values
# lats = huss.lat.values
# lonbounds = lons[(lons <= maxlon) & (lons > minlon)]
# latbounds = lats[(lats <= maxlat) & (lats > minlat)]
# lonidx1 = np.where(lons == np.min(lonbounds))[0][0]
# lonidx2 = np.where(lons == np.max(lonbounds))[0][0]
# latidx1 = np.where(lats == np.min(latbounds))[0][0]
# latidx2 = np.where(lats == np.max(latbounds))[0][0]
