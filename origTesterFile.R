library(rgdal)

path <- "/Users/ty/Documents/Github/cft/data/"
shapefile_name <- "SGI_exterior_boundary" #"test_shapefile" 

# Read in the area of interest
aoi <- readOGR(dsn=paste(path,shapefile_name,sep=""), 
               layer=shapefile_name)
crs(aoi) <- "+init=EPSG:4326"

library(cft)
library(osmdata)
library(hddtools)
library(sf)
bb <- getbb("boulder,co",  format_out = "sf_polygon")

aoi <- as_Spatial(geocode_OSM("Boulder,CO",  as.sf = TRUE), cast = TRUE)
library(tmaptools)





d <- cftdata(aoi = aoi, area_name = "test", parameters = "pr",
             years = c(2050, 2050), models = "bcc-csm1-1", scenarios = "rcp45",
             local_dir = path, ncores=parallel::detectCores())

# align CRS
crs(aoi) <- "+init=EPSG:4326"
crs(raster(d$local_path))
aoi_reproject <- spTransform(aoi,crs(raster(d$local_path))) #sp::CRS("+init=EPSG:4326"))
crs(aoi_reproject)

# plot to confirm
pdf(paste(path,"cft_alignment_problem2.pdf",sep=""), height=8.5, width=11)
plot(raster(d$local_path), col=terrain.colors(100))
plot(aoi_reproject, add=TRUE)
dev.off()





st_crs(aoi)
st_crs(grid_ref)

grid_ref











