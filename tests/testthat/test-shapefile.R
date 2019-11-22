test_that("Test download_shapefile for file and shapefile object", {
    # Same url: the state of Colorado
    url <- "https://www2.census.gov/geo/tiger/TIGER2016/COUSUB/tl_2016_08_cousub.zip"

    # Expected Path for .shp 
    dir <- tempdir()
    path <- file.path(dir, "shapefiles", "tl_2016_08_cousub", "tl_2016_08_cousub.shp")
    
    # Return area of interest
    aoi <- get_shapefile(path = url, 
                         shp_name = "tl_2016_08_cousub",
                         local_dir = dir)

    expect_true(file.exists(path))
    expect_s4_class(aoi, "SpatialPolygonsDataFrame")
})


test_that("Test get_park_boundaries for file and shapefile object", {
  clean_up <- function() {
    unlink(list.files(pattern = "nps_boundary", 
                      recursive = TRUE, 
                      full.names = TRUE, 
                      include.dirs = TRUE), 
           force = TRUE, recursive = TRUE) # ensure no previous files
  }
  
  parkname <- "Yellowstone National Park"
  
  dir <- "."
  clean_up()
  path <- file.path(dir, "shapefiles", "nps_boundary", "nps_boundary.shp")

  aoi <- get_park_boundaries(parkname, local_dir = dir)

  expect_true(file.exists(path))
  expect_s4_class(aoi, "SpatialPolygonsDataFrame")
  clean_up()
})


test_that("Default NPS boundary URL is valid", {
  expect_true(RCurl::url.exists(nps_boundary_url()))
})


test_that("Local shapefiles are readable", {
  aoi <- get_shapefile(path = system.file("shape/nc.shp", package="sf"))
  expect_s4_class(aoi, "SpatialPolygonsDataFrame")
})

test_that("Invalid park names raise errors", {
  expect_error(get_park_boundaries("Poodlebear National Monument"), 
               regexp = "not contained")
})
