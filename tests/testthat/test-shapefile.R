test_that("Test download_shapefile for file and shapefile object", {
    # Always breaks in travis
    skip_on_travis()

    # Same url: the state of Colorado     
    url <- paste0("https://www2.census.gov/geo/tiger/TIGER2016/COUSUB/", 
                  "tl_2016_08_cousub.zip")

    # Expected Path for .shp 
    dir <- tempdir()
    path <- file.path(dir, "tl_2016_08_cousub", "tl_2016_08_cousub.shp")
    
    # Return area of interest
    aoi <- get_shapefile(path = url, shp_name = "tl_2016_08_cousub",
                         dir_loc = dir)

    # Check that the local file was written
    expect_true(file.exists(path))

    # Check that the returned object is the right class
    expect_true(class(aoi) == "SpatialPolygonsDataFrame")

})


test_that("Test get_park_boundaries for file and shapefile object", {
  # always breaks in travis
  skip_on_travis()
  
  # Same park: Yellowstone
  parkname <- "Yellowstone National Park"

  # Expected path
  dir <- tempdir()
  path <- file.path(dir, "nps_boundary", "nps_boundary.shp")

  # Return area of interest
  aoi <- get_park_boundaries(parkname, dir_loc = dir)

  # Check that the local file was written
  expect_true(file.exists(path))
  
  # Check that the returned object is the right class
  expect_true(class(aoi) == "SpatialPolygonsDataFrame")
  
})


test_that("Default NPS boundary URL is valid", {
  expect_true(RCurl::url.exists(nps_boundary_url()))
})
