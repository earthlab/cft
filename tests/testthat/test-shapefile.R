test_that("Test download_shapefile for file and shapefile object", {
    # Always breaks in travis
    skip_on_travis()

    # Same url: the state of Colorado     
    url <- paste0("https://www2.census.gov/geo/tiger/TIGER2016/COUSUB/", 
                  "tl_2016_08_cousub.zip")

    # Expected Path for .shp 
    local_dir <- tempdir()
    path <- file.path(local_dir, "shapefiles", "tl_2016_08_cousub",
                      "tl_2016_08_cousub.shp")
    
    # Return area of interest
    aoi <- get_shapefile(path = url, shp_name = "tl_2016_08_cousub",
                         local_dir = local_dir)

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
  local_dir <- tempdir()
  local_path <- file.path(local_dir, "shapefiles", "nps_boundary", "nps_boundary.shp")

  # Return area of interest
  aoi <- get_park_boundaries(parkname, local_dir = local_dir)

  # Check that the local file was written
  expect_true(file.exists(local_path))
  
  # Check that the returned object is the right class
  expect_true(class(aoi) == "SpatialPolygonsDataFrame")

})


test_that("Default NPS boundary URL is valid", {
  expect_false(httr::http_error(nps_boundary_url()))
})
