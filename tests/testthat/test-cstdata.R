test_that("get_park_boundaries provides a shapefile and spatial polygon", {
  skip_on_travis()
  download_dir <- tempdir()
  aoi <-get_park_boundaries(parkname = "Yellowstone National Park",
                            dir = download_dir)
  expected_path <- file.path(download_dir, "shapefiles", "nps_boundary.shp")
  expect_true(file.exists(expected_path))
  expect_true(class(aoi) == "SpatialPolygonsDataFrame")
})

# test_that("Test that config_aws properly stores configuration file", {
#   skip_on_travis()
#   fake_bucket <- "fake_bucket"
#   fake_key <- "a;dfhlgjhsd"
#   fake_skey <- "a;hjslkdjghsdkjgh"
#   region <- "us-west-2"
#   # how do we test user input?
# })

# filter years

# get_aoi_indexes

# get_aoi_info

# get_grouped _queries

# retrieve_subset