test_that("Not providing a shapefile or park name raises an error", {
  expect_error(cstdata(shp_path = NA, national_park = NA),
               regexp = "No location data/AOI data were provided")
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