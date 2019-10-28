test_that("Not providing a shapefile or park name raises an error", {
  expect_error(cstdata(shp_path = NA, national_park = NA),
               regexp = "No location data/AOI data were provided")
})

test_that("Providing both a shapefile and park name raises an error", {
  shp_path = paste0("http://www.cbrfc.noaa.gov/downloads/files/gis/",
                    "cbrfcBasinBoundary.tar.gz")
  park_name = "Yellowstone National Park"
  expect_error(cstdata(shp_path = shp_path, national_park = park_name),
               regexp = "Both a shapefile and a national park were provided.")
})

test_that("Providing a shapefile but no area name raises an error", {
  shp_path = paste0("http://www.cbrfc.noaa.gov/downloads/files/gis/",
                    "cbrfcBasinBoundary.tar.gz")
  expect_error(cstdata(shp_path = shp_path, area_name = NA),
               regexp = "Please provide the name you would like to use")
})


test_that("Provider no options for data storage raises an error", {
  expect_error(cstdata(national_park = "Yellowstone National Park",
                       store_locally = FALSE, store_remotely = FALSE),
               regexp = "Please set the store_locally and/or the")
})

# I need to add more filters to reduce the time this takes
# test_that("If park name is provided with no area name, the park name is used", {
#   expect_error(
#     cstdata(national_park = "Yellowstone National Park", area_name = NA))
# })





# filter years

# get_aoi_indexes

# get_aoi_info

# get_grouped _queries

# retrieve_subset
