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


test_that("Providing no options for data storage raises an error", {
  expect_error(cstdata(national_park = "Yellowstone National Park",
                       store_locally = FALSE, store_remotely = FALSE),
               regexp = "Please set the store_locally and/or the")
})


test_that("A full run of cstdata completes and saves an expected file", {

  # This should create one file.
  local_dir <- tempdir()
  file_refs <- cstdata(national_park = "Acadia National Park",
                       years = c(2004, 2008), models = c("bcc-csm1-1"),
                       parameters = c("pr"), scenarios = c("rcp45"),
                       local_dir = local_dir)

  # And this is the expected file name and path
  exp_file = paste0("pr_acadia_national_park_bcc-csm1-1_r1i1p1_rcp45_",
                    "macav2metdata_2004_2008_daily.nc")
  exp_path = file.path(local_dir, "acadia_national_park", exp_file)

  # So if this exists it worked
  expect_true(file.exists(exp_path))
})
