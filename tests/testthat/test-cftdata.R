
test_that("Not providing a shapefile or park name raises an error", {
  expect_error(cftdata(),
               regexp = "No location data/AOI data were provided")
})

test_that("Providing both a shapefile and park name raises an error", {
  shp_path <- paste0("http://www.cbrfc.noaa.gov/downloads/files/gis/",
                     "cbrfcBasinBoundary.tar.gz")
  expect_error(cftdata(shp_path = shp_path, park = "Yellowstone National Park"),
               regexp = "Both a shapefile and a national park were provided.")
})

test_that("Providing a shapefile but no area name raises an error", {
  shp_path <- paste0("http://www.cbrfc.noaa.gov/downloads/files/gis/",
                     "cbrfcBasinBoundary.tar.gz")
  expect_error(cftdata(shp_path = shp_path),
               regexp = "Please provide the name you would like to use")
})

test_that("A full run of cftdata completes and saves an expected file", {
  local_dir <- "."
  file_refs <- cftdata(park = "Acadia National Park",
                       years = c(2004, 2005),
                       models = "bcc-csm1-1",
                       parameters = "pr",
                       scenarios = "rcp45",
                       local_dir = local_dir, 
                       ncores = 2)

  expected_file <- paste0("pr_acadia_national_park_bcc-csm1-1_r1i1p1_rcp45_",
                          "macav2metdata_2004_2005_daily.nc")
  expected_path <- file.path(local_dir, "acadia_national_park", expected_file)

  expect_true(file.exists(expected_path))
})

test_that("A cftdata run on a one pixel park completes successfully", {
  local_dir <- "."
  file_refs <- cftdata(park = "Wolf Trap National Park for the Performing Arts",
                       years = c(2004, 2005),
                       models = "bcc-csm1-1",
                       parameters = "pr",
                       scenarios = "rcp45",
                       local_dir = local_dir, 
                       ncores = 2)
  expected_file <- paste0("pr_wolf_trap_national_park_for_the_performing_arts_",
                          "bcc-csm1-1_r1i1p1_rcp45_macav2metdata_2004_2005_daily.nc")
  expected_path <- file.path(local_dir, "wolf_trap_national_park_for_the_performing_arts",
                             expected_file)
  
  expect_true(file.exists(expected_path))
})
