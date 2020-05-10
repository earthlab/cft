dir <- "."

test_that("Test download_shapefile for file and shapefile objects", {
    url <- paste0("https://www2.census.gov/geo/tiger/TIGER2019/COUSUB/",
                  "tl_2019_44_cousub.zip")
    path <- file.path(dir, "shapefiles", "tl_2019_44_cousub",
                      "tl_2019_44_cousub.shp")
    aoi <- get_aoi(park = NA, 
                   area_name = NA,
                   shp_path = url,
                   local_dir = dir)
    expect_true(file.exists(path))
    expect_s4_class(aoi, "SpatialPolygonsDataFrame")
})


test_that("Test get_park_boundaries for file and shapefile object", {
  parkname <- "Acadia National Park"

  path <- file.path(dir, "shapefiles", "nps_boundary", "nps_boundary.shp")

  aoi <- get_park_boundaries(parkname, local_dir = dir)

  expect_true(file.exists(path))
  expect_s4_class(aoi, "SpatialPolygonsDataFrame")
})

test_that("Default NPS boundary URL is valid", {
  skip_on_ci()
  expect_false(httr::http_error(nps_boundary_url()))
})

test_that("Invalid park names raise errors", {
  expect_error(get_park_boundaries("Poodlebear National Park", local_dir = "."),
               regexp = "is not contained in the national park boundary data")
})

test_that("Invalid shapefile names raise errors", {
  expect_error(get_shapefile("poodlebear_national_park.shp", local_dir = "."),
               regexp = "Cannot read poodlebear_national_park.shp")
})
