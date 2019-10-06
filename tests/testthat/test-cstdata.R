test_that("get_park_boundaries provides a shapefile", {
  skip_on_travis()
  download_dir <- tempdir()
  aoi <-get_park_boundaries(parkname = "Yellowstone National Park",
                            dir = download_dir)
  expected_path <- file.path(download_dir, "shapefiles", "nps_boundary.shp")
  expect_true(file.exists(expected_path))
  expect_true(class(aoi) == "SpatialPolygonsDataFrame")
})
