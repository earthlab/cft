# 
# test_that("Test get_park_boundaries for file and shapefile object", {
#   parkname <- "Yellowstone National Park"
# 
#   dir <- "."
#   path <- file.path(dir, "shapefiles", "nps_boundary", "nps_boundary.shp")
# 
#   aoi <- get_park_boundaries(parkname, local_dir = dir)
# 
#   expect_true(file.exists(path))
#   expect_s4_class(aoi, "SpatialPolygonsDataFrame")
# })
# 
# test_that("Default NPS boundary URL is valid", {
#   skip_on_ci()
#   expect_false(httr::http_error(nps_boundary_url()))
# })
# 
# test_that("Invalid park names raise errors", {
#   expect_error(get_park_boundaries("Poodlebear National Park", local_dir = "."),
#                regexp = "is not contained in the national park boundary data")
# })
