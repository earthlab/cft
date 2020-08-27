
test_that("cft_df generates a tibble", {
  aoi <- rgdal::readOGR(system.file("extdata", "wolftrap.geojson", package = "cft"))
  file_refs <- cftdata(aoi = aoi, 
                       area_name = "wolftrap",
                       parameters = "tasmax",
                       years = c(2020, 2021), 
                       models = "CCSM4", 
                       scenarios = "rcp85")
  out <- cft_df(file_reference = file_refs)
  expect_s3_class(out, "data.frame")
})
