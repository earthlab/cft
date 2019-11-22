test_that("Test get_queries", {
  local_dir <- tempdir()
  aoi <- get_park_boundaries("Acadia National Park", local_dir = local_dir)
  years <- c(2000, 2001)
  models <- "bcc-csm1-1"
  parameters <- "pr"
  scenarios <-"rcp45"
  arg_ref <- Argument_Reference()
  grid_ref <- Grid_Reference()

  queries <- get_queries(aoi, local_dir, years, models, parameters,
                         scenarios, arg_ref, grid_ref)
  
  expect_true(class(queries) == "list")
  # queries should contain a path to one nc file
  expect_true(1 == sum(grepl("\\.nc$", unlist(queries))))
})


test_that("Test get_aoi_info", {
  grid_ref <- Grid_Reference()
  aoi <- get_park_boundaries("Acadia National Park")

  aoi_info <- get_aoi_info(aoi, grid_ref)
  testthat::expect_named(aoi_info, 
                         c("aoilats", "aoilons", "mask_matrix", "resolution"))
})


test_that("Test retrieve_subset", {
  url1 = paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/",
                "agg_macav2metdata_pr_bcc-csm1-1_r1i1p1_historical_1950_2005",
                "_CONUS_daily.nc?precipitation[0:1:20453][455:1:465]",
                "[1347:1:1362]#fillmismatch")
  url2 = paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/",
                "agg_macav2metdata_pr_bcc-csm1-1_r1i1p1_historical_1950_2005",
                "_CONUS_daily.nc?precipitation[0:1:20453][455:1:465]",
                "[1347:1:1362]#fillmismatch")
  filename = paste0("pr_acadia_national_park_bcc-csm1-1_r1i1p1_rcp45_",
                    "macav2metdata_2000_2001_daily.nc")
  query = list(c(url1, url2), filename)

  local_dir = tempdir()
  years <- c(2000, 2001)
  grid_ref <- Grid_Reference()
  aoi <- get_park_boundaries("Acadia National Park", local_dir = local_dir)
  aoi_info <- get_aoi_info(aoi, grid_ref)
  aws_creds <- NA
  store_locally <- TRUE
  store_remotely <- FALSE

  subset <- retrieve_subset(query, years, aoi_info, local_dir, aws_creds,
                            store_locally, store_remotely)
  expect_true(file.exists(subset$local_path))
  expect_true(grepl("\\.nc$", subset$local_file))
})