# Let's not download the same file everytime
local_dir <- "."

test_that("Test that filter years returns correct start and end days", {
  days <- filter_years(start_year = 1977, end_year = 1981,
                       available_start = 1950, available_end = 2099)
  expect_true(days[1] == 9862)
  expect_true(days[length(days)] == 11687)
})

test_that("Test that incorrect inputs in 'filter_years' returns errors", {
  # Start year is after end year
  expect_error(filter_years(start_year = 1981, end_year = 1977,
                            available_start = 1950, available_end = 2099))

  # Start year is before available start year
  expect_error(filter_years(start_year = 1949, end_year = 1981,
                            available_start = 1950, available_end = 2099))

  # End year is after available end year
  expect_error(filter_years(start_year = 1977, end_year = 2100,
                            available_start = 1950, available_end = 2099))

})

test_that("Test that 'get_aoi_indexes' returns correct spatial indices", {
  area_name <- "Acadia National Park"
  aoi <- get_park_boundaries(area_name, local_dir = local_dir)
  grid_ref <- Grid_Reference()
  indices <- get_aoi_indexes(aoi, grid_ref)

  # There is a slight possibility that the park boundary extent will change
  exp_indices <- list("y1" = 455, "y2" = 465, "x1" = 1347, "x2" = 1362)
  expect_true(all(unlist(indices) == unlist(exp_indices)))
  
})


test_that("Test that 'get_queries' returns expected paths", {

  # Sample arguments
  area_name <- "Acadia National Park"
  aoi <- get_park_boundaries(area_name, local_dir = local_dir)
  area_name <- gsub(" ", "_", tolower(area_name))
  years <- c(2000, 2001)
  models <- "bcc-csm1-1"
  parameters <- "pr"
  scenarios <-"rcp45"
  arg_ref <- Argument_Reference()
  grid_ref <- Grid_Reference()

  # Get a query object
  queries <- get_queries(aoi, area_name, years, models, parameters,
                         scenarios, arg_ref, grid_ref)

  # Unlist this object
  queries <- unlist(queries)

  # Expected returns (without subsetting in case of border changes)
  url1 <- paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/",
                 "agg_macav2metdata_pr_bcc-csm1-1_r1i1p1_historical_1950_2005",
                 "_CONUS_daily.nc?precipitation")
  url2 <- paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/",
                 "agg_macav2metdata_pr_bcc-csm1-1_r1i1p1_rcp45_2006_2099",
                 "_CONUS_daily.nc?precipitation")
  ncfile <- paste0("pr_acadia_national_park_bcc-csm1-1_r1i1p1_rcp45_",
                   "macav2metdata_2000_2001_daily.nc")

  # The first object should contain the expected historical url
  expect_true(grepl(url1, queries[1], fixed = TRUE))

  # The second object should contain the expected modeled url
  expect_true(grepl(url2, queries[2], fixed = TRUE))

  # The third object should contain a .nc file
  expect_true(queries[3] == ncfile)
})

test_that("Test get_aoi_info", {
  grid_ref <- Grid_Reference()
  aoi <- get_park_boundaries("Acadia National Park", local_dir = local_dir)

  aoi_info <- get_aoi_info(aoi, grid_ref)
  testthat::expect_named(aoi_info, 
                         c("aoilats", "aoilons", "mask_matrix", "resolution"))
})


test_that("Test retrieve_subset", {

  # Sample arguments
  url1 <- paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/",
                "agg_macav2metdata_pr_bcc-csm1-1_r1i1p1_historical_1950_2005",
                "_CONUS_daily.nc?precipitation[0:1:20453][455:1:465]",
                "[1347:1:1362]#fillmismatch")
  url2 <- paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/",
                "agg_macav2metdata_pr_bcc-csm1-1_r1i1p1_historical_1950_2005",
                "_CONUS_daily.nc?precipitation[0:1:20453][455:1:465]",
                "[1347:1:1362]#fillmismatch")
  filename <- paste0("pr_acadia_national_park_bcc-csm1-1_r1i1p1_rcp45_",
                    "macav2metdata_2000_2001_daily.nc")
  area_name <- "acadia_national_park"
  elements <- c(model = "bcc-csm1-1", rcp = "rcp45", ensemble = "r1i1p1",
                "year1" = 2000, "year2" = 2001, internal_varname = "precipitation")
  query <- list(c(url1, url2), filename, elements)
  years <- c(2000, 2001)
  grid_ref <- Grid_Reference()
  aoi <- get_park_boundaries("Acadia National Park", local_dir = local_dir)
  aoi_info <- get_aoi_info(aoi, grid_ref)

  subset <- retrieve_subset(query, years, aoi_info, area_name, 
                            local_dir = local_dir)

  expect_true(file.exists(subset$local_path))
  expect_true(grepl("\\.nc$", subset$local_file))
})
