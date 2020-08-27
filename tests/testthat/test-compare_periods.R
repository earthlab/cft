context("compare_periods")

aoi <- rgdal::readOGR(system.file("extdata", "wolftrap.geojson", package = "cft"))
file_refs <- cftdata(aoi = aoi, 
                     area_name = "wolftrap",
                     parameters = c("tasmax", "pr"),
                     years = c(2005, 2007), 
                     models = "CCSM4", 
                     scenarios = "rcp85")

df <- cft_df(file_reference = file_refs, ncores = 2)

test_that("Test compare_periods", {
  comparison <- compare_periods(df,
                                var1 = "pr",
                                var2 = "tasmax",
                                agg_fun = "mean",
                                target_period = c(2005, 2006),
                                reference_period = c(2007, 2007),
                                scenarios = "rcp85")
  expect_s3_class(comparison, "data.frame")
})

test_that("Months can be formatted from integers", {
  month_string <- format_months(1:3)
  expect_identical(month_string, c("Jan", "Feb", "Mar"))
})

test_that("Invalid filter params raise errors", {
  expect_error(compare_periods(df, var1 = "foo", var2 = "bar"), 
               regexp = "The requested variables are not present")
})

test_that("Invalid aggregation functions raise errors", {
  expect_error(
    compare_periods(
      df,
      var1 = "pr",
      var2 = "tasmax",
      agg_fun = "bammallammadingo",
      target_period = c(2005, 2005),
      reference_period = c(2006, 2006),
      scenarios = "rcp85"), 
    regexp = "is not available"
  )
})

test_that("Invalid target year ranges raise errors.", {
  expect_error(
    compare_periods(
      df,
      var1 = "pr",
      var2 = "tasmax",
      agg_fun = "mean",
      target_period = c(2222, 2223),
      reference_period = c(2004, 2004),
      scenarios = "rcp85"), 
    regexp = "The requested target period is at least partially")
})

test_that("Invalid reference year ranges raise errors.", {
  expect_error(
    compare_periods(
      df,
      var1 = "pr",
      var2 = "tasmax",
      agg_fun = "mean",
      target_period = c(2005, 2005),
      reference_period = c(1100, 1100),
      scenarios = "rcp85"), 
    regexp = "The requested reference period is at least partially")
})

test_that("Providing a single year for target period works.", {
  comparison <- compare_periods(df,
                                var1 = "pr",
                                var2 = "tasmax",
                                agg_fun = "mean",
                                target_period = 2007,
                                reference_period = 2005:2006,
                                scenarios = "rcp85")
  expect_s3_class(comparison, "data.frame")
})

test_that("Providing a single year for reference period works.", {
  comparison <- compare_periods(df,
                                var1 = "pr",
                                var2 = "tasmax",
                                agg_fun = "mean",
                                reference_period = 2007,
                                target_period = 2005:2006,
                                scenarios = "rcp85")
  expect_s3_class(comparison, "data.frame")
})

test_that("Providing invalid scenario raises errors.", {
  expect_error(
    comparison <- compare_periods(df,
                                  var1 = "pr",
                                  var2 = "tasmax",
                                  agg_fun = "mean",
                                  target_period = 2005,
                                  reference_period = 2006,
                                  scenarios = "rcp9000"),
    regexp = "The requested scenarios are not present")
})

test_that("format_months return expected error", {
  expect_error(
    format_months("may"), regexp = "must be numeric"
  )
})