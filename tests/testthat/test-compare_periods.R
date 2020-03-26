context("compare_periods")
reticulate::use_condaenv("cst")

file_refs <- cstdata(park = "Acadia National Park",
                     years = c(2004, 2005),
                     models = c("bcc-csm1-1"),
                     parameters = c("pr", "tasmax"),
                     scenarios = c("rcp45"),
                     local_dir = getwd(), 
                     ncores = 2)

df <- cst_df(file_reference = file_refs, ncores = 2)

test_that("Test compare_periods", {
  comparison <- compare_periods(df,
                                var1 = "pr",
                                var2 = "tasmax",
                                agg_fun = "mean",
                                target_period = c(2005, 2005),
                                reference_period = c(2004, 2004),
                                scenarios = "rcp45")
  expect_s3_class(comparison, "data.frame")
})

test_that("Months can be formatted from integers", {
  month_string <- format_months(1:3)
  expect_identical(month_string, c("Jan", "Feb", "Mar"))
})

test_that("Temperature conversion succeeds", {
  test_df <- data.frame(parameter_long = "air_temperature", 
                        units = "K", 
                        value = 273.15, 
                        stringsAsFactors = FALSE)
  out_df <- convert_temperature(test_df, "value")
  expect_equal(out_df$value, 0)
  expect_equal(out_df$units, "C")
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
      reference_period = c(2004, 2004),
      scenarios = "rcp45"), 
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
      scenarios = "rcp45"), 
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
      reference_period = c(2099, 2099),
      scenarios = "rcp45"), 
    regexp = "The requested reference period is at least partially")
})

test_that("Providing a single year for target/reference period works.", {
  comparison <- compare_periods(df,
                                var1 = "pr",
                                var2 = "tasmax",
                                agg_fun = "mean",
                                target_period = 2005,
                                reference_period = 2004,
                                scenarios = "rcp45")
  expect_s3_class(comparison, "data.frame")
})

test_that("Providing invalid scenario raises errors.", {
  expect_error(
    comparison <- compare_periods(df,
                                  var1 = "pr",
                                  var2 = "tasmax",
                                  agg_fun = "mean",
                                  target_period = 2005,
                                  reference_period = 2004,
                                  scenarios = "rcp9000"),
    regexp = "The requested scenarios are not present")
})

