test_that("Test that config_aws properly stores configuration file", {
  skip_on_travis()
  fake_bucket <- "fake_bucket"
  fake_key <- "a;dfhlgjhsd"
  fake_skey <- "a;hjslkdjghsdkjgh"
  region <- "us-west-2"
  
  # Strategies:
  # https://stackoverflow.com/questions/41372146/test-interaction-with-users-in-r-package
  # https://rdrr.io/cran/readr/src/tests/testthat/test-read-lines.R
  # https://stackoverflow.com/questions/38461391/avoid-pauses-due-to-readline-while-testing
})


test_that("Test that config_aws writes a file with expected keys", {

  # Start test mode
  # original_test_mode <- getOption('cstdata.test_mode')
  options('cstdata.test_mode' = TRUE)

  # options('cstdata.test_mode')
  
  # What happens?
  config_aws()

  # Now we can check output?
  config <- readRDS("~/.aws/cstdata_config.RDS")

  # Expected keys
  expkeys <- c("bucket", "key", "skey", "region")

  # Undo test mode
  options('my_package.test_mode' = FALSE)

  # Delete the config file
  unlink("~/.aws/cstdata_config.RDS")
  
  # Expect keys, testing for aws access would be problematic
  expect_true(all(names(config) == expkeys))


})


