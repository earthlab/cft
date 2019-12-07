test_that("Missing AWS credentials raises an error", {
  Sys.setenv(AWS_ACCESS_KEY_ID = "", AWS_SECRET_ACCESS_KEY = "")
  expect_error(verify_aws_credentials(), 
               regexp = "you must provide your credentials")
})