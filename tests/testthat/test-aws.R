test_that("Test that the 'ask' function is storing inputs", {
  # Write the answer to a file and ask it for input.
  f = file()

  # Set the package connection to this file
  options(cstdata.connection = f)

  # Write answer in advance
  write("Blue...no!", f)
  color <- ask("What is your favorite color?")

  # Test for the expected output
  testthat::expect_true(color == "Blue...no!")

  # Reset the connection source
  options(cstdata.connection = stdin())
  close(f)
})

test_that("Test that 'config_aws' properly stores user inputs", {
  # Create target file path
  config_path <- file.path(tempdir(), "cstdata_test_config.RDS")

  # Create a file to store inputs and set the package connection to it
  f = file()
  options(cstdata.connection = f)

  # Write sample inputs in advance
  write("a bucket\na key\nan skey\nus-west-2", f)

  # Get the configuration object
  config <- config_aws(aws_config_path = config_path)

  # These are our expected keys and values
  expkeys <- c("bucket", "key", "skey", "region")
  expvalues <- c("a bucket", "a key", "an skey", "us-west-2")

  # Check that the file was written and check keys and sample values
  expect_true(file.exists(config_path))
  expect_true(all(names(config) == expkeys))

  # Delete the config file, close the input file, and reset the connection
  unlink(config_path)
  options(cstdata.connection = stdin())
  close(f)
})

test_that("config_aws creates dirs", {
  path <- file.path(tempdir(), "aws-creds", "out.rds")
  config_aws(path)
  expect_true(file.exists(dirname(path)))
})