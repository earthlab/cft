test_that("Test that get_ncores uses 2 cores when it should", {
  Sys.setenv(`_R_CHECK_LIMIT_CORES_`="TRUE")
  cores <- get_ncores()
  expect_equal(cores, 2L)
  Sys.setenv(`_R_CHECK_LIMIT_CORES_`="")
})

test_that("Test that get_ncores uses half of available cores", {
  cores <- get_ncores()
  expect_equal(cores, parallel::detectCores() / 2)
})
