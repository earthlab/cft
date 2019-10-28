test_that("Grid_Reference is initiated", {
  expect_equal(object = class(grid_reference)[1],
               expected = "Grid_Reference")
})


test_that("Argument_Reference is initiated", {
  expect_equal(object = class(argument_reference)[1],
               expected = "Argument_Reference")
})
