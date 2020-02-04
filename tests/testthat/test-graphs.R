
test_that("Test scatterplot with 'difference'", {
  file_refs <- cstdata(park = "Acadia National Park",
                       years = c(2004, 2005),
                       models = c("bcc-csm1-1"),
                       parameters = c("pr"),
                       scenarios = c("rcp45"),
                       local_dir = ".")
  graph_data <- cstdata::scatterplot(file_refs,
                                     var1 = "pr",
                                     var2 = "pr",
                                     agg_fun = "mean",
                                     difference = TRUE,
                                     target_period = c(2005, 2005),
                                     reference_period = c(2004, 2004),
                                     months1 = c(1, 2, 3),
                                     months2 = c(4, 5, 6),
                                     scenarios = "rcp45")
  expect_s3_class(graph_data, "data.frame")
})