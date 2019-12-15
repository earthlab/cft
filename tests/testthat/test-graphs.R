# # Could we share a file_df from a single run across all tests?
# test_that("Test scatterplot with 'difference'", {
#   # Not this one, use the one from test-cstdata?
#   file_df <- cstdata::cstdata(park = "Yellowstone National Park",
#                               years = c(1950, 2099),
#                               local_dir = "~/cstdata_test",
#                               ncores = 4)
# 
#   # This returns a data frame of the values and makes a graph in the viewer.
#   graph_data <- cstdata::scatterplot(file_df,
#                                      var1 = "tasmax",
#                                      var2 = "pr",
#                                      agg_fun = "mean",
#                                      difference = TRUE,
#                                      target_period = c(2060, 2070),
#                                      reference_period = c(1990, 2019),
#                                      scenarios = c("rcp45", "rcp85"))
# 
#   # Okay, what do we expect in computer terms??
#   expect_true()
# 
# })