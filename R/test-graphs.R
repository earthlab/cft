# test_that("Test graph case when no reference is provided with 'difference'", {
#   # We will need to share the outputs of a call for each test.
#   file_df <- cstdata::cstdata(park = "Yellowstone National Park",
#                               years = c(1950, 2099),
#                               local_dir = "~/cstdata_test",
#                               ncores = 4)
# 
#   # We should see a friendly reminder to provide a reference period.
#   expect_error(
#     scatterplot(file_df = list(), var1 = "tasmax", var2 = "pr",
#                 agg_fun = "mean", difference = TRUE,
#                 target_period = c(2080, 2099), reference_period = NA),
#     regexp = "The argument 'difference' was specified with no reference")
# })