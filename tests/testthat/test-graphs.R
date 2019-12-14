# 
# file_df <- cstdata::cstdata(park = "Yellowstone National Park",
#                             years = c(1950, 2099),
#                             local_dir = "~/cstdata_test",
#                             ncores = 4)
# 
# graph_data <- scatterplot(file_df,
#                           var1 = "tasmax",
#                           var2 = "pr",
#                           agg_fun = "mean",
#                           difference = TRUE,
#                           target_period = c(2060, 2070),
#                           reference_period = c(1990, 2019),
#                           scenarios = c("rcp45", "rcp85")
#                           )
