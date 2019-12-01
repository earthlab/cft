# Just a sample to practice this

scatter <- function(var1 = "tasmax", var2 = "precipitation") {
  # Sample set of model outputs
  file_df <- cstdata::cstdata(park = "Acadia National Park",
                              years = c(2050, 2055),
                              local_dir = "~/cstdata_test",
                              ncores = 4)

  # Arg Reference
  ref <- cstdata::argument_reference

  # Variable reference data frame
  var_df <- ref$variables

  # Get the file pattern associated with each variable
  if (!var1 %in% names(var_df)) {
    key1 <- names(vardf)[var_df == var1]
  } else {
    key1 <- var1
    var1 <- var_df[[key1]]
  }
  if (!var2 %in% names(var_df)) {
    key2 <- names(vardf)[var_df == var2]
  } else {
    key2 <- var2
    var2 <- vardf[[key2]]
  }

  # Get the files for each variable  <----------------------------------------- We need a better way
  pattern1 = paste0("/", key1, "_")
  pattern2 = paste0("/", key2, "_")
  var1_files <- file_df$local_path[grep(pattern1, file_df$local_path)]
  var2_files <- file_df$local_path[grep(pattern2, file_df$local_path)]
  var1_files <- as.character(var1_files)
  var2_files <- as.character(var2_files)

  # Open each data set in this folder
  var1_ncs <- lapply(var1_files, FUN = ncdf4::nc_open)
  var2_ncs <- lapply(var2_files, FUN = ncdf4::nc_open)
  
  # And we would scatter the mean max temperature with the mean precipitation < Left off here....
  var1_means <- c()
  for ( nc in var1_ncs ) {
    model <- ncdf4::ncatt_get(nc, 0)$model
    v1_mean <-  mean(ncdf4::ncvar_get(nc, var1), na.rm=TRUE)
    var1_means[model] <- v1_mean
  }
  var2_means <- c()
  for ( nc in var2_ncs ) {
    model <- ncdf4::ncatt_get(nc, 0)$model
    v2_mean <-  mean(ncdf4::ncvar_get(nc, var2), na.rm=TRUE)
    var2_means[model] <- v2_mean
  }

  # Create a data frame out of this
  df <- dplyr::tibble(models = names(var1_means),
                      var1 = var1_means,
                      var2 = var2_means)

  # Title and labels
  xlabel <- tools::toTitleCase(gsub("_", " ", var1))
  ylabel <- tools::toTitleCase(gsub("_", " ", var2))

  # Create the basic plot
  graph <- plotly::plot_ly(df,
                           x = ~var1,
                           y = ~var2,
                           text = ~models,
                           mode = "markers",
                           type = "scatter",
                           size = 5,
                           # color = ~var1,
                           marker = list(width=0),
                           showlegend = FALSE
                           )
  graph %>% plotly::layout(
    # autosize = TRUE,
    xaxis = list(title = xlabel),
    yaxis = list(title = ylabel),
    legend = list(label = xlabel),
    margin = list(t = 110),
    title = list(
      y = 1.45,
      text = "<b>Sample Scatterplot</b>",
      pad = list(
        t = 140
      ),
      font = list(
        family = "Times New Roman",
        fontface = "bold",
        size = 28
        )
      )
    )

}

# Okay, so the plan is to gather the data using cstdata. That will give us
# a file reference object. We can feed that reference object into a generator
# object...or we could just have cstdata return this generator object.
# This object will store all of the pointers to the files on disk along with
# attributes and reference objects that might be useful (e.g., full model
# institution names). The attributes will store the season, time periods,
# variables, models, color, size, graph type, etc, that will be used when it is
# called to generate a plot:

# Example:
# data_gen <- cstdata(...)
# data_gen$graph_type = "scatter"
# data_gen$yvar = "tasmax"
# data_gen$xvar = "pr"
# data_gen$title = "Sample Scatterplot"
# data_gen$plot()

# And we'll get a plot! Without having to start over, we could change a few
# attributes and get a different plot.