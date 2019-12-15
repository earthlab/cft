# sample output with chart studio: https://chart-studio.plot.ly/~Travissius/49/#/

convert_KC <- function(file_df, column) {
  # Locate temperature rows
  temp_rows <- grep("air_temperature",file_df$internal)
  
  # Get the values in Kelvin
  k <- unlist(file_df[[column]][temp_rows])

  # Convert to Celisus
  c <- k - 273.15

  # Reassign new values to graph_data
  file_df[[column]][temp_rows] <- c

  # Reassign C to graph units
  file_df$units[temp_rows] <- "C"

  return(file_df)
}

extract <- function(row, time_period, fun, day1, day2) {

  # Which is which
  file <- unlist(row$local_path)
  variable <- unname(unlist(row$internal))

  # Open File
  nc <- ncdf4::nc_open(file)

  # Get the matrix of values
  subset <- ncdf4::ncvar_get(nc, variable)

  # Get the matrix slices associated with the dates requested
  subset <- subset[day1: day2]
  
  # Apply the aggregation function
  value <- fun(subset, na.rm=TRUE)

  return(value)
}

get_fun <- function(file_df, time_period, agg_fun) {

  # Check that these files contain data within the earliest reference year
  year1 <- min(as.numeric(file_df$year1))
  year2 <- min(as.numeric(file_df$year2))
  if (year1 > time_period[[1]] | year2 < time_period[[2]]) {
    msg <- paste0("The data provided does not contain the years requested.")
    stop(msg)
  }

  # Match the aggregation function string to a function 
  tryCatch({
    fun = match.fun(agg_fun)
  }, error = function(e) {
    stop(paste0("The aggregation function '", agg_fun,
                "' is not available."))
  })

  # Which days go with the years given
  base = as.Date("1950-01-01")
  date1 = as.Date(paste0(as.character(time_period[[1]]), "-01-01"))
  date2 = as.Date(paste0(as.character(time_period[[2]]), "-12-31"))
  day1 = as.numeric(date1 - base)
  day2 = as.numeric(date2 - base)


  # Extract values
  tryCatch({
    ##### Attempt One #####
    # # Setup parallelization
    # if (identical(ncores, NA)) ncores <- get_ncores()
    # pbapply::pboptions(use_lb = FALSE)
    # cl <- parallel::makeCluster(ncores - 1)
    # doParallel::registerDoParallel(cl)
    # parallel::clusterExport(cl, c("extract"))
    # parallel::clusterEvalQ(cl, {library(ncdf4)})
    # 
    # # This is returning NAs with the clusters, must be ncdf4 interface
    # file_df$agg_value <- pbapply::pbapply(file_df,
    #                                       MARGIN = 1,
    #                                       FUN = extract,
    #                                       time_period = time_period,
    #                                       fun = fun,
    #                                       day1 = day1,
    #                                       day2 = day2)

    ##### Attempt 2 parallel works 2X faster #####
    cl <- parallel::makeCluster(get_ncores())
    doSNOW::registerDoSNOW(cl)
    `%dopar%` <- foreach::`%dopar%`
    pb <- progress::progress_bar$new(
      format = "[:bar] |:percent ~:eta",
      total = nrow(file_df),
      complete = "+",
      incomplete = " ",
      current = " ",
      width = 60)

    progress_bar <- function(n) pb$tick()
    opts <- list(progress = progress_bar)
    values <- foreach::foreach(i = 1: nrow(file_df),
                               .options.snow = opts) %dopar% {
      # Which is which
      row <- file_df[i, ]
      file <- unlist(row$local_path)
      variable <- unname(unlist(row$internal))
      
      # Open File
      nc <- ncdf4::nc_open(file)

      # Get the matrix of values
      subset <- ncdf4::ncvar_get(nc, variable)
  
      # Get the matrix slices associated with the dates requested
      subset <- subset[, , day1: day2]

      # Apply the aggregation function
      value <- fun(subset, na.rm=TRUE)
      
      return(value)
                               }
    pb$terminate()

  }, error = function(e) {
     parallel::stopCluster(cl) 
     stop(e)
  })

  # Close cluster
  parallel::stopCluster(cl)

  # Return these values
  return(values)
}


#' Scatterplot
#' @param file_df Description coming soon!
#' @param var1 Description coming soon!
#' @param var2 Description coming soon!
#' @param agg_fun Description coming soon!
#' @param difference Description coming soon!
#' @param target_period Description coming soon!
#' @param reference_period Description coming soon!
#' @param scenarios Description coming soon!
#' @export
scatterplot <- function(file_df,
                        var1 = "tasmax",
                        var2 = "pr",
                        agg_fun = "mean",
                        difference = TRUE,
                        target_period = c(2080, 2099),
                        reference_period = c(1990, 2019),
                        scenarios = c("rcp45", "rcp85")  # <------------------ They will be able to specify any grouping variable, and the function will plot accordingly. At least that's the plan.
                        ) {
  '
  var1 = "tasmax"
  var2 = "pr"
  agg_fun = "mean"
  difference = TRUE
  reference_period = c(1950, 2010)
  target_period = c(2080, 2099)
  '
  # Sample set of model outputs  <--------------------------------------------- It takes a minute to get these out. Still some trouble with open node connections.
  if (identical(file_df, NA)) {
    # Just raise error here, but for developing let's get one
    file_df <- cstdata::cstdata(park = "Yellowstone National Park",
                                years = c(1950, 2099),
                                local_dir = "~/cstdata_test",
                                ncores = 4)  # <------------------------------- Change this
  }

  if (difference & identical(reference_period, NA)) {
    msg <- paste("The argument 'difference' was specified with no reference", 
                 "time period. Please provide a time period with which to",
                 "compare the target period.")
    stop(msg)
  }

  # Arg Reference
  ref <- cstdata::argument_reference

  # Variable reference data frame
  var_ref <- ref$variables

  # Add the internal variable name (could just do this in cstdata)
  file_df$internal <- lapply(file_df$parameter, FUN = function(x) var_ref[x])
  
  # Filter out the other variables
  file_df <- file_df[file_df$parameter == var1 | file_df$parameter == var2,]
  rownames(file_df) <- NULL

  # Check that each file list is longer than 0
  if (nrow(file_df) == 0) {
    stop("There are no stored files that contain the information requested.")
  }

  # Get the aggregating function value for each file within the target period
  print("Calculating Target Period Values")
  file_df$agg_value <- get_fun(file_df, target_period, agg_fun)
  file_df <- convert_KC(file_df, "agg_value")

  # If a difference is required
  if (difference) {

    # Get the aggregating function value for the reference period
    print("Calculating Reference Period Values")
    file_df$ref_agg_value <- get_fun(file_df, reference_period, agg_fun)
    file_df <- convert_KC(file_df, "ref_agg_value")
    
    # And get the difference between the future and now
    agg_value <- as.numeric(file_df$agg_value)
    ref_agg_value <- as.numeric(file_df$ref_agg_value)
    file_df$difference <- agg_value - ref_agg_value

    # And set the difference to the "graph_data" column
    file_df$graph_data <- file_df$difference

    # Add in file labels
    file_df$label <- paste0("Difference in ", file_df$full_var_name, " (",
                            file_df$units, ")")

  } else {
    # Set graph value and label
    file_df$graph_data <- file_df$agg_value
    file_df$label <- paste0(file_df$full_var_name, file_df$units)
  }

  
  
  # Difference in averages
  # 1 get singular mean for historical refrence period
  # 2 get the same for the target period
  # 3 just take the difference
  # 4 plot that
  # title: Place, diff in mean value between historical period and target period
  # Leave variable on axes
  # Convert from K to C
  # ...
  # Axes text: diff in "x" in "unit"
  # Color by RCP

  
  # Now split the data up into separate data frame
  cols <- c("graph_data", "model", "rcp")
  var1_df <- file_df[file_df$parameter == var1, cols]
  names(var1_df) <- c("var1", "model", "rcp")
  var2_df <- file_df[file_df$parameter == var2, cols]
  names(var2_df) <- c("var2", "model", "rcp")

  # And join based on shared rcps and models
  graph_df <- merge(var1_df, var2_df, by=c("model", "rcp"))
  graph_df <- as.data.frame(lapply(graph_df, unlist))
  graph_df$rcp <- factor(graph_df$rcp)
  
  # Title and labels
  location <- tools::toTitleCase(gsub("_", " ", unique(file_df$area_name)))
  if (difference) {
    title = paste0(location,
                   "\nDifference in Mean Values: ",
                   reference_period[[1]], "-", reference_period[[2]],
                   " vs ",
                   target_period[[1]], "-", target_period[[2]])
  }
  xlabel <- unique(file_df$label[file_df$parameter == var1])
  ylabel <- unique(file_df$label[file_df$parameter == var2])
  
  # Create the basic plot
  `%>%` <- dplyr::`%>%`
  graph <- plotly::plot_ly(graph_df,
                           x = ~var1,
                           y = ~var2,
                           color = ~rcp,
                           colors = c("blue", "orange"),
                           mode = "markers",
                           size = 5,
                           text = ~model,
                           type = "scatter",
                           showlegend = FALSE)

  # Make it show the chart studio button
  
  # Graph the plot with this layout
  graph <- graph %>% plotly::layout(
    # autosize = TRUE,
    xaxis = list(title = xlabel),
    yaxis = list(title = ylabel),
    legend = list(label = xlabel),
    margin = list(t = 125),
    title = list(
      y = 1.65,
      text = title,
      pad = list(
        t = 150
      ),
      font = list(
        family = "Times New Roman",
        fontface = "bold",
        size = 28
        )
      )
    ) %>% plotly::config(showEditInChartStudio = TRUE,
                         displaylogo = FALSE)

  print(graph)
  names(graph_df) <- c("model", "rcp", var1, var2)
  return(graph_df)
}
