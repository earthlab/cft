# sample output with chart studio: https://chart-studio.plot.ly/~Travissius/51/#/
#' Climate Scenarios Toolkit Scatterplot
#' 
#' Renders a scatterplot between two model variables from an assortment of
#' General Circulation Models. This function uses an user-specified function to
#' aggregate model outputs within a specified area and time period. Users may
#' also choose to return differences between the results of that aggregating
#' function between two time periods. Uses the output of `cstdata` as a
#' reference to local or remote files and meta data. 
#' 
#' @param file_df A file reference data frame output from a `cstdata` run.
#' (data frame)
#' @param agg_fun The aggregating function to applied to the model variables
#' (e.g., "mean"). (character)
#' @param var1 The x-axis variable, see cstdata::argument_reference
#' (e.g., "pr"). (character).
#' @param var2 The y-axis variable, see cstdata::argument_reference
#' (e.g., "tasmax").  character)
#' @param months1 The months of the year with which to include in the
#' aggregation of variable 1 (e.g., c("aug", "sep", "oct")). (vector)
#' @param months2 The months of the year with which to include in the
#' aggregation of variable 2 (e.g., c("apr", "may", "jun")). (vector)
#' @param target_period The time period within which to apply the aggreating
#' value function (e.g., c(2050, 2075)). (vector)
#' @param reference_period The time period with which to compare the target
#' period (e.g., c(1980, 2010)). (vector)
#' @param difference Whether or not to return values as the difference between
#' the target and reference periods. Requires an input for `reference_period`.
#' (logical).
#' @param scenarios A vector of representative concetration pathway keys
#' (e.g., c("rcp45", rcp85")). (vector)
#' @export
scatterplot <- function(file_df = NA,
                        agg_fun = "mean",
                        var1 = "tasmax",
                        var2 = "pr",
                        months1 = "all", 
                        months2 = "all", 
                        target_period = c(2080, 2099),
                        reference_period = c(1990, 2019),
                        difference = TRUE,
                        scenarios = c("rcp45", "rcp85")  # <------------------ They will be able to specify any grouping variable, and the function will plot accordingly. At least that's the plan.
                        ) {
  '
  var1 = "tasmax"
  var2 = "pr"
  months1 = c("aug", "sep", "oct")
  months2 = c("apr", "may", "jun")
  agg_fun = "mean"
  difference = TRUE
  reference_period = c(1950, 2010)
  target_period = c(2080, 2099)
  file_df <- cstdata::cstdata(park = "Yellowstone National Park",
                            years = c(1950, 2099),
                            local_dir = "~/cstdata_test",
                            ncores = get_ncores())
  '
  # Sample set of model outputs  <--------------------------------------------- It takes a minute to get these out. Still some trouble with open node connections.
  if (identical(file_df, NA)) {
    msg <- paste("Please provide a file reference object from a `cstdata`",
                 "run")
    stop(msg)
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

  # If they didn't specify months, or used the flag "all", use all months
  if (identical(months1, "all")) {
    months1 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  }
  if (identical(months2, "all")) {
    months2 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  }

  # Reformat and map the requested months to their variable strings
  month_map <- c()
  month_map[[var1]] <- format_months(months1)
  month_map[[var2]] <- format_months(months2)

  # Add this to the file reference object
  file_df$months <- lapply(file_df$parameter, FUN = function(x) month_map[[x]])
  
  # Check that each file list is longer than 0
  if (nrow(file_df) == 0) {
    stop("There are no stored files that contain the information requested.")
  }

  # Get the aggregating function value for each file within the target period
  print("Calculating Target Period Values")
  file_df$agg_value <- get_fun(file_df, agg_fun, target_period)
  file_df <- convert_KC(file_df, "agg_value")

  # If a difference is required
  if (difference) {

    # Get the aggregating function value for the reference period
    print("Calculating Reference Period Values")
    file_df$ref_agg_value <- get_fun(file_df, agg_fun, reference_period)
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

  # Now split the data up into separate data frames
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
  xvar <- unique(file_df$label[file_df$parameter == var1])
  yvar <- unique(file_df$label[file_df$parameter == var2])
  xmonths <- unique(file_df$months[file_df$parameter == var1])
  ymonths <- unique(file_df$months[file_df$parameter == var2])
  xmonths <- lapply(unlist(xmonths), FUN = function(x) substr(x, 1, 1))
  ymonths <- lapply(unlist(ymonths), FUN = function(y) substr(y, 1, 1))
  xmonths <- paste0("(", paste0(xmonths, collapse = ""), ")")
  ymonths <- paste0("(", paste0(ymonths, collapse = ""), ")")
  xlabel <- paste(xvar, xmonths)
  ylabel <- paste(yvar, ymonths)
  
  # Create the basic plot
  `%>%` <- dplyr::`%>%`
  graph <- plotly::plot_ly(graph_df,
                           x = ~var1,
                           y = ~var2,
                           color = ~rcp,
                           colors = c("blue", "orange"),
                           mode = "markers",
                           size = 7,
                           text = ~model,
                           type = "scatter")

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

format_months <- function(months) {
  #' Reformat months inputs to %b style

  # To convert to padded strings if numbers
  rfrmt <- function(x) {
    x <- paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2, 3)))
    return(x)
  }

  # The numeric case
  if (class(months) == "numeric") {
    months <- unlist(lapply(months, FUN = function(x) sprintf("%02d", x)))
    yms <- paste0("1900-", months)
    ymds <- as.Date(paste0(yms, "-01"))
    months <- format(ymds, "%b")

  # The character case (numbers as characters won't work)
  } else if (class(months) == "character") {
    months <- unlist(lapply(months, FUN = rfrmt))
  }

  # Return standardized list
  return(months)
}

get_days_since <- function(year_range, months) {
  '
  months = c(1,2,3)
  months = c("jan", "feb", "mar")
  months = c("Januray", "February", "March")
  '
  # Make sure the months object is unlisted
  months <- unlist(months)

  # Get all of the dates within the year range
  base = as.Date("1950-01-01")
  date1 = as.Date(paste0(as.character(year_range[[1]]), "-01-01"))
  date2 = as.Date(paste0(as.character(year_range[[2]]), "-12-31"))
  dates = seq(date1, date2, by = "day")

  # Filter out dates not in the list
  dates <- dates[format(dates, "%b") %in% months]

  # Now we want days since 1950
  days = dates - base
  
  # return these to use to query the netcdf objects
  return(days)
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

get_fun <- function(file_df, agg_fun, time_period) {

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

  # Extract values
  tryCatch({
    ##### Attempt One (doesn't work) #####
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
    
    ##### Attempt 2 ######
    cl <- parallel::makeCluster(4) #get_ncores())
    doSNOW::registerDoSNOW(cl)
    `%dopar%` <- foreach::`%dopar%`
    pb <- progress::progress_bar$new(format = "[:bar] |:percent ~:eta",
                                     total = nrow(file_df), complete = "+",
                                     incomplete = " ", current = " ",
                                     width = 60)
    progress_bar <- function(n) pb$tick()
    opts <- list(progress = progress_bar)
    values <- foreach::foreach(i = 1: nrow(file_df),
                              .options.snow = opts,
                              .export = "get_days_since") %dopar% {
      # Which is which
      row <- file_df[i, ]
      file <- unlist(row$local_path)
      variable <- unname(unlist(row$internal))
      months <- row$months

      # Open File
      nc <- ncdf4::nc_open(file)
     
      # Get the matrix of values
      subset <- ncdf4::ncvar_get(nc, variable)

      # Filter dates for this variable
      days <- get_days_since(time_period, months)

      # Get the matrix slices
      subset <- subset[, , days]
     
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
