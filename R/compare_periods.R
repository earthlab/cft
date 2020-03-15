#' Compare climate data among time periods
#' 
#' Compare climate data between a reference and target period. This can be
#' useful to understand how projected future climate differs from historical
#' conditions. The `compare_periods` function generates climate summary 
#' statistics for each period, and computes the difference in values between
#' the target and reference period. Uses the output of `cstdata()` as an input. 
#' 
#' @param file_df A file reference data frame output from a `cstdata()` run.
#' (data.frame)
#' @param agg_fun The aggregating function to applied to the model variables
#' (e.g., "mean"). (character)
#' @param var1 The x-axis variable, see cst::argument_reference
#' (e.g., "pr"). (character).
#' @param var2 The y-axis variable, see cst::argument_reference
#' (e.g., "tasmax").  character)
#' @param months1 The months of the year with which to include in the
#' aggregation of variable 1 (e.g., 5:7 for May, June, and July. (int vector)
#' @param months2 The months of the year with which to include in the
#' aggregation of variable 2 (e.g., 5:7 for May, June, and July. (int vector)
#' @param target_period The time period within which to apply the aggreating
#' value function (e.g., c(2050, 2075)). (vector)
#' @param reference_period The time period with which to compare the target
#' period (e.g., c(1980, 2010)). (vector)
#' @param scenarios A vector of representative concetration pathway keys
#' (e.g., c("rcp45", rcp85")). (vector)
#' 
#' @return A data.frame with columns containing `target_summary` and 
#' `reference_summary` values, and their difference (the column `diff_summary`).
#' 
#' @importFrom magrittr %>%
#' @export
compare_periods <- function(
  file_df,
  agg_fun = "mean",
  var1 = "tasmax",
  var2 = "pr",
  months1 = 1:12, 
  months2 = 1:12, 
  target_period = c(2080, 2099),
  reference_period = c(1990, 2019),
  scenarios = c("rcp45", "rcp85")
  ) {

  file_df <- file_df[file_df$parameter %in% c(var1, var2), ]
  if (nrow(file_df) == 0) {
    stop("There are no stored files that contain the information requested.")
  }
  
  # Reformat and map the requested months to their variable strings
  month_map <- c()
  month_map[[var1]] <- format_months(months1)
  month_map[[var2]] <- format_months(months2)

  file_df$months <- lapply(file_df$parameter, FUN = function(x) month_map[[x]])
  
  message("Calculating Target Period Values")
  file_df <- get_fun(file_df, agg_fun, target_period, "target_summary")

  message("Calculating Reference Period Values")
  file_df <- get_fun(file_df, agg_fun, reference_period, "reference_summary")
  
  file_df$diff_summary <- file_df$target_summary - file_df$reference_summary

  file_df$label <- paste0("Difference in ", file_df$full_var_name,
                          " (", file_df$units, ")")
  return(file_df)
}

# convert temperature from kelvin to celsius if necessary
convert_temperature <- function(file_df, column) {
  temperature_rows <- grep("air_temperature", file_df$parameter_long)
  temperature_kelvin <- unlist(file_df[[column]][temperature_rows])
  temperature_c <- temperature_kelvin - 273.15
  file_df[[column]][temperature_rows] <- temperature_c
  file_df$units[temperature_rows] <- "C"
  return(file_df)
}

# Reformat months from integers to %b style
format_months <- function(months) {
  if (!class(months) %in% c("numeric", "integer")) {
    stop(paste("The months argument must be numeric, but was", class(months)))
  }
  ym <- paste0("1900-", sprintf("%02d", months))
  ymds <- as.Date(paste0(ym, "-01"))
  months <- format(ymds, "%b")
  return(months)
}

get_fun <- function(file_df, agg_fun, time_period, new_colname) {

  # Check that these files contain data within the earliest reference year
  year1 <- min(as.numeric(file_df$year1))
  year2 <- min(as.numeric(file_df$year2))
  if (time_period[[1]] < year1 | time_period[[2]] > year2) {
    msg <- paste0("The data provided does not contain the years requested.")
    stop(msg)
  }

  # Match the aggregation function string to a function 
  tryCatch({
    fun <- match.fun(agg_fun)
  }, error = function(e) {
    stop(paste0("The aggregation function '", agg_fun,
                "' is not available."))
  })

  cl <- parallel::makeCluster(2)
  on.exit(parallel::stopCluster(cl))
  values <- parallel::parApply(
    cl = cl,
    X = file_df,
    MARGIN = 1,
    FUN = function(row) {

      # Find days since 1950-01-01 to query the netcdf objects
      days_since_1950 <- function(year_range, months) {
        months <- unlist(months)
  
        # Get all of the dates within the year range
        base <- as.Date("1950-01-01")
        date1 <- as.Date(paste0(as.character(year_range[[1]]), "-01-01"))
        date2 <- as.Date(paste0(as.character(year_range[[2]]), "-12-31"))
        dates <- seq(date1, date2, by = "day")
  
        # Filter out dates not in the list
        dates <- dates[format(dates, "%b") %in% months]
  
        n_days_since <- dates - base
        return(n_days_since)
      }
  
      file <- unlist(row["local_path"])
      variable_name <- unname(unlist(row["parameter_long"]))
      nc <- ncdf4::nc_open(file)
      values <- ncdf4::ncvar_get(nc, variable_name)

      # use days since 1950 to index positions of all days in file
      filter_days <- days_since_1950(time_period, row["months"])
      all_days <- nc$dim$time$vals
      day_indices <- match(filter_days, all_days)
      values <- values[, , day_indices]
      agg <- fun(values, na.rm=TRUE)
  
      return(agg)
    })
  file_df[, new_colname] <- unlist(values)

  # convert from K to C if working with temperature data
  if ("air_temperature" %in% file_df$parameter_long) {
    file_df <- convert_temperature(file_df, new_colname)
  }

  return(file_df)
}
