#' Compare climate data among time periods
#' 
#' Compare climate data between a reference and target period. This can be
#' useful to understand how projected future climate differs from historical
#' conditions. The `compare_periods` function generates climate summary 
#' statistics for each period, and computes the difference in values between
#' the target and reference period. Uses the output of `cstdata()` as an input. 
#' 
#' @param df A data frame output from a `cst_df()` run.
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
#' @importFrom rlang .data
#' @export
compare_periods <- function(
  df,
  agg_fun = "mean",
  var1 = "tasmax",
  var2 = "pr",
  months1 = 1:12, 
  months2 = 1:12, 
  target_period = c(2080, 2099),
  reference_period = c(1990, 2019),
  scenarios = c("rcp45", "rcp85")
  ) {

  # Make sure the requested variables are available
  if (!var1 %in% names(df) | !var2 %in% names(df)) {
    stop("The requested variables are not present in the data frame provided.")
  }

  # Check that the requested scenarios are availabe and filter if so
  if (! any(scenarios %in% df$rcp) ) {
    stop("The requested scenarios are not present in the data frame provided.")
  } else {
    df <- df[df$rcp %in% scenarios, ]
  }

  # If the periods are given as single years, turn them into 'ranges'
  if (length(target_period) == 1) target_period = c(target_period, target_period)
  if (length(reference_period) == 1) reference_period = c(reference_period, reference_period)

  # Make sure the requested target years are available
  years = format(df$date, "%Y")
  if (! any(years >= target_period[1]) | ! any(years <= target_period[2])) {
    stop("The requested target period is at least partially unavailable in this data frame.")
  }

  # Make sure the requested reference years are available
  if (! any(years >= reference_period[1]) | ! any(years <= reference_period[2])) {
    stop("The requested reference period is at least partially unavailable in this data frame.")
  }

  # Create a mapping of the requested months to their variable strings
  month_map <- c()
  month_map[[var1]] <- format_months(months1)
  month_map[[var2]] <- format_months(months2)
  df$month = vapply(df$date, FUN = function(x) format(x, "%b"), character(1))  # <-------------- too slow
  df$year = vapply(df$date, FUN = function(x) format(x, "%Y"), character(1))

  # Get the comparisons for each variable
  df1 <- df_difference(df, var1, agg_fun, target_period, reference_period, month_map)
  df2 <- df_difference(df, var2, agg_fun, target_period, reference_period, month_map)

  # Combine into one data frame
  cdf <- rbind(df1, df2)

  # Rearrange columns
  cdf <- cdf[c("model", "rcp", "variable", "units", "value_reference", "value_target",
               "difference" )]

  return(cdf)
}


# convert file reference object temperature from kelvin to celsius if necessary
convert_temperature <- function(file_df, column) {
  temperature_rows <- grep("air_temperature", file_df$parameter_long)
  temperature_kelvin <- unlist(file_df[[column]][temperature_rows])
  temperature_c <- temperature_kelvin - 273.15
  file_df[[column]][temperature_rows] <- temperature_c
  file_df$units[temperature_rows] <- "C"
  return(file_df)
}


# Get the difference in values for one variable
df_difference <- function(df, variable, agg_fun, target_period, reference_period, month_map) {

  # Match the aggregation function string to a function 
  tryCatch({
    fun <- match.fun(agg_fun)
  }, error = function(e) {
    stop(paste0("The aggregation function '", agg_fun,
                "' is not available."))
  })

  # Reference Period
  df_ref <- df %>%
    dplyr::select(.data$rcp, .data$model, .data$year, .data$month, variable) %>%
    dplyr::filter(.data$month %in% month_map[[variable]]) %>%
    dplyr::filter(.data$year >= reference_period[1],
                  .data$year <= reference_period[2]) %>%
    dplyr::group_by(.data$rcp, .data$model) %>%
    dplyr::summarise("value" = fun(!!as.name(variable)))
  
  # Target Period
  df_tar <- df %>%
    dplyr::select(.data$rcp, .data$model, .data$year, .data$month, variable) %>%
    dplyr::filter(.data$month %in% month_map[[variable]]) %>%
    dplyr::filter(.data$year >= target_period[1],
                  .data$year <= target_period[2]) %>%
    dplyr::group_by(.data$rcp, .data$model) %>%
    dplyr::summarise("value" = fun(!!as.name(variable)))
  
  # Join
  df = dplyr::left_join(df_ref, df_tar, by = c("rcp", "model"),
                        suffix = c("_reference", "_target"))
  
  # Find the difference in values between target and reference periods
  df = df %>% dplyr::mutate(difference = .data$value_target - .data$value_reference)

  # Add variable name
  df$variable <- variable

  # Add in units
  arg_ref <- Argument_Reference()
  internal_var <- as.character(arg_ref$variables[ variable])
  units = as.character(arg_ref$units[internal_var])
  df$units <- units

  return(df)
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
