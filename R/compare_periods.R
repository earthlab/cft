#' Compare climate data among time periods
#' 
#' Compare climate data between a reference and target period. This can be
#' useful to understand how projected future climate differs from historical
#' conditions. The `compare_periods` function generates climate summary 
#' statistics for each period, and computes the difference in values between
#' the target and reference period. Uses the output of `cftdata()` as an input. 
#' 
#' @param df A data frame output from a `cft_df()` run.
#' (data.frame)
#' @param agg_fun The aggregating function to applied to the model variables
#' (e.g., "mean"). (character)
#' @param var1 The x-axis variable.
#' (e.g., "pr"). (character).
#' @param var2 The y-axis variable.
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

  # Add periods in case that's helpful
  if ( length(unique(reference_period)) == 1 ) {
    ref_print = as.character(reference_period[1])
  } else {
    ref_print = paste(reference_period[1], reference_period[2], sep = " - ")
  }
  if ( length(unique(target_period)) == 1 ) {
    target_print = as.character(target_period[1])
  } else {
    target_print = paste(target_period[1], target_period[2], sep = " - ")
  }
  cdf["reference_period"] <- ref_print
  cdf["target_period"] <- target_print

  # Rearrange columns
  cdf <- cdf[c("model", "rcp", "parameter", "units", "reference_period", "target_period",
               "reference_value", "target_value", "difference" )]

  return(cdf)
}

# Get the difference in values for one parameter
df_difference <- function(df, parameter, agg_fun, target_period, reference_period, month_map) {

  # Match the aggregation function string to a function 
  tryCatch({
    fun <- match.fun(agg_fun)
  }, error = function(e) {
    stop(paste0("The aggregation function '", agg_fun,
                "' is not available."))
  })

  # Reference Period
  df_ref <- df %>%
    dplyr::select(.data$rcp, .data$model, .data$year, .data$month, parameter) %>%
    dplyr::filter(.data$month %in% month_map[[parameter]]) %>%
    dplyr::filter(.data$year >= reference_period[1],
                  .data$year <= reference_period[2]) %>%
    dplyr::group_by(.data$rcp, .data$model) %>%
    dplyr::summarise("value" = fun(!!as.name(parameter)))
  
  # Target Period
  df_tar <- df %>%
    dplyr::select(.data$rcp, .data$model, .data$year, .data$month, parameter) %>%
    dplyr::filter(.data$month %in% month_map[[parameter]]) %>%
    dplyr::filter(.data$year >= target_period[1],
                  .data$year <= target_period[2]) %>%
    dplyr::group_by(.data$rcp, .data$model) %>%
    dplyr::summarise("value" = fun(!!as.name(parameter)))

  # Join
  df <- dplyr::left_join(df_ref, df_tar, by = c("rcp", "model"))
  names(df) <- c("rcp", "model", "reference_value", "target_value")                     

  # Find the difference in values between target and reference periods
  df = df %>% dplyr::mutate(difference = .data$target_value - .data$reference_value)

  # Add variable name
  df$parameter <- parameter

  # Add in units
  arg_ref <- get_reference("maca")  # <--------------------------------------------------------- infer or provide this, there might be outputs from two datasets here, also dont filter for rcp if not available
  internal_var <- as.character(arg_ref$variables[parameter])
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
