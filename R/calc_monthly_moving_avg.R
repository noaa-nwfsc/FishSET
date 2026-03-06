#' Calculate monthly moving window averages
#'
#' @param df Primary data frame or data.table containing the observations.
#' @param name String, the name of the new column to be added to \code{df} 
#'   containing the calculated moving averages. Defaults to \code{"moving_avg"}.
#' @param year_col String, the name of the column containing the year (integer).
#' @param month_col String, the name of the column containing the month (integer).
#' @param group_cols Character vector, variable(s) from \code{df} that define how to 
#'   group the data for averaging (e.g. \code{c("VESSEL_ID", "ZONE")}). The moving 
#'   average is calculated independently for each group defined here.
#' @param value_col String, the name of the column containing the numeric values 
#'   to be averaged.
#' @param window_size Numeric, the size of the moving window in months. Defaults to 3.
#' @param month_lag Numeric, the number of months to lag the window. Defaults to 1.
#'   For example, a lag of 1 starts the average from the previous month.
#' @param year_lag Numeric, the number of years to lag the reference date. Defaults to 0.
#'   If set to 1, the function looks up the date from exactly one year prior before 
#'   applying the \code{month_lag} and \code{window_size}.
#' @param fill_empty_expectation Numeric or \code{NA}. Value used to fill \code{NA} 
#'   values in the resulting moving average column. Defaults to \code{NA}.
#' 
#' @importFrom data.table as.data.table := .SD setnames setorder
#' 
#' @returns Returns the original \code{df} as a data.table with the additional 
#'   column specified by \code{name}.
#' @export

calc_monthly_moving_avg <- function(df,
                                    name,
                                    year_col, 
                                    month_col, 
                                    group_cols,
                                    value_col,
                                    window_size = 3,
                                    month_lag = 1,
                                    year_lag = 0,
                                    fill_empty_expectation = NA) {
  
  dt <- as.data.table(df)
  
  # Create unified group ID
  # Use a temp name to avoid clashes
  temp_id_col <- "TEMP_UNIFIED_ID_XYZ"
  dt[, (temp_id_col) := do.call(paste, c(.SD, sep = "_")), .SDcols = group_cols]
  
  # Prepare unique vectors
  unique_times <- unique(dt[, .(get(year_col), get(month_col))])
  setnames(unique_times, c("yr", "mth"))
  setorder(unique_times, yr, mth)
  
  unique_groups <- unique(dt[[temp_id_col]])
  
  # Call C++ function
  res_mat <- calculate_monthly_avg(
    unique_years = unique_times$yr,
    unique_months = unique_times$mth,
    unique_groups = unique_groups,
    
    obs_years = dt[[year_col]],
    obs_months = dt[[month_col]],
    obs_groups = dt[[temp_id_col]],
    obs_values = dt[[value_col]],
    
    window_size = window_size,
    month_lag = month_lag,
    year_lag = year_lag,
    weighted = FALSE
  )
  
  # Convert to data table
  n_times <- nrow(unique_times)
  n_groups <- length(unique_groups)
  
  lookup_table <- data.table(
    yr = rep(unique_times$yr, times = n_groups),
    mth = rep(unique_times$mth, times = n_groups),
    group_id = rep(unique_groups, each = n_times),
    val_placeholder = as.vector(res_mat) # Flattens matrix column by column
  )
  
  # Rename columns to match the input df and the desired target column name
  setnames(lookup_table, 
           old = c("yr", "mth", "group_id", "val_placeholder"), 
           new = c(year_col, month_col, temp_id_col, name))
  
  dt <- merge(dt, lookup_table, by = c(year_col, month_col, temp_id_col), all.x = TRUE)
  
  # Handle missing expectations
  if (!is.na(fill_empty_expectation)) {
    # Replace NAs in the NEW column with the provided value
    # We use get() to refer to the column by its string name
    dt[is.na(get(name)), (name) := fill_empty_expectation]
  }
  
  # Cleanup
  dt[, (temp_id_col) := NULL]
  
  return(as.data.frame(dt))
}
