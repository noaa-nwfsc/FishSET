#' Calculate monthly moving window averages
#'
#' @param project Sting, name of the FishSET project.
#' @param df Primary data frame or data.table containing the observations.
#' @param var_name String, the name of the new column to be added to \code{df} 
#'   containing the calculated moving averages. Defaults to \code{"moving_avg"}.
#' @param grid_name String, name of the gridded table to save to the FishSET database.
#' @param append_to_existing Logical, If TRUE, looks for an existing GridTable in the 
#'   database and merges the new column into it. Defaults to FALSE (creates a new table).
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

calc_monthly_moving_avg <- function(project,
                                    df,
                                    var_name,
                                    grid_name,
                                    append_to_existing,
                                    year_col, 
                                    month_col, 
                                    group_cols,
                                    value_col,
                                    window_size = 3,
                                    month_lag = 1,
                                    year_lag = 0,
                                    fill_empty_expectation = NA) {
  
  dt <- as.data.table(df)
  
  # Create full grid of all grouping variables
  group_list <- lapply(group_cols, function(col) unique(dt[[col]]))
  full_groups <- do.call(data.table::CJ, group_list)
  setnames(full_groups, group_cols)
  
  # Create unified group ID
  temp_id_col <- "TEMP_UNIFIED_ID_XYZ" # Use a temp name to avoid clashes
  full_groups[, (temp_id_col) := do.call(paste, c(.SD, sep = "_")), .SDcols = group_cols]
  unique_groups <- full_groups[["TEMP_UNIFIED_ID_XYZ"]]
  
  # Create the unified ID on the observation data so C++ can map it
  dt[, (temp_id_col) := do.call(paste, c(.SD, sep = "_")), .SDcols = group_cols]
  
  # Prepare unique vectors
  unique_times <- unique(dt[, .(get(year_col), get(month_col))])
  setnames(unique_times, c("yr", "mth"))
  setorder(unique_times, yr, mth)
  
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
           new = c(year_col, month_col, temp_id_col, var_name))
  
  # Re-attach the original separate group_cols
  group_map <- full_groups
  lookup_table <- merge(lookup_table, group_map, by = temp_id_col, all.x = TRUE)
  lookup_table[, (temp_id_col) := NULL]
  
  # Handle missing expectations
  if (!is.na(fill_empty_expectation)) {
    # Replace NAs in the NEW column with the provided value
    # We use get() to refer to the column by its string name
    fill_val <- as(fill_empty_expectation, class(lookup_table[[var_name]]))
    lookup_table[is.na(get(var_name)), (var_name) := fill_val]
  }
  
  # Save to database
  if (append_to_existing) {
    grid_table_name <- paste0(project, grid_name, "GridTable")
    
    # Try to load existing grid
    existing_grid <- tryCatch({
      table_view(grid_table_name, project, convert_dates = FALSE)
    }, error = function(cond) {
      message("Existing grid '", grid_name, "' not found. Creating a new one instead.")
      return(NULL)
    })
    
    if (!is.null(existing_grid)) {
      existing_dt <- as.data.table(existing_grid)
      # Merge new column into existing grid using full outer join
      final_grid <- merge(existing_dt,
                          lookup_table,
                          by = c(year_col, month_col, group_cols),
                          all = TRUE)
    } else {
      final_grid <- lookup_table
    }
  } else {
    final_grid <- lookup_table
  }
  
  # Convert back to data.frame for FishSET
  final_grid_df <- as.data.frame(final_grid)
  
  # Save to FishSET database
  load_grid(grid = final_grid_df,
            name = grid_name,
            project = project,
            over_write = TRUE)
  
  return(invisible(final_grid_df))
}
