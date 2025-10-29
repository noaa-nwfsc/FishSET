#' 
#' Fit Autoregressive (AR) Models for Multiple Groups
#'
#' @description This function fits separate autoregressive (AR) models of a specified order (`p`)
#' for multiple groups or categories within a dataset. It processes raw observation
#' data, calculates daily averages for each group, fits an AR(p) model via
#' ordinary least squares (`lm`), and returns the in-sample fitted values in a
#' wide-format matrix.
#'
#' @details The function first aggregates the raw `obs_values` by calculating the mean for
#' each unique `obs_groups` and `obs_dates` combination. It then iterates through
#' each group, creates lagged predictor variables, and fits a linear model
#' (`value ~ lag1 + lag2 + ...`). The output matrix is dense, containing a row
#' for every date in `unique_dates` and a column for every group in
#' `unique_groups`. Cells for which a fitted value could not be computed
#' (e.g., at the start of a series) will contain `NA`.
#'
#' @param unique_dates A `Date` vector containing all unique, sorted dates that
#'   will form the rows of the output matrix.
#' @param unique_groups A `character` vector containing all unique, sorted group
#'   names that will form the columns of the output matrix.
#' @param obs_dates A `Date` vector of the same length as `obs_values`,
#'   indicating the date of each observation.
#' @param obs_groups A `character` vector of the same length as `obs_values`,
#'   indicating the group of each observation.
#' @param obs_values A `numeric` vector containing the observation values.
#' @param lag_p An `integer` specifying the order of the AR model (i.e., the
#'   number of lags to use as predictors).
#'   
#' @keywords internal
#'
#' @return
#' A `matrix` with dates as rownames and groups as colnames. Each cell contains
#' the in-sample fitted value from the AR(p) model for that group and date.
#'
#' @importFrom data.table data.table := shift setorder
#'
#' @export
#' 
fit_ar_models <- function(unique_dates,
                          unique_groups,
                          obs_dates,
                          obs_groups,
                          obs_values,
                          lag_p,
                          empty_catch = NA) {
  
  # Initialize the final output matrix with NAs ---------------------------------------------------
  # This creates the dense date x group structure.
  result_matrix <- matrix(
    NA_real_,
    nrow = length(unique_dates),
    ncol = length(unique_groups),
    dimnames = list(as.character(unique_dates), unique_groups)
  )
  
  # Aggregate raw observations to get daily averages for each group -------------------------------
  obs_dt <- data.table(
    date = obs_dates,
    group = obs_groups,
    value = obs_values
  )
  daily_avg <- obs_dt[, .(value = mean(value)), by = .(group, date)]
  
  # Handle NAs ------------------------------------------------------------------------------------
  if (is.na(empty_catch)) {
    daily_avg <- na.omit(daily_avg)  
  }
  
  # Loop through each group to fit a separate AR model --------------------------------------------
  for (current_group in unique_groups) {
    # Subset the data for the current group and ensure it's sorted by date
    group_dt <- daily_avg[group == current_group]
    data.table::setorder(group_dt, date)
    
    # Skip if there's not enough data to create the lags
    if (nrow(group_dt) <= lag_p) {
      next
    }
    
    ## Create the lagged predictor columns using data.table's shift()
    lag_cols <- paste0("lag", 1:lag_p)
    group_dt[, (lag_cols) := data.table::shift(value, n = 1:lag_p, type = "lag")]
    
    ## Build the regression formula dynamically
    # e.g., "value ~ lag1 + lag2 + lag3"
    formula_str <- paste("value ~", paste(lag_cols, collapse = " + "))
    ar_formula <- as.formula(formula_str)
    
    ## Fit the linear model
    # na.action = na.omit automatically removes the starting rows with NA lags
    model <- lm(ar_formula, data = group_dt, na.action = na.omit)
    
    ## Extract fitted values and their corresponding dates
    fitted_vals <- fitted(model)
    
    # To get the correct dates, we find which rows were NOT omitted
    # na.action(model) returns the indices of the omitted rows
    # The dates for the fitted values are the ones *not* in that list.
    fitted_dates <- as.character(group_dt[-na.action(model), date])
    
    ## Populate the result matrix with the fitted values
    result_matrix[fitted_dates, current_group] <- fitted_vals
  }
  
  return(result_matrix)
}
