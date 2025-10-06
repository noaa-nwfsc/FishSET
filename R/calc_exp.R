#' Calculate expected catch/revenue
#' 
#' @param dataset Data 
#' @param catch catch
#' @param price price
#' @param defineGroup define group
#' @param temp.var temporal variable
#' @param temporal temporal 
#' @param calc.method calculation method
#' @param lag.method lag method
#' @param empty.catch empty catch
#' @param empty.expectation empty expectation
#' @param temp.window temporal window
#' @param day.lag temporal lag
#' @param year.lag year lag
#' @param dummy.exp dummy matrix
#' @param weight_avg weighted average
#' @param Alt Alternative choice list
#' @keywords internal
#' @importFrom data.table := as.data.table .I .GRP fcoalesce dcast setnames 
#'             setcolorder setkey .SD data.table set copy rbindlist setorder
#' @importFrom lubridate floor_date year years %m-%
#' @importFrom stats aggregate lm coef na.pass ar.ols predict
#' @returns Returns a list containing the expected catch/revenue matrix, 
#'   dummy matrix (if \code{dummy.exp = TRUE}), and list of input args. 
#' 
calc_exp <- function(dataset,
                     catch,
                     price = NULL,
                     defineGroup = NULL,
                     temp.var = NULL,
                     temporal = "daily",
                     calc.method = "standardAverage", 
                     lag.method = "simple",
                     empty.catch = NULL,
                     empty.expectation = NULL,
                     temp.window = 7,
                     day.lag = 1,
                     year.lag = 0,
                     dummy.exp = FALSE,
                     weight_avg = FALSE,
                     Alt) {
  
  # Initial data prep -----------------------------------------------------------------------------
  catch_name <- catch                   # save variable name for settings
  dataZoneTrue <- Alt[["dataZoneTrue"]] # used for catch and other variables
  choice <- Alt[["choice"]]             # used for catch and other variables
  
  # Use to data.table for efficiency
  dt <- as.data.table(dataset)
  
  # Create a unique occasion ID for later joins
  dt[, occasion_id := .I]
  
  # Determine fleet/grouping structure
  if (is_value_empty(defineGroup)) {
    dt[, fleet := 1L]
  } else {
    dt[, fleet := .GRP, by = defineGroup]
  }
  
  # Subset to relevant data zone and create core calculation table
  z_ind <- which(dataZoneTrue == 1)
  
  # Non temporal ----------------------------------------------------------------------------------
  if (is_value_empty(temp.var) || temp.var == "none") {
    # Select columns
    df_no_temp <- dt[z_ind, c("fleet", catch), with = FALSE]
    
    # Rename and add necessary columns
    setnames(df_no_temp, catch, "catch_val")
    df_no_temp[, ':=' (
      zones = as.character(choice[z_ind]),
      catch_val = as.numeric(catch_val)
    )]
    
    # Handle revenue calculation
    if (!is_value_empty(price) && price != "none") {
      price_vec <- as.numeric(dataset[[price]][z_ind])
      df_no_temp[, catch_val := catch_val * price_vec]
    }
    
    # Calculate overall mean per group/area
    allCatch <- if (is_value_empty(defineGroup)) {
      df_no_temp[, .(mean_catch = mean(catch_val, na.rm = TRUE)), by = zones]
    } else {
      df_no_temp[, ID := paste0(fleet, zones)]
      df_no_temp[, .(mean_catch = mean(catch_val, na.rm = TRUE)), by = ID]
    }
    
    matrix_cols_df <- allCatch[, !"mean_catch"]
    matrix_cols <- unlist(matrix_cols_df)
    
    # Create the final matrix by repeating the mean values for each observation
    exp_matrix <- matrix(allCatch$mean_catch,
                         nrow = nrow(dataset),
                         ncol = nrow(allCatch),
                         dimnames = list(NULL, matrix_cols),
                         byrow = TRUE
    )
    
    # Return directly from the function
    return(list(exp = exp_matrix,
                dummy = NULL,
                settings = list("catch" = catch_name, 
                                "price" = price, 
                                "defineGroup" = defineGroup, 
                                "temp.var" = temp.var, 
                                "temporal" = temporal, 
                                "calc.method" = calc.method, 
                                "lag.method" = lag.method, 
                                "empty.catch" = empty.catch, 
                                "empty.expectation" = empty.expectation, 
                                "temp.window" = temp.window, 
                                "day.lag" = day.lag, 
                                "year.lag" = year.lag, 
                                "dummy.exp" = dummy.exp, 
                                "weight_avg" = weight_avg)
    ))
  }
  
  # Temporal --------------------------------------------------------------------------------------
  # Prepare data for temporal calculations
  df <- dt[z_ind, c("occasion_id", "fleet", catch, temp.var), with = FALSE]
  
  setnames(df, c(catch, temp.var), c("catch_val", "date"))
  
  df[, ':='(
    zones = as.character(choice[z_ind]),
    date = date_parser(date),
    catch_val = as.numeric(catch_val)
  )]
  
  # Handle revenue calculation
  if (!is_value_empty(price) && price != "none") {
    price_vec <- as.numeric(dataset[[price]][z_ind])
    df[, catch_val := catch_val * price_vec]
  }
  
  df[, dateFloor := floor_date(date, unit = "day")]
  
  df[, ID := if (is_value_empty(defineGroup)) as.character(zones) else paste0(fleet, zones)]
  
  # Check if fleet/group results in single observations per ID
  if (any(table(df$ID) == 1)) {
    total_single_obs <- sum(table(df$ID) == 1)
    perc_single_obs <- (total_single_obs / length(table(df$ID) == 1)) * 100
    
    warning(paste0(
      "The selected grouping variable results in ",
      perc_single_obs,
      "% group-zone combinations with a single value. Expected catch/revenue will be empty for 
      these group-zone combinations."
    ))
  }
  
  # Handle empty catch values with joins
  if (!is_value_empty(empty.catch) && anyNA(df$catch_val)) {
    if (empty.catch == 0) {
      df[is.na(catch_val), catch_val := 0]
      
    } else if (empty.catch == "allCatch") {
      df[, year := lubridate::year(dateFloor)]
      yearly_means <- df[, .(mean_c = mean(catch_val, na.rm = TRUE)), by = year]
      df[yearly_means, on = "year", catch_val := fcoalesce(catch_val, i.mean_c)]
      
    } else if (empty.catch == "groupedCatch") {
      df[, year := lubridate::year(dateFloor)]
      grouped_means <- df[, .(mean_c = mean(catch_val, na.rm = TRUE)), by = .(year, fleet)]
      df[grouped_means, on = c("year", "fleet"), catch_val := fcoalesce(catch_val, i.mean_c)]
    }
  }
  
  all_dates_in_range <- seq(from = min(df$dateFloor), to = max(df$dateFloor), by = "day")
  
  # Daily = TRUE, Sequential = FALSE
  temporal_daily = if (temporal == "daily") TRUE else FALSE
  
  # Moving window average
  if (calc.method == "standardAverage") {
    exp_values <- calculate_moving_avg(
      unique_dates = all_dates_in_range,
      unique_groups =  unique(df$ID), 
      obs_dates =  df$dateFloor,
      obs_groups =  df$ID,
      obs_values =  df$catch_val, 
      window_size =  temp.window, 
      lag = day.lag,
      year_lag = year.lag,
      temporal = temporal_daily,
      weighted = weight_avg
    )  
    
    # Autoregressive model
  } else {
    exp_values <- fit_ar_models(
      unique_dates = unique(df$date),
      unique_groups = unique(df$ID),
      obs_dates = df$date,
      obs_groups = df$ID,
      obs_values = df$catch_val,
      lag_p = day.lag
    )
  }
  
  # Reformat matrix and merge with observed dates
  exp_values <- as.data.frame(exp_values) %>% 
    mutate(date = as.Date(rownames(exp_values)))
  exp_df <- data.frame(date = df$dateFloor) %>%
    left_join(., exp_values, by = "date") %>%
    select(!date)
  
  # Check for missing columns and fill in with NAs
  missing_cols <- setdiff(unique(df$ID), names(exp_df))
  if (length(missing_cols) > 0) {
    exp_df[missing_cols] = NA
  }
  
  # Set the order of columns in the matrix
  setcolorder(exp_df, sort(unique(df$ID)))
  
  # Change to matrix
  exp_matrix <- as.matrix(exp_df)
  
  # Dummy matrix for catch ------------------------------------------------------------------------
  if (dummy.exp) {
    dum_matrix <- (!is.na(exp_matrix)) * 1
    
  } else {
    dum_matrix <- NULL
  }
  
  # Handle empty expectation values ---------------------------------------------------------------
  if (empty.expectation == 1e-4) {
    # If 1e-04, fill both NAs and 0s with 1e-04
    exp_matrix[is.na(exp_matrix)] <- 1e-4
    exp_matrix[exp_matrix == 0] <- 1e-4
    
  } else if (empty.expectation == 0) {
    # If 0, fill NAs with 0
    exp_matrix[is.na(exp_matrix)] <- 0
  } else {
    # If NULL, fill NAs with 1e-04
    exp_matrix[is.na(exp_matrix)] <- 1e-4
  }

  return(
    list(
      exp = exp_matrix,
      dummy = get0("dum_matrix"),
      settings = list("catch" = catch_name, 
                      "price" = price, 
                      "defineGroup" = defineGroup, 
                      "temp.var" = temp.var, 
                      "temporal" = temporal, 
                      "calc.method" = calc.method, 
                      "lag.method" = lag.method, 
                      "empty.catch" = empty.catch, 
                      "empty.expectation" = empty.expectation, 
                      "temp.window" = temp.window, 
                      "day.lag" = day.lag, 
                      "year.lag" = year.lag, 
                      "dummy.exp" = dummy.exp, 
                      "weight_avg" = weight_avg)
    )
  )
}