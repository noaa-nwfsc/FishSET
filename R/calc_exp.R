#' Calculate expected catch/revenue
#' 
#' @param dataset Primary data containing information on hauls or trips. Table in FishSET 
#'   database contains the string 'MainDataTable'. 
#' @param alt_name Optional character string. Name of the alternative structure. Defaults to \code{NULL}.
#' @param catch Variable from \code{dataset} containing catch data.
#' @param price Optional, variable from \code{dataset} containing price/value data.  
#'   Price is multiplied against \code{catch} to generated revenue. If revenue exists 
#'   in \code{dataset} and you wish to use this revenue instead of price, then \code{catch} 
#'   must be a vector of 1 of length equal to \code{dataset}. Defaults to \code{NULL}.
#' @param defineGroup Optional, variable from \code{dataset} that defines how to split 
#'   the fleet. Defaults to treating entire dataframe \code{dataset} as a fleet.
#' @param temp_var Optional, temporal variable from \code{dataset}. Set to \code{NULL} 
#'   if temporal patterns in catch should not be considered.
#' @param temporal String, choices are \code{"daily"} or \code{"sequential"}. Should 
#'   time, if \code{temp_var} is defined, be included as a daily timeline or sequential 
#'   order of recorded dates. For daily, catch on dates with no record are filled 
#'   with \code{NA}. The choice affects how the rolling average is calculated. If 
#'   temporal is daily then the window size for average and the temporal lag are 
#'   in days. If sequential, then averaging will occur over the specified number 
#'   of observations, regardless of how many days they represent.
#' @param calc_method String, how catch values are average over window size. Select 
#'   standard average (\code{"standardAverage"}), simple lag regression of means 
#'   (\code{"simpleLag"}), or weights of regressed groups (\code{"weights"})
#' @param temp_window Numeric, temporal window size. If \code{temp_var} is not \code{NULL}, 
#'   set the window size to average catch over. Defaults to 7 (7 days if \code{temporal} 
#'   is \code{"daily"}).
#' @param day_lag Numeric, temporal lag time. If \code{temp_var} is not \code{NULL}, 
#'   how far back to lag \code{temp_window}.
#' @param year_lag If expected catch should be based on catch from previous year(s), 
#'   set \code{year_lag} to the number of years to go back.
#' @param empty_catch String, replace empty catch with \code{NA}, \code{0}, mean 
#'   of all catch (\code{"allCatch"}), or mean of grouped catch (\code{"groupCatch"}).
#' @param empty_expectation Numeric, how to treat empty expectation values. Choices 
#'   are to not replace (\code{NULL}) or replace with 0.0001 or 0.
#' @param dummy_exp Logical, should a dummy variable be created? If \code{TRUE}, 
#'   output dummy variable for originally missing value. If \code{FALSE}, no dummy 
#'   variable is outputted. Defaults to \code{FALSE}.
#' @param weight_avg Logical, if \code{TRUE} then all observations for a given zone on a 
#'   given date will be included when calculating the mean, thus giving more 
#'   weight to days with more observations in a given zone. If \code{FALSE}, then the 
#'   daily mean for a zone will be calculated prior to calculating the mean across the
#'   time window.
#' @param Alt Alternative choice list loaded from the FishSET database.
#' @keywords internal
#' @importFrom data.table := as.data.table .I .GRP fcoalesce dcast setnames setcolorder setkey .SD data.table set copy rbindlist setorder
#' @importFrom lubridate floor_date year years %m-%
#' @importFrom stats aggregate lm coef na.pass ar.ols predict
#' @return Returns a list containing the expected catch/revenue matrix, 
#'   dummy matrix (if \code{dummy_exp = TRUE}), and list of input args. 
#'
calc_exp <- function(dataset,
                     alt_name = NULL,
                     catch,
                     price = NULL,
                     defineGroup = NULL,
                     temp_var = NULL,
                     temporal = "daily",
                     calc_method = "standardAverage", 
                     temp_window = 7,
                     day_lag = 1,
                     year_lag = 0,
                     empty_catch = NULL,
                     empty_expectation = 1e-4,
                     dummy_exp = FALSE,
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
  if ("fleet" %in% names(dt) && (is_value_empty(defineGroup) || defineGroup != "fleet")) {
    dt[, fleet := NULL] # Drop existing column to prevent type coercion
  }
  
  if (is_value_empty(defineGroup)) {
    dt[, fleet := 1L]
  } else {
    # If defineGroup == "fleet", this groups by the original string column and safely overwrites it
    dt[, fleet := .GRP, by = defineGroup]
  }
  
  # Subset to relevant data zone and create core calculation table
  z_ind <- which(dataZoneTrue == 1)
  
  # Non temporal ----------------------------------------------------------------------------------
  if (is_value_empty(temp_var) || tolower(temp_var) == "none") {
    # Select columns
    df_no_temp <- dt[z_ind, c("fleet", catch), with = FALSE]
    
    # Rename and add necessary columns
    setnames(df_no_temp, catch, "catch_val")
    df_no_temp[, ':=' (
      zones = as.character(choice[z_ind]),
      catch_val = as.numeric(catch_val)
    )]
    
    # Handle revenue calculation
    if (!is_value_empty(price) && tolower(price) != "none") {
      price_vec <- as.numeric(dataset[[price]][z_ind])
      df_no_temp[, catch_val := catch_val * price_vec]
    }
    
    # Calculate overall mean per group/area
    if (is_value_empty(defineGroup)) {
      allCatch <- df_no_temp[, .(mean_catch = mean(catch_val, na.rm = TRUE)), by = zones]
      allCatch[, fleet := 1L] # Add dummy fleet for consistent joining
      
    } else {
      allCatch <- df_no_temp[, .(mean_catch = mean(catch_val, na.rm = TRUE)), by = .(fleet, zones)]
    }
    
    # Create a skeleton of all observations and all unique zones
    unique_zones <- unique(df_no_temp$zones)
    skeleton <- as.data.table(expand.grid(occasion_id = dt$occasion_id, 
                                          zones = unique_zones, 
                                          stringsAsFactors = FALSE))
    
    # Join the fleet assignment from the original data
    skeleton[dt, fleet := i.fleet, on = "occasion_id"]
    
    # Join the expected catch onto the skeleton
    skeleton[allCatch, exp_val := i.mean_catch, on = c("fleet", "zones")]
    
    # Reshape to wide format: N_obs rows, Zones as columns
    exp_wide <- dcast(skeleton, occasion_id ~ zones, value.var = "exp_val")
    setorder(exp_wide, occasion_id) # Ensure original row order is maintained
    
    # Convert to matrix (dropping the occasion_id column)
    exp_matrix <- as.matrix(exp_wide[, -"occasion_id", with = FALSE])

    
  # Temporal --------------------------------------------------------------------------------------
  } else {
    # Prepare data for temporal calculations
    df <- dt[z_ind, c("occasion_id", "fleet", catch, temp_var), with = FALSE]
    
    setnames(df, c(catch, temp_var), c("catch_val", "date"))
    
    df[, ':='(
      zones = as.character(choice[z_ind]),
      date = date_parser(date),
      catch_val = as.numeric(catch_val)
    )]
  
    # Handle revenue calculation
    if (!is_value_empty(price) && tolower(price) != "none") {
      price_vec <- as.numeric(dataset[[price]][z_ind])
      df[, catch_val := catch_val * price_vec]
    }
    
    df[, dateFloor := floor_date(date, unit = "day")]
    
    df[, ID := if (is_value_empty(defineGroup)) as.character(zones) else paste0(fleet, zones)]
  
    # Handle empty catch values with joins
    if (!is_value_empty(empty_catch) && anyNA(df$catch_val)) {
      if (empty_catch == 0) {
        df[is.na(catch_val), catch_val := 0]
        
      } else if (empty_catch == "allCatch") {
        df[, year := lubridate::year(dateFloor)]
        yearly_means <- df[, .(mean_c = mean(catch_val, na.rm = TRUE)), by = year]
        df[yearly_means, on = "year", catch_val := fcoalesce(catch_val, i.mean_c)]
        
      } else if (empty_catch  == "groupCatch") {
        df[, year := lubridate::year(dateFloor)]
        grouped_means <- df[, .(mean_c = mean(catch_val, na.rm = TRUE)), by = .(year, fleet)]
        df[grouped_means, on = c("year", "fleet"), catch_val := fcoalesce(catch_val, i.mean_c)]
      }
    }
  
    all_dates_in_range <- seq(from = min(df$dateFloor), to = max(df$dateFloor), by = "day")
    
    # Daily = TRUE, Sequential = FALSE
    temporal_daily = (temporal == "daily")
  
    # Moving window average
    if (calc_method == "standardAverage") {
      exp_values <- calculate_moving_avg(
        unique_dates = all_dates_in_range,
        unique_groups =  unique(df$ID), 
        obs_dates =  df$dateFloor,
        obs_groups =  df$ID,
        obs_values =  df$catch_val, 
        window_size =  temp_window, 
        lag = day_lag,
        year_lag = year_lag,
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
        lag_p = day_lag
      )
    }
  
    # Convert the returned exp_values to long format
    exp_values_df <- as.data.frame(exp_values)
    exp_values_df$date <- as.Date(rownames(exp_values_df))
    
    exp_long <- as.data.table(exp_values_df)
    exp_long <- data.table::melt(exp_long, 
                                 id.vars = "date", 
                                 variable.name = "ID", 
                                 value.name = "exp_val")
  
    # Extract fleet and zone mapping from the original ID
    id_map <- unique(df[, .(ID, fleet, zones)])
    exp_long[id_map, c("fleet", "zones") := .(i.fleet, i.zones), on = "ID"]
    
    # Create a skeleton for all observations and all unique zones
    unique_zones <- unique(df$zones)
    skeleton <- as.data.table(expand.grid(occasion_id = dt$occasion_id, 
                                          zones = unique_zones, 
                                          stringsAsFactors = FALSE))
    
    # Ensure the main dt has dateFloor for matching missing out-of-zone data
    dt[, temp_date := date_parser(get(temp_var))]
    dt[, dateFloor := floor_date(temp_date, unit = "day")]
    
    # Join the date and fleet for each observation from the original dt
    skeleton[dt, `:=`(fleet = i.fleet, dateFloor = i.dateFloor), on = "occasion_id"]
  
    # Join the calculated expectations using date, fleet, and zone
    skeleton[exp_long, exp_val := i.exp_val, on = .(dateFloor = date, fleet, zones)]
    
    # Reshape to wide format: N_obs rows, Zones as columns
    exp_wide <- dcast(skeleton, occasion_id ~ zones, value.var = "exp_val")
    setorder(exp_wide, occasion_id) # Ensure original row order is maintained
    
    # Convert to matrix and enforce alphabetical column order
    exp_matrix <- as.matrix(exp_wide[, -"occasion_id", with = FALSE])
    if (length(colnames(exp_matrix)) > 1) {
      exp_matrix <- exp_matrix[, order(colnames(exp_matrix))]  
    }
  }
  
  # Dummy matrix for catch ------------------------------------------------------------------------
  if (dummy_exp) {
    dum_matrix <- (!is.na(exp_matrix)) * 1
    
  } else {
    dum_matrix <- NULL
  }
  
  # Handle empty expectation values ---------------------------------------------------------------
  if (!is.null(empty_expectation)) {
    if (empty_expectation == 1e-4) {
      # If 1e-04, fill both NAs and 0s with 1e-04
      exp_matrix[is.na(exp_matrix)] <- 1e-4
      exp_matrix[exp_matrix == 0] <- 1e-4
      
    } else if (empty_expectation == 0) {
      # If 0, fill NAs with 0
      exp_matrix[is.na(exp_matrix)] <- 0
    }
  }
  # If empty_expectation is NULL, NAs are intentionally left as NAs
  
  return(
    list(
      exp = exp_matrix,
      dummy = get0("dum_matrix"),
      settings = list("catch" = catch_name, 
                      "alt_name" = alt_name,
                      "price" = price, 
                      "defineGroup" = defineGroup, 
                      "temp_var" = temp_var, 
                      "temporal" = temporal, 
                      "calc_method" = calc_method, 
                      "empty_catch" = empty_catch, 
                      "empty_expectation" = empty_expectation, 
                      "temp_window" = temp_window, 
                      "day_lag" = day_lag, 
                      "year_lag" = year_lag, 
                      "dummy_exp" = dummy_exp, 
                      "weight_avg" = weight_avg)
    )
  )
}