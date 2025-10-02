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
#'             setcolorder setkey .SD data.table set copy rbindlist
#' @importFrom lubridate floor_date year years %m-%
#' @importFrom stats aggregate lm coef na.pass
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
  
  # ## Core moving window calculation ----
  # results_list <- lapply(unique_dates, function(current_date) {
  #   
  #   window_end <- (current_date - years(year.lag)) - day.lag
  #   window_start <- window_end - temp.window + 1
  #   
  #   # Subset data for the current window
  #   data_in_window <- df[dateFloor >= window_start & dateFloor <= window_end]
  #   
  #   local_res <- if (nrow(data_in_window) == 0) {
  #     # If no data in window, create a table with NA for all zones
  #     data.table(ID = altc_names, exp_catch = NA_real_)
  #   } else {
  #     if (weight_avg) {
  #       # Average all points in the window
  #       data_in_window[, .(exp_catch = mean(catch_val, na.rm = TRUE)), by = ID]
  #     } else {
  #       # Two-step average: first by day, then average daily means
  #       daily_means <- data_in_window[, .(daily_mean = mean(catch_val, na.rm = TRUE)), by = .(ID, dateFloor)]
  #       daily_means[, .(exp_catch = mean(daily_mean, na.rm = TRUE)), by = ID]
  #     }
  #   }
  #   
  #   # Ensure all possible zones are present for this date using a merge
  #   template <- data.table(ID = altc_names, key = "ID")
  #   final_res_for_date <- merge(template, local_res, by = "ID", all.x = TRUE)
  #   
  #   final_res_for_date[, dateFloor := current_date]
  #   return(final_res_for_date)
  # })
  
  # unique_dates <- unique(df$dateFloor)
  
  altc_names <- if (is_value_empty(defineGroup)) unique(df$zones) else unique(df$ID)
  all_dates_in_range <- seq(from = min(df$dateFloor), to = max(df$dateFloor), by = "day")
  
  moving_avg_catch <- calculate_moving_avg(
    all_dates_in_range,
    unique(df$ID), 
    df$dateFloor,
    df$ID,
    df$catch_val, 
    temp.window, 
    day.lag
  )
  
  moving_avg_catch <- as.data.frame(moving_avg_catch) %>% 
    mutate(date = as.Date(rownames(moving_avg_catch)))
  
  moving_avg_catch <- data.frame(date = df$dateFloor) %>%
    left_join(., moving_avg_catch, by = "date") %>%
    dplyr::rename(dateFloor = date)
  
  # ec_small_long <- data.table::rbindlist(results_list)
  # # Reshape to wide matrix and do some post-processing
  # ec_small <- dcast(ec_small_long, dateFloor ~ ID, value.var = "exp_catch")

  missing_cols <- setdiff(altc_names, names(moving_avg_catch))
  
  if (length(missing_cols) > 0) {
    moving_avg_catch[, (missing_cols) := NA_real_]
  }
  
  setcolorder(moving_avg_catch, c("dateFloor", sort(altc_names)))
  
  # if (calc.method == "simpleLag") {
  #   ec_mat_lag <- as.matrix(ec_small[, -1])
  #   
  #   for (i in seq_len(ncol(ec_mat_lag))) {
  #     ec_area <- ec_mat_lag[, i]
  #     if (sum(!is.na(ec_area)) < 2) next
  #     dwa_1 <- ec_area[-length(ec_area)]
  #     dwa_2 <- ec_area[-1]
  #     dwa_mod <- tryCatch(lm(dwa_1 ~ dwa_2), error = function(e) NULL)
  #     if (!is.null(dwa_mod)) {
  #       ec_mat_lag[, i] <- predict(dwa_mod, newdata = data.frame(dwa_2 = ec_area))
  #     }
  #   }
  #   
  #   ec_small[, (sort(altc_names)) := as.data.table(ec_mat_lag)]
  # }
  
  # Finalize output
  # occasions <- df[, .(occasion_id, dateFloor)]
  
  # if (dummy.exp) {
  #   dum_dt <- data.table::copy(ec_small)
  #   
  #   for (col in altc_names) {
  #     data.table::set(dum_dt, j = col, value = as.numeric(!is.na(dum_dt[[col]])))
  #   }
  #   
  #   final_dummy <- dum_dt[occasions, on = "dateFloor"]
  #   setkey(final_dummy, occasion_id)
  #   dum_matrix <- as.matrix(final_dummy[, .SD, .SDcols = sort(altc_names)])
  #   
  # } else {
  #   dum_matrix <- NULL
  # }
  
  # setkey(ec_small, dateFloor)
  # final_exp <- ec_small[occasions, on = "dateFloor"]
  
  # if (is.null(empty.expectation)) {
  #   # If NULL, fill NAs with 1e-04
  #   for (col in altc_names) {
  #     # Use fcoalesce for a fast, in-place replacement of NAs
  #     final_exp[, (col) := fcoalesce(final_exp[[col]], 1e-04)]
  #   }
  # } else if (empty.expectation == 1e-04) {
  #   # If 1e-04, fill both NAs and 0s with 1e-04
  #   for (col in altc_names) {
  #     final_exp[is.na(get(col)) | get(col) == 0, (col) := 1e-04]
  #   }
  # } else if (empty.expectation == 0) {
  #   # If 0, fill NAs with 0
  #   for (col in altc_names) {
  #     final_exp[, (col) := fcoalesce(final_exp[[col]], 0)]
  #   }
  # }
  
  # setkey(final_exp, occasion_id)
  # exp_matrix <- as.matrix(final_exp[, .SD, .SDcols = sort(altc_names)])
  exp_matrix <- moving_avg_catch %>%
    select(!dateFloor)
  
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