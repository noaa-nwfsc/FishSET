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
#' @param temp.lag temporal lag
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
                     temp.lag = 0,
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
                                "temp.lag" = temp.lag, 
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
  
  altc_names <- if (is_value_empty(defineGroup)) unique(df$zones) else unique(df$ID)
  unique_dates <- unique(df$dateFloor)
  
  ## Core moving window calculation ----
  results_list <- lapply(unique_dates, function(current_date) {
    
    window_end <- (current_date - years(year.lag)) - temp.lag
    window_start <- window_end - temp.window + 1
    
    # Subset data for the current window
    data_in_window <- df[dateFloor >= window_start & dateFloor <= window_end]
    
    local_res <- if (nrow(data_in_window) == 0) {
      # If no data in window, create a table with NA for all zones
      data.table(ID = altc_names, exp_catch = NA_real_)
    } else {
      if (weight_avg) {
        # Average all points in the window
        data_in_window[, .(exp_catch = mean(catch_val, na.rm = TRUE)), by = ID]
      } else {
        # Two-step average: first by day, then average daily means
        daily_means <- data_in_window[, .(daily_mean = mean(catch_val, na.rm = TRUE)), by = .(ID, dateFloor)]
        daily_means[, .(exp_catch = mean(daily_mean, na.rm = TRUE)), by = ID]
      }
    }
    
    # Ensure all possible zones are present for this date using a merge
    template <- data.table(ID = altc_names, key = "ID")
    final_res_for_date <- merge(template, local_res, by = "ID", all.x = TRUE)
    
    final_res_for_date[, dateFloor := current_date]
    return(final_res_for_date)
  })
  ec_small_long <- data.table::rbindlist(results_list)
  
  # Reshape to wide matrix and do some post-processing
  ec_small <- dcast(ec_small_long, dateFloor ~ ID, value.var = "exp_catch")

  missing_cols <- setdiff(altc_names, names(ec_small))
  
  if (length(missing_cols) > 0) {
    ec_small[, (missing_cols) := NA_real_]
  }
  setcolorder(ec_small, c("dateFloor", sort(altc_names)))
  
  if (calc.method == "simpleLag") {
    ec_mat_lag <- as.matrix(ec_small[, -1])
    
    for (i in seq_len(ncol(ec_mat_lag))) {
      ec_area <- ec_mat_lag[, i]
      if (sum(!is.na(ec_area)) < 2) next
      dwa_1 <- ec_area[-length(ec_area)]
      dwa_2 <- ec_area[-1]
      dwa_mod <- tryCatch(lm(dwa_1 ~ dwa_2), error = function(e) NULL)
      if (!is.null(dwa_mod)) {
        ec_mat_lag[, i] <- predict(dwa_mod, newdata = data.frame(dwa_2 = ec_area))
      }
    }
    
    ec_small[, (sort(altc_names)) := as.data.table(ec_mat_lag)]
  }
  
  # Finalize output
  occasions <- df[, .(occasion_id, dateFloor)]
  
  if (dummy.exp) {
    dum_dt <- data.table::copy(ec_small)
    
    for (col in altc_names) {
      data.table::set(dum_dt, j = col, value = as.numeric(!is.na(dum_dt[[col]])))
    }
    
    final_dummy <- dum_dt[occasions, on = "dateFloor"]
    setkey(final_dummy, occasion_id)
    dum_matrix <- as.matrix(final_dummy[, .SD, .SDcols = sort(altc_names)])
    
  } else {
    dum_matrix <- NULL
  }
  
  setkey(ec_small, dateFloor)
  final_exp <- ec_small[occasions, on = "dateFloor"]
  
  if (is.null(empty.expectation)) {
    # If NULL, fill NAs with 1e-04
    for (col in altc_names) {
      # Use fcoalesce for a fast, in-place replacement of NAs
      final_exp[, (col) := fcoalesce(final_exp[[col]], 1e-04)]
    }
  } else if (empty.expectation == 1e-04) {
    # If 1e-04, fill both NAs and 0s with 1e-04
    for (col in altc_names) {
      final_exp[is.na(get(col)) | get(col) == 0, (col) := 1e-04]
    }
  } else if (empty.expectation == 0) {
    # If 0, fill NAs with 0
    for (col in altc_names) {
      final_exp[, (col) := fcoalesce(final_exp[[col]], 0)]
    }
  }
  
  setkey(final_exp, occasion_id)
  exp_matrix <- as.matrix(final_exp[, .SD, .SDcols = sort(altc_names)])
  
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
                      "temp.lag" = temp.lag, 
                      "year.lag" = year.lag, 
                      "dummy.exp" = dummy.exp, 
                      "weight_avg" = weight_avg)
    )
  )
  
  
  
  
  # # TODO: check if this is needed
  # fleetNAN <- which(is.nan(fleet))
  # 
  # if (any(!is_empty(fleetNAN))) {
  #   
  #   fleet[fleetNAN] <- Inf
  # }
  # 
  # catch <- as.numeric(dataset[[catch]][z_ind])
  # 
  # if (price != "none" && !is_value_empty(price)) {
  #   
  #   price <- as.numeric(dataset[[price]][z_ind])
  #   catch <- catch * price
  # }
  # 
  # # no time var ----
  # # Time variable not chosen if temp.var is empty
  # # NOTE currently doesn't allow dummy or other options if no time detected
  # if (temp.var == "none" || is_value_empty(temp.var)) {
  #   
  #   allCatch <- stats::aggregate(catch, by = list(areas = areas, fleet = fleet), 
  #                                FUN = mean, na.rm = TRUE) 
  #   # TODO: update for groups
  #   exp_matrix <- matrix(allCatch$x, nrow = nrow(dataset), ncol = length(altc_areas),
  #                        dimnames = list(NULL, allCatch$areas), byrow = TRUE)
  #   
  #   newDumV <- list()
  #   
  # } else {
  #   
  #   # with time var ----
  #   date <- date_parser(dataset[[temp.var]][z_ind]) 
  #   
  #   dateFloor <- lubridate::floor_date(date, unit = "day")
  #   
  #   if (temporal == "daily") { # check if temp.var exists
  #     
  #     tLine <- seq.Date(from = min(dateFloor, na.rm=TRUE), to = max(dateFloor, na.rm=TRUE), by = "day")
  #     
  #     # fill in missing days 
  #     missing_days <- tLine[!tLine %in% dateFloor]
  #     
  #     if (length(missing_days) > 0) {
  #       
  #       missing_days_df <- 
  #         expand.grid(fleet = unique(fleet), areas = altc_areas,
  #                     catch = NA, dateFloor = missing_days)
  #     }
  #     
  #   } else if (temporal == "sequential") {
  #     # observation time line
  #     tLine <- unique(sort(dateFloor))
  #   }
  #   
  #   df <- data.frame(fleet = fleet,
  #                    areas = as.character(areas), 
  #                    catch = as.numeric(catch),
  #                    dateFloor = dateFloor)
  #   
  #   if (temporal == "daily" && length(missing_days) > 0) {
  #     
  #     df <- rbind(df, missing_days_df)
  #   }
  #   
  #   df <- df[order(df$dateFloor), ]
  #   
  #   #  ID = fleet + zone
  #   df$ID <- paste0(df$fleet, df$areas)
  #   
  #   # Lag time
  #   if (sum(duplicated(df$ID)) == 0) {
  #     
  #     temp.lag <- 0
  #     warning("Selected groups and choice data results in only single observations. ",
  #             "Cannot use lag time for chosen group and choice data. Setting lag time to 0.",
  #             call. = FALSE)
  #     
  #   } else if ((sum(duplicated(df$ID)) / length(df$ID)) < .25) {
  #     
  #     temp.lag <- 0
  #     warning(paste0(
  #       "Selected groups and choice data results in ", 
  #       sum(duplicated(df$ID) == FALSE) / length(df$ID) * 100,
  #       "% of observations with only single observations. Cannot use lag time for ",
  #       "choosen group and choice data. Setting lag time to 0."),
  #       call. = FALSE)
  #   }
  #   
  #   # empty catch ----
  #   
  #   na_ind <- is.na(df$catch)
  #   
  #   if (sum(na_ind) > 0) {
  #     
  #     # Note: Is NA necessary/desirable? Depends on how it's treated later in 
  #     # modeling process.
  #     if (is.na(empty.catch) || is_value_empty(empty.catch)) {
  #       
  #       df$catch[na_ind] <- NA  
  #       
  #     } else if (empty.catch == 0) {
  #       
  #       df$catch[na_ind] <- 0
  #       
  #     } else if (empty.catch == "allCatch") {
  #       # TODO: make this more efficient
  #       # Q: ave yearly catch across all areas or by area?
  #       # Note: allow user to make this choice. No right answer for default
  #       year_all <- lubridate::year(df$date)
  #       
  #       yr <- aggregate(df$catch, by = list(year = year_all), 
  #                       FUN = mean, na.rm = TRUE)
  #       
  #       for (i in seq_along(yr$year)) {
  #         
  #         df[na_ind & year_all == yr$year[i], "catch"] <- yr$x[i]
  #       }
  #       
  #     } else if (empty.catch == "groupedCatch") {
  #       # average catch for year and fleet
  #       
  #       # Q: fleet's ave yearly catch across all areas or by area?
  #       # Note: allow user to make this choice. No right answer for default
  #       # Do both? Show user output?
  #       # old function (group by year, fleet, and area)
  #       myfunc_GC <- function(x, y) {
  #         
  #         year_all <- lubridate::year(df$date)
  #         x_year <- lubridate::year(x)
  #         ind <- year_all == x_year & df$ID == y
  #         
  #         mean(df$catch[ind], na.rm = TRUE)
  #       }
  #       
  #       # current version (group by year and fleet)
  #       year_all <- lubridate::year(df$date)
  #       
  #       yr <- aggregate(df$catch, 
  #                       by = list(year = year_all, fleet = df$fleet), 
  #                       FUN = mean, na.rm = TRUE)
  #       
  #       # TODO: make this more efficient
  #       for (i in seq_along(yr$year)) {
  #         
  #         ind <- na_ind & year_all == yr$year[i] & df$fleet == yr$fleet[i]
  #         
  #         df[ind, "catch"] <- yr$x[i]
  #       }
  #     }
  #   }
  #   
  #   # Moving window ----
  #   
  #   window_ave <- function(x, weight_avg) {
  #     
  #     ind <-
  #       df$dateFloor >= ((x - lubridate::years(year.lag)) - temp.lag - temp.window + 1) &
  #       df$dateFloor <= ((x - lubridate::years(year.lag)) - temp.lag)
  #     
  #     new_df <- df[ind, ]
  #     new_df$dateFloor = as.character(new_df$dateFloor)
  #     
  #     # TODO: simplify
  #     if (is_value_empty(defineGroup)) {
  #       
  #       if (sum(ind) == 0) { # empty dataframe
  #         
  #         return(data.frame(areas = altc_names, catch = NA))
  #       }
  #       
  #       # Note: could remove fleet
  #       if(weight_avg == FALSE){
  #         # Get the average for each date-area combination prior to calculating the window average
  #         tmp_df <- aggregate(catch ~ areas + fleet + dateFloor, data = new_df,
  #                             FUN = mean, na.action = stats::na.pass, na.rm = TRUE)
  #         
  #         aggregate(catch ~ areas + fleet, data = tmp_df,
  #                   FUN = mean, na.action = stats::na.pass, na.rm = TRUE)
  #       } else {
  #         
  #         aggregate(catch ~ areas + fleet, data = new_df,
  #                   FUN = mean, na.action = stats::na.pass, na.rm = TRUE)
  #       }
  #       
  #     } else {
  #       
  #       if (sum(ind) == 0) { # empty dataframe
  #         # TODO: update for fleet
  #         return(data.frame(ID = altc_names, catch = NA))
  #       }
  #       
  #       if(weight_avg == FALSE){
  #         tmp_df <- aggregate(catch ~ ID + dateFloor, data = new_df,
  #                             FUN = mean, na.action = na.pass, na.rm = TRUE)
  #         
  #         aggregate(catch ~ ID, data = tmp_df,
  #                   FUN = mean, na.action = na.pass, na.rm = TRUE)
  #       } else {
  #         aggregate(catch ~ ID, data = new_df,
  #                   FUN = mean, na.action = na.pass, na.rm = TRUE)
  #       }
  #     }
  #   }
  #   
  #   # list entries contain the window avg for each day
  #   ave_list <- lapply(unique(df$dateFloor), window_ave, weight_avg = weight_avg)
  #   
  #   # add loop to extract values 
  #   
  #   area_nm <- if (is_value_empty(defineGroup)) "areas" else "ID"
  #   
  #   ave_list <- lapply(ave_list, function(i) {
  #     
  #     # return avg for each area. Return NA if missing
  #     i$catch[match(altc_names, i[[area_nm]])]
  #   })
  #   
  #   # combine into "small" exp catch matrix
  #   ec_small <- do.call(rbind, ave_list)
  #   
  #   dimnames(ec_small) <- list(as.character(tLine), altc_names)
  #   
  #   # calc method ----
  #   if (calc.method == "simpleLag") {
  #     # at this point could use means to get a regression compared to a lag of 
  #     # the same calculation at all zones, then use that to predict...
  #     
  #     # need to multiply polys by constant
  #     
  #     if (lag.method == "simple") {
  #       
  #       # TODO: have someone verify that this works correctly
  #       # Note: revisit this
  #       
  #       ec_len <- nrow(ec_small)
  #       
  #       for (i in seq_len(ncol(ec_small))) {
  #         
  #         # dwa = daily window avg
  #         ec_area <- ec_small[, i]
  #         
  #         dwa_1 <- ec_area[-ec_len] # remove last day (DV)
  #         
  #         dwa_2 <- ec_area[-1] # remove first day (IV)
  #         
  #         dwa_mod <- stats::lm(dwa_1 ~ dwa_2)
  #         
  #         pval <- polyval(rev(stats::coef(dwa_mod)), ec_area) # use predict() instead?
  #         
  #         ec_small[, i] <- pval
  #       }
  #       
  #     } else { # grouped
  #       
  #       # TODO: have someone verify that this is working correctly
  #       polys <- ec_small[-1, ] / ec_small[-nrow(ec_small), ]
  #       polys <- rbind(polys, NA)
  #       polys[nrow(polys), ] <- apply(polys, 2, FUN = mean, na.rm = TRUE)
  #       ec_small <- ec_small * polys
  #     }
  #   }
  #   
  #   # TODO: No code for calc.method = weights (not in Matlab code either)
  #   
  #   # dummy ----
  #   exp_dates <- rownames(ec_small)
  #   
  #   if (dummy.exp) {
  #     
  #     dum_matrix <- !is.na(ec_small)
  #     dum_matrix <- apply(dum_matrix, 2, as.numeric)
  #     
  #     dum_matrix <- lapply(date, function(x) {
  #       
  #       ind <- which(exp_dates == x)
  #       dum_matrix[ind, , drop = FALSE]
  #     })
  #     
  #     dum_matrix <- do.call(rbind, dum_matrix)
  #     # order columns
  #     dum_matrix <- dum_matrix[, order(colnames(dum_matrix))]
  #     
  #   } else {
  #     
  #     dum_matrix <- NULL
  #   }
  #   
  #   # empty exp ----
  #   
  #   if (is_value_empty(empty.expectation)) {
  #     
  #     ec_small[is.na(ec_small)] <- 0.0001
  #     
  #   } else if (empty.expectation == 1e-04) {
  #     
  #     ec_small[is.na(ec_small)] <- 1e-04
  #     ec_small[ec_small == 0] <- 1e-04
  #     
  #   } else if (empty.expectation == 0) {
  #     
  #     ec_small[is.na(ec_small)] <- 0
  #     
  #   } else {
  #     # case 'no replacement'
  #     ec_small[is.na(ec_small)] <- 0.0001
  #   }
  #   
  #   # convert to occasion (n obs by altc) ----
  #   exp_matrix <- lapply(as.character(date), function(x) {
  #     ind <- which(exp_dates == x)
  #     ec_small[ind, , drop = FALSE]
  #   })
  #   
  #   exp_matrix <- do.call(rbind, exp_matrix)
  #   # order columns
  #   exp_matrix <- exp_matrix[, order(colnames(exp_matrix))]
  # }
  # 
  # return(list(exp = exp_matrix, 
  #             dummy = get0("dum_matrix"),
  #             settings = list("catch" = catch_name, 
  #                             "price" = price, 
  #                             "defineGroup" = defineGroup, 
  #                             "temp.var" = temp.var, 
  #                             "temporal" = temporal, 
  #                             "calc.method" = calc.method, 
  #                             "lag.method" = lag.method, 
  #                             "empty.catch" = empty.catch, 
  #                             "empty.expectation" = empty.expectation, 
  #                             "temp.window" = temp.window, 
  #                             "temp.lag" = temp.lag, 
  #                             "year.lag" = year.lag, 
  #                             "dummy.exp" = dummy.exp, 
  #                             "weight_avg" = weight_avg))
  # )
}