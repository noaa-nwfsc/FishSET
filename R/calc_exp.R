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
  # TODO: Finish documenting function
  
  # TODO: when revenue col exists, either automatically create col of ones (currently user must do this) 
  # or allow catch arg to also be revenue. Use generic name (e.g. value) 
  
  catch_name <- catch # save variable name for settings
  
  dataZoneTrue <- Alt[["dataZoneTrue"]] # used for catch and other variables
  choice <- Alt[["choice"]] # used for catch and other variables
  
  # Random grid sampling arguments
  grid_sample <- Alt[["grid_sample"]] # TRUE indicates randomly sampling zones
  grid_sample_n <- Alt[["grid_sample_n"]] # Number of zones to sample
  rand_alts_mat <- Alt[["rand_alts_mat"]] # Randomly sampled zones
  
  # user-defined ----
  # check whether defining a group or using all fleet averaging
  if (is_value_empty(defineGroup)) {
    # just use an id=ones to get all info as one group
    fleet <- rep(1, nrow(dataset))
    
    # Define by group case
  } else {
    fleet <- as.integer(as.factor(dataset[[defineGroup]]))
  }
  
  z_ind <- which(dataZoneTrue == 1) # index for which observations are included in the model
  fleet <- fleet[z_ind]
  
  areas <- choice[z_ind] # mapping to to the map file (zones)
  
  if(!grid_sample){ # aggregating by zones
    altc_areas <- as.character(unique(areas)) 
    altc_fleet <- unique(paste0(fleet, areas))  
  } else { # random sampling from grid
    altc_areas <- as.character(unique(as.vector(rand_alts_mat))) 
    altc_fleet <- unique(paste0(fleet, areas))  
  }
  
  altc_names <- if (is_value_empty(defineGroup)) altc_areas else altc_fleet
  
  areaNA <- which(is.na(areas))
  
  # TODO: check if this is needed 
  if (any(!is_empty(areaNA))) {
    
    areas[areaNA] <- Inf # better for grouping than nas because aggregated
  }
  
  # TODO: check if this is needed
  fleetNAN <- which(is.nan(fleet))
  
  if (any(!is_empty(fleetNAN))) {
    
    fleet[fleetNAN] <- Inf
  }
  
  catch <- as.numeric(dataset[[catch]][z_ind])
  
  if (price != "none" && !is_value_empty(price)) {
    price <- as.numeric(dataset[[price]][z_ind])
    catch <- catch * price
  }
  
  # no time var ----
  # Time variable not chosen if temp.var is empty
  # NOTE currently doesn't allow dummy or other options if no time detected
  if (temp.var == "none" || is_value_empty(temp.var)) {
    
    allCatch <- stats::aggregate(catch, by = list(areas = areas, fleet = fleet), 
                                 FUN = mean, na.rm = TRUE) 
    # TODO: update for groups
    exp_matrix <- matrix(allCatch$x, nrow = nrow(dataset), ncol = length(altc_areas),
                         dimnames = list(NULL, allCatch$areas), byrow = TRUE)
    
    newDumV <- list()
    
  } else {
    
    # with time var ----
    date <- date_parser(dataset[[temp.var]][z_ind]) 
    
    dateFloor <- lubridate::floor_date(date, unit = "day")
    
    if (temporal == "daily") { # check if temp.var exists
      
      tLine <- seq.Date(from = min(dateFloor), to = max(dateFloor), by = "day")
      
      # fill in missing days 
      missing_days <- tLine[!tLine %in% dateFloor]
      
      if (length(missing_days) > 0) {
        
        missing_days_df <- 
          expand.grid(fleet = unique(fleet), areas = altc_areas,
                      catch = NA, dateFloor = missing_days)
      }
      
    } else if (temporal == "sequential") {
      # observation time line
      tLine <- unique(sort(dateFloor))
    }
    
    df <- data.frame(fleet = fleet,
                     areas = as.character(areas), 
                     catch = as.numeric(catch),
                     dateFloor = dateFloor)
    
    if (temporal == "daily" && length(missing_days) > 0) {
      
      df <- rbind(df, missing_days_df)
    }
    
    df <- df[order(df$dateFloor), ]
    
    #  ID = fleet + zone
    df$ID <- paste0(df$fleet, df$areas)
    
    # Lag time
    if (sum(duplicated(df$ID)) == 0) {
      
      temp.lag <- 0
      warning("Selected groups and choice data results in only single observations. ",
              "Cannot use lag time for chosen group and choice data. Setting lag time to 0.",
              call. = FALSE)
      
    } else if ((sum(duplicated(df$ID)) / length(df$ID)) < .25) {
      
      temp.lag <- 0
      warning(paste0(
        "Selected groups and choice data results in ", 
        sum(duplicated(df$ID) == FALSE) / length(df$ID) * 100,
        "% of observations with only single observations. Cannot use lag time for ",
        "choosen group and choice data. Setting lag time to 0."),
        call. = FALSE)
    }
    
    # empty catch ----
    
    na_ind <- is.na(df$catch)
    
    if (sum(na_ind) > 0) {
      
      # Note: Is NA necessary/desirable? Depends on how it's treated later in 
      # modeling process.
      if (is.na(empty.catch) || is_value_empty(empty.catch)) {
        
        df$catch[na_ind] <- NA  
        
      } else if (empty.catch == 0) {
        
        df$catch[na_ind] <- 0
        
      } else if (empty.catch == "allCatch") {
        # TODO: make this more efficient
        # Q: ave yearly catch across all areas or by area?
        # Note: allow user to make this choice. No right answer for default
        year_all <- lubridate::year(df$date)
        
        yr <- aggregate(df$catch, by = list(year = year_all), 
                        FUN = mean, na.rm = TRUE)
        
        for (i in seq_along(yr$year)) {
          
          df[na_ind & year_all == yr$year[i], "catch"] <- yr$x[i]
        }
        
      } else if (empty.catch == "groupedCatch") {
        # average catch for year and fleet
        
        # Q: fleet's ave yearly catch across all areas or by area?
        # Note: allow user to make this choice. No right answer for default
        # Do both? Show user output?
        # old function (group by year, fleet, and area)
        myfunc_GC <- function(x, y) {
          
          year_all <- lubridate::year(df$date)
          x_year <- lubridate::year(x)
          ind <- year_all == x_year & df$ID == y
          
          mean(df$catch[ind], na.rm = TRUE)
        }
        
        # current version (group by year and fleet)
        year_all <- lubridate::year(df$date)
        
        yr <- aggregate(df$catch, 
                        by = list(year = year_all, fleet = df$fleet), 
                        FUN = mean, na.rm = TRUE)
        
        # TODO: make this more efficient
        for (i in seq_along(yr$year)) {
          
          ind <- na_ind & year_all == yr$year[i] & df$fleet == yr$fleet[i]
          
          df[ind, "catch"] <- yr$x[i]
        }
      }
    }
    
    # Moving window ----
    
    ################################################################################################################
    ################################################################################################################
    # TRY NEW METHOD FOR CALC EXP CATCH
    
    # Notes: 
    # - This currently works for the "daily" timeline, but need to make some adjustments for "sequential" timeline.
    #
    
    # Get a single observation and the corresponding alternatives
    # NEED TO LOOP THROUGH THESE DATA, but could also put this in an lapply()
    
    # lapply(1:nrow(dataset), function(tmp_dataset, tmp_alts, i){
    microbenchmark::microbenchmark(
      "test1" = {
        tmp_exp_mat <- lapply(1:nrow(dataset), function(tmp_dataset, tmp_alts, temp.var, year.lag, temp.lag, temp.window, weight_avg, i){
          irow_dataset <- tmp_dataset[i,]
          irow_alts <- tmp_alts[i,]
          
          tmp_dataset <- tmp_dataset %>%
            filter(tmp_dataset$ZoneID %in% irow_alts) %>% # BASE SUBSETTING IS FASTER THAN DPLYR dataset[dataset$ZoneID %in% tmp_alts,] # NEED TO ADD ZONE ID AS INPUT
            filter(.[[temp.var]] >= (irow_dataset[[temp.var]] - lubridate::years(year.lag)) - temp.lag - temp.window + 1) %>%
            filter(.[[temp.var]] <= (irow_dataset[[temp.var]] - lubridate::years(year.lag)) - temp.lag) %>%
            group_by(ZoneID) %>%
            summarize(expected_val = mean(landed_thousands, na.rm = TRUE)) %>%
            ungroup() %>% 
            left_join(data.frame(ZoneID = irow_alts), ., by = "ZoneID") %>%
            select(expected_val)
          
          as.vector(tmp_dataset$expected_val)
        }, 
        tmp_dataset = dataset, 
        tmp_alts = rand_alts_mat, 
        temp.var = temp.var, 
        year.lag = year.lag,
        temp.lag = temp.lag,
        temp.window = temp.window,
        weight_avg = weight_avg)
      },
      
      "test2" = {
        
      }
    )
    
    
    # tmp_exp_mat <- lapply(1:nrow(dataset), function(tmp_dataset, tmp_alts, temp.var, year.lag, temp.lag, temp.window, weight_avg, i){
    #   irow_dataset <- tmp_dataset[i,]
    #   irow_alts <- tmp_alts[i,]
    #   
    #   tmp_dataset <- tmp_dataset %>%
    #     filter(tmp_dataset$ZoneID %in% irow_alts) %>% # BASE SUBSETTING IS FASTER THAN DPLYR dataset[dataset$ZoneID %in% tmp_alts,] # NEED TO ADD ZONE ID AS INPUT
    #     filter(.[[temp.var]] >= (irow_dataset[[temp.var]] - lubridate::years(year.lag)) - temp.lag - temp.window + 1) %>%
    #     filter(.[[temp.var]] <= (irow_dataset[[temp.var]] - lubridate::years(year.lag)) - temp.lag) %>%
    #     group_by(ZoneID) %>%
    #     summarize(expected_val = mean(landed_thousands, na.rm = TRUE)) %>%
    #     ungroup() %>% 
    #     left_join(data.frame(ZoneID = irow_alts), ., by = "ZoneID") %>%
    #     select(expected_val)
    #   
    #   as.vector(tmp_dataset$expected_val)
    # }, 
    # tmp_dataset = dataset, 
    # tmp_alts = rand_alts_mat, 
    # temp.var = temp.var, 
    # year.lag = year.lag,
    # temp.lag = temp.lag,
    # temp.window = temp.window,
    # weight_avg = weight_avg)
    # toc()
    
    tmp_exp_mat <- do.call(rbind, tmp_exp_mat)
    
    
    tmp_exp_mat <- lapply(1:nrow(dataset), function(tmp_dataset, tmp_alts, temp.var, year.lag, temp.lag, temp.window, weight_avg, i){
      
    },
    tmp_dataset = dataset, 
    tmp_alts = rand_alts_mat, 
    temp.var = temp.var, 
    year.lag = year.lag,
    temp.lag = temp.lag,
    temp.window = temp.window,
    weight_avg = weight_avg
    )
      
        test_time2 <- dataset[dataset$ZoneID %in% tmp_alts,]
        test_time2 <- test_time2[test_time2[[temp.var]] >= start_dates_i & test_time2[[temp.var]] <= end_dates_i, ]
        expected_val <- tapply(test_time2$landed_thousands, test_time2$ZoneID, mean, na.rm = TRUE)
        expected_val_result <- expected_val[as.character(tmp_alts)]
      
    
    
    
    
    start_dates <- dataset[[temp.var]] - lubridate::years(year.lag) - temp.lag - temp.window + 1
    end_dates <- dataset[[temp.var]] - lubridate::years(year.lag) - temp.lag
    start_dates_i <- start_dates[i]
    end_dates_i <- end_dates[i]
    
    i <- 88
    tmp_x <- dataset[i,]
    tmp_alts <- rand_alts_mat[i,]
    weight_avg <- weight_avg
    
    # Filter data for zones and dates
    tmp1 <- dataset %>%
      filter(dataset$ZoneID %in% tmp_alts) %>% # NEED TO ADD ZONE ID AS INPUT
      filter(.[[temp.var]] >= (tmp_x[[temp.var]] - lubridate::years(year.lag)) - temp.lag - temp.window + 1) %>%
      filter(.[[temp.var]] <= (tmp_x[[temp.var]] - lubridate::years(year.lag)) - temp.lag) %>% 
      group_by(ZoneID) %>%
      summarize(exp_val = mean(landed_thousands, na.rm = TRUE)) %>%
      ungroup() %>% 
      left_join(data.frame(ZoneID = tmp_alts), ., by = "ZoneID")
    
    tmp_exp_mat[i,] <- tmp1$exp_val 
    
    
    
    
    ################################################################################################################
    ################################################################################################################
    

    window_ave <- function(x, weight_avg) {
      
      #TODO: Add radius search for grid sample method
      #TODO: For grid sampling, we could omit date/zone combinations that are not included as alternatives?
      
      ind <-
        df$dateFloor >= ((x - lubridate::years(year.lag)) - temp.lag - temp.window + 1) &
        df$dateFloor <= ((x - lubridate::years(year.lag)) - temp.lag)
      
      new_df <- df[ind, ]
      new_df$dateFloor = as.character(new_df$dateFloor)
      
      # TODO: simplify
      if (is_value_empty(defineGroup)) {

        if (sum(ind) == 0) { # empty dataframe

          return(data.frame(areas = altc_names, catch = NA))
        }

        # Note: could remove fleet
        if(weight_avg == FALSE){
          # Get the average for each date-area combination prior to calculating the window average
          tmp_df <- aggregate(catch ~ areas + fleet + dateFloor, data = new_df,
                              FUN = mean, na.action = stats::na.pass, na.rm = TRUE)
          
          aggregate(catch ~ areas + fleet, data = tmp_df,
                    FUN = mean, na.action = stats::na.pass, na.rm = TRUE)
        } else {

          aggregate(catch ~ areas + fleet, data = new_df,
                    FUN = mean, na.action = stats::na.pass, na.rm = TRUE)
        }

      } else {

        if (sum(ind) == 0) { # empty dataframe
          # TODO: update for fleet
          return(data.frame(ID = altc_names, catch = NA))
        }

        if(weight_avg == FALSE){
          tmp_df <- aggregate(catch ~ ID, dateFloor, data = new_df,
                              FUN = mean, na.action = na.pass, na.rm = TRUE)

          aggregate(catch ~ ID, data = tmp_df,
                    FUN = mean, na.action = na.pass, na.rm = TRUE)
        } else {
          aggregate(catch ~ ID, data = new_df,
                    FUN = mean, na.action = na.pass, na.rm = TRUE)
        }
      }
    }
    
    # list entries contain the window avg for each day
    ## TODO: NO NEED TO RUN THE LAPPLY FUNCTION FOR DATES WITHOUT ANY CATCH DATA
    tic()
    ave_list <- lapply(unique(df$dateFloor), window_ave, weight_avg = weight_avg)
    toc()
    
    # add loop to extract values 
    
    area_nm <- if (is_value_empty(defineGroup)) "areas" else "ID"
    
    ave_list <- lapply(ave_list, function(i) {
      
      # return avg for each area. Return NA if missing
      i$catch[match(altc_names, i[[area_nm]])]
    })
    
    # combine into "small" exp catch matrix
    ec_small <- do.call(rbind, ave_list)
    
    dimnames(ec_small) <- list(as.character(tLine), altc_names)
    
    # calc method ----
    if (calc.method == "simpleLag") {
      # at this point could use means to get a regression compared to a lag of 
      # the same calculation at all zones, then use that to predict...
      
      # need to multiply polys by constant
      
      if (lag.method == "simple") {
        
        # TODO: have someone verify that this works correctly
        # Note: revisit this
        
        ec_len <- nrow(ec_small)
        
        for (i in seq_len(ncol(ec_small))) {
          
          # dwa = daily window avg
          ec_area <- ec_small[, i]
          
          dwa_1 <- ec_area[-ec_len] # remove last day (DV)
          
          dwa_2 <- ec_area[-1] # remove first day (IV)
          
          dwa_mod <- stats::lm(dwa_1 ~ dwa_2)
          
          pval <- polyval(rev(stats::coef(dwa_mod)), ec_area) # use predict() instead?
          
          ec_small[, i] <- pval
        }
        
      } else { # grouped
        
        # TODO: have someone verify that this is working correctly
        polys <- ec_small[-1, ] / ec_small[-nrow(ec_small), ]
        polys <- rbind(polys, NA)
        polys[nrow(polys), ] <- apply(polys, 2, FUN = mean, na.rm = TRUE)
        ec_small <- ec_small * polys
      }
    }
    
    # TODO: No code for calc.method = weights (not in Matlab code either)
    
    # dummy ----
    exp_dates <- rownames(ec_small)
    
    if (dummy.exp) {
      
      dum_matrix <- !is.na(ec_small)
      dum_matrix <- apply(dum_matrix, 2, as.numeric)
      
      dum_matrix <- lapply(date, function(x) {
        
        ind <- which(exp_dates == x)
        dum_matrix[ind, , drop = FALSE]
      })
      
      dum_matrix <- do.call(rbind, dum_matrix)
      # order columns
      dum_matrix <- dum_matrix[, order(colnames(dum_matrix))]
      
    } else {
      
      dum_matrix <- NULL
    }
    
    # empty exp ----
    
    if (is_value_empty(empty.expectation)) {
      
      ec_small[is.na(ec_small)] <- 0.0001
      
    } else if (empty.expectation == 1e-04) {
      
      ec_small[is.na(ec_small)] <- 1e-04
      ec_small[ec_small == 0] <- 1e-04
      
    } else if (empty.expectation == 0) {
      
      ec_small[is.na(ec_small)] <- 0
      
    } else {
      # case 'no replacement'
      ec_small[is.na(ec_small)] <- 0.0001
    }
    
    # convert to occasion (n obs by altc) ----
    tic()
    exp_matrix <- lapply(as.character(date), function(x) {
      ind <- which(exp_dates == x)
      ec_small[ind, , drop = FALSE]
    })
    toc()
    
    exp_matrix <- do.call(rbind, exp_matrix)
    # order columns
    exp_matrix <- exp_matrix[, order(colnames(exp_matrix))]
  }
  
  return(list(exp = exp_matrix, dummy = get0("dum_matrix"),
              settings = list("catch" = catch_name, "price" = price, "defineGroup" = defineGroup, "temp.var" = temp.var, "temporal" = temporal, 
                              "calc.method" = calc.method, "lag.method" = lag.method, "empty.catch" = empty.catch, 
                              "empty.expectation" = empty.expectation, "temp.window" = temp.window, "temp.lag" = temp.lag, 
                              "year.lag" = year.lag, "dummy.exp" = dummy.exp, "weight_avg" = weight_avg))
  )
}



##### TEST SOLUTION FROM AI #####
moving_average_2d_optimized <- function(mat, window_size) {
  # Check if window_size is valid
  if (window_size <= 0 | window_size %% 1 != 0) {
    stop("Window size should be a positive integer.")
  }
  
  # Get the number of rows and columns
  nrows <- nrow(mat)
  ncols <- ncol(mat)
  
  # Initialize the result matrix
  result <- matrix(NA, nrow = nrows, ncol = ncols)
  
  # Loop through each column (spatial zones)
  for (j in 1:ncols) {
    # Initialize a running sum and the count of non-NA values in the window
    running_sum <- 0
    running_count <- 0
    
    # Loop through each row (dates)
    for (i in 1:nrows) {
      # Add the current value to the running sum if it's not NA
      if (!is.na(mat[i, j])) {
        running_sum <- running_sum + mat[i, j]
        running_count <- running_count + 1
      }
      
      # If the window has exceeded the desired size, remove the value that is sliding out
      if (i > window_size) {
        if (!is.na(mat[i - window_size, j])) {
          running_sum <- running_sum - mat[i - window_size, j]
          running_count <- running_count - 1
        }
      }
      
      # Store the moving average in the result matrix (avoid division by zero)
      if (running_count > 0) {
        result[i, j] <- running_sum / running_count
      }
    }
  }
  
  return(result)
}

# Example usage: matrix with dates as rows and spatial zones as columns
set.seed(42)
dates <- seq.Date(from = as.Date("2024-01-01"), by = "days", length.out = 10)
zones <- c("Zone1", "Zone2", "Zone3")
data_matrix <- matrix(rnorm(30), nrow = 10, ncol = 3, dimnames = list(dates, zones))

# Introduce some missing data (NA values)
data_matrix[3, 2] <- NA  # Missing data in Zone2 on 2024-01-03
data_matrix[5, 1] <- NA  # Missing data in Zone1 on 2024-01-05
data_matrix[8, 3] <- NA  # Missing data in Zone3 on 2024-01-08

# Print the original matrix with missing data
print("Original Data Matrix (with missing values):")
print(data_matrix)

# Apply moving average with a window size of 3
window_size <- 3
moving_avg_result <- moving_average_2d_optimized(data_matrix, window_size)

# Print the moving average result
print("\nOptimized Moving Average Result (with missing values handled):")
print(moving_avg_result)