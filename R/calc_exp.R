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
#' @param Alt Alternative choice list
#' @keywords internal
#' @importFrom lubridate floor_date year years
#' @importFrom stats aggregate lm coef
#' @returns Returns a list containing the expected catch/revenue matrix and 
#'   dummy matrix (if \code{dummy.exp = TRUE}). 
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
                     Alt) {
  # TODO: Finish documenting function
  
  # TODO: when revenue col exists, either automatically create col of ones (currently user must do this) 
  # or allow catch arg to also be revenue. Use generic name (e.g. value) 

  
  dataZoneTrue <- Alt[["dataZoneTrue"]] # used for catch and other variables
  choice <- Alt[["choice"]] # used for catch and other variables
  
  # user-defined ----
  # check whether defining a group or using all fleet averaging
  if (is_value_empty(defineGroup)) {
    # just use an id=ones to get all info as one group
    fleet <- rep(1, dim(dataset)[1])
    # Define by group case
  } else {
    
    fleet <- as.integer(as.factor(dataset[[defineGroup]]))
  }
  
  z_ind <- which(dataZoneTrue == 1)
  fleet <- fleet[z_ind]
  
  areas <- choice$g[z_ind] # mapping to to the map file (zones)
  altc_areas <- as.character(unique(areas)) 
  altc_fleet <- unique(paste0(fleet, areas))
  
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
                         dimnames = list(NULL, altc_areas), byrow = TRUE)
    
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
          
          mean(df$lag.value[ind], na.rm = TRUE)
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
    
    # lag ----
    if (temp.lag > 0) {
      
      df$lag.value <- c(rep(NA, temp.lag), df$catch[-c(1:temp.lag)])
      
      # this will omit the first ob ID even if duplicated ex: duplicated(rep(1, 3))
      # TODO: count IDs and replace catch for IDs with single count? Remove? 
      df$lag.value[which(!duplicated(df$ID))] <- NA
      
    } else {
      
      df$lag.value <- df$catch
    }
    
    if (temp.lag > 2) { # TODO: check this
      
      for (i in seq_len(temp.lag - 1)) {
        
        df$lag.value[(which(!duplicated(df$ID)) + temp.lag - i)] <- NA
      }
    }
    
    # Moving window ----
    
    window_ave <- function(x) {
      
      ind <- 
        df$dateFloor >= x - lubridate::years(year.lag) - temp.window & 
        df$dateFloor <= x - lubridate::years(year.lag)
      
      # TODO: simplify
      if (is_value_empty(defineGroup)) {
        
        if (sum(ind) == 0) { # empty dataframe
          
          return(data.frame(areas = altc_names, lag.value = NA))
        }
        
        # Note: could remove fleet
        aggregate(lag.value ~ areas + fleet, data = df[ind, ], 
                  FUN = mean, na.action = na.pass, na.rm = TRUE) # remove na.rm = TRUE?
        
      } else {
        
        if (sum(ind) == 0) { # empty dataframe
          # TODO: update for fleet
          return(data.frame(ID = altc_names, lag.value = NA))
        }
        
        aggregate(lag.value ~ ID, data = df[ind, ], 
                  FUN = mean, na.action = na.pass, na.rm = TRUE) # remove na.rm = TRUE?
      }
    }
    # list entries contain the window avg for each day
    ave_list <- lapply(unique(df$dateFloor), window_ave)
    
    # add loop to extract values 
    
    area_nm <- if (is_value_empty(defineGroup)) "areas" else "ID"
    
    ave_list <- lapply(ave_list, function(i) {
      
      # return avg for each area. Return NA if missing
      i$lag.value[match(altc_names, i[[area_nm]])]
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
    if (dummy.exp) {
      
      dum_matrix <- !is.na(ec_small)
      dum_matrix <- apply(dum_matrix, 2, as.numeric)
      
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
    # TODO: find faster method for this
    
    exp_dates <- rownames(ec_small)
    
    exp_matrix <- lapply(date, function(x) {
      
      ind <- which(exp_dates == x)
      ec_small[ind, , drop = FALSE]
    })
    
    exp_matrix <- do.call(rbind, exp_matrix)
  }
  
  list(exp = exp_matrix, dummy = dum_matrix)
}