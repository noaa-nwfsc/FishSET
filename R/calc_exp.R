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
    numData <- rep(1, dim(dataset)[1])
    # Define by group case
  } else {
    
    numData <- as.integer(as.factor(dataset[[defineGroup]]))
  }
  
  z_ind <- which(dataZoneTrue == 1)
  numData <- numData[z_ind]
  
  spData <- choice$g[z_ind] # mapping to to the map file (zones)
  altc_areas <- as.character(unique(spData)) 
  altc_fleet <- unique(paste0(numData, spData))
  
  altc_names <- if (is_value_empty(defineGroup)) altc_areas else altc_fleet
  
  spNA <- which(is.na(spData))
  
  # TODO: check if this is needed 
  if (any(!is_empty(spNA))) {
    
    spData[spNA] <- rep(Inf, length(spNA)) # better for grouping than nas because aggregated
  }
  
  # TODO: check if this is needed
  numNAN <- which(is.nan(numData))
  
  if (any(!is_empty(numNAN))) {
    
    numData[numNAN] <- rep(Inf, length(numNAN))
  }
  
  catchData <- as.numeric(dataset[[catch]][z_ind])
  
  if (price != "none" && !is_value_empty(price)) {
    
    priceData <- as.numeric(dataset[[price]][z_ind])
    catchData <- catchData * priceData
  }
  
  # no time var ----
  # Time variable not chosen if temp.var is empty
  # NOTE currently doesn't allow dummy or other options if no time detected
  if (temp.var == "none" || is_value_empty(temp.var)) {
    
    allCatch <- stats::aggregate(catchData, by = list(spData = spData, numData = numData), 
                                 FUN = mean, na.rm = TRUE) 
    # TODO: update for groups
    exp_matrix <- matrix(allCatch$x, nrow = nrow(dataset), ncol = length(altc_areas),
                         dimnames = list(NULL, altc_areas), byrow = TRUE)
    
    newDumV <- list()
    
  } else {
    
    # with time var ----
    tiData <- date_parser(dataset[[temp.var]][z_ind]) 
    
    tiDataFloor <- lubridate::floor_date(tiData, unit = "day")
    
    
    if (temporal == "daily") { # check if temp.var exists
      # daily time line
      # Note: order matters 
      # Note: na.rm = TRUE?
      tLine <- seq.Date(from = min(tiDataFloor), to = max(tiDataFloor), by = "day")
      
      # fill in missing days 
      missing_days <- tLine[!tLine %in% tiDataFloor]
      
      if (length(missing_days) > 0) {
        
        missing_days_df <- 
          expand.grid(numData = unique(numData), spData = altc_areas,
                      catchData = NA, tiDataFloor = missing_days)
      }
      
    } else if (temporal == "sequential") {
      # observation time line
      tLine <- unique(tiDataFloor)
    }
    
    df <- data.frame(numData = numData,
                     spData = as.character(spData), 
                     catchData = as.numeric(catchData),
                     tiDataFloor = tiDataFloor)
    
    if (temporal == "daily" && length(missing_days) > 0) {
      
      df <- rbind(df, missing_days_df)
    }
    
    df <- df[order(df$tiDataFloor), ]
    
    #  ID = fleet + zone
    df$ID <- paste0(df$numData, df$spData)
    
    # Lag time
    # Note: this is ad hoc, revisit 
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
    
    na_ind <- is.na(df$catchData)
    
    if (sum(na_ind) > 0) {
      
      # TODO: treat -Inf before replacing empty catch
      # Note: Is NA necessary/desirable? Depends on how it's treated later in 
      # modeling process.
      if (is.na(empty.catch) || is_value_empty(empty.catch)) {
        
        df$catchData[na_ind] <- NA  
        
      } else if (empty.catch == 0) {
        
        df$catchData[na_ind] <- 0
        
      } else if (empty.catch == "allCatch") {
        
        # Q: ave yearly catch across all areas or by area?
        # TODO: make this more efficient
        # Note: allow user to make this choice. No right answer for default
        year_all <- lubridate::year(df$tiData)
        
        yr <- aggregate(df$catchData, by = list(year = year_all), 
                        FUN = mean, na.rm = TRUE)
        
        for (i in seq_along(yr$year)) {
          
          df[na_ind & year_all == yr$year[i], "catchData"] <- yr$x[i]
        }
        
      } else if (empty.catch == "groupedCatch") {
        # average catch for year and fleet
        
        # Q: fleet's ave yearly catch across all areas or by area?
        # Note: allow user to make this choice. No right answer for default
        # Do both? Show user output?
        # old function (group by year, fleet, and area)
        myfunc_GC <- function(x, y) {
          
          year_all <- lubridate::year(df$tiData)
          x_year <- lubridate::year(x)
          ind <- year_all == x_year & df$ID == y
          
          mean(df$lag.value[ind], na.rm = TRUE)
        }
        
        # current version (group by year and fleet)
        year_all <- lubridate::year(df$tiData)
        
        yr <- aggregate(df$catchData, 
                        by = list(year = year_all, fleet = df$numData), 
                        FUN = mean, na.rm = TRUE)
        
        # TODO: make this more efficient
        for (i in seq_along(yr$year)) {
          
          ind <- na_ind & year_all == yr$year[i] & df$numData == yr$fleet[i]
          
          df[ind, "catchData"] <- yr$x[i]
        }
      }
    }
    
    # lag ----
    if (temp.lag > 0) {
      
      df$lag.value <- c(rep(NA, temp.lag), df$catchData[-c(1:temp.lag)])
      # this will omit the first ob ID even if duplicated ex: duplicated(rep(1, 3))
      # TODO: count IDs and replace catch for IDs with single count
      # df$lag.value[which(!duplicated(df$ID))] <- NA
      
    } else {
      
      df$lag.value <- df$catchData
    }
    
    
    if (temp.lag > 2) { # TODO: check this
      
      for (i in seq_len(temp.lag - 1)) {
        
        df$lag.value[(which(!duplicated(df$ID)) + temp.lag - i)] <- NA
      }
    }
    
    # Moving window ----
    
    window_ave <- function(x) {
      
      ind <- 
        df$tiData >= x - lubridate::years(year.lag) - temp.window & 
        df$tiData <= x - lubridate::years(year.lag)
      
      
      # TODO: simplify
      if (is_value_empty(defineGroup)) {
        
        # TODO: handle sum(ind) == 0 (zero-row dataframe)
        # Ex: the first year in year.lag = 1
        # replace w/ NA? 
        
        if (sum(ind) == 0) { # empty dataframe
          
          return(data.frame(spData = altc_names, lag.value = NA))
        }
        
        # Note: could remove numData
        aggregate(lag.value ~ spData + numData, data = df[ind, ], 
                  FUN = mean, na.action = na.pass, na.rm = TRUE) 
        
      } else {
        
        if (sum(ind) == 0) { # empty dataframe
          # TODO: update for fleet
          return(data.frame(ID = altc_names, lag.value = NA))
        }
        
        aggregate(lag.value ~ ID, data = df[ind, ], 
                  FUN = mean, na.action = na.pass, na.rm = TRUE) 
      }
    }
    
    ave_list <- lapply(unique(df$tiData), window_ave)
    
    # add loop to extract values 
    
    area_nm <- if (is_value_empty(defineGroup)) "spData" else "ID"
    
    ave_list <- lapply(seq_along(ave_list), function(i) {
      
      # TODO: simplify
      ave_list[[i]]$lag.value[match(altc_names, ave_list[[i]][[area_nm]])]
    })
    
    # combine into "small" matrix
    ec_small <- do.call(rbind, ave_list)
    
    dimnames(ec_small) <- list(as.character(tLine), altc_names)
    # Note: pickup here next time 
    # calc method ----
    if (calc.method == "simpleLag") {
      # at this point could use means to get a regression compared to a lag of 
      # the same calculation at all zones, then use that to predict...
      
      # need to multiply polys by constant
      
      if (lag.method == "simple") {
        
        # TODO: have someone verify that this works correctly
        
        ec_len <- nrow(ec_small)
        
        for (i in seq_len(ncol(ec_small))) {
          
          # original approach
          # (dra = daily rolling average)
          ec_area <- ec_small[, i]
          
          dra <- ec_area[-ec_len]
          
          dra_lag <- ec_area[-1]
          
          dra_mod <- stats::lm(dra ~ dra_lag)
          
          pval <- polyval(stats::coef(dra_mod), ec_area)
          
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
    
    # TODO: No code for calc.method = weights 
    
    # dummy ----
    if (dummy.exp) {
      
      dum_matrix <- as.numeric(!is.na(ec_small)) # convert to numeric w/o losing matrix?
      dum_matrix <- matrix(dum_matrix, 
                           nrow = length(tLine), 
                           ncol = length(altc_names),
                           dimnames = list(as.character(tLine), 
                                           altc_names))
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
    
    exp_matrix <- 
      lapply(tiData, function(x) {
        
        ind <- which(exp_dates == x)
        
        ec_small[ind, , drop = FALSE]
      })
    
    exp_matrix <- do.call(rbind, exp_matrix)
  }
  
  list(exp = exp_matrix, dummy = dum_matrix)
}