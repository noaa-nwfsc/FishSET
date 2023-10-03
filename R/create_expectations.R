#' Create expected catch/expected revenue matrix
#'
#' Create expected catch or expected revenue matrix. The matrix is required for 
#' the \code{\link{logit_c}} model. Multiple user-defined matrices can be saved
#' by setting \code{replace.output = FALSE} and re-running the function. 

#' @param dat  Primary data containing information on hauls or trips. Table in FishSET 
#'   database contains the string 'MainDataTable'.
#' @param project String, name of project.
#' @param catch Variable from \code{dat} containing catch data.
#' @param price Optional, variable from \code{dat} containing price/value data.  
#'   Price is multiplied against \code{catch} to generated revenue. If revenue exists 
#'   in \code{dat} and you wish to use this revenue instead of price, then \code{catch} 
#'   must be a vector of 1 of length equal to \code{dat}. Defaults to \code{NULL}.
#' @param defineGroup Optional, variable from \code{dat} that defines how to split 
#'   the fleet. Defaults to treating entire dataframe \code{dat} as a fleet.
#' @param temporal String, choices are \code{"daily"} or \code{"sequential"}. Should 
#'   time, if \code{temp.var} is defined, be included as a daily timeline or sequential 
#'   order of recorded dates. For daily, catch on dates with no record are filled 
#'   with \code{NA}. The choice affects how the rolling average is calculated. If 
#'   temporal is daily then the window size for average and the temporal lag are 
#'   in days. If sequential, then averaging will occur over the specified number 
#'   of observations, regardless of how many days they represent.
#' @param temp.var Optional, temporal variable from \code{dat}. Set to \code{NULL} 
#'   if temporal patterns in catch should not be considered.
#' @param calc.method String, how catch values are average over window size. Select 
#'   standard average (\code{"standardAverage"}), simple lag regression of means 
#'   (\code{"simpleLag"}), or weights of regressed groups (\code{"weights"})
#' @param lag.method  String, use regression over entire group (\code{"simple"}) 
#'   or for grouped time periods (\code{"grouped"}).
#' @param empty.catch String, replace empty catch with \code{NA}, \code{0}, mean 
#'   of all catch (\code{"allCatch"}), or mean of grouped catch (\code{"groupCatch"}).
#' @param empty.expectation Numeric, how to treat empty expectation values. Choices 
#'   are to not replace (\code{NULL}) or replace with 0.0001 or 0.
#' @param temp.window Numeric, temporal window size. If \code{temp.var} is not \code{NULL}, 
#'   set the window size to average catch over. Defaults to 14 (14 days if \code{temporal} 
#'   is \code{"daily"}).
#' @param temp.lag Numeric, temporal lag time. If \code{temp.var} is not \code{NULL}, 
#'   how far back to lag \code{temp.window}.
#' @param year.lag If expected catch should be based on catch from previous year(s), 
#'   set \code{year.lag} to the number of years to go back.
#' @param dummy.exp Logical, should a dummy variable be created? If \code{TRUE}, 
#'   output dummy variable for originally missing value. If \code{FALSE}, no dummy 
#'   variable is outputted. Defaults to \code{FALSE}.
#' @param default.exp Whether to run default expectations. Defaults to \code{FALSE}.
#'   Alternatively, a character string containing the names of default expectations 
#'   to run can be entered. Options include "recent", "older", "oldest", and 
#'   "logbook". The logbook expectation is only run if \code{defineGroup} is used. 
#'   "recent" will not include \code{defineGroup}. Setting \code{default.exp = TRUE}
#'   will include all four options. See Details for how default expectations are 
#'   defined. 
#' @param replace.output Logical, replace existing saved expected catch data frame 
#'   with new expected catch data frame? If \code{FALSE}, new expected catch data 
#'   frames appended to previously saved expected catch data frames. Default is 
#'   \code{TRUE}. If \code{TRUE}
#' @param weight_avg Logical, if \code{TRUE} then all observations for a given zone on a 
#'   given date will be included when calculating the mean, thus giving more 
#'   weight to days with more observations in a given zone. If \code{FALSE}, then the 
#'   daily mean for a zone will be calculated prior to calculating the mean across the
#'   time window.
#' @export create_expectations
#' @return Function saves a list of expected catch matrices to the FishSET database
#'   as \code{projectExpectedCatch}. The list includes 
#'   the expected catch matrix from the user-defined choices, recent fine grained
#'   information, older fine grained information, oldest fine grained information,
#'   and logbook level information. Additional expected catch cases can be added 
#'   to the list by specifying \code{replace.output = FALSE}. The list is 
#'   automatically saved to the FishSET database and is called 
#'   in \code{\link{make_model_design}}. The expected catch output does not need 
#'   to be loaded when defining or running the model.
#' @details Function creates an expectation of catch or revenue for alternative 
#'   fishing zones (zones where they could have fished but did not). The output is 
#'   saved to the FishSET database and called by the \code{\link{make_model_design}} 
#'   function. \code{\link{create_alternative_choice}} must be called first as observed 
#'   catch and zone inclusion requirements are defined there.\cr The primary choices 
#'   are whether to treat data as a fleet or to group the data (\code{defineGroup}) 
#'   and the time frame of catch data for calculating expected catch. Catch is averaged 
#'   along a daily or sequential timeline (\code{temporal}) using a rolling average. 
#'   \code{temp.window} and \code{temp.lag} determine the window size and temporal 
#'   lag of the window for averaging. Use \code{\link{temp_obs_table}} before using 
#'   this function to assess the availability of data for the desired temporal moving 
#'   window size. Sparse data is not suited for shorter moving window sizes. For very 
#'   sparse data, consider setting \code{temp.var} to \code{NULL} and excluding 
#'   temporal patterns in catch. \cr
#'   Empty catch values are considered to be times of no fishing activity. Values 
#'   of 0 in the catch variable are considered times when fishing activity occurred 
#'   but with no catch. These points are included in the averaging and dummy creation 
#'   as points in time when fishing occurred. \cr
#'   Four default expected catch cases will be run:
#'   \itemize{
#'   \item{recent: Moving window size of two days. In this case, there is 
#'   no grouping, and catch for entire fleet is used.}
#'   \item{older: Moving window size of seven days and lag of two days. In this 
#'   case, vessels are grouped (or not) based on \code{defineGroup} argument.}
#'   \item{oldest: Moving window of seven days and lag of eight days. In this 
#'   case, vessels are grouped (or not) based on \code{defineGroup} argument.}
#'   \item{logbook: Moving window size of 14 days and lag of one year, seven days. 
#'   Only used if fleet is defined in \code{defineGroup}.}
#' }
#' @return newGridVar,  newDumV
#' @examples
#' \dontrun{
#' create_expectations(pollockMainDataTable, "pollock", "OFFICIAL_TOTAL_CATCH_MT",
#'   price = NULL, defineGroup = "fleet", temp.var = "DATE_FISHING_BEGAN",
#'   temporal = "daily", calc.method = "standardAverage", lag.method = "simple",
#'   empty.catch = "allCatch", empty.expectation = 0.0001, temp.window = 4,
#'   temp.lag = 2, year.lag = 0, dummy.exp = FALSE, replace.output = FALSE
#' )
#' }
#'
create_expectations <-
  
  function(dat,
           project,
           catch,
           price = NULL,
           defineGroup = NULL,
           temp.var = NULL,
           temporal = "daily",
           calc.method = "standardAverage",
           lag.method = "simple",
           empty.catch = NULL,
           empty.expectation = 1e-04,
           temp.window = 7,
           temp.lag = 0,
           year.lag = 0,
           dummy.exp = FALSE,
           default.exp = FALSE,
           replace.output = TRUE,
           weight_avg = FALSE) {
  
  # TODO: custom names, need exp.name arg

  # TODO: when revenue col exists, either automatically create col of ones (currently user must do this) 
  # or allow catch arg to also be revenue. Use generic name (e.g. value) 
    
  # TODO: Check whether/which default options have already been run, notify user
  # that they don't need to be re-run -- unless the alt choice matrix has been changed
  # in which case the previous ec matrices are no longer valid and replace.output must 
  # be TRUE.
  
  # Call in data sets
  out <- data_pull(dat, project = project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  Alt <- unserialize_table(paste0(project, "AltMatrix"), project)

  # checks ----
  
  column_check(dataset, c(catch, price, temp.var, defineGroup))
  
  if (all(is_empty(date_cols(dataset)))) {
    
    warning("No time variable found, only averaging in groups and per zone is capable",
            call. = FALSE)
  }
  
  # Check that define group is either empty of an actual variable in the dataset
  if (!is_value_empty(defineGroup)) {
    
    if (any(is.null(dataset[[defineGroup]]))) { # Note: this won't work check for NAs instead?
      
      stop("defineGroup not recognized. Check that parameter is correctly defined",
           call. = FALSE)
    }
  }
  
  # check for NaNs and Inf/-Infs in catch
  if (qaqc_helper(dataset[catch], "NaN")) {
    
    stop("NaNs detected in catch variable. Use nan_filter() to remove.", call. = FALSE)
  }
  
  if (qaqc_helper(dataset[catch], "Inf")) {
    
    stop("Inf/-Infs detected in catch variable. Remove before running `create_expectations()`.", 
         call. = FALSE)
  }
  
  stopifnot("empty.expectations must be numeric" = is.numeric(empty.expectation))

  # default exp catch ----
  
  # old defaults:
  # 1) short-term: 2 day window, no lag
  # 2) medium-term: 7 day window, no lag
  # 3) long-term: 7 day window, 1 year lag
  
  # Abbot Wilen 2011 Appendix (Alan's preference for default)
  # 1) Recent fine grained info: 1 day lag (2 day window?)
  # 2) Older fine grained info: 2 day lag, window 7 days 
  # 3) Oldest fine grained info: 8 day lag, window 7 days
  # 4) "Logbook" data: l year lag - 7 days, window 14 days
  # 5) "Coarse grained" info: grouped of sites across entire history of catch (?) 
  
  # within each: observe fleet-wide or individual catch (2 and 3)
  # 4 and 5 always fleet-wide
  
  # empty exp lists
  recent_exp <- list()
  older_exp <- list()
  oldest_exp <- list()
  logbook_exp <- list()
  
  if (!is_value_empty(default.exp)) {
    
    if (isTRUE(default.exp) || "recent" %in% default.exp) {
      
      recent_exp <- calc_exp(dataset = dataset, catch = catch, price = price,
                             defineGroup = NULL, temp.var = temp.var,
                             temp.window = 2, temp.lag = 0, year.lag = 0, 
                             temporal = temporal, calc.method = calc.method,
                             lag.method = lag.method, empty.catch = empty.catch,
                             empty.expectation = empty.expectation, dummy.exp = dummy.exp,
                             Alt = Alt)
    }
    
    if (isTRUE(default.exp) || "older" %in% default.exp) {
      
      older_exp <- calc_exp(dataset = dataset, catch = catch, price = price,
                            defineGroup = defineGroup, temp.var = temp.var,
                            temp.window = 7, temp.lag = 2, year.lag = 0, 
                            temporal = temporal, calc.method = calc.method,
                            lag.method = lag.method, empty.catch = empty.catch,
                            empty.expectation = empty.expectation, dummy.exp = dummy.exp,
                            Alt = Alt)
    }
   
    if (isTRUE(default.exp) || "oldest" %in% default.exp) {
      
      oldest_exp <- calc_exp(dataset = dataset, catch = catch, price = price,
                             defineGroup = defineGroup, temp.var = temp.var,
                             temp.window = 7, temp.lag = 8, year.lag = 0,
                             temporal = temporal, calc.method = calc.method,
                             lag.method = lag.method, empty.catch = empty.catch,
                             empty.expectation = empty.expectation, dummy.exp = dummy.exp,
                             Alt = Alt)
    }
   
    if (isTRUE(default.exp) || "logbook" %in% default.exp) {
      
      if (!is_value_empty(defineGroup)) {
        
        logbook_exp <- calc_exp(dataset = dataset, catch = catch, price = price,
                                defineGroup = defineGroup, temp.var = temp.var,
                                temp.window = 14, temp.lag = 7, year.lag = 1, 
                                temporal = temporal, calc.method = calc.method,
                                lag.method = lag.method, empty.catch = empty.catch,
                                empty.expectation = empty.expectation, dummy.exp = dummy.exp,
                                Alt = Alt)
      }
    }
  }
  
  # user-defined ----
  user_exp <- calc_exp(dataset = dataset, catch = catch, price = price,
                       defineGroup = defineGroup, temp.var = temp.var, 
                       temp.window = temp.window, temp.lag = temp.lag, 
                       year.lag = year.lag, temporal = temporal, 
                       calc.method = calc.method, lag.method = lag.method, 
                       empty.catch = empty.catch, empty.expectation = empty.expectation,
                       dummy.exp = dummy.exp, weight_avg = weight_avg, Alt = Alt)
  
  
  r <- nchar(sub("\\.[0-9]+", "", mean(as.matrix(user_exp$exp), na.rm = TRUE))) 
  sscale <- 10^(r - 1)

  # exp catch list ----
  ExpectedCatch <- list(
    scale = sscale,
    # TODO: Use alternative approach for determining units
    units = ifelse(grepl("lbs|pounds", catch, ignore.case = TRUE), "LBS", "MTS"), # units of catch data
    recent = recent_exp$exp,
    recent_dummy = recent_exp$dummy,
    recent_settings = recent_exp$settings,
    older = older_exp$exp,
    older_dummy = older_exp$dummy,
    older_settings = older_exp$settings,
    oldest = oldest_exp$exp,
    oldest_dummy = oldest_exp$dummy,
    oldest_settings = oldest_exp$settings,
    logbook = logbook_exp$exp,
    logbook_dummy = logbook_exp$dummy,
    logbook_settings = logbook_exp$settings,
    exp1 = user_exp$exp,
    exp1_dummy = user_exp$dummy,
    exp1_settings = user_exp$settings
  )
  
  single_sql <- paste0(project, "ExpectedCatch")

  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  if (replace.output == FALSE) {
    
    # recursive naming function
    exp_nm_r <- function(n1, n2, v1) {
      # check if new names match any existing names
      if (n1 %in% n2) {
        # replace digit w/ new value
        n1 <- gsub('\\d+', v1, n1)
        
        if (n1 %in% n2) {
          # if still matches, add 1 to new values and rerun func
          v1 <- v1 + 1
          exp_nm_r(n1, n2, v1)
          
        } else n1
        
      } else n1
    }
    
    if (table_exists(single_sql, project)) {
      
      # get previous list
      ExpectedCatchOld <- unserialize_table(single_sql, project)
      # names of all matrices
      exp_names <- names(ExpectedCatchOld)[!names(ExpectedCatchOld) %in% c('scale', 'units')]
      exp_new_names <- c('exp1', 'exp1_dummy', 'exp1_settings')
      # generate new names
      exp_new_names <- vapply(exp_new_names, 
                              function(x) exp_nm_r(x, exp_names, 1), 
                              character(1))
      # get default exp names
      exp_names2 <- names(ExpectedCatch)[!names(ExpectedCatch) %in% c('scale', 'units', 'exp1', 'exp1_dummy', 'exp1_settings')]
      # see if default was run
      non_empty_exp <- vapply(exp_names2, function(x) !is.null(ExpectedCatch[[x]]), logical(1))
      
      # update expected catch list 
      if (any(non_empty_exp)) {
        
        for (i in names(non_empty_exp)[non_empty_exp]) {
          
          ExpectedCatchOld[[i]] <- ExpectedCatch[[i]]
        }
      }
      # merge previous list into newest list
      ExpectedCatch <- c(ExpectedCatchOld,
                         setNames(list(ExpectedCatch$exp1, ExpectedCatch$exp1_dummy, ExpectedCatch$exp1_settings),
                                  exp_new_names))
    }
  }

  if (table_exists(single_sql, project)) {
    
    table_remove(single_sql, project)
  }

  DBI::dbExecute(fishset_db, paste("CREATE TABLE IF NOT EXISTS", single_sql, 
                                   "(data ExpectedCatch)"))
  DBI::dbExecute(fishset_db, paste("INSERT INTO", single_sql, "VALUES (:data)"),
    params = list(data = list(serialize(ExpectedCatch, NULL)))
  )
  
  message('Expected catch/revenue matrix saved to FishSET database')

  create_expectations_function <- list()
  create_expectations_function$functionID <- "create_expectations"
  create_expectations_function$args <- list(
    dat, project, catch, price, defineGroup, temp.var, temporal, calc.method, 
    lag.method, empty.catch, empty.expectation, temp.window, temp.lag, year.lag, 
    dummy.exp, default.exp, replace.output
  )
  create_expectations_function$kwargs <- list()

  log_call(project, create_expectations_function)
}
