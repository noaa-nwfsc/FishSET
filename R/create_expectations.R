#' Create expected catch/expected revenue matrix
#'
#' Create expected catch or expected revenue matrix. The matrix is required for 
#' the logit_c model.

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
#' @param replace.output Logical, replace existing saved expected catch data frame 
#'   with new expected catch data frame? If \code{FALSE}, new expected catch data 
#'   frames appended to previously saved expected catch data frames. Default is \code{TRUE}
#' @importFrom lubridate floor_date year
#' @importFrom DBI dbGetQuery
#' @importFrom stats aggregate reshape coef lm
#' @export create_expectations
#' @return Function returns a list of expected catch matrices. The list includes 
#'   the expected catch matrix from the user-defined choices, the near-term, the 
#'   medium-term, and the long-term expected catch matrices. Additional expected 
#'   catch cases can be added to the list by specifying \code{replace.output} to 
#'   \code{FALSE}. The model run function will run through each expected catch case 
#'   provided. The list is automatically saved to the FishSET database and is called 
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
#'   Three default expected catch cases will be run:
#'   \itemize{
#'   \item{Near-term: Moving window size of two days. In this case, vessels are 
#'   grouped based on \code{defineGroup} argument.}
#'   \item{Medium-term: Moving window size of seven days. In this case, there is 
#'   no grouping, and catch for entire fleet is used.}
#'    \item{Long-term: Moving window size of seven days from the previous year. 
#'    In this case, there is no grouping and catch for entire fleet is used.}
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
           empty.expectation = NULL,
           temp.window = 7,
           temp.lag = 0,
           year.lag = 0,
           dummy.exp = FALSE,
           replace.output = TRUE) {
    

  # TODO: when revenue col exists, either automatically create col of ones (currently user must do this) 
  # or allow catch arg to also be revenue. Use generic name (e.g. value) 
  
  # Call in data sets
  out <- data_pull(dat, project = project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  Alt <- unserialize_table(paste0(project, "altmatrix"), project)

  # checks ----
  # for now if no time, only allow mean based on group without options TODO: allow
  # options from tab 2, currently turned off ti=find([data.isTime])# TODO add option for other time
  if (all(is_empty(date_cols(dataset)))) {
    
    warning("No time variable found, only averaging in groups and per zone is capable")
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

  # default exp catch ----
  # TODO: Allow user to select which of these to run (if any).
  # TODO: Skip short, med, long expectations if they already exist or else overwrite
  # Otherwise they will be duplicated in the exp list. 
    
  # 1. Option 1. Short-term, individual grouping t - 2 (window)
  short_exp <- calc_exp(dataset = dataset, catch = catch, price = price,
                        defineGroup = defineGroup, temp.var = temp.var,
                        temp.window = 2, temp.lag = 0, year.lag = 0,  # "short"
                        temporal = temporal, calc.method = calc.method,
                        lag.method = lag.method, empty.catch = empty.catch,
                        empty.expectation = empty.expectation, dummy.exp = dummy.exp,
                        Alt = Alt)
 
  # 2. Option 2 medium: group by fleet (all vessels in dataset) t -7
  med_exp <- calc_exp(dataset = dataset, catch = catch, price = price,
                      defineGroup = defineGroup, temp.var = temp.var,
                      temp.window = 7, temp.lag = 0, year.lag = 0,  # "medium"
                      temporal = temporal, calc.method = calc.method,
                      lag.method = lag.method, empty.catch = empty.catch,
                      empty.expectation = empty.expectation, dummy.exp = dummy.exp,
                      Alt = Alt)
  
 # 3. option 3  last year, group by fleet t-7
  long_exp <- calc_exp(dataset = dataset, catch = catch, price = price,
                       defineGroup = defineGroup, temp.var = temp.var, 
                       temp.window = 7, temp.lag = 0, year.lag = 1,  # "long"
                       temporal = temporal, calc.method = calc.method, 
                       lag.method = lag.method, empty.catch = empty.catch,
                       empty.expectation = empty.expectation, dummy.exp = dummy.exp,
                       Alt = Alt)

  # user-defined ----
  user_def_exp <- calc_exp(dataset = dataset, catch = catch, price = price,
                           defineGroup = defineGroup, temp.var = temp.var, 
                           temp.window = temp.window, temp.lag = temp.lag, 
                           year.lag = year.lag, temporal = temporal, 
                           calc.method = calc.method, lag.method = lag.method, 
                           empty.catch = empty.catch, empty.expectation = empty.expectation, 
                           dummy.exp = dummy.exp, Alt = Alt)
  
  
  r <- nchar(sub("\\.[0-9]+", "", mean(as.matrix(user_def_exp$exp), na.rm = TRUE))) 
  sscale <- 10^(r - 1)

  # exp catch list ----
  
  ExpectedCatch <- list(
    short_exp = short_exp$exp,
    short_exp_newDumV = short_exp$dummy,
    med_exp = med_exp$exp, 
    med_exp_newDumV = med_exp$dummy,
    long_exp = long_exp$exp, 
    long_exp_newDumv = long_exp$dummy,
    user_defined_exp = user_def_exp$exp, 
    scale = sscale,
    newDumV = user_def_exp$dummy,
    # TODO: Use alternative approach for determining units
    units = ifelse(grepl("lbs|pounds", catch, ignore.case = TRUE), "LBS", "MTS") # units of catch data 
  )

  single_sql <- paste0(project, "ExpectedCatch")

  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  if (replace.output == FALSE) {
    
    if (table_exists(single_sql, project)) {
      # TODO: this is inefficient if just changing user-defined exp catch. 
      # Short, med, and long exp don't need to be duplicated
      
      ExpectedCatchOld <- unserialize(DBI::dbGetQuery(fishset_db, paste0("SELECT data FROM ", project, "ExpectedCatch LIMIT 1"))$data[[1]])
      ExpectedCatch <- c(ExpectedCatchOld, ExpectedCatch)
      
    } else {
      
      ExpectedCatch <- ExpectedCatch
    }
  }

  if (table_exists(single_sql, project)) {
    
    table_remove(single_sql, project)
  }

  DBI::dbExecute(fishset_db, paste("CREATE TABLE IF NOT EXISTS", single_sql, "(data ExpectedCatch)"))
  DBI::dbExecute(fishset_db, paste("INSERT INTO", single_sql, "VALUES (:data)"),
    params = list(data = list(serialize(ExpectedCatch, NULL)))
  )

  create_expectations_function <- list()
  create_expectations_function$functionID <- "create_expectations"
  create_expectations_function$args <- list(
    dat, project, catch, price, defineGroup, temp.var, temporal, calc.method, lag.method,
    empty.catch, empty.expectation, temp.window, temp.lag, year.lag, dummy.exp, replace.output
  )
  create_expectations_function$kwargs <- list()

  log_call(project, create_expectations_function)
}
