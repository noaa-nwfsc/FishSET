#' Create expected catch/expected revenue matrix
#'
#' Create expected catch or expected revenue matrix. The matrix is required for 
#' the \code{\link{logit_c}} model. Multiple user-defined matrices can be saved
#' by setting \code{replace.output = FALSE} and re-running the function. 

#' @param dat  Primary data containing information on hauls or trips. Table in FishSET 
#'   database contains the string 'MainDataTable'.
#' @param project String, name of project.
#' @param name Name of the expected matrix to be saved
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
#' @param day.lag Numeric, temporal lag time. If \code{temp.var} is not \code{NULL}, 
#'   how far back to lag \code{temp.window}.
#' @param year.lag If expected catch should be based on catch from previous year(s), 
#'   set \code{year.lag} to the number of years to go back.
#' @param dummy.exp Logical, should a dummy variable be created? If \code{TRUE}, 
#'   output dummy variable for originally missing value. If \code{FALSE}, no dummy 
#'   variable is outputted. Defaults to \code{FALSE}.
#' @param replace.output Logical, replace existing saved expected catch data frame 
#'   with new expected catch data frame? If \code{FALSE}, new expected catch data 
#'   frames appended to previously saved expected catch data frames. Default is 
#'   \code{TRUE}. If \code{TRUE}
#' @param weight_avg Logical, if \code{TRUE} then all observations for a given zone on a 
#'   given date will be included when calculating the mean, thus giving more 
#'   weight to days with more observations in a given zone. If \code{FALSE}, then the 
#'   daily mean for a zone will be calculated prior to calculating the mean across the
#'   time window.
#' @param outsample Logical, if \code{TRUE} then generate expected catch matrix for 
#'   out-of-sample data. If \code{FALSE} generate for primary data table. Defaults to
#'   \code{outsample = FALSE}
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
#'   \code{temp.window} and \code{day.lag} determine the window size and temporal 
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
#'   day.lag = 2, year.lag = 0, dummy.exp = FALSE, replace.output = FALSE,
#'   weight_avg = FALSE, outsample = FALSE
#' )
#' }
#'
create_expectations <-
  function(dat,
           project,
           name,
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
           day.lag = 1,
           year.lag = 0,
           dummy.exp = FALSE,
           replace.output = TRUE,
           weight_avg = FALSE,
           outsample = FALSE) {
  
  # TODO: when revenue col exists, either automatically create col of ones (currently user must do this) 
  # or allow catch arg to also be revenue. Use generic name (e.g. value) 

  # First check if expected catch matrix with this name is already saved
  if (table_exists(paste0(project, "ExpectedCatch"), project)) {
    # get previous list
    exp_mats <- unserialize_table(paste0(project, "ExpectedCatch"), project)
    exp_names <- names(exp_mats)[!names(exp_mats) %in% c('scale', 'units')]
    if (name %in% exp_names) {
      stop("An expected catch matrix with this name already exist. Please enter a new name for
           this expected catch matrix.")
    }
  }
    
  # Call in data sets
  out <- data_pull(dat, project = project)
  dataset <- out$dataset
  
  if(!outsample){ # in-sample
    dat <- parse_data_name(dat, "main", project)  
    Alt <- unserialize_table(paste0(project, "AltMatrix"), project)
      
  } else { # out-of-sample
    dat <- parse_data_name(dat, "outsample", project)  
    Alt <- unserialize_table(paste0(project, "AltMatrixOutSample"), project)
    
  }
  
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
  
  if (class(dataset[[temp.var]]) != "Date") {
    dataset[[temp.var]] <- as.Date(dataset[[temp.var]])
  }
  
  # user-defined ----
  user_exp <- calc_exp(dataset = dataset, catch = catch, price = price,
                       defineGroup = defineGroup, temp.var = temp.var, 
                       temp.window = temp.window, day.lag = day.lag, 
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
    exp1 = user_exp$exp,
    exp1_dummy = user_exp$dummy,
    exp1_settings = user_exp$settings
  )
  
  names(ExpectedCatch)[which(names(ExpectedCatch) == "exp1")] <- name
  names(ExpectedCatch)[which(names(ExpectedCatch) == "exp1_dummy")] <- paste0(name,"_dummy")
  names(ExpectedCatch)[which(names(ExpectedCatch) == "exp1_settings")] <- paste0(name,"_settings")
  
  # Is this for testing out of sample data?
  if(!outsample){
    single_sql <- paste0(project, "ExpectedCatch")
  } else {
    single_sql <- paste0(project, "ExpectedCatchOutSample")
  }
  
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  DBI::dbExecute(fishset_db, paste("CREATE TABLE IF NOT EXISTS", single_sql, 
                                   "(data ExpectedCatch)"))
  DBI::dbExecute(fishset_db, paste("INSERT INTO", single_sql, "VALUES (:data)"),
    params = list(data = list(serialize(ExpectedCatch, NULL))))
  
  if(!outsample){
    message('Expected catch/revenue matrix saved to FishSET database')  
  } else {
    message('Out-of-sample expected catch/revenue matrix saved to FishSET database')  
  }
  
  create_expectations_function <- list()
  create_expectations_function$functionID <- "create_expectations"
  create_expectations_function$args <- list(
    dat, project, catch, price, defineGroup, temp.var, temporal, calc.method, 
    lag.method, empty.catch, empty.expectation, temp.window, day.lag, year.lag, 
    dummy.exp, default.exp, replace.output, weight_avg, outsample
  )
  create_expectations_function$kwargs <- list()

  log_call(project, create_expectations_function)
}
