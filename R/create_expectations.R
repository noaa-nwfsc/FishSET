#' Create expected catch/expected revenue matrix
#'
#' Create expected catch or expected revenue matrix. The matrix is required for 
#' the \code{\link{logit_c}} model. Multiple matrices (with unique names) can be saved in a 
#' project.
#' 
#' @param dat Primary data containing information on hauls or trips. Table in FishSET 
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
#' @param temp_var Optional, temporal variable from \code{dat}. Set to \code{NULL} 
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
#'   set the window size to average catch over. Defaults to 14 (14 days if \code{temporal} 
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
#' @param outsample Logical, if \code{TRUE} then generate expected catch matrix for 
#'   out-of-sample data. If \code{FALSE} generate for primary data table. Defaults to
#'   \code{outsample = FALSE}
#' @export create_expectations
#' @return Function saves a list of expected catch matrices to the FishSET database
#'   as \code{projectExpectedCatch}. The list includes 
#'   the expected catch matrix from the user-defined choices. Multiple expected catch cases 
#'   can be added to the list by specifying unique names. The list is automatically saved to 
#'   the FishSET database and is called in \code{\link{make_model_design}}. The expected catch 
#'   output does not need to be loaded when defining or running the model.
#' @details Function creates an expectation of catch or revenue for alternative 
#'   fishing zones (zones where they could have fished but did not). The output is 
#'   saved to the FishSET database and called by the \code{\link{make_model_design}} 
#'   function. \code{\link{create_alternative_choice}} must be called first as observed 
#'   catch and zone inclusion requirements are defined there.\cr 
#'   The primary choices are whether to treat data as a fleet or to group the data 
#'   (\code{defineGroup}) and the time frame of catch data for calculating expected catch. 
#'   Catch is averaged along a daily or sequential timeline (\code{temporal}) using a rolling 
#'   average. \code{temp_window} and \code{day_lag} determine the window size and temporal 
#'   lag of the window for averaging. Use \code{\link{temp_obs_table}} before using 
#'   this function to assess the availability of data for the desired temporal moving 
#'   window size. Sparse data is not suited for shorter moving window sizes. For very 
#'   sparse data, consider setting \code{temp_var} to \code{NULL} and excluding 
#'   temporal patterns in catch. \cr
#'   Empty catch values are considered to be times of no fishing activity. Values 
#'   of 0 in the catch variable are considered times when fishing activity occurred 
#'   but with no catch. These points are included in the averaging and dummy creation 
#'   as points in time when fishing occurred. \cr
#' @examples
#' \dontrun{
#' create_expectations(pollockMainDataTable, "pollock", "exp1", "OFFICIAL_TOTAL_CATCH_MT",
#'   price = NULL, defineGroup = "fleet", temp_var = "DATE_FISHING_BEGAN",
#'   temporal = "daily", calc_method = "standardAverage", 
#'   empty_catch = "allCatch", empty_expectation = 0.0001, temp_window = 4,
#'   day_lag = 2, year_lag = 0, dummy_exp = FALSE, 
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
           temp_var = NULL,
           temporal = "daily",
           calc_method = "standardAverage",
           temp_window = 7,
           day_lag = 1,
           year_lag = 0,
           empty_catch = NULL,
           empty_expectation = 1e-04,
           dummy_exp = FALSE,
           weight_avg = FALSE,
           outsample = FALSE) {
    
    # Check if expected matrix with this name already exists --------------------------------------
    if (!outsample && table_exists(paste0(project, "ExpectedCatch"), project)) {
      # get previous list
      exp_mats <- unserialize_table(paste0(project, "ExpectedCatch"), project)
      exp_names <- names(exp_mats)[!names(exp_mats) %in% c('scale', 'units')]
      if (name %in% exp_names) {
        stop("An expected catch matrix with this name already exist. Please enter a new name for
           this expected catch matrix.")
      }
    }
    
    # Pull and prepare data -----------------------------------------------------------------------
    out <- data_pull(dat, project = project)
    dataset <- out$dataset
    
    # Determine if using in-sample or out-of-sample data ------------------------------------------
    # Set data source based on the outsample flag
    data_type <- if (!outsample) "main" else "outsample"
    alt_matrix_name <- if (!outsample) "AltMatrix" else "AltMatrixOutSample"
    
    dat <- parse_data_name(dat, data_type, project)
    Alt <- unserialize_table(paste0(project, alt_matrix_name), project)
    
    # Perform initial data quality and parameter checks -------------------------------------------
    column_check(dataset, c(catch, price, temp_var, defineGroup))
    
    if (all(is_empty(date_cols(dataset)))) {
      warning("No time variable found, only averaging in groups and per zone is capable",
              call. = FALSE)
    }
    
    if (!is_value_empty(defineGroup)) {
      if (!all(defineGroup %in% names(dataset))) { # Check that groups exist in the dataset
        stop("One or more values for defineGroup input not found in the main data table",
             call. = FALSE)
      }
    }
    
    if (qaqc_helper(dataset[catch], "NaN")) {
      stop("NaNs detected in catch variable. Use nan_filter() to remove.", call. = FALSE)
    }
    
    if (qaqc_helper(dataset[catch], "Inf")) {
      stop("Inf/-Infs detected in catch variable. Remove before running `create_expectations()`.", 
           call. = FALSE)
    }
    
    # Ensure empty_expectation is numeric
    stopifnot("empty_expectations must be numeric" = is.numeric(empty_expectation))
    
    # Ensure the temporal variable is a Date type
    var_class <- class(dataset[[temp_var]])
    if (!("Date" %in% var_class || any(grepl("POSIX", var_class)))) {
      dataset[[temp_var]] <- as.Date(dataset[[temp_var]])
    }
    
    # Calculate expactations ----------------------------------------------------------------------
    user_exp <- calc_exp(dataset = dataset, 
                         catch = catch, 
                         price = price,
                         defineGroup = defineGroup, 
                         temp_var = temp_var, 
                         temp_window = temp_window, 
                         day_lag = day_lag, 
                         year_lag = year_lag, 
                         temporal = temporal, 
                         calc_method = calc_method, 
                         empty_catch = empty_catch, 
                         empty_expectation = empty_expectation,
                         dummy_exp = dummy_exp, 
                         weight_avg = weight_avg, 
                         Alt = Alt)
    
    # Determine scaling factor for the results ----------------------------------------------------
    r <- nchar(sub("\\.[0-9]+", "", mean(as.matrix(user_exp$exp), na.rm = TRUE))) 
    sscale <- 10^(r - 1)
    
    # Define the SQL table name -------------------------------------------------------------------
    # Is this for testing out of sample data?
    if(!outsample){
      single_sql <- paste0(project, "ExpectedCatch")
    } else {
      single_sql <- paste0(project, "ExpectedCatchOutSample")
    }
    
    # Assemble the expected catch list for storage ------------------------------------------------
    if (!outsample && table_exists(single_sql, project)) {
      # Get existing list
      ExpectedCatch <- unserialize_table(single_sql, project)
      
      # Add new expected catch matrix
      tmp_ExpectedCatch <- list(exptmp = user_exp$exp,
                                exptmp_dummy = user_exp$dummy,
                                exptmp_settings = user_exp$settings)
      
      # Combine the lists
      ExpectedCatch <- c(ExpectedCatch, tmp_ExpectedCatch)
      
    } else {
      ExpectedCatch <- list(
        scale = sscale,
        # TODO: Use alternative approach for determining units
        units = ifelse(grepl("lbs|pounds", catch, ignore.case = TRUE), "LBS", "MTS"), # catch units
        exptmp = user_exp$exp,
        exptmp_dummy = user_exp$dummy,
        exptmp_settings = user_exp$settings
      )  
    }
    
    # Rename elements in the list
    names(ExpectedCatch)[which(names(ExpectedCatch) == "exptmp")] <- name
    names(ExpectedCatch)[which(names(ExpectedCatch) == "exptmp_dummy")] <- paste0(name,"_dummy")
    names(ExpectedCatch)[which(names(ExpectedCatch) == "exptmp_settings")] <- 
      paste0(name,"_settings")
    
    # Connect to DB and save the results ----------------------------------------------------------
    fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project))
    on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
    
    # Remove the table first then save again
    table_remove(single_sql, project)
    
    DBI::dbExecute(fishset_db, 
                   paste("CREATE TABLE IF NOT EXISTS", 
                         single_sql, 
                         "(data ExpectedCatch)")
    )
    DBI::dbExecute(fishset_db, 
                   paste("INSERT INTO", 
                         single_sql, 
                         "VALUES (:data)"),
                   params = list(data = list(serialize(ExpectedCatch, NULL)))
    )
    
    # Display confirmation message ----------------------------------------------------------------
    if(!outsample){
      message('Expected catch/revenue matrix saved to FishSET database')  
    } else {
      message('Out-of-sample expected catch/revenue matrix saved to FishSET database')  
    }
    
    # Log the function call -----------------------------------------------------------------------
    create_expectations_function <- list()
    create_expectations_function$functionID <- "create_expectations"
    create_expectations_function$args <- 
      list(dat, 
           project, 
           name,
           catch, 
           price, 
           defineGroup, 
           temp_var, 
           temporal, 
           calc_method, 
           empty_catch, 
           empty_expectation, 
           temp_window, 
           day_lag, 
           year_lag, 
           dummy_exp, 
           weight_avg, 
           outsample
      )
    create_expectations_function$kwargs <- list()
    
    log_call(project, create_expectations_function)
  }