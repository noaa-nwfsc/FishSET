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
           defineGroup = "fleet",
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
  
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  # Alt <- unserialize(DBI::dbGetQuery(fishset_db, paste0("SELECT AlternativeMatrix FROM ", project, "altmatrix LIMIT 1"))$AlternativeMatrix[[1]])
  Alt <- unserialize_table(paste0(project, "altmatrix"), project)

  dataZoneTrue <- Alt[["dataZoneTrue"]] # used for catch and other variables
  choice <- Alt[["choice"]] # used for catch and other variables

  # for now if no time, only allow mean based on group without options TODO: allow
  # options from tab 2, currently turned off ti=find([data.isTime])# TODO add option for other time
  if (all(is_empty(date_cols(dataset)))) {
    
    warning("No time variable found, only averaging in groups and per zone is capable")
  }

  # TODO: treating entire dataset as one fleet when defineFleet = "fleet" is confusing,
  # esp if your fleet variable is named "fleet". Change default to NULL. 
  
  # Check that define group is either empty of an actual variable in the dataset
  if (defineGroup != "fleet") {
    
    if (any(is.null(dataset[[defineGroup]]))) {
      
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

  # TODO: Allow user to select which of these to run (if any).
  # TODO: Skip short, med, long expectations if they already exist or else overwrite
  # Otherwise they will be duplicated in the exp list. 
    
  ## 1. Option 1. Short-term, individual grouping t - 2 (window)
 short_exp <- short_expectations(
    dat = dataset, project = project, catch = catch, price = price,
    defineGroup = defineGroup, temp.var = temp.var, temporal = temporal,
    calc.method = calc.method, lag.method = lag.method, empty.catch = empty.catch,
    empty.expectation = empty.expectation, dummy.exp = FALSE
  )


   ## 2. Option 2 medium: group by fleet (all vessels in dataset) t -7
  med_exp <- medium_expectations(
    dat = dataset, project = project, catch = catch, price = price,
    defineGroup = defineGroup, temp.var = temp.var, temporal = temporal,
    calc.method = calc.method, lag.method = lag.method, empty.catch = empty.catch,
    empty.expectation = empty.expectation, dummy.exp = FALSE
  )

  ## 3. option 3  last year, group by fleet t-7
  long_exp <- long_expectations(
    dat = dataset, project = project, catch = catch, price = price,
    defineGroup = defineGroup, temp.var = temp.var, temporal = temporal,
    calc.method = calc.method, lag.method = lag.method, empty.catch = empty.catch,
    empty.expectation = empty.expectation, dummy.exp = FALSE
  )


  # check whether defining a group or using all fleet averaging
  if (defineGroup == "fleet") {
    # just use an id=ones to get all info as one group
    numData <- rep(1, dim(dataset)[1])
    # Define by group case
  } else {
    
    numData <- as.integer(as.factor(dataset[[defineGroup]]))
  }

  z_ind <- which(dataZoneTrue == 1)
  numData <- numData[z_ind]
  
  spData <- choice$g[z_ind] # mapping to to the map file (zones)
  spNA <- which(is.na(spData))
  
  # is this checking for NaNs, NAs? Not sure this check works
  if (any(!is_empty(spNA))) {
    
    spData[spNA] <- rep(Inf, length(spNA)) # better for grouping than nas because aggregated
  }
  
  numNAN <- which(is.nan(numData))
  
  if (any(!is_empty(numNAN))) {
    
    numData[numNAN] <- rep(Inf, length(numNAN))
  }

  
  temp <- data.frame(numData = numData, spData = as.character(spData))
  B <- unique(temp) # B are actual unique items
  
  # C = row ID of those unique items
  C <- match(paste(temp$numData, temp$spData, sep = "*"), 
             paste(B$numData, B$spData, sep = "*")) 

  catchData <- as.numeric(dataset[[catch]][z_ind])
  
  if (price != "none" && !is_value_empty(price)) {
    
    priceData <- as.numeric(dataset[[price]][z_ind])
    catchData <- catchData * priceData
  }

  # Time variable not chosen if temp.var is empty
  # NOTE currently doesn't allow dummy or other options if no time detected
  if (temp.var == "none" || is_value_empty(temp.var)) {
    # temp.var <-  colnames(dataset)[grep("date", colnames(dataset), ignore.case=TRUE)[1]]
    #  }

    allCatch <- stats::aggregate(catchData, list(C), mean, na.rm = TRUE) # currently no replacement for nans
    # Above line is grouping by the alternatives through the C above
    # [bi,~,ci]=unique([numData],'rows','Stable')
    bi <- unique(numData)
    ci <- match(numData, unique(numData))

    newCatch <- as.data.frame(matrix(NA, nrow = length(C), ncol = length(unique(B[, 2])))) # preallocating
    colnames(newCatch) <- names(table(B[, 2]))
    # col are alt choices, rows are observations
    for (w in 1:length(C)) {
      
      col <- B[C[w], 2]
      newCatch[which(ci == ci[w]), col] <- allCatch[C[w], 2]
    }
    
    newDumV <- list()
    # End No time variable temp.var
  } else {

    tiData <- date_parser(dataset[[temp.var]][z_ind]) # this part involves time which is more complicated

    if (temporal == "daily") {
      # daily time line
      tiDataFloor <- lubridate::floor_date(tiData, unit = "day") # assume, we are talking day of for time
      tLine <- sort(unique(tiDataFloor)) 
      
      tLine <- seq.Date(from = min(tLine), to = max(tLine), by = "day")
      
    } else if (temporal == "sequential") {
      # case u1 # observation time line
      tiDataFloor <- tiData # just keeping things consistent
      tLine <- data.frame(sort(unique(tiData))) # unique(tiData)
    }

    timeRange <- temp.window
    lagTime <- temp.lag
    yearRange <- year.lag

    # Use R's rollapply and lag function to calculate moving average
    # Empty values are replaced as defined above
    
    df <- data.frame(numData = numData,
                     spData = as.character(spData), 
                     catchData = as.numeric(catchData),
                     tiData = as.Date(tiData))
    
    # Note: this Averages catch by zone, day, and fleet before applying window fun 
    
    df2 <- stats::aggregate(catchData ~ numData + spData + tiData, 
                            data = df, FUN = mean, na.rm = TRUE) 
    
    df2 <- df2[order(df2$numData, df2$spData, df2$tiData), ]
    
    #  ID = fleet + zone
    df2$ID <- paste0(df2$numData, df2$spData)

    # Lag time
    if (sum(duplicated(df2$ID)) == 0) {
      
      lagTime <- 0
      warning("Selected groups and choice data results in only single observations. ",
              "Cannot use lag time for chosen group and choice data. Setting lag time to 0.")
      
    } else if ((sum(duplicated(df2$ID)) / length(df2$ID)) < .25) {
      
      lagTime <- 0
      warning(paste0(
        "Selected groups and choice data results in ", 
        sum(duplicated(df2$ID) == FALSE) / length(df2$ID) * 100,
        "% of observations with only single observations. Cannot use lag time for ",
        "choosen group and choice data. Setting lag time to 0.")
        )
    }

    if (lagTime > 0) {
      
      df2$lag.value <- c(rep(NA, lagTime), df2$catchData[-c(1:lagTime)])
      # Ask why values from non-duplicated IDs are replaced w/ NAs 
      df2$lag.value[which(!duplicated(df2$ID))] <- NA
      
    } else {
      
      df2$lag.value <- df2$catchData
    }
    
    
    if (lagTime > 2) { # check this
      
      for (i in 1:(lagTime - 1)) {
        
        df2$lag.value[(which(!duplicated(df2$ID)) + lagTime - i)] <- NA
      }
    }

    # Moving window averaging
    myfunc_ave <- function(x, y) {
      
      ind <- df2$tiData >= x - yearRange - timeRange & 
        df2$tiData <= x - yearRange & df2$ID == y
      
      mean(df2$lag.value[ind], na.rm = TRUE)
    }
    
    df2$ra <- mapply(myfunc_ave, df2$tiData, df2$ID)
    # dd <- purrr::map2_dbl(df2$tiData, df2$ID, myfunc_ave) # consider if this is worth using instead

    # #Replace empty values
    na_ind <- is.na(df2$ra)
    
    if (sum(na_ind) > 0) {
      
      # TODO: treat -Inf before replacing empty catch
      
      if (is.na(empty.catch) || is_value_empty(empty.catch)) { # Default
        
        # "AllCatch" treatment: avg catch for year
        myfunc_emp <- function(x) {
          
          year_all <- lubridate::year(df2$tiData)
          x_year <- lubridate::year(x)
          ind <- year_all == x_year
          
          mean(df2$lag.value[ind], na.rm = TRUE)
        }
        
        df2$ra[na_ind] <- vapply(df2$tiData[na_ind], myfunc_emp, numeric(1))
        
        # replaceValue <- NA
      } else if (empty.catch == 0) {
        
        df2$ra[na_ind] <- 0
        
      } else if (empty.catch == "allCatch") {
        
        myfunc_AC <- function(x) {
          
          year_all <- lubridate::year(df2$tiData)
          x_year <- lubridate::year(x)
          ind <- year_all == x_year
          
          mean(df2$lag.value[ind], na.rm = TRUE)
        }
        
        df2$ra[na_ind] <- vapply(df2$tiData[na_ind], myfunc_AC, numeric(1))
        
      } else if (empty.catch == "groupedCatch") {
        # average catch for year and fleet
        myfunc_GC <- function(x, y) {
          
          year_all <- lubridate::year(df2$tiData)
          x_year <- lubridate::year(x)
          ind <- year_all == x_year & df2$ID == y
          
          mean(df2$lag.value[ind], na.rm = TRUE)
        }
        
        df2$ra[na_ind] <- mapply(myfunc_GC, df2$tiData[na_ind], df2$ID[na_ind])
      }
    }

    # df2$ra <- zoo::rollapply(df2$lag.value, timeRange, mean, partial = T, fill = replaceValue)

    # reshape catch data to wide format with date as columns
    
    # meanCatchSimple <- 
    #   stats::reshape(df2[, c("numData", "spData", "tiData", "ra")],
    #                  idvar = c("numData", "spData"), timevar = "tiData", direction = "wide")
    
    meanCatchSimple <-
      tidyr::pivot_wider(df2[c("ID", "numData", "spData", "tiData", "ra")], 
                         names_from = "tiData", values_from = "ra", 
                         values_fill = NA)
    
    # Note: tibbles don't use rownames, adjust code (first 3 cols are ID, fishery, and zone)
    # rownames(meanCatchSimple) <- paste0(meanCatchSimple$spData, meanCatchSimple$numData)
    
    
    dummyTrack <- meanCatchSimple[, -c(1:3)] # preallocate for tracking no value

    if (calc.method == "simpleLag") {
      # at this point could use means to get a regression compared to a lag of the same calculation at all zones, then use that to predict...

      # need to multiply polys by constant

      if (lag.method == "simple") {
        
        meanCatch <- matrix(NA, nrow = nrow(meanCatchSimple), ncol = ncol(meanCatchSimple) - 3)
        
        for (q in 1:nrow(meanCatchSimple)) {
          
          # old approach with ID as rownames (first 2 cols are fishery and zone)
          # meanCatch[q, ] <- polyval(
          #   stats::coef(stats::lm(as.numeric(meanCatchSimple[q, 3:(ncol(meanCatchSimple) - 1)]) ~
          #   as.numeric(meanCatchSimple[q, 4:ncol(meanCatchSimple)]))),
          #   as.numeric(meanCatchSimple[q, 3:ncol(meanCatchSimple)])
          # ) # polyval(polys[q,],meanCatchSimple[q,])
          
          # using tibble (confirm that this works)
          meanCatch[q, ] <- polyval(
            stats::coef(stats::lm(as.numeric(meanCatchSimple[q, 4:(ncol(meanCatchSimple) - 1)]) ~
                                    as.numeric(meanCatchSimple[q, 5:ncol(meanCatchSimple)]))),
            as.numeric(meanCatchSimple[q, 4:ncol(meanCatchSimple)])
          )
        }
        
      } else {
        # case 2
        # old version using rownames
        
        # polys <- as.data.frame(meanCatchSimple[, 4:ncol(meanCatchSimple)]) /
        #   as.data.frame(meanCatchSimple[, 3:(ncol(meanCatchSimple) - 1)])
        # # pad last measurement mean of polys. Using means is more robust against extreme values.
        # polys[, (ncol(polys) + 1)] <- apply(as.data.frame(meanCatchSimple[, 4:ncol(meanCatchSimple)]) /
        #   as.data.frame(meanCatchSimple[, 3:(ncol(meanCatchSimple) - 1)]), 1, mean, na.rm = T) # polys[, ncol(polys)]
        # meanCatch <- meanCatchSimple[, -c(1, 2)] * polys
        
        # w/ tibble (check that this works)
        polys <- as.data.frame(meanCatchSimple[, 4:ncol(meanCatchSimple)]) /
          as.data.frame(meanCatchSimple[, 3:(ncol(meanCatchSimple) - 1)])
        # pad last measurement mean of polys. Using means is more robust against extreme values.
        polys[, (ncol(polys) + 1)] <- apply(as.data.frame(meanCatchSimple[, 4:ncol(meanCatchSimple)]) /
                                              as.data.frame(meanCatchSimple[, 3:(ncol(meanCatchSimple) - 1)]), 1, mean, na.rm = T) # polys[, ncol(polys)]
        meanCatch <- meanCatchSimple[, -c(1, 2)] * polys
      }
      
    } else if (calc.method == "weights") {
      
      print("do nothing?")
      meanCatch <- meanCatchSimple[, -c(1:3)]
      
    } else { # standardAverage
      
      meanCatch <- meanCatchSimple[, -c(1:3)]
    }

    bi <- match(tiDataFloor, unique(tiData), nomatch = 0) 

    # this is the time for each alternative ('occurence level')
    temp <- data.frame(numData = numData, tiDataFloor = tiDataFloor)
    
    # unique fleet + date
    bit <- unique(temp)
    cit <- match(paste(temp$numData, temp$tiDataFloor, sep = "*"), 
                 paste(bit$numData, bit$tiDataFloor, sep = "*"))

    newCatch <- matrix(NA, nrow = length(bi), ncol = length(unique(B$spData)), 
                       dimnames = list(NULL, names(table(B$spData))))
    # Q: leave as matrix?
    newCatch <- as.data.frame(newCatch)
    
    # TODO: This is slow, needs to be more efficient 
    for (w in 1:length(bi)) {
      
      # zone
      zone <- B$spData[C[w]]
      # fleet
      flt <- B$numData[C[w]]
      # ID
      ID <- paste0(flt, zone)
      
      # the following is the output that is NROWS by number of alternatives
      val <- meanCatch[meanCatchSimple$ID == ID,
                       colnames(meanCatch) == tiDataFloor[w]]
      
      # loop shouldn't be necessary but no loop results in out of memory issue
      
      newCatch[cit == cit[w], zone] <- val
    }
    
    return(NULL)
    
    if (dummy.exp) {
      
      dv <- matrix(1, dim(meanCatch)[1], dim(meanCatch)[2], 
                   dimnames = dimnames(meanCatch))
      
      dv[is.na(meanCatch)] <- 0
      
      dummyV <- matrix(0, dim(newCatch)[1], dim(newCatch)[2], 
                       dimnames = dimnames(newCatch))
      
      for (w in 1:length(bi)) {
       
        zone <- B$spData[C[w]]
        dummyV[cit == cit[w], zone] <- dv[C[w], bi[w]]
        # the following is the output that is NROWS by number of alternatives
      }
      
      newDumV <- list(matrix = dummyV,
                      scale = 1,
                      units = "T/F")
      
    } else {
      
      newDumV <- NULL
    }
    
    if (is_value_empty(empty.expectation)) {
      
      newCatch[is.na(newCatch)] <- 0.0001
      
    } else if (empty.expectation == 1e-04) {
      
      newCatch[is.na(newCatch)] <- 1e-04
      newCatch[newCatch == 0] <- 1e-04
      
    } else if (empty.expectation == 0) {
      
      newCatch[is.na(newCatch)] <- 0
      
    } else {
      # case 'no replacement'
      newCatch[is.na(newCatch)] <- 0.0001
    }

  } # end of time vs not time

  r <- nchar(sub("\\.[0-9]+", "", mean(as.matrix(newCatch), na.rm = TRUE))) 
  sscale <- 10^(r - 1)

  ExpectedCatch <- list(
    short_exp = short_exp[1],
    short_exp_newDumV = short_exp[2],
    med_exp = med_exp[1], # 
    med_exp_newDumV = med_exp[2],
    long_exp = long_exp[1], 
    long_exp_newDumv = long_exp[2],
    user_defined_exp = newCatch, 
    scale = sscale,
    newDumV = newDumV,
    units = ifelse(grepl("lbs|pounds", catch, ignore.case = TRUE) == TRUE, "LBS", "MTS") # units of catch data
    # newGridVar.file=[]
  )

  single_sql <- paste0(project, "ExpectedCatch")

  if (replace.output == FALSE) {
    
    if (table_exists(single_sql, project)) {
      
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
