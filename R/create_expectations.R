#' Expected catch

#' @param dat  Main data frame containing data on hauls or trips. Table in fishset_db database should contain the string `MainDataTable`.
#' @param gridfile Spatial data. Shape, json, and csv formats are supported.
#' @param catch Variable containing catch data.
#' @param temporal Daily (Daily time line) or sequential (sequential order)
#' @param temp.var Variable containing temporal data
#' @param calc.method Select standard average (standardAverage), simple lag regression of means (simpleLag), or weights of regressed groups (weights)
#' @param lag.method  Use regression over entire group (simple) or for grouped time periods (grouped)
#' @param empty.catch Replace empty catch with NA, 0, mean of all catch (allCatch), or mean of grouped catch(groupCatch) 
#' @param empty.expectation Do not replace (NULL) or replace with 0.0001 or 0
#' @param temp.window Temporal window size. Defaults to 1
#' @param temp.lag Temporal lag time in days.
#' @param dummy.exp T/F. Defaults to False. If false, no dummy variable is outputted. If true, output dummy variable for originally missing value.
#' @param AltMatrixName Does not need to specified if ALT has been generated in \code{\link{create_alternative_choice}} function.
#' @param defineGroup If empty, data is treated as a fleet
#' @importFrom lubridate floor_date
#' @importFrom zoo rollapply
#' @importFrom DBI dbGetQuery
#' @importFrom stats aggregate reshape coef lm
#' @importFrom signal polyval
#' @export create_expectations
#' @return newGridVar dataframe. Saved to the global environment. Dataframe called in make_model_design
#' @details Used during model creation to create an expectation of catch for alternative choices that are added to the model design file.
#' The expectations created have several options and are created based on the group and time averaging choices of the user.
#' The spatial alternatives are built in to the function and come from the structure Alt. 
#' The primary choices are whether to treat data as a fleet or to group the data (defineGroup) and the time frame of catch data for calculating expected catch.
#' Values must be provided for `defineGroup` and `temporal` parameters. If data should be treated as a fleet, set defineGroup to NULL. If the entire record of catch data is to be used, set temporal to NULL.
#' Empty catch values are considered to be times of no fishing activity whereas values of 0 in the catch variable 
#' are considered fishing activity with no catch and so those are included in the averaging and dummy creation as a point in time when fishing occurred.

#' @return newGridVar,  newDumV
#' @examples 
#' \dontrun{
#' create_expectations(MainDataTable, adfg, 'OFFICIAL_TOTAL_CATCH_MT',  temporal='daily', 
#'                      temp.var="DATE_FISHING_BEGAN", calc.method='standard average', 
#'                      lag.method='simple',  empty.catch='all catch', empty.expectation= 0.0001, 
#'                      temp.window=4, temp.lag=2, dummy.exp=FALSE, 
#'                      AltMatrixName='pcodaltmatrix20110101', defineGroup=NULL)
#' }


create_expectations <- function(dat, gridfile, catch, defineGroup = NULL, temp.var=NULL, temporal = c("daily", "sequential"), 
                                calc.method = c("standardAverage", "simpleLag", "weights"), lag.method = c("simple", "grouped"),
                                empty.catch = c(NULL, 0, "allCatch", "groupedCatch"), empty.expectation = c(NULL, 1e-04, 0),  
                                temp.window = 1, temp.lag = 1, dummy.exp = FALSE, AltMatrixName = NULL) {
  
  #Call in datasets
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
  if(is.character(dat)==TRUE){
    if(is.null(dat)==TRUE | table_exists(dat)==FALSE){
      print(DBI::dbListTables(fishset_db))
      stop(paste(dat, 'not defined or does not exist. Consider using one of the tables listed above that exist in the database.'))
    } else {
      dataset <- table_view(dat)
    }
  } else {
    dataset <- dat  
  }
  DBI::dbDisconnect(fishset_db)
  
  
  if (!exists("Alt")) {
    if (!exists('AltMatrixName')) {
      stop('Alternative choice matrix not found. Please run the createAlternativeChoice() function or 
           define the Alternative choice matrix name from the sqlite database. Name will contain altmatrix in it along with date created and project if defined.')
      } else {
      fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
      Alt <- unserialize(DBI::dbGetQuery(fishset_db, "SELECT AlternativeMatrix FROM", AltMatrixName, "LIMIT 1")$AlternativeMatrix[[1]])
      DBI::dbDisconnect(fishset_db)
    }
  }
  
  dataZoneTrue <- Alt[["dataZoneTrue"]]  # used for catch and other variables
  choice <- Alt[["choice"]]  # used for catch and other variables
  zoneRow <- Alt[["zoneRow"]]
  
  # for now if no time, only allow mean based on group without options TODO: allow
  # options from tab 2, currently turned off ti=find([data.isTime])# TODO add option for other time
  if (!any(grepl("DATE|MIN", colnames(dataset)))) {
    warning("No time variable found, only averaging in groups and per zone is capable")
  }
  
  # Check that define group is either empty of an actual variable in the dataset
  if (!is.empty(defineGroup)) {
    if (any(is.null(dataset[[defineGroup]]))) {
      stop("defineGroup not recognized. Check that parameter is correctly defined")
    }
  }
  
  
  # check whether defining a group or using all fleet averaging 
  if (is.null(defineGroup)) {
    # just use an id=ones to get all info as one group
    numData <- data.frame(rep(1, dim(dataset)[1]))  #ones(size(data(1).dataColumn,1),1)
    # Define by group case u1hmP1
  } else {
    numData = as.integer(dataset[[defineGroup]])
  }
  
  numData = as.data.frame(numData)[which(dataZoneTrue == 1), ]  #(Alt.dataZoneTrue,:)
  spData = choice[which(dataZoneTrue == 1), ]  # mapping to to the map file
  spNAN = which(is.na(spData) == T)
  if (any(!is.empty(spNAN))) {
    spData[spNAN] = rep(Inf, length(spNAN))  # better for grouping than nans because aggregated
  }
  numNAN = which(is.nan(numData) == T)
  if (any(!is.empty(numNAN))) {
    numData[numNAN] = rep(Inf, length(numNAN))
  }
  
  # [B,I,C]=unique([numData,spData],'rows')
  temp <- cbind(numData, as.character(spData))
  B <- unique(temp)  # B are actual unique items
  # I = something not so importat
  C <- match(paste(temp[, 1], temp[, 2], sep = "*"), paste(B[, 1], B[, 2], sep = "*"))  #C = row ID of those unique items
  
  catchData <- as.numeric(dataset[[catch]][which(dataZoneTrue == 1)])
  
  # Time variable not chosen if temp.var is empty
  #NOTE currently doesn't allow dummy or other options if no time detected
  if (is.empty(temp.var)) {
    
    allCatch = stats::aggregate(catchData, list(C), mean, na.rm = T)  #accumarray(C,catchData,[],@nanmean)# currently no replacement for nans
    # Above line is grouping by the alternatives through the C above
    # [bi,~,ci]=unique([numData],'rows','Stable')
    bi <- unique(numData)
    ci <- match(numData, unique(numData))
    
    newCatch = as.data.frame(matrix(NA, nrow = length(C), ncol = length(unique(B[,2])))) #nan(length(C),length(Alt.zoneRow))# preallocating
    colnames(newCatch) = names(table(B[, 2]))
    # col are alt choices, rows are observations
    for (w in 1:length(C)) {
      col <- B[C[w], 2]
      newCatch[which(ci == ci[w]), col] <- allCatch[C[w], 2]
    }
    
    # End No time variable temp.var
  } else {
    tiData <- dataset[[temp.var]][which(dataZoneTrue == 1)]  #(ti(get(mp3V1,'Value'))).dataColumn(Alt.dataZoneTrue,:) # this part involves time which is more complicated
    
    if (temporal == "daily") {
      # daily time line
      tiDataFloor <- lubridate::floor_date(as.Date(tiData), unit = "day")  # assume, we are talking day of for time
      tLine <- sort(unique(tiDataFloor))  #min(tiDataFloor):max(tiDataFloor)
    } else if (temporal == 'sequential') {
      # case u1 # observation time line
      tiDataFloor <- tiData  # just keeping things consistent
      tLine <- data.frame(unique(tiData))  #unique(tiData) 
    }
    
    
    
    timeRange <- temp.window
    lagTime <- temp.lag
    yearRange <- 1  #Not used?  get(jhSpinnerYear,'Value')
    
    # NOTE, currently this is replaced midstream. 
    #If we want to have different ways of handling emptys versus edges of catch then would change that here 
    if (empty.catch == "NULL") {
      replaceValue <- NA
    } else if (empty.catch == "0") {
      replaceValue <- 0
    } else if (empty.catch == "allCatch") {
      replaceValue <- mean(catchData, na.rm = T)
    } else if (empty.catch == "groupedCatch") {
      replaceValue <- aggregate(catchData, list(C), mean, na.rm = T)
    }
    
    # Use R's rollapply and lag function to calculate moving average 
    # Empty values are replaced as defined above
    df <- as.data.frame(cbind(numData, spData = as.character(spData), catchData = as.numeric(catchData), 
                              tiData = as.Date(tiData)))
    df$tiData <- as.Date(tiData)
    df$catchData <- as.numeric(df$catchData)
    # rolledAvg <-
    df2 <- suppressWarnings(stats::aggregate(df, by = list(numData = numData, spData = spData, tiData = tiData), 
                     mean, na.rm = T))[, c(1, 2, 3, 6)]
    df2 <- df2[order(df2$numData, df2$spData, df2$tiData), ]
    df2$ID <- paste(df2$numData, df2$spData, sep = "")
    df2$lag.value <- c(rep(NA, lagTime), df2$catchData[-c(1:lagTime)])
    df2$lag.value[which(!duplicated(df2$ID))] <- NA
    x <- lagTime
    for (i in 1:(lagTime - 1)) {
      df2$lag.value[(which(!duplicated(df2$ID)) + lagTime - i)] <- NA
    }
    df2$ra <- zoo::rollapply(df2$lag.value, timeRange, mean, partial = T, fill = replaceValue)
    
    # meanCatchSimple <- left_join(df, rolledAvg) reshape catch data to wide format
    meanCatchSimple <- stats::reshape(df2[, c("numData", "spData", "tiData", "ra")], 
                               idvar = c("numData", "spData"), timevar = "tiData", direction = "wide")
    dummyTrack <- meanCatchSimple[, -c(1, 2)]  # preallocate for tracking no value
    
    
    if (calc.method == "standardAverage") {
      meanCatch <- meanCatchSimple[, -c(1, 2)]
    } else if (calc.method == "simpleLag") {
      # at this point could use means to get a regression compared to a lag of the same calculation at all zones, then use that to predict...
      
      # need to multiply polys by constant
      
      if (lag.method == "simple") {
#        polys <- data.frame(matrix(NA, nrow = nrow(meanCatchSimple), ncol = 2))  #nan(size(meanCatchSimple,1),2)
        for (q in 1:nrow(meanCatchSimple)) {
          meanCatch[q, ] <- signal::polyval(stats::coef(stats::lm(as.numeric(meanCatchSimple[q, 3:(ncol(meanCatchSimple) - 1)]) ~
                                                            as.numeric(meanCatchSimple[q, 4:ncol(meanCatchSimple)])  
                                            )), 
                                    as.numeric(meanCatchSimple[q, 3:ncol(meanCatchSimple)]))  #polyval(polys[q,],meanCatchSimple[q,])
        }
      } else {
        # case 2
        polys <- as.data.frame(meanCatchSimple[, 4:ncol(meanCatchSimple)]) / 
                                    as.data.frame(meanCatchSimple[, 3:(ncol(meanCatchSimple) - 1)])
        # pad last measurement mean of polys. Using means is more robust against extreme values.
        polys[, (ncol(polys) + 1)] <- apply(as.data.frame(meanCatchSimple[, 4:ncol(meanCatchSimple)]) / 
                                              as.data.frame(meanCatchSimple[, 3:(ncol(meanCatchSimple) - 1)]), 1, mean, na.rm=T)#polys[, ncol(polys)]
        meanCatch <- meanCatchSimple[, -c(1, 2)] * polys
      }
    } else if (calc.method == "weights") {
      print("do nothing?")
      meanCatch <- meanCatchSimple[, -c(1, 2)]
    }
    
    
    ## ----> This was empty in FishSET program. ADD LATER??? <-----## check choice for
    ## post calculation dummy issue, this really only affects instances where empty
    ## values have been replaced earlier with a number other then 0 or nan
    ## dummyChoice=get(dp2V3,'String') switch dummyChoice{get(dp2V3,'Value')}#FIXME
    ## this only takes into account values that did not exist as opposed to NAN values
    
    # case 'no replacement'
    
    # case 'replace expectations with nan for originally missing values'
    # meanCatch(emptyCellsCatch)=nan case 'replace expectations with 0 for originally
    # missing values' meanCatch(emptyCellsCatch)=0
    
    # end
    
    
    bi <- match(tiDataFloor, tLine, nomatch = 0)  # [~,bi]=ismember(tiDataFloor,tLine)
    
    # this is the time for each alternative ('occurence level')
    # #[bit,~,cit]=unique([numData, tiDataFloor],'rows','Stable')
    temp <- cbind(numData, tiDataFloor)
    bit <- unique(cbind(numData, tiDataFloor))
    cit <- match(paste(temp[, 1], temp[, 2], sep = "*"), paste(bit[, 1], bit[, 2], sep = "*"))
    
    
    # dummyChoiceOut=get(dp2V4,'String') no dummy variable

      newCatch <- data.frame(matrix(NA, nrow = length(bi), ncol = length(unique(B[, 2]))))
      colnames(newCatch) = names(table(B[, 2]))
      
      for (w in 1:length(bi)) {
        # if ~isinf(B(C(w),end))
        col <- B[C[w], 2]
        # the following is the output that is NROWS by number of alternatives
        newCatch[which(cit == cit[w]), col] <- meanCatch[C[w], bi[w]]  ## loop shouldn't be necessary but no loop results in out of memory issue
      }
      
      if (empty.expectation == 1e-04) {
        newCatch[is.na(newCatch)] <- 1e-04
        newCatch[newCatch == 0] <- 1e-04
      } else if (empty.expectation == 0) {
        # (case 'replace with 0'
        newCatch[is.na(newCatch)] <- 0  
      } else {
        # case 'no replacement'
        newCatch = newCatch
      }
      
      
    if (dummy.exp == TRUE) {
      
      dv <- as.data.frame(matrix(1, dim(meanCatch)[1], dim(meanCatch)[2]))  #length(ones(size(meanCatch))
      # dv(~emptyCellsCatch)=1% non empty=1 deprecated
      dv[is.na(dummyTrack), ] <- 0  #
      # Mis.Dum, takes a value of one whenever the expected location- and haul-specific
      # estimates of revenues and bycatch are not empty NOTE: currently doesn't include catch==0
      dummyV <- as.data.frame(matrix(0, dim(newCatch)[1], dim(newCatch)[2]))  #zeros(size(newCatch))
      for (w in 1:length(bi)) {
        # if ~isinf(B(C(w),end))
        col <- B[C[w], 2]  #col=find(Alt.zoneRow==B(C(w),end))
        dummyV[which(cit == cit[w]), col] <- dv[C[w], bi[w]]
        # the following is the output that is NROWS by number of alternatives
      }

  newDumV <- list(
    matrix = dummyV,
    scale = 1,
  # name = name,
    units = 'T/F'
  # file = []
   )
  attach('newDumV', newDumVm, pos=1)
  
  #replaceEmptyExpAll=get(dp2V5,'String')# replace empty catch
  if(empty.expectation==0.0001) {
     newCatch[is.na(newCatch)] <- 0.0001
     newCatch[newCatch==0] <- 0.0001
  } else if (empty.expectation==0) { #(case 'replace with 0'
     newCatch[is.na(newCatch)] <- 0 #c(NULL, 0.0001, 0)  #switch replaceEmptyExpAll{get(dp2V5,'Value')}
  } else { #case 'no replacement'
     newCatch = newCatch
  }

  } #end dummy.exp==TRUE


  }# end of time vs not time
  r <- nchar(sub('\\.[0-9]+', '', mean(as.matrix(newCatch),na.rm=T))) #regexp(num2str(nanmax(nanmax(newCatch))),'\.','split')
  sscale <- 10^(r-1)  

  ExpectedCatch <- list(
     matrix = newCatch,
     scale = sscale,
     units = ifelse(grepl('lbs|pounds', catch, ignore.case = T)==T, 'LBS', 'MTS') #units of catch data
     #newGridVar.file=[]
     )
  assign("ExpectedCatch", value = ExpectedCatch, pos = 1)

  if(!exists('logbody')) { 
    logging_code()
  } 
  create_expectations_function <- list()
  create_expectations_function$functionID <- 'create_expectations'
  create_expectations_function$args <- c(deparse(substitute(dat)), deparse(substitute(gridfile)), catch, temporal, temp.var, calc.method, lag.method, 
                                    empty.catch, empty.expectation, temp.window, temp.lag, dummy.exp)
  create_expectations_function$kwargs <- list('AltMatrixName'=AltMatrixName, 'defineGroup'=defineGroup)
  create_expectations_function$output <- c()
  functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (create_expectations_function)
  logbody$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE),paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
  assign("functionBodyout", value = functionBodyout, pos = 1)
}

 

