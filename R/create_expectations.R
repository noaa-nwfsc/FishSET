#' Expected catch

#' @param dataset Observe or VMS data
#' @param gridfile gridded data
#' @param catch Catch variable for averaging
#' @param temporal Daily = Daily time line or sequential = sequential order
#' @param temp.var Temporal variable for averaging
#' @param calc.method select standard average, simple lag = simple lag regression of means, or weights= weights of regressed groups
#' @param lag.method Simple: use region over entire group. Grouped: Use regression for individual time periods
#' @param empty.catch Replace empty catch with NAN, 0, all catch = mean of all catch, group catch = mean of grouped catch
#' @param empty.expectation Do not replace or replace with 0.0001 or 0
#' @param temp.window Window size for averaging. Defaults to 1
#' @param temp.lag Lag time for averaging
#' @param dummy.exp T/F. Defaults to False. If false, no dummy variable is outputted. If true, output dummy variable for originally missing value.
#' @param AltMatrixName Does not need to specified if ALT has been generated in createAlternativeChoice function
#' @param defineGroup If empty, data is treated as a fleet
#' @importFrom lubridate floor_date
#' @importFrom zoo rollapply
#' @importFrom DBI dbGetQuery
#' @importFrom stats aggregate reshape coef lm
#' @export create_expectations
#' @return newGridVar dataframe. Saved to the global environment. Dataframe called in make_model_design
#' @details Used during model creation to create an expectations of catch for alternative choices that are added to the model design file.
#' The expectations created have several options and are created based on the group and time averaging choices of the user.
#' The spatial alternatives are built in to the function and come from the structure Alt.
#' NOTE: currently empty values and values ==nan are considered to be times of no fishing activity whereas values in the catch variable choosen ==0
#' are considered fishing activity with no catch and so those are included in the averaging and dummy creation as a point in time when fishing occurred.

#' @return newGridVar,  newDumV
# 

# lubridate # to get floor of temporal variables


create_expectations <- function(dataset, gridfile, catch, temporal = c("daily", "sequential"), temp.var, 
                                calc.method = c("standard average", "simple lag", "weights"), lag.method = c("simple", "grouped"),
                                empty.catch = c(NULL, 0, "all catch", "grouped catch"), empty.expectation = c(NULL, 1e-04, 0),  
                                temp.window = 1, temp.lag = 1, dummy.exp = FALSE, AltMatrixName = NULL, defineGroup = NULL) {
  
  if (!exists("Alt")) {
    if (!exists('AltMatrixName')) {
      fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
      Alt <- unserialize(DBI::dbGetQuery(fishset_db, "SELECT AlternativeMatrix FROM data LIMIT 1")$AlternativeMatrix[[1]])
      DBI::dbDisconnect(fishset_db)
      if (!exists("Alt")) {
        stop("Alternative Choice Matrix does not exist. Please run the createAlternativeChoice() function.")
      }
    }
  }
  
  dataZoneTrue <- Alt[["dataZoneTrue"]]  # used for catch and other variables
  choice <- Alt[["choice"]]  # used for catch and other variables
  zoneRow <- Alt[["zoneRow"]]
  
  # for now if no time.. only allow mean based on group without options TODO: allow
  # options from tab 2, currently turned off ti=find([data.isTime])# TODO add optin
  # for other time
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
  if (!is.empty(spNAN)) {
    spData[spNAN] = Inf  # better for grouping than nans because aggregated
  }
  numNAN = which(is.nan(numData) == T)
  if (!is.empty(numNAN)) {
    numData[numNAN] = Inf
  }
  
  # [B,I,C]=unique([numData,spData],'rows')
  temp <- cbind(numData, as.character(spData))
  B <- unique(temp)  # B are actual unique items
  # I = something not so importat
  C <- match(paste(temp[, 1], temp[, 2], sep = "*"), paste(B[, 1], B[, 2], sep = "*"))  #C = row ID of those unique items
  
  catchData <- as.numeric(dataset[[catch]][which(dataZoneTrue == 1)])
  
  # Time variable not chosen if isempty(ti #NOTE currently doesn't allow dummy or other options if no time detected
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
    } else if (empty.catch == "all catch") {
      replaceValue <- mean(catchData, na.rm = T)
    } else if (empty.catch == "grouped catch") {
      replaceValue <- aggregate(catchData, list(C), mean, na.rm = T)
    }
    
    # Use R's rollapply and lag function to calculate moving average 
    # Empty values are replaced as defined above
    df <- as.data.frame(cbind(numData, spData = as.character(spData), catchData = as.numeric(catchData), 
                              tiData = as.Date(tiData)))
    df$tiData <- as.Date(tiData)
    df$catchData <- as.numeric(df$catchData)
    # rolledAvg <-
    df2 <- stats::aggregate(df, by = list(numData = numData, spData = spData, tiData = tiData), 
                     mean, na.rm = T)[, c(1, 2, 3, 6)]
    df2 <- df2[order(df2$numData, df2$spData, df2$tiData), ]
    df2$ID <- paste(df2$numData, df2$spData, sep = "")
    df2$lag.value <- c(rep(NA, lagTime), df2$catchData, n = -lagTime)
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
    
    
    if (calc.method == "standard average") {
      meanCatch <- meanCatchSimple[, -c(1, 2)]
    } else if (calc.method == "simple lag") {
      # at this point could use means to get a regression compared to a lag of the same
      # calculation at all zones, then use that to predict...
      
      # need to multiply polys by constant
      
      # switch get(cp2V2,'Value')
      if (lag.method == "simple") {
        polys <- data.frame(matrix(NA, nrow = nrow(meanCatchSimple), ncol = 2))  #nan(size(meanCatchSimple,1),2)
        for (q in 1:nrow(meanCatchSimple)) {
          meanCatch[q, ] <- polyval(stats::coef(stats::lm(as.numeric(meanCatchSimple[q,4:ncol(meanCatchSimple)]) ~  
                                            as.numeric(meanCatchSimple[q, 3:(ncol(meanCatchSimple) - 1)]))), 
                                    as.numeric(meanCatchSimple[q, 3:ncol(meanCatchSimple)]))  #polyval(polys[q,],meanCatchSimple[q,])
        }
      } else {
        # case 2
        polys <- as.data.frame(meanCatchSimple[, 4:ncol(meanCatchSimple)]) / 
                                    as.data.frame(meanCatchSimple[, 3:(ncol(meanCatchSimple) - 1)])
        # pad last measurement?with same as end
        polys[, (ncol(polys) + 1)] <- polys[, ncol(polys)]
        meanCatch <- meanCatchSimple[, -c(1, 2)] * polys
      }
    } else if (calc.method == "weights") {
      print("do nothing?")
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
    if (dummy.exp == FALSE) {
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
        newCatch[is.na(newCatch)] <- 0  #c(NULL, 0.0001, 0)  #switch replaceEmptyExpAll{get(dp2V5,'Value')}
      } else {
        # case 'no replacement'
        newCatch = newCatch
      }
      
      
    } else if (dummy.exp == TRUE) {
      
      newCatch <- as.data.frame(matrix(NA, nrow = length(bi), ncol = ncol(zoneRow)))
      dv <- as.data.frame(matrix(1, dim(meanCatch)[1], dim(meanCatch)[2]))  #length(ones(size(meanCatch))
      # dv(~emptyCellsCatch)=1% non empty=1 deprecated
      dv[is.na(dummyTrack), ] <- 0  #
      # Mis.Dum, takes a value of one whenever the expected location- and haul-specific
      # estimates of revenues and bycatch are not empty NOTE: currently doesn't include catch==0
      dummyV <- as.data.frame(matrix(0, dim(newCatch)[1], dim(newCatch)[2]))  #zeros(size(newCatch))
      for (w in 1:length(bi)) {
        # if ~isinf(B(C(w),end))
        col <- B[C[w], 2]  #col=find(Alt.zoneRow==B(C(w),end))
        newCatch[which(cit == cit[w]), col] <- meanCatch[C[w], bi[w]]  #newCatch(cit==cit(w),col)=meanCatch(C(w),bi(w))
        dummyV[which(cit == cit[w]), col] <- dv[C[w], bi[w]]
        # the following is the output that is NROWS by number of alternatives
      }

  newDumV <<- list(
    matrix = dummyV,
    scale = 1,
  # name = name,
    units = 'T/F'
  # file = []
   )

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
  r <- nchar(sub('\\.[0-9]+', '', mean(mean(newCatch,na.rm=T),na.rm=T))) #regexp(num2str(nanmax(nanmax(newCatch))),'\.','split')
  sscale <- 10^(r-1)  

  ExpectedCatch <<- list(
     matrix = newCatch,
     scale = sscale,
     units = ifelse(grepl('lbs|pounds', catch, ignore.case = T)==T, 'LBS', 'MTS') #units of catch data
     #newGridVar.file=[]
     )


#write(layout.json.ed(trace, 'create_expectations', deparse(substitute(dataset)), x='', 
#                          msg=paste('gridfile:', deparse(substitute(gridfile)), ', catch:', deparse(substitute(catch)), 
#                                    ', defineGroup:', defineGroup, ', temporal:', temporal, ', temp.var:', temp.var, ',temp.window:', temp.window,
#                                    ', temp.lag:', temp.lag, ', calc.method:', calc.method, ', lag.method:', lag.method, ', empty.catch:', empty.catch,
#                                    ', empty.expectation:', empty.expectation, ', empty.expectation:', empty.expectation)), 
#           paste(getwd(),'/Logs/',Sys.Date(),'.json', sep=''), append=T )


create_expectations_function <- list()
create_expectations_function$functionID <- 'create_expectations'
create_expectations_function$args <- c(deparse(substitute(dataset)), deparse(substitute(gridfile)), catch, temporal, temp.var, calc.method, lag.method, 
                                    empty.catch, empty.expectation, temp.window, temp.lag)
create_expectations_function$kwargs <- list('AltMatrixName'=AltMatrixName, 'defineGroup'=defineGroup)
functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (create_expectations_function)
body$fishset_run <- list(infoBodyout, functionBodyout)
write(jsonlite::toJSON(body, pretty = TRUE, auto_unbox = TRUE),paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
list2env(functionBodyout, envir = .GlobalEnv)
}


