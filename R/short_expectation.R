#' Short expectations
#' @param dat  Main data frame containing data on hauls or trips. Table in FishSET database should contain the string `MainDataTable`.
#' @param project Name of project. Used to pull working alternative choice matrix from FishSET database.
#' @param catch Variable containing catch data.
#' @param price Variable containing price/value data. Used in calculating expected revenue. Leave null if calculating expected catch. Multiplied against catch to generated revenue.
#' @param defineGroup If empty, data is treated as a fleet
#' @param temp.var Variable containing temporal data
#' @param temporal Daily (Daily time line) or sequential (sequential order)
#' @param calc.method Select standard average (standardAverage), simple lag regression of means (simpleLag), or weights of regressed groups (weights)
#' @param lag.method  Use regression over entire group (simple) or for grouped time periods (grouped)
#' @param empty.catch Replace empty catch with NA, 0, mean of all catch (allCatch), or mean of grouped catch(groupCatch)
#' @param empty.expectation Do not replace (NULL) or replace with 0.0001 or 0
#' @param dummy.exp Logical, should dummy variable be outputted. If true, output dummy variable for originally missing value.
#' @importFrom lubridate floor_date
#' @importFrom zoo rollapply
#' @importFrom DBI dbGetQuery
#' @importFrom stats aggregate reshape coef lm
#' @importFrom signal polyval
#' @export short_expectations
#' @keywords internal
#' @return Expected catch matrix. Saved to database via create_expectations

short_expectations <- function(dat, project, catch, price, defineGroup, temp.var, temporal, calc.method,
                               lag.method, empty.catch, empty.expectation, dummy.exp) {

  # Call in datasets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  if (!exists("Alt")) {
    fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project))
    on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
    
    Alt <- unserialize(DBI::dbGetQuery(fishset_db, paste0("SELECT AlternativeMatrix FROM ", project, "altmatrix LIMIT 1"))$AlternativeMatrix[[1]])
  }

  dataZoneTrue <- Alt[["dataZoneTrue"]] # used for catch and other variables
  choice <- Alt[["choice"]] # used for catch and other variables
 
      
  # check whether defining a group or using all fleet averaging
  if (defineGroup == "fleet") {
    # just use an id=ones to get all info as one group
    numData <- data.frame(rep(1, dim(dataset)[1])) # ones(size(data(1).dataColumn,1),1)
    # Define by group case u1hmP1
  } else {
    numData <- as.integer(as.factor(dataset[[defineGroup]]))
  }

 #Requires temporal data. Need to grab data if not provided
      #Identify the first date variable
  if(temp.var=='none' || is_empty(temp.var)){
      is.convertible.to.date <- function(x) !is.na(as.Date(as.character(x), tz = 'UTC', format = '%Y-%m-%d'))
      temp.var <- names(which(apply(dataset, 2, is.convertible.to.date)[1,]==TRUE)[1])
      print(paste('temp.var was not specified. Using', temp.var, 'instead.'))
  }
#  if (temp.var == "none" | is_empty(temp.var)) {
#    temp.var <- colnames(dataset)[grep("date", colnames(dataset), ignore.case = TRUE)[1]]
#    if (length(grep("date", colnames(dataset), ignore.case = TRUE)) > 1) {
#      warning("More than one column matches argument. First column will be used to define temporal variable.")
#    }
#  }

  numData <- as.data.frame(numData)[which(dataZoneTrue == 1), ] # (Alt.dataZoneTrue,:)
  spData <- choice[which(dataZoneTrue == 1), ] # mapping to to the map file
  spNAN <- which(is.na(spData) == T)
  if (any(!is_empty(spNAN))) {
    spData[spNAN] <- rep(Inf, length(spNAN)) # better for grouping than nans because aggregated
  }
  numNAN <- which(is.nan(numData) == T)
  if (any(!is_empty(numNAN))) {
    numData[numNAN] <- rep(Inf, length(numNAN))
  }

  temp <- cbind(numData, as.character(spData))
  B <- unique(temp) # B are actual unique items
  C <- match(paste(temp[, 1], temp[, 2], sep = "*"), paste(B[, 1], B[, 2], sep = "*")) # C = row ID of those unique items

  catchData <- as.numeric(dataset[[catch]][which(dataZoneTrue == 1)])
  if (price != "none" && !is_empty(price)) {
    priceData <- as.numeric(dataset[[price]][which(dataZoneTrue == 1)])
    catchData <- catchData * priceData
  }
  
  # Time variable not chosen if temp.var is empty
  tiData <- as.Date(dataset[[temp.var]][which(dataZoneTrue == 1)], origin = "1970-01-01") # (ti(get(mp3V1,'Value'))).dataColumn(Alt.dataZoneTrue,:) # this part involves time which is more complicated
  if (temporal[1] == "daily") {
    # daily time line
    tiDataFloor <- lubridate::floor_date(as.Date(tiData), unit = "day") # assume, we are talking day of for time
    tLine <- sort(unique(tiDataFloor)) # min(tiDataFloor):max(tiDataFloor)
    tLine <- data.frame(as.Date(min(tLine):max(tLine), origin = "1970-01-01"))
  } else if (temporal[1] == "sequential") {
    # case u1 # observation time line
    tiDataFloor <- tiData # just keeping things consistent
    tLine <- data.frame(sort(unique(tiData))) # unique(tiData)
  } else {
    tiDataFloor <- lubridate::floor_date(as.Date(tiData), unit = "day") # assume, we are talking day of for time
    tLine <- sort(unique(tiDataFloor)) # min(tiDataFloor):max(tiDataFloor)
    tLine <- data.frame(as.Date(min(tLine):max(tLine), origin = "1970-01-01"))
    warning("Temporal time frame not specified. Using daily time line.")
  }

  timeRange <- 2
  lagTime <- 0
  yearRange <- 0 # Not used?  get(jhSpinnerYear,'Value')

  
  # Use R's rollapply and lag function to calculate moving average
  # Empty values are replaced as defined above
  df <- as.data.frame(cbind(numData,
    spData = as.character(spData), catchData = as.numeric(catchData),
    tiData = as.Date(tiData)
  ))
  df$tiData <- as.Date(tiData)
  df$catchData <- as.numeric(df$catchData)
  # rolledAvg <-
  df2 <- suppressWarnings(stats::aggregate(df,
    by = list(numData = numData, spData = spData, tiData = tiData),
    mean, na.rm = T
  ))[, c(1, 2, 3, 6)]
  df2 <- df2[order(df2$numData, df2$spData, df2$tiData), ]
  df2$ID <- paste(df2$numData, df2$spData, sep = "")

  if (length(which(duplicated(df2$ID) == TRUE)) == 0) {
    lagTime <- 0
    warning("Selected groups and choice data results in only single observations. Cannot use lag time for chosen group and choice data.
               Setting lag time to 0.")
  } else if ((length(which(duplicated(df2$ID) == TRUE)) / length(df2$ID)) < .25) {
    lagTime <- 0
    warning(paste0(
      "Selected groups and choice data results in ", length(which(duplicated(df2$ID) == FALSE)) / length(df2$ID) * 100,
      "% of observations with only single observations. Cannot use lag time for choosen group and choice data. Setting lag time to 0."
    ))
  }

  if (lagTime > 0) {
    df2$lag.value <- c(rep(NA, lagTime), df2$catchData[-c(1:lagTime)])
    df2$lag.value[which(!duplicated(df2$ID))] <- NA
  } else {
    df2$lag.value <- df2$catchData
  }
  if (lagTime > 2) {
    for(i in 1:(lagTime - 1)) {
      df2$lag.value[(which(!duplicated(df2$ID)) + lagTime - i)] <- NA
    }
  }

  myfunc_ave <- function(x, y) {
    mean(df2[df2$tiData >= x - yearRange - timeRange & df2$tiData <= x - yearRange & df2$ID == y, "lag.value"], na.rm = TRUE)
  }
  df2$ra <- mapply(myfunc_ave, df2$tiData, df2$ID)

  # #Replace empty values
  if (empty.catch == "NA" || is_empty(empty.catch)) {
    myfunc_emp <- function(x) {
      mean(df2[lubridate::year(df2$tiData) >= format(as.Date(x), format = "%Y") &
        lubridate::year(df2$tiData) < lubridate::year(x) + 1, "lag.value"], na.rm = TRUE)
    }
    df2$ra[which(is.na(df2$ra) == TRUE)] <- unlist(lapply(df2$tiData[which(is.na(df2$ra) == TRUE)], myfunc_emp))
    # replaceValue <- NA
  } else if (empty.catch == "0") {
    df2$ra[which(is.na(df2$ra) == TRUE)] <- 0
  } else if (empty.catch == "allCatch") {
    myfunc_AC <- function(x) {
      mean(df2[lubridate::year(df2$tiData) >= format(as.Date(x), format = "%Y") &
        lubridate::year(df2$tiData) < lubridate::year(x) + 1, "lag.value"], na.rm = TRUE)
    }
    df2$ra[which(is.na(df2$ra) == TRUE)] <- unlist(lapply(df2$tiData[which(is.na(df2$ra) == TRUE)], myfunc_AC))
    # replaceValue <- mean(catchData, na.rm = T)
  } else if (empty.catch == "groupedCatch") {
    myfunc_GC <- function(x, y) {
      mean(df2[lubridate::year(df2$tiData) >= format(as.Date(x), format = "%Y") &
        lubridate::year(df2$tiData) < lubridate::year(x) + 1 & df2$ID == y, "lag.value"], na.rm = TRUE)
    }
    df2$ra[which(is.na(df2$ra) == TRUE)] <- mapply(myfunc_GC, df2$tiData[which(is.na(df2$ra) == TRUE)], df2$ID[which(is.na(df2$ra) == TRUE)])
    # replaceValue <- aggregate(catchData, list(C), mean, na.rm = T)
  } else {
    myfunc_emp <- function(x) {
      mean(df2[lubridate::year(df2$tiData) >= format(as.Date(x), format = "%Y") &
                 lubridate::year(df2$tiData) < lubridate::year(x) + 1, "lag.value"], na.rm = TRUE)
    }
    df2$ra[which(is.na(df2$ra) == TRUE)] <- unlist(lapply(df2$tiData[which(is.na(df2$ra) == TRUE)], myfunc_emp))
  }

  # meanCatchSimple <- left_join(df, rolledAvg) reshape catch data to wide format
  meanCatchSimple <- stats::reshape(df2[, c("numData", "spData", "tiData", "ra")],
    idvar = c("numData", "spData"), timevar = "tiData", direction = "wide"
  )
  rownames(meanCatchSimple) <- paste0(meanCatchSimple$spData, meanCatchSimple$numData)
  
  dummyTrack <- meanCatchSimple[, -c(1, 2)] # preallocate for tracking no value

if (calc.method == "simpleLag") {
    # at this point could use means to get a regression compared to a lag of the same calculation at all zones, then use that to predict...

    # need to multiply polys by constant

    if (lag.method == "simple") {
      meanCatch <- matrix(NA, nrow = nrow(meanCatchSimple), ncol = ncol(meanCatchSimple) - 2)
      #        polys <- data.frame(matrix(NA, nrow = nrow(meanCatchSimple), ncol = 2))  #nan(size(meanCatchSimple,1),2)
      for (q in 1:nrow(meanCatchSimple)) {
        meanCatch[q, ] <- signal::polyval(
          stats::coef(stats::lm(as.numeric(meanCatchSimple[q, 3:(ncol(meanCatchSimple) - 1)]) ~
          as.numeric(meanCatchSimple[q, 4:ncol(meanCatchSimple)]))),
          as.numeric(meanCatchSimple[q, 3:ncol(meanCatchSimple)])
        ) # polyval(polys[q,],meanCatchSimple[q,])
      }
    } else {
      # case 2
      polys <- as.data.frame(meanCatchSimple[, 4:ncol(meanCatchSimple)]) /
        as.data.frame(meanCatchSimple[, 3:(ncol(meanCatchSimple) - 1)])
      # pad last measurement mean of polys. Using means is more robust against extreme values.
      polys[, (ncol(polys) + 1)] <- apply(as.data.frame(meanCatchSimple[, 4:ncol(meanCatchSimple)]) /
        as.data.frame(meanCatchSimple[, 3:(ncol(meanCatchSimple) - 1)]), 1, mean, na.rm = T) # polys[, ncol(polys)]
      meanCatch <- meanCatchSimple[, -c(1, 2)] * polys
    }
  } else if (calc.method == "weights") {
    print("do nothing?")
    meanCatch <- meanCatchSimple[, -c(1, 2)]
  } else {
    meanCatch <- meanCatchSimple[, -c(1, 2)]
  }

  bi <- match(tiDataFloor, unique(tiData), nomatch = 0) # [~,bi]=ismember(tiDataFloor,tLine)

  # this is the time for each alternative ('occurence level')
  # #[bit,~,cit]=unique([numData, tiDataFloor],'rows','Stable')
  temp <- cbind(numData, tiDataFloor)
  bit <- unique(cbind(numData, tiDataFloor))
  cit <- match(paste(temp[, 1], temp[, 2], sep = "*"), paste(bit[, 1], bit[, 2], sep = "*"))

  # dummyChoiceOut=get(dp2V4,'String') no dummy variable

  newCatch <- data.frame(matrix(NA, nrow = length(bi), ncol = length(unique(B[, 2]))))
  colnames(newCatch) <- names(table(B[, 2]))

  for (w in 1:length(bi)) {
    # if ~isinf(B(C(w),end))
    col <- B[C[w], 2]
    # the following is the output that is NROWS by number of alternatives
    newCatch[which(cit == cit[w]), col] <-  meanCatch[which(rownames(meanCatchSimple)==paste0(B[C[w],2], B[C[w],1])), 
                                                      which(sub("^[^.]*.","", colnames(meanCatch))==tiDataFloor[w])]
  }

  if (is_empty(empty.expectation)) {
    newCatch[is.na(newCatch)] <- 0.0001
  } else if (empty.expectation == 0.0001) {
    newCatch[is.na(newCatch)] <- 0.0001
    newCatch[newCatch == 0] <- 0.0001
  } else if (empty.expectation == 0) {
    # (case 'replace with 0'
    newCatch[is.na(newCatch)] <- 0
  } else {
    # case 'no replacement'
    newCatch[is.na(newCatch)] <- 0.0001
  }


  if (dummy.exp == TRUE) {
    dv <- as.data.frame(matrix(1, dim(meanCatch)[1], dim(meanCatch)[2])) # length(ones(size(meanCatch))
    # dv(~emptyCellsCatch)=1% non empty=1 deprecated
    dv[is.na(dummyTrack), ] <- 0 #
    # Mis.Dum, takes a value of one whenever the expected location- and haul-specific
    # estimates of revenues and bycatch are not empty NOTE: currently doesn't include catch==0
    dummyV <- as.data.frame(matrix(0, dim(newCatch)[1], dim(newCatch)[2])) # zeros(size(newCatch))
    for (w in 1:length(bi)) {
      # if ~isinf(B(C(w),end))
      col <- B[C[w], 2] # col=find(Alt.zoneRow==B(C(w),end))
      dummyV[which(cit == cit[w]), col] <- dv[C[w], bi[w]]
      # the following is the output that is NROWS by number of alternatives
    }

    newDumV <- list(
      matrix = dummyV,
      scale = 1,
      # name = name,
      units = "T/F"
      # file = []
    )


    # replaceEmptyExpAll=get(dp2V5,'String')# replace empty catch
    if (is_empty(empty.expectation)) {
      newCatch[is.na(newCatch)] <- 0.0001
    } else if (empty.expectation == 0.0001) {
      newCatch[is.na(newCatch)] <- 0.0001
      newCatch[newCatch == 0] <- 0.0001
    } else if (empty.expectation == 0) { # (case 'replace with 0'
      newCatch[is.na(newCatch)] <- 0 # c(NULL, 0.0001, 0)  #switch replaceEmptyExpAll{get(dp2V5,'Value')}
    } else { # case 'no replacement'
      newCatch[is.na(newCatch)] <- 0.0001
    }
  } else {
    newDumV <- NULL
  } # end dummy.exp==TRUE


  # end of time vs not time
  r <- nchar(sub("\\.[0-9]+", "", mean(as.matrix(newCatch), na.rm = T))) # regexp(num2str(nanmax(nanmax(newCatch))),'\.','split')
  sscale <- 10^(r - 1)

  ShortExpectedCatch <- list(newCatch, newDumV)

  # newGridVar.file=[]

  return(ShortExpectedCatch)
}
