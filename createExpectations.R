#' Expected catch

#' @param name Name of outputed expected catch matrix. Defaults to 'Expected catch'.
#' @param dataset Observe or VMS data
#' @param gridfile gridded data
#' @param AltMatrixName Does not need to specified if ALT has been generated in createAlternativeChoice function
#' @param catch Catch variable for averaging
#' @param defineGroup If empty, data is treated as a fleet
#' @param temporal Daily = Daily time line or sequential = sequential order
#' @param temp.var Temporal variable for averaging
#' @param temp.window Window size for averaging. Defaults to 1
#' @param temp.lag Lag time for averaging
#' @param calc.method select standard average, simple lag = simple lag regression of means, or weights= weights of regressed groups
#' @param lag.method Simple: use region over entire group. Grouped: Use regression for individual time periods
#' @param empty.catch Replace empty catch with NAN, 0, all catch = mean of all catch, group catch = mean of grouped catch
#' @param empty.expectation Do not replace or replace with 0.0001 or 0
#' @param dummy.exp T/F. Defaults to False. If false, no dummy variable is outputted. If true, output dummy variable for originally missing value.
#' @details Used during model creation to create an expectations of catch for alternative choices that are added to the model design file.
#' The expectations created have several options and are created based on the group and time averaging choices of the user.
#' The spatial alternatives are built in to the function and come from the structure Alt.
#' NOTE: currently empty values and values ==nan are considered to be times of no fishing activitiy whereas values in the catch variable choosen ==0
#' are considered fishing activity with no catch and so those are included in the averaging and dummy creation as a point in time when fishing occoured.

#' @returns newGridVar,  newDumV
#

library(lubridate) # to get floor of temporal variables


expectations <- function(name='Expected catch',dataset, gridfile, catch, AltMatrixName, defineGroup=NULL, temporal=c('daily','sequential'), temp.var, temp.window=1,
                         temp.lag=1, calc.method=c('standard average', 'simple lag', 'weights'), lag.method=c('simple','grouped'),
                         empty.catch=c(NULL, 0, 'all catch', 'grouped catch'), empty.expectation=c(NULL, 0.0001, 0), dummy.exp=FALSE){

     if(!exists('Alt')){
          if(!exists(AltMatrixName)) {
               stop('Alternative Choice Matrix does not exist. Please run the createAlternativeChoice() function.')
          }}

     dataZoneTrue <- Alt[['dataZoneTrue']] # used for catch and other variables
     choice <- Alt[['choice']] # used for catch and other variables
     zoneRow <- Alt[['zoneRow']]


###----Done----####
# for now if no time.. only allow mean based on group without options  TODO: allow options from tab 2, currently turned off
#ti=find([data.isTime]);# TODO add optin for other time
if (!any(grepl('DATE|MIN', colnames(dataset)))) {
warning('No time variable found, only averaging in groups and per zone is capable')
}

#Check that define group is either empty of an actual variable in the dataset
if(!is.empty(defineGroup)){
     if(any(is.null(dataset[[defineGroup]]))){
          stop('defineGroup not recognized. Check that parameter is correctly defined')
     }
}


# check whether defining a group or using all fleet averaging
#All fleet averaging
if(is.null(defineGroup)){
# just use an id=ones to get all info as one group
numData <- data.frame(rep(1, dim(dataset)[1]))  #ones(size(data(1).dataColumn,1),1);
#Define by group
} else { #case u1hmP1
numData = as.integer(datasest[[defineGroup]]) # #lmV=cdv(get(listMain,'Value'));# general variable choices from grouping selection #[data(lmV).dataColumn]; # get data based on above
}

numData = as.data.frame(numData)[which(dataZoneTrue==1),] #(Alt.dataZoneTrue,:);
spData = choice[which(dataZoneTrue==1),]# mapping to to the map file
spNAN = which(is.nan(spData)==T);
if(!is.empty(spNAN)){
     spData[spNAN] = Inf; # better for grouping than nans because aggregated
}
numNAN=which(is.nan(numData)==T)
if(!is.empty(numNAN)){
numData[numNAN] = INF
}

#[B,I,C]=unique([numData,spData],'rows');
temp <- cbind(numData, as.character(spData))
B <- unique(temp) # B are actual unique items
# I = something not so importat
C <-match(paste(temp[,1],temp[,2],sep="*"), paste(B[,1],B[,2],sep="*")) #C = row ID of those unique items

catchData <- dataset[[catch]][which(dataZoneTrue==1)]

#Time variable not chosen
if (is.empty(temp.var)) { #if isempty(ti)#NOTE currently doesn't allow dummy or other options if no time detected

allCatch=aggregate(catchData, list(C), mean, na.rm=T) #accumarray(C,catchData,[],@nanmean);# currently no replacement for nans
# Above line is grouping by the alternatives through the C above
#[bi,~,ci]=unique([numData],'rows','Stable');
bi <- unique(numData)
ci <- match(numData, unique(numData))

newCatch=as.data.frame(matrix(NA, nrow=length(C), ncol=length(unique(B[,2])))) #nan(length(C),length(Alt.zoneRow));# preallocating
colnames(newCatch)=names(table(B[,2]))
 # col are alt choices, rows are observations
for (w in 1:length(C)){
      col <- B[C[w],2]
      newCatch[which(ci==ci[w]),col] <- allCatch[C[w],2]
}
#     if (!is.inf(B[C[w],])){
#          col=find(zoneRow==B[C[w],])
#     if( !isempty(col)){
#          newCatch(ci==ci[w],col)=allCatch(C[w],1);# loop shouldn't be necessary but no loop results in out of memory issue
# note this assigns for all the cells of the matrix.
#     }
#          }

# End No temp.var
}
else { #temp.var
tiData <- dataset[[temp.var]][which(dataZoneTrue==1)] #(ti(get(mp3V1,'Value'))).dataColumn(Alt.dataZoneTrue,:); # this part involves time which is more complicated

if(temporal=='daily'){ #case u0 # daily time line
tiDataFloor <- floor_date(as.Date(tiData), unit = 'day') # assume, we are talking day of for time
tLine <- sort(unique(tiDataFloor)) #min(tiDataFloor):max(tiDataFloor)
} else if(temporal==sequential) { #case u1 # observation time line
tiDataFloor <- tiData # just keeping things consistent
tLine <- data.frame(unique(tiData)) #unique(tiData)'; #'
}

###----Done----####
## -------> THIS NEEDS WORK <-------###
allCatch <- vector(mode='list',length(tLine)) #cell(nrow(B),length(tLine))# what if there are multiple catches on the same day, ( mean first or just keep values together somehow)
# for every zone, alt combination, below we group all observations by time.
for (q in 1:nrow(B)){
Bt <- unique(tiDataFloor[which(C==q)])  #[Bt,It,Ct]=unique(tiDataFloor(C==q),'stable')
Ct <- match(tiDataFloor[which(C==q)], Bt)
groupedCatch <- accumarray(Ct,catchData[which(C==q)])#accumarray(Ct,catchData[which(C==q)],[],@(x) {x});  #groupedCatch=accumarray(Ct,catchData(C==q),[],@(x) {x});
ai <- is.element(tLine,tiDataFloor[which(C==q)]) #[ai,~]=ismember(tLine,tiDataFloor(C==q));# for everytime.. yes/no
allCatch[q,which(ai==TRUE)] <- data.frame(groupedCatch)
}

#replace emptys with something -- from second tab

# NOTE, currently this is replaced midstream.. if we want to
# have different ways of handling emptys versus edges of catch
# then would change that here
#FIXME need to add ability to change NANs as well as emptys as
#these are actually different
if(empty.catch=='NULL') {
  replaceValue <- NULL } else if(empty.catch =='0') {
    replaceValue <- 0 } else if(empty.catch == 'all catch') {
      replaceValue <-  mean(catchData, na.rm = T) } else if(empty.catch == 'grouped catch') {
        replaceValue <-  aggregate(catchData, list(C), mean, na.rm=T)
}

emptyCellsCatch <- which(is.empty(allCatch)) #find(cellfun(@isempty,allCatch));# used later for filling
#            allCatch(emptyCells)={nan};%FIXME make options

# Values from spinner menus on first tab
timeRange <- temp.window #get(jhSpinnerRange,'Value');
lagTime <- temp.lag #get(jhSpinnerLag,'Value');
yearRange <- 1 #Not used?  get(jhSpinnerYear,'Value');
#then reshape each row depending on range of window size
#tempRangeMatrix=cell(size(allCatch,2),timeRange);
#tempRangeMatrix=nan(size(allCatch,2),timeRange);
#put catch in place depending on lag/window size
#NOTE FIXME.. may need to change this depending on whether understanding of lag=0 or lag==1...right now if range =4 , lag
#4 means average of first day only as opposed to previous 4 complete without recent day


meanCatchSimple=data.frame(matrix(NA, nrow=nrow(B), ncol=length(tLine))) #nan(size(B,1),size(tLine,2));
dummyTrack <- meanCatchSimple # preallocate for tracking no value
for (q in 1:nrow(B)){
tempRangeMatrix <- vector(mode='list',timeRange) #cell(size(allCatch,2),timeRange);
for (qz in 1:timeRange){
  if (qz<=lagTime) {
    rowStart <- lagTime-qz+1
    rowEnd <- lagTime-qz
    tempRangeMatrix[rowStart:ncol(tempRangeMatrix),qz] <- allCatch[q,1:(ncol(tempRangeMatrix)-rowEnd)] #do I need to transpose second half?
  } else {
    rowStart <- qz+1-lagTime
    rowEnd <- qz-lagTime
    tempRangeMatrix[1:(ncol(tempRangeMatrix)-rowEnd),qz] <- allCatch[q,rowStart:ncol(tempRangeMatrix)] #do I need to transpose second half?
  }
# so through here we've shifted the values according to the
# lags and windows, but have not averaged.
}
# replace emptys with something
emptyCells=find(cellfun(@isempty,tempRangeMatrix));

if (length(replaceValue)>1) {
tempRangeMatrix(emptyCells)={replaceValue(q)}
} else {
tempRangeMatrix(emptyCells)={replaceValue} #FIXME make options
}
# get number of points
numVal=sum(cellfun(@(x) sum(~isnan(x)),tempRangeMatrix),2);# rowSums() sum up number of real values  for each day
numVal[which(numVal==0)] <- nan
tempRangeSumNans=cellfun(@nansum,tempRangeMatrix);
meanCatchSimple[q,] <- rowSums(tempRangeSumNans,na.rm=T)/numVal  #meanCatchSimple(q,:)=nansum(tempRangeSumNans,2)./numVal; # this is the mean catch over the window

#meanCatchSimple(q,:)=nanmean(tempRangeMatrix,2);

dummyTrack[q,] <- numVal # puts a nan when no value is present FIXME only takes into account missing values or nans ( not 0 catch)
} #end for loop


if (calc.method == 'standard average') {
meanCatch=meanCatchSimple
} else if (calc.method == 'simple lag') {
# at this point could use means to get a regression compared to a lag of the same calculation at all zones, then use that to predict...

# need to multiply polys by constant

#switch get(cp2V2,'Value')
  if(lag.method=='simple'){
    polys=dataframe(matrix(NA, nrow=nrow(meanCatchSimple), ncol=2)) #nan(size(meanCatchSimple,1),2);
    for (q in 1:nrow(meanCatchSimple)) {
      polys[q,] <-coef(lm(meanCatchSimple[q,-1] ~ meanCatchSimple[q,1:(ncol(meanCatchSimple)-1)])) # polyfit(meanCatchSimple(q,1:end-1),meanCatchSimple(q,2:end),1);
      meanCatch[q,] <- lm(polys[q,]~meanCatchSimple[q,]) #polyval(polys[q,],meanCatchSimple[q,])
    }
  } else {
#case 2
    polys <- meanCatchSimple[,-1]/meanCatchSimple[,1:(ncol(meanCatchSimple)-1)]
#pad last measurement?with same as end
    polys[,(ncol(polys)+1)] <- polys[,ncol(polys)]
    meanCatch <- meanCatchSimple*polys
  }
} else if(calc.method == 'weights') {
print("do nothing?")
}



#check choice for post calculation dummy issue, this really only affects instances where empty values have been replaced earlier with a number other then 0 or nan
dummyChoice=get(dp2V3,'String');
switch dummyChoice{get(dp2V3,'Value')}#FIXME this only takes into account values that did not exist as opposed to NAN values

case 'no replacement'

case 'replace expectations with nan for originally missing values'
meanCatch(emptyCellsCatch)=nan;
case 'replace expectations with 0 for originally missing values'
meanCatch(emptyCellsCatch)=0;

end


bi <- match(tiDataFloor, tLine, nomatch = 0)# [~,bi]=ismember(tiDataFloor,tLine);   # WORK ON GETTING THIS

# this is the time for each alternative ("occurence level")
# #[bit,~,cit]=unique([numData, tiDataFloor],'rows','Stable');
temp <- cbind(numData, tiDataFloor)
bit <- unique(cbind(numData, tiDataFloor))
cit <- match(paste(temp[,1],temp[,2],sep="*"), paste(bit[,1],bit[,2],sep="*"))


dummyChoiceOut=get(dp2V4,'String');
switch dummyChoiceOut{get(dp2V4,'Value')}#FIXME this only takes into account values that did not exist as opposed to NAN values
#NOTE: need to check that not overestimating empties.. that others rows/col could be filled out like in no time example
if(dummy.exp == FALSE) { #no dummy variable
newCatch=nan(length(bi),length(Alt.zoneRow));
for w=1:length(bi)
if ~isinf(B(C(w),end));
col=find(Alt.zoneRow==B(C(w),end));
# the following is the output that is NROWS by
# number of alternatives
newCatch(cit==cit(w),col)=meanCatch(C(w),bi(w));# loop shouldn't be necessary but no loop results in out of memory issue
end
end
replaceEmptyExpAll=get(dp2V5,'String');# replace empty catch

switch replaceEmptyExpAll{get(dp2V5,'Value')}
case 'replace with 0.0001'
newCatch(isnan(newCatch))=0.0001;
newCatch(newCatch==0)=0.0001;
case 'no replacement'

case 'replace with 0'
newCatch(isnan(newCatch))=0;

} else if(dummy.exp == TRUE){

newCatch=nan(length(bi),length(Alt.zoneRow));
dv=ones(size(meanCatch));
#dv(~emptyCellsCatch)=1;% non empty=1 deprecated
dv(isnan(dummyTrack))=0;   #
#Mis.Dum, takes a value of one whenever the expected location- and haul-specific estimates of revenues and bycatch are not empty
#NOTE: currently doesn't include catch==0
dummyV=zeros(size(newCatch));
for (w=1:length(bi)) {
if ~isinf(B(C(w),end));
col=find(Alt.zoneRow==B(C(w),end));
newCatch(cit==cit(w),col)=meanCatch(C(w),bi(w)) # loop shouldnt be necessary buit no loop results in out of memory issue
dummyV(cit==cit(w),col)=dv(C(w),bi(w));
}
}

newDumV <- list(
  matrix = dummyV,
  scale = 1,
  name = name,
  units = 'T/F',
  file = []
)

replaceEmptyExpAll=get(dp2V5,'String');# replace empty catch

switch replaceEmptyExpAll{get(dp2V5,'Value')}
case 'replace with 0.0001'
newCatch(isnan(newCatch))=0.0001;
case 'no replacement'

case 'replace with 0'
newCatch(isnan(newCatch))=0;


end


end

}# end of time vs not time
r <- nchar(sub('\\.[0-9]+', '', mean(mean(newCatch,na.rm=T),na.rm=T))) #regexp(num2str(nanmax(nanmax(newCatch))),'\.','split');
sscale=10^(r-1)  #FIXME may need to use mean for scaling instead of max



newGridVar <<- list(
     matrix = newCatch,
     scale = sscale,
     name = name,
     units = ifelse(grepl('lbs|pounds',catch, ignore.case = T)==T, 'LBS', 'MTS') #units of catch data
     #newGridVar.file=[];
     )
}


