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




expectations <- function(dataset, gridfile, catch, AltMatrixName, defineGroup=NULL, temporal=c('daily','sequential'), temp.var, temp.window=1, 
                         temp.lag=1, calc.method=c('standard average', 'simple lag', 'weights'), 
                         empty.catch=c(NULL, 0, 'all catch', 'grouped catch'), empty.expectation=c(NULL, 0.0001, 0), dummy.exp=FALSE){

     if(!exists('Alt')){
          if(!exists(AltMatrixName)) {
               stop('Alternative Choice Matrix does not exist. Please run the createAlternativeChoice() function.')
          }}
     
     dataZoneTrue <- Alt[['dataZoneTrue']] # used for catch and other variables
     choice <- Alt[['choice']] # used for catch and other variables
     zoneRow <- Alt[['zoneRow']]
     
     
newGridVar <- c()
newDumV <- c() 
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

numData = numData[which(dataZoneTrue==1),] #(Alt.dataZoneTrue,:);
spData = choice[which(dataZoneTrue==1),]; # mapping to to the map file
spNAN = which(is.nan(spData)==T);
if(!is.empty(spNAN)){
     spData[spNAN] = Inf; # better for grouping than nans because aggregated
}
numNAN=which(is.nan(numData)==T)
if(!is.empty(numNAN)){
numData[numNAN] = INF
}
###----Done----####

#[B,I,C]=unique([numData,spData],'rows'); 
temp <- cbind(numData, as.character(spData))
B <- unique(temp) # B are actual unique items
# I = something not so importat  
C <-match(paste(temp[,1],temp[,2],sep="*"), paste(B[,1],B[,2],sep="*")) #C = row ID of those unique items

catchData <- dataset[[catch]][which(dataZoneTrue==1)] # values from pull downs

#Time variable not chosen
if (!any(grepl('DATE|MIN', colnames(dataset)))) { #if isempty(ti)#NOTE currently doesn't allow dummy or other options if no time detected

allCatch=aggregate(catchData, list(C), mean, na.rm=T) #accumarray(C,catchData)#accumarray(C,catchData,[],@nanmean);# currently no replacement for nans
# Above line is grouping by the alternatives through the C above
#[bi,~,ci]=unique([numData],'rows','Stable'); 
bi <- unique(numData)
ci <- match(numData, unique(numData))

newCatch=as.data.frame(matrix(NA, nrow=length(C), ncol=length(zoneRow))) #nan(length(C),length(Alt.zoneRow));# preallocating
# col are alt choices, rows are observations
for (w in 1:length(C)){
     
#     if (!is.inf(B[C[w],])){
#          col=find(zoneRow==B[C[w],])
#     if( !isempty(col)){ 
#          newCatch(ci==ci[w],col)=allCatch(C[w],1);# loop shouldn't be necessary but no loop results in out of memory issue
# note this assigns for all the cells of the matrix.
#     }
#          }
} # end for loop

#newDumV <- c()
# End No    
} else {
     tiData=data(ti(get(mp3V1,'Value'))).dataColumn(Alt.dataZoneTrue,:); # this part involves time which is more complicated


switch get(h,'SelectedObject') # the time versus observation button
case u0 # daily time line
tiDataFloor=floor(tiData);# assume, we are talking day of for time
tLine=nanmin(tiDataFloor):nanmax(tiDataFloor);
case u1 # observation time line
tiDataFloor=tiData;# just keeping things consistent
tLine=unique(tiData)'; #'
end

allCatch=cell(size(B,1),length(tLine));# what if there are multiple catches on the same day, ( mean first or just keep values together soemhow)
# for every zone, alt combination, below we group all
# observations by time.
for q=1:size(B,1);
[Bt,It,Ct]=unique(tiDataFloor(C==q),'stable');#FIXME.. what if time is NAN?
groupedCatch=accumarray(Ct,catchData(C==q),[],@(x) {x});
[ai,~]=ismember(tLine,tiDataFloor(C==q));# for everytime.. yes/no
allCatch(q,ai)=groupedCatch' #'

end

#replace emptys with something -- from second tab

# NOTE, currently this is replaced midstream.. if we want to
# have diffenert ways of handling emptys versus edges of catch
# then would change that here
#FIXME need to add ability to change NANs as well as emptys as
#these are actually different
replV=get(dp2V2,'Value');
switch replV
case 1
replaceValue=nan;
case 2
replaceValue=0;
case 3
replaceValue=nanmean(catchData);
case 4
replaceValue=accumarray(C,catchData,[],@nanmean);
otherwise


end
emptyCellsCatch=find(cellfun(@isempty,allCatch));# used later for filling
#            allCatch(emptyCells)={nan};%FIXME make options

# Values from spinner menus on first tab
timeRange=get(jhSpinnerRange,'Value');
lagTime=get(jhSpinnerLag,'Value');
yearRange=get(jhSpinnerYear,'Value');
#then reshape each row depending on range of window size
#tempRangeMatrix=cell(size(allCatch,2),timeRange);
#tempRangeMatrix=nan(size(allCatch,2),timeRange);
#put catch in place depending on lag/window size
#NOTE FIXME.. may need to change this depending on whether
#understanding of lag=0 or lag==1...right now if range =4 , lag
#4 means average of first day only as opposed to previous 4
#complete without recent day


meanCatchSimple=nan(size(B,1),size(tLine,2));
dummyTrack=meanCatchSimple; # preallocate for tracking no value
for q=1:size(B,1);
tempRangeMatrix=cell(size(allCatch,2),timeRange);
for qz=1:timeRange;
if qz<=lagTime
rowStart=lagTime-qz+1;
rowEnd=lagTime-qz;
tempRangeMatrix(rowStart:end,qz)=allCatch(q,1:end-rowEnd)' #'
else
rowStart=qz+1-lagTime;
rowEnd=qz-lagTime;
tempRangeMatrix(1:end-rowEnd,qz)=allCatch(q,rowStart:end)' #'

end
# so through here we've shifted the values according to the
# lags and windows, but have not averaged. 

end
# replace emptys with soemthing
emptyCells=find(cellfun(@isempty,tempRangeMatrix));

if length(replaceValue)>1
tempRangeMatrix(emptyCells)={replaceValue(q)};
else
tempRangeMatrix(emptyCells)={replaceValue} #FIXME make options
end
# get number of points
numVal=sum(cellfun(@(x) sum(~isnan(x)),tempRangeMatrix),2);# sum up number of real values  for each day
numVal(numVal==0)=nan;
tempRangeSumNans=cellfun(@nansum,tempRangeMatrix);
meanCatchSimple(q,:)=nansum(tempRangeSumNans,2)./numVal; # this is the mean catch over the window 

#meanCatchSimple(q,:)=nanmean(tempRangeMatrix,2);

dummyTrack(q,:)=numVal;# puts a nan when no value is present FIXME only takes into account missing values or nans ( not 0 catch)
end


avgTypesAvail=get(cp2V1,'String');

switch avgTypesAvail{get(cp2V1,'value')}
case 'standard avg'
meanCatch=meanCatchSimple;
case 'simple lag regression of means'
# at this point could use means to get a regrssion compared to a
# lag of the same calculation at all zones, then use that to
# predict...

# need to multiply polys by constant

switch get(cp2V2,'Value')
case 1
polys=nan(size(meanCatchSimple,1),2);
warning('off','MATLAB:polyfit:RepeatedPointsOrRescale')
for q=1:size(meanCatchSimple,1)
polys(q,:) = polyfit(meanCatchSimple(q,1:end-1),meanCatchSimple(q,2:end),1);
meanCatch(q,:)=polyval(polys(q,:),meanCatchSimple(q,:));
end


case 2
polys=meanCatchSimple(:,2:end)./meanCatchSimple(:,1:end-1);
%pad last measurement?with same as end
polys(:,end+1)=polys(:,end);
meanCatch=meanCatchSimple.*polys;

end


case 'weights or regressed groups'

otherwise

end



#check choice for post calulation dummy issue, this really only affect instances where empty values have been replaced earlier with a number other then 0 or nan
dummyChoice=get(dp2V3,'String');
switch dummyChoice{get(dp2V3,'Value')}#FIXME this only takes into account values that did not exist as opposed to NAN values

case 'no replacement'

case 'replace expectations with nan for originally missing values'
meanCatch(emptyCellsCatch)=nan;
case 'replace expectations with 0 for originally missing values'
meanCatch(emptyCellsCatch)=0;

end


[~,bi]=ismember(tiDataFloor,tLine);   # WORK ON GETTING THIS

# this is the time for each alternative ("occurence level")
[bit,~,cit]=unique([numData, tiDataFloor],'rows','Stable');



dummyChoiceOut=get(dp2V4,'String');
switch dummyChoiceOut{get(dp2V4,'Value')}#FIXME this only takes into account values that did not exist as opposed to NAN values
#NOTE: need to check that not overestimating empties.. that
#others rows/col could be filled out lke in no time example
case 'no dummy variable'
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

end
newDumV=[];

case 'ouput a dummy variable for originally missing values'
newCatch=nan(length(bi),length(Alt.zoneRow));
dv=ones(size(meanCatch));
#dv(~emptyCellsCatch)=1;% non empty=1 deprecated
dv(isnan(dummyTrack))=0;   #
#Mis.Dum, takes a value of one whenever the expected location- and haul-specific
#estimates of revenues and bycatch are not empty 
#NOTE: currently doesn't include catch==0
dummyV=zeros(size(newCatch));
for w=1:length(bi)
if ~isinf(B(C(w),end));
col=find(Alt.zoneRow==B(C(w),end));
newCatch(cit==cit(w),col)=meanCatch(C(w),bi(w)) # loop shouldnt be necessary buit no loop results in out of memory issue
dummyV(cit==cit(w),col)=dv(C(w),bi(w));
end
end

newDumV.matrix=dummyV;
newDumV.scale=1;
newDumV.name={['dummy_' get(ED1,'string')]};
newDumV.units='T/F';
newDumV.file=[];

replaceEmptyExpAll=get(dp2V5,'String');# replace empty catch

switch replaceEmptyExpAll{get(dp2V5,'Value')}
case 'replace with 0.0001'
newCatch(isnan(newCatch))=0.0001;
case 'no replacement'

case 'replace with 0'
newCatch(isnan(newCatch))=0;


end


end

}# end of tim vs not time

r=regexp(num2str(nanmax(nanmax(newCatch))),'\.','split');
sscale=10^(length(r{1})-1)  #FIXME may need to use mean for scaling instead of max



newGridVar <- list(
     matrix = newCatch,
     scale = sscale,;
     name = {get(ED1,'string')},
     units  =data(cth(get(cp1V1,'Value'))).units
     #newGridVar.file=[];
     )
}


















function cancelFCN(hObject,event)
newGridVar=[];
uiresume(hfig)
delete(hfig)
end





end
