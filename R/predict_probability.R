predict_probability <- function(probLogit, probDataModelIn, probDataModelOut, zoneClose, tacAllowed, zoneClosedFish){
  
  #' Subfunction of \code{\link{model_prediction}} for summing predictions with closures
  #' @param probLogit Parameter estimates
  #' @param probDataModelIn Parameter estimates for closed zones
  #' @param probDataModelOut Parameter estiumates for open zones
  #' @param zoneClose Sum of probabilitys with 0 fishing allowed
  #' @param tacAllowed Decimal (percent) of TAC allowed
  #' @param zoneClosedFish Index of zones that are closed to fishing
  #' @keywords internal
  #' @export
  #' @return List
  #'   probPredict: Prediction by location of fishing
  #'   sumPredctIn: Total probablilty of fishing inside of closers
  #'   sumPredictOut: Total probablitily of fishing outside of closures
  
  # if closure is inside 
  
  if(sum(tacAllowed)==0){
    tryCatch(
      expr = { 
        probPredictIn <- data.frame(zoneID = probDataModelIn[,1], prob = (probDataModelIn[,2]*tacAllowed)) # [probDataModelIn (probDataModelIn*tacAllowed)] 
      },
      error = function(e){
        probPredictIn=c()
      }
    )
    #try
    #probPredictIn=[probDataModelIn[,1] (probDataModelIn[,2] .*tacAllowed)] #if all 0 then make =0 so that we arent dividing by 0
    #catch
    #probPredictIn=[];
    #end
    probPredictOut <- data.frame(zoneID = probDataModelOut[,1], prob = probDataModelOut[,2]/sum(probDataModelOut[,2]))
    
  } else {
    probPredictIn <- cbind(probDataModelIn[,1], (probDataModelIn[,2]*tacAllowed)/(sum(probDataModelIn[,2])-zoneClose))
    probPredictOut <- cbind(probDataModelOut[,1], probDataModelOut[,2]/sum(probDataModelOut[,2])*(1-mean(tacAllowed[tacAllowed>0]))) # FIXME: added mean for now but currently this wouldnt work for individual TAC'sassumes that left over TAC is redistributed% problem with indivdual TAC per zone
  
 }
  
  #probPredictIn((probPredictIn(:,1)==zoneClosedFish),2)=0; % zeros zone
  #for other closures but assumes that its is in the IN component.. this will need to be changed/Not necessary any more
  
  probPredict <- rbind(probPredictIn, probPredictOut)
  probPredict <- probPredict[order(probPredict$zoneID),]
  
  #if isempty(probPredictIn)
  #    sumPredictIn=nan;# may change in future currently passed but not used later
  #else
  
  sumPredictIn=sum(probPredictIn[,2], na.rm=TRUE)
  sumPredictOut=sum(probPredictOut[,2], na.rm=TRUE)
  out <- list(sumPredictIn=sumPredictIn, sumPredictOut=sumPredictOut, probPredict=probPredict)
  return(out)
  
}
