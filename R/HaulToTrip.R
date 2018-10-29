#' Collapse the dataframe from haul to trip.
#' Each row should be a unique trip. Reduces the number of rows.
#'
#'
#' @param dataset dataframe or matrix over which to apply filter
#' @param x column in dataframe over which to filter will be applied
#' @param exp Filter expression. Should take on the form of 'x<100' or is.na(x)==F.
#' @param save.filter Whether to save the filterTable as a csv file
#' @param log.dat Whether to print filterTable to the log file
#' @param use.filter.Table TRUE or FALSE. If true then data is subsetted based on a filter in filterTable and exp is the row containing the filter.
#' @keywords 
#' @method
#' @export
#' @return 
#' @return 
#' @details 

#' @examples
#' MainDataTable and MainDataTableInfo

#1. Identify variables that define the level of unique occurrences in the data set - rowGroup
#2. Define collapse function
#3. Run code



#Couple of issues (like with data data, but THIS WORKS!!!!
trial <- function(dataset,dataindex,varnameindex,genTypeName,... ){
     
     argList <- (as.character(match.call(expand.dots=FALSE)$...))
     
     idmaker = function(vec){
          return(paste(sort(vec), collapse=""))
     }
     int <- as.data.frame(cbind(dataset, 
                                rowID=as.numeric(factor(apply(as.matrix(dataset[, eval(substitute(argList))]), 1, idmaker)))
     ))
     print(is.data.frame(int))
     #MainDataTableInfo[MainDataTableInfo$variable_name == names(int)  , ]
     results <- lapply(1:(ncol(int)-1), function(x)
          if(is.na(as.data.frame(dataindex[dataindex[,varnameindex]==colnames(int)[x], genTypeName]))==TRUE) { 
               aggregate(.~rowID, data.frame(int[c('rowID',colnames(int)[x])]), FUN=head, 1)
          }
          else if(as.data.frame(dataindex[dataindex[,varnameindex]==colnames(int)[x], genTypeName])=='Time') {  
               aggregate(.~rowID, data.frame(int[c('rowID',colnames(int)[x])]), min)
          }
          else if(as.data.frame(dataindex[dataindex[,varnameindex]==colnames(int)[x], genTypeName])=='Other Numeric') {
               aggregate(.~rowID, data.frame(int[c('rowID',colnames(int)[x])]), mean)
          }
          else if(as.data.frame(dataindex[dataindex[,varnameindex]==colnames(int)[x], genTypeName])=='Latitude') {
               aggregate(.~rowID, data.frame(int[c('rowID',colnames(int)[x])]), FUN=head, 1)
          }
          else {
               aggregate(.~rowID, data.frame(int[c('rowID',colnames(int)[x])]), FUN=head, 1)
          }
     )
     results <- as.data.frame(results)
     results <- results[,-grep("rowID.", names(results), value=FALSE)]
     return(results)
}

#CAN DELETE REST



ColObs <- function(dataset){
     #print unique rows for each variable
     int <- data.frame(apply(dataset, 2, function(x) length(unique(x))))
     int$ID <- rownames(int)
     colnames(int)[1]='UniqueObs'
     rownames(int) <- 1:nrow(int)
     int <- int[order(int$UniqueObs, decreasing=TRUE),] 
     int <- subset(int, UniqueObs!=nrow(dataset))
     cat("Variables containing all unique observations or containaing the word 'Haul' have been removed.
         Use this table of unique observations to help identify variables that define the level of unique occurrences.\n")
     print(int[-grep('HAUL',int$ID),])
}

rowID <- function(dataset, ...){
     argList <- (as.character(match.call(expand.dots=FALSE)$...))
 #    print(with(MainDataTable,(paste0(argList))) %>% table())
     idmaker = function(vec){
          return(paste(sort(vec), collapse=""))
     }
     dataset <- cbind(dataset, 
                      rowID=as.numeric(factor(apply(as.matrix(dataset[, eval(substitute(argList))]), 1, idmaker)))
                      )
     print(head(dataset))
}

#1. Creat a function that defines method for reduction
#   Needs to be able to be edited by 'type' and by column
myfun <- function(){
     int <- data.frame(matrix(MainDataTableInfo$generalType, nrow=1, ncol=nrow(MainDataTableInfo)))
     colnames(int)=MainDataTableInfo$variable_name
     int
}


collData <- function(dataset, dataindex, varnameindex, genTypeName, rowID, x){ 
     if(dataindex[dataindex[,varnameindex]==x, genTypeName]=='Time')  {aggregate(dataset[,x], as.list(dataset[,rowID]), min)}
       else if(dataindex[dataindex[,varnameindex]==x, genTypeName]=='Other Numeric')  {aggregate(x~rowID,dataset, mean)}
         else if(dataindex[dataindex[,varnameindex]==x, genTypeName]=='Latitude')  {aggregate(x~rowID,dataset, FUN=head, 1)}
           else if(is.na(dataindex[dataindex[,varnameindex]==x, genTypeName])==TRUE)  {aggregate(x~rowID,dataset, FUN=head, 1)}
             else {aggregate(x~rowID,dataset, FUN=head, 1)}
}

dataset=MainDataTable
dataindex=MainDataTableInfo
varnameindex=variable_name
(x) DATE_FISHING_BEGAN
genTypeName=generalType
rowID=PERMIT
#2. Apply this function is 'apply'
#   Put rowID and myFun into one function?
for (g in twos){
     datETTmp = datET[probes[genes==g],]
     datETOut[g,] = as.numeric(method(datETTmp))
     whichTest    = apply(datETTmp,1,whichTestFn)
     rowsOut[g] = (names(whichTest)[whichTest==max(whichTest)])[1]
     count = count + 1;
     if (count %% 1000 == 0) collectGarbage();
}
