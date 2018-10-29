#' Collapse the dataframe from haul to trip.
#' Each row should be a unique trip. Reduces the number of rows.
#' Unique trips are defined based on selected column(s). For example, landing permit number and disembared port. This id column is used to collapse the data to trip level.
#'
#'
#' @param dataset dataframe or matrix over which to apply filter
#' @param dataindex Dataframe that contains information on each column of the dataset
#' @param varnameindex The column in dataindex that contains the column names of dataset
#' @param genTypeName column in dataindex containing information on the general category of each column in dataset (time, numeric, etc)
#' @param rowID created in the first half of the HaulToTrip function. It is based on the column(s) identified as defining individual trips
#' @param ... Column(s) that define the individual trip.
#' @keywords 
#' @method
#' @export
#' @return 
#' @return 
#' @details 

#' @examples
#' MainDataTable and MainDataTableInfo
#' dat <- HaultToTrip(MainDataTable, MainDataTableInfo,'variable_name','generaltype','PERMIT','DISEMBARKED_PORT')




#Couple of issues (like with data data, but THIS WORKS!!!!
HaulToTRip <- function(dataset,dataindex,varnameindex,genTypeName,... ){
    # Create a column that indicates unique trip levels 
     argList <- (as.character(match.call(expand.dots=FALSE)$...))
     
     idmaker = function(vec){
          return(paste(sort(vec), collapse=""))
     }
     int <- as.data.frame(cbind(dataset, 
                                rowID=as.numeric(factor(apply(as.matrix(dataset[, eval(substitute(argList))]), 1, idmaker)))
     ))

     #Collapse data based on rowID and defined function
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
