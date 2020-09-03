

#' Change data class
#'
#' View data class for each variable and call appropriate functions to change data class as needed.
#'
#' @param dat Main data frame over which to apply function. Table in FishSET 
#'   database should contain the string `MainDataTable`.
#' @param project name of project.
#' @param x A character string of variable(s) in \code{dat} that will be changed to \code{newclass}. 
#'   One ore more variables may be included. Default set to NULL.   
#' @param newclass A character string of data classes that \code{x} should be changed to. Length of \code{newclass}
#'   should match the length of \code{x} unless all variables in \code{x} should be the same \code{newclass}.
#'   Defaults to NULL.
#' @param savedat Logical. Should the data table be saved in the FishSET database, replacing the working data table
#'  in the database? Defaults to FALSE.
#' @details Returns a table with data class for each variable. The table is generated after variable classes are 
#'   changed if \code{x} and \code{newclass} are specified. \code{x} and \code{newclass} should be NULL the 
#'   first time the function is run. Changes to the working data should be saved to the FishSET database. 
#'   Set \code{savedat} to TRUE. 
#' @return Table with data class for each variable and the working data with modified data class as specified.
#' @importFrom DBI dbWriteTable dbConnect dbDisconnect
#' @examples
#' \dontrun{
#' #View table without changing class or saving
#' changeclass(pollockMainDataTable, "myproject")
#'
#' #Change class for a single variable and save data table to FishSET database
#' changeclass(pollockMainDataTable, "myproject", x = "HAUL", newclass = numeric, savedat=TRUE)
#' 
#' #Change class for multiple variables and save data table to FishSET database
#' changeclass(pollockMainDataTable, "myproject", x = c("HAUL","DISEMBARKED_PORT"),
#'  newclass = c(numericm factor), savedat=TRUE)
#' }
#' @export changeclass

changeclass <- function(dat, project, x=NULL, newclass=NULL, savedat=FALSE){

# Call in datasets
out <- data_pull(dat)
dat <- out$dat
dataset <- out$dataset


  #change data
    #Conversion is based on starting and ending class
if(!is.null(x)){
  origclass <- sapply(dataset[,x], class)
  origclass <- toupper(origclass)
}
if(!is.null(newclass)){
  newclass <- toupper(newclass)
}
  
  #Change to numeric
    #from character and date
      if(length(which((origclass == 'CHARACTER' | origclass == 'DATE') & (newclass=='NUMERIC')))==1){
          dataset[,names(which((origclass=='CHARACTER'|origclass=='DATE')&(newclass=='NUMERIC')))] <- 
            as.numeric(dataset[,names(which((origclass=='CHARACTER'|origclass=='DATE')&(newclass=='NUMERIC')))])
      }
      if(length(which((origclass == 'CHARACTER' | origclass == 'DATE') & (newclass=='NUMERIC')))>1){
          dataset[,names(which((origclass=='CHARACTER'|origclass=='DATE')&(newclass=='NUMERIC')))] <- 
            apply(dataset[,names(which((origclass=='CHARACTER'|origclass=='DATE')&(newclass=='NUMERIC')))], 2, function(x) as.numeric(x))
      }
    #from factor
      if(length(which((origclass == 'FACTOR')&(newclass=='NUMERIC')))>1){
        dataset[,names(which((origclass=='FACTOR')&(newclass=='NUMERIC')))] <- 
          apply(dataset[,names(which((origclass=='FACTOR')&(newclass=='NUMERIC')))], 2, function(x) as.numeric(as.character(x)))
      }
      if(length(which((origclass == 'FACTOR')&(newclass=='NUMERIC')))==1){
        dataset[,names(which((origclass=='FACTOR')&(newclass=='NUMERIC')))] <- 
          as.numeric(dataset[,names(which((origclass=='FACTOR')&(newclass=='NUMERIC')))])
      }

  #Change to character    
    #numeric
    if(length(which((origclass == 'NUMERIC')&(newclass=='CHARACTER')))>1){
      dataset[,names(which((origclass=='NUMERIC')&(newclass=='CHARACTER')))] <- 
        apply(dataset[,names(which((origclass=='NUMERIC')&(newclass=='CHARACTER')))], 2, function(x) as.character(x))
    }
    if(length(which((origclass == 'NUMERIC')&(newclass=='CHARACTER')))==1){
      dataset[,names(which((origclass=='NUMERIC')&(newclass=='CHARACTER')))] <- 
        as.character(dataset[,names(which((origclass=='NUMERIC')&(newclass=='CHARACTER')))])
    }
    #factor
    if(length(which((origclass == 'FACTOR')&(newclass=='CHARACTER')))>1){
      dataset[,names(which((origclass=='FACTOR')&(newclass=='CHARACTER')))] <- 
        apply(dataset[,names(which((origclass=='FACTOR')&(newclass=='CHARACTER')))], 2, function(x) as.character.factor(x))
    }
    if(length(which((origclass == 'FACTOR')&(newclass=='CHARACTER')))==1){
      dataset[,names(which((origclass=='FACTOR')&(newclass=='CHARACTER')))] <- 
        as.character.factor(dataset[,names(which((origclass=='FACTOR')&(newclass=='CHARACTER')))])
    }
    #date
    if(length(which((origclass == 'DATE')&(newclass=='CHARACTER')))>1){
      dataset[,names(which((origclass=='DATE')&(newclass=='CHARACTER')))] <- 
        apply(dataset[,names(which((origclass=='DATE')&(newclass=='CHARACTER')))], 2, function(x) as.character.Date(x))
    }
    if(length(which((origclass == 'DATE')&(newclass=='CHARACTER')))==1){
      dataset[,names(which((origclass=='DATE')&(newclass=='CHARACTER')))] <- 
        as.character.Date(dataset[,names(which((origclass=='DATE')&(newclass=='CHARACTER')))])
    }

  #Change to factor
    #numeric, character, date
    if(length(which((origclass == 'NUMERIC' | origclass == 'CHARACTER' | origclass == 'DATE') & (newclass=='FACTOR')))==1){
      dataset[,names(which((origclass == 'NUMERIC' | origclass=='CHARACTER'|origclass=='DATE') & (newclass=='FACTOR')))] <- 
        as.factor(dataset[,names(which((origclass == 'NUMERIC' | origclass=='CHARACTER'|origclass=='DATE') & (newclass=='FACTOR')))])
    }
    if(length(which((origclass == 'NUMERIC' | origclass == 'CHARACTER' | origclass == 'DATE') & (newclass=='FACTOR')))>1){
      g <- names(which((origclass=='NUMERIC'| origclass == 'CHARACTER' | origclass == 'DATE') & (newclass=='FACTOR')))
      for(i in 1:length(g)){
        dataset[,g[i]] <- as.factor(dataset[,g[i]])
      }
    }

  #Change to date  
    #numeric  #character #factor
    #as.Date.numeric(x)
    if(length(which((origclass == 'NUMERIC' | origclass == 'CHARACTER' | origclass == 'FACTOR') & (newclass=='DATE')))==1){
      dataset[,names(which((origclass=='NUMERIC'| origclass == 'CHARACTER' | origclass == 'FACTOR') & (newclass=='DATE')))] <- 
        as.POSIXct(dataset[,names(which((origclass=='NUMERIC'| origclass == 'CHARACTER' | origclass == 'FACTOR') & (newclass=='DATE')))])
    }
    if(length(which((origclass == 'NUMERIC'| origclass == 'CHARACTER' | origclass == 'FACTOR') & (newclass=='DATE'))) > 1){
      g <- names(which((origclass=='NUMERIC'| origclass == 'CHARACTER' | origclass == 'FACTOR') & (newclass=='DATE')))
      for(i in 1:length(g)){
      dataset[,g[i]] <- as.POSIXct(dataset[,g[i]])
      }
    }
 

#Print table  
  if(any(newclass == "DATE")){
    temp <- as.data.frame(sapply(dataset, class))[2, ] 
    temp <- droplevels(temp)
    g <- data.frame(cbind(t(temp), t(dataset[1,])))
    colnames(g)=c('Class', 'Value')
    print(g)
  } else {
   g<- as.data.frame(cbind(t(sapply(dataset, class)), t(dataset[1,])))
   colnames(g) = c('Class', 'Value')
    print(g)
  }


  #save data
  if(savedat==TRUE & (is.null(x) & is.null(newclass)) == FALSE){
    suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
    DBI::dbWriteTable(fishset_db, paste0(project, "MainDataTable"), dataset, overwrite = TRUE)
    DBI::dbDisconnect(fishset_db)
  }
  
  #Log the function
  changeclass_function <- list()
  changeclass_function$functionID <- "changeclass"
  changeclass_function$args <- list(dat, project, x, newclass, savedat)
  log_call(changeclass_function)
  
  if((is.null(x) & is.null(newclass)) == FALSE){
  return(dataset)
  }
}
