#' create_seasonal_ID
#' Create season identifier  
#'
#' @param dat Main data frame over which to apply function. Table in fishset_db database should contain the string `MainDataTable`.
#' @param seasonal.dat Name of table in fishset_db containing date of fishery season(s).
#' @param use.location TRUE/FALSE If true, fishery season dates depend on fishery location. Column names containing location must match the two data sets.
#' @param use.geartype TRUE/FALSE If true, fishery season dates depend on gear type. Column name containing gear type must match the two data sets.
#' @param sp.col Column containing species names in seasonaldat. 
#' @param target Name of target species. If `target` is NULL, runs through fisheries in order listed in seasonal.dat
#' @return  The input dataset with variables SeasonID, and seasonID*fishery (`seasonIDChinook`). 
#' @details Uses a table of season dates for fisheries to create season ID variables. 
#' Output is a SeasonID variable and multiple SeasonID*fishery variables.
#' The seasonID variable is a vector where each row is the fishery season based on dates of the observation. 
#' If target fishery `target` is defined then the function returns SeasonID as vector of the target fishery `target` or `Other`.
#'  If `target` is not defined, then, for each row, SeasonID is the first fishery listed in seasonal.dat for which fishery season date encompasses the dates for that row in the main data table. 
#' SeasonID*fishery variables are a TRUE/FALSE seasonID vector for each fishery (labeled by seasonID and fishery) where TRUE indicates the dates for a given row in the main data table fall within the fishery dates for that fishery.
#' @examples 
#' \dontrun{ 
#'  MainDataTable <- create_seasonal_ID(MainDataTable, seasonal, use.location = TRUE,  
#'  use.geartype = TRUE, sp.col = 'SPECIES', target = 'POLLOCK')
#'  }
#'

create_seasonal_ID <- function (dat, seasonal.dat, use.location=c(TRUE,FALSE), use.geartype=c(TRUE,FALSE), sp.col, target=NULL){

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
  if(is.character(seasonal.dat)==TRUE){
    if(is.null(seasonal.dat)==TRUE | table_exists(seasonal.dat)==FALSE){
      print(DBI::dbListTables(fishset_db))
      stop(paste(seasonal.dat, 'not defined or does not exist. Consider using one of the tables listed above that exist in the database.'))
    } else {
      seasonaldat <- table_view(seasonal.dat)
    }
  } else {
    seasonaldat <- seasonal.dat  
  }
  
  DBI::dbDisconnect(fishset_db)
  
  
# Test that location_data match
  if(use.location == TRUE){
  if(!any(match(names(dataset)[which(grepl('area|zon', names(dataset), ignore.case=TRUE))], names(seasonaldat), nomatch=0)>0)){
    stop('Area or zone must be defined and match in both the main dataset and the seasonal dataset. No match found.')
  } else {
    loc.name <-  names(seasonaldat)[
                    match(names(dataset)[which(grepl('area|zon', names(dataset), ignore.case=TRUE))],
                          names(seasonaldat), nomatch=0)[
                                    which(match(names(dataset)[which(grepl('area|zon', names(dataset), ignore.case=TRUE))], 
                                                names(seasonaldat), nomatch=0)>0)
                                    ]
                    ]
  } 
  }
  
  if(use.geartype == TRUE){
    if(!any(match(names(dataset)[which(grepl('gear', names(dataset), ignore.case=TRUE))], names(seasonaldat), nomatch=0)>0)){
      stop('Gear type must be defined and match in both the main dataset and the seasonal dataset. No match found.')
    }  else {
    gear.name <-  names(seasonaldat)[
      match(names(dataset)[which(grepl('gear', names(dataset), ignore.case=TRUE))],
            names(seasonaldat), nomatch=0)[
              which(match(names(dataset)[which(grepl('gear', names(dataset), ignore.case=TRUE))], 
                          names(seasonaldat), nomatch=0)>0)
              ]
      ]
  } 
  }
  
  seasonaldat[,sp.col] <- gsub("(\\s+)|([[:punct:]])", "_", seasonaldat[,sp.col])
  sp.use <- unique(as.vector(seasonaldat[[sp.col]]))
      sp.use[which(grepl("(\\s+)|([[:punct:]])",sp.use)==TRUE)] <- 
        paste(strsplit(sp.use[which(grepl("(\\s+)|([[:punct:]])",sp.use)==TRUE)], "(\\s+)|([[:punct:]])")[[1]][1],'.*',
               strsplit(sp.use[which(grepl("(\\s+)|([[:punct:]])",sp.use)==TRUE)], "(\\s+)|([[:punct:]])")[[1]][2], sep='')
  
  
  dat.temp <- dataset[which(grepl('date', names(dataset), ignore.case=TRUE) == TRUE)]#
  dat.start <- as.data.frame(apply(dat.temp, 1, function(x) FishSET:::find_first(x)))  
  dat.end <- as.data.frame(apply(dat.temp, 1, function(x) FishSET:::find_last(x)))  
  
  rownames(dataset)=make.names(dat.start[,1], unique=TRUE)
  dat.start <- as.Date(sapply(gsub("\\.|[[:digit:]]", "", rownames(dataset)), function(x) dataset[[x, x]]), origin='1970-01-01')
  rownames(dataset)=make.names(dat.end[,1], unique=TRUE)
  dat.end <- as.Date(sapply(gsub("\\.|[[:digit:]]", "", rownames(dataset)), function(x) dataset[[x, x]]), origin='1970-01-01')
  rownames(dataset) = 1:nrow(dataset)
 
  

  #Get names of species that are match seasonal and the dataset
  spp <- unique(gsub(paste0("(",paste(sp.use, collapse="|"),")","(*SKIP)(*FAIL)|."), "", 
                     names(dataset)[which(grepl(paste(sp.use, collapse="|"), colnames(dataset), ignore.case=TRUE)==TRUE)], 
                     perl = TRUE, ignore.case=TRUE))
 
 # for(i in 1:length(spp)){
  #  dataset[[paste0('SeasonID',spp[i])]] <- NA
  #}
  loc <- grep(paste0(unique(gsub(',', '|', seasonaldat[[loc.name]])), collapse="|"),unique(as.character(dataset[[loc.name]])), value=TRUE)

## --- Create season ID for target species --- ##
  dataset[['SeasonID']] <- NA
  if(is.null(target)==FALSE){
    seasonsub <- seasonaldat[which(grepl(target, seasonaldat[[sp.col]], ignore.case=TRUE)==TRUE),]
    if(use.location==FALSE&use.geartype==FALSE){
      seasontemp <- seasonsub
      if(dim(seasontemp)[1] > 1){
        seasontemp <- seasontemp[1,]
        warning('More than one record exists. Only the first record will be used.')
      } 
      if(all(seasontemp[which(grepl('date', names(seasontemp), ignore.case=TRUE) == TRUE)]=='')==TRUE){
          dataset[['SeasonID']] <- NA
        } else {
           dataset[['SeasonID']] <-ifelse((dat.start > FishSET:::date_parser(seasontemp[[FishSET:::find_first(seasontemp)]][1])) %in%  
                                            (dat.end < FishSET:::date_parser(seasontemp[[FishSET:::find_last(seasontemp)]][1]))==TRUE,
                                    target, 'Other')
      }
    } else {
      for(j in 1:length(loc)){
        seasontemp <- seasonsub[which(grepl(as.character(loc[j]), gsub(',', '|', seasonsub[[loc.name]]))==TRUE),]
        
      if(dim(seasontemp)[1] > 1) {
        if(use.geartype==TRUE){
          #If more than row of the species need to do something - use gear type?
          if(grepl('trawl', dataset[[gear.name]][1], ignore.case = TRUE) == TRUE) {
            seasontemp <- seasontemp[which(grepl('trawl', seasontemp[[gear.name]], ignore.case = TRUE) == TRUE),]
          } else if(grepl('seine|gill', dataset[[gear.name]][1], ignore.case = TRUE) == TRUE) {
            seasontemp <- seasontemp[which(grepl('seine|gill', seasontemp[[gear.name]], ignore.case = TRUE) == TRUE),]  
          } else if(grepl('hook|line', dataset[[gear.name]][1], ignore.case = TRUE) == TRUE){
            seasontemp <- seasontemp[which(grepl('hook|line', seasontemp[[gear.name]], ignore.case = TRUE) == TRUE),]  
          } else if(grepl('pot', dataset[[gear.name]][1], ignore.case = TRUE) == TRUE) {
            seasontemp <- seasontemp[which(grepl('pot', seasontemp[[gear.name]], ignore.case = TRUE) == TRUE),]  
          }
        } else {
          seasontemp <- seasontemp[1,]
          warning('More than one record exists. Only the first record will be used.')
        }
      } 
      if(dim(seasontemp[which(grepl('date', names(seasontemp), ignore.case=TRUE) == TRUE)])[1]==0||
         all(seasontemp[which(grepl('date', names(seasontemp), ignore.case=TRUE) == TRUE)]=='')==TRUE){
        dataset[which(dataset[[loc.name]]==loc[j]),'SeasonID'] <- NA
      } else {
        dataset[which(dataset[[loc.name]]==loc[j]),'SeasonID'] <- 
                  ifelse((dat.start[which(dataset[[loc.name]]==loc[j])] > FishSET:::date_parser(seasontemp[[FishSET:::find_first(seasontemp)]][1])) %in%  
                          (dat.end[which(dataset[[loc.name]]==loc[j])] < FishSET:::date_parser(seasontemp[[FishSET:::find_last(seasontemp)]][1]))==TRUE,
                  target, 'Other')
      }
      }
    }
  } else {
  ## -- No target species -- ##
    seasonsub <- subset(seasonaldat, seasonaldat[which(grepl('date', names(seasonaldat), ignore.case=TRUE) == TRUE)[1]]!='' & 
                          seasonaldat[which(grepl('date', names(seasonaldat), ignore.case=TRUE) == TRUE)[2]]!='')
    if(use.location==FALSE&use.geartype==FALSE){
      i <- 1
      while(all(is.na(dataset[['SeasonID']]))==TRUE){
          seasontemp <- seasonsub[i,]
          dataset[which(is.na(dataset[['SeasonID']]==TRUE)), 'SeasonID'] <- 
                    ifelse(((dat.start[which(is.na(dataset[['SeasonID']])==TRUE)] > FishSET:::date_parser(seasontemp[[FishSET:::find_first(seasontemp)]][1])) %in%  
                            (dat.end[which(is.na(dataset[['SeasonID']])==TRUE)] < FishSET:::date_parser(seasontemp[[FishSET:::find_last(seasontemp)]][1]))==TRUE),
               as.character(seasontemp[[sp.col]]), NA)
      i <- i +1
    }
    } else {
      #Add location now
      i <- 1
      j <- 1
      while(all(is.na(dataset[['SeasonID']]))==TRUE){ 
          seasontemp <- seasonsub[i,]
            seasontemp <- seasontemp[which(grepl(as.character(loc[j]), gsub(',', '|', seasontemp[[loc.name]]))==TRUE),]
            if(all(FishSET:::is_empty(seasontemp))==TRUE){
              next
            }
          if(dim(seasontemp)[1] > 1) {
            if(use.geartype==TRUE){
              #If more than row of the species need to do something - use gear type?
              if(grepl('trawl', dataset[[gear.name]][1], ignore.case = TRUE) == TRUE) {
                seasontemp <- seasontemp[which(grepl('trawl', seasontemp[[gear.name]], ignore.case = TRUE) == TRUE),]
              } else if(grepl('seine|gill', dataset[[gear.name]][1], ignore.case = TRUE) == TRUE) {
                seasontemp <- seasontemp[which(grepl('seine|gill', seasontemp[[gear.name]], ignore.case = TRUE) == TRUE),]  
              } else if(grepl('hook|line', dataset[[gear.name]][1], ignore.case = TRUE) == TRUE){
                seasontemp <- seasontemp[which(grepl('hook|line', seasontemp[[gear.name]], ignore.case = TRUE) == TRUE),]  
              } else if(grepl('pot', dataset[[gear.name]][1], ignore.case = TRUE) == TRUE) {
                seasontemp <- seasontemp[which(grepl('pot', seasontemp[[gear.name]], ignore.case = TRUE) == TRUE),]  
              }
            } else {
              seasontemp <- seasontemp[1,]
              warning('More than one record exists. Only the first record will be used.')
            }
          } 
            
          if(dim(seasontemp[which(grepl('date', names(seasontemp), ignore.case=TRUE) == TRUE)])[1]==0||
             all(seasontemp[which(grepl('date', names(seasontemp), ignore.case=TRUE) == TRUE)]=='')==TRUE){
            dataset[which(is.na(dataset$SeasonID)==TRUE) && which(dataset[[loc.name]]==loc[j]),'SeasonID'] <- NA
          } else {
            dataset[which(is.na(dataset$SeasonID)==TRUE) && which(dataset[[loc.name]]==loc[j]),'SeasonID'] <-
                    ifelse (((dat.start[which(is.na(dataset$SeasonID)==TRUE) && which(dataset[[loc.name]]==loc[j])] > FishSET:::date_parser(seasontemp[[FishSET:::find_first(seasontemp)]][1])) %in%  
                              (dat.end[which(is.na(dataset$SeasonID)==TRUE) && which(dataset[[loc.name]]==loc[j])] < FishSET:::date_parser(seasontemp[[FishSET:::find_last(seasontemp)]][1]))==TRUE),
                     as.character(seasontemp[[sp.col]]), NA)
          }
          
       j <- j +1      
       i <- i +1
       }
    }
  }
 
## --- Create seasonID for all species --- ##
    for(i in 1:length(spp)){
  seasonsub <- seasonaldat[tolower(seasonaldat[[sp.col]]) == tolower(spp)[i],]
  
  if(use.location==FALSE&use.geartype==FALSE){
    if(dim(seasontemp)[1] > 1){
      seasontemp <- seasonsub[1,]
      warning('More than one record exists. Only the first record will be used.')
    } else {
      seasontemp <- seasonsub[1,]
    }
      if(all(seasontemp[which(grepl('date', names(seasontemp), ignore.case=TRUE) == TRUE)]=='')==TRUE){
        dataset[[paste0('SeasonID',spp[i])]] <- FALSE
      } else {
        dataset[[paste0('SeasonID',spp[i])]] <- (dat.start > FishSET:::date_parser(seasontemp[[FishSET:::find_first(seasontemp)]][1])) %in%  
          (dat.end < FishSET:::date_parser(seasontemp[[FishSET:::find_last(seasontemp)]][1]))
    }
  } else if(use.location==FALSE&use.geartype==TRUE){
    if(dim(seasontemp)[1] > 1){
      if(grepl('trawl', dataset[[gear.name]][1], ignore.case = TRUE) == TRUE) {
        seasontemp <- seasontemp[which(grepl('trawl', seasontemp[[gear.name]], ignore.case = TRUE) == TRUE),]
      } else if(grepl('seine|gill', dataset[[gear.name]][1], ignore.case = TRUE) == TRUE) {
        seasontemp <- seasontemp[which(grepl('seine|gill', seasontemp[[gear.name]], ignore.case = TRUE) == TRUE),]  
      } else if(grepl('hook|line', dataset[[gear.name]][1], ignore.case = TRUE) == TRUE){
        seasontemp <- seasontemp[which(grepl('hook|line', seasontemp[[gear.name]], ignore.case = TRUE) == TRUE),]  
      } else if(grepl('pot', dataset[[gear.name]][1], ignore.case = TRUE) == TRUE) {
        seasontemp <- seasontemp[which(grepl('pot', seasontemp[[gear.name]], ignore.case = TRUE) == TRUE),]  
      }
    } else {
      seasontemp <- seasonsub[1,]
    }
    if(all(seasontemp[which(grepl('date', names(seasontemp), ignore.case=TRUE) == TRUE)]=='')==TRUE){
      dataset[[paste0('SeasonID',spp[i])]] <- FALSE
    } else {
      dataset[[paste0('SeasonID',spp[i])]] <- (dat.start > FishSET:::date_parser(seasontemp[[FishSET:::find_first(seasontemp)]][1])) %in%  
        (dat.end < FishSET:::date_parser(seasontemp[[FishSET:::find_last(seasontemp)]][1]))
    }
  }
  
  if(use.location==TRUE){
    for(j in 1:length(loc)){
    seasontemp <- seasonsub[which(grepl(as.character(loc[j]), gsub(',', '|', seasonsub[[loc.name]]))==TRUE),]

  
  if(dim(seasontemp)[1] > 1) {
    if(use.geartype==TRUE){
      #If more than row of the species need to do something - use gear type?
      if(grepl('trawl', dataset[[gear.name]][1], ignore.case = TRUE) == TRUE) {
        seasontemp <- seasontemp[which(grepl('trawl', seasontemp[[gear.name]], ignore.case = TRUE) == TRUE),]
      } else if(grepl('seine|gill', dataset[[gear.name]][1], ignore.case = TRUE) == TRUE) {
        seasontemp <- seasontemp[which(grepl('seine|gill', seasontemp[[gear.name]], ignore.case = TRUE) == TRUE),]  
      } else if(grepl('hook|line', dataset[[gear.name]][1], ignore.case = TRUE) == TRUE){
        seasontemp <- seasontemp[which(grepl('hook|line', seasontemp[[gear.name]], ignore.case = TRUE) == TRUE),]  
      } else if(grepl('pot', dataset[[gear.name]][1], ignore.case = TRUE) == TRUE) {
        seasontemp <- seasontemp[which(grepl('pot', seasontemp[[gear.name]], ignore.case = TRUE) == TRUE),]  
      }
    } else {
      seasontemp <- seasontemp[1,]
      warning('More than one record exists. Only the first record will be used.')
    }
  } 
   if(dim(seasontemp[which(grepl('date', names(seasontemp), ignore.case=TRUE) == TRUE)])[1]==0||
          all(seasontemp[which(grepl('date', names(seasontemp), ignore.case=TRUE) == TRUE)]=='')==TRUE){
     dataset[which(dataset[[loc.name]]==loc[j]),paste0('SeasonID',spp[i])] <- FALSE
   } else {
     dataset[which(dataset[[loc.name]]==loc[j]),paste0('SeasonID',spp[i])] <- 
       (dat.start[which(dataset[[loc.name]]==loc[j])] > FishSET:::date_parser(seasontemp[[FishSET:::find_first(seasontemp)]][1])) %in%  
          (dat.end[which(dataset[[loc.name]]==loc[j])] < FishSET:::date_parser(seasontemp[[FishSET:::find_last(seasontemp)]][1]))
   }
    }
  }
    }


  if(!exists('logbody')) { 
    logbody <- list()
    infoBodyout <- list()
    functionBodyout <- list()
    infobody <- list()
    
    infobody$rundate <- Sys.Date()
    infoBodyout$info <- list(infobody)
    
    functionBodyout$function_calls <- list()
    
    logbody$fishset_run <- list(infoBodyout, functionBodyout)
    
  } 
  create_seaonal_ID_function <- list()
  create_seaonal_ID_function$functionID <- 'create_seaonal_ID'
  create_seaonal_ID_function$args <- c(deparse(substitute(dat)),  deparse(substitute(seasonal.dat)), use.location, use.geartype, sp.col)
  create_seaonal_ID_function$kwargs <- list('target'=target)
  create_seaonal_ID_function$output <- deparse(substitute(dataset))
  functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (create_seaonal_ID_function)
  logbody$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE),paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
  assign("functionBodyout", value = functionBodyout, pos = 1)
  
  return(dataset)
}
