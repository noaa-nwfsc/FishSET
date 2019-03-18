#' create_seasonal_ID
#' Connects the variables in a table of season dates to main data table based on two column to the data in the main workspace.
#' A new variable is made with the Season identifier (TRUE/FALSE) in it. 
#'
#' @param dataset Name of table in database containing model measures of fit
#' @param seasonaldat Append model selection to modelChosen table or delete existing table and save new table
#' @param use.location TRUE/FALSE If true, season dates depend on fishery location. Colnames of location must match the two datasets
#' @param use.geartype TRUE/FALSE If true, season dates depend on gear type. Column names must match the two datasets
#' @param target Name of target species. If not null, will return a vector with name of fishery season for each row of dataset. Will first check the dates of the fishery season for target species before checking other species.
#' @param sp.col Column name containing species names in seasonaldat 
#' @return dataset Returns the input dataset with two new variables
# @example 
#  MainDataTable <- create_seasonal_ID(MainDataTable, seasonal, use.location = TRUE, TRUE, target='', all.species=TRUE, sp.col='SPECIES')



create_seasonal_ID <- function (dataset, seasonaldat, use.location=c(TRUE,FALSE), use.geartype=c(TRUE,FALSE), sp.col, target=NULL){

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
  dat.start <- as.data.frame(apply(dat.temp, 1, function(x) find_first(x)))  
  dat.end <- as.data.frame(apply(dat.temp, 1, function(x) find_last(x)))  
  
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
           dataset[['SeasonID']] <-ifelse((dat.start > date_parser(seasontemp[[find_first(seasontemp)]][1])) %in%  
                                            (dat.end < date_parser(seasontemp[[find_last(seasontemp)]][1]))==TRUE,
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
                  ifelse((dat.start[which(dataset[[loc.name]]==loc[j])] > date_parser(seasontemp[[find_first(seasontemp)]][1])) %in%  
                          (dat.end[which(dataset[[loc.name]]==loc[j])] < date_parser(seasontemp[[find_last(seasontemp)]][1]))==TRUE,
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
                    ifelse(((dat.start[which(is.na(dataset[['SeasonID']])==TRUE)] > date_parser(seasontemp[[find_first(seasontemp)]][1])) %in%  
                            (dat.end[which(is.na(dataset[['SeasonID']])==TRUE)] < date_parser(seasontemp[[find_last(seasontemp)]][1]))==TRUE),
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
            if(all(is.empty(seasontemp))==TRUE){
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
                    ifelse (((dat.start[which(is.na(dataset$SeasonID)==TRUE) && which(dataset[[loc.name]]==loc[j])] > date_parser(seasontemp[[find_first(seasontemp)]][1])) %in%  
                              (dat.end[which(is.na(dataset$SeasonID)==TRUE) && which(dataset[[loc.name]]==loc[j])] < date_parser(seasontemp[[find_last(seasontemp)]][1]))==TRUE),
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
        dataset[[paste0('SeasonID',spp[i])]] <- (dat.start > date_parser(seasontemp[[find_first(seasontemp)]][1])) %in%  
          (dat.end < date_parser(seasontemp[[find_last(seasontemp)]][1]))
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
      dataset[[paste0('SeasonID',spp[i])]] <- (dat.start > date_parser(seasontemp[[find_first(seasontemp)]][1])) %in%  
        (dat.end < date_parser(seasontemp[[find_last(seasontemp)]][1]))
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
       (dat.start[which(dataset[[loc.name]]==loc[j])] > date_parser(seasontemp[[find_first(seasontemp)]][1])) %in%  
          (dat.end[which(dataset[[loc.name]]==loc[j])] < date_parser(seasontemp[[find_last(seasontemp)]][1]))
   }
    }
  }
    }

 # write(layout.json.ed(trace, 'create_seaonal_ID', deparse(substitute(dataset)), x='', 
 #                      msg=paste('seasonaldat:', deparse(substitute(seasonaldat)), ', use.location:', use.location, 
 #                                ', use.geartype:', use.geartype, ', target:', target, ', sp.col:', sp.col)), 
 #       paste(getwd(),'/Logs/',Sys.Date(),'.json', sep=''), append=T )

  create_seaonal_ID_function <- list()
  create_seaonal_ID_function$functionID <- 'create_seaonal_ID'
  create_seaonal_ID_function$args <- c(deparse(substitute(dataset)),  deparse(substitute(seasonaldat)), use.location, use.geartype, sp.col)
  create_seaonal_ID_function$kwargs <- list('target'=target)
  create_seaonal_ID_function$output <- deparse(substitute(dataset))
  functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (create_seaonal_ID_function)
  body$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(body, pretty = TRUE, auto_unbox = TRUE),paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
  list2env(functionBodyout, envir = .GlobalEnv)
  
  return(dataset)
}
