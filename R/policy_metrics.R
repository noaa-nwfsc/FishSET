#' Policy change metrics
#' @param dat  Primary data containing information on hauls or trips.
#'   Table in the FishSET database contains the string 'MainDataTable'.
#' @param project Name of project
#' @param tripID Trip identifier. Can be 'row' or the name or names of variables that define trips. 
#'     If \code{tripID='row'} then each row of the  primary dataset is condsidered to be a unique trip
#' @param vesselID Vessel identifier. Varible name in primary dataset that contains unique vessel identifier.
#' @param catchID Name of variable in primary dataset that contains catch data.
#' @param price Name of variable containing data on revenue or price data.
#' @param datevar Name of variable containing date data. Used to split data into years.
#' @importFrom gt gt tab_spanner
#' @importFrom lubridate year
#' @importFrom dplyr group_by
#' @importFrom yaml read_yaml
#' @export
#' @return  Tables containing basic metrics on effects of proposed zone closures.
#' @details The policy change metrics reflect the impact of proposed policies in the absence of changes in fisher behavior.
#'     Percent of vessels is calculated from the unique vessel identifiers grouped by year and zone. Trips are identified using the 
#'     tripID argument, otherwise each row is assumed to be a trip. If \code{price} is not defined then percent of revenue loss will 
#'     be reported as NA.

policy_metrics <- function(dat, project, tripID='row', vesselID, catchID, datevar=NULL, price=NULL){
#library(tables)

###TO DO
 #1) split by year
   #2) Split by closure scenario
     #3) add additional zones closure
 # https://stackoverflow.com/questions/17560683/create-a-table-in-r-with-header-expanding-on-two-columns-using-xtable-or-any-pac

  # Empty vectors
  closure <- c()
  zone.closure <- c()
  tac <- c()
  temp <- c()

  
  #Helper function
  header.true <- function(df) {
    names(df) <- as.character(unlist(df[1,]))
    df[-1,]
  }
  
### Data function so we don't have so many interim functions in memory
 
  return_dat_int <- function(dataset, zone.closure){
    dataset$closed <- ifelse(dataset$ZoneID %in% zone.closure, 'closed', 'open')
    dataset$closed_expand <- ifelse(dataset$ZoneID %in% zone.closure, zone.closure, 'open')
    dataset <- unique(dataset[, c('year', 'ZoneID', vesselID, 'closed', 'closed_expand')])
    return(dataset)
  }
  
  return_dat <- function(dataset, zone.closure){

      dataset$closed <- ifelse(dataset$ZoneID %in% zone.closure, 'closed', 'open')
      dataset$closed_expand <- ifelse(dataset$ZoneID %in% zone.closure, zone.closure, 'open')
      temp <- dataset[, c('closed', 'ZoneID', 'rowID', 'price', 'year', catchID)]

      #aggregate to trip level if not at trip level already
      temp <- as.data.frame(c(aggregate(temp[,-c(which(colnames(temp)=='ZoneID'), which(colnames(temp)=='closed'))], list(rowID=temp$rowID), FUN = sum)[,-1],
         as.data.frame(aggregate(temp[,c('rowID', 'ZoneID')], list(rowID=temp$rowID), FUN = head)[,3]),
         as.data.frame(aggregate(temp[,c('rowID', 'closed')], list(rowID=temp$rowID), FUN = head)[,3])))

      colnames(temp)[5] <- 'ZoneID'
      colnames(temp)[6] <- 'all_closed'

      temp$closed <- ifelse(temp$all_closed=='open', 'open', temp$ZoneID)
      
      return(temp)
  }
  
###  Metric functions
  ##Function- change in value
    valchange <- function(temp){
      x <- aggregate(temp$price, list(temp$year, temp$closed), function(x)round((sum(x)/sum(temp$price))*100,2))
      x1 <- aggregate(temp$price, list(temp$year, temp$all_closed), function(x)round((sum(x)/sum(temp$price))*100,2))
      #x <- as.data.frame(t(rbind(x,x1)))
      x <- reshape(data=x, idvar='Group.1', timevar='Group.2',v.names='x', direction='wide')
      x1 <- reshape(data=x1, idvar='Group.1', timevar='Group.2',v.names='x', direction='wide')
      percent_loss_revenue <- cbind(x[,-ncol(x)], x1[,-1])
      colnames(percent_loss_revenue) = sub("x.", "", colnames(percent_loss_revenue))
      colnames(percent_loss_revenue)[1] <- 'year'
      percent_loss_revenue <- as.data.frame(percent_loss_revenue, stringsAsFactors=FALSE)
      return(percent_loss_revenue)
    }


  ##Function- % of vessels
    perc_vessels <- function(dataset){
      percent_of_vessels <-  as.data.frame(cbind(round(prop.table(table(dataset$year, dataset$closed_expand))*100,2),
                                                 round(prop.table(table(dataset$year, dataset$closed))*100,2)), stringsAsFactors=FALSE)
       percent_of_vessels <- percent_of_vessels[,-which(colnames(percent_of_vessels)=='open')[1]]
       percent_of_vessels <- data.frame(cbind(rownames(percent_of_vessels), percent_of_vessels), stringsAsFactors=FALSE)
       colnames(percent_of_vessels) = sub("X", "", colnames(percent_of_vessels))
       colnames(percent_of_vessels)[1] <- 'year'
     
      return(percent_of_vessels)
    }

  ##Function - % of trips
    perc_trips <- function(temp){
      percent_of_trips <- as.data.frame(cbind(round(prop.table(table(temp$year, temp$closed))*100,2),
                                              round(prop.table(table(temp$year, temp$all_closed))*100,2)), stringsAsFactors = FALSE)#
       percent_of_trips <- percent_of_trips[,-which(colnames(percent_of_trips)=='open')[1]]
       percent_of_trips <- as.data.frame(cbind(rownames(percent_of_trips), percent_of_trips), stringsAsFactors=FALSE)
       colnames(percent_of_trips) = sub("X", "", colnames(percent_of_trips))
       colnames(percent_of_trips)[1] <- 'year'
     
      #percent_of_trips <- data.frame(as.list(percent_of_trips))
      #colnames(percent_of_trips) = colnames(valchange(temp))
      
      return(percent_of_trips)
    }
                      #round(table(temp$closed)/nrow(temp)*100,2)[1]

  ##Function - loss in catch
    perc_catch <- function(temp){
      x <- aggregate(temp[[catchID]], list(temp$year, temp$closed), function(x)round((sum(x)/sum(temp[[catchID]]))*100,2))
      x1 <- aggregate(temp[[catchID]], list(temp$year, temp$all_closed), function(x)round((sum(x)/sum(temp[[catchID]]))*100,2))
      x <- reshape(data=x, idvar='Group.1', timevar='Group.2',v.names='x', direction='wide')
      x1 <- reshape(data=x1, idvar='Group.1', timevar='Group.2',v.names='x', direction='wide')
      percent_loss_catch <- cbind(x[,-ncol(x)], x1[,-1])
      colnames(percent_loss_catch) = sub("x.", "", colnames(percent_loss_catch))
      colnames(percent_loss_catch)[1] <- 'year'
      percent_loss_catch <- as.data.frame(percent_loss_catch, stringsAsFactors=FALSE)
      return(percent_loss_catch)
    }

    
###Read in data
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main")
  
 
  # create trip identifier
  if(tripID == 'row') {
    dataset$rowID <- 1:nrow(dataset)
  } else if(length(tripID)>1 || length(dataset[[tripID]]) != length(unique(dataset[[tripID]]))){
    dataset <- ID_var(dataset, project = project, vars = tripID, type = "integer", name = "rowID", 
                drop = FALSE, log_fun = FALSE)
  } else {
    dataset$rowID <- dataset[[tripID]]
  }
  
  if(!is.null(datevar)){
    dataset$year <- lubridate::year(dataset[[datevar]])
  } else {
    dataset$year <- 'All years'
  }
  yearlab <- unique(dataset$year)
  
  if(is.null(price)){
    dataset$price <- NA
  } else {
    dataset$price <- dataset[[price]]
  }
  
##Read in zone closure information
  if(utils::file_test("-f",paste0(locoutput(project), pull_output(project, type='zone', fun='closures')))) {
    closures <- yaml::read_yaml(paste0(locoutput(project), pull_output(project, type='zone', fun='closures')))
  } else {
    stop('Closure table not found. Run the zone_closure function.')
  }
  
  scenarioname <- unlist(lapply(X = closures, FUN = `[[`, "scenario"))
  nzone <- unlist(lapply(lapply(X = closures, FUN = `[[`, "zone"), length))+2
###Loop through closures to build the output table
  
for(i in 1:length(closures)){
  
  tac <- as.numeric(closures[[i]]$tac)
  closure[i] <- closures[[i]]$zone
  zone.closure <- stringr::str_remove(closure, "Zone_")

  temp <- return_dat(dataset, zone.closure)

#Create the table

    if(!exists('t1')){
      t1 <-  rbind(perc_vessels(return_dat_int(dataset, zone.closure)), perc_trips(temp), 
             perc_catch(temp), valchange(temp))
    } else {
      t2 <- t1
      t1 <-  rbind(perc_vessels(return_dat_int(dataset, zone.closure)), perc_trips(temp), 
               perc_catch(temp), valchange(temp))
      t1 <- cbind('t2', 't1')
    }
  }
  
if(all(t1$year==1)) t1$year <- 'All years'  
t1$colname <- rep(c("Percent of vessels", 'Percent of trips', 'Percent catch affected', 'Percent revenue affected'),
                  each=length(unique(t1$year)))

  #Plotting
  gt_tab <-  t1 %>% dplyr::group_by(year) %>% gt(rowname_col = "colname") %>%
                      gt::tab_spanner(label=scenarioname[1], columns=1:nzone[1]) 
    
  
  nz <- nzone[1]
  
                    
  if(length(scenarioname) > 1){
    for(i in 2:length(scenarioname)){
      gt_tab <- gt_tab %>% gt::tab_spanner(label=scenarioname[i], columns=(nz+1):(nz+nzone[i]))
      nz <- nzone[i]
    }
  }                
  
  
  policy_metrics_function <- list()
  policy_metrics_function$functionID <- "haul_to_trip"
  policy_metrics_function$args <- list(dat, project, tripID, vesselID, catchID, datevar, price)
  log_call(project, policy_metrics_function)
  
  save_plot(project, "policy_metrics", gt_tab)
  print(gt_tab)
##Function 3 - quartile (share of fleet impacted)
 # 100% (.75-1) effort affected for X % of fleet
  #75% (.5-.75)
  #50% (.25-.5)
  #25% (.025-.25)
  #0% (0-.025)
  
#  x <- table(dataset[[vesselID]], dataset$closed)
#  t2 <- (percent_of_vessels = round((table(cut(x[,1]/rowSums(x), breaks = c(0,.025,.25,.5,.75,1), 
#                                               labels=c('0%','25%','50%','75%','100%'), include.lowest = TRUE))/nrow(x))*100,2))
#  names(dimnames(t2)) <- c("Percent of vessels by percent of trips affected")

##Function 6 - % of value/revenue by vessel

  #Log function call
  
}
