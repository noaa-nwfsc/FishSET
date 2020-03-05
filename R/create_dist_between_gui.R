#' Distance between two points
create_dist_between_for_gui <- function(dat, start, end, units, portTable=NULL, gridfile=NULL,
                                        lon.dat=NULL, lat.dat=NULL, cat=NULL, lon.grid=NULL, lat.grid=NULL){
  #' @param dat Main data frame over which to apply function. Table in fishset_db database should contain the string `MainDataTable`.
  #' @param start  Starting location. Should be a port, lat/long location, or the centroid of zonal assignment. 
  #' @param end  Ending location. Should be a port, lat/long location, or the centroid of the fishing zone or area. 
  #' @param units Unit of distance (miles, kilometers)
  #' @param portTable Data table containing port data
  #' @param gridfile Grid file
  #' @param lon.dat Variable containing longitude from main dataset
  #' @param lat.dat Variable containing latitude from main dataset
  #' @param lon.grid Variable containing longitude from main grid file
  #' @param lat.grid Variable containing latitude from main grid file
  #' @param cat Variable in grid file designating column containing zones or areas.
  #' @export
  #' @importFrom geosphere distGeo midPoint
  #' @importFrom jsonlite  toJSON
  #' @description   Creates a vector of distance between two points. The start and end points must be different vectors. 
  #' If the start or ending points are from a port or the center of a fishing zone or area, 
  #' then a prompt will appear asking for further parameters to be specified. 
  #' If the starting or ending points are a port, then latitude and longitude are extracted from the port table 
  #' stored in the fishset_db database.  In this case, PortTable must be specified.  If the starting or ending points 
  #' are the center of the fishing zone or area, then the assignment_column function will be called to assign each observation
  #'  to a zone. The find_centroid function will then be called to determine the centroid of each zone. Distance measurements will be between these centroids. 

  
  #Call in datasets
  if(start[1]==end[1]){
    warning('Starting and ending vectors are identical.')
  } else {
    fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase())
    
    #Call in datasets
    out <- data_pull(dat)
    dat <- out$dat
    dataset <- out$dataset
 
       x <- 0      
       
       if(start[1]=='centroid'|end[1]=='centroid'){
        dat2 <- assignment_column(dat=dataset, gridfile=gridfile, hull.polygon=FALSE, lon.grid=lon.grid, lat.grid=lat.grid, 
                                   lon.dat = lon.dat, lat.dat=lat.dat, cat=cat, closest.pt = TRUE)
        int <- find_centroid(dat2, gridfile=gridfile, lon.grid==lon.grid, lat.grid==lat.grid, 
                           lon.dat=lon.dat, lat.dat=lat.dat, cat=cat, weight.var=NULL)
      }
    if((grepl('port', start[1], ignore.case=TRUE)|grepl('port', end[1], ignore.case=TRUE))==TRUE){
       port.table <- table_view(portTable)
      }
  
#  start.long <- c()
 # start.lat <- c()
    
   
    if(grepl('port', start[1], ignore.case=TRUE)){
      start.lat <- as.numeric(sapply(trimws(dataset[[start]]), 
                                     function(x) port.table[which(port.table[['Port_Name']] == x), "Port_Lat"]))
      
      start.long <- as.numeric(sapply(trimws(dataset[[start]]), 
                                      function(x) port.table[which(port.table[['Port_Name']] == x), "Port_Long"]))
    }   
   if(start[1]=='centroid'){
      start.lat <- as.numeric(sapply(trimws(dat2[['ZoneID']]), 
                                     function(x) int[which(int[['ZoneID']] == x), "cent.lat"]))
      start.long <- as.numeric(sapply(trimws(dat2[['ZoneID']]), 
                                      function(x) int[which(int[['ZoneID']] == x), "cent.lon"]))
   }
      
      
  if(grepl('lat|lon', start[1], ignore.case=TRUE)){
        start.long <- dataset[[start[2]]]
        start.lat <- dataset[[start[1]]]
 
        if (any(abs(start.long) > 180)) {
          warning("Longitude is not valid (outside -180:180). Function not run")
          #stop("Longitude is not valid (outside -180:180.")
          x <- 1
        }
        if (any(abs(start.lat) > 90)) {
          warning("Latitude is not valid (outside -90:90. Function not run") 
          x <-1    
          # stop("Latitude is not valid (outside -90:90.")
        } 
        
        
    }
    
    
    if(grepl('port', end[1], ignore.case=TRUE)){
      end.lat <- as.numeric(sapply(trimws(dataset[[end]]), 
                                   function(x) port.table[which(port.table[['Port_Name']] == x), "Port_Lat"]))
      end.long <- as.numeric(sapply(trimws(dataset[[end]]), 
                                    function(x) port.table[which(port.table[['Port_Name']] == x), "Port_Long"]))
    } else if(end[1]=='centroid'){
      end.lat <- as.numeric(sapply(trimws(dat2[['ZoneID']]), 
                                   function(x) int[which(int[['ZoneID']] == x), "cent.lat"]))
      end.long <- as.numeric(sapply(trimws(dat2[['ZoneID']]), 
                                    function(x) int[which(int[['ZoneID']] == x), "cent.lon"]))
    } else {
      end.lat <- dataset[[end[1]]]
      end.long <- dataset[[end[2]]]
      if (any(abs(end.long) > 180)) {
        warning("Longitude is not valid (outside -180:180). Function not run")
        #stop("Longitude is not valid (outside -180:180.")
        x <- 1
      }
      if (any(abs(end.lat) > 90)) {
        warning("Latitude is not valid (outside -90:90. Function not run") 
        x <-1    
        # stop("Latitude is not valid (outside -90:90.")
      } 
    }
    
    if(x==1){
    # Get distance between points
    if(units=='midpoint'){
      distBetween <- geosphere::midPoint(cbind(start.long,start.lat), cbind(end.long,end.lat))
    } else {
      distBetween <- geosphere::distGeo(cbind(start.long,start.lat), 
                                        cbind(end.long,end.lat), a = 6378137, f = 1/298.257223563)
    }
    
    if(units=='miles') {
      distBetween <- distBetween*0.000621371192237334 
    } else if(units=='kilometers'){
      distBetween <- distBetween/1000
    } 
    

    #Log the function     
    create_dist_between_function <- list()
    create_dist_between_function$functionID <- 'create_dist_between'
    create_dist_between_function$args <- c(dat, start, end, units)
    create_dist_between_function$kwargs <- c(portTable, deparse(substitute(gridfile)), lon.dat, lat.dat, cat, lon.grid, lat.grid)

    log_call(create_dist_between_function)
    
    return(distBetween)
    }
  }
}

