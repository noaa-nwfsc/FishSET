# Create variables or matrix.

##--- CPUE ----##
#' Create catch per unit effort 
cpue <- function(dat, xWeight, xTime, name='cpue') {
  #' @param dat Main data frame over which to apply function. Table in FishSET database should contain the string `MainDataTable`.
  #' @param xWeight Weight variable
  #' @param xTime Time variable. Must be weeks, days, hours, or minutes.
  #' @param name Name of created variable. Used in the logging function to reproduce work flow. Defaults to name of the function if not defined.
  #' @export cpue 
  #' @details Creates the catch per unit effort variable. Catch variable must be in weight (lbs, mts). Effort variable should be a measurement of duration in time.
  #' @examples 
  #' \dontrun{
  #' pcodMainDataTable$cpue <- cpue('pcodMainDataTable', 'OFFICIAL_TOTAL_CATCH_MT', 'DURATION_IN_MIN') 
  #' }  

  #Call in datasets
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset
  tmp <- 0
 
         if(!is.numeric(dataset[[xTime]])|!is.numeric(dataset[[xWeight]])){
           tmp = 1
          warning('Data must be numeric. CPUE not calculated')
         }
  
  if(tmp == 0){
  # Check that Weight variable is indeed a weight variable
       if (!grepl("Duration", xTime, ignore.case = TRUE)) {
        warning("xTime should be a measurement of time. Use the create_duration function. CPUE calculated.")
      } 
      if (!grepl("LB|Pounds|MT", xWeight, ignore.case = TRUE)){
        warning("xWeight must a measurement of mass. CPUE calculated.")
      }
  
  cpue <- dataset[[xWeight]]/dataset[[xTime]]
 
  create_var_cpue_function <- list()
  create_var_cpue_function$functionID <- 'cpue'
  create_var_cpue_function$args <- c(dat, xWeight, xTime)
  create_var_cpue_function$kwargs <- list()
  create_var_cpue_function$output <- paste0(deparse(substitute(dat)),'$',name)

  log_call(create_var_cpue_function)
  return(cpue)
  }
}

##---- Dummy  Variables ----##
#dummy_num
dummy_num <- function(dat, var, value, opts='more_less', name='dummy_num'){
#' Create a dummy variable based on selected values
#' @param dat Main data frame over which to apply function. Table in FishSET database should contain the string `MainDataTable`.
#' @param var Variable in data frame to create dummy variable off of
#' @param value The set value will depend whether the data is a date, factor, or numeric. If date, value should be a year, if factor, value should be a level
#' within the variable, if number value should be a single number or range of numbers [use c(1,5)]
#' @param opts Choices are x_y and more_less. x_y sets the selected year, factor, or numeric value (single or range) to 0 and all other values to 1.
#' more_less sets sets values before the set year or numberic value to 0 and all values greater than the year or value to 1. 
#' Default is set to 'more_less'.
#' @param name Name of created dummy variable. Used in the logging function to reproduce work flow. Defaults to name of the function if not defined.
#' @details This function creates a dummy variable. How the dummy variable is created depends upon whether the variable the dummy variable should be created
#' from is a date, factor, or numeric variable. 
#' For date variables, the dummy variable is defined by a date (year) and may be either this year versus all other years (x_y) or before vs after this year (more_less).
#' Use this function to create a variable defining whether a not policy action had been implemented. 
#' For example, to create an ammendment 80 variable you would type: `dummy_num('MainDataTable', 'Haul_date', 2008, 'more_less', 'ammend80')` 
#' For factor variables, the only option is to compare selected levels against all others.
#' For example, to set a variable specifying whether fishers targeted pollock or something else type: `dummy_num('MainDataTable', 'GF_TARGET_FT', c('Pollock - bottom', 'Pollock - midwater'), 'x_y', 'pollock_target')`
#' For numeric variables, you can set a single value or a range of continuous values and contrast either the selected value(s) against all others (x_y) or less than the 
#' selected value versus more than the selected value (more_less). For more_less, the mean is used as the critical value is a range of values is proviced.
#' @export

 
   #Pull in data
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset
  
  if(grepl('dat|year', var, ignore.case=TRUE)){
        if(length(value==6)){
          dataset[[var]] <- format(as.Date(dataset[[var]]), "%Y%m")
        } else if(length(value==4)){
          dataset[[var]] <- format(as.Date(dataset[[var]]), "%Y")
        } else {
          dataset[[var]] <- format(as.Date(dataset[[var]]), "%m")  
        }
        if(opts=='x_y'){
          out <- ifelse(as.numeric(dataset[[var]])== value, 0, 1)
          } else {
          out <- ifelse(as.numeric(dataset[[var]])< value, 0, 1)
          }
  } else if(is.numeric(dataset[[var]])) {
        if(opts=='x_y'){
          out <- ifelse(dataset[[var]]>= min(value) & dataset[[var]] <= max(value), 0, 1)
        } else {
          out <- ifelse(dataset[[var]]< mean(value), 0, 1)
        }
  } else if(is.factor(dataset[[var]])|is.character(dataset[[var]])) {
        out <- ifelse(trimws(dataset[[var]], 'both') == trimws(value, 'both'), 0, 1)
  } else { 
    (warning('variable is not recognized as being a date, factor, or numeric. Function not run.'))
  }
  
  create_var_dummy_num_function <- list()
  create_var_dummy_num_function$functionID <- 'dummy_num'
  create_var_dummy_num_function$args <- c(dat, var, value, opts)
  create_var_dummy_num_function$kwargs <- list()
  create_var_dummy_num_function$output <- paste0(deparse(substitute(dat)),'$',name)
  
  log_call(create_var_dummy_num_function)
  return(out)
}

#' Create new dummy variable
dummy_var <- function(dat, DumFill = 'TRUE', name='dummy_var') {
  #' @param dat Main data frame over which to apply function. Table in FishSET database should contain the string `MainDataTable`.
  #' @param DumFill Fill the dummy variable with TRUE or FALSE
  #' @param name Name of created dummy variable. Used in the logging function to reproduce work flow. Defaults to name of the function if not defined.
  #' @export dummy_var
  #' @details Creates a dummy variable of either FALSE or TRUE with length of the number of rows of the data set. 
  #' @examples 
  #' \dontrun{
  #' MainDataTable$dummyvar <- dummy_var(MainDataTable, DumFill=TRUE)
  #' }

  #Pull in data
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset
  
  dummyvar <- as.vector(rep(DumFill, nrow(dataset)))

  create_var_dummy_var_function <- list()
  create_var_dummy_var_function$functionID <- 'dummy_var'
  create_var_dummy_var_function$args <- c(dat, DumFill)
  create_var_dummy_var_function$kwargs <- list()
  create_var_dummy_var_function$output <- paste0(deparse(substitute(dat)),'$',name)
 
  log_call(create_var_dummy_var_function)
  return(dummyvar)
}

#' Create dummy matrix from a coded ID variable
dummy_matrix <- function(dat, x) {
  #' @param dat Main data frame over which to apply function. Table in FishSET database should contain the string `MainDataTable`.
  #' @param x Variable in dataset used to generate dummy matrix
  #' @export dummy_matrix
  #' @details Creates a dummy matrix of TRUE/FALSE with dimensions \emph{(number of observations in dataset) x (number of factors in x)} where each column is a unique factor level. Values are TRUE if the value in the column matches the column factor level and FALSE otherwise.
  #' @examples 
  #' \dontrun{
  #' PortMatrix <- dummy_matrix(MainDataTable, 'PORT_CODE')
  #'}
   
  
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset
  
    # create the matrix
  factor.levels <- levels(as.factor(dataset[[x]]))
  int <- data.frame(matrix(rep(dataset[[x]], length(factor.levels)), ncol = length(factor.levels)))
  colnames(int) = factor.levels
  # change matrix to TRUE/FALSE
  int <- data.frame(lapply(1:length(factor.levels), function(x) ifelse(int[, x] == colnames(int)[x], TRUE, FALSE)))
  colnames(int) = paste(x, "_", levels(as.factor(dataset[[x]])))
  
  create_var_dummy_matrix_function <- list()
  create_var_dummy_matrix_function$functionID <- 'dummy_matrix'
  create_var_dummy_matrix_function$args <- c(dat, x)
  create_var_dummy_matrix_function$kwargs <- list()
  create_var_dummy_matrix_function$output <- c('')
  log_call(create_var_dummy_matrix_function)
  return(int)
}

##---- Coded variables ----##
#' Create quantile variable
set_quants <- function(dat, x, quant.cat = c(0.2, 0.25, 0.4), name='set_quants') {
  #' @param dat Main data frame over which to apply function. Table in FishSET database should contain the string `MainDataTable`.
  #' @param x Variable to transform into quantiles
  #' @param quant.cat Quantile categories. Includes 0.2, 0.25, and 0.4.
  #' @param name Name of created vector. Used in the logging function to reproduce work flow. Defaults to name of the function if not defined.
  #' @export set_quants
  #' @details Creates a coded variable of 5-6 levels based on the quantiles of x. 
  #' Quantile options are: 
  #' \itemize{
  #'   \item{.2:  (0\%, 20\%, 40\%, 60\%, 80\%, 100\%)}
  #'   \item{.25: (0\%, 25\%, 50\%, 75\%, 100\%)}
  #'   \item{.4:  (0\%, 10\%, 50\%, 90\%, 100\%)}
  #'   }
  #' @examples 
  #' \dontrun{
  #' MainDataTable$haul.quant <- set_quants(MainDataTable, 'HAUL', quant.cat=.2)
  #' }
  #
  
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset
  tmp <- 0
  
  if(!is.numeric(dataset[[x]])){
    tmp <- 1
    warning('Variable must be numeric. Function not run.')
  }
  
if(tmp == 0){  
if (quant.cat == 0.2) {
    prob.def = c(0, 0.2, 0.4, 0.6, 0.8, 1)
  } else if (quant.cat == 0.25) {
    prob.def = c(0, 0.25, 0.5, 0.75, 1)
  } else if (quant.cat == 0.4) {
    prob.def = c(0, 0.1, 0.5, 0.9, 1)
  }
  #var.name <- paste("TRIP_OTC_MT", "quantile", sep = ".")
  var.name <- as.integer(cut(dataset[[x]], quantile(dataset[[x]], probs = prob.def), 
                             include.lowest = TRUE))
  
  create_var_set_quants_function <- list()
  create_var_set_quants_function$functionID <- 'set_quants'
  create_var_set_quants_function$args <- c(dat, x, quant.cat)
  create_var_set_quants_function$kwargs <- list()
  create_var_set_quants_function$output <- paste0(deparse(substitute(dat)),'$',name)
 
  log_call(create_var_set_quants_function)
  return(var.name)
}
}

##---- Numeric  Variables ----##
#' Create numeric variables using arithmetic expression
create_var_num <- function(dat, x, y, method, name='create_var_num') {
  #' @param dat Main data frame over which to apply function. Table in FishSET database should contain the string `MainDataTable`.
  #' @param x variable  Variable will be the numerator if `method` is division. 
  #' @param y variable  Variable will be the denominator if `method` is division.
  #' @param method Arithmetic expression. Options include: sum, addition, subtraction, multiplication, and division.
  #' @param name Name of created vector. Used in the logging function to reproduce work flow. Defaults to name of the function if not defined.
  #' @export create_var_num
  #' @details Creates a new numeric variable based on defined arithmetic expression `method`. New variable is added to the data set.
  #' @examples 
  #' \dontrun{
  #' MainDataTable$tot_salmon <- create_var_num(MainDataTable, 'HAUL_CHINOOK', 'HAUL_CHUM',
  #'                                             'sum',' TimeChange')
  #' }
  
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset
  tmp <- 0
  
  if (is.numeric(dataset[[x]]) == FALSE | is.numeric(dataset[[y]]) == FALSE) {
    tmp <- 1
    warning("Variables must be numeric")
   }
  
  if(tmp==0){
  if (grepl("add|sum", method, ignore.case = TRUE)) {
    name <- dataset[[x]] + dataset[[y]]
  } else if (grepl("sub", method, ignore.case = TRUE)) {
    name <- dataset[[x]] - dataset[[y]]
  } else if (grepl("mult", method, ignore.case = TRUE)) {
    name <- dataset[[x]] * dataset[[y]]
  } else if (grepl("div", method, ignore.case = TRUE)) {
    name <- dataset[[x]]/dataset[[y]]
  }
  
  create_var_num_function <- list()
   create_var_num_function$functionID <- 'create_var_num'
   create_var_num_function$args <- c(dat, x, y, method)
   create_var_num_function$kwargs <- list()
   create_var_num_function$output <- paste0(deparse(substitute(dat)),'$',name)
   log_call(create_var_num_function)

  return(name)
  }
}

##---- Spatial  Variables ----##
#' Calculate latitude and longitude of haul midpoint 
create_mid_haul <- function(dat, start=c('lon', 'lat'), end=c('lon','lat'), name='mid_haul') {
#' @param dat Main data frame over which to apply function. Table in FishSET database should contain the string `MainDataTable`.
#' @param start Starting location of haul. Must be specificied as vector containing longitudinal data and vector containing latitudinal data separated by comma.
#' @param end Ending location of haul. Must be specificied as vector containing longitudinal data and vector containing latitudinal data separated by comma.
#' @param name Name of new variable. Defaults to `mid_haul`
#' @details Returns midpoint of each haul. Each row of data must be a unique haul. Requires a start and end point for each observations.
#' @return Main data frame with two new variables added, lat and lon of midpoint.
#' @importFrom geosphere distGeo midPoint
#' @export
#' @examples 
#' \dontrun{
#' MainDataTable <- create_mid_haul(MainDataTable, start = c("LonLat_START_LON", 
#'                 "LonLat_START_LAT"), end = c("LonLat_END_LON", "LonLat_END_LAT"),
#'                  name='mid_haul') 
#' }
#
  
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset
  tmp <- 0
  
 if(is_empty(start)||is_empty(end)) {
   tmp <- 1
   warning('Starting and end locations must both be specified. Function not run.')
 }

  if(dim(dataset[[start]])[1] != dim(dataset[[end]])[1]){
    tmp <- 1
    warning('Starting and ending locations are of different lengths. Function not run.')
  }

  if (any(abs(dataset[[start]][1]) > 180)|any(abs(dataset[[end]][1]) > 180)) {
    warning("Longitude is not valid (outside -180:180). Function not run")
    #stop("Longitude is not valid (outside -180:180.")
    tmp <- 1
  }
  if (any(abs(dataset[[start]][2]) > 90)|any(abs(dataset[[end]][2]) > 90)) {
    warning("Latitude is not valid (outside -90:90. Function not run") 
    tmp <-1    
    # stop("Latitude is not valid (outside -90:90.")
  } 
  
  if(tmp==0){
  distBetween <- geosphere::midPoint(dataset[[start]], dataset[[end]])
  colnames(distBetween)= c(paste0(name,'lon'), paste0(name,'lat'))
  out <- cbind(dataset, distBetween)
  
  create_mid_haul_function <- list()
  create_mid_haul_function$functionID <- 'create_mid_haul'
  create_mid_haul_function$args <- c(dat, start, end)
  create_mid_haul_function$kwargs <- list()
  create_mid_haul_function$output <- paste0(dat,'$',name)
  log_call(create_mid_haul_function)
  
  return(out)
  }
}

create_trip_centroid <- function(dat, lon, lat, weight.var=NULL, ...) {
  ##----trip centroid-----#
  #' Calculate centroid of each trip 
  #' @param dat Main data frame over which to apply function. Table in FishSET database should contain the string `MainDataTable`.
  #' @param lat Vector containing latitudinal data.
  #' @param lon Vector containing longitudinal data.
  #' @param weight.var Variable for weighted average.
  #' @param ... Column(s) that identify the individual trip.
  #' @details Returns centroid of each trip
  #' @importFrom geosphere distGeo midPoint
  #' @export 
  #' @examples 
  #' \dontrun{
  #' pollockMainDataTable <- create_trip_centroid('pollockMainDataTable', 
  #'                               'LonLat_START_LON', 'LonLat_START_LAT', 
  #'                               weight.var=NULL, 'DISEMBARKED_PORT','EMBARKED_PORT')
  #' }
  
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset
  
  x <- 0
  if (any(abs(dataset[[lon]]) > 180)) {
    warning("Longitude is not valid (outside -180:180). Function not run")
    #stop("Longitude is not valid (outside -180:180.")
    x <- 1
  }
  if (any(abs(dataset[[lat]]) > 90)) {
    warning("Latitude is not valid (outside -90:90. Function not run") 
    x <-1    
    # stop("Latitude is not valid (outside -90:90.")
  } 
  
  if(x==1){
  if(grepl('input', as.character(match.call(expand.dots = FALSE)$...)[1])==TRUE){
    argList <- eval(...) } else {
      argList <- (as.character(match.call(expand.dots = FALSE)$...))
    }
  
  idmaker = function(vec) {
    return(paste(sort(vec), collapse = ""))
  }
  int <- as.data.frame(cbind(dataset, rowID = as.numeric(factor(apply(as.matrix(dataset[, eval(substitute(argList))]), 1, idmaker)))))
  #int <- int[, c(colnames(sapply(dataindex[[varnameindex]], grepl, colnames(int))), "rowID")]
  cat(length(unique(int$rowID)), 'unique trips were identified using', argList, '\n')
  # Handling of empty variables
  if (any(apply(int, 2, function(x) all(is.na(x))) == TRUE)) {
    int <- int[, -which(apply(int, 2, function(x) all(is.na(x))) == TRUE)]
  } else {
    int <- int
  }
  
  if (is_empty(weight.var)) {
    int$cent.lon <- stats::ave(int[[lon]], int[['rowID']])
    int$cent.lat <- stats::ave(int[[lat]], int[['rowID']])
  } else {
    # weighted centroid
    int$cent.lon <- stats::ave(int[c(lon, weight.var)], int[['rowID']], 
                               FUN = function(x) stats::weighted.mean(x[[lon]], x[[weight.var]]))[[1]]
    int$cent.lat <- stats::ave(int[c(lat, weight.var)], int[['rowID']], 
                               FUN = function(x) stats::weighted.mean(x[[lat]], x[[weight.var]]))[[1]]
  }
  
  create_trip_centroid_function <- list()
  create_trip_centroid_function$functionID <- 'create_trip_centroid'
  create_trip_centroid_function$args <- c(dat, dat, lon, lat, weight.var, argList)
  create_trip_centroid_function$kwargs <- list()
  log_call(create_trip_centroid_function)

  return(int)
  }
}

#' Histogram of latitude and longitude by grouping variable
spatial_hist <- function(dat, group){
#' @param dat Main data frame over which to apply function. Table in FishSET database should contain the string `MainDataTable`.
#' @param group Vector containing grouping categories
#' @import ggplot2
#' @importFrom reshape2 melt
#' @return histogram of latitude and longitude by grouping variable
#' @export
#' @examples 
#' \dontrun{
#' spatial_summary(MainDataTable, 'GEAR_TYPE')
#' }

  requireNamespace("ggplot2")
  
  dat <- dat[, c(grep(group, names(dat)), grep('lon|lat', names(dat), ignore.case=TRUE))]
  melt.dat <- reshape2::melt(dat)
  ggplot(melt.dat, aes(value, group=group, fill=group))+
         geom_histogram(position="identity", alpha=0.5, binwidth=0.25)+ 
          facet_wrap(~ variable, scales = "free")+ scale_color_grey() + scale_fill_grey() + theme_classic()
}

#hot-spot analysis ( getis ord and morans I)

#'spatial summary statistics
spatial_summary <- function(dat, stat.var=c('length','no_unique_obs','perc_total','mean','median','min','max','sum'), 
               variable, gridfile, lon.grid, lat.grid, lon.dat, lat.dat, cat){
  #' @param dat Main data frame containing data on hauls or trips. Table in FishSET database should contain the string `MainDataTable`.
  #' @param stat.var Options are length, no_unique_obs, perc_total, mean, median, min, max, and sum
  #' @param gridfile Spatial data. Shape, json, and csv formats are supported.
  #' @param variable Vector to summarize over date and zone
  #' @param lon.dat Column containing longitude data in main data frame.
  #' @param lat.dat Column containing latitude data in main data frame.
  #' @param lon.grid Column containing longitude data in gridfile.
  #' @param lat.grid lColumn containing latitude data in gridfile.
  #' @param cat  Column in gridfile that identifies the individual areas or zones. If gridfile is class sf, `cat` should be name of list containing information on zones. 
  #' @importFrom graphics par lines plot
  #' @export
  #' @description Returns plot of selected variable by against date and zone.
  #' @details
  #' \tabular{rlll}{
  #' length: \tab Number of observations \cr
  #' no_unique_obs: \tab Number of unique observations \cr 
  #' perc_total: \tab Percent of total observations \cr 
  #' mean: \tab Mean \cr
  #' median: \tab  Median \cr 
  #' min: \tab  Minimum\cr
  #' max: \tab Maximum \cr
  #' sum: \tab Sum \cr
  #' }
  #' 
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset
  
  
  dataset <- assignment_column(dat=dataset,  gridfile=gridfile, hull.polygon = TRUE, lon.grid, lat.grid, 
                    lon.dat, lat.dat, cat, closest.pt = TRUE, epsg=NULL)
  date.var <- grep('date', names(dataset), ignore.case=TRUE)[1]
  var <- grep(variable, names(dataset), ignore.case=TRUE)[1]
  
   if(stat.var=='mean'){ 
     lab <- paste('mean', names(dataset)[var])
   } else if(stat.var=='median'){
     lab <- paste('median', names(dataset)[var])
   } else if(stat.var=='min'){
     lab <- paste('min', names(dataset)[var])
   } else if(stat.var=='max'){
     lab <- paste('max', names(dataset)[var])
   } else if(stat.var=='sum'){
     lab <- paste('sum', names(dataset)[var])
   } else if(stat.var=='length'){
     lab <- paste('No. observations', names(dataset)[var])
   }
  graphics::par(mfrow=c(1,2))
  
  if(stat.var=='no_unique_obs'){
    graphics::plot(aggregate(dataset[[variable]], by=list(dataset[,date.var]), function(x) length(unique(x))), type = "l", lty = 1, ylab=paste('No. unique obs', names(dataset)[var], 'per day'), xlab='datasete')
    graphics::lines(aggregate(dataset[[variable]], by=list(dataset[,date.var]), function(x) length(unique(x))))
    graphics::plot(aggregate(dataset[[variable]], by=list(as.factor(dataset$ZoneID)), function(x) length(unique(x))), type = "l", lty = 1, ylab=paste('No. unique obs', names(dataset)[var]), xlab='Zone')
    lines(aggregate(dataset[[variable]], by=list(as.factor(dataset$ZoneID)), function(x) length(unique(x))))
  } else if(stat.var=='perc_total'){
    plot(aggregate(dataset[[variable]], by=list(dataset[,date.var]), function(x) length((x))/length(dataset[[variable]])*100), type = "l", lty = 1, ylab=paste('Percent total', names(dataset)[var], 'per day'), xlab='datasete')
    lines(aggregate(dataset[[variable]], by=list(dataset[,date.var]), function(x) length((x))/length(dataset[[variable]])*100))
    plot(aggregate(dataset[[variable]], by=list(as.factor(dataset$ZoneID)), function(x) length((x))/length(dataset[[variable]])*100), type = "l", lty = 1, ylab=paste('Percent total', names(dataset)[var]), xlab='Zone')
    lines(aggregate(dataset[[variable]], by=list(as.factor(dataset$ZoneID)), function(x) length((x))/length(dataset[[variable]])*100))
  } else {
    plot(aggregate(dataset[[variable]], by=list(dataset[,date.var]), stat.var), type = "l", lty = 1, ylab=paste(lab, 'per day'), xlab='datasete')
    lines(aggregate(dataset[[variable]], by=list(dataset[,date.var]), stat.var))
    plot(aggregate(dataset[[variable]], by=list(as.factor(dataset$ZoneID)), stat.var), type='l', ylab=lab, xlab='Zone')
    lines(aggregate(dataset[[variable]], by=list(as.factor(dataset$ZoneID)), stat.var))
  }
}

#' Distance between two points
create_dist_between <- function(dat, start, end, units=c('miles','meters','km','midpoint')){
 #' @param dat Main data frame over which to apply function. Table in FishSET database should contain the string `MainDataTable`.
 #' @param start  Starting location. Should be a port, lat/long location, or the centroid of zonal assignment. If port is desired, start should be the vector name containing the port name. Latitude and longitude for the port are extracted from the port table. If a lat, long location is desired then start should be specified as c(name of lon vector, name of lat vector). The order must be lon, lat. If the center point of the fishing zone or area is to be used then start should be 'centroid'.
 #' @param end  Ending location. Should be a port, lat/long location, or the centroid of the fishing zone or area. If port is desired, end should be the vector name containing the port name. Latitude and longitude for the port are extracted from the port table. If a lat, long location is desired then end should be specified as c(name of lat vector, name of lon vector). If the center point of the fishing zone or area is to be used then end should be 'centroid'.
 #' @param units  Unit of measurement for calculated distance between start and ending points. Can be in miles, meters, kilometers, or midpoint location
 #' @export
 #' @importFrom geosphere distGeo midPoint
 #' @description   Creates a vector of distance between two points. The start and end points must be different vectors. If the start or ending points are from a port or the center of a fishing zone or area, then a prompt will appear asking for further parameters to be specified. If the starting or ending points are a port, then latitude and longitude are extracted from the port table stored in the FishSET database.  In this case, PortTable must be specified.  If the starting or ending points are the center of the fishing zone or area, then the assignment_column function will be called to assign each observation to a zone. The find_centroid function will then be called to determine the centroid of each zone. Distance measurements will be between these centroids. 
 #' @details 
  #' \tabular{rlll}{
  #' portTable: \tab Port table from FishSET database. Required if start or end is a port vector. \cr
  #' gridfile: \tab patial data set Can be shape file data frame or list. Required if start or end is centroid. \cr 
  #' lon.dat: \tab Longitude of points from dataset. Required if start or end is centroid. \cr 
  #' lat.dat: \tab Latitude of points from dataset. Required if start or end is centroid. \cr
  #' lon.grid: \tab Longitude of points from gridfile. Required if start or end is centroid. \cr 
  #' lat.grid: \tab Longitude of points from gridfile. Required if start or end is centroid. \cr
  #' cat: \tab Variable defining the individual areas or zones. Required if start or end is centroid. \cr 
  #' }
#' @examples 
#' \dontrun{
#' MainDataTable$DistCentPort <- create_dist_between(MainDataTable,'centroid','EMBARKED_PORT', 
#'                                                  units='miles')
#' MainDataTable$DistLocLock <- create_dist_between(MainDataTable,c('LonLat_START_LON',
#'                   'LonLat_START_LAT'),c('LonLat_END_LON','LonLat_END_LAT'), units='midpoint')
#' MainDataTable$DistPortPort <- create_dist_between(MainDataTable,'DISEMBARKED_PORT',
#'                                      'EMBARKED_PORT', units='meters')
#' }

  # \tabular{AddPromptparams}{

#    head(create_dist_between(dat,'centroid','EMBARKED_PORT', units='miles'))
#   head(create_dist_between(dat,c('LonLat_START_LON','LonLat_START_LAT'),c('LonLat_END_LON','LonLat_END_LAT'), units='midpoint'))
#       head(create_dist_between(dat,'DISEMBARKED_PORT','EMBARKED_PORT', units='meters'))
  
  #Call in datasets
  if(start[1]==end[1]){
    warning('Starting and ending vectors are identical.')
  } else {
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase())
 
  #Call in datasets
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset
  
  if(any(grepl('port', c(start[1],end[1]), ignore.case=TRUE))){
    #  in port table
    fun <- function(){
      readline("What is the table name in fishset_db containing port data?  ")
      #return(PortTable)
    }
    vars <- if(interactive()) fun()
      if(table_exists(gsub("\'|\"","",vars[1]))==FALSE){
        print(DBI::dbListTables(fishset_db))
        stop(paste('PortTable', 'not defined or does not exist. Consider using one of the tables listed above that exist in the database.'))
      } else {
        port.table <- table_view(gsub("\'|\"","",vars[1]))
      }
  }
  DBI::dbDisconnect(fishset_db)
  
    x <- 0
    
  if(any(grepl('centroid', c(start[1],end[1]), ignore.case=TRUE))){
    fun <- function(){
     gridfile <- readline("What is the name of the spatial data set? Can be shape file, data frame, or list?")
     long.grid <- readline("What is the name of the vector containing longitude of points from spatial data set?")
     lat.grid <- readline("What is the name of the vector containing latitude of points from spatial data set?")
     lon.dat <- readline("What is the name of the vector containing longitude of points from data set?")
     lat.dat <-readline("What is the name of the vector containing latitude of points from data set?") 
     cat <- readline("What is the name of the vector defining the individual areas or zones from the spatial data set?")
     out <- c(gridfile, long.grid,lat.grid,lon.dat,lat.dat,cat )
    }
    vars <- if(interactive()) fun()
    
    dataset <- assignment_column(dat=dataset, gridfile=eval(parse(text=vars[1])),hull.polygon=FALSE, lon.grid=gsub('\"|\'',"",vars[2]), lat.grid=gsub('\"|\'',"",vars[3]), 
                                 lon.dat = gsub('\"|\'',"",vars[4]), lat.dat=gsub('\"|\'',"",vars[5]), cat=gsub('\"|\'',"",vars[6]), closest.pt = TRUE)
    int <- find_centroid(dataset, gridfile=eval(parse(text=vars[1])), lon.grid==gsub('\"|\'',"",vars[2]), lat.grid==gsub('\"|\'',"",vars[3]), 
                         lon.dat=gsub('\"|\'',"",vars[4]), lat.dat=gsub('\"|\'',"",vars[5]), cat=gsub('\"|\'',"",vars[6]), weight.var=NULL)
  }
  
  if(grepl('port', start[1], ignore.case=TRUE)){
    start.lat <- as.numeric(sapply(trimws(dataset[[start]]), 
                          function(x) port.table[which(port.table[['Port_Name']] == x), "Port_Lat"]))
    
    start.long <- as.numeric(sapply(trimws(dataset[[start]]), 
                      function(x) port.table[which(port.table[['Port_Name']] == x), "Port_Long"]))
   
  } else if(start[1]=='centroid'){
    start.lat <- as.numeric(sapply(trimws(dataset[['ZoneID']]), 
                                        function(x) int[which(int[['ZoneID']] == x), "cent.lat"]))
    start.long <- as.numeric(sapply(trimws(dataset[['ZoneID']]), 
                                         function(x) int[which(int[['ZoneID']] == x), "cent.lon"]))
    
  } else {
    start.long <- dataset[[start[1]]]
    start.lat <- dataset[[start[2]]]

    if (any(abs(start.long) > 180)) {
      warning("Longitude is not valid (outside -180:180). Function not run")
      x <- 1
    }
    if (any(abs(start.lat) > 90)) {
      warning("Latitude is not valid (outside -90:90. Function not run") 
      x <-1    
    } 
    
  }
  
   
  if(grepl('port', end[1], ignore.case=TRUE)){
    end.lat <- as.numeric(sapply(trimws(dataset[[end]]), 
                                           function(x) port.table[which(port.table[['Port_Name']] == x), "Port_Lat"]))
    end.long <- as.numeric(sapply(trimws(dataset[[end]]), 
                                           function(x) port.table[which(port.table[['Port_Name']] == x), "Port_Long"]))
  } else if(end[1]=='centroid'){
    end.lat <- as.numeric(sapply(trimws(dataset[['ZoneID']]), 
                                        function(x) int[which(int[['ZoneID']] == x), "cent.lat"]))
    end.long <- as.numeric(sapply(trimws(dataset[['ZoneID']]), 
                                         function(x) int[which(int[['ZoneID']] == x), "cent.lon"]))
  } else {
    end.lat <- dataset[[end[2]]]
    end.long <- dataset[[end[1]]]
    if (any(abs(end.long) > 180)) {
      warning("Longitude is not valid (outside -180:180). Function not run")
      x <- 1
    }
    if (any(abs(end.lat) > 90)) {
      warning("Latitude is not valid (outside -90:90. Function not run") 
      x <-1    
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
  create_dist_between_function$kwargs <- list(vars)
  
  log_call(create_dist_between_function)
  return(distBetween)
    }
  }
  }


##---- Temporal  Variables ----##
#' Create duration of time variable
create_duration <- function(dat, start, end, units = c("week", "day", "hour", "minute"), name='create_duration') {
  #' @param dat Main data frame over which to apply function. Table in FishSET database should contain the string `MainDataTable`.
  #' @param start Variable indicating start of time period
  #' @param end Variable indicating end of time period
  #' @param units Unit of time for calculating duration. Must be weeks, days, hours, or minutes.
  #' @param name Name of created vector. Used in the logging function to reproduce work flow. Defaults to name of the function if not defined.
  #' @importFrom lubridate interval as.duration dweeks ddays dhours dminutes
  #' @export create_duration 
  #' @details Calculates the duration of time between two temporal variables based on defined unit. The new variable is added to the dataset. 
  #' @examples 
  #' \dontrun{
  #' MainDataTable$TripDur <- create_duration(MainDataTable, 'TRIP_START', 'TRIP_END',  units='minute')
  #' }
  
  #Call in datasets
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset
  
  
  if (any(grepl("date|min|hour|week|month|TRIP_START|TRIP_END", start, ignore.case = TRUE)) == FALSE) {
    warning("Function is designed for temporal variables")
  }
  if (any(grepl("date|min|hour|week|month|TRIP_START|TRIP_END", end, ignore.case = TRUE)) == FALSE) {
    warning("Function is designed for temporal variables")
  }
  
  elapsed.time <- lubridate::interval(date_parser(dataset[[start]]), date_parser(dataset[[end]]))
  if (units == "week") {
    dur <- lubridate::as.duration(elapsed.time)/lubridate::dweeks(1)
  } else if (units == "day") {
    dur <- lubridate::as.duration(elapsed.time)/lubridate::ddays(1)
  } else if (units == "hour") {
    dur <- lubridate::as.duration(elapsed.time)/lubridate::dhours(1)
  } else if (units == "minute") {
    dur <- lubridate::as.duration(elapsed.time)/lubridate::dminutes(1)
  }
  
  create_var_temp_function <- list()
  create_var_temp_function$functionID <- 'create_duration'
  create_var_temp_function$args <- c(dat, start, end, units)
  create_var_temp_function$kwargs <- list()
  create_var_temp_function$output <- paste0(deparse(substitute(dat)),'$',name)
  log_call(create_var_temp_function)
  
 return(dur)
}
