#Define source location
locdatabase <- function(){
#' Returns the location of the database
#' @details if loc is not in the working directory, then the default location is use
#' @export
#' @examples 
#' \dontrun{
#' locdatabase() # will return sqlite database location within the fishset package
#' loc <- getwd()
#' locdatabase() #will return sqlite database location as within the working directory
#' }
 if(exists('loc')) { loc=loc} else { loc=NULL}
  if(is.null(loc)){
  paste0(system.file(package='FishSET'), '/fishset_db.sqlite') 
  } else {
    paste0(loc, '/fishset_db.sqlite')
  }
}

loclog <- function(){
  #Define source location for log folder
    #' Returns the location of the log folder
    #' @details if loc2 is not in the working environment, then the default location is use
    #' @export
    #' @examples 
    #' \dontrun{
    #' loclog() # will return log folder location within the fishset package
    #' loc2 <- getwd()
    #' loclog() #will return log folder location as within the working directory
    #' }
    if(exists('loc2')) { loc2=loc2} else { loc2=NULL}
    if(is.null(loc2)){
      paste0(system.file(package='FishSET'), '/Logs/')
      
    } else {
      paste0(loc2, '/Logs/')
    }
  }

locoutput <- function(){
  #Define source location for output folder
  #' Returns the location of the output folder
  #' @details if loc2 is not in the working environment, then the default location is use
  #' @export
  #' @examples 
  #' \dontrun{
  #' locoutput() # will return output folder location within the fishset package
  #' loc2 <- getwd()
  #' locoutput() #will return output folder location as within the working directory
  #' }
  if(exists('loc2')) { loc2=loc2} else { loc2=NULL}
  if(is.null(loc2)){
    paste0(system.file(package='FishSET'), '/output/')
    
  } else {
    paste0(loc2, '/output/')
  }
}

pull_info_data <- function(project){
#' Pull the most recent data index file for given project
#' @param project Name of project, such as pollock
#' @export

g <- tables_database()
g <- g[grep(paste0('Info.*',project,'|',project,'.*Info'), g)]
g <- gsub("[^0-9\\.]", "", g[grep('Info.', g)])[which(gsub("[^0-9\\.]", "", g[grep('Info.', g)]) == max(gsub("[^0-9\\.]", "", g[grep('Info.', g)])))]
paste0(project,'MainDataTableInfo',g)
}

table_format <- function(x) {
#' Import and format saved tables to notebook file
#' @param x Name of table saved in inst/output
#' @export
#' @importFrom pander panderOptions pander 

tab_int <- read.csv(paste0(getwd(),'/inst/output/', x, '.csv'))
pander::panderOptions('table.alignment.default', function(df)
  ifelse(sapply(df, is.numeric), 'right', 'left'))
pander::panderOptions('table.emphasize.rownames',TRUE)
pander::panderOptions('table.split.table', Inf)
pander::panderOptions('graph.fontsize',8)
pander::panderOptions('table.style', 'multiline')
if(grepl('summary', x)){
  colnames(tab_int)[1] <- 'Variable'  
  pander::pander(tab_int)
} else {
  pander::pander(tab_int)
}
}

plot_format <- function(x){
  #' Import and format plots to notebook file
  #' @param x Name of plot saved in inst/output
  #' @importFrom knitr include_graphics
  #' @export
  knitr::include_graphics(paste0(getwd(), '/inst/output/',x,'.png'))
}

vgsub <- function(pattern, replacement, x, ...) {
  #' vgsub function
  #' @param pattern pattern
  #' @param replacement replacement
  #' @param x x
  #' @param ... Additional arguments
  #' @export
  for (i in 1:length(pattern)) x <- gsub(pattern[i], replacement[i], x, ...)
  x
  return(x)
}

trim_space <- function(x, what = c("both", "leading", "trailing", "none"), space.regex = "[:space:]", ...) {
  #' trim space function
  #' @param x variable of interest
  #' @param what Choices are both, leading, trailing, none
  #' @param space.regex Default set to [:space:]
  #' @param ... Additional arguments
  #' @export
  if (missing(x)) 
    stop("nothing to trim spaces to =(")
  re <- switch(match.arg(what), both = sprintf("^[%s]+|[%s]+$", space.regex, space.regex), 
               leading = sprintf("^[%s]+", space.regex), trailing = sprintf("[%s]+$", space.regex), 
               none = {
                 return(x)
               })
  vgsub(re, "", x, ...)
}

is_empty <- function(x, trim = TRUE, ...) {
  #' Empty variable check
  #' @param x x
  #' @param trim defaults to true
  #' @param ... Additional arguments
  #' @export
  if (length(x) <= 1) {
    if (is.null(x)) 
      return(TRUE)
    if (length(x) == 0) 
      return(TRUE)
    if (is.na(x) || is.nan(x)) 
      return(TRUE)
    if (is.character(x) && nchar(ifelse(trim, trim_space(x), x)) == 0) 
      return(TRUE)
    if (is.logical(x) && !isTRUE(x)) 
      return(TRUE)
    if (is.numeric(x) && x == 0) 
      return(TRUE)
    return(FALSE)
  } else sapply(x, is_empty, trim = trim, ...)
}

find_first <- function(y){
  #' Find earliest date
  #' @param y variable of interest
  #' @export
  g <- y[which(grepl('date', names(y), ignore.case=TRUE) == TRUE)]
  if(all(g=='')==TRUE||all(is_empty(g)==TRUE)==TRUE) {warning('All date variables are empty')}
  g2 <- date_parser(as.vector(unlist(c(g))))
  names(g)[which(g2==min(g2, na.rm=TRUE))[1]]
}

find_last <- function(y){
  #' Find latest date
  #' @param y variable of interest
  #' @export
  
  g <- y[which(grepl('date', names(y), ignore.case=TRUE) == TRUE)]
  if(all(g=='')==TRUE||all(is_empty(g)==TRUE)==TRUE) {warning('All date variables are empty')}
  g2 <- date_parser(as.vector(unlist(c(g))))
  names(g)[which(g2==max(g2, na.rm=TRUE))[1]]
}

accumarray <- function(subs, val, sz = NULL, func = sum, fillval = 0) {
  #' Accumarray fucntion
  #' @param subs subs
  #' @param val val
  #' @param sz sz
  #' @param func set to sum
  #' @param fillval set to 0
  #' @export
  
  stopifnot(is.numeric(subs), is.numeric(val))
  subs <- floor(subs)
  val <- c(val)
  if (any(subs < 1)) 
    stop("Argument 'subs' must be a matrix of integer indices.")
  matrix_p <- TRUE
  if (is.vector(subs)) {
    subs <- as.matrix(subs)
    matrix_p <- FALSE
  }
  n <- nrow(subs)
  m <- ncol(subs)
  if (length(val) < n) 
    stop("Length of 'vals' must not be smaller than no. of rows of 'subs'.")
  dm <- apply(subs, 2, max)
  if (!is.null(sz)) {
    if (length(sz) != ncol(subs) || any(sz < dm)) 
      stop("Argument 'sz' does not fit with 'subs'.")
    dm <- sz
  }
  if (m == 1) {
    A <- rep(fillval, dm)
    for (i in unique(subs)) {
      A[i] <- func(val[subs == i], na.rm = T)
    }
    if (matrix_p) 
      A <- as.matrix(A)
  } else {
    cm <- cumprod(dm[1:(m - 1)])
    A <- array(fillval, dim = dm)
    K <- numeric(n)
    for (i in 1:n) {
      K[i] <- subs[i, 1] + sum(cm * (subs[i, 2:m] - 1))
    }
    for (i in unique(K)) {
      A[i] <- func(val[K == i], na.rm = T)
    }
  }
  return(A)
}

skewness <- function(x, na.rm=FALSE) {
  #' Calculate skewness
  #' @param x variable of interest
  #' @param na.rm set to FALSE
  #' @export
  if(na.rm==TRUE){
    x = x[is.na(x)==FALSE]
  } else {
    x = x
  }
  n <- length(x)
  v <- var(x)
  m <- mean(x)
  third.moment <- (1/(n - 2)) * sum((x - m)^3)
  third.moment/(var(x)^(3/2))
}

date_parser <- function(dates){
  #' Parse date variable
  #' @param dates Variable containing dates
  #' @importFrom lubridate dym ymd myd ydm dmy mdy
  #' @export
  
  dates <- trimws(dates)
  dates <- sub(' .*','\\1',dates)
  if(!all(is.na(suppressWarnings(lubridate::mdy(dates)))==T)) {
    lubridate::mdy(dates)
  } else if(!all(is.na(suppressWarnings(lubridate::dmy(dates)))==T)) {
    lubridate::dmy(dates)
  } else if(!all(is.na(suppressWarnings(lubridate::ymd(dates)))==T)) {
    lubridate::ymd(dates)
  } else if(!all(is.na(suppressWarnings(lubridate::ydm(dates)))==T)) {
    lubridate::ydm(dates)
  } else if(!all(is.na(suppressWarnings(lubridate::myd(dates)))==T)) {
    lubridate::myd(dates)
  } else if(!all(is.na(suppressWarnings(lubridate::dym(dates)))==T)) {
    lubridate::dym(dates)
  } else {
    stop('Date format not recognized. Format date before proceeding')
  }
}

find_original_name <- function(fun) {
  #' find original name
  #' @param fun function
  #' @export
  objects <- ls(envir = environment(fun))
  for (i in objects) {
    if (identical(fun, get(i, envir = environment(fun)))) {
      return(i)
    }
  }
}

degree <- function(dat, lat=NULL, lon=NULL, latsign=FALSE, lonsign=FALSE){
  #' Convert lat/long coordinates to decimal degrees
  #' @param dat Data table containing latitude and longitude data
  #' @param lat Name of vector containing latitude data
  #' @param lon Name of vector containg longitude data
  #' @param latsign If TRUE, transforms sign from positive to minus or minus to positive
  #' @param lonsign If TRUE, transforms sign from positive to minus or minus to positive
  #' @export degree
  #' @importFrom OSMscale degree
  #' @importFrom stringr str_replace
  #' @details Uses the degree function to convert lat long coordinates to decimal degrees.
  #' @return The original dataframe with the latitudes and longitudes converted to decimal degrees.
  #' Changing the sign, transforms all values in the variable. 
  #' @examples 
  #' \dontrun{
  #' dat <- degree(MainDataTable, 'LatLon_START_LAT', 'LatLon_START_LON', latsign=FALSE, lonsign=FALSE)
  #' }
  #' 

  if(!is.null(lat)){
    if(!is.numeric(dat[[lat]])) {
      temp = gsub("\u00b0|'|\"", "", dat[[lat]])
      temp[lengths(gregexpr(" ", temp))==1&!is.na(temp)] <- paste(temp[lengths(gregexpr(" ", temp))==1&!is.na(temp)], '00')
      dat[[lat]] <- as.numeric(sapply(strsplit(temp, "\\s+"), '[', 1))+ as.numeric(sapply(strsplit(temp, "\\s+"), '[', 2))/60+as.numeric(sapply(strsplit(temp, "\\s+"), '[', 3))/360
      
    }  else if(any(nchar(trunc(abs(dat[[lat]])))>2, na.rm=T)){
        nm <- !is.na(dat[[lat]])&dat[[lat]] < 0
        dat[[lat]] <- abs(dat[[lat]])
        i <- nchar(abs(dat[[lat]]))<=4&!is.na(dat[[lat]])
        dat[[lat]][i] <- paste0(dat[[lat]][i], '00')
        dat[[lat]] <-  format(as.numeric(stringr::str_pad(abs(as.numeric(dat[[lat]])), 6, pad = "0")), scientific=FALSE)
        dat[[lat]] <- as.numeric(substr(dat[[lat]], start = 1, stop = 2)) + as.numeric(substr(dat[[lat]], start = 3, stop = 4))/60 + as.numeric(substr(dat[[lat]], start = 5, stop = 6))/3600  
        dat[[lat]][nm] <- dat[[lat]][nm]*-1
    } else {
        dat <- dat
      }
  }
  if(!is.null(lon)){
    if(!is.numeric(dat[[lon]])) {
      temp = gsub("\u00b0|'|\"", "", dat[, lon])
      temp[lengths(gregexpr(" ", temp))==1&!is.na(temp)] <- paste(temp[lengths(gregexpr(" ", temp))==1&!is.na(temp)], '00')
      dat[[lon]] <- as.numeric(sapply(strsplit(temp, "\\s+"), '[', 1))+ as.numeric(sapply(strsplit(temp, "\\s+"), '[', 2))/60+as.numeric(sapply(strsplit(temp, "\\s+"), '[', 3))/360
      
    } else if(any(nchar(trunc(abs(as.numeric(dat[[lon]]))))>3, na.rm=T)){
        nm <- !is.na(dat[[lon]])&as.numeric(dat[[lon]]) < 0
        dat[[lon]] <- abs(dat[[lon]])
        i <- nchar(dat[[lon]])<=5&!is.na(dat[[lon]])
        dat[[lon]][i] <- paste0(dat[[lon]][i], '00')
        dat[[lon]] <-  format(as.numeric(stringr::str_pad(as.numeric(dat[[lon]]), 7, pad = "0")), scientific=FALSE)
        dat[[lon]] <- as.numeric(substr(dat[[lon]], start = 1, stop = 3)) + as.numeric(substr(dat[[lon]], start = 4, stop = 5))/60 + as.numeric(substr(dat[[lon]], start = 6, stop = 7))/3600  
        dat[[lon]][nm] <- dat[[lon]][nm]*-1
    } else {
        dat <- dat
      }
    }

  if(latsign==TRUE&!is.null(lat)){
    dat[[lat]] <- -1*dat[[lat]]
  } else {
    dat <- dat
  }
  if(lonsign==TRUE&!is.null(lon)){
    dat[[lon]] <- -1*dat[[lon]]
  } else {
    dat <- dat
  }
  return(dat)
}

data_pull <- function(dat){
  #' Pull data from sqlite database
  #' @param dat Data table 
  #' @export data_pull 
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase())
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
  DBI::dbDisconnect(fishset_db)
  
  if(is.character(dat)==TRUE){
    dat <- dat
  } else {
    dat <- deparse(substitute(dat))
  }
  return(list(dat=dat,dataset=dataset))

}

fishset_theme <- ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
                       panel.grid.minor = ggplot2::element_blank(), 
                       panel.background = ggplot2::element_blank(), 
                       axis.line = ggplot2::element_line(colour = "black"), 
                       axis.text = ggplot2::element_text(size=11), 
                       axis.title = ggplot2::element_text(size=11))

save_table <- function(table, project, func_name, ...) {
  #' Save table to output folder
  #' @param table table name.
  #' @param project project name.
  #' @param func_name function name.
  #' @param ... addition arguments passsed to write.csv function. 
  #' @export
  #' @examples 
  #' \dontrun {
  #' 
  #' save_table(count, project, "species_catch")
  #' 
  #' }   
  write.csv(table, paste0(locoutput(), project, "_", func_name, "_", Sys.Date(), '.csv'))
  
}

save_plot <- function(project, func_name, ...) {
  #' Save table to output folder
  #' @param project name of project.
  #' @param func_name function name.
  #' @param ... addition arguments passed to the ggsave function. 
  #' @export
  #' @examples 
  #' \dontrun {
  #' 
  #' save_plot(project, "species_catch")
  #' 
  #' }
  
  ggplot2::ggsave(file = paste0(locoutput(), project, "_", func_name, "_", Sys.Date(), '.png'), ...)
  
}

date_factorize <- function(dataset, date_col, date_code) {
  #' Convert date variable of type character to ordered factor
  #' @param dataset data frame containg date variable.
  #' @param date_col date variable of type character to convert to ordered factor.
  #' @param date_code date code used to format date variable.
  
  if (date_code %in% c("%a", "%A", "%b", "%B")) { 
    
    if (date_code == "%b") {
      
      dataset[[date_col]] <- factor(dataset[[date_col]], 
                                    levels = month.abb, 
                                    ordered = T)
      
    } else if (date_code == "%B") {
      
      dataset[[date_col]] <- factor(dataset[[date_col]], 
                                    levels = month.name, 
                                    ordered = T)
      
    } else if (date_code == "%a") {
      
      dataset[[date_col]] <- factor(dataset[[date_col]], 
                                    levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), 
                                    ordered = T)
      
    } else {
      
      dataset[[date_col]] <- factor(dataset[[date_col]], 
                                    levels = c("Sunday", "Monday", "Tuesday", "Wednesday", 
                                               "Thursday", "Friday", "Saturday"), 
                                    ordered = T)
    }
    
  } else {
    
    stop("Date format is not character type")
    
  }
  dataset
}

text_filepath <- function(project, fun_name) {
  #' Create a filepath for a .txt document in the output folder
  #' @param project Name of project.
  #' @param fun_name Name of function. 
  #' @return Useful for saving messages generated in functions. 
  #' @examples 
  #' \dontrun{
  #' 
  #' cat("message", file = text_filepath("my_project", "qaqc_output"))
  #' 
  #' }
  
  paste0(locoutput(), project, "_", fun_name, Sys.Date(), ".txt")
  
}



##---------------------------##
outlier_plot_int <- function(dat, x, dat.remove = "none", x.dist = "normal", plot_type) {
  #' Evaluate outliers through plots
  #' @param dat Main data frame over which to apply function. Table in fishet_db database should contain the string `MainDataTable`.
  #' @param x Column in dataf rame to check for outliers
  #' @param dat.remove Defines method to subset the data. Choices include: none, 5_95_quant, 25_75_quant, mean_2SD, median_2SD, mean_3SD, median_3SD
  #' @param x.dist Distribution of the data. Choices include: normal, lognormal, exponential, weibull, poisson, negative binomial
  #' @param plot_type Which plot to reeturn
  #' @importFrom graphics points
  #' @importFrom ggpubr annotate_figure text_grob
  #' @import ggplot2
  #' @details  The function returns three plots, the data, a probability plot, and a Q-Q plot. The data plot is the value of
  #'  x against row number. Red points are all the data without any points removed. The blue points are the subsetted data. If `dat.remove` is `none`, then only blue points will be shown. 
  #'  The probability plot is a histogram of the data with the fitted probability distribution based on `x.dist`. The Q-Q plot plots are
  #'  sampled quantiles against theoretical quantiles. 
  #'  
  #' @export 
  #' @return Plot of the data
  
  
  requireNamespace('ggplot2')
  
  dataset <- dat
  x.name <- x
  if (is.numeric(dataset[, x]) == T) {
    # Begin outlier check
    dataset$val <- 1:nrow(dataset)
    if (dat.remove == "none") {
      dataset$Points <- 'Kept'
    } else {
      if (dat.remove == "5_95_quant") {
        dataset$Points <- ifelse(dataset[, x] < stats::quantile(dataset[, x], 0.95, na.rm=TRUE) & 
          dataset[, x] > stats::quantile(dataset[, x], 0.05, na.rm=TRUE), 'Kept','Removed')
      } else if (dat.remove == "25_75_quant") {
        dataset$Points  <-ifelse(dataset[, x] < stats::quantile(dataset[, x], 0.75, na.rm=TRUE) & 
                                   dataset[, x] > stats::quantile(dataset[, x], 0.25, na.rm=TRUE), 'Kept','Removed')
      } else if (dat.remove == "mean_2SD") {
        dataset$Points  <- ifelse(dataset[, x] < (mean(dataset[, x], na.rm = T) + 2 * 
                                             stats::sd(dataset[, x], na.rm = T)) & 
                             dataset[, x] > (mean(dataset[, x], na.rm = T) - 2 * stats::sd(dataset[, x], na.rm = T)), 'Kept','Removed')
      } else if (dat.remove == "median_2SD") {
        dataset$Points  <- ifelse(dataset[, x] < (stats::median(dataset[, x], na.rm = T) + 2 * stats::sd(dataset[, x], na.rm = T)) & 
                             dataset[, x] > (stats::median(dataset[, x], na.rm = T) - 2 * stats::sd(dataset[, x], na.rm = T)),  'Kept','Removed')
      } else if (dat.remove == "mean_3SD") {
        dataset$Points  <- ifelse(dataset[, x] < (mean(dataset[, x], na.rm = T) + 3 * stats::sd(dataset[, x], na.rm = T)) & 
                             dataset[, x] > (mean(dataset[, x], na.rm = T) - 3 * stats::sd(dataset[, x], na.rm = T)),  'Kept','Removed')
      } else if (dat.remove == "median_3SD") {
        dataset$Points  <- ifelse(dataset[, x] < (stats::median(dataset[, x], na.rm = T) + 3 * stats::sd(dataset[, x], na.rm = T)) & 
                             dataset[, x] > (stats::median(dataset[, x], na.rm = T) - 3 * stats::sd(dataset[, x], na.rm = T)),  'Kept','Removed')
      }
    }  #End Outlier mod
    
    mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text=element_text(size=11),
                     axis.title=element_text(size=11))                                                                                          
    
    # Hist
    ##Plot 2!  
    if (x.dist == "normal") {    
      arg.return <- stat_function(fun = dnorm, colour = "blue", 
                               args = list(mean = mean(dataset[dataset$Points=='Kept',x], na.rm = TRUE), sd = sd(dataset[dataset$Points=='Kept',x], na.rm = TRUE)))
    } else if (x.dist == "lognormal") {
      # lognormal
      arg.return <-  stat_function(fun = dlnorm, colour = "blue", 
                               args = list(mean = mean(log(dataset[dataset$Points=='Kept',x]), na.rm = TRUE), sd = sd(log(dataset[dataset$Points=='Kept',x]), na.rm = TRUE)))
    } else if (x.dist == "exponential") {
      # Exponential
      arg.return <- stat_function(fun = dexp, colour = "blue", 
                               args = list(rate = 1/mean(dataset[dataset$Points=='Kept',x],na.rm=TRUE)))
    } else if (x.dist == "weibull") {
      # Weibull
      arg.return <- stat_function(fun = dweibull, colour = "blue", 
                               args = list(shape = 1.2/sqrt(var(log(dataset[dataset$Points=='Kept',x]),na.rm=TRUE)), 
                                           scale = mean(dataset[dataset$Points=='Kept',x],na.rm=TRUE) + 0.572/(1.2/sqrt(var(log(dataset[dataset$Points=='Kept',x]),na.rm=TRUE)))))
    } else if (x.dist == "poisson") {
      # Poisson
      arg.return <-  stat_function(fun = dpois, colour = "blue", 
                               args = list(lambda = mean(dataset[dataset$Points=='Kept',x],na.rm=TRUE)))
    } else if (x.dist == "negative binomial") {
      # Negative Binomial
      arg.return <- stat_function(fun = dnbinom, colour = "blue", 
                               args = list( mean(dataset[dataset$Points=='Kept',x],na.rm=TRUE)^2/(var(dataset[dataset$Points=='Kept',x],na.rm=TRUE) - mean(dataset[dataset$Points=='Kept',x],na.rm=TRUE)), 
                                            mu = mean(dataset[dataset$Points=='Kept',x],na.rm=TRUE)))
    }
    
    #Plot3
    # Probability plot
    quants <- seq(0, 1, length = length(dataset[dataset$Points=='Kept',x]) + 2)[2:(length(dataset[dataset$Points=='Kept',x]) + 1)]
    # normal
    if (x.dist == "normal") {
      fit_quants <- stats::qnorm(quants, mean(dataset[dataset$Points=='Kept',x],na.rm=TRUE), sd(dataset[dataset$Points=='Kept',x],na.rm=TRUE))
    } else if (x.dist == "lognormal") {
      # lognormal
      fit_quants <- stats::qlnorm(quants, mean = mean(log(dataset[dataset$Points=='Kept',x]),na.rm=TRUE), sd = sd(log(dataset[dataset$Points=='Kept',x]),na.rm=TRUE))
    } else if (x.dist == "exponential") {
      # Exponential
      fit_quants <- stats::qexp(quants, rate = 1/mean(dataset[dataset$Points=='Kept',x],na.rm=TRUE))
    } else if (x.dist == "weibull") {
      # Weibull
      fit_quants <- stats::qweibull(quants, shape = 1.2/sqrt(var(log(dataset[dataset$Points=='Kept',x]),na.rm=TRUE)), 
                                    scale = mean(dataset[dataset$Points=='Kept',x],na.rm=TRUE) + 0.572/(1.2/sqrt(var(log(dataset[dataset$Points=='Kept',x]),na.rm=TRUE))))
    } else if (x.dist == "poisson") {
      # Poisson
      fit_quants <- stats::qpois(quants, lambda = mean(dataset[dataset$Points=='Kept',x],na.rm=TRUE))
    } else if (x.dist == "negative binomial") {
      # Negative Binomial
      fit_quants <- stats::qnbinom(quants, size = mean(dataset[dataset$Points=='Kept',x],na.rm=TRUE)^2/(var(dataset[dataset$Points=='Kept',x],na.rm=TRUE) - mean(dataset[dataset$Points=='Kept',x],na.rm=TRUE)), 
                                   mu = mean(dataset[dataset$Points=='Kept',x],na.rm=TRUE))
    }
    
    data_quants <- stats::quantile(as.numeric(dataset[dataset$Points=='Kept',x]), quants,na.rm=TRUE)
    # create Q-Q plot
    temp <- data.frame(fit_quants, data_quants) 
    p3 <- ggplot(temp, aes(x=fit_quants, y=data_quants)) + geom_point(shape=1) + geom_abline() +
      labs(x='Theoretical Quantiles', y='Sample Quantiles', title=paste('Q-Q plot of', x.dist, 'fit against data'))+
      mytheme
    
    if(plot_type=='1'){
      return(dataset)
    } else if(plot_type=='2'){
        suppressWarnings(arg.return)
      } else if(plot_type=='3'){
        suppressWarnings(return(temp))
      }
    #Put it all together
    #fig <- suppressWarnings(ggpubr::ggarrange(p1, p2, p3 , ncol = 2, nrow = 2))
    # labels = c("A", "B", "C"),
    #fig <- ggpubr::annotate_figure(fig, top = ggpubr::text_grob(paste("Plots for ", x, " with ", x.dist, 
    #                                                                  " distribution and data removed based on '", dat.remove,
    #                                                                  "'. \nBlue: included points   Red: removed points"), size = 10))         
    
  } else {
    # Actions to take if data is not numeric
    print("Data is not numeric. Plots not generated.")
  }
}

#shiny_running = function () {
  # Look for `runApp` call somewhere in the call stack.
#  frames = sys.frames()
#  calls = lapply(sys.calls(), `[[`, 1)
#  call_name = function (call)
#    if (is.function(call)) '<closure>' else deparse(call)
#  call_names = vapply(calls, call_name, character(1))
  
#  target_call = grep('^runApp$', call_names)
  
#  if (length(target_call) == 0)
#    return(FALSE)
  
  # Found a function called `runApp`, verify that it’s Shiny’s.
#  target_frame = frames[[target_call]]
#  namespace_frame = parent.env(target_frame)
#  isNamespace(namespace_frame) && environmentName(namespace_frame) == 'shiny'
#}

fishset_theme <- ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
                                panel.grid.minor = ggplot2::element_blank(), 
                                panel.background = ggplot2::element_blank(), 
                                axis.line = ggplot2::element_line(colour = "black"), 
                                axis.text = ggplot2::element_text(size=11), 
                                axis.title = ggplot2::element_text(size=11))

save_table <- function(table, project, func_name, ...) {
  #' Save table to output folder
  #' @param table table name.
  #' @param project project name.
  #' @param func_name function name.
  #' @param ... addition arguments passsed to write.csv function. 
  #' @export
  #' @examples 
  #' \dontrun {
  #' 
  #' save_table(count, project, "species_catch")
  #' 
  #' }   
  write.csv(table, paste0(locoutput(), project, "_", func_name, "_", Sys.Date(), '.csv'))
  
}

save_plot <- function(project, func_name, ...) {
  #' Save table to output folder
  #' @param project name of project.
  #' @param func_name function name.
  #' @param ... addition arguments passed to the ggsave function. 
  #' @export
  #' @examples 
  #' \dontrun {
  #' 
  #' save_plot(project, "species_catch")
  #' 
  #' }
  
  ggplot2::ggsave(file = paste0(locoutput(), project, "_", func_name, "_", Sys.Date(), '.png'), ...)
  
}

date_factorize <- function(dataset, date_col, date_code) {
  #' Convert date variable of type character to ordered factor
  #' @param dataset data frame containg date variable.
  #' @param date_col date variable of type character to convert to ordered factor.
  #' @param date_code date code used to format date variable.
  
  if (date_code %in% c("%a", "%A", "%b", "%B")) { 
    
    if (date_code == "%b") {
      
      dataset[[date_col]] <- factor(dataset[[date_col]], 
                                    levels = month.abb, 
                                    ordered = T)
      
    } else if (date_code == "%B") {
      
      dataset[[date_col]] <- factor(dataset[[date_col]], 
                                    levels = month.name, 
                                    ordered = T)
      
    } else if (date_code == "%a") {
      
      dataset[[date_col]] <- factor(dataset[[date_col]], 
                                    levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), 
                                    ordered = T)
      
    } else {
      
      dataset[[date_col]] <- factor(dataset[[date_col]], 
                                    levels = c("Sunday", "Monday", "Tuesday", "Wednesday", 
                                               "Thursday", "Friday", "Saturday"), 
                                    ordered = T)
    }
    
  } else {
    
    stop("Date format is not character type")
    
  }
  dataset
}

