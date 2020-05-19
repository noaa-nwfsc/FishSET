
#' Define source location
#' @keywords internal
#' @export
locdatabase <- function(){
 if(exists('loc')) { loc=loc} else { loc=NULL}
  if(is.null(loc)){
  paste0(system.file(package='FishSET'), '/fishset_db.sqlite') 
  } else {
    paste0(loc, '/fishset_db.sqlite')
  }
}

loclog <- function(){
    #' Returns the location of the log folder
    #' @keywords internal
    #' @export
    #' @details if loc2 is not in the working environment, then the default location is use
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
  #' Output location
  #' @keywords internal
  #' @export
  if(exists('loc2')) { loc2=loc2} else { loc2=NULL}
  if(is.null(loc2)){
    paste0(system.file(package='FishSET'), '/output/')
    
  } else {
    paste0(loc2, '/output/')
  }
}

loc_map <- function(){
    #' Define source location for MapViewer folder
    #' Returns the location of the MapViewer folder
    #' @keywords internal
    #' @export
    #' @details if loc2 is not in the working environment, then the default location is use
    #' @examples 
    #' \dontrun{
    #' loc_map() # will return output folder location within the fishset package
    #' loc2 <- getwd()
    #' loc_map() #will return output folder location as within the working directory
    #' }
    if(exists('loc2')) { loc2=loc2} else { loc2=NULL}
    if(is.null(loc2)){
      paste0(system.file(package='FishSET'), '/MapViewer/')
      
    } else {
      paste0(loc2, '/MapViewer/')
    } 
}

pull_info_data <- function(project){
#' Pull the most recent data index file for given project
#' @keywords internal
#' @export
#' @param project Name of project, such as pollock

g <- tables_database()
g <- g[grep(paste0('Info.*',project,'|',project,'.*Info'), g)]
g <- gsub("[^0-9\\.]", "", g[grep('Info.', g)])[which(gsub("[^0-9\\.]", "", g[grep('Info.', g)]) == max(gsub("[^0-9\\.]", "", g[grep('Info.', g)])))]
paste0(project,'MainDataTableInfo',g)
}

table_format <- function(x) {
#' Import and format saved tables to notebook file
#' @param x Name of table saved in inst/output
#' @keywords internal
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
  #' @keywords internal
  #' @export
  #' @importFrom knitr include_graphics
  #' 
  knitr::include_graphics(paste0(getwd(), '/inst/output/',x,'.png'))
}

current_log <- function() {
  #' Lists most recent log file
  #' @keywords internal
  #' @export
  #' @details Prints the name of the most recent log file, but not the filepath.
  
  logs <- list.files(loclog())
  
  g <- gsub("[^0-9]", "", logs)
  
  log <- logs[which(g == max(g))]
  
  log
}

summary_table <- function(project) {
  #' Display dataset summary table
  #'
  #'@param project Name of project.
  #'@export
  #'@importFrom tibble rownames_to_column 
  #'@details Displays the most recent table created by \code{\link{summary_stats}} 
  #'as a dataframe. Can be used in console or notebook. 
  
  date <- gsub(".json", "", current_log())
  
  sum_tab <- read.csv(paste0(locoutput(), project, "_summary_stats_", date, ".csv"), 
                      strip.white = TRUE, check.names = FALSE)
  
  rownames(sum_tab) <- c("Min", "Median", "Mean", "Max", "Missing", 
                         "Unique Obs.", "No. 0's")
  
  sum_tab <- apply(sum_tab, 2, function(x) gsub(".*:", "", x))
  
  sum_tab <- apply(sum_tab, 2, function(x) trimws(x))
  
  sum_tab <- as.data.frame(t(sum_tab))
  
  sum_tab <- tibble::rownames_to_column(sum_tab, "Variable")
  
  sum_tab <- sum_tab[-1, ]
  
  sum_tab
}

pull_table <- function(project, table) {
  #' Retrieve name of the most recent table from a project
  #' 
  #' @param project Name of project.
  #' @param table Name of table, e.g. "MainDataTable". 
  
  tab <- tables_database()
  
  tab <- grep(paste0(project, table), tab, value = TRUE)
  
  if (table == "MainDataTable") {
    
    tab <- tab[!grepl("Info", tab)]
    
  } else if (table == "MainDataTableInfo") {
    
    tab <- grep("Info", tab, value = TRUE)
  }
  
  tab <- gsub("[^0-9\\.]", "", tab)
  
  tab <- tab[tab == max(tab)]
  
  tab <- paste0(project, table, tab)
  
  if (table_exists(tab)) {
    
    tab
    
  } else {
    
    warning(tab, " does not exist.")
  }
}


model_out_summary <- function(project) {
  #' Retrieve most recent summary of model output  
  #' 
  #' @param project Name of project
  
  p_mod <- pull_table(project, "modelOut")
  
  results <- model_out_view(p_mod)
  
  modeltab <- data.frame(Model_name = rep(NA, length(results)), 
                         covergence = rep(NA, length(results)), 
                         Stand_Errors = rep(NA, length(results)), 
                         Hessian = rep(NA, length(results)))
  
  for (i in seq_along(results)) {
    
    modeltab[i, 1] <- results[[i]]$name
    modeltab[i, 2] <- results[[i]]$optoutput$convergence
    modeltab[i, 3] <- toString(round(results[[i]]$seoutmat2, 3))
    modeltab[i, 4] <- toString(round(results[[i]]$H1, 5))
  }
  
  modeltab
}


model_error_summary <- function(project) {
  #' Retrieve most recent summary of model error  
  #' 
  #' @param project Name of project
  
  p_mod <- pull_table(project, "modelOut")
  
  results <- model_out_view(p_mod)
  
  error_out <- data.frame(Model_name = rep(NA, length(results)), 
                          Model_error = rep(NA, length(results)), 
                          Optimization_error = rep(NA, length(results)))

  for (i in seq_along(results)) {
    
    error_out[i, 1] <- results[[i]]$name
    error_out[i, 2] <- ifelse(is.null(results[[i]]$errorExplain), 'No error reported', 
                              toString(results[[i]]$errorExplain))
    error_out[i, 3] <- ifelse(is.null(results[[i]]$optoutput$optim_message), 'No message reported', 
                              toString(results[[i]]$optoutput$optim_message))
  }
  
  error_out
}


model_fit_summary <- function(project) {
  #' Retrieve most recent summary of model fit  
  #' 
  #' @param project Name of project
  
  p_mod <- pull_table(project, "modelOut")
  
  results <- model_out_view(p_mod)
  
  fit_tab <- data.frame(Model_name = rep(NA, length(results)), 
                        AIC = rep(NA, length(results)), AICc = rep(NA, length(results)),
                        BIC = rep(NA, length(results)), PseudoR2 = rep(NA, length(results)))
  
  for (i in seq_along(results)) {
    
    fit_tab[i, 1] <- results[[i]]$name
    fit_tab[i, 2] <- results[[i]]$MCM$AIC
    fit_tab[i, 3] <- results[[i]]$MCM$AICc
    fit_tab[i, 4] <- results[[i]]$MCM$BIC
    fit_tab[i, 5] <- results[[i]]$MCM$PseudoR2
  }
  
  fit_tab
}


vgsub <- function(pattern, replacement, x, ...) {
  #' vgsub function
  #' @param pattern pattern
  #' @param replacement replacement
  #' @param x x
  #' @param ... Additional arguments
  #' @keywords internal
  #' @export
  #' 
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
  #' @keywords internal
  #' @export
  #' 
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
  #' @keywords internal
  #' @export
  #' 
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
  #' @keywords internal
  #' @export
  #' 
  g <- y[which(grepl('date', names(y), ignore.case=TRUE) == TRUE)]
  if(all(g=='')==TRUE||all(is_empty(g)==TRUE)==TRUE) {warning('All date variables are empty')}
  g2 <- date_parser(as.vector(unlist(c(g))))
  names(g)[which(g2==min(g2, na.rm=TRUE))[1]]
}

find_last <- function(y){
  #' Find latest date
  #' @param y variable of interest
  #' @keywords internal
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
  #' @keywords internal
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
  #' @keywords internal
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
  #' @keywords internal
  #' @export
  #' @importFrom lubridate dym ymd myd ydm dmy mdy

  
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

date_time_parser <- function(dates){
  #' Parse date-time variable
  #' @param dates Variable containing date-times
  #' @keywords internal
  #' @export
  #' @importFrom lubridate mdy_hms mdy_hm dmy_hms dmy_hm ymd_hms ymd_hm ydm_hms ydm_hm
  
  dates <- trimws(dates)
  
  if (all(grepl("^.*\\s\\d{2}:\\d{2}:\\d{2}$", dates))) {
    
    if (all(!is.na(suppressWarnings(lubridate::mdy_hms(dates))))) {
      lubridate::mdy_hms(dates)
    } else if (all(!is.na(suppressWarnings(lubridate::dmy_hms(dates))))) {
      lubridate::dmy_hms(dates)
    } else if (all(!is.na(suppressWarnings(lubridate::ymd_hms(dates))))) {
      lubridate::ymd_hms(dates)
    } else if (all(!is.na(suppressWarnings(lubridate::ydm_hms(dates))))) {
      lubridate::ydm_hms(dates)
    }
    
  } else if (grepl("^.*\\s\\d{2}:\\d{2}$", dates)) {
    
    if (all(!is.na(suppressWarnings(lubridate::mdy_hm(dates))))) {
      lubridate::mdy_hm(dates)
    } else if (all(!is.na(suppressWarnings(lubridate::dmy_hm(dates))))) {
      lubridate::dmy_hm(dates)
    } else if (all(!is.na(suppressWarnings(lubridate::ymd_hm(dates))))) {
      lubridate::ymd_hm(dates)
    } else if (all(!is.na(suppressWarnings(lubridate::ydm_hm(dates))))) {
      lubridate::ydm_hms(dates)
    }
    
  } else {
    
    warning('Date-time format not recognized. Format date-time before proceeding')
    
    dates
  }
}

find_original_name <- function(fun) {
  #' find original name
  #' @param fun function
  #' @keywords internal
  #' @export
  objects <- ls(envir = environment(fun))
  for (i in objects) {
    if (identical(fun, get(i, envir = environment(fun)))) {
      return(i)
    }
  }
}

data_pull <- function(dat){
  #' Pull data from sqlite database
  #' @param dat Data table 
  #' @keywords internal
  #' @export

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
  #' @keywords internal
  #' @export
  #' @examples 
  #' \dontrun{
  #' save_table(count, project, "species_catch")
  #' }   
  write.csv(table, paste0(locoutput(), project, "_", func_name, "_", Sys.Date(), '.csv'))
  
}

save_plot <- function(project, func_name, ...) {
  #' Save table to output folder
  #' @param project name of project.
  #' @param func_name function name.
  #' @param ... addition arguments passed to the ggsave function. 
  #' @keywords internal
  #' @export
  #' @examples 
  #' \dontrun{
  #' save_plot(project, "species_catch")
  #' }
  
  ggplot2::ggsave(file = paste0(locoutput(), project, "_", func_name, "_", Sys.Date(), '.png'), ...)
  
}

periods_list <- list("%B" = month.name,
                     "%b" = month.abb,
                     "%A" = c("Sunday", "Monday", "Tuesday", "Wednesday", 
                              "Thursday", "Friday", "Saturday"),
                     "%a" = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),
                     "%m" = 1:12,
                     "%w" = 0:6,
                     "%d" = 1:31,
                     "%j" = 1:365,
                     "%U" = 1:52)

date_factorize <- function(dataset, date_col, date_code) {
  #' Convert date variable of type character to ordered factor
  #' @keywords internal
  #' @export
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
  #' @keywords internal
  #' @export
  #' @param project Name of project.
  #' @param fun_name Name of function. 
  #' @return Useful for saving messages generated in functions. 
  #' @examples 
  #' \dontrun{
  #' cat("message", file = text_filepath("my_project", "qaqc_output"))
  #' }
  
  paste0(locoutput(), project, "_", fun_name, Sys.Date(), ".txt")
  
}

date_title <- function(plot, filter_date, filter_value) {
  #' Add date to ggplot title
  #' @keywords internal
  #' @export
  #' @param plot ggplot2 plot object to add new title to.
  #' @param filter_date The \code{filter_date} parameter in function.
  #' @param filter_value The values used to filter the data table used in plot.
  #' @return A plot with the year, month, or year-month included in tittle. 
  #' @importFrom dplyr case_when
 
  
  fv_len <- length(filter_value)
  
  if (filter_date == "year") {
    
    if (fv_len == 1) {
      
      plot$labels$subtitle <- paste0("Year: ", filter_value)
      
    } else {
      
      plot$labels$subtitle <- paste0("Year: ", 
                                     filter_value[1], "-", filter_value[fv_len])
    }
    
  } else if (filter_date == "month") {
    
    month <- dplyr::case_when(filter_value == 1 ~ "Jan",
                              filter_value == 2 ~ "Feb",
                              filter_value == 3 ~ "Mar",
                              filter_value == 4 ~ "Apr",
                              filter_value == 5 ~ "May",
                              filter_value == 6 ~ "Jun",
                              filter_value == 7 ~ "Jul",
                              filter_value == 8 ~ "Aug",
                              filter_value == 9 ~ "Sep",
                              filter_value == 10 ~ "Oct",
                              filter_value == 11 ~ "Nov",
                              filter_value == 12 ~ "Dec")
    
    if (fv_len == 1) {
      
      plot$labels$subtitle <- paste0(month)
      
    } else {
      
      plot$labels$subtitle <- paste0(month[1], "-", month[fv_len])
    }
    
  } else if (filter_date == "year-month") {
    
    y_num <- filter_value[[1]]
    m_num <- filter_value[[2]]
    y_len <- length(y_num)
    m_len <- length(m_num)
    
    month <- dplyr::case_when(m_num == 1 ~ "Jan",
                              m_num == 2 ~ "Feb",
                              m_num == 3 ~ "Mar",
                              m_num == 4 ~ "Apr",
                              m_num == 5 ~ "May",
                              m_num == 6 ~ "Jun",
                              m_num == 7 ~ "Jul",
                              m_num == 8 ~ "Aug",
                              m_num == 9 ~ "Sep",
                              m_num == 10 ~ "Oct",
                              m_num == 11 ~ "Nov",
                              m_num == 12 ~ "Dec")
    
    if (y_len == 1 & m_len == 1) {
      
      plot$labels$subtitle <- paste(month, y_num)
      
    } else if (y_len > 1 & m_len == 1) {
      
      plot$labels$subtitle <- paste0(month, " ", y_num[1], "-", y_num[y_len])
      
    } else if (y_len == 1 & m_len > 1) {
      
      plot$labels$subtitle <- paste0(month[1], "-", month[m_len], " ", y_num)
      
    } else if (y_len > 1 & m_len > 1) {
      
      plot$labels$subtitle <- paste0(month[1], "-", month[m_len], " ",
                                     y_num[1], "-", y_num[y_len])
      
    } else {
      
      warning("Invalid year-month length.")
    }
  }
  
  plot
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
  #' @keywords internal
  #' @export
  #' @details  The function returns three plots, the data, a probability plot, and a Q-Q plot. The data plot is the value of
  #'  x against row number. Red points are all the data without any points removed. The blue points are the subsetted data. If `dat.remove` is `none`, then only blue points will be shown. 
  #'  The probability plot is a histogram of the data with the fitted probability distribution based on `x.dist`. The Q-Q plot plots are
  #'  sampled quantiles against theoretical quantiles. 
  #'  
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

#' quietly_test
#' capture messages if exist and print to shiny app
#' @param .f function name
#' @export
#' @keywords internal
#' @importFrom purrr quietly safely
#
quietly_test <- function(.f) {
  fun1 <- purrr::quietly(.f ) 
  fun <- purrr::safely(fun1)
  function(...) {
    res <- fun(...)
    
    if(!is.null(res$error)) {  # safely output
      showNotification(res$error$message, duration = 10, type="error")
      return(res$result)
    }
    res <- res$result # quietly output
    if(!is.null(res$warnings) && length(res$warnings) > 0) {
      lapply(unique(res$warnings), showNotification, duration = 10, type="warning")
    }
    return(res$result)
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
