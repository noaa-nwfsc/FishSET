
locproject <- function() {
  #' Define projects folder location
  #'
  #'@export
  #'@keywords internal
  
  if (exists("loc2")) loc2 <- loc2
  else loc2 <- NULL
  
  if (!exists('loc2') || is.null(loc2)) {
    
    proj_dir <- paste0(system.file(package = "FishSET"), "/projects")
    
  } else {
    
    proj_dir <- paste0(loc2, "/projects")
  }
  
  proj_dir
}

project_exists <- function(project) {
  #' Check if project exists
  #' 
  #' @param project Project name. 
  #' 
  #' @export
  #' @keywords internal
  
  projdir <- paste0(locproject(), "/", project)
  
  dir.exists(projdir)
}

#Check if project folder exists
#If not, create the folder

check_proj <- function(project = NULL) {
  #' Check for project folder. Create folders if required
  #' @param project Project name
  #' @keywords internal
  #' @export
  
  if (!is.null(project)) {
      
    proj_dir <- paste0(locproject(), '/', project)
  
    # check if projects folder exists
    if (!file.exists(locproject())) {
      
      dir.create(file.path(locproject()), showWarnings = FALSE)
    }
     
    if (project_exists(project) == FALSE) {
      
      dir.create(file.path(proj_dir), showWarnings = FALSE)
      
      #Logs
      dir.create(file.path(paste0(proj_dir, '/src')), showWarnings = FALSE)
      
      #output
      dir.create(file.path(paste0(proj_dir, '/output')), showWarnings = FALSE)
      
      #database
      fishset_db <- DBI::dbConnect(RSQLite::SQLite(), paste0(proj_dir, '/fishset_db.sqlite'))
      on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
      
      #data
      dir.create(file.path(paste0(proj_dir, '/data')), showWarnings = FALSE)
      
      #doc (report)
      dir.create(file.path(paste0(proj_dir, '/doc')), showWarnings = FALSE)
      
      #MapViewer
      dir.create(file.path(paste0(proj_dir, '/MapViewer')), showWarnings = FALSE)
    }
  } else {
    
    warning('Project name must be specified.')
  }
}

erase_project <- function(project) {
  #' Erase project folder
  #' 
  #' @param project Project name.
  #' @export
  #' @keywords internal
  
  if (project_exists(project)) {
    
    unlink(paste0(locproject(), "/", project), recursive = TRUE)
  
  } else {
    
    warning(paste0("Project \"", project, "\" does not exists"))
  }
}


locdatabase <- function(project) {
  #' Define source location
  #' @param project Project name
  #' @keywords internal
  #' @export
  #' 
  
  if(!exists('project')||is.null(project)){
    warning('Project name must be provided.')
  } else {
    if (exists("loc2")) {
      loc2 <- loc2
      } else {
    loc2 <- NULL
    }
  
      if (!exists('loc2')||is.null(loc2)) {
      paste0(system.file(package = "FishSET"), "/projects/", project, "/fishset_db.sqlite")
    } else {
      paste0(loc2, "/projects/", project, "/fishset_db.sqlite")
    }
  }
}

loclog <- function(project) {
  #' Returns the location of the log folder
  #' @param project Project name
  #' @keywords internal
  #' @export
  #' @details if loc2 is not in the working environment, then the default location is use
  #' @examples
  #' \dontrun{
  #' loclog('pollock') # will return log folder location for project pollock within the fishset package
  #' }
  
  if(is.null(project)){
    warning('Project name must be provided.')
  } else {
    
    if (exists("loc2")) {
      loc2 <- loc2
    } else {
      loc2 <- NULL
    }
    if (!exists('loc2')||is.null(loc2)) {
      paste0(system.file(package = "FishSET"), "/projects/", project, "/src/")
    } else {
      paste0(loc2, "/projects/", project, "/src/")
    }
  }
}

locoutput <- function(project) {
  #' Output location
  #' @param project Project name
  #' @keywords internal
  #' @export
 
  if(is.null(project)){
    warning('Project name must be provided.')
  } else {
  

    if (!exists('loc2')||is.null(loc2)) {
      paste0(system.file(package = "FishSET"), "/projects/", project, "/output/")
    } else {
      paste0(loc2, "/projects/", project, "/output/")
    }
  }
}

loc_map <- function(project) {
  #' Define source location for MapViewer folder
  #' Returns the location of the MapViewer folder
  #' @param project Project name
  #' @keywords internal
  #' @export
  #' @details if loc2 is not in the working environment, then the default location is use
  #' @examples
  #' \dontrun{
  #' loc_map() # will return output folder location within the fishset package
  #' loc2 <- getwd()
  #' loc_map() #will return output folder location as within the working directory
  #' }

  if(is.null(project)){
    warning('Project name must be provided.')
  } else {
    
    if (!exists('loc2')||is.null(loc2)) {
      paste0(system.file(package = "FishSET"), "/projects/", project, "/MapViewer/")
    } else {
      paste0(loc2, "/projects/", project, "/MapViewer/")
    }
  }
}

loc_data <- function(project) {
  #' Define source location for data folder
  #' Returns the location of the data folder
  #' @param project Project name
  #' @keywords internal
  #' @export
  #' @details if loc2 is not in the working environment, then the default location is use
  
  
  if(is.null(project)){
    warning('Project name must be provided.')
  } else {
    
    if (!exists('loc2')||is.null(loc2)) {
      paste0(system.file(package = "FishSET"), "/projects/", project, "/data/")
    } else {
      paste0(loc2, "/projects/", project, "/data/")
    }
  }
}

loc_meta <- function(project) {
  #' Define source location for meta file
  #' @param project Project name.
  #' @keywords internal
  #' @export
  #' 
  
  paste0(locproject(), "/", project, "/doc/meta_log.json")
}


#pull_info_data <- function(project) {
#  #' Pull the most recent data index file for given project
#  #' @keywords internal
#  #' @export
#  #' @param project Name of project, such as pollock

#  g <- tables_database()
#  g <- g[grep(paste0("Info.*", project, "|", project, ".*Info"), g)]
#  g <- gsub("[^0-9\\.]", "", g[grep("Info.", g)])[which(gsub("[^0-9\\.]", "", g[grep("Info.", g)]) == max(gsub("[^0-9\\.]", "", g[grep("Info.", g)])))]
#  paste0(project, "MainDataTableInfo", g)
#}

find_project <- function(dat, project=NULL){
#' Find project
#' @param dat Data table name
#' @param project Project Name
#' @export
#' @keywords internal

  if(is.null(project)){
    if(grepl('MainDataTable', dat)){
    project <- sub("\\MainDataTable", "", dat)
    return(project)
    } else {
      "Project name must be supplied."
    }
   
  } else {
    project <- project
    return(project)
  }
  
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
  if (missing(x)) {
    stop("nothing to trim spaces to =(")
  }
  re <- switch(match.arg(what), both = sprintf("^[%s]+|[%s]+$", space.regex, space.regex),
    leading = sprintf("^[%s]+", space.regex), trailing = sprintf("[%s]+$", space.regex),
    none = {
      return(x)
    }
  )
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
    if (is.null(x)) {
      return(TRUE)
    }
    if (length(x) == 0) {
      return(TRUE)
    }
    if (is.na(x) || is.nan(x)) {
      return(TRUE)
    }
    if (is.character(x) && nchar(ifelse(trim, trim_space(x), x)) == 0) {
      return(TRUE)
    }
    if (is.logical(x) && !isTRUE(x)) {
      return(TRUE)
    }
    if (is.numeric(x) && x == 0) {
      return(TRUE)
    }
    return(FALSE)
  } else {
    sapply(x, is_empty, trim = trim, ...)
  }
}

is_value_empty <- function(x) {
  #' Empty value check
  #' @param x A value, input, or argument to check.
  #' @keywords internal
  #' @export
  
  if (is.null(x)) {
    TRUE
  } else if (length(x) == 0) {
    TRUE
  } else if (is.character(x) && nchar(trimws(x)) == 0) {
    TRUE
  } else {
    FALSE
  }
}

find_first <- function(y) {
  #' Find earliest date
  #' @param y variable of interest
  #' @keywords internal
  #' @export
  #'
  g <- y[which(grepl("date", names(y), ignore.case = TRUE) == TRUE)]
  if (all(g == "") == TRUE || all(is_empty(g) == TRUE) == TRUE) {
    warning("All date variables are empty")
  }
  g2 <- date_parser(as.vector(unlist(c(g))))
  names(g)[which(g2 == min(g2, na.rm = TRUE))[1]]
}

find_last <- function(y) {
  #' Find latest date
  #' @param y variable of interest
  #' @keywords internal
  #' @export

  g <- y[which(grepl("date", names(y), ignore.case = TRUE) == TRUE)]
  if (all(g == "") == TRUE || all(is_empty(g) == TRUE) == TRUE) {
    warning("All date variables are empty")
  }
  g2 <- date_parser(as.vector(unlist(c(g))))
  names(g)[which(g2 == max(g2, na.rm = TRUE))[1]]
}

qaqc_helper <-function(dat, fun, output = "logical") {
  #' Helper function for testing data quality issues in MainDataTable
  #' @param dat Dataframe to test for quality issues. 
  #' @param fun A function or custom function that returns a single logical value 
  #'   to apply to each column in \code{dat}. There are three quick options for 
  #'   common checks: \code{"NA"}, \code{"NaN"}, and \code{"Inf"}. 
  #' @param output \code{"logical"} returns a single logical value for each column in
  #'   \code{dat}. \code{"names"} returns the column names that evaluate to \code{TRUE}.
  #' @keywords internal
  #' @export
  #' @details Returns a vector of logical values (\code{output = "logical"}) or a vector of 
  #'   column names where the condition evaluated by \code{fun} returns TRUE 
  #'   (\code{output = "names"}). 
  #' @examples 
  #' \dontrun{
  #'  outputs names of columns containing NAs
  #'     qaqc_helper(pollockMainDataTable, "NA", "names")
  #'  outputs logical vector of columns containing strings
  #'      qaqc_helper(pollockMainDataTable, is.character)
  #' custom function example
  #'      qaqc_helper(pollockMainDataTable, function(x) = all(is.na(x)), "names") 
  #' }
  
  end <- FALSE
  
  fun_value <- function() {
    if (output %in% c("logical", "names")) logical(1)
    else if (output == "value") numeric(1)
  }
  
  if (is.character(fun)) {
    
    if (fun %in% c("NA", "NaN", "Inf")) {
      
      qaqc_fun <- switch(fun, "NA" = function(x) anyNA(x), 
                         "NaN" = function(x) any(is.nan(x)), 
                         "Inf" = function(x) any(is.infinite(x)))
      
    } else if (is.function(match.fun(fun))) {
      
      qaqc_fun <- fun
      
    } else {
      
      end <- TRUE
      warning("Invalid function entered into qaqc_helper()")
    }
    
  } else if (is.function(fun)) {
    
    qaqc_fun <- fun
    
  } else {
    
    end <- TRUE
    warning("Invalid function entered into qaqc_helper()")
  }
  
  if (end == FALSE) {
    
    out <- vapply(dat, qaqc_fun, FUN.VALUE = fun_value())
    
   if (output == "names") {
      
      names(which(out))
     
   } else {
      
     out
    }
  }
}

accumarray <- function(subs, val, sz = NULL, func = sum, fillval = 0) {
  #' Accumarray function
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
  if (any(subs < 1)) {
    stop("Argument 'subs' must be a matrix of integer indices.")
  }
  matrix_p <- TRUE
  if (is.vector(subs)) {
    subs <- as.matrix(subs)
    matrix_p <- FALSE
  }
  n <- nrow(subs)
  m <- ncol(subs)
  if (length(val) < n) {
    stop("Length of 'vals' must not be smaller than no. of rows of 'subs'.")
  }
  dm <- apply(subs, 2, max)
  if (!is.null(sz)) {
    if (length(sz) != ncol(subs) || any(sz < dm)) {
      stop("Argument 'sz' does not fit with 'subs'.")
    }
    dm <- sz
  }
  if (m == 1) {
    A <- rep(fillval, dm)
    for (i in unique(subs)) {
      A[i] <- func(val[subs == i], na.rm = T)
    }
    if (matrix_p) {
      A <- as.matrix(A)
    }
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

skewness <- function(x, na.rm = FALSE) {
  #' Calculate skewness
  #' @param x variable of interest
  #' @param na.rm set to FALSE
  #' @keywords internal
  #' @export

  if (na.rm == TRUE) {
    x <- x[is.na(x) == FALSE]
  } else {
    x <- x
  }
  n <- length(x)
  v <- var(x)
  m <- mean(x)
  third.moment <- (1 / (n - 2)) * sum((x - m)^3)
  third.moment / (var(x)^(3 / 2))
}

date_parser <- function(dates) {
  #' Parse date variable
  #' @param dates Variable containing dates
  #' @keywords internal
  #' @export
  #' @importFrom lubridate dym ymd myd ydm dmy mdy

  dates <- trimws(dates)
  dates <- sub(" .*", "\\1", dates)
  if (!all(is.na(suppressWarnings(lubridate::mdy(dates))) == T)) {
    lubridate::mdy(dates)
  } else if (!all(is.na(suppressWarnings(lubridate::dmy(dates))) == T)) {
    lubridate::dmy(dates)
  } else if (!all(is.na(suppressWarnings(lubridate::ymd(dates))) == T)) {
    lubridate::ymd(dates)
  } else if (!all(is.na(suppressWarnings(lubridate::ydm(dates))) == T)) {
    lubridate::ydm(dates)
  } else if (!all(is.na(suppressWarnings(lubridate::myd(dates))) == T)) {
    lubridate::myd(dates)
  } else if (!all(is.na(suppressWarnings(lubridate::dym(dates))) == T)) {
    lubridate::dym(dates)
  } else {
    stop("Date format not recognized. Format date before proceeding")
  }
}

date_time_parser <- function(dates) {
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
    warning("Date-time format not recognized. Format date-time before proceeding")

    dates
  }
}

date_check <- function(dat, date) {
  #' Parse date/date-time variable
  #' @param dat Dataframe containing variable to convert to date/date-time.
  #' @param date Date variable name to convert.
  #' @keywords internal
  #' @returns Checks whether variable can be converted to date or date-time. Returns
  #'  dataframe with converted variable.
  #' @importFrom lubridate is.POSIXt is.Date
  #' @export

  end <- FALSE

  if (lubridate::is.POSIXt(dat[[date]])) {
    dat[[date]] <- dat[[date]]
  } else if (lubridate::is.Date(dat[[date]])) {
    dat[[date]] <- dat[[date]]
  } else if (all(grepl("^.*\\s\\d{2}:\\d{2}:\\d{2}$", dat[[date]])) |
    all(grepl("^.*\\s\\d{2}:\\d{2}$", dat[[date]]))) {
    dat[[date]] <- date_time_parser(dat[[date]])
  } else if (all(grepl("^\\d{4}-\\d{2}-\\d{2}$", dat[[date]]))) {
    dat[[date]] <- date_parser(dat[[date]])
  } else {
    warning("Start date format not recognized.")
    end <- TRUE
  }

  if (end == FALSE) {
    dat
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

data_pull <- function(dat, project) {
  #' Pull data from sqlite database
  #' @param dat Data table
  #' @param project Project name
  #' @keywords internal
  #' @export

  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project=project))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  if (is.character(dat) == TRUE) {
    if (is.null(dat) == TRUE | table_exists(dat, project) == FALSE) {
      print(DBI::dbListTables(fishset_db))
      stop(paste(dat, "not defined or does not exist. Consider using one of the tables listed above that exist in the database."))
    } else {
      dataset <- table_view(dat, project)
    }
  } else {
    dataset <- dat
  }

  if (is.character(dat) == TRUE) {
    dat <- dat
  } else {
    dat <- deparse(substitute(dat))
  }
  return(list(dat = dat, dataset = dataset))
}

parse_data_name <- function(dat, type) {
  
  #' Parse data name for logging 
  #' 
  #' @param dat Data table to be parsed. 
  #' @param type String, the data type: "main", "aux", "grid", "port", or "spat".
  #' @importFrom shiny isRunning
  #' @importFrom rlang caller_env
  #' @keywords internal
  #' @details If called while the shiny app is running, the data table name is pulled from 
  #'   the FishSET environment (`fishset_env`). Otherwise, the data table from the caller 
  #'   environment is used. 
  #' @export

  dat_type <- switch(type, "main" = "dat_name", "aux" = "aux_name", 
                   "grid" = "grid_name", "port" = "port_name", "spat" = "spat_name")
  
  if (shiny::isRunning()) {
    
    dat <- get_fishset_env(dat_type)
    
  } else { 
    
    if (!is.character(dat)) {
      
      dat <- deparse(substitute(dat, rlang::caller_env())) 
    }
  }
  
  dat
}

msg_print <- function(temp_file) {
  #' Display temporary file using message or print
  #' 
  #' If shiny is running, prints. Otherwise, uses message. 
  #'
  #' @param temp_file temporary file to display. j
  #' @export
  #' @keywords internal
  #' @importFrom shiny isRunning
  #'
  
  if (shiny::isRunning()) print(suppressWarnings(readLines(temp_file, warn = FALSE)))
  else message(paste(readLines(temp_file, warn = FALSE), collapse = "\n"))
}

agg_helper <- function(dataset, value, period = NULL, group = NULL, fun = "sum") {
  #' Aggregating function 
  #'
  #' @param dataset `MainDataTable` to aggregate. 
  #' @param value String, name of variable to aggregate. 
  #' @param period String, name of period variable to aggregate by.
  #' @param group String, name of grouping variable(s) to aggregate by.
  #' @param fun String, function name to aggregate by. Also accepts anonymous functions.
  #' @export
  #' @keywords internal
  #' @importFrom rlang syms expr expr_text
  #' @importFrom stringi stri_isempty
  #' @importFrom stats aggregate reformulate
  
  end <- FALSE
  agg_cols <- c(value, period, group)
  
  if (any(!(agg_cols %in% names(dataset)))) {
    
    warning("The column(s) ",
      paste(agg_cols[!(agg_cols %in% names(dataset))], collapse = ", "),
            " are not in data table.")
    end <- TRUE
  }
  
  if (end == FALSE) {
    
    by_fm <- paste(unique(c(period, group)), collapse = " + ")
    
    value <- rlang::syms(value)
    val_fm <- rlang::expr(cbind(!!!value))
    val_fm <- rlang::expr_text(val_fm)
    
    # if period and group are NULL
    if (stringi::stri_isempty(by_fm)) by_fm <- val_fm
    
    agg_fm <- stats::reformulate(by_fm, val_fm)
    
    stats::aggregate(agg_fm, data = dataset, FUN = fun, na.action = NULL)
  }
}

spars <- function(x, dname) {
#' Helper function for sparsity functions
#' @param x Name of table
#' @param dname time period
#' @export
#' @keywords internal
  if (dname == "1 weeks") {
    y <- lubridate::round_date(as.Date(rownames(x)), "1 weeks")
  } else if (dname == "2 weeks") {
    y <- lubridate::round_date(as.Date(rownames(x)), "2 weeks")
  } else if (dname == "3 months") {
    y <- lubridate::round_date(as.Date(rownames(x)), "3 months")
  } else {
    y <- format(as.Date(rownames(x)), dname)
  }
  x <- suppressWarnings(aggregate(x, by = list(y), FUN = sum)[, -1])
  x <- ifelse(x > 0, 1, 0)
  x <- unique(x)
  # print(head(x))
  return(colSums(x == 0) / nrow(x))
}

perc_of_total <- function(dat, value_var, group = NULL, drop = FALSE, 
                          val_type = "perc", output = "dataset") {
  
  #' Calculate grouped percentages 
  #' 
  #' @param dat Data table to summarize.
  #' @param value_var String, variable name(s) for calculating total. 
  #' @param group String, grouping variable(s) to group `value_var` by. 
  #' @param drop Logical, whether to drop the total column. 
  #' @param val_type String, whether to convert value output to percentage 
  #'   \code{"perc"} or proportion \code{"prop"}. 
  #' @param output String, whether to add new variables to dataset (\code{"dataset"})
  #'   or return a summary table (\code{"summary"})
  #' @importFrom dplyr group_by across mutate ungroup select all_of
  #' @importFrom purrr map2
  #' @importFrom magrittr %>%
  #' @keywords internal
  #' @export
  #' 
 
  . <- NULL
  
  val_total <- paste0(value_var, "_total") 
  val_perc <- paste0(value_var, "_perc")
  p_val <- ifelse(val_type == "perc", 100, 1)
  
  if (output == "dataset") {
    
    dat <- 
      dat %>% 
      {if (!is.null(group)) dplyr::group_by(., dplyr::across(group)) else .} %>% 
      
      dplyr::mutate(dplyr::across(value_var, sum, .names = "{.col}_total")) %>% 
      dplyr::ungroup() 
    
    dat[val_perc] <- 
      purrr::map2(dat[value_var], dat[val_total], ~ (.x / .y) * p_val)
    
  } else if (output == "summary") {
    
    dat <- 
      dat %>% 
      {if (!is.null(group)) dplyr::group_by(., dplyr::across(group)) else .} %>% 
      
      dplyr::summarize(dplyr::across(value_var, sum, .names = "{.col}_total")) %>% 
      dplyr::ungroup() 
    
    dat[val_perc] <- 
      lapply(val_total, function(x) dat[[x]]/sum(dat[[x]]) * p_val)
  }
  
  if (drop) dat <- dplyr::select(dat, -dplyr::all_of(val_total))
  
  dat
}


add_missing_dates <- function(dataset, date = NULL, value, sub_date = NULL, group = NULL, 
                              facet_by = NULL, fun = "sum") {
  #' Add missing dates to `MainDataTable`.
  #'
  #' @param dataset Object containing `MainDataTable`. 
  #' @param date String, name of date variable to find missing days.
  #' @param sub_date String, name of date variable to subset by. 
  #' @param value String, name of value variable to be aggregated by \code{agg_helper}.
  #' @param group String, name of grouping variable(s).
  #' @param facet_by String, name of variable(s) to be facetted (split). 
  #' @export
  #' @keywords internal
  #' @importFrom dplyr anti_join bind_rows
  
  if (is.null(date) & is.null(group) & is.null(facet_by)) {
    
    dataset
  
  } else {
    
    if (!is.null(date) | !is.null(sub_date)) {
      
      dataset[unique(c(date, sub_date))] <- 
        lapply(dataset[unique(c(date, sub_date))], function(x) {
          
          if (any(!(class(x) %in% c("Date", "POSIXct", "POSIXt")))) {
            date_parser(x)
          } else x
        })
    } 
    
    if (!is.null(date)) {
      
      full_dates <- seq.Date(from = min(dataset[[date]], na.rm = TRUE), 
                             to = max(dataset[[date]], na.rm = TRUE), 
                             by = "day")
    }
    
    if (!is.null(sub_date) & !is.null(date)) {
      if (date == sub_date) {
        sub_date <- NULL
      }
    }
    
    cols <- unique(c(date, sub_date, group, facet_by))
    aux <- unique(c(group, facet_by))
    
    if (run_confid_check()) {
      
     check <- get_confid_check()
     cols <- unique(c(cols, check$v_id))
     aux <- unique(c(aux, check$v_id))
    }
    
    missing <- lapply(dataset[aux], function(x) unique(x))
    
    if (!is.null(date)) missing[[date]] <- full_dates
    
    missing <- do.call(expand.grid, list(missing))
    
    missing <- dplyr::anti_join(missing, dataset[cols])
    
    if (nrow(missing) > 0) {
      
      if (!is.null(sub_date)) missing[sub_date] <- NA
      
      if (fun == "sum") missing[value] <- 0
      else if (fun == "count") missing[value] <- NA # agg_helper will treat this as zero
      
      dataset <- dplyr::bind_rows(dataset[unique(c(cols, value))], missing)
    }
    
    dataset
  }
}


subset_date <- function(dataset, date, filter, value) {
  #' Subset dataset by date value/range
  #' 
  #' @param dataset `MainDataTable` to filter.
  #' @param date String, name of date variable to subset by.
  #' @param filter String, filter type. 
  #' @param value A range of dates if \code{filter = "date_range"}, or integer if
  #'   using a period filter. 
  #' @export
  #' @keywords internal
  
  if (filter == "date_range") {
      
    dataset <- dataset[dataset[[date]] >= value[1] & dataset[[date]] <= value[2], ]
    
  } else {
    
    pf <- switch(filter, "year-month" = c("%Y", "%m"), "year-week" = c("%Y", "%U"),
                 "year-day" = c("%Y", "%j"), "year" = "%Y", "month" = "%m", "week" = "%U",
                 "day" = "%j")
    
    if (grepl("-", filter)) {
      
      dataset <- dataset[(as.integer(format(dataset[[date]], pf[1])) %in% value[[1]]) & 
                           (as.integer(format(dataset[[date]], pf[2])) %in% value[[2]]), ]
      
    } else {
      
      dataset <- dataset[as.integer(format(dataset[[date]], pf)) %in% value, ]
    }
  }
  
  dataset
}


subset_var <- function(dataset, filter_by = NULL, filter_value = NULL, filter_expr = NULL) {
  #' Subset `MainDataTable` by variable
  #' 
  #' @param dataset dataset `MainDataTable` to filter.
  #' @param filter_by String, name of variable used to subset `MainDataTable`.
  #' @param filter_value Vector of values to subset by (inclusive). 
  #' @param filter_expr String, a valid R expression used to subset `MainDataTable`.
  #' @export
  #' @keywords internal 
  #' @importFrom rlang parse_expr

  if (!is.null(filter_by) & !is.null(filter_value)) {
      
      rows <- dataset[[filter_by]] %in% filter_value
      
      if (is.logical(rows)) {
        
        dataset <- dataset[rows, ]
        
      } else {
        
        warning("Invalid filter expression.")
      }
    } 
    
    if (!is_empty(filter_expr)) {
      
      p_expr <- rlang::parse_expr(filter_expr)
      
      rows <- eval(p_expr, envir = dataset)
      
      if (is.logical(rows)) {
        
        dataset <- dataset[rows, ]
        
      } else {
        
        warning("Invalid filter expression.")
      }
    }

    dataset
  } 

fishset_theme <- function() {
#' Default FishSET plot theme
#' 
#' @keywords internal
#' @export
#' @import ggplot2

  ggplot2::theme(
  panel.grid.major = ggplot2::element_blank(),
  panel.grid.minor = ggplot2::element_blank(),
  panel.background = ggplot2::element_blank(),
  axis.line = ggplot2::element_line(colour = "black"),
  axis.text = ggplot2::element_text(size = 11),
  axis.title = ggplot2::element_text(size = 11))
}

num_breaks <- function(per) {
  #'
  #'Plot breaks for numeric variable
  #'
  #'@param per period to set breaks for.
  #'@export
  #'@keywords internal
  #'
  i <- length(unique(per))
  
  if (i <= 15) {
    
    breaks <- seq(min(per), max(per), by = 1)
    
  } else if (i > 15 & i < 30) {
    
    breaks <- seq(min(per), max(per), by = 2)
    
  } else if (i >= 30 & i <= 54) {
    
    breaks <- seq(min(per), max(per), by = 4)
    
  } else {
    
    breaks <- seq(min(per), max(per), by = 20)
  }
  
  breaks
} 

n_breaks <- function(x) {
  #'
  #' Plot breaks
  #'
  #' @param x Value to determine breaks
  #' @export
  #' @keywords internal

  i <- unique(x)
  len <- length(i)
  if (len <= 15) {
    breaks <- len
  } else {
    breaks <- 10
  }
  breaks
}

save_table <- function(table, project, func_name, ...) {
  #' Save table to output folder
  #' @param table table name.
  #' @param project project name.
  #' @param func_name function name.
  #' @param ... addition arguments passed to write.csv function.
  #' @keywords internal
  #' @export
  #' @examples
  #' \dontrun{
  #' save_table(count, project, "species_catch")
  #' }
  write.csv(table, paste0(locoutput(project=project), project, "_", func_name, "_", Sys.Date(), ".csv"))
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

  ggplot2::ggsave(file = paste0(locoutput(project=project), project, "_", func_name, "_", Sys.Date(), ".png"), ...)
}

periods_list <- list(
  "%B" = month.name,
  "%b" = month.abb,
  "%A" = c(
    "Sunday", "Monday", "Tuesday", "Wednesday",
    "Thursday", "Friday", "Saturday"
  ),
  "%a" = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),
  "%m" = 1:12,
  "%w" = 0:6,
  "%d" = 1:31,
  "%j" = 1:365,
  "%U" = 1:52
)

order_factor <- function(dat, fac, val, rev = FALSE) {
  #'
  #' Order variable
  #'
  #' @param dat dataframe containing variable to order.
  #' @param fac variable name to order.
  #' @param val value variable to order by.
  #' @param rev logical, reverse order.
  #' @return Returns entire dataframe with ordered factor.
  #' @importFrom stats aggregate reformulate
  #' @export
  #' @keywords internal
  #'
  
  agg <- stats::aggregate(stats::reformulate(fac, val), dat, FUN = sum)

  ord <- unique(agg[[fac]][order(agg[[val]], decreasing = rev)])

  dat[[fac]] <- factor(dat[[fac]], levels = ord)

  dat
}

date_factorize <- function(dataset, date_col, date_code) {
  #' Convert date variable of type character to ordered factor
  #' @keywords internal
  #' @export
  #' @param dataset data frame containing date variable.
  #' @param date_col date variable of type character to convert to ordered factor.
  #' @param date_code date code used to format date variable.

  if (date_code %in% c("%Y-%m", "%a", "%A", "%b", "%B")) {
    if (date_code == "%b") {
      dataset[[date_col]] <- factor(dataset[[date_col]],
        levels = month.abb,
        ordered = TRUE)
      
    } else if (date_code == "%B") {
      dataset[[date_col]] <- factor(dataset[[date_col]],
        levels = month.name,
        ordered = TRUE)
      
    } else if (date_code == "%a") {
      dataset[[date_col]] <- factor(dataset[[date_col]],
        levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),
        ordered = TRUE)
      
    } else if (date_code == "%A") {
      dataset[[date_col]] <- factor(dataset[[date_col]],
        levels = c(
          "Sunday", "Monday", "Tuesday", "Wednesday",
          "Thursday", "Friday", "Saturday"),
        ordered = TRUE)
      
    } else if (date_code == "%Y-%m") {
      
      dataset[[date_col]] <- factor(dataset[[date_col]], 
                                    levels = sort(unique(dataset[[date_col]])),
                                    ordered = TRUE)
    }
  } else {
    stop("Date format is not character type")
  }
  dataset
}

facet_period <- function(dataset, facet_date, date, period = NULL) {
  #' Create date variables for facetting
  #' 
  #' @param dataset Dataset used to create tables/plots in function.
  #' @param facet_date String, period to facet by ("year", "month", and "week").
  #' @param date String, Data variable used to convert to periods.
  #' @param period String, period name. Only needed if summarizing over time. 
  #' @keywords internal
  
  # checks for duplicates 
  if (!is.null(period)) {
    
    if (any(period == facet_date)) facet_date <- facet_date[-which(facet_date == period)]
  } 
  
  if (any(facet_date %in% names(dataset))) {
    
    facet_date <- facet_date[-which(facet_date %in% names(dataset))]
  }
  
  if (!is_value_empty(facet_date)) {
    
    dataset[facet_date] <- lapply(facet_date, function(x) {
      
      per <- switch(x, "year" = "%Y", "month" = "%b", "week" = "%U")
      
      if (per == "%b") {
        
        factor(format(dataset[[date]], per), levels = month.abb, ordered = TRUE) 
        
      } else as.integer(format(dataset[[date]], per))
    })
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

  paste0(locoutput(project=project), project, "_", fun_name, Sys.Date(), ".txt")
}

week_labeller <- function(breaks, year) {
  #' X-axis scale labeller for weekly data
  #' @param breaks scale label breaks 
  #' @param year vector of years from summary table
  #' @export
  #' @keywords internal
  #' @importFrom lubridate weeks
  #'
  
  if (is.factor(year)) year <- as.character(year)
  
  min_year <- min(year)
  wk_lab <- as.Date(paste0(min_year, "-01-01"))
  
  wk_lab <- wk_lab + lubridate::weeks(breaks)
  
  format(wk_lab, "%b %d")
}

date_title <- function(plot, filter_date, date_value) {
  #' Add date to ggplot title
  #' @keywords internal
  #' @export
  #' @param plot ggplot2 plot object to add new title to.
  #' @param filter_date The \code{filter_date} parameter in function.
  #' @param date_value The values used to filter the data table used in plot.
  #' @return A plot with the year, month, or year-month included in tittle.

  fv_len <- length(date_value)

  if (filter_date == "year") {
    if (fv_len == 1) {
      plot$labels$subtitle <- paste0("Year: ", date_value)
    } else {
      plot$labels$subtitle <- paste0(
        "Year: ",
        date_value[1], "-", date_value[fv_len]
      )
    }
  } else if (filter_date == "month") {
    
    month <- switch(date_value, "1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr",
                    "5" = "May", "6" = "Jun", "7" = "Jul", "8" = "Aug", "9" = "sep", 
                    "10" = "Oct", "11" = "Nov", "12" = "Dec")

    if (fv_len == 1) {
      plot$labels$subtitle <- paste0(month)
    } else {
      plot$labels$subtitle <- paste0(month[1], "-", month[fv_len])
    }
  } else if (filter_date == "year-month") {
    y_num <- date_value[[1]]
    m_num <- date_value[[2]]
    y_len <- length(y_num)
    m_len <- length(m_num)
    
    month <- switch(m_num, "1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr",
                    "5" = "May", "6" = "Jun", "7" = "Jul", "8" = "Aug", "9" = "sep", 
                    "10" = "Oct", "11" = "Nov", "12" = "Dec")

    if (y_len == 1 & m_len == 1) {
      plot$labels$subtitle <- paste(month, y_num)
    } else if (y_len > 1 & m_len == 1) {
      plot$labels$subtitle <- paste0(month, " ", y_num[1], "-", y_num[y_len])
    } else if (y_len == 1 & m_len > 1) {
      plot$labels$subtitle <- paste0(month[1], "-", month[m_len], " ", y_num)
    } else if (y_len > 1 & m_len > 1) {
      plot$labels$subtitle <- paste0(
        month[1], "-", month[m_len], " ",
        y_num[1], "-", y_num[y_len]
      )
    } else {
      warning("Invalid year-month length.")
    }
  }

  plot
}

deparse_name <- function(dat) {
  #' Deparse a data table for log 
  #'
  #'@param dat A dataframe object or string to deparse for \code{log_call} function. 
  #'@export
  #'@keywords internal
  
  if (is.character(dat)) {
    dat
  } else if (is.object(dat)) {
    deparse(substitute(dat))
  }
}
#Find project directories

list_dirs <- function(path=".", pattern=NULL, all.dirs=FALSE,
                      full.names=FALSE, ignore.case=FALSE) {
  #' Find directories
  #' @param path file path
  #' @param pattern Use to define specific dirs to search for
  #' @param all.dirs Logical
  #' @param full.names Logical, Full name or directory name
  #' @param ignore.case Logical
  #' @export
  #' @keywords internal
  #' 
  # use full.names=TRUE to pass to file.info
  all <- list.files(path, pattern, all.dirs,
                    full.names=TRUE, recursive=FALSE, ignore.case)
  dirs <- all[file.info(all)$isdir]
  # determine whether to return full names or just dir names
  if(isTRUE(full.names))
    return(dirs)
  else
    return(basename(dirs))
}

simpleCap <- function(x) {
  #' Convert case to upper
  #' @param x Variable
  #' @keywords internal
  #' @export
  
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

## ----Shiny util functions-----
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


  requireNamespace("ggplot2")

  dataset <- dat
  x.name <- x
  if (is.numeric(dataset[, x]) == T) {
    # Begin outlier check
    dataset$val <- 1:nrow(dataset)
    if (dat.remove == "none") {
      dataset$Points <- "Kept"
    } else {
      if (dat.remove == "5_95_quant") {
        dataset$Points <- ifelse(dataset[, x] < stats::quantile(dataset[, x], 0.95, na.rm = TRUE) &
          dataset[, x] > stats::quantile(dataset[, x], 0.05, na.rm = TRUE), "Kept", "Removed")
      } else if (dat.remove == "25_75_quant") {
        dataset$Points <- ifelse(dataset[, x] < stats::quantile(dataset[, x], 0.75, na.rm = TRUE) &
          dataset[, x] > stats::quantile(dataset[, x], 0.25, na.rm = TRUE), "Kept", "Removed")
      } else if (dat.remove == "mean_2SD") {
        dataset$Points <- ifelse(dataset[, x] < (mean(dataset[, x], na.rm = T) + 2 *
          stats::sd(dataset[, x], na.rm = T)) &
          dataset[, x] > (mean(dataset[, x], na.rm = T) - 2 * stats::sd(dataset[, x], na.rm = T)), "Kept", "Removed")
      } else if (dat.remove == "median_2SD") {
        dataset$Points <- ifelse(dataset[, x] < (stats::median(dataset[, x], na.rm = T) + 2 * stats::sd(dataset[, x], na.rm = T)) &
          dataset[, x] > (stats::median(dataset[, x], na.rm = T) - 2 * stats::sd(dataset[, x], na.rm = T)), "Kept", "Removed")
      } else if (dat.remove == "mean_3SD") {
        dataset$Points <- ifelse(dataset[, x] < (mean(dataset[, x], na.rm = T) + 3 * stats::sd(dataset[, x], na.rm = T)) &
          dataset[, x] > (mean(dataset[, x], na.rm = T) - 3 * stats::sd(dataset[, x], na.rm = T)), "Kept", "Removed")
      } else if (dat.remove == "median_3SD") {
        dataset$Points <- ifelse(dataset[, x] < (stats::median(dataset[, x], na.rm = T) + 3 * stats::sd(dataset[, x], na.rm = T)) &
          dataset[, x] > (stats::median(dataset[, x], na.rm = T) - 3 * stats::sd(dataset[, x], na.rm = T)), "Kept", "Removed")
      }
    } # End Outlier mod

    mytheme <- theme(
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size = 11),
      axis.title = element_text(size = 11)
    )

    # Hist
    ## Plot 2!
    if (x.dist == "normal") {
      arg.return <- stat_function(
        fun = dnorm, colour = "blue",
        args = list(mean = mean(dataset[dataset$Points == "Kept", x], na.rm = TRUE), sd = sd(dataset[dataset$Points == "Kept", x], na.rm = TRUE))
      )
    } else if (x.dist == "lognormal") {
      # lognormal
      arg.return <- stat_function(
        fun = dlnorm, colour = "blue",
        args = list(mean = mean(log(dataset[dataset$Points == "Kept", x]), na.rm = TRUE), sd = sd(log(dataset[dataset$Points == "Kept", x]), na.rm = TRUE))
      )
    } else if (x.dist == "exponential") {
      # Exponential
      arg.return <- stat_function(
        fun = dexp, colour = "blue",
        args = list(rate = 1 / mean(dataset[dataset$Points == "Kept", x], na.rm = TRUE))
      )
    } else if (x.dist == "weibull") {
      # Weibull
      arg.return <- stat_function(
        fun = dweibull, colour = "blue",
        args = list(
          shape = 1.2 / sqrt(var(log(dataset[dataset$Points == "Kept", x]), na.rm = TRUE)),
          scale = mean(dataset[dataset$Points == "Kept", x], na.rm = TRUE) + 0.572 / (1.2 / sqrt(var(log(dataset[dataset$Points == "Kept", x]), na.rm = TRUE)))
        )
      )
    } else if (x.dist == "poisson") {
      # Poisson
      arg.return <- stat_function(
        fun = dpois, colour = "blue",
        args = list(lambda = mean(dataset[dataset$Points == "Kept", x], na.rm = TRUE))
      )
    } else if (x.dist == "negative binomial") {
      # Negative Binomial
      arg.return <- stat_function(
        fun = dnbinom, colour = "blue",
        args = list(mean(dataset[dataset$Points == "Kept", x], na.rm = TRUE)^2 / (var(dataset[dataset$Points == "Kept", x], na.rm = TRUE) - mean(dataset[dataset$Points == "Kept", x], na.rm = TRUE)),
          mu = mean(dataset[dataset$Points == "Kept", x], na.rm = TRUE)
        )
      )
    }

    # Plot3
    # Probability plot
    quants <- seq(0, 1, length = length(dataset[dataset$Points == "Kept", x]) + 2)[2:(length(dataset[dataset$Points == "Kept", x]) + 1)]
    # normal
    if (x.dist == "normal") {
      fit_quants <- stats::qnorm(quants, mean(dataset[dataset$Points == "Kept", x], na.rm = TRUE), sd(dataset[dataset$Points == "Kept", x], na.rm = TRUE))
    } else if (x.dist == "lognormal") {
      # lognormal
      fit_quants <- stats::qlnorm(quants, mean = mean(log(dataset[dataset$Points == "Kept", x]), na.rm = TRUE), sd = sd(log(dataset[dataset$Points == "Kept", x]), na.rm = TRUE))
    } else if (x.dist == "exponential") {
      # Exponential
      fit_quants <- stats::qexp(quants, rate = 1 / mean(dataset[dataset$Points == "Kept", x], na.rm = TRUE))
    } else if (x.dist == "weibull") {
      # Weibull
      fit_quants <- stats::qweibull(quants,
        shape = 1.2 / sqrt(var(log(dataset[dataset$Points == "Kept", x]), na.rm = TRUE)),
        scale = mean(dataset[dataset$Points == "Kept", x], na.rm = TRUE) + 0.572 / (1.2 / sqrt(var(log(dataset[dataset$Points == "Kept", x]), na.rm = TRUE)))
      )
    } else if (x.dist == "poisson") {
      # Poisson
      fit_quants <- stats::qpois(quants, lambda = mean(dataset[dataset$Points == "Kept", x], na.rm = TRUE))
    } else if (x.dist == "negative binomial") {
      # Negative Binomial
      fit_quants <- stats::qnbinom(quants,
        size = mean(dataset[dataset$Points == "Kept", x], na.rm = TRUE)^2 / (var(dataset[dataset$Points == "Kept", x], na.rm = TRUE) - mean(dataset[dataset$Points == "Kept", x], na.rm = TRUE)),
        mu = mean(dataset[dataset$Points == "Kept", x], na.rm = TRUE)
      )
    }

    data_quants <- stats::quantile(as.numeric(dataset[dataset$Points == "Kept", x]), quants, na.rm = TRUE)
    # create Q-Q plot
    temp <- data.frame(fit_quants, data_quants)
    p3 <- ggplot(temp, aes(x = fit_quants, y = data_quants)) +
      geom_point(shape = 1) +
      geom_abline() +
      labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = paste("Q-Q plot of", x.dist, "fit against data")) +
      mytheme

    if (plot_type == "1") {
      return(dataset)
    } else if (plot_type == "2") {
      suppressWarnings(arg.return)
    } else if (plot_type == "3") {
      suppressWarnings(return(temp))
    }
    # Put it all together
    # fig <- suppressWarnings(ggpubr::ggarrange(p1, p2, p3 , ncol = 2, nrow = 2))
    # labels = c("A", "B", "C"),
    # fig <- ggpubr::annotate_figure(fig, top = ggpubr::text_grob(paste("Plots for ", x, " with ", x.dist,
    #                                                                  " distribution and data removed based on '", dat.remove,
    #                                                                  "'. \nBlue: included points   Red: removed points"), size = 10))
  } else {
    # Actions to take if data is not numeric
    print("Data is not numeric. Plots not generated.")
  }
}

quietly_test <- function(.f) {
  #' quietly_test
  #' capture console messages if exist and print to shiny app
  #' @param .f function name
  #' @export
  #' @keywords internal
  #' @importFrom purrr quietly safely
  #

  fun1 <- purrr::quietly(.f)
  fun <- purrr::safely(fun1)
  function(...) {
    res <- fun(...)

    if (!is.null(res$error)) { # safely output
      showNotification(res$error$message, duration = 10, type = "error")
      return(res$result)
    }
    res <- res$result # quietly output
    if (!is.null(res$warnings) && length(res$warnings) > 0) {
      lapply(unique(res$warnings), showNotification, duration = 10, type = "warning")
    }
    return(res$result)
  }
}

deleteButtonColumn <- function(df, id, ...) {
#' A column of delete buttons for each row in the data frame for the first column
#'
#' @param df data frame
#' @param id id prefix to add to each actionButton. The buttons will be id'd as id_INDEX.
#' @return A DT::datatable with escaping turned off that has the delete buttons in the first column 
#'   and \code{df} in the other function to create one action button as string
#' @keywords internal
#' @export

  f <- function(i) {
    # https://shiny.rstudio.com/articles/communicating-with-js.html
    as.character(actionButton(paste(id, i, sep="_"), label = NULL, icon = icon('trash'),
                              onclick = 'Shiny.setInputValue(\"deletePressed\",  this.id, {priority: "event"})'))
  }
  
  deleteCol <- unlist(lapply(seq_len(nrow(df)), f))
  
  # Return a data table
  DT::datatable(cbind(delete = deleteCol, df),
                # Need to disable escaping for html as string to work
                escape = FALSE,
                options = list(
                  # Disable sorting for the delete column
                  columnDefs = list(list(targets = 1, sortable = FALSE))
                ))
}
#Check if project folder exists. If not. Add it.

parseDeleteEvent <- function(idstr) {
  #' Extracts the row id number from the id string
#' @param idstr the id string formated as id_INDEX
#' @return INDEX from the id string id_INDEX
#' @export
#' @keywords internal

  res <- as.integer(sub(".*_([0-9]+)", "\\1", idstr))
  if (! is.na(res)) res
}

find_lon <- function(dat) {
 #' Find columns that may be longitude data
 #' @param dat Dataset to search over
 #' @keywords internal
 #' @export
 
  cols <- colnames(dat)
  lon_match <- stringi::stri_count_regex(cols, '(?=LON|Lon|lon)')
  lon_cols <- which(lon_match %in% max(lon_match))
  
  cols[lon_cols]
}

find_lat <- function(dat) {
  #' Find columns that may be latitude data
  #' @param dat Dataset to search over
  #' @keywords internal
  #' @export
  
  cols <- colnames(dat)
  lat_match <- stringi::stri_count_regex(cols, '(?=LAT|Lat|lat)')
  lat_cols <- which(lat_match %in% max(lat_match))
  
  cols[lat_cols]
}

find_lonlat <- function(dat) {
  #' Find columns that may be longitude or latitude data
  #' @param dat Dataset to search over
  #' @keywords internal
  #' @export
  
  grep("lon|lat", colnames(dat), ignore.case = TRUE, value = TRUE)
}

numeric_cols <- function(dat, out = "names") {
  #' Find numeric columns in data frame
  #' 
  #' @param dat MainDataTable, dataframe, or list to check.
  #' @param out Whether to return the column \code{"names"} (the default) or a logical vector 
  #'   (\code{"logical"}).
  #' @export
  #' @keywords internal
  #' @examples 
  #' \dontrun{
  #' numeric_cols(pollockMainDataTable)
  #' }
  
  num_cols <- vapply(dat,  FUN = is.numeric, FUN.VALUE = logical(1))
  
  if (out == "names") names(num_cols[num_cols])
  else if (out == "logical") num_cols
}

category_cols <- function(dat, out = "names") {
  #' Find categorical columns in data frame
  #' 
  #' @param dat MainDataTable, dataframe, or list to check.
  #' @param out Whether to return the column \code{"names"} (the default) or a logical vector 
  #'   (\code{"logical"}).
  #' @export
  #' @keywords internal
  #' @examples 
  #' \dontrun{
  #' category_cols(pollockMainDataTable)
  #' }
  
  class_list <- lapply(dat, class) 
  cat_class <- c("factor", "character", "integer", "logical")
  cat_cols <- vapply(class_list, function(x) any(cat_class %in% x), FUN.VALUE = logical(1))
  
  if (out == "names") names(cat_cols[cat_cols])
  else if (out == "logical") cat_cols
}

date_cols <- function(dat, out = "names") {
  #' Find columns that can be converted to Date class
  #' 
  #' @param dat MainDataTable or dataframe to check.
  #' @param out Whether to return the column \code{"names"} (the default) or a logical vector 
  #'   (\code{"logical"}).
  #' @export
  #' @keywords internal
  #' @importFrom stringr str_trim str_remove
  #' @importFrom purrr map_lgl
  #' @importFrom rlang expr
  #' @importFrom lubridate mdy dmy ymd ydm dym
  #' @examples 
  #' \dontrun{
  #' date_cols(pollockMainDataTable) # returns column names
  #' date_cols(pollockMainDataTable, "logical")
  #' }
  
  # named logical vector to preserve col order
  date_lgl <- logical(ncol(dat))
  names(date_lgl) <- names(dat)
  
  # lubridate functions to test for
  date_funs <- list(lubridate::mdy, lubridate::dmy, lubridate::ymd, 
                    lubridate::ydm, lubridate::dym)
  
  date_helper <- function(dates, fun) {
    
    dates <- stringr::str_trim(dates)
    
    # remove time info
    dates <- stringr::str_remove(dates, "\\s\\d{2}:\\d{2}:\\d{2}$")
    
    out <- rlang::expr(!all(is.na(suppressWarnings((!!fun)(!!dates)))))
    
    eval(out)
  }
  
  # apply each function to date vector
  date_apply <- function(dates) {
    
    any(purrr::map_lgl(date_funs, function(fun) date_helper(dates, fun)))
  }
  
  # number of rows to check 
  nr <- nrow(dat)
  
  if (nr > 1000) dat_slice <- 1000
  else dat_slice <- round(nr * .5)
  
  # find cols that can be successfully converted to date
  # numeric cols excluded for efficiency and to prevent false positives  
  date_cols <- purrr::map_lgl(dat[!numeric_cols(dat, "logical")][seq_len(dat_slice), ], date_apply)
  
  date_cols <- date_cols[date_cols]
  
  if (out == "names") names(date_cols)
  else if (out == "logical") {
    date_lgl[names(date_cols)] <- date_cols # replace w/ TRUE
    date_lgl
  }
}

find_dev <- function(x, y){
#'  
#' Find how many standard deviations point \code{x} is from mean of \code{y}.
#' @param x value to check
#' @param y data vector. Must be numeric
#' @export
#' @keywords internal
  
  
  i <- 1  
  dv <- sd(y, na.rm=TRUE)*i
  m <- mean(y, na.rm=TRUE)
  if(x>0){
    while(x > (m+dv)){
      i <- i+1
      dv <- sd(y, na.rm=TRUE)*i
    }
  } else {
    while(x < (m-dv)){
      i <- i+1
      dv <- sd(y, na.rm=TRUE)*i
    }
  }
  return(i)
}


  

# shiny_running = function () {
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
# }

