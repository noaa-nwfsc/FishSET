# File directory functions ----
choose_directory = function() {
  #' Choose directory
  #' @export
  #' @keywords internal
  
  # Note: these methods won't add trailing slashes to dir path
  title = 'Identify location of FishSET folder and save output'
  
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    
    out <- rstudioapi::selectDirectory(caption = title)
    
  } else if (requireNamespace("utils", quietly = TRUE) & .Platform$OS.type == "windows") {
  
    out <- utils::choose.dir(caption = title)
    
  } else if (requireNamespace("tcltk", quietly = TRUE)) {
    
    out <- tcltk::tk_choose.dir(caption = title)
    
  } else {
    
    out <- readline('Please enter directory path: ') # Console method
    
    if (!dir.exists(out)) {
      
      stop("Directory does not exist: ", out, call. = FALSE)
    }
  }
  
  if (length(out) == 0 || is.na(out)) {
    
    stop("Invalid directory. Function stopped.", call. = FALSE)
  }
  
  return(out)
}

loc <- function() {
    #' Define FishSETFolder location
    #' @keywords internal
    #' @export
    
    newdir <- choose_directory()
    
    # If user is selecting the location to create the FSF
    # TODO: check that chosen dir ends in 'FishSETFolder'
    if (grepl('FishSETFolder', as.character(newdir)) == FALSE) {
      
      newdir <- paste0(newdir, '/FishSETFolder/')
      # this will silently fail if dir already exists
      dir.create(file.path(newdir), showWarnings = FALSE)
    } 
    # otherwise, return the loc of the existing FSF
    return(newdir)
  }

locproject <- function() {
  #' Define projects folder location
  #'
  #'@export
  #'@keywords internal
  # TODO: change folderpath to fsfolderpath, or FSFolderpath?
  fp_exists <- exists("folderpath", where = ".GlobalEnv")
  
  if (fp_exists) {
    
    proj_dir <- get("folderpath", envir = as.environment(1L))
    
    # Possible that user altered folderpath, check if valid
    if (!dir.exists(proj_dir)) {
      
      stop("The folder in 'folderpath' does not exist. Delete 'folderpath' or run ", 
           "update_folderpath() to select the location of the FishSET Folder.", 
           call. = FALSE)
    }
    
  } else {
    # have user select folder
    proj_dir <- loc()
  }
  
  # add trailing slash
  proj_dir <- paste0(normalizePath(proj_dir), "/")
  
  # create the new path to FishSETFolder
  
  if (!fp_exists) {
    
    pos <- 1
    envir = as.environment(pos)
    assign('folderpath', proj_dir, envir = envir)
  }
  
  return(proj_dir)
}

update_folderpath <- function() {
  #' Update FishSETFolder location
  #' 
  #' Select the location of the FishSET folder. This can be helpful if 
  #' switching between different FishSET folders or if 'folderpath' is 
  #' inaccurate.  
  #' 
  #' @export
  #' @keywords internal
  
  fs_path <- loc()
  
  proj_dir <- paste0(normalizePath(fs_path), "/") # add trailing slash
  
  pos <- 1
  envir = as.environment(pos)
  assign('folderpath', proj_dir, envir = envir)
  fs_path
}

project_exists <- function(project) {
  #' Check if project exists
  #' 
  #' @param project Project name. 
  #' 
  #' @export
  #' @keywords internal
  
  # Q: what if folderpath doesn't exist or is faulty?
  if (!is.null(project)) {
    
    projdir <- paste0(locproject(), project)
    dir.exists(projdir)
    
  } else FALSE
}

#Check if project folder exists
#If not, create the folder

check_proj <- function(project = NULL) {
  #' Check for project folder. Create folders if required
  #' @param project Project name
  #' @keywords internal
  #' @export
  
  if (!is.null(project)) {
      
    
    appDir <- system.file( "report", 'report_template.Rmd', package = "FishSET")
    # check if projects folder exists
    if (!file.exists(locproject())) {
      
      dir.create(file.path(locproject()), showWarnings = FALSE)
    }
    
    proj_dir <- paste0(locproject(), '/', project) 
    
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
      dir.create(file.path(paste0(proj_dir, '/data/spat')), showWarnings = FALSE)
      
      #doc (report)
      dir.create(file.path(paste0(proj_dir, '/doc')), showWarnings = FALSE)
      file.copy(appDir, paste0(proj_dir, "/doc/"))
      
      create_proj_settings(project)
      
      #MapViewer
      fs_mv <- system.file("MapViewer", package = "FishSET")
      file.copy(from = fs_mv, to = file.path(proj_dir), recursive = TRUE)
      
      
    } else {
      #Cases where the root folder exists but the subfolders have been deleted.
      if (!file.exists(paste0(locproject(),  project, "/src"))) {
        
        dir.create(file.path(paste0(proj_dir, '/src')), showWarnings = FALSE)
      }
      
      if (!file.exists(paste0(locproject(),  project, "/output"))) {
        
        dir.create(file.path(paste0(proj_dir, '/output')), showWarnings = FALSE)
      }
      
      if (!file.exists(paste0(locproject(),  project, "/fishset_db.sqlite"))) {
        
        fishset_db <- DBI::dbConnect(RSQLite::SQLite(), paste0(proj_dir, '/fishset_db.sqlite'))
        on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
      }
      
      if (!file.exists(paste0(locproject(), project, "/data"))) {
        
        dir.create(file.path(paste0(proj_dir, '/data')), showWarnings = FALSE)
        dir.create(file.path(paste0(proj_dir, '/data/spat')), showWarnings = FALSE)
      }
      
      if (!file.exists(paste0(locproject(),  project, "/doc"))) {
        
        dir.create(file.path(paste0(proj_dir, '/doc')), showWarnings = FALSE)
        file.copy(appDir, paste0(locproject(), project, "/doc/report_template.Rmd"))
      }
      
      if (!file.exists(paste0(locproject(),  project, "/MapViewer"))) {
        
        fs_mv <- system.file("MapViewer", package = "FishSET")
        file.copy(from = fs_mv, to = file.path(proj_dir), recursive = TRUE)
      }
      
      if (!file.exists(paste0(locproject(), project, "/doc/report_template.Rmd"))) {
        
        file.copy(appDir, paste0(locproject(), project, "/doc/report_template.Rmd"))
      }
    }
    
  } else {
    # TODO: see if changing to stop() would break anything (thinking about app)
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
    
    unlink(paste0(locproject(),  project), recursive = TRUE)
    message('Project ', project, ' deleted')
  
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
  
  if(!is.null(project)){
      paste0(locproject(),  project, "/fishset_db.sqlite")
  } else {
    paste0(locproject(), "/fishset_db.sqlite")
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
    warning('Project name must be supplied.')
  } else {
      paste0(locproject(), project, "/src/")
    } 
}

locoutput <- function(project) {
  #' Output location
  #' @param project Project name
  #' @keywords internal
  #' @export
 
  if(is.null(project)){
    warning('Project name must be supplied.')
  } else {
  
  
      paste0(locproject(), project, "/output/")
  
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
    warning('Project name must be supplied.')
  } else {
      paste0(locproject(), project, "/MapViewer/")
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
    warning('Project name must be supplied.')
  } else {
      paste0(locproject(), project, "/data/")
  }
}

loc_meta <- function(project) {
  #' Define source location for meta file
  #' @param project Project name.
  #' @keywords internal
  #' @export
  #' 
  if(is.null(project)){
    warning('Project name must be supplied.')
  } else {
  paste0(locproject(), project, "/doc/meta_log.json")
  }
}


loc_doc <- function(project) {
  #' Define source location for doc folder
  #' Returns the location of the doc folder
  #' @param project Project name
  #' @keywords internal
  #' @export
  #' @details if loc2 is not in the working environment, then the default location is use
  
  if(is.null(project)){
    warning('Project name must be supplied.')
  } else {
    paste0(locproject(), project, "/doc/")
  }
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

# ----

file_nm_check <- function(file_nm) {
  #' Check name or file name
  #' 
  #' Check a name or file name for illegal characters. 
  #' @param file_nm String, the file name or name to check. 
  #' @keywords internal
  #'
  
  illegal_chars <- c('#', '<', '>', '#', '\\$', '\\+', '%', '!', '`', '&', '\\*','\'', 
                     '\"', '\\|', '\\{', '\\}', '\\?', '=', '/', '\\\\',':', '@')
  
  ill_print <- gsub("\\\\", "", illegal_chars)
  ill_print[20] <- "\\"
  ill_print <- paste(ill_print, collapse = ", ")
  
  ill_chars <- paste0(illegal_chars, collapse = "|")
  
  if (any(grepl(ill_chars, file_nm))) {
    
    stop("Invalid name or file name. The following characters are not allowed: ", 
         ill_print, call. = FALSE)
  }
}


# QAQC functions ----
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
    if (is.character(x) && nchar(trimws(x)) == 0) {
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
  #' @importFrom stats na.omit
  #' @export
  
  if (inherits(x, "try-error")) return(TRUE)
  if (is.null(x))  return(TRUE)
  if (length(x) == 0) return(TRUE)
  if (all(is.na(x))) return(TRUE)
  if (is.character(x) && !any(nzchar(stats::na.omit(x)))) return(TRUE)
  if (is.logical(x) && !any(stats::na.omit(x))) return(TRUE)
  
  return(FALSE)
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


unique_rows <- function(dat) {
  #' Return unique rows
  #' 
  #' Check whether all rows are in a dataset are unique. If not, returns only the
  #' unique rows. 
  #' 
  #' @param dat Data to check for non-unique rows.
  #' @keywords internal
  #' @importFrom dplyr distinct
  #' @examples 
  #' \dontrun{
  #' dat <- unique_rows(dat)
  #' }
  
  if (nrow(dat) != nrow(dplyr::distinct(dat))) {
    
    message('Duplicate rows found and removed.')
    dat <- dplyr::distinct(dat)
  }
  
  dat
}


empty_vars <- function(dat, remove = TRUE) {
  #' Check for empty variables
  #' 
  #' Detects variables that contain all \code{NA}s and removes them if 
  #' \code{remove = TRUE}. 
  #' 
  #' @param dat The data.frame to check. 
  #' @param remove Logical, whether to remove empty variables. 
  #' @keywords internal
  #' @export
  #' @examples
  #' \dontrun{
  #' dat <- empty_vars(dat)
  #' }
  
  empty_ind <- qaqc_helper(dat, is_value_empty)
  
  if (any(empty_ind)) {
    
    empty_vars <- names(dat[empty_ind])
    
    if (remove == TRUE) {
      
      dat <- dat[!empty_ind]
      
      warning("The following variables were empty and removed: ", 
              paste(names(empty_ind[empty_ind]), collapse = ", "), call. = FALSE)
      
    } else {
      
      warning("the following variables are empty, consider removing: ", 
              paste(empty_vars, collapse = ", "), call. = FALSE)
    }
  } 
  
  dat
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

polyval <- function (coef, z) {
  #' polyval
  #' @param coef coeffient
  #' @param z numeric
  #' @export
  #' @keywords internal
  
  lz <- length(z)
  if (!lz) 
    return(numeric(0))
  n <- length(coef)
  if (!n) {
    z[] <- 0
    return(z)
  }
  if (!(mode(coef) == "numeric") && !(mode(coef) == "complex")) 
    stop("Argument 'coef' must be a real or complex vector.")
  d_z <- dim(z)
  dim(z) <- lz
  y <- outer(z, (n - 1):0, "^") %*% coef
  dim(y) <- d_z
  return(y)
}
# ----

date_parser <- function(dates, args=NULL) {
  #' Parse date variable
  #' @param dates Variable containing dates
  #' @keywords internal
  #' @export
  #' @importFrom lubridate dym ymd myd ydm dmy mdy

  dates <- trimws(dates)
  dates <- sub(" .*", "\\1", dates)
  if (!all(is.na(suppressWarnings(lubridate::mdy(dates))))) {
    lubridate::mdy(dates, args)
  } else if (!all(is.na(suppressWarnings(lubridate::dmy(dates))))) {
    lubridate::dmy(dates, args)
  } else if (!all(is.na(suppressWarnings(lubridate::ymd(dates))))) {
    lubridate::ymd(dates, args)
  } else if (!all(is.na(suppressWarnings(lubridate::ydm(dates))))) {
    lubridate::ydm(dates, args)
  } else if (!all(is.na(suppressWarnings(lubridate::myd(dates))))) {
    lubridate::myd(dates, args)
  } else if (!all(is.na(suppressWarnings(lubridate::dym(dates))))) {
    lubridate::dym(dates, args)
  } else {
    stop("Date format not recognized. Format date before proceeding.", call. = FALSE)
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
    stop("Date format not recognized.", call. = FALSE)
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
  
  if (is.character(dat)) {
    
    if (is.null(dat) | table_exists(dat, project) == FALSE) {
      
      print(project_tables(project))
      stop(paste(dat, "not defined or does not exist. Consider using one of the",
                 "tables listed above that exist in the database."))
      
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

parse_data_name <- function(dat, type, project) {
  
  #' Parse data name for logging 
  #' 
  #' @param dat Data table to be parsed. 
  #' @param type String, the data type: "main", "aux", "grid", "port", or "spat".
  #' @param project project name. 
  #' @importFrom shiny isRunning
  #' @importFrom rlang caller_env
  #' @keywords internal
  #' @details If called while the shiny app is running, the data table name is pulled from 
  #'   the FishSET project settings file. Otherwise, the data table from the caller 
  #'   environment is used. 
  #' @export
  
  
  if (shiny::isRunning()) {

    p_set <- get_proj_settings(project)
    dat <- p_set$tables[[type]]

  } else {

    if (!is.character(dat)) {

      if (type == "main") {

        dat <- deparse(substitute(dat, rlang::caller_env()))

      } else if (type == "aux") {

        dat <- deparse(substitute(aux, rlang::caller_env()))

      } else if (type == "grid") {

        dat <- deparse(substitute(grid, rlang::caller_env()))

      } else if (type == "port") {

        dat <- deparse(substitute(port, rlang::caller_env()))

      } else if (type == "spat") {

        dat <- deparse(substitute(spat, rlang::caller_env()))

      } else if (type == "outsample") {

        dat <- deparse(substitute(outsample, rlang::caller_env()))
      }
    }
  }
  
  dat
}

data_upload_helper <- function(dat, type, ...) {
  #' Upload data from file, FishSET DB, or working environment
  #' 
  #' Helper function that can read data from file, from FishSET DB, or a dataframe
  #' in the working environment. Used for data upload functions: 
  #' \code{load_maindata}, \code{load_port}, \code{load_aux}, \code{load_grid},
  #' \code{load_spatial}.
  #' 
  #' @param dat Reference to a dataframe. This can be a filepath, the name of an
  #'   existing FishSET table, or a dataframe object in the working environment.
  #' @param type The type of data to upload. Options include \code{"main"}, 
  #' \code{"port"}, \code{"grid"}, \code{"aux"}, and \code{"spat"}.
  #' @param ... Additional arguments passed to \code{\link{read_dat}}.
  #' @importFrom utils glob2rx
  #' @examples 
  #' \dontrun{
  #' dataset <- data_upload_helper(dat, type = "main")
  #' }
  
  if (is.character(dat)) {
    
    if (file.exists(dat)) {
      
      dataset <- read_dat(dat, ...)
      
    } else {
      
      # TODO: for type = spat, copy from spatial data folder
      
      type_vec <- 
        switch(type, 
               "main" = c("main table", "raw main table"),
               "port" = c("port table", "raw port table"), 
               "grid" = c("grid table", "raw grid table"),
               "aux" = c("aux table", "raw aux table"),
               "spat" = c("spat table", "raw spat table"),
               "outsample" = c("outsample table", "raw outsample table"))
      
      # check if table exists in FishSET DB
      f_tabs <- fishset_tables() # contains table, project, and type
      
      f_tabs <- subset(f_tabs, type %in% type_vec)
      
      main_tabs <- f_tabs$table
      
      if (dat %in% main_tabs) {
        
        dat_string <- utils::glob2rx(dat)
        
        f_proj <- f_tabs$project[grepl(dat_string, main_tabs)]
        
        dataset <- table_view(dat, project = f_proj)
        
      } else { # table/filepath not found
        
        if (!is_value_empty(file_ext(dat))) { # contains a filepath extension
          
          stop("File not found.", call. = FALSE)
          
        } else { # assume user is referencing a FishSET table
          
          stop("FishSET table not found. Run fishset_tables() to see all FishSET tables.", 
               call. = FALSE)
        }
      }
    }
    
  } else {
    
    if (is.data.frame(dat)) {
      
      dataset <- dat
      
    } else {
      
      stop("'dat' not recognized. 'dat' must be a filepath, a FishSET DB table, ",
           "or a dataframe object in working environment.", call. = FALSE)
    }
  }
  
  dataset
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


name_check <- function(dat, names, repair = FALSE) {
  #' Check for unique and syntatcic column names
  #' 
  #' Used for creating new columns.
  #' 
  #' @param dat Dataset that will contain new columns. 
  #' @param names New names to be added to dataset.
  #' @param repair Logical, whether to return repaired column names (\code{repair = TRUE})
  #'   or just check for unique column names (\code{repair = FALSE}).
  #' @importFrom vctrs vec_as_names
  #' @details \code{name_check()} first checks to see if new column names are unique
  #'   and returns an error if not. When \code{repair = TRUE}, \code{name_check()} 
  #'   will check for unique column names and returns new column names that are
  #'   unique and syntactic (see \code{\link[vctrs]{vec_as_names}} for details).
  #' 

  if (is_value_empty(names)) {
    
    stop("Name is missing.", call. = FALSE)
  }
  
  # TODO: Check for case-insensitive names (SQLite requirement?)
  # check that names are unique, returns error if not
  d_names <- colnames(dat)
  dup_names <- names %in% d_names
  
  if (any(dup_names)) {
    
    stop("Duplicate names found: ", paste(names[dup_names], collapse = ", "), call. = FALSE)
  }
  # fallback check
  vctrs::vec_as_names(names = c(colnames(dat), names), repair = "check_unique")
  
  if (repair) {
    
    # repair names
    # out <- vctrs::vec_as_names(names = c(colnames(dat), names), repair = "universal")
    # 
    # o_len <- length(out)
    # n_len <- length(names)
    # n_ind <- (o_len - (n_len - 1)):o_len
    # 
    # out[n_ind] # return repaired nms
    vctrs::vec_as_names(names = names, repair = "universal")
  }
}


column_check <- function(dat, cols) {
  #' Check that column names exists
  #' 
  #' Check whether user supplied column names exist in the data.
  #' 
  #' @param dat The data used in function.
  #' @param cols String, the column names used in function. 
  #' @keywords internal
  #' 
  ind <- !cols %in% colnames(dat)
  
  if (any(ind)) {
    
    non_cols <- cols[ind]
    msg <- paste("The following columns are not in the dataset:", 
                 paste(non_cols, collapse = ", "))
    stop(msg, call. = FALSE)
  }
}

agg_helper <- function(dataset, value, period = NULL, group = NULL, within_group = NULL, 
                       fun = "sum", count = FALSE, format_tab = "decimal") {
  #' Aggregating function 
  #'
  #'
  #' @param dataset `MainDataTable` to aggregate. 
  #' @param value String, name of variable to aggregate. 
  #' @param period String, name of period variable to aggregate by. Primarily for 
  #'   internal use. Places temporal variables to the right-end of the summary
  #'   table.
  #' @param group String, name of grouping variable(s) to aggregate by.
  #' @param within_group String, name of grouping variable(s) for calculating
  #'   within group percentages. \code{fun = "percent"} and \code{period} or 
  #'   \code{group} are required. 
  #' @param fun String, function name to aggregate by. Also accepts anonymous functions.
  #'   To calculate percentage, set \code{fun = "percent"}; this will return the 
  #'   percent of total when \code{within_group = NULL}. 
  #' @param count Logical, if \code{TRUE} then returns the number of observations by
  #'   \code{period} and/or \code{group}.
  #' @param format_tab String. Options include \code{"decimal"} (default), 
  #' \code{"scientific"}, and \code{"PrettyNum"} (rounds to two decimal places
  #'   and uses commas).
  #' @export
  #' @importFrom dplyr group_by across all_of count summarise ungroup mutate
  #' @examples 
  #' \dontrun{
  #' 
  #' # total catch by port
  #' agg_helper(pollockMainDataTable, value = "OFFICIAL_TOTAL_CATCH_MT", 
  #'            group = "PORT_CODE", fun = "sum")
  #' 
  #' # count permits
  #' agg_helper(pollockMainDataTable, value = "PERMIT", count = TRUE, fun = NULL)
  #' 
  #' # count permits by gear type
  #' agg_helper(pollockMainDataTable, value = "PERMIT", group = "GEAR_TYPE",
  #'            count = TRUE, fun = NULL)
  #'
  #' # percent of total by gear type
  #' agg_helper(pollockMainDataTable, value = "PERMIT", group = "GEAR_TYPE",
  #'            count = TRUE, fun = "percent")
  #'  
  #' # within group percentage          
  #' agg_helper(pollockMainDataTable, value = "OFFICIAL_TOTAL_CATCH_MT", 
  #'            fun = "percent", group = c("PORT_CODE", "GEAR_TYPE"), 
  #'            within_group = "PORT_CODE")
  #' }
  
  . <- NULL

  agg_cols <- unique(c(value, period, group))
  
  column_check(dataset, agg_cols)
    
  calc_perc <- FALSE
  
  # TODO: use !match.fun(fun) instead? 
  if (!is.function(fun)) {
    
    if (count && !is.null(fun) && fun != "percent") {
      
      warning("Argument 'fun' ignored. Performing count.")
    } 
    
    if (!is.null(fun) && fun == "percent") {
      
      calc_perc <- TRUE
      fun <- "sum"
      
    } else {
      
      # stop("Invalid function.", call. = FALSE)
    }
  } 
  
  tab_out <- 
    dataset %>% 
    dplyr::group_by(dplyr::across(.cols = c(dplyr::all_of(period), 
                                            dplyr::all_of(group)))) %>% 
    {
      if (count) {
        
        dplyr::count(., dplyr::across(dplyr::all_of(value)), sort = TRUE)
        
      } else {
        
        dplyr::summarise(., dplyr::across(dplyr::all_of(value), 
                                          .fns = match.fun(fun))) # .names = {.col}_{.fns}?
      }
      
    } %>% dplyr::ungroup()
  
  if (calc_perc) {
    # TODO: check that a group/period has been used, otherwise within_group doesn't work 
    if (!is.null(within_group)) {
      
      if (is_value_empty(c(group, period))) {
        
        stop("'within_group' requires that 'group' or 'period' be used.", call. = FALSE)
      }
      
      tab_out <- 
        tab_out %>% dplyr::group_by(across(dplyr::all_of(within_group)))
    }
    
    if (count) {
      
      tab_out <-  tab_out %>% dplyr::mutate(perc = n/sum(n) * 100)
      
    } else {
      
      tab_out <-
        tab_out %>% 
        dplyr::mutate(dplyr::across(dplyr::all_of(value), 
                                    .fns = ~ .x/sum(.x) * 100, 
                                    .names = "{.col}_perc"))
      
      cols <- names(tab_out)
      value <- cols[!cols %in% c(group, period)]
    }
  }
  
  if (count) value <- "n"
  
  if (format_tab == "prettyNum") {
    
    pretty_fun <- function(x) prettyNum(x, big.mark = ",", digits = 2)
    
    tab_out <- 
      tab_out %>% 
      dplyr::mutate(dplyr::across(dplyr::all_of(value), 
                                  .fns = pretty_fun))
    
  } else if (format_tab == "scientific") {
    
    pretty_fun <- function(x) formatC(x, format = "e", digits = 2)
    
    tab_out <- 
      tab_out %>% 
      dplyr::mutate(dplyr::across(dplyr::all_of(value), .fns = pretty_fun))
  }
  
  tab_out
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
  #' @importFrom dplyr group_by across mutate ungroup select all_of %>% summarize 
  #' @importFrom purrr map2
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


# change to add_missing_values/levels, or expand_data
expand_data <- function(dataset, project, date = NULL, value, sub_date = NULL,
                        period = NULL, group = NULL, facet_by = NULL, fun = "sum") {
  #' Add missing dates and variable combos to `MainDataTable`.
  #'
  #' @param dataset Object containing `MainDataTable`. 
  #' @param project Name of project. 
  #' @param date String, name of date variable to find missing days.
  #' @param sub_date String, name of date variable to subset by. 
  #' @param period String, name of period variable(s).
  #' @param value String, name of value variable to be aggregated by \code{agg_helper}.
  #' @param group String, name of grouping variable(s).
  #' @param facet_by String, name of variable(s) to be facetted (split). 
  #' @export
  #' @keywords internal
  #' @importFrom dplyr anti_join bind_rows
  #' @details This function expands the data to include missing periods/dates
  #'   and combinations of grouping variables that will be used to aggregate the
  #'   data. Only variables needed to aggregate the data are kept to minimize memory usage.
  #'   If confidentiality checks are turned on, the vessel ID column is included
  #'   as well.

  if (!is.null(c(period, group, facet_by))) {
  
    # convert date col to date type (may be redundant)
    if (!is.null(date)) {
      
      dataset[date] <- 
        lapply(dataset[date], function(x) {
          
          if (any(!(class(x) %in% c("Date", "POSIXct", "POSIXt")))) {
            date_parser(x)
          } else x
        })
    } 
    
    # Find smallest period, then fill missing dates 
    # E.g. no need to include every day if aggregating by year
    per_cols <- unique(c(period, group, facet_by))
    
    if ("cal_date" %in% per_cols) add_per <- "day"
    else if ('weekday' %in% per_cols) add_per <- 'day'
    else if ('weekday_abv' %in% per_cols) add_per <- 'day'
    else if ('day_of_month' %in% per_cols) add_per <- 'day'
    else if ('day_of_year' %in% per_cols) add_per <- 'day'
    else if ("week" %in% per_cols) add_per <- "week"
    else if ("month" %in% per_cols) add_per <- "month"
    else if ('year_month' %in% per_cols) add_per <- 'month'
    else if ('month_year' %in% per_cols) add_per <- 'month'
    else if ("year" %in% per_cols) add_per <- "year"
    else if (is.null(period)) add_per <- NULL
    else stop('invalid period used.')
    
    if (!is.null(add_per)) {
      # create full dates
      from_date <- min(dataset[[date]], na.rm = TRUE)
      to_date <- max(dataset[[date]], na.rm = TRUE)
      full_dates <- seq.Date(from = from_date, to = to_date, by = add_per)
    }

    cols <- unique(c(date, group, facet_by))
    cols_exp <- unique(c(group, facet_by))
    
    # add vessel ID if running confid check
    if (run_confid_check(project)) {
      
      check <- get_confid_check(project)
      
      if (!check$v_id %in% c(cols, value)) {
        cols <- c(cols, check$v_id) # include ves ID in data
      }
    }
    # Remove unnecessary cols
    dataset <- dataset[unique(c(cols, value))]
 
    # list of unique values for each grouping column
    unique_vals <- lapply(dataset[cols_exp], unique)
    
    # add full dates if aggregating by period
    if (!is.null(period)) unique_vals[[date]] <- full_dates
    
    # all combos of full dates and/or unique group values
    unique_vals <- do.call(expand.grid, list(unique_vals))
    # full join, unmatched values will contain NAs
    dataset <- dplyr::full_join(unique_vals, dataset)
    
    if (fun == "sum") {
      # replace NA with zero
      replace_list <- lapply(seq(value), function(x) 0)
      replace_list <- setNames(replace_list, nm = value)
      dataset <- tidyr::replace_na(dataset, replace = replace_list)
    }
  }
  
  dataset
}


sub_date_check <- function(sub_date, date, filter_date, group, facet_by) {
  #' Check subset date variable
  #' 
  #' @param sub_date String, name of date column to subset by.
  #' @param date String, name of date column used in creating period variables.
  #' @param filter_date The type of date filter to apply to the data. 
  #' @param group String, name of group variable(s). Many fleet function allow 
  #'   users to create a year, month, or week variable to group by. If grouping 
  #'   by period and \code{sub_date} and \code{date} are null, the function is 
  #'   stopped. 
  #' @param facet_by String, name of facetting variable(s). Many fleet function allow 
  #'   users to create a year, month, or week variable to facet by. If splitting 
  #'   by period and \code{sub_date} and \code{date} are null, the function is 
  #'   stopped. 
  #' @keywords internal
  #' @returns \code{sub_date}. When used in a function, assign output to \code{sub_date}.
  
  if (!is.null(filter_date) && is.null(sub_date)) {
    
    if (!is.null(date)) sub_date <- date
    else {
      stop("Argument 'sub_date' required when filtering by date.", call. = FALSE)
    }
  }
  
  if (!is.null(facet_by) && any(facet_by %in% c("year", "month", "week"))) {
    if (is.null(sub_date)) {
      if (!is.null(date)) sub_date <- date
      else {
        stop("Spliting by a function-created date variable ('year', ",
             "'month', or 'week') requires a date variable.", call. = FALSE)
      }
    }
  }
  
  if (!is.null(group) && (any(group %in% c("year", "month", "week")))) {
    if (is.null(sub_date)) {
      if (!is.null(date)) sub_date <- date
      else {
        stop("Grouping by a function-created date variable ('year', ",
             "'month', or 'week') requires a date variable.", call. = FALSE)
      }
    }
  }
  
  sub_date
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
  
  if (nrow(dataset) == 0) {
    
    stop("Filtered data table has zero rows. Check filter parameters.", call. = FALSE)
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
        
        stop("Invalid filter expression.", call. = FALSE)
      }
    } 
    
    if (!is_empty(filter_expr)) {
      
      p_expr <- rlang::parse_expr(filter_expr)
      
      rows <- eval(p_expr, envir = dataset)
      
      if (is.logical(rows)) {
        
        dataset <- dataset[rows, ]
        
      } else {
        
        stop("Invalid filter expression.", call. = FALSE)
      }
    }
  
  if (nrow(dataset) == 0) {
    
    stop("Filtered data table has zero rows. Check filter parameters.", call. = FALSE)
  }

  dataset
} 


period_check <- function(period, date) {
  #' Check for valid period
  #' 
  #' @param period String, name of period to create. This is used by fleet functions
  #'   to summarize data by period. 
  #' @param date String, name of date variable used to create period variable.
  #' @keywords internal
  #' @returns A period code used to format \code{date}. Assign output to a variable
  #'   when used inside a function.  
  
  if (!is.null(period)) {
    
    if (is.null(date)) stop("Enter a date variable.", call. = FALSE)
    
    periods <- c("year_month", "month_year", "year", "month", "week", 
                 "weekday", "day_of_month", "day_of_year", "cal_date")
    
    if (period %in% periods == FALSE) {
      
      stop("Invalid period. Please select a valid period name (see documentation for details).",
           call. = FALSE)
      
    } else {
      
      switch(period, year_month = "%Y-%m", month_year = "%m-%Y", year = "%Y",
             month = "%b", week = "%U", weekday = "%a", day_of_month = "%d", 
             day_of_year = "%j", cal_date = NULL)
    }
  }
}


# Plotting helper functions ----
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
  #' @param ... addition arguments passed to \code{\link{write.csv}}.
  #' @keywords internal
  #' @export
  #' @examples
  #' \dontrun{
  #' save_table(count, project, "species_catch")
  #' }
  
  fn <- 
    paste0(locoutput(project=project), project, "_", func_name, "_", Sys.Date(), ".csv")
  write.csv(table, fn, ...)
}

save_ntable <- function(table, project, func_name, id = "num", ...) {
  #' Save list of tables to output folder
  #' @param table List containing tables to save.
  #' @param project project name.
  #' @param func_name Name of function used to create table.
  #' @param id String, id to append to function name. Options include "seq" to 
  #'   save by list entry number or "name" to save by list entry name. 
  #' @param ... addition arguments passed to \code{\link{write.csv}}.
  #' @keywords internal
  #' @export
  #' @examples
  #' \dontrun{
  #' save_ntable(tab_list, project, "species_catch")
  #' }
  
  if (id == "num") vec <- seq_along(table)
  else if (id == "name") vec <- names(table)
  
  lapply(vec, function(x) {
    
    fn <- paste0(func_name,"_", x)
    save_table(table[[x]], project, func_name = fn, ...)
  })
}

save_plot <- function(project, func_name, ...) {
  #' Save plot to output folder
  #' @param project name of project.
  #' @param func_name Name of function used to create plot.
  #' @param ... addition arguments passed to \code{\link[ggplot2]{ggsave}}.
  #' @keywords internal
  #' @export
  #' @importFrom ggplot2 ggsave last_plot
  #' @examples
  #' \dontrun{
  #' save_plot(project, "species_catch")
  #' }

  p_set <- get_proj_settings(project)
  
  filename <- paste0(locoutput(project), project, "_", func_name, "_", Sys.Date())
  
  if("plotly" %in% class(...)){
    fname <- paste0(filename, ".rds")
  } else {
    fname <- paste0(filename, ".png")
  }

  if (!is.null(p_set$save_plot_rds) && p_set$save_plot_rds) {

    fn_rds <- paste0(filename, ".RDS")
    saveRDS(object = ggplot2::last_plot(), file = fn_rds)
  }

  p_size <- get_proj_settings(project)$plot_size

  if("plotly" %in% class(...)){
    saveRDS(..., fname)
  } else {
    ggplot2::ggsave(file = fname, width = p_size[1], height = p_size[2], ...)
  }
}


save_nplot <- function(project, func_name, plot_list, id = "num", ...) {
  #' Save table to output folder
  #' @param project name of project.
  #' @param func_name Name of function used to create plot.
  #' @param ... addition arguments passed to \code{\link[ggplot2]{ggsave}}.
  #' @keywords internal
  #' @export
  #' @examples
  #' \dontrun{
  #' save_nplot(project, "species_catch", plot_list)
  #' }
  
  if (id == "num") vec <- seq_along(plot_list)
  else if (id == "name") vec <- names(plot_list)
  
  if("plotly" %in% class(plot_list[[1]])){
    lapply(vec, function(x) {
      fn <- paste0(func_name, "_", x)
      save_plot(project, func_name = fn, plot_list[[x]])
    })
  } else {
    lapply(vec, function(x) {

      fn <- paste0(func_name, "_", x)
      save_plot(project, func_name = fn, plot_list[[x]], ...)
    })
  }
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
#----
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

mvgrnd <- function(m,sigma,n){
  #' Multivariate normal random generator
  #' @param m mean vector
  #' @param sigma variance-covariance matrix
  #' @param n number of iterations
  #' @export
  #' @keywords internal
  
  U = chol(sigma) #Cholesky decomposition. U is an upper triangular matrix
  
  d = length(m)
 
   y <- matrix(NA, nrow=n, ncol=d)
  
 
  for(i in 1:n){
    y[i,1:d] = t(m) + rnorm(d) %*% U
  }
  return(y)
}

repmat <- function(X, m, n){
  #' R verions of matlabs repmat
  #' @param X matrix
  #' @param m nrows
  #' @param n n cols
  #' @keywords internal
  #' @export
  
  mx <- dim(X)[1]
  nx <- dim(X)[2]
  return(matrix(t(matrix(X,mx,nx*n)),mx*m,nx*n,byrow=T))
}

# Check whether this is better than check_spatdat
gridcheck <- function(spatialdat, catdat, londat=NULL, latdat=NULL, lon.grid=NULL, lat.grid=NULL){
  #' Check that spatial data is a sf object. Convert if not.
  #' @param spatialdat The spatial dataframe
  #' @param catdat Variable that names polygons
  #' @param londat Longitude data from primary dataset
  #' @param latdat Latitude data from primary dataset
  #' @param lon.grid Variable in spatialdat containing longitude data
  #' @param lat.grid Variable in spatialdat containing latitude data
  #' @export
  #' @keywords internal
  if (any(grepl("Spatial", class(spatialdat)))) {
    if(any(class(spatialdat) %in% c("sp", "SpatialPolygonsDataFrame"))) {
      spatialdat <- sf::st_as_sf(spatialdat)
      if(any(grepl('PROJCRS',  sf::st_crs(spatialdat)))){
      spatialdat <- st_transform(spatialdat, "+proj=longlat +ellps=WGS84 +datum=WGS84")
      }
    } else {
      if (is_empty(lon.grid) | is_empty(lat.grid)) {
        warning("lat.grid and lon.grid must be supplied to convert sp object to a sf object.")
      } else {
        spatialdat <- sf::st_as_sf(
          x = spatialdat,
          zone = catdat,
          coords = c(londat, latdat),
          crs = "+proj=longlat +datum=WGS84"
        )
      }
    }
  }
  gridfile <- sf::st_shift_longitude(spatialdat)
}


bbox <- function(dat, lon, lat, f = 0.05) {
  #' Compute bounding box for a dataframe with lon/lat columns. 
  #' 
  #' @param dat Dataframe containing longitude/latitude columns.
  #' @param lon Name of Longitude column.
  #' @param lat Name of Latitude column.
  #' @param f Number specifying the fraction by which to extend the range.
  #' @export
  #' @keywords internal
  #' @importFrom grDevices extendrange
  
  lon_range <- grDevices::extendrange(range(dat[[lon]], na.rm = TRUE), f = f)
  lat_range <- grDevices::extendrange(range(dat[[lat]], na.rm = TRUE), f = f)
  
  c(left = lon_range[1], bottom = lat_range[1], 
    right = lon_range[2], top = lat_range[2])
}

## ----Shiny util functions-----

use_prompter <- function() {
  
#' Wrapper for prompter::use_prompt()
#' 
#' @importFrom prompter use_prompt
#' @export
#' @keywords internal
#' 

  prompter::use_prompt()
}


add_prompter <- function(ui_element,
                         position = "bottom",
                         message = NULL,
                         type = NULL,
                         size = NULL,
                         permanent = FALSE,
                         rounded = FALSE,
                         animate = TRUE,
                         bounce = FALSE,
                         arrow = TRUE,
                         shadow = TRUE) {
  
#' Wrapper for prompter::add_prompt()
#' 
#' @importFrom prompter add_prompt
#' @export
#' @keywords internal
  
  prompter::add_prompt(ui_element, position = position, message = message,
                       type = type, size = size, permanent = permanent,
                       rounded = rounded, animate = animate, bounce = bounce,
                       arrow = arrow, shadow = shadow)
  
}


outlier_plot_int <- function(dat, x, dat_remove = "none", x_dist = "normal", 
                             sd_val = NULL, plot_type) {
  #' Evaluate outliers through plots
  #' @param dat Primary data frame over which to apply function. Table in fishet_db 
  #'   database should contain the string `MainDataTable`.
  #' @param x Column in dataframe to check for outliers.
  #' @param dat_remove Defines method to subset the data. Choices include: 'none', 
  #'   '5_95_quant', '25_75_quant', 'mean_2SD', 'median_2SD', 'mean_3SD', 'median_3SD'.
  #' @param x_dist Distribution of the data. Choices include: 'normal', 'lognormal', 
  #'   'exponential', 'weibull', 'poisson', 'negative binomial'.
  #' @param sd_val User-defined rule.
  #' @param plot_type Which plot to return.
  #' @importFrom graphics points
  #' @import ggplot2
  #' @keywords internal
  #' @export
  #' @details  The function returns three plots, the data, a probability plot, 
  #'  and a Q-Q plot. The data plot is the value of \code{x} against row number. 
  #'  Red points are all the data without any points removed. The blue points are 
  #'  the subsetted data. If `dat_remove` is `none`, then only blue points will 
  #'  be shown. The probability plot is a histogram of the data with the fitted 
  #'  probability distribution based on `x_dist`. The Q-Q plot plots are
  #'  sampled quantiles against theoretical quantiles.
  #'
  #' @return Plot of the data


  requireNamespace("ggplot2")

  dataset <- dat
  x.name <- x
  
  if(!is.null(sd_val) & is.numeric(sd_val)){
    dat_remove <- sd_val
  }
  
  if (is.numeric(dataset[[x]])) {
    # Begin outlier check
    dataset$val <- 1:nrow(dataset)
    if (dat_remove == "none") {
      dataset$Points <- "Kept"
    } else {
      if(is.numeric(dat_remove)){
        dataset$Points <- ifelse(dataset[[x]] < (mean(dataset[[x]], na.rm = T) + dat_remove * stats::sd(dataset[[x]], na.rm = T)) &
                                   dataset[[x]] > (mean(dataset[[x]], na.rm = T) - dat_remove * stats::sd(dataset[[x]], na.rm = T)), "Kept", "Removed")
      } else if (dat_remove == "5_95_quant") {
        dataset$Points <- ifelse(dataset[[x]] < stats::quantile(dataset[[x]], 0.95, na.rm = TRUE) &
          dataset[[x]] > stats::quantile(dataset[[x]], 0.05, na.rm = TRUE), "Kept", "Removed")
      } else if (dat_remove == "25_75_quant") {
        dataset$Points <- ifelse(dataset[[x]] < stats::quantile(dataset[[x]], 0.75, na.rm = TRUE) &
          dataset[[x]] > stats::quantile(dataset[[x]], 0.25, na.rm = TRUE), "Kept", "Removed")
      } else if (dat_remove == "mean_2SD") {
        dataset$Points <- ifelse(dataset[[x]] < (mean(dataset[[x]], na.rm = T) + 2 *
          stats::sd(dataset[[x]], na.rm = T)) &
          dataset[[x]] > (mean(dataset[[x]], na.rm = T) - 2 * stats::sd(dataset[[x]], na.rm = T)), "Kept", "Removed")
      } else if (dat_remove == "median_2SD") {
        dataset$Points <- ifelse(dataset[[x]] < (stats::median(dataset[[x]], na.rm = T) + 2 * stats::sd(dataset[[x]], na.rm = T)) &
          dataset[[x]] > (stats::median(dataset[[x]], na.rm = T) - 2 * stats::sd(dataset[[x]], na.rm = T)), "Kept", "Removed")
      } else if (dat_remove == "mean_3SD") {
        dataset$Points <- ifelse(dataset[[x]] < (mean(dataset[[x]], na.rm = T) + 3 * stats::sd(dataset[[x]], na.rm = T)) &
          dataset[[x]] > (mean(dataset[[x]], na.rm = T) - 3 * stats::sd(dataset[[x]], na.rm = T)), "Kept", "Removed")
      } else if (dat_remove == "median_3SD") {
        dataset$Points <- ifelse(dataset[[x]] < (stats::median(dataset[[x]], na.rm = T) + 3 * stats::sd(dataset[[x]], na.rm = T)) &
          dataset[[x]] > (stats::median(dataset[[x]], na.rm = T) - 3 * stats::sd(dataset[[x]], na.rm = T)), "Kept", "Removed")
      }
    } # End Outlier mod

    mytheme <- theme(
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size = 11),
      axis.title = element_text(size = 11)
    )
 
    # Hist
    # Plot 2!
    if (x_dist == "normal") {
      arg.return <- stat_function(
        fun = dnorm, colour = "blue",
        args = list(mean = mean(unlist(dataset[dataset$Points == "Kept", x], use.names = FALSE), na.rm = TRUE), sd = sd(unlist(dataset[dataset$Points == "Kept", x], use.names = FALSE), na.rm = TRUE))
      )
    } 
    else if (x_dist == "lognormal") {
      # lognormal
      arg.return <- stat_function(
        fun = dlnorm, colour = "blue",
        args = list(mean = mean(log(unlist(dataset[dataset$Points == "Kept", x], use.names = FALSE)), na.rm = TRUE), sd = sd(log(unlist(dataset[dataset$Points == "Kept", x], use.names = FALSE)), na.rm = TRUE))
      )
    } else if (x_dist == "exponential") {
      # Exponential
      arg.return <- stat_function(
        fun = dexp, colour = "blue",
        args = list(rate = 1 / mean(unlist(dataset[dataset$Points == "Kept", x], use.names = FALSE), na.rm = TRUE))
      )
    } else if (x_dist == "weibull") {
      # Weibull
      arg.return <- stat_function(
        fun = dweibull, colour = "blue",
        args = list(
          shape = 1.2 / sqrt(var(log(unlist(dataset[dataset$Points == "Kept", x], use.names = FALSE)), na.rm = TRUE)),
          scale = mean(dataset[dataset$Points == "Kept", x], na.rm = TRUE) + 0.572 / (1.2 / sqrt(var(log(unlist(dataset[dataset$Points == "Kept", x], use.names = FALSE)), na.rm = TRUE)))
        )
      )
    } else if (x_dist == "poisson") {
      # Poisson
      arg.return <- stat_function(
        fun = dpois, colour = "blue",
        args = list(lambda = mean(unlist(dataset[dataset$Points == "Kept", x], use.names = FALSE), na.rm = TRUE))
      )
    } else if (x_dist == "negative binomial") {
      # Negative Binomial
      arg.return <- stat_function(
        fun = dnbinom, colour = "blue",
        args = list(mean(unlist(dataset[dataset$Points == "Kept", x], use.names = FALSE), na.rm = TRUE)^2 / (var(unlist(dataset[dataset$Points == "Kept", x], use.names = FALSE), na.rm = TRUE) - mean(unlist(dataset[dataset$Points == "Kept", x], use.names = FALSE), na.rm = TRUE)),
          mu = mean(unlist(dataset[dataset$Points == "Kept", x], use.names = FALSE), na.rm = TRUE)
        )
      )
    }

    # Plot3
    # Probability plot
    quants <- seq(0, 1, length = length(unlist(dataset[dataset$Points == "Kept", x], use.names = FALSE)) + 2)[2:(length(unlist(dataset[dataset$Points == "Kept", x], use.names = FALSE)) + 1)]
    # normal
    if (x_dist == "normal") {
      fit_quants <- stats::qnorm(quants, mean(unlist(dataset[dataset$Points == "Kept", x], use.names = FALSE), na.rm = TRUE), sd(unlist(dataset[dataset$Points == "Kept", x], use.names = FALSE), na.rm = TRUE))
    } else if (x_dist == "lognormal") {
      # lognormal
      fit_quants <- stats::qlnorm(quants, mean = mean(log(unlist(dataset[dataset$Points == "Kept", x], use.names = FALSE)), na.rm = TRUE), sd = sd(log(unlist(dataset[dataset$Points == "Kept", x], use.names = FALSE)), na.rm = TRUE))
    } else if (x_dist == "exponential") {
      # Exponential
      fit_quants <- stats::qexp(quants, rate = 1 / mean(unlist(dataset[dataset$Points == "Kept", x], use.names = FALSE), na.rm = TRUE))
    } else if (x_dist == "weibull") {
      # Weibull
      fit_quants <- stats::qweibull(quants,
        shape = 1.2 / sqrt(var(log(unlist(dataset[dataset$Points == "Kept", x], use.names = FALSE)), na.rm = TRUE)),
        scale = mean(unlist(dataset[dataset$Points == "Kept", x], use.names = FALSE), na.rm = TRUE) + 0.572 / (1.2 / sqrt(var(log(unlist(dataset[dataset$Points == "Kept", x], use.names = FALSE)), na.rm = TRUE)))
      )
    } else if (x_dist == "poisson") {
      # Poisson
      fit_quants <- stats::qpois(quants, lambda = mean(unlist(dataset[dataset$Points == "Kept", x], use.names = FALSE), na.rm = TRUE))
    } else if (x_dist == "negative binomial") {
      # Negative Binomial
      fit_quants <- stats::qnbinom(quants,
        size = mean(unlist(dataset[dataset$Points == "Kept", x], use.names = FALSE), na.rm = TRUE)^2 / (var(unlist(dataset[dataset$Points == "Kept", x], use.names = FALSE), na.rm = TRUE) - mean(dataset[dataset$Points == "Kept", x], na.rm = TRUE)),
        mu = mean(unlist(dataset[dataset$Points == "Kept", x], use.names = FALSE), na.rm = TRUE)
      )
    }

    data_quants <- stats::quantile(as.numeric(unlist(dataset[dataset$Points == "Kept", x], use.names = FALSE)), quants, na.rm = TRUE)
    # create Q-Q plot
    temp <- data.frame(fit_quants, data_quants)
    p3 <- ggplot(temp, aes(x = fit_quants, y = data_quants)) +
      geom_point(shape = 1) +
      geom_abline() +
      labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = paste("Q-Q plot of", x_dist, "fit against data")) +
      mytheme

    if (plot_type == "1") {
      return(dataset)
    } else if (plot_type == "2") {
      suppressWarnings(return(arg.return))
    } else if (plot_type == "3") {
      suppressWarnings(return(temp))
    }
    # Put it all together
    
    # p_title <- paste0("Plots for ", x, " with ", x_dist,
    #                  " distribution and data removed based on '", dat_remove,
    #                  "'. \nBlue: included points   Red: removed points")
    # TODO: p1 and p2 out of scope
    # fig <- gridExtra::grid.arrange(p1, p2, p3, ncol = 2, nrow = 2,
    #                                top = grid::textGrob(p_title, gp = grid::gpar(fontsize = 10)))
    # 
    # fig
    
    # fig <- suppressWarnings(ggpubr::ggarrange(p1, p2, p3 , ncol = 2, nrow = 2))
    # labels = c("A", "B", "C"),
    # fig <- ggpubr::annotate_figure(fig, top = ggpubr::text_grob(paste("Plots for ", x, " with ", x_dist,
    #                                                                  " distribution and data removed based on '", dat_remove,
    #                                                                  "'. \nBlue: included points   Red: removed points"), size = 10))
  } else {
    # Actions to take if data is not numeric
    print("Data is not numeric. Plots not generated.")
  }
}

quietly_test <- function(.f, show_msg = FALSE) {
  #' quietly_test
  #' capture console messages if exist and print to shiny app
  #' @param .f function name
  #' @param show_msg Logical, whether to show messages in shiny app. 
  #' @export
  #' @keywords internal
  #' @importFrom purrr quietly safely
  #

  fun1 <- purrr::quietly(.f)
  fun <- purrr::safely(fun1)
  
  function(...) {
    res <- fun(...)

    if (!is.null(res$error)) { # safely output
      
      showNotification(conditionMessage(res$error), duration = 60, type = "error")
      return(res$result)
    }
    
    res <- res$result # quietly output
    
    if (!is.null(res$warnings) && length(res$warnings) > 0) {
      
      lapply(unique(res$warnings), showNotification, duration = 60, type = "warning")
    }
    
    if (show_msg) {
      
      if (!is.null(res$messages) && length(res$message) > 0) {
        
        lapply(unique(res$messages), showNotification, duration = 60, type = "default")
      }
    }
    
    return(res$result)
  }
}


quiet_safe_test <- function(.f) {
  #' Quietly and safely test function
  #' 
  #' Used for package functions: safely catches errors and quietly catches warnings
  #' and messages. Both capture results.
  #' 
  #' @param .f Name of a function to quietly and safely test. 
  #' @importFrom purrr quietly safely
  #' @keywords internal
  #' @seealso \code{\link[purrr]{quietly}} and \code{\link[purrr]{safely}}
  
  fun1 <- purrr::quietly(.f)
  fun <- purrr::safely(fun1)
  
  function(...) fun(...)
}


deleteButtonColumn <- function(df, id, ...) {
#' A column of delete buttons for each row in the data frame for the first column
#'
#' @param df data frame
#' @param id id prefix to add to each actionButton. The buttons will be id'd as id_INDEX.
#' @return A DT::datatable with escaping turned off that has the delete buttons in the first column 
#'   and \code{df} in the other function to create one action button as string
#' @importFrom DT datatable
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
# ----

parseDeleteEvent <- function(idstr) {
  #' Extracts the row id number from the id string
#' @param idstr the id string formated as id_INDEX
#' @return INDEX from the id string id_INDEX
#' @export
#' @keywords internal

  res <- as.integer(sub(".*_([0-9]+)", "\\1", idstr))
  if (! is.na(res)) res
}

# Helper functions to find specific types of variables (date, port, catch, etc) ---- 
find_lon <- function(dat) {
 #' Find columns that may be longitude data
 #' @param dat Data set to search over
 #' @keywords internal
 #' @importFrom stringi stri_count_regex
 #' @export
 
  # if (all(is_empty(dat[1, ]))) return(NULL)
  
  cols <- colnames(dat)[grep('lon', colnames(dat), ignore.case = TRUE)]
  lat_find <- stringi::stri_count_regex(cols, '(?=LAT|Lat|lat)')
  
  if (any(lat_find > 0)) {
    
    lat_rem <- which(lat_find %in% max(lat_find))
    cols <- cols[-c(lat_rem)]
  }
  
  return(cols)
}

find_lat <- function(dat) {
  #' Find columns that may be latitude data
  #' @param dat Data set to search over
  #' @keywords internal
  #' @importFrom stringi stri_count_regex
  #' @export
  #' 

  # if (all(is_empty(dat[1,]))) return()
  
  cols <- colnames(dat)[grep('lat', colnames(dat), ignore.case = TRUE)]
  lon_find <- stringi::stri_count_regex(cols, '(?=LON|Lon|lon)')
  
  if (any(lon_find > 0)) {
    
    lon_rem <- which(lon_find %in% max(lon_find))
    cols <- cols[-c(lon_rem)]
  }
  
  return(cols)
}

find_lonlat <- function(dat) {
  #' Find columns that may be longitude or latitude data
  #' @param dat Dataset to search over
  #' @keywords internal
  #' @export
  
  grep("lon|lat", colnames(dat), ignore.case = TRUE, value = TRUE)
}

find_datetime <- function(dat) {
  #' Find columns that may be date/datetime 
  #' @param dat Dataset to search over
  #' @keywords internal
  #' @export
  
  c_nms <- grep('date|hour|time|year|day', colnames(dat), 
                ignore.case = TRUE, value = TRUE)
  
  c_class <- vapply(dat, function(d) {
    
    any(lubridate::is.Date(d), lubridate::is.POSIXt(d))
  }, logical(1))
  
  out <- names(dat)[c_class]
  unique(out, c_nms)
}

find_catch <- function(dat) {
  #' Find columns that may contain catch 
  #' @param dat Dataset to search over
  #' @keywords internal
  #' @export
  #' 
  grep("catch|haul|mt|lb|ton|pound|weight|metric|kilo|mass", colnames(dat), 
       ignore.case = TRUE, value = TRUE)
}

find_port <- function(dat) {
  #' Find columns that may contain ports 
  #' @param dat Dataset to search over
  #' @keywords internal
  #' @export
  
  grep("port", colnames(dat), ignore.case = TRUE, value = TRUE)
}

find_duration <- function(dat) {
  #' Find columns that may contain durations 
  #' @param dat Dataset to search over
  #' @keywords internal
  #' @export
  #' 
  #' 
  grep("date|min|hour|week|month|TRIP_START|TRIP_END", 
       names(dat), ignore.case = TRUE, value = TRUE)
}

find_value <- function(dat) {
  #' Find columns that may contain value  
  #' @param dat Dataset to search over
  #' @keywords internal
  #' @export
  
  grep("dollar|val|euro|price|cost|earn", colnames(dat), ignore.case = TRUE, 
       value = TRUE)
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

date_cols <- function(dat, out = "names", type = 'both') {
  #' Find columns that can be converted to Date or Date-time class
  #' 
  #' @param dat MainDataTable or dataframe to check.
  #' @param out Whether to return the column \code{"names"} (the default) or a logical vector 
  #'   (\code{"logical"}).
  #' @param type String, the type of date column to test for. Options are 
  #' \code{"date"}, \code{"date_time"}, or \code{"both"}. 
  #' @export
  #' @keywords internal
  #' @importFrom purrr map_lgl
  #' @importFrom lubridate mdy dmy ymd ydm dym ymd_hms dmy_hms mdy_hms ydm_hms
  #' @examples 
  #' \dontrun{
  #' date_cols(pollockMainDataTable) # returns column names
  #' date_cols(pollockMainDataTable, "logical")
  #' }
  
  if (!type %in% c('date', 'date_time', 'both')) {
    
    stop('Invaild date type. Options are "date" and "date_time".', call. = FALSE)
  }
  
  # named logical vector to preserve col order
  date_lgl <- logical(ncol(dat))
  names(date_lgl) <- names(dat)
  
  date_f <- list(lubridate::mdy, lubridate::dmy, lubridate::ymd, lubridate::ydm, 
                 lubridate::dym)
  
  date_time_f <- list(lubridate::ymd_hms, lubridate::dmy_hms, lubridate::mdy_hms, 
                      lubridate::ydm_hms)
  
  # lubridate functions to test for
  date_funs <- switch(type, 
                      date = date_f, 
                      date_time = date_time_f, 
                      both = c(date_f, date_time_f))
  
  date_helper <- function(dates, fun) {
    
    if (all(is.na(dates))) return(FALSE)
    # track NAs before converting
    na_ind <-is.na(dates)
    dates <- trimws(dates)
    # remove time info
    # dates <- gsub("\\s\\d{2}:\\d{2}:\\d{2}$", "", dates)
    # attempt covert column to date
    out <- suppressWarnings(fun(dates))
    # the # of NAs from non-NA values
    na_sum <- sum(is.na(out[!na_ind]))
    # If no NAs after conversion, return TRUE
    if (na_sum == 0) return(TRUE)
    # prop of NAs after conversion (over originally non-NA values)
    na_prop <- na_sum/length(out[!na_ind])
    # Return TRUE if NA prop is at or below .5
    na_prop <= .5
  }
  
  # apply each function to date vector
  date_apply <- function(dates) {
    
    out <- purrr::map_lgl(date_funs, function(fun) date_helper(dates, fun))
    
    any(out)
  }
  
  # number of rows to check 
  nr <- nrow(dat)
  
  if (nr > 1000) dat_slice <- 1000
  else dat_slice <- nr
  
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
# ----

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

pull_shiny_output <- function(project, fun = NULL, type = "plot", conf = TRUE) {
  #'
  #' Retrieve output file name by project, function, and type
  #'
  #' @param project Name of project
  #' @param fun Name of function.
  #' @param type Whether to return the \code{"plot"} (.png), \code{"table"} (.csv),
  #'  "notes" (.txt) or \code{"all"} files matching the project name, function, and date.
  #' @param conf Logical, whether to return suppressed confidential data. 
  #'    Unsuppressed output will be pulled if suppressed output is not available. 
  #' @export
  #' @keywords internal
  #' @examples
  #' \dontrun{
  #' pull_output("pollock", "species_catch", type = "plot")
  #' }
  end <- FALSE
  
  outs <- project_files(project)
  ext <- switch(type, "plot" = ".*\\.png$", "table" = ".*\\.csv$", 
                "notes" = ".*\\.txt$", "zone" = ".*\\.yaml")
  
  out <- grep(ext, outs, value = TRUE)
  
  if (length(out) == 0) {
    
    message("No ", type, " found for project '", project,"'.", sep = "")
    end <- TRUE
  }
  
  if (!end) {
    
    if (!is.null(fun)) {
      
      out <- grep(paste0("_", fun), out, value = TRUE)
      
      if (length(out) == 0) {
        
        message("No ", type, " output for function ", fun, " exists for project ",
                "'", project, "'. ", sep = "")
        end <- TRUE
      }
      
    } else out <- grep("_notes_", out, value = TRUE)
    
    if (!end) {
      
      
      if (!end) {
        # check for suppressed output
        conf_ind <- grepl("_confid_", out)
        
        if (conf) {
          
          if (sum(conf_ind) > 0) out <- out[conf_ind]
          
        } else {
          
          if (sum(!conf_ind) > 0) out <- out[!conf_ind]
        }
        
        out
      }
    }
  }
}


fishset_viridis <- function(n) {
  #' viridis color function
  #' @param n data
  #' @export
  #' @keywords internal
  
  if (n == 0) {
    return(character(0))
  }
  
  map <- data.frame(R=c(0.18995,0.19483,0.19956,0.20415,0.20860,0.21291,0.21708,0.22111,0.22500,0.22875,0.23236,0.23582,0.23915,0.24234,0.24539,0.24830,0.25107,0.25369,0.25618,0.25853,0.26074,0.26280,0.26473,0.26652,0.26816,0.26967,0.27103,0.27226,0.27334,0.27429,0.27509,
                        0.27576,0.27628,0.27667,0.27691,0.27701,0.27698,0.27680,0.27648,0.27603,0.27543,0.27469,0.27381,0.27273,0.27106,0.26878,0.26592,0.26252,0.25862,0.25425,0.24946,0.24427,0.23874,0.23288,0.22676,0.22039,0.21382,0.20708,0.20021,0.19326,0.18625,0.17923,0.17223,0.16529,0.15844,0.15173,0.14519,0.13886,0.13278,
                        0.12698,0.12151,0.11639,0.11167,0.10738,0.10357,0.10026,0.09750,0.09532,0.09377,0.09287,0.09267,0.09320,0.09451,0.09662,0.09958,0.10342,0.10815,0.11374,0.12014,0.12733,0.13526,0.14391,0.15323,0.16319,0.17377,0.18491,0.19659,0.20877,0.22142,0.23449,0.24797,0.26180,0.27597,0.29042,0.30513,0.32006,
                        0.33517,0.35043,0.36581,0.38127,0.39678,0.41229,0.42778,0.44321,0.45854,0.47375,0.48879,0.50362,0.51822,0.53255,0.54658,0.56026,0.57357,0.58646,0.59891,0.61088,0.62233,0.63323,0.64362,0.65394,0.66428,0.67462,0.68494,0.69525,0.70553,0.71577,0.72596,0.73610,0.74617,0.75617,0.76608,0.77591,0.78563,
                        0.79524,0.80473,0.81410,0.82333,0.83241,0.84133,0.85010,0.85868,0.86709,0.87530,0.88331,0.89112,0.89870,0.90605,0.91317,0.92004,0.92666,0.93301,0.93909,0.94489,0.95039,0.95560,0.96049,0.96507,0.96931,0.97323,0.97679,0.98000,0.98289,0.98549,0.98781,0.98986,0.99163,0.99314,0.99438,0.99535,0.99607,
                        0.99654,0.99675,0.99672,0.99644,0.99593,0.99517,0.99419,0.99297,0.99153,0.98987,0.98799,0.98590,0.98360,0.98108,0.97837,0.97545,0.97234,0.96904,0.96555,0.96187,0.95801,0.95398,0.94977,0.94538,0.94084,0.93612,0.93125,0.92623,0.92105,0.91572,0.91024,0.90463,0.89888,0.89298,0.88691,0.88066,0.87422,
                        0.86760,0.86079,0.85380,0.84662,0.83926,0.83172,0.82399,0.81608,0.80799,0.79971,0.79125,0.78260,0.77377,0.76476,0.75556,0.74617,0.73661,0.72686,0.71692,0.70680,0.69650,0.68602,0.67535,0.66449,0.65345,0.64223,0.63082,0.61923,0.60746,0.59550,0.58336,0.57103,0.55852,0.54583,0.53295,0.51989,0.50664,0.49321,
                        0.4796),
                    G=c(0.07176,0.08339,0.09498,0.10652,0.11802,0.12947,0.14087,0.15223,0.16354,0.17481,0.18603,0.19720,0.20833,0.21941,0.23044,0.24143,0.25237,0.26327,0.27412,0.28492,0.29568,0.30639,0.31706,0.32768,0.33825,0.34878,0.359260,0.36970,0.38008,0.39043,0.40072,0.41097,0.42118,0.43134,0.44145,0.45152,0.46153,0.47151,
                        0.48144,0.49132,0.50115,0.51094,0.52069,0.53040,0.54015,0.54995,0.55979,0.56967,0.57958,0.58950,0.59943,0.60937,0.61931,0.62923,0.63913,0.64901,0.65886,0.66866,0.67842,0.68812,0.697750,0.70732,0.71680,0.72620,0.73551,0.74472,0.75381,0.76279,0.77165,0.78037,0.78896,0.79740,0.80569,0.81381,0.82177,0.82955,
                        0.83714,0.84455,0.85175,0.85875,0.86554,0.87211,0.87844,0.88454,0.89040,0.89600,0.90142,0.90673,0.91193,0.91701,0.92197,0.926800,0.93151,0.93609,0.94053,0.94484,0.94901,0.95304,0.95692,0.96065,0.96423,0.96765,0.97092,0.97403,0.97697,0.97974,0.98234,0.98477,0.98702,0.98909,0.99098,0.99268,0.99419,0.99551,
                        0.99663,0.99755,0.99828,0.99879,0.99910,0.99919,0.99907,0.99873,0.99817,0.99739,0.99638,0.99514,0.99366,0.99195,0.98999,0.98775,0.98524,0.98246,0.97941,0.97610,0.97255,0.96875,0.96470,0.96043,0.95593,0.95121,0.94627,0.94113,0.93579,0.93025,0.92452,0.91861,0.91253,0.90627,0.89986,0.89328,0.88655,0.87968,
                        0.87267,0.86553,0.85826,0.85087,0.84337,0.83576,0.82806,0.82025,0.81236,0.80439,0.79634,0.78823,0.78005,0.77181,0.76352,0.75519,0.746820,0.73842,0.73000,0.72140,0.71250,0.70330,0.69382,0.68408,0.67408,0.66386,0.65341,0.64277,0.63193,0.62093,0.60977,0.59846,0.58703,0.57549,0.56386,0.55214,0.54036,0.52854,
                        0.51667,0.50479,0.49291,0.48104,0.46920,0.45740,0.44565,0.43399,0.42241,0.41093,0.39958,0.38836,0.37729,0.36638,0.35566,0.34513,0.33482,0.32473,0.31489,0.30530,0.29599,0.28696,0.27824,0.26981,0.26152,0.25334,0.24526,0.23730,0.22945,0.22170,0.21407,0.20654,0.19912,0.19182,0.18462,0.17753,0.17055,0.16368,
                        0.15693,0.15028,0.14374,0.13731,0.13098,0.12477,0.11867,0.11268,0.10680,0.10102,0.09536,0.08980,0.08436,0.07902,0.07380,0.06868,0.063670,0.05878,0.05399,0.04931,0.04474,0.04028,0.03593,0.03169,0.02756,0.02354,0.01963,
                        0.01583),
                    B=c(0.23217,0.26149,0.29024,0.31844,0.34607,0.37314,0.39964,0.42558,0.45096,0.47578,0.50004,0.52373,0.54686,0.56942,0.59142,0.61286,0.63374,0.65406,0.67381,0.69300,0.71162,0.72968,0.74718,0.76412,0.78050,0.79631,0.81156,0.82624,0.84037,0.85393,0.86692,0.87936,0.89123,0.90254,0.91328,0.92347,0.93309,0.94214,
                        0.95064,0.95857,0.96594,0.97275,0.97899,0.98461,0.98930,0.99303,0.99583,0.99773,0.99876,0.99896,0.99835,0.99697,0.99485,0.99202,0.98851,0.98436,0.97959,0.97423,0.96833,0.96190,0.95498,0.94761,0.93981,0.93161,0.92305,0.91416,0.90496,0.89550,0.88580,0.87590,0.86581,0.85559,0.84525,0.83484,0.82437,0.81389,
                        0.80342,0.79299,0.78264,0.77240,0.76230,0.75237,0.74265,0.73316,0.72393,0.71500,0.70599,0.69651,0.68660,0.67627,0.66556,0.65448,0.64308,0.63137,0.61938,0.60713,0.59466,0.58199,0.56914,0.55614,0.54303,0.52981,0.51653,0.50321,0.48987,0.47654,0.46325,0.45002,0.43688,0.42386,0.41098,0.39826,0.38575,0.37345,
                        0.36140,0.34963,0.33816,0.32701,0.31622,0.30581,0.29581,0.28623,0.27712,0.26849,0.26038,0.25280,0.24579,0.23937,0.23356,0.22835,0.22370,0.21960,0.21602,0.21294,0.21032,0.20815,0.20640,0.20504,0.20406,0.20343,0.20311,0.20310,0.20336,0.20386,0.20459,0.20552,0.20663,0.20788,0.20926,0.21074,0.21230,0.21391,
                        0.21555,0.21719,0.21880,0.22038,0.22188,0.22328,0.22456,0.22570,0.22667,0.22744,0.22800,0.22831,0.22836,0.22811,0.22754,0.22663,0.22536,0.22369,0.22161,0.21918,0.21650,0.21358,0.21043,0.20706,0.20348,0.19971,0.19577,0.19165,0.18738,0.18297,0.17842,0.17376,0.16899,0.16412,0.15918,0.15417,0.14910,0.14398,
                        0.13883,0.13367,0.12849,0.12332,0.11817,0.11305,0.10797,0.10294,0.09798,0.09310,0.08831,0.08362,0.07905,0.07461,0.07031,0.06616,0.06218,0.05837,0.05475,0.05134,0.04814,0.04516,0.04243,0.03993,0.03753,0.03521,0.03297,0.03082,0.02875,0.02677,0.02487,0.02305,0.02131,0.01966,0.01809,0.01660,0.01520,0.01387,
                        0.01264,0.01148,0.01041,0.00942,0.00851,0.00769,0.00695,0.00629,0.00571,0.00522,0.00481,0.00449,0.00424,0.00408,0.00401,0.00401,0.00410,0.00427,0.00453,0.00486,0.00529,0.00579,0.00638,0.00705,0.00780,0.00863,0.00955,
                        0.01055)
  )
  
  
  
  
  map_cols <- grDevices::rgb(map$R, map$G, map$B)
  fn_cols <- grDevices::colorRamp(map_cols, space = "Lab", interpolate = "spline")
  cols <- fn_cols(seq(0, 1, length.out = n))/255
  grDevices::rgb(cols[, 1], cols[, 2], cols[, 3], alpha = 1)
}

