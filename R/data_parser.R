#  Import data
#str_trim

read_dat <- function(x, data.type=NULL, ...) {
  #' Import data into R
  #' @param x Name and path of dataset to be read in.
  #' @param data.type Optional. Data type can be defined by user or based on the file extension,
  #'   Leave \code{data.type} as NULL if data type is to based on file extension.
  #'   R, comma deliminated, tab deliminated, excel, matlab, json, geojson, shape, sas,
  #'    spss, and stata data files are recognized. 
  #' @param ... Optional arguments 
  #' @importFrom sf read_sf
  #' @importFrom R.matlab readMat
  #' @importFrom jsonlite fromJSON
  #' @importFrom haven read_spss read_stata read_sas
  #' @importFrom utils read.table
  #' @importFrom readxl read_excel
  #' @details Uses the appropriate function to read in data based on data type.
  #'   Supported data types include shape, csv, json, matlab, R, spss, and stata files.
  #'   Additional arguments can be added, such as the seperator agument \code{sep='\\t'} for  
  #'   reading in tab deliminated files. For more details, see \code{\link[base]{load}} for loading R objects, 
  #'   \code{\link{read.csv}} for reading in comma deliminated files,
  #'   \code{\link[utils]{read.table}} for reading in tab deliminated files, 
  #'   \code{\link[readxl]{read_excel}} for reading in excel files (xls, xlsx), 
  #'   \code{\link[sf]{read_sf}} for reading in shape or geojson files, 
  #'   \code{\link[R.matlab]{readMat}} for reading in matlab data files,
  #'   \code{\link[haven]{read_stata}} for reading in stata data files,
  #'   \code{\link[haven]{read_spss}} for reading in spss data files,
  #'   \code{\link[haven]{read_sas}} for reading in sas data files, and 
  #'   \code{\link[jsonlite]{fromJSON}} for reading in json files.
  #' @export
  #' @examples
  #' \dontrun{
  #' dat <- read_dat('C:/data/nmfs_manage_simple.shp')
  #' }
  
  if(is.null(data.type)) {
        data.type <- sub('.*\\.', '', x)
  }
  
  data.type <- tolower(data.type)
  
  if (data.type == 'rdata' | data.type == "r") {
    return(as.data.frame(get(load(x))))
  } else if (data.type == "mat" | data.type == 'matlab') {
    cat('Data returned as named list structure. Further processing is required.')
    R.matlab::readMat(x, ...)
  } else if (data.type == "json"){
    cat('Data may require additional processing.')
    jsonlite::fromJSON(x, ...)
  } else if (data.type == "geojson") {
    sf::st_read(x, ...)
  } else if (data.type == "csv") {
    read.csv(x, ...)
  } else if (data.type == 'sas7bdat' | data.type == 'sas') {
    as.data.frame(haven::read_sas(x, ...))
  } else if (data.type == "sav" | data.type == 'zsav' | data.type == 'por' | data.type == 'sas') {
    as.data.frame(haven::read_spss(x, ...))
  } else if (data.type == "dta" | data.type == "stata") {
    as.data.frame(haven::read_stata(x, ...))
  } else if (data.type == 'shp' | data.type == "shape") {
    sf::st_read(x, ...) #' ~/path/to/file.shp'
  } else if (data.type == "xls" | data.type == 'xlsx' | data.type == 'excel'){
    as.data.frame(readxl::read_excel(x, ...))
  } else {
    utils::read.table(x, sep='\t', ...)
  }
}

# Load main data table into working environment
load_data <- function(project, name = NULL) {
  #' Load data from FishSET database into working environment
  #' @param project String, name of project.
  #' @param name Optional, name of table in FishSET database. Use this argument
  #'  if pulling raw or dated table (not the working table).
  #' @export
  #' @return Data loaded to working environment as the project and ‘MainDataTable’.
  #' @details
  #'   Pulls the primary data table from the FishSET database and loads it into the working 
  #'   environment as the project and MainDataTable. For example, if the project was pollock, 
  #'   then data would be saved to the working environment as 'pollockMainDataTable'.
  #' @examples
  #' \dontrun{
  #' load_data('pollock')
  #' load_data('pollock', 'pollockMainDataTable20190101')
  #' }

  if (is.null(name)) {
    if (table_exists(paste0(project, "MainDataTable")) == FALSE) {
      warning("Table not found")
      tables_database()
    } else {
      dat <- table_view(paste0(project, "MainDataTable"))
    }
  } else {
    if (table_exists(paste0(name)) == FALSE) {
      warning("Table not found")
      tables_database()
    } else {
      dat <- table_view(name)
    }
    hack <- function(key, val, pos){
      assign(key,val, envir=as.environment(pos)
      )} 
    hack(paste0(project, "MainDataTable"), dat, 1L)
    #
   # assign(paste0(project, "MainDataTable"), dat, envir = .GlobalEnv)
  }


  # Log the function
  load_data_function <- list()
  load_data_function$functionID <- "load_data"
  load_data_function$args <- list(project, name)

  log_call(load_data_function)

  return(dat)
}

# Save modified data to FishSET database
save_dat <- function(dat, project) {
  #' Save modified primary data table to FishSET database
  #' @param dat Name of data frame in working environment to save to FishSET database.
  #' @param project String, name of project.
  #' @details Use function to save modified data to the FishSET database. The primary data is only saved
  #'   automatically in data upload and data check functions. It is therefore, advisable to save the modified
  #'   data to the database before moving on to modeling functions. Users should use primary data in the
  #'   working environment for assessing data quality issues, modifying the data, and generating new variables.
  #'   Pulling the primary data from the FishSET database on each function without manually saving will result in a loss of changes.
  #' @importFrom DBI dbWriteTable dbDisconnect
  #' @export
  #' @examples
  #' \dontrun{
  #' save_dat(pollockMainDataTable, 'pollock')
  #' }

  suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
  DBI::dbWriteTable(fishset_db, paste0(project, "MainDataTable"), dat, overwrite = TRUE)
  DBI::dbWriteTable(fishset_db, paste0(project, "MainDataTable_mod", format(Sys.Date(), format = "%Y%m%d")), dat, overwrite = TRUE)
  DBI::dbDisconnect(fishset_db)
}

fishset_compare <- function(x, y, compare = c(TRUE, FALSE)) {
  #' Compare loaded data table to previously saved version of data table
  #' @param x Updated data table to be saved.
  #' @param y Previously saved version of data table.
  #' @param compare Logical, if TRUE, compares \code{x} to \code{y} before saving \code{x} to FishSET database.
  #' @export
  #' @importFrom DBI dbConnect dbDisconnect dbListTables
  #' @details Function is optional. It is designed to check for consistency between versions of the same data frame
  #' so that the logged functions can be used to rerun the previous analysis on the updated data.
  #' The column names, including spelling and capitalization, must match the previous version
  #' to use the logged functions to rerun code after data has been updated (i.e., new year of data).
  #' The function is called by the data import functions (\code{\link{load_maindata}},
  #' \code{\link{load_port}}, \code{\link{load_aux}}, \code{\link{load_grid}}).
  #'  Set the \code{compare} argument to TRUE to compare column names of the new and
  #' previously saved data tables. The new data tables will be saved to the FishSET database if column names match.
  #' Set the \code{compare} argument to FALSE if no previous versions of the data table exist in the FishSET database.
  #' No comparison will be made and the new file will be saved to the database.

  fishset_db <- suppressWarnings(DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
  if (compare == TRUE) {
    if (is.null(y) == TRUE | table_exists(y) == FALSE) {
      print(DBI::dbListTables(fishset_db))
      warning(paste(y, "not defined or does not exist. Consider using one of the tables listed above that exist in the database."))
    } else {
      old <- table_view(y)
      new <- toupper(colnames(x))
      old <- toupper(colnames(old))
      if (is.na(table(is.na(match(new, old)))[2]) == TRUE) {
        cat("Column names match between previous and new data sets. New data frame uploaded to database")
        # DBI::dbWriteTable(fishset_db, paste(paste(deparse(substitute(x)),Sys.Date(), sep=''), deparse(substitute(x))))
      } else {
        cat(length(table(is.na(match(new, old)))[2]), "/", length(match(new, old)), " mismatches", sep = "")
        cat(noquote(paste(deparse(substitute(x)), "[", as.character(which(is.na(match(new, old)) == TRUE)), "]", sep = "")), ":", new[which(is.na(match(
          new,
          old
        )) == TRUE)])
        cat(noquote(paste(deparse(substitute(y)), "[", as.character(which(is.na(match(new, old)) == TRUE)), "]", sep = "")), ":", old[which(is.na(match(
          new,
          old
        )) == TRUE)])
        warning("Column names did not match. Data set will not be uploaded to database")
      }
    }
  } else {
    cat("")
  }
  DBI::dbDisconnect(fishset_db)
}

load_maindata <- function(dat, over_write = TRUE, project, compare = FALSE, y = NULL) {
  #' Load, parse, and save data to the FishSET database
  #'
  #' Load, parse, and save primary dataset to the FishSET database.
  #' @param dat Primary data containing information on hauls or trips.
  #'   Table in the FishSET database contains the string 'MainDataTable'.
  #' @param over_write Logical, If TRUE, saves data over previously saved data table in the FishSET database.
  #' @param project String, name of project.
  #' @param compare Logical, whether to compare new data frame to previously saved data frame \code{y}.
  #' @param y Name of previously saved table in FishSET database. \code{y} must be defined if \code{compare} is TRUE.
  #' @importFrom jsonlite toJSON
  #' @importFrom DBI dbConnect dbDisconnect dbWriteTable
  #' @export
  #' @details Runs the \code{fishset_compare} function if \code{compare} is TRUE and calls the
  #'  \code{\link{data_verification}} function to check for common data issues and that latitude
  #'  and longitude are defined. Then generates an index table that contains units,
  #'  data format, and information on specialized variables. Finally, the datasets (main and index tables)
  #'  are saved in the FishSET database as raw and working tables. In both cases, the table name is the \code{project}
  #'  and the table type, 'MainDataTable' or 'MainDataTableInfo'. Date is also attached to the name for the
  #'  raw data. The main data is also loaded into the working environment as ‘projectMainDataTable’.
  #'
  #' @examples
  #' \dontrun{
  #' load_maindata(dat = Mydata, over_write = TRUE, project = 'pollock',
  #'               compare = TRUE, y = 'MainDataTable01012011')
  #' }

  dataset <- dat
  # Call in datasets
  suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase()))

  if (compare == TRUE) {
    fishset_compare(dataset, y, compare)
  }
  ## -----------MainDataTable--------------------##
  #QC
  check <- 0
  # check that names are unique in dataset
  x <- colnames(dataset)
    if (length(x) == length(unique(x)) & length(toupper(x)) != length(unique(toupper(x)))) {
    warning("\nData set will not be saved to database. 
        Duplicate case-insensitive column names. Sqlite column names are case insensitive.")
    check <- 1
  } else if(length(x) != length(unique(x))) {
    warning("\nVariable names are not unique.\n")
    check <- 1
  }
  
  
  if (any(grepl("area|zone", names(dataset), ignore.case = TRUE)) == FALSE & 
      (any(grepl("lat", names(dataset), ignore.case = TRUE)) == FALSE |
       any(grepl("lon", names(dataset), ignore.case = TRUE)) ==  FALSE)) {
    warning("Neither Latitude/Longitude or Area/Zone variables are included. Data will not be saved.")
    check = 1
  }
  
  if(check == 1) { 
    warning('Dataset not saved. Check that column names are case-insensitive unique and that latitude/longitude
          or area/zone are included.')
  } else {
      
    n <- grep("DATE", colnames(dataset), ignore.case = TRUE)
    for (i in 1:length(n)) {
      dataset[, n[i]] <- format(date_parser(dataset[, n[i]]), "%Y-%m-%d %H:%M:%S")
    }


  ## --------- MainDataTableInfo -------------- ##
  MainDataTableInfo <- data.frame(
    variable_name = colnames(dataset), 
    units = c(ifelse(grepl("DATE|TRIP_END|TRIP_START", colnames(dataset), ignore.case = TRUE), "yyyymmdd", 
                     ifelse(grepl("MIN", colnames(dataset), ignore.case = TRUE), "min", 
                            ifelse(grepl("FATHOMS", colnames(dataset)), "fathoms", 
                                   ifelse(grepl("HOURS|CHINOOK|CHUM|PROPORTION|SIZE", colnames(dataset), ignore.case = TRUE), "numeric", 
                                          ifelse(grepl("DOLLARS", colnames(dataset), ignore.case = TRUE), "dollars", 
                                                 ifelse(grepl("POUNDS|LBS", colnames(dataset), ignore.case = TRUE), "lbs", 
                                                        ifelse(grepl("Lon|Lat|", colnames(dataset), ignore.case = TRUE), "decimal degrees", 
                                                               ifelse(grepl("PERCENT",colnames(dataset), ignore.case = TRUE), "percent", 
                                                                      ifelse(grepl("MT", colnames(dataset), ignore.case = TRUE), "metric tons", 
                                                                             ifelse(grepl("WEEK",colnames(dataset), ignore.case = TRUE), "WK", 
                                                                                    ifelse(grepl("WEEK", colnames(dataset), ignore.case = TRUE), "Y/N", NA)))))))))))), 
    generalType = c(ifelse(grepl("DATE|MIN", colnames(dataset), ignore.case = TRUE), "Time", 
                           ifelse(grepl("IFQ", colnames(dataset), ignore.case = TRUE), "Flag", 
                                  ifelse(grepl("ID", colnames(dataset),ignore.case = TRUE), "Code", 
                                         ifelse(grepl("Long|Lat", colnames(dataset), ignore.case = TRUE), "Latitude", 
                                                ifelse(grepl("TYPE|PROCESSOR|LOCATION|METHOD",colnames(dataset), ignore.case = TRUE), "Code String", 
                                                       ifelse(grepl("CHINOOK|CHUM|FATHOMS|DOLLARS|LBS|PROPORTION|VALUE|PERCENT|MT", colnames(dataset), ignore.case = TRUE), "Other Numeric", 
                                                              ifelse(grepl("HAUL|AREA|PERFORMANCE|PERMIT", colnames(dataset), ignore.case = TRUE), "Code Numeric", NA)))))))),
    isXY = ifelse(grepl("HOURS|CHINOOK|CHUM|PROPORTION|SIZE", colnames(dataset), ignore.case = TRUE), 1, 0), 
    isID = ifelse(grepl("ID", colnames(dataset), ignore.case = TRUE), 1, 0),
    variable_link = rep(NA, length(colnames(dataset))), 
    isTime = ifelse(grepl("DATE|MIN", colnames(dataset), ignore.case = TRUE),1, 0), 
    isCatch = ifelse(grepl("CATCH|POUNDS|LBS", colnames(dataset), ignore.case = TRUE), 1, 0), 
    isEffort = ifelse(grepl("DURATION", colnames(dataset),ignore.case = TRUE), 1, 0), 
    isCPUE = rep(0, length(colnames(dataset))), 
    isLon = ifelse(grepl("LON", colnames(dataset), ignore.case = TRUE),1, 0), 
    isLat = ifelse(grepl("LAT", colnames(dataset), ignore.case = TRUE), 1, 0), 
    isValue = ifelse(grepl("DOLLARS", colnames(dataset), ignore.case = TRUE), 1, 0), 
    isZoneArea = ifelse(grepl("AREA", colnames(dataset), ignore.case = TRUE), 1, 0), 
    isPort = ifelse(grepl("PORT", colnames(dataset), ignore.case = TRUE), 1, 0), 
    isPrice = rep(0, length(colnames(dataset)), ignore.case = TRUE), 
    isTrip = ifelse(grepl("TRIP", colnames(dataset), ignore.case = TRUE), 1, 0), 
    isHaul = ifelse(grepl("HAUL", colnames(dataset), ignore.case = TRUE), 1, 0),
    isOther = rep(0, length(colnames(dataset))), 
    tableLink = rep(NA, length(colnames(dataset)))
  )

  if (table_exists(paste0(project, "MainDataTable", format(Sys.Date(), format = "%Y%m%d"))) == FALSE | over_write == TRUE) {
    DBI::dbWriteTable(fishset_db, paste0(project, "MainDataTable", format(Sys.Date(), format = "%Y%m%d")), dataset, overwrite = over_write)
    DBI::dbWriteTable(fishset_db, paste0(project, "MainDataTableInfo", format(Sys.Date(), format = "%Y%m%d")), MainDataTableInfo, overwrite = over_write)
    print("Table saved to database")
  } else { 
    warning(paste0(project, "MainDataTable", format(Sys.Date(), format = "%Y%m%d"), " was not saved. Table exists in database. Set over_write to TRUE."))
  }
  if (table_exists(paste0(project, "MainDataTable")) == FALSE | over_write == TRUE) {
    DBI::dbWriteTable(fishset_db, paste0(project, "MainDataTable_raw"), dataset, overwrite = over_write)
    DBI::dbWriteTable(fishset_db, paste0(project, "MainDataTable"), dataset, overwrite = over_write)
    DBI::dbWriteTable(fishset_db, paste0(project, "MainDataTableInfo"), MainDataTableInfo, overwrite = over_write)

     # log function
  load_maindata_function <- list()
  load_maindata_function$functionID <- "load_maindata"
  load_maindata_function$args <- list(deparse(substitute(dat)), over_write, project, compare, y)

  log_call(load_maindata_function)
  
  #Make data availlable
  hack <- function(key, val, pos){
    assign(key,val, envir=as.environment(pos)
           )} 
  hack(paste0(project, "MainDataTable"), dataset, 1L)

   message("\n! Data saved to database as ", paste0(project, "MainDataTable", format(Sys.Date(), format = "%Y%m%d")), " (raw) and ", 
       paste0(project, "MainDataTable"), " (working). \nTable is also in the working environment. !"
  )

  } else {
    warning(paste0(project, "MainDataTable was not saved. Table already exists in database. Set over_write to TRUE."))
  }
  DBI::dbDisconnect(fishset_db)

   }
}

main_mod <- function(dat, x, new.unit = NULL, new.type = NULL, new.class = NULL) {
  #' Modify the data index table
  #'
  #' Modify the data index (MainDataTableInfo) table
  #' 
  #' @param dat Table containing information on variables in the primary dataset.
  #'   Table in FishSET database should contain the string 'MainDataTableInfo'.
  #' @param x Name of variable in the MainDataTableInfo table that is to be modified.
  #' @param new.unit Units. Categories include: \code{"fathoms"}, \code{"decimal degrees"}, \code{"dollars"}, \code{"lbs"}, \code{"metric tons"},
  #'   \code{"min"}, \code{"numeric"}, \code{"percent"}, \code{"WK"}, \code{"Y/N"}, \code{"yyyymmdd"}.
  #' @param new.type General type. Categories include: \code{"Time"}, \code{"Flag"}, \code{"Code"}, \code{"Latitude"}, \code{"Code String"}, \code{"Other Numeric"},
  #'   \code{"Code Numeric"}.
  #' @param new.class Specialized variable category. Standard categories include: \code{"isCPUE"}, \code{"isLon"}, \code{"isLat"},
  #'   \code{"isValue"}, \code{"isZoneArea"}, \code{"isPort"}.
  #' @details  Modify the units \code{(new.unit)}, data format \code{(new.type)}, and specialized variable
  #'   class \code{(new.class)} of the MainDataTableInfo table. Updated MainDataTableInfo file is saved to the
  #'   FishSET database. It is advisable to use the working table and not the raw table as the
  #'   modifications will automatically be saved over the input table name.
  #' @export
  #' @examples
  #' \dontrun{
  #' main_mod('pollockMainDataTableInfo01012011', x = 'DISEMBARKED_PORT',
  #'          new.unit = 'yyyymmdd', new.type = 'Other', new.class = 'isPort')
  #' }

  # Call in data sets
  suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
  if (is.character(dat) == TRUE) {
    if (is.null(dat) == TRUE | table_exists(dat) == FALSE) {
      print(DBI::dbListTables(fishset_db))
      stop(paste(dat, "not defined or does not exist. Consider using one of the tables listed above that exist in the database."))
    } else {
      dataset <- table_view(dat)
    }
  } else {
    dataset <- dat
  }
  DBI::dbDisconnect(fishset_db)


  if (!is.null(new.unit)) {
    dataset[dataset[["variable_name"]] == x, "units"] <- new.unit
  }
  if (!is.null(new.type)) {
    dataset[dataset[["variable_name"]] == x, "generalType"] <- new.type
  }
  if (!is.null(new.class)) {
    dataset[dataset[["variable_name"]] == x, c(4, 5, 7:19)] <- 0
    dataset[dataset[["variable_name"]] == x, new.class] <- 1
  }

  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase())
  DBI::dbWriteTable(fishset_db, dataset, dataset, overwrite = TRUE)
  DBI::dbDisconnect(fishset_db)
  print("Data saved to database")

  # log function
  main_mod_function <- list()
  main_mod_function$functionID <- "main_mod"
  main_mod_function$args <- list(deparse(substitute(dat)), deparse(substitute(x)), new.unit, new.type, new.class)

  log_call(main_mod_function)
  return(dataset)
}

load_port <- function(dat, port_name, over_write = TRUE, project = NULL, compare = FALSE, y = NULL) {
  #' Load, parse, and save port data to FishSET database
  #'
  #' @param dat Dataset containing port data. At a minimum, must include three columns, the port names, and the latitude and longitude of ports.
  #' @param port_name Variable containing port names. Names should match port names in primary dataset.
  #' @param over_write Logical, if TRUE, saves over data table previously saved in the FishSET database.
  #' @param project String, name of project.
  #' @param compare Logical, should new data be compared to previously saved data frame \code{y}.
  #' @param y Name of previously saved table in FishSET database. \code{y} must be defined if \code{compare} is TRUE.
  #' @export
  #' @importFrom jsonlite toJSON
  #' @importFrom DBI dbConnect dbDisconnect dbWriteTable
  #' @details Runs a series of checks on the port data. The function checks that
  #'   each row is unique, that no variables are empty, and that column names are case-insensitive unique. 
  #'   There data issues are resolved before the data is saved to the database.
  #'  If checks pass, runs the fishset_compare function and
  #' saves the new data frame to the FishSET database.  The data is saved in the FishSET database
  #' as the raw data and the working data. In both cases, the table name is the `project` and `PortTable`.
  #' Date is also attached to the name for the raw data.
  #' @examples
  #' \dontrun{
  #' load_port(PortTable, over_write = TRUE, project  ='pollock',
  #'            compare = TRUE, y = 'pollockPortTable01012011')
  #' }

  val <- 0
  x <- dat
  if (all(grepl("Lon", names(x), ignore.case = TRUE) == FALSE) == TRUE) {
    warning("Latitude and Longitude must be specified")
    val <- 1
  }
  if (is.na(table(grepl("Lon", names(x), ignore.case = TRUE))[2]) == FALSE & table(grepl("Lon", names(x), ignore.case = TRUE))[2] > 1) {
    warning("Multiple latitude or longitude columns. Only one allowed.")
    val <- 1
  }
  if (all(grepl("name|id|code|PORT", names(x), ignore.case = TRUE) == FALSE) == TRUE) {
    warning("Port identification not found. Check that unique port ID (name, id, code) is included.")
    val <- 1
  }

  if (!is.numeric(port_name)) {
    colnames(x)[grep(port_name, colnames(x))] <- "Port_Name"
  } else {
    colnames(x)[port_name] <- "Port_Name"
  }

  colnames(x)[grep("LON", colnames(x), ignore.case = TRUE)] <- "Port_Long"
  colnames(x)[grep("LAT", colnames(x), ignore.case = TRUE)] <- "Port_Lat"

  #data_verification_call(x, project)
  #unique rows
  if(dim(x)[1] != dim(unique(x))[1]){
    print('Duplicate rows found and removed.')
    x <- unique(x)
  }
  #unique column names
  if(length(toupper(colnames(x))) != length(unique(toupper(colnames(x))))){
    print('Duplicate case-insensitive column names found. Duplicate column names adjusted.')
    colnames(x)[which(duplicated(colnames(x)))] <- paste0(colnames(x)[which(duplicated(colnames(x)))], '.1')
  }
  #empty variables
  if (any(apply(x, 2, function(x) all(is.na(x))) == TRUE)) {
    print(names(which(apply(x, 2, function(x) all(is.na(x))) == TRUE)), 'is empty and was removed.')
    x <- x[,-(which(apply(x, 2, function(x) all(is.na(x))) == TRUE))]
  }

  if (compare == TRUE) {
    fishset_compare(x, y, compare)
  }

  if (val == 0) {
    suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
    if (table_exists(paste0(project, "PortTable")) == FALSE | over_write == TRUE) {
      DBI::dbWriteTable(fishset_db, paste0(project, "PortTable", format(Sys.Date(), format = "%Y%m%d")), x, overwrite = over_write)
      DBI::dbWriteTable(fishset_db, paste0(project, "PortTable"), x, overwrite = over_write)
    } else {
      warning(paste("Table not saved.", paste0(project, "PortTable"), "exists in database, and overwrite is FALSE."))
    }
    DBI::dbDisconnect(fishset_db)
    print("Data saved to database")

    load_port_function <- list()
    load_port_function$functionID <- "load_port"
    load_port_function$args <- list(deparse(substitute(dat)), deparse(substitute(port_name)), over_write, project, compare, deparse(substitute(y)))
    load_port_function$kwargs <- list()
    load_port_function$output <- c("")
    log_call(load_port_function)
  }
}

load_aux <- function(dat, aux, x, over_write = TRUE, project = NULL) {
  #' Load, parse, and save auxiliary data to FishSET database
  #'
  #' Auxiliary data is additional data that connects the primary dataset.
  #' Function pulls the data, parses it, and then and saves the data to the FishSET database.
  #' @param dat Primary data containing information on hauls or trips.
  #'   Table in the FishSET database contains the string 'MainDataTable'.
  #' @param aux Auxiliary data to be saved to the FishSET database.
  #' @param x Name of auxiliary data frame to be saved.
  #' @param over_write Logical, If TRUE, saves data over previously
  #'   saved data table in the FishSET database.
  #' @param project String, name of project.
  #' @importFrom jsonlite toJSON
  #' @importFrom DBI dbConnect dbDisconnect dbWriteTable
  #' @export
  #' @details Auxiliary data is any additional data required beyond the primary data and the port data.
  #'   Auxiliary data can be any data that can be merged with the primary dataset (ex. prices by date, vessel
  #'   characteristics, or fishery season). The auxiliary data does not have to be at a haul or trip level
  #'   but must contain a variable to connect the auxiliary data to the primary dataset. The function checks
  #'  that at least one column name of the auxiliary data matches a column name in the primary dataset. The function checks that
  #'   each row is unique, that no variables are empty, and that column names are case-insensitive unique. 
  #'   There data issues are resolved before the data is saved to the database.
  #'   The data is saved in the FishSET database as the raw data and the working data. In
  #'  both cases, the table name is the \code{project} and the file name \code{x}. Date is also added to the name for the raw data.
  #' @examples
  #' \dontrun{
  #' load_aux(pcodMainDataTable, x = FisherySeason, over_write = TRUE, project = 'pcod')
  #' }

  # Call in datasets
  val <- 0

  suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
  if (is.character(dat) == TRUE) {
    if (is.null(dat) == TRUE | table_exists(dat) == FALSE) {
      print(DBI::dbListTables(fishset_db))
      warning(paste(dat, "not defined or does not exist. Consider using one of the tables listed above that exist in the database."))
      val <- 1
    } else {
      old <- table_view(dat)
    }
  } else {
    old <- dat
  }
  DBI::dbDisconnect(fishset_db)


  if (any(colnames(aux) %in% colnames(old)) == FALSE) {
    warning("No shared columns. Column names do not match between two data sets.")
    val <- 1
  }
  if (val == 0) {
    #data_verification_call(x, project)
    #unique rows
    if(dim(aux)[1] != dim(unique(aux))[1]){
      print('Duplicate rows found and removed.')
      aux <- unique(aux)
    }
    #unique column names
    if(length(toupper(colnames(aux))) != length(unique(toupper(colnames(aux))))){
      print('Duplicate case-insensitive column names found. Duplicate column names adjusted.')
      colnames(aux)[which(duplicated(colnames(aux)))] <- paste0(colnames(aux)[which(duplicated(colnames(aux)))], '.1')
    }
    #empty variables
    if (any(apply(aux, 2, function(x) all(is.na(x))) == TRUE)) {
      print(names(which(apply(aux, 2, function(x) all(is.na(x))) == TRUE)), 'is empty and was removed.')
      aux <- aux[,-(which(apply(aux, 2, function(x) all(is.na(x))) == TRUE))]
    }
    

    fishset_db <- suppressWarnings(DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
    if (table_exists(paste0(project, x)) == FALSE | over_write == TRUE) {
      DBI::dbWriteTable(fishset_db, paste0(project, x, format(Sys.Date(), format = "%Y%m%d")), aux, overwrite = over_write)
      DBI::dbWriteTable(fishset_db, paste0(project, x), aux, overwrite = over_write)
      print("Data saved to database")
    } else {
      warning(paste("Table not saved.", paste0(project, x), "exists in database, and overwrite is FALSE."))
    }

    DBI::dbDisconnect(fishset_db)

    load_aux_function <- list()
    load_aux_function$functionID <- "load_aux"
    load_aux_function$args <- list(deparse_name(dat), deparse_name(aux), x, over_write, project)
    log_call(load_aux_function)
  }
}

load_grid <- function(dat, grid, x, over_write = TRUE, project = NULL) {
  #' Load, parse, and save gridded data to FishSET database
  #'
  #' Gridded data is data that varies by two dimensions. Column names must be zone names. Load, parse, and save gridded data to FishSET database
  #' @param dat Primary data containing information on hauls or trips. 
  #'   Table in FishSET database contains the string 'MainDataTable'.
  #' @param grid Gridded data to be saved to FishSET database. 
  #' @param x Name of gridded data frame to be saved.
  #' @param over_write Logical, If TRUE, saves dat over previously saved data table in the FishSET database.
  #' @param project String, name of project.
  #' @details Grid data is an optional data frame that contains a variable that varies by the map grid (ex.
  #'   sea surface temperature, wind speed). Data can also vary by a second dimension (e.g., date/time). Both
  #'   dimensions in the gridded data file need to be variables included in the primary dataset.
  #'   The grid locations (zones) must define the columns and the optional second dimension defines the rows.
  #'   The row variable must have the exact name as the variable
  #'   in the main data frame that it will be linked to. 
  #'   The function checks that each row is unique, that no variables are empty, and that column names are case-insensitive unique. 
  #'   There data issues are resolved before the data is saved to the database.The data is saved in the FishSET database as the raw
  #'   data and the working data. In both cases, the table name is the \code{project} and the file name \code{x}.
  #'   Date is attached to the name for the raw data.
  #' @export
  #' @examples
  #' \dontrun{
  #' load_grid(dat = 'pcodMainDataTable', x = SeaSurfaceTemp, over_write = TRUE, project = 'pcod')
  #' }
  val <- 0
  fishset_db <- suppressWarnings(DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
  if (is.character(dat) == TRUE) {
    if (is.null(dat) == TRUE | table_exists(dat) == FALSE) {
      print(DBI::dbListTables(fishset_db))
      warning(paste(dat, "not defined or does not exist. Consider using one of the tables listed above that exist in the database."))
      val <- 1
    } else {
      old <- table_view(dat)
    }
  } else {
    old <- dat
  }
  DBI::dbDisconnect(fishset_db)

  if (any(colnames(grid) %in% colnames(old)) == FALSE) {
    warning("No shared columns. Column names do not match between two data sets. Gridded data set not saved.")
    val <- 1
  }

  if (val == 0) {
    #data_verification_call(x, project)
    #unique rows
    if(dim(grid)[1] != dim(unique(grid))[1]){
      print('Duplicate rows found and removed.')
      grid <- unique(grid)
    }
    #unique column names
    if(length(toupper(colnames(grid))) != length(unique(toupper(colnames(grid))))){
      print('Duplicate case-insensitive column names found. Duplicate column names adjusted.')
      colnames(grid)[which(duplicated(colnames(grid)))] <- paste0(colnames(grid)[which(duplicated(colnames(grid)))], '.1')
    }
    #empty variables
    if (any(apply(grid, 2, function(x) all(is.na(x))) == TRUE)) {
      print(names(which(apply(grid, 2, function(x) all(is.na(x))) == TRUE)), 'is empty and was removed.')
      grid <- grid[,-(which(apply(grid, 2, function(x) all(is.na(x))) == TRUE))]
    }
    

    fishset_db <- suppressWarnings(DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
    if (table_exists(paste0(project, x)) == FALSE | over_write == TRUE) {
      DBI::dbWriteTable(fishset_db, paste0(project, x), grid, overwrite = over_write)
      print("Data saved to database")
    } else {
      warning(paste("Table not saved.", paste0(project, x), "exists in database, and overwrite is FALSE."))
    }
    DBI::dbDisconnect(fishset_db)

    load_gridded_function <- list()
    load_gridded_function$functionID <- "load_grid"
    load_gridded_function$args <- list(deparse_name(dat), deparse_name(grid), x, over_write, project)
    log_call(load_gridded_function)
  }
}

dataindex_update <- function(dat, dataindex) {
  #' Update MainDataTableInfo
  #'
  #' Automates updating the dataindex (MainDataTableInfo) table saved to the FishSET database after modifying the primary dataset. Function should be run after variables have been added or removed from the primary dataset.
  #'
  #' @param  dat Primary data containing information on hauls or trips.
  #'   Table in the FishSET database contains the string 'MainDataTable'.
  #' @param dataindex String, dataindex table name in the FishSET database.
  #'   Table name is usually the project and ‘MainDataTableInfo’. Name must be in quotes.
  #' @importFrom DBI dbConnect dbDisconnect dbWriteTable
  #' @export
  #' @details The MainDataTableInfo table is first created when the MainDataTable is loaded and saved to the
  #'   FishSET database. However, this table may not match the variables in \code{dat} after the FishSET variable
  #'   creation functions have been run. It may be necessary to update the MainDataTableInfo table in the FishSET database.
  #'   Running this function adds information on variables created using the FishSET data creation functions.
  #' @examples
  #' \dontrun{
  #' dataindex_update(dat = pcodMainDataTable, dataindex = 'pcodMainDataTableInfo')
  #' }

  fishset_db <- suppressWarnings(DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
  if (is.character(dat) == TRUE) {
    if (is.null(dat) == TRUE | table_exists(dat) == FALSE) {
      print(DBI::dbListTables(fishset_db))
      stop(paste(dat, "not defined or does not exist. Consider using one of the tables listed above that exist in the database."))
    } else {
      dataset <- table_view(dat)
    }
  } else {
    dataset <- dat
  }

  MainDataTableInfo <- data.frame(
    variable_name = colnames(dataset), units = c(ifelse(grepl("DATE|TRIP_END|TRIP_START", colnames(dataset), ignore.case = TRUE),
      "yyyymmdd", ifelse(grepl("MIN", colnames(dataset), ignore.case = TRUE), "min", ifelse(grepl("FATHOMS", colnames(dataset)), "fathoms", ifelse(grepl("HOURS|CHINOOK|CHUM|PROPORTION|SIZE",
        colnames(dataset),
        ignore.case = TRUE
      ), "numeric", ifelse(grepl("DOLLARS", colnames(dataset), ignore.case = TRUE), "dollars", ifelse(grepl("POUNDS|LBS",
        colnames(dataset),
        ignore.case = TRUE
      ), "lbs", ifelse(grepl("Lon|Lat|", colnames(dataset), ignore.case = TRUE), "decimal degrees", ifelse(grepl("PERCENT",
        colnames(dataset),
        ignore.case = TRUE
      ), "percent", ifelse(grepl("MT", colnames(dataset), ignore.case = TRUE), "metric tons", ifelse(grepl("WEEK",
        colnames(dataset),
        ignore.case = TRUE
      ), "WK", ifelse(grepl("WEEK", colnames(dataset), ignore.case = TRUE), "Y/N", NA))))))))))
    )), generalType = c(ifelse(grepl("DATE|MIN",
      colnames(dataset),
      ignore.case = TRUE
    ), "Time", ifelse(grepl("IFQ", colnames(dataset), ignore.case = TRUE), "Flag", ifelse(grepl("ID", colnames(dataset),
      ignore.case = TRUE
    ), "Code", ifelse(grepl("Long|Lat", colnames(dataset), ignore.case = TRUE), "Latitude", ifelse(grepl("TYPE|PROCESSOR|LOCATION|METHOD",
      colnames(dataset),
      ignore.case = TRUE
    ), "Code String", ifelse(grepl("CHINOOK|CHUM|FATHOMS|DOLLARS|LBS|PROPORTION|VALUE|PERCENT|MT", colnames(dataset),
      ignore.case = TRUE
    ), "Other Numeric", ifelse(grepl("HAUL|AREA|PERFORMANCE|PERMIT", colnames(dataset), ignore.case = TRUE), "Code Numeric", NA)))))))),
    isXY = ifelse(grepl("HOURS|CHINOOK|CHUM|PROPORTION|SIZE", colnames(dataset), ignore.case = TRUE), 1, 0), isID = ifelse(grepl("ID", colnames(dataset),
      ignore.case = TRUE
    ), 1, 0), variable_link = rep(NA, length(colnames(dataset))), isTime = ifelse(grepl("DATE|MIN", colnames(dataset), ignore.case = TRUE),
      1, 0
    ), isCatch = ifelse(grepl("CATCH|POUNDS|LBS", colnames(dataset), ignore.case = TRUE), 1, 0), isEffort = ifelse(grepl("DURATION", colnames(dataset),
      ignore.case = TRUE
    ), 1, 0), isCPUE = rep(0, length(colnames(dataset))), isLon = ifelse(grepl("LON", colnames(dataset), ignore.case = TRUE),
      1, 0
    ), isLat = ifelse(grepl("LAT", colnames(dataset), ignore.case = TRUE), 1, 0), isValue = ifelse(grepl("DOLLARS", colnames(dataset), ignore.case = TRUE),
      1, 0
    ), isZoneArea = ifelse(grepl("AREA", colnames(dataset), ignore.case = TRUE), 1, 0), isPort = ifelse(grepl("PORT", colnames(dataset), ignore.case = TRUE),
      1, 0
    ), isPrice = rep(0, length(colnames(dataset)), ignore.case = TRUE), isTrip = ifelse(grepl("TRIP", colnames(dataset), ignore.case = TRUE),
      1, 0
    ), isHaul = ifelse(grepl("HAUL", colnames(dataset), ignore.case = TRUE), 1, 0), isOther = rep(0, length(colnames(dataset))), tableLink = rep(
      NA,
      length(colnames(dataset))
    )
  )


  DBI::dbWriteTable(fishset_db, dataindex, MainDataTableInfo, overwrite = TRUE)
  DBI::dbDisconnect(fishset_db)

  return(MainDataTableInfo)
}
