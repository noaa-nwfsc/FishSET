#  Import data
#str_trim

read_dat <- function(x, data.type=NULL, is.map = FALSE, drv = NULL, dbname = NULL, user = NULL, password = NULL, ...) {
  #' Import data from local file directory or webpage into the R environment
  #' @param x Name and path of dataset to be read in. To load data directly from a webpage, \code{x} should be the web address.  
  #' @param data.type Optional. Data type can be defined by user or based on the file extension.
  #'    If undefined, \code{data.type} is the string after the last period or equal sign. \code{data.type} must be 
  #'    defined if \code{x} is the path to a shape folder, if the file is a google spreadsheet use \code{data.type = 'google'},
  #'     or if the correct extension cannot be derived from \code{x}.
  #'    R, comma-deliminated, tab-deliminated, excel, matlab, json, geojson, sas,
  #'    spss, stata, and html, and XML data extensions do not have to be specified. 
  #' @param is.map logical, set \code{is.map} to TRUE if data is a spatial file.  
  #'   Spatial files ending in .json will not be read in properly unless \code{is.map} is true.
  # @param save 
  #' @param drv Use with sql files. Database driver.
  #' @param dbname Use with sql files. If required, database name.
  #' @param user Use with sql files.  If required, user name for SQL database.
  #' @param password Use with sql files. If required, SQL database password.
  #' @param ... Optional arguments 
  #' @importFrom sf read_sf
  #' @importFrom rgdal readOGR
  #' @importFrom R.matlab readMat
  #' @importFrom jsonlite fromJSON
  #' @importFrom haven read_spss read_stata read_sas
  #' @importFrom utils read.table read.delim
  #' @importFrom readxl read_excel
  #' @importFrom xml2 read_xml read_html
  #' @importFrom RCurl getURL
  #' @importFrom googlesheets4 read_sheet
  #' @importFrom readODS read.ods
  #' @importFrom DBI dbDisconnect dbConnect
  #' @details Uses the appropriate function to read in data based on data type.
  #'   Use \code{\link[FishSET]{write_dat}} to save data to the \code{data} folder in the \code{project} directory.
  #'      
  #'   Supported data types include shape, csv, json, matlab, R, spss, and stata files.
  #'
  #'   Use \code{data.type = 'shape'} if \code{x} is the path to a shape folder. 
  #'   Use \code{data.type = 'google'} if the file is a google spreadsheet.
  #'   
  #'   For sql files, use \code{data.type = 'sql'}. The function will connect to the specified DBI and pull the table. 
  #'   Users must specify the DBI driver (\code{drv}), for example: \code{RSQLite::SQLite()}, \code{RPostgreSQL::PostgreSQL()}, 
  #'   \code{odbc::odbc()}. 
  #'   Further arguments may be required, including database name (\code{dbname}), user id (\code{user}), 
  #'   and password (\code{password}). 
  #'     
  #'   Additional arguments can be added, such as the seperator agument \code{sep=','}, skip lines \code{skip = 2},
  #'   and header \code{header = FALSE}. 
  #'   
  #'   To specify the seperator argument for a deliminated file, include tab-deliminated, specify \code{data.type = 'delim'}.
  #'   
  #'   For more details, see \code{\link[base]{load}} for loading R objects, 
  #'   \code{\link[utils]{read.table}} for reading in comma and tab deliminated files,
  #'   \code{\link[readxl]{read_excel}} for reading in excel files (xls, xlsx), 
  #'   \code{\link[sf]{st_read}} for reading in geojson files, 
  #'   \code{\link[rdgal]{readOGR}} for reading in shape files,
  #'   \code{\link[R.matlab]{readMat}} for reading in matlab data files,
  #'   \code{\link[haven]{read_dta}} for reading in stata data files,
  #'   \code{\link[haven]{read_spss}} for reading in spss data files,
  #'   \code{\link[haven]{read_sas}} for reading in sas data files, and 
  #'   \code{\link[jsonlite]{fromJSON}} for reading in json files.
  #'   See \code{read.delim} in \code{\link[utils]{read.table}} for reading in deliminated files. Include the filed separator character \code{sep = ""}
  #'   \code{\link[xml2]{read_xml}} for reading in XML files. Further processing may be required.
  #'   \code{\link[xml2]{read_html}} for reading in html tables.
  #'   See \code{read_sheet} in \code{\link[googlesheets4]{range_read}} for reading in google spreadsheets.
  #'       Google spreadsheets require \code{data.type} be specified. Use \code{data.type = 'google'}.
  #'   \code{\link[readODS]{read_ods}} for reading in open document spreadsheets.
  #' @export
  #' @examples
  #' \dontrun{
  #' # Read in shape file
  #'   dat <- read_dat('C:/data/nmfs_manage_simple', data.type = 'shape')
  #' # Read in spatial data file in json format
  #'   dat <- read_dat('C:/data/nmfs_manage_simple.json', is.map = TRUE)
  #' # read in data directly from webpage
  #'   dat <- read_dat("https://s3.amazonaws.com/assets.datacamp.com/blog_assets/test.txt", 
  #'      data.type = 'delim', sep='', header = FALSE)
  #'  
  #'  #Save the data to project directory
  #'    write_dat(dat, file_type = "json", project='pollock')
  #' }
  #' 
  
  
  if(is.null(data.type)) {
    if(grepl('=', sub('.*\\.', '', x)) == F){
        data.type <- sub('.*\\.', '', x)
    } else {
       data.type <- sub('.*\\=', '', x)
    }
  }
  
  data.type <- tolower(data.type)
  
  if(data.type == 'json' & is.map == TRUE) {
    data.type = 'geojson'
  }
  
  if (data.type == 'rdata' | data.type == "r") {
    out <- get(load(x))
    if (any(c("sf", "sp") %in% class(out))) {
      out
    } else {
      return(as.data.frame(out))
    }
  } else if (data.type == "rds") {
    readRDS(x, ...)
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
  } else if (data.type == "sav" | data.type == 'sav' | data.type == 'por' | data.type == 'sas') {
    as.data.frame(haven::read_spss(x, ...))
  } else if (data.type == "dta" | data.type == "stata") {
    as.data.frame(haven::read_stata(x, ...))
  } else if (data.type == 'shp' | data.type == "shape") {
    rgdal::readOGR(dsn=x, verbose=FALSE, ...)
  } else if (data.type == "xls" | data.type == 'xlsx' | data.type == 'excel'){
    as.data.frame(readxl::read_excel(x, ...))
  } else if (data.type == 'txt') {
    utils::read.table(x, sep='\t', ...)
  } else if (data.type == 'delim'){
    utils::read.delim(x, ...)
  } else if(data.type == 'xml'){
    xml2::read_xml(x, ...)
  } else if(data.type == 'html'){
    xml2::read_html(RCurl::getURL(x), stringsAsFactors = FALSE, ...)
  } else if(data.type == 'google'){
    googlesheets4::read_sheet(x, ...)
  } else if(data.type == 'ods'){
    readODS::read_ods(x, ...)
  } else if(data.type == 'sql') {
    conn <- DBI::dbConnect(
      drv = drv,
      dbname = dbname,
      user = user,
      password = password,
      options(connectionObserver = NULL)
    )
    out <- DBI::dbGetQuery(conn, paste("SELECT * FROM", x))
    DBI::dbDisconnect(conn)
    return(out)
  } else {
    cat('Data extension not recognized.')
  }

}

write_dat <- function (dat, path=NULL, file_type = "csv", project, ...) {
  #' Write a data table to local file directory
  #'
  #'@param dat Name of data frame in working environment to save to file. 
  #'@param path String, path or connection to write to. If left empty, the file will be written to the dat folder in the project directory.
  #'@param file_type String, the type of file to write to. Options include \code{"csv"},
  #'  \code{"txt"} (tab-separated text file), \code{"xlsx"} (excel), \code{"rdata"}, \code{"json"}, 
  #'  \code{"stata"}, \code{"spss"},
  #'  \code{"sas"}, and \code{"matlab"}.
  #'@param project String, project name. 
  #'@param ... Additional arguments passed to writing function. See "details" for 
  #'  the list of functions. 
  #'@importFrom openxlsx write.xlsx
  #'@importFrom jsonlite write_json
  #'@importFrom haven write_dta write_sav write_sas
  #'@importFrom R.matlab writeMat
  #'@importFrom shiny isRunning
  #'@importFrom utils write.table
  #'@importFrom geojsonio geojson_json
  #'@importFrom sf st_write
  #'@export
  #'@details  
  #' Leave \code{path = NULL} to save \code{dat} to the \code{data} folder in the \code{project} directory
  #'See \code{\link[utils]{write.table}}  for csv and tab-separated files, 
  #'    \code{\link[base]{save}} for R data files, 
  #'    \code{\link[openxlsx]{write.xlsx}},
  #'    \code{\link[jsonlite]{read_json}} for json files, 
  #'    \code{\link[sf]{st_write}} for geojson files,
  #'    \code{\link[haven]{read_dta}} for Stata files, 
  #'    \code{\link[haven]{read_spss}} for SPSS files, 
  #'    \code{\link[haven]{read_sas}} for SAS files, and 
  #'    \code{\link[R.matlab]{writeMat}} for Matlab files. 
  #'@examples
  #'\dontrun{
  #' # Save to the default data folder in project directory
  #'    write_dat(pollockMainDataTable, type = "csv", "pollock")
  #' # Save to defined directory location
  #'    write_dat(pollockMainDataTable, path = "C://data/pollock_dataset.csv", type = "csv", "pollock")
  #' }
  
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main")
  if(is.null(path)){
    path <- loc_data(project = project)
  }
  
  if (file_type == "csv") {
    
    write.csv(dataset, file = path, row.names = FALSE, ...)
    
  } else if (file_type == "txt") { 
    
    utils::write.table(dataset, file = path, sep = "\t", row.names = FALSE, ...)
    
  } else if (file_type == "xlsx") {
    
    openxlsx::write.xlsx(dataset, file = path, ...)
    
  } else if (file_type == "rdata") {
    
    save(dataset, file = path, ...)
    
  } else if (file_type == "json") {
    
    jsonlite::write_json(dataset, path = path, ...)
    
  } else if (file_type == 'geojson') {
   
    sf::st_write(dataset, dsn = paste0(path, dat, ".geojson"))
    
  } else if (file_type == 'shape') {
    #Convert to geojson and then save
    dataset <- geojsonio::geojson_json(dataset)
    sf::st_write(dataset, dsn = paste0(path, dat, ".geojson"))
    
  } else if (file_type == "stata") {
    
    haven::write_dta(dataset, path = path, ...)
    
  } else if (file_type == "spss") {
    
    haven::write_sav(data = dataset, path = path, ...)
    
  } else if (file_type == "sas") {
    # has column length restriction
    haven::write_sas(dataset, path = path, ...)
    
  } else if (file_type == "matlab") {
    # has column length restriction (32 characters)
    R.matlab::writeMat(con = path, dataset, ...)
    
  } else {
    warning("Data extention not recognized.")
    end <- TRUE
  }
  
  if (end == FALSE) {
    
    path <- path.expand(path)
    
    # Log the function
    write_dat_function <- list()
    write_dat_function$functionID <- "write_data"
    write_dat_function$args <- list(dat, path, file_type, project)
    write_dat_function$kwargs <- list(...)
    
    log_call(project, write_dat_function)
    
    message(paste("Table saved to", path))
    invisible(TRUE)
    
  } else {
    
    invisible(FALSE)
  }
}

# Read in main data table from database into working environment
load_data <- function(project, name = NULL) {
  #' Load data from FishSET database into the R environment
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
  #' 

  # check_proj(project)

 hack <- function(key, val, pos){
      assign(key,val, envir=as.environment(pos)
      )} 
  
 dat <- NULL
 
  if (is.null(name)) {
    if (table_exists(paste0(project, "MainDataTable"), project) == FALSE) {
      warning("Table not found")
      tables_database(project)
    } else {
      dat <- table_view(paste0(project, "MainDataTable"), project)
      if (fishset_env_exists() == FALSE)  create_fishset_env()
    }
  } else {
    if (table_exists(name, project) == FALSE) {
      warning("Table not found")
      tables_database(project)
    } else {
      dat <- table_view(name, project)
      if (fishset_env_exists() == FALSE)  create_fishset_env()
    }
   
   #
   # assign(paste0(project, "MainDataTable"), dat, envir = .GlobalEnv)
  }
 if(!is.null(dat)) hack(paste0(project, "MainDataTable"), dat, 1L)
 
  # Log the function
  load_data_function <- list()
  load_data_function$functionID <- "load_data"
  load_data_function$args <- list(project, name)

  log_call(project, load_data_function)

 # return(dat)
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

  suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project)))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  DBI::dbWriteTable(fishset_db, paste0(project, "MainDataTable"), dat, overwrite = TRUE)
  DBI::dbWriteTable(fishset_db, paste0(project, "MainDataTable_mod", format(Sys.Date(), format = "%Y%m%d")), dat, overwrite = TRUE)
  
  invisible(TRUE)
}

fishset_compare <- function(x, y, compare = c(TRUE, FALSE), project) {
  #' Compare imported data table to the previously saved version of the data table
  #' @param x Updated data table to be saved.
  #' @param y Previously saved version of data table.
  #' @param compare Logical, if TRUE, compares \code{x} to \code{y} before saving \code{x} to FishSET database.
  #' @param project Name of project
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

  fishset_db <- suppressWarnings(DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project)))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  if (compare == TRUE) {
    if (is.null(y) == TRUE | table_exists(y, project) == FALSE) {
      print(DBI::dbListTables(fishset_db))
      warning(paste(y, "not defined or does not exist. Consider using one of the tables listed above that exist in the database."))
    } else {
      old <- table_view(y, project)
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
}

load_maindata <- function(dat, project, over_write = TRUE, compare = FALSE, y = NULL) {
  #' Import, parse, and save data to the FishSET database
  #'
  #' @param dat Primary data containing information on hauls or trips.
  #'  Should be the full path to the file or name of table in the FishSET database.
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
  #'  data format, and information on specialized variables. Finally, the dataset
  #'  is saved in the FishSET database as raw and working tables. The table name is the \code{project}
  #'  and the table type, 'MainDataTable'. Date is also attached to the name for the
  #'  raw data. The main data is also loaded into the working environment as ‘projectMainDataTable’.
  #'
  #' @examples
  #' \dontrun{
  #' load_maindata(dat = Mydata, over_write = TRUE, project = 'pollock',
  #'               compare = TRUE, y = 'MainDataTable01012011')
  #' }
  #' 

  if (is.character(dat)) {
    
    dataset <- read_dat(dat)
    
  } else {
    
    dataset <- dat
  }

  if (compare == TRUE) {
    
    fishset_compare(dataset, y, compare, project = project)
  }
  
  # Quality Checks
  check <- TRUE
  
  # check that names are unique in dataset
  x <- colnames(dataset)
  
  if (length(x) == length(unique(x)) & length(toupper(x)) != length(unique(toupper(x)))) {
      
    warning("\nData set will not be saved to database. 
        Duplicate case-insensitive column names. Sqlite column names are case insensitive.")
    check <- FALSE
    
  } else if (length(x) != length(unique(x))) {
    
    warning("\nVariable names are not unique.\n")
    check <- FALSE
  }
  
  if (any(grepl("area|zone", names(dataset), ignore.case = TRUE)) == FALSE & 
      (any(grepl("lat", names(dataset), ignore.case = TRUE)) == FALSE |
       any(grepl("lon", names(dataset), ignore.case = TRUE)) ==  FALSE)) {
    
    warning("Neither Latitude/Longitude or Area/Zone variables are included. Data will not be saved.")
    check <- FALSE
  }
  
  if (check == FALSE) { 
    
    warning('Dataset not saved. Check that column names are case-insensitive unique and that latitude/longitude
          or area/zone are included.')
    
    invisible(FALSE)
    
  } else { # Checks passed
      
    # check if project folder exists
    check_proj(project)
    
    # convert date columns
    n <- grep("DATE", colnames(dataset), ignore.case = TRUE, value = TRUE)
    
    dataset[n] <- lapply(n, function(x) {
      
      format(date_parser(dataset[[x]]), "%Y-%m-%d %H:%M:%S")
    })
    
    # Save tables to FishSET DB
    suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), 
                                                  locdatabase(project = project)))
    on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
    
    raw_tab_name <- paste0(project, "MainDataTable", 
                       format(Sys.Date(), format = "%Y%m%d"))
  
    raw_tab_exists <- table_exists(raw_tab_name, project = project)
    
    if (raw_tab_exists == FALSE | over_write == TRUE) {
      
      DBI::dbWriteTable(fishset_db, raw_tab_name, dataset, overwrite = over_write)
      print("Table saved to database")
      
    } else { 
      
      warning(paste(raw_tab_name, "was not saved. Table exists in database. Set over_write to TRUE."))
    }
    
    work_tab_name <- paste0(project, "MainDataTable")
    
    work_tab_exists <- table_exists(work_tab_name, project = project)
    
    if (work_tab_exists == FALSE | over_write == TRUE) {
      
      DBI::dbWriteTable(fishset_db, work_tab_name, dataset, overwrite = over_write)
  
      # log function
      load_maindata_function <- list()
      load_maindata_function$functionID <- "load_maindata"
      load_maindata_function$args <- list(deparse(substitute(dat)), over_write, 
                                          project, compare, y)
    
      log_call(project, load_maindata_function)
      
      #Make data available
      makeavail <- function(key, val, pos) {
        
        assign(key, val, envir = as.environment(pos))
      } 
      
      makeavail(work_tab_name, dataset, 1L)
    
      message("\n! Data saved to database as ", raw_tab_name, " (raw) and ", 
              work_tab_name, " (working). \nTable is also in the working environment. !")
       
      # add to fishset_env
      if (fishset_env_exists() == FALSE)  create_fishset_env()
       
      edit_fishset_env("dat_name", work_tab_name)
      invisible(TRUE)
    
    } else {
      
      warning(paste0(project, "MainDataTable was not saved. Table already exists",
                     " in database. Set over_write to TRUE."))
      invisible(FALSE)
    }
  }
}


load_port <- function(dat, port_name, project, over_write = TRUE, compare = FALSE, y = NULL) {
  #' Import, parse, and save port data to FishSET database
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

   check <- TRUE
  #Load data is required
  if(is.character(dat)){
    x <- read_dat(dat)
  } else {
    x <- dat
  }
   
  if (all(grepl("Lon", names(x), ignore.case = TRUE) == FALSE) == TRUE) {
    warning("Latitude and Longitude must be specified")
    check <- FALSE
  }
   
  if (is.na(table(grepl("Lon", names(x), ignore.case = TRUE))[2]) == FALSE & table(grepl("Lon", names(x), ignore.case = TRUE))[2] > 1) {
    warning("Multiple latitude or longitude columns. Only one allowed.")
    check <- FALSE
  }
   
  if (all(grepl("name|id|code|PORT", names(x), ignore.case = TRUE) == FALSE) == TRUE) {
    warning("Port identification not found. Check that unique port ID (name, id, code) is included.")
    check <- FALSE
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
    fishset_compare(x, y, compare, project = project)
  }

  if (check == FALSE) {
    
    warning("Port table not saved.")
    invisible(FALSE)
    
  } else {
    
    suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project)))
    on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
    
    if (table_exists(paste0(project, "PortTable"), project) == FALSE | over_write == TRUE) {
      DBI::dbWriteTable(fishset_db, paste0(project, "PortTable", format(Sys.Date(), format = "%Y%m%d")), x, overwrite = over_write)
      DBI::dbWriteTable(fishset_db, paste0(project, "PortTable"), x, overwrite = over_write)
      
      if (fishset_env_exists() == FALSE)  create_fishset_env()
      edit_fishset_env("port_name", paste0(project, "PortTable"))
      
      load_port_function <- list()
      load_port_function$functionID <- "load_port"
      load_port_function$args <- list(deparse(substitute(dat)), deparse(substitute(port_name)), 
                                      project, over_write, compare, deparse(substitute(y)))
      load_port_function$kwargs <- list()
      load_port_function$output <- c("")
      log_call(project, load_port_function)
      
      message("Port table saved to database")
      invisible(TRUE)
      
    } else {
      
      warning(paste("Port table not saved.", paste0(project, "PortTable"), 
                    "exists in database, and overwrite is FALSE."))
      invisible(FALSE)
    }
  }
}

load_aux <- function(dat, aux, x, over_write = TRUE, project = NULL) {
  #' Import, parse, and save auxiliary data to FishSET database
  #'
  #' Auxiliary data is additional data that connects the primary dataset.
  #' Function pulls the data, parses it, and then and saves the data to the FishSET database.
  #' @param dat Primary data containing information on hauls or trips.
  #'   Table in the FishSET database contains the string 'MainDataTable'.
  #' @param aux File name, including path of auxiliary data.
  #' @param x Name auxiliary data should be saved as in FishSET database.
  #' @param over_write Logical, If TRUE, saves data over previously
  #'   saved data table in the FishSET database.
  #' @param project String, name of project.
  #' @importFrom jsonlite toJSON
  #' @importFrom DBI dbConnect dbDisconnect dbWriteTable
  #' @export
  #' @details Auxiliary data is any additional data beyond the primary data and the port data.
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
  check <- TRUE

  suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project)))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  if (is.character(dat) == TRUE) {
    if (is.null(dat) == TRUE | table_exists(dat, project) == FALSE) {
      print(DBI::dbListTables(fishset_db))
      warning(paste(dat, "not defined or does not exist. Consider using one of the tables listed above that exist in the database."))
      check <- FALSE
    } else {
      old <- table_view(dat, project)
    }
  } else {
    old <- dat
  }

   if(is.character(aux)){
      aux <- read_dat(aux)
    } else {
      aux = aux
    }

  if (any(colnames(aux) %in% colnames(old)) == FALSE) {
    
    warning("No shared columns. Column names do not match between two data sets.")
    check <- FALSE
  }
  
  if (check == FALSE) {
    
    warning("Auxiliary table not saved.")
    invisible(FALSE)
    
  } else {
   
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

    if (table_exists(paste0(project, x), project) == FALSE | over_write == TRUE) {
      
      DBI::dbWriteTable(fishset_db, paste0(project, x, "AuxTable", format(Sys.Date(), format = "%Y%m%d")), 
                        aux, overwrite = over_write)
      
      DBI::dbWriteTable(fishset_db, paste0(project, x, "AuxTable"), aux, overwrite = over_write)
      
      if (fishset_env_exists() == FALSE)  create_fishset_env()
      edit_fishset_env("aux_name", paste0(project, x))
      
      load_aux_function <- list()
      load_aux_function$functionID <- "load_aux"
      load_aux_function$args <- list(deparse_name(dat), deparse_name(aux), x, over_write, project)
      log_call(project, load_aux_function)
      
      message("Auxiliary table saved to database.")
      invisible(TRUE)
      
    } else {
      
      warning(paste("Table not saved.", paste0(project, x), 
                    "exists in database, and overwrite is FALSE."))
      invisible(FALSE)
    }
  }
}

load_grid <- function(dat, grid, x, over_write = TRUE, project = NULL) {
  #' Import, parse, and save gridded data to FishSET database
  #'
  #' Gridded data is data that varies by two dimensions. Column names must be zone names. Load, parse, and save gridded data to FishSET database
  #' @param dat Primary data containing information on hauls or trips. 
  #'   Table in FishSET database contains the string 'MainDataTable'.
  #' @param grid File name, including path, of gridded data. 
  #' @param x Name gridded data should be saved as in FishSET database.
  #' @param over_write Logical, If TRUE, saves dat over previously saved data table in the FishSET database.
  #' @param project String, name of project.
  #' @details Grid data is an optional data frame that contains a variable that varies by the map grid (ex.
  #'   sea surface temperature, wind speed). Data can also vary by a second dimension (e.g., date/time). Both
  #'   dimensions in the gridded data file need to be variables included in the primary dataset.
  #'   The grid locations (zones) must define the columns and the optional second dimension defines the rows.
  #'   The row variable must have the exact name as the variable in the main data frame that it will be linked to. 
  #'   The function DOES NOT check that column and row variables match a variable in the primary data set.
  #'   The function checks that each row is unique, that no variables are empty, and that column names are case-insensitive unique. 
  #'   These data issues are resolved before the data is saved to the database.
  #'    The data is saved in the FishSET database as the raw
  #'   data and the working data. In both cases, the table name is the \code{project} and the file name \code{x}.
  #'   Date is attached to the name for the raw data.
  #' @export
  #' @examples
  #' \dontrun{
  #' load_grid(dat = 'pcodMainDataTable', x = SeaSurfaceTemp, over_write = TRUE, project = 'pcod')
  #' }
  check <- TRUE
  fishset_db <- suppressWarnings(DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project)))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  if (is.character(dat) == TRUE) {
    if (is.null(dat) == TRUE | table_exists(dat, project) == FALSE) {
      print(DBI::dbListTables(fishset_db))
      warning(paste(dat, "not defined or does not exist. Consider using one of the tables listed above that exist in the database."))
      check <- FALSE
    } else {
      old <- table_view(dat, project)
    }
  } else {
    old <- dat
  }

  #if (any(colnames(grid) %in% colnames(old)) == FALSE) {
    message("Column names must match zone IDs. Optional secondary dimension must match a variable in the primary dataset.")
 #   check <- FALSE
 # }

    if(is.character(grid)){
      
      grid <- read_dat(grid)
    } else {
      grid <- grid
    }
    
  if (check == FALSE) { 
    
    warning("Grid table not saved.")
    invisible(FALSE)
    
  } else {
    
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
    

    if (table_exists(paste0(project, x), project) == FALSE | over_write == TRUE) {
      
      DBI::dbWriteTable(fishset_db, paste0(project, x, "GridTable"), grid, overwrite = over_write)
      
      if (fishset_env_exists() == FALSE)  create_fishset_env()
      edit_fishset_env("grid_name", paste0(project, x))
      
      load_gridded_function <- list()
      load_gridded_function$functionID <- "load_grid"
      load_gridded_function$args <- list(deparse_name(dat), deparse_name(grid), x, over_write, project)
      log_call(project, load_gridded_function)
      
      message("Grid table saved to database.")
      invisible(TRUE)
      
    } else {
      
      warning(paste("Grid table not saved.", paste0(project, x), 
                    "exists in database, and overwrite is FALSE."))
      invisible(FALSE)
    }
  }
}


