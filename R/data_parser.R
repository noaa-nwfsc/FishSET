#  Import data

read_dat <- function(x,
                     data.type = NULL,
                     is.map = FALSE,
                     drv = NULL,
                     dbname = NULL,
                     user = NULL,
                     password = NULL,
                     ...) {
  
  #' Import data from local file directory or webpage into the R environment
  #' 
  #' @param x Name and path of dataset to be read in. To load data directly from 
  #'   a webpage, \code{x} should be the web address.  
  #' @param data.type Optional. Data type can be defined by user or based on the 
  #'   file extension. If undefined, \code{data.type} is the string after the 
  #'   last period or equal sign. \code{data.type} must be defined if \code{x} is 
  #'   the path to a shape folder, if the file is a Google spreadsheet use 
  #'   \code{data.type = 'google'}, or if the correct extension cannot be derived 
  #'   from \code{x}. R, comma-delimited, tab-delimited, excel, Matlab, json, 
  #'   geojson, sas, spss, stata, and html, and XML data extensions do not have 
  #'   to be specified. 
  #' @param is.map logical, for .json file extension, set \code{is.map = TRUE} 
  #'   if data is a spatial file. Spatial files ending in .json will not be read 
  #'   in properly unless \code{is.map = TRUE}.
  #' @param drv Use with sql files. Database driver.
  #' @param dbname Use with sql files. If required, database name.
  #' @param user Use with sql files.  If required, user name for SQL database.
  #' @param password Use with sql files. If required, SQL database password.
  #' @param ... Optional arguments 
  #' @importFrom sf read_sf st_read st_transform
  #' @importFrom jsonlite fromJSON
  #' @importFrom readr read_csv read_delim read_rds read_tsv
  #' @importFrom readxl read_excel
  #' @importFrom xml2 read_xml read_html
  #' @importFrom RCurl getURL
  #' @importFrom DBI dbDisconnect dbConnect
  #' @importFrom rlang check_installed
  #' @details Uses the appropriate function to read in data based on data type.
  #'   Use \code{\link[FishSET]{write_dat}} to save data to the \code{data} folder 
  #'   in the \code{project} directory. Supported data types include shape, csv, 
  #'   json, matlab, R, spss, and stata files. Use \code{data.type = 'shape'} if 
  #'   \code{x} is the path to a shape folder. Use \code{data.type = 'google'} if 
  #'   the file is a Google spreadsheet.
  #'   
  #'   For sql files, use \code{data.type = 'sql'}. The function will connect to 
  #'   the specified DBI and pull the table. Users must specify the DBI driver 
  #'   (\code{drv}), for example: \code{RSQLite::SQLite()}, 
  #'   \code{RPostgreSQL::PostgreSQL()}, \code{odbc::odbc()}. Further arguments 
  #'   may be required, including database name (\code{dbname}), user id 
  #'   (\code{user}), and password (\code{password}). 
  #'   
  #'   Additional arguments can be added, such as skip lines \code{skip = 2} and 
  #'   header \code{header = FALSE}. To specify the separator argument for a 
  #'   delimited file, include tab-delimited, specify \code{data.type = 'delim'}.
  #'  
  #'   For more details, see \code{\link[base]{load}} for loading R objects, 
  #'   \code{\link[readr]{read_csv}} for reading in comma separated value files,
  #'   \code{\link[readr]{read_tsv}} for reading in tab separated value files,
  #'   \code{\link[readr]{read_delim}} for reading in delimited files,
  #'   \code{\link[readxl]{read_excel}} for reading in excel files (xls, xlsx), 
  #'   \code{\link[sf]{st_read}} for reading in geojson , GeoPackage files, and 
  #'   shape files, \code{\link[R.matlab]{readMat}} for reading in matlab data files,
  #'   \code{\link[haven]{read_dta}} for reading in stata data files,
  #'   \code{\link[haven]{read_spss}} for reading in spss data files,
  #'   \code{\link[haven]{read_sas}} for reading in sas data files, and 
  #'   \code{\link[jsonlite]{fromJSON}} for reading in json files.
  #'   \code{\link[xml2]{read_xml}} for reading in XML files. Further processing 
  #'   may be required. \code{\link[xml2]{read_html}} for reading in html tables.
  #'   See \code{read_sheet} in \code{\link[googlesheets4]{range_read}} for 
  #'   reading in google spreadsheets. Google spreadsheets require \code{data.type} 
  #'   be specified. Use \code{data.type = 'google'}. \code{\link[readODS]{read_ods}} 
  #'   for reading in open document spreadsheets.
  #' @export
  #' @examples
  #' \dontrun{
  #' # Read in shape file
  #' dat <- read_dat('C:/data/nmfs_manage_simple', data.type = 'shape')
  #' 
  #' # Read in spatial data file in json format
  #' dat <- read_dat('C:/data/nmfs_manage_simple.json', is.map = TRUE)
  #' 
  #' # read in data directly from web page
  #' dat <- read_dat("https://s3.amazonaws.com/assets.datacamp.com/blog_assets/test.txt", 
  #'                 data.type = 'delim', sep = '', header = FALSE)
  #' }
  #' 
  
  
  if (is.null(data.type)) {
    
    if (grepl('=', sub('.*\\.', '', x)) == FALSE) {
      
      data.type <- sub('.*\\.', '', x)
      
    } else {
      
      data.type <- sub('.*\\=', '', x)
    }
  }
  
  data.type <- tolower(data.type)
  
  if (data.type == 'json' & is.map == TRUE) {
    
    data.type = 'geojson'
  }
  
  if (data.type == 'rdata' | data.type == "r") {
    
    out <- get(load(x))
    
    if (any(c("sf", "sp") %in% class(out))) out
    else return(as.data.frame(out))
    
  } else if (data.type == "rds") {
    
    readr::read_rds(x, ...)
    
  } else if (data.type == "mat" | data.type == 'matlab') {
    
    rlang::check_installed("R.matlab", "to use `readMat()`")
    
    cat('Data returned as named list structure. Further processing is required.')
    R.matlab::readMat(x, ...)
    
  } else if (data.type == "json") {
    
    cat('Data may require additional processing.')
    jsonlite::fromJSON(x, ...)
    
  } else if (data.type %in% c('geojson', 'geopackage', 'gpkg', 'shp', 'shape')) {
    
    sf::st_read(x, ...)
    
  }  else if (data.type == "csv") {
    
    readr::read_csv(x, ...)
    
  } else if (data.type == 'sas7bdat' | data.type == 'sas') {
    
    rlang::check_installed("haven", "to use `read_sas()`")
    as.data.frame(haven::read_sas(x, ...))
    
  } else if (data.type %in% c("sav", "spss", "por", "sas")) {
    
    rlang::check_installed("haven", "to use `read_spss()`")
    as.data.frame(haven::read_spss(x, ...))
    
  } else if (data.type == "dta" | data.type == "stata") {
    
    rlang::check_installed("haven", "to use `read_stata()`")
    as.data.frame(haven::read_stata(x, ...))
    
  } else if (data.type %in% c("xls", "xlsx", "excel")) {
    
    as.data.frame(readxl::read_excel(x, ...))
    
  } else if (data.type %in% c('txt', 'delim')) {
    
    readr::read_delim(x, ...)
    
  } else if (data.type == 'xml') {
    
    xml2::read_xml(x, ...)
    
  } else if (data.type == 'html') {
    
    xml2::read_html(RCurl::getURL(x), stringsAsFactors = FALSE, ...)
    
  } else if (data.type == 'google') {
    
    rlang::check_installed("googlesheets4", "to use `read_sheet()`")
    
    googlesheets4::read_sheet(x, ...)
    
  } else if (data.type == 'ods') {
    
    rlang::check_installed("readODS", "to use `read_ods()`")
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

write_dat <- function (dat, project, path=NULL, file_type = "csv",  ...) {
  #' Write a data table to local file directory
  #'
  #'@param dat Name of data frame in working environment to save to file. 
  #'@param project String, project name. 
  #'@param path String, path or connection to write to. If left empty, the file 
  #'  will be written to the dat folder in the project directory.
  #'@param file_type String, the type of file to write to. Options include \code{"csv"},
  #'  \code{"txt"} (tab-separated text file), \code{"xlsx"} (excel), \code{"rdata"}, 
  #'  \code{"json"}, \code{"stata"}, \code{"spss"}, \code{"sas"}, and \code{"matlab"}.
  #'@param ... Additional arguments passed to writing function. See "details" for 
  #'  the list of functions. 
  #'@importFrom openxlsx write.xlsx
  #'@importFrom jsonlite write_json
  #'@importFrom utils write.table
  #'@importFrom sf st_write
  #'@importFrom rlang check_installed
  #'@export
  #'@details  
  #' Leave \code{path = NULL} to save \code{dat} to the \code{data} folder in the 
  #' \code{project} directory
  #'See \code{\link[utils]{write.table}}  for csv and tab-separated files, 
  #'    \code{\link[base]{save}} for R data files, 
  #'    \code{\link[openxlsx]{write.xlsx}},
  #'    \code{\link[jsonlite]{read_json}} for json files, 
  #'    \code{\link[sf]{st_write}} for geojson files,
  #'    \code{\link[haven]{read_dta}} for Stata files, 
  #'    \code{\link[haven]{read_spss}} for SPSS files, 
  #'    \code{\link[haven]{read_sas}} for SAS files, and 
  #'    \code{\link[R.matlab]{writeMat}} for Matlab files, and
  #'    \code{\link[sf]{st_write}} for shape files.
  #'@examples
  #'\dontrun{
  #' # Save to the default data folder in project directory
  #' write_dat(pollockMainDataTable, type = "csv", "pollock")
  #' 
  #' # Save to defined directory location
  #' write_dat(pollockMainDataTable, path = "C://data/pollock_dataset.csv", 
  #'           type = "csv", "pollock")
  #'           
  #' # Save shape file
  #' write_dat(ST6, path = "C://data//ST6.shp", type = "shp", project = 'Pollock')
  #' }
  
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  if (is.null(path)) {
    
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
    
    sf::st_write(dataset, dsn = paste0(path, dat, ".shp"), driver="ESRI Shapefile")
    
  } else if (file_type == "stata") {
    
    rlang::check_installed("haven", "to use `write_dta()`")
    haven::write_dta(dataset, path = path, ...)
    
  } else if (file_type == "spss") {
    
    rlang::check_installed("haven", "to use `write_sav()`")
    haven::write_sav(data = dataset, path = path, ...)
    
  } else if (file_type == "sas") {
    
    # has column length restriction
    rlang::check_installed("haven", "to use `write_sas()`")
    haven::write_sas(dataset, path = path, ...)
    
  } else if (file_type == "matlab") {
    # has column length restriction (32 characters)
    rlang::check_installed("R.matlab", "to use `writeMat()`")
    R.matlab::writeMat(con = path, dataset, ...)
    
  } else {
    
    stop("Data extention not recognized.", call. = FALSE)
  }
  
  path <- path.expand(path)
  
  # Log the function
  write_dat_function <- list()
  write_dat_function$functionID <- "write_dat"
  write_dat_function$args <- list(dat, path, file_type, project)
  write_dat_function$kwargs <- list(...)
  
  log_call(project, write_dat_function)
  
  message(paste("Table saved to", path))
  invisible(TRUE)
}

# Read in primary data table from database into working environment
load_data <- function(project, name = NULL) {
  #' Load data from FishSET database into the R environment
  #' 
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
  #' 
  #' load_data('pollock', 'pollockMainDataTable20190101')
  #' }
  #' 
  
  # check_proj(project)
  
  hack <- function(key, val, pos){
    assign(key, val, envir=as.environment(pos)
    )} 
  
  dat <- NULL
  
  if (is.null(name)) {
    if (table_exists(paste0(project, "MainDataTable"), project) == FALSE) {
      warning("Table not found")
      tables_database(project)
    } else {
      dat <- table_view(paste0(project, "MainDataTable"), project)
    }
  } else {
    if (table_exists(name, project) == FALSE) {
      warning("Table not found")
      tables_database(project)
    } else {
      dat <- table_view(name, project)
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
  #' 
  #' @param dat Name of data frame in working environment to save to FishSET database.
  #' @param project String, name of project.
  #' @details Use function to save modified data to the FishSET database. The primary 
  #'   data is only saved automatically in data upload and data check functions. 
  #'   It is therefore advisable to save the modified data to the database before 
  #'   moving on to modeling functions. Users should use primary data in the
  #'   working environment for assessing data quality issues, modifying the data, 
  #'   and generating new variables. Pulling the primary data from the FishSET 
  #'   database on each function without manually saving will result in a loss of changes.
  #' @importFrom DBI dbWriteTable dbDisconnect
  #' @importFrom RSQLite SQLite
  #' @export
  #' @examples
  #' \dontrun{
  #' save_dat(pollockMainDataTable, 'pollock')
  #' }
  
  suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project)))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  # convert date columns to character (sqlite coerces to numeric)
  d_cols <- date_cols(dat)
  dat[d_cols] <- lapply(d_cols, function(d) as.character(dat[[d]]))
  
  # TODO: Not sure why we overwrite the working table and create a _mod table. Revisit this.
  DBI::dbWriteTable(fishset_db, paste0(project, "MainDataTable"), dat, overwrite = TRUE)
  DBI::dbWriteTable(fishset_db, paste0(project, "MainDataTable_mod", 
                                       format(Sys.Date(), format = "%Y%m%d")),
                    dat, overwrite = TRUE)
  
  invisible(TRUE)
}

# TODO: Fix this function
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
  #' 
  
  
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

load_maindata <- function(dat, project, over_write = FALSE, compare = FALSE, y = NULL) {
  #' Import, parse, and save data to the FishSET Database
  #' 
  #' \code{load_maindata()} saves the primary dataset to the FishSET Database (located
  #' in the FishSETFolder) and is a required step. The primary data will also be loaded 
  #' into the working environment as a dataframe named "projectMainDataTable". 
  #' Running \code{load_maindata()} creates a new project directory in the FishSETFolder.
  #' To see a list of existing projects run \code{projects()} or open the FishSETFolder.
  #'
  #' @param dat Primary data containing information on hauls or trips. This can be
  #'   the full path to the file, the name of a main table in the FishSET database,
  #'   or a dataframe object in the working environment. Main tables in the FishSET 
  #'   database contain the string 'MainDataTable'. A complete list of FishSET
  #'   tables can be display by running \code{fishset_tables()}. 
  #' @param over_write Logical, If \code{TRUE}, saves data over previously saved data 
  #'   table in the FishSET database. Defaults to \code{FALSE}.
  #' @param project String, name of project. Cannot contain spaces. 
  #' @param compare Logical, whether to compare new dataframe to previously saved 
  #'   dataframe \code{y}. See \code{\link{fishset_compare}}.
  #' @param y Name of previously saved table in FishSET Database. \code{y} must 
  #'   be defined if \code{compare = TRUE}.
  #' @importFrom DBI dbConnect dbDisconnect dbWriteTable
  #' @importFrom RSQLite SQLite
  #' @importFrom tibble as_tibble
  #' @export
  #' @details The dataset is saved in the FishSET database as raw and working tables. 
  #'   The table name is the \code{project} and the table type, 'MainDataTable'. 
  #'   The raw table is the original, unedited table. The working table contains 
  #'   any changes made to the table after uploading. An eight digit date string 
  #'   is included in the name of the raw table (e.g. "pollockMainDataTable20220210"). 
  #'   The primary data is loaded into the working environment as ‘projectMainDataTable’.
  #'   The \code{fishset_compare} argument compares \code{dat} to an existing FishSET 
  #'   table in \code{y} and returns a message noting basic differences between the two.
  #'   The column names are checked for case-insensitivity and uniqueness.  
  #' @seealso \code{\link{save_dat}}, \code{\link{write_dat}}, \code{\link{load_data}},
  #'   \code{\link{fishset_tables}}
  #' @examples
  #' \dontrun{
  #' # upload data from filepath
  #' load_maindata(dat = "PATH/TO/DATA", project = "pollock")
  #' 
  #' # upload from dataframe in working environment
  #' load_maindata(dat = Mydata, project = 'pollock', over_write = TRUE, 
  #'               compare = TRUE, y = 'MainDataTable01012011')
  #'               
  #' # upload from an exisitng FishSET primary data table
  #' looad_maindata(dat = "pollockMainDataTable", project = "pollock2020")
  #' }
  #' 

  # project name check
  stopifnot("Project name cannot contain spaces." = !grepl("\\s", project),
            "Project name cannot be empty." = !is_value_empty(project))
  
  dataset <- data_upload_helper(dat, "main")
  
  # coerce to tibble
  # TODO: customize column name check (case-insensitive)
  dataset <- tibble::as_tibble(dataset)
  
  if (compare == TRUE) {
    
    fishset_compare(dataset, y, compare, project = project)
  }
  
  # Quality Checks
  pass <- TRUE
  
  # check that names are unique in dataset
  x <- colnames(dataset)
  
  if (length(x) == length(unique(x)) & length(toupper(x)) != length(unique(toupper(x)))) {
    
    warning("\nData set will not be saved to database. 
        Duplicate case-insensitive column names. Sqlite column names are case insensitive.")
    pass <- FALSE
    
  } else if (length(x) != length(unique(x))) {
    
    warning("\nVariable names are not unique.\n")
    pass <- FALSE
  }
  
  if (pass == FALSE) { 
    
    warning('Dataset not saved. Check that column names are case-insensitive ',
            'unique and that latitude/longitude or area/zone are included.')
    
    invisible(FALSE)
    
  } else { # Checks passed
    
    # TODO: check that project name is unique? Overwrite project arg? 
    # check if project folder exists
    check_proj(project)
    
    # convert date columns to character (sqlite coerces to numeric)
    d_cols <- date_cols(dataset)
    dataset[d_cols] <- lapply(d_cols, function(d) as.character(dataset[[d]]))
    
    # Save tables to FishSET DB
    suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), 
                                                  locdatabase(project = project)))
    on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
    
    raw_tab_name <- paste0(project, "MainDataTable", 
                           format(Sys.Date(), format = "%Y%m%d"))
    
    raw_tab_exists <- table_exists(raw_tab_name, project = project)
    
    if (raw_tab_exists == FALSE | over_write == TRUE) {
      
      DBI::dbWriteTable(fishset_db, raw_tab_name, dataset, overwrite = over_write)
      message("Table saved to database")
      
    } else { 
      
      warning(paste(raw_tab_name, "was not saved. Table exists in database. Set over_write to TRUE."))
    }
    
    work_tab_name <- paste0(project, "MainDataTable")
    
    work_tab_exists <- table_exists(work_tab_name, project = project)
    
    if (work_tab_exists == FALSE | over_write == TRUE) {
      
      DBI::dbWriteTable(fishset_db, work_tab_name, dataset, overwrite = over_write)
      
      # convert date variables back to date
      dataset[d_cols] <- lapply(dataset[d_cols], date_parser)
      
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
      invisible(TRUE)
      
    } else {
      
      warning(paste0(project, "MainDataTable was not saved. Table already exists",
                     " in database. Set over_write to TRUE."))
      invisible(FALSE)
    }
  }
}


load_outsample <- function(dat, project, over_write = FALSE, compare = FALSE, y = NULL){
  #' Import, parse, and save out-of-sample data to FishSET database
  #' 
  #' \code{load_outsample()} saves out-of-sample dataset to the FishSET Database (located
  #' in the FishSETFolder) and the structure must match the primary dataset. A project must exist before 
  #' running \code{load_outsample()}. See \code{\link{load_maindata}} to create a new project. Note: if the 
  #' data are out-of-sample temporally then upload a new datafile, if the data are only out-of-sample spatially
  #' then upload the primary data file in this function.
  #'
  #' @param dat Out-of-sample data containing information on hauls or trips with same structure as the primary data table. 
  #'   This can be the full path to the file, the name of a out-of-sample table in the FishSET database,
  #'   or a dataframe object in the working environment. Out-of-sample tables in the FishSET 
  #'   database contain the string 'OutSampleDataTable'. A complete list of FishSET
  #'   tables can be viewed by running \code{fishset_tables()}. 
  #' @param over_write Logical, If \code{TRUE}, saves data over previously saved data 
  #'   table in the FishSET database. Defaults to \code{FALSE}.
  #' @param project String, name of project.
  #' @param compare Logical, whether to compare new dataframe to previously saved 
  #'   dataframe \code{y}. See \code{\link{fishset_compare}}.
  #' @param y Name of previously saved table in FishSET Database. \code{y} must 
  #'   be defined if \code{compare = TRUE}.
  #' @importFrom DBI dbConnect dbDisconnect dbWriteTable
  #' @importFrom RSQLite SQLite
  #' @importFrom tibble as_tibble
  #' @export
  #' @details The out-of-sample dataset is saved in the FishSET database as raw and working tables. 
  #'   The table name is the \code{project} and the table type, 'OutSampleDataTable'. 
  #'   The raw table is the original, unedited table. The working table contains 
  #'   any changes made to the table after uploading. An eight digit date string 
  #'   is included in the name of the raw table (e.g. "pollockOutSampleDataTable20220210"). 
  #'   The out-of-sample data is loaded into the working environment as ‘projectOutSampleDataTable’.
  #'   The \code{fishset_compare} argument compares \code{dat} to an existing FishSET 
  #'   table in \code{y} and returns a message noting basic differences between the two.
  #'   The column names are checked for case-insensitivity and uniqueness.  
  #' @seealso \code{\link{load_maindata}}, \code{\link{save_dat}}, \code{\link{write_dat}}, \code{\link{load_data}},
  #'   \code{\link{fishset_tables}}
  #' @examples
  #' \dontrun{
  #' # upload data from filepath
  #' load_outsample(dat = "PATH/TO/DATA", project = "pollock")
  #' 
  #' # upload from dataframe in working environment
  #' load_outsample(dat = MyData, project = 'pollock', over_write = TRUE, 
  #'               compare = TRUE, y = 'OutSampleDataTable01012011')
  #'               
  #' # upload from an exisitng FishSET out-of-sample data table
  #' load_outsample(dat = "pollockOutSampleDataTable", project = "pollock")
  #' }
  #' 
  
  # Check if the project exists
  if (project_exists(project) == FALSE) {
    
    stop("Project '", project, "' does not exist. Check spelling or create a",
         " new project with load_maindata().", call. = TRUE)
  }
  
  check <- TRUE
  
  outsample <- data_upload_helper(dat, "outsample")
  
  # coerce to tibble
  # TODO: customize column name check (case-insensitive)
  outsample <- tibble::as_tibble(outsample)
  
  if (compare == TRUE) {
    fishset_compare(outsample, y, compare, project = project)
  }
  
  # Quality Checks
  pass <- TRUE
  
  # check that names are unique in dataset
  x <- colnames(outsample)
  
  if (length(x) == length(unique(x)) & length(toupper(x)) != length(unique(toupper(x)))) {
    
    warning("\nData set will not be saved to database. 
        Duplicate case-insensitive column names. Sqlite column names are case insensitive.")
    pass <- FALSE
    
  } else if (length(x) != length(unique(x))) {
    
    warning("\nVariable names are not unique.\n")
    pass <- FALSE
  }
  
  # TODO: remove or change these checks, not comprehensive
  # if (any(grepl("area|zone", names(dataset), ignore.case = TRUE)) == FALSE & 
  #     (any(grepl("lat", names(dataset), ignore.case = TRUE)) == FALSE |
  #      any(grepl("lon", names(dataset), ignore.case = TRUE)) ==  FALSE)) {
  #   
  #   warning("Neither Latitude/Longitude or Area/Zone variables are included. Data will not be saved.")
  #   pass <- FALSE
  # }
  
  if (pass == FALSE) { 
    
    warning('Dataset not saved. Check that column names are case-insensitive ',
            'unique and that latitude/longitude or area/zone are included.')
    
    invisible(FALSE)
    
  } else { # Checks passed
    
    # convert date columns to character (sqlite coerces to numeric)
    d_cols <- date_cols(outsample)
    outsample[d_cols] <- lapply(d_cols, function(d) as.character(outsample[[d]]))
    
    # Save table to FishSET DB
    suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project)))
    on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
    
    raw_tab_name <- paste0(project, "OutSampleDataTable", format(Sys.Date(), format = "%Y%m%d"))
    
    raw_tab_exists <- table_exists(raw_tab_name, project = project)
    
    if (raw_tab_exists == FALSE | over_write == TRUE) {
      
      DBI::dbWriteTable(fishset_db, raw_tab_name, outsample, overwrite = over_write)
      message("Table saved to database")
      
    } else { 
      
      warning(paste(raw_tab_name, "was not saved. Table exists in database. Set over_write to TRUE."))
    }
    
    work_tab_name <- paste0(project, "OutSampleDataTable")
    
    work_tab_exists <- table_exists(work_tab_name, project = project)
    
    if (work_tab_exists == FALSE | over_write == TRUE) {
      
      DBI::dbWriteTable(fishset_db, work_tab_name, outsample, overwrite = over_write)
      
      # convert date variables back to date
      outsample[d_cols] <- lapply(outsample[d_cols], date_parser)
      
      # log function
      load_outsample_function <- list()
      load_outsample_function$functionID <- "load_outsample"
      load_outsample_function$args <- list(deparse(substitute(dat)), over_write, 
                                           project, compare, y)
      
      log_call(project, load_outsample_function)
      
      #Make data available
      makeavail <- function(key, val, pos) {
        
        assign(key, val, envir = as.environment(pos))
      }
      
      makeavail(work_tab_name, outsample, 1L)
      
      message("\n! Data saved to database as ", raw_tab_name, " (raw) and ", 
              work_tab_name, " (working). \nTable is also in the working environment. !")
      invisible(TRUE)
      
    } else {
      
      warning(paste0(project, "OutSampleDataTable was not saved. Table already exists",
                     " in database. Set over_write to TRUE."))
      invisible(FALSE)
    }
  }
}


load_port <- function(dat, port_name, project, over_write = TRUE, compare = FALSE, y = NULL) {
  #' Import, parse, and save port data to FishSET database
  #'
  #' A project must exist before running \code{load_port()}. See \code{\link{load_maindata}}
  #' to create a new project. 
  #'
  #' @param dat Dataset containing port data. At a minimum, must include three 
  #'   columns, the port names, and the latitude and longitude of ports. \code{dat}
  #'   can be a filepath, a existing FishSET table, or a dataframe in the working
  #'   environment. 
  #' @param port_name Variable containing port names. Names should match port names 
  #'   in primary dataset.
  #' @param over_write Logical, if TRUE, saves over data table previously saved 
  #'   in the FishSET database.
  #' @param project String, name of project.
  #' @param compare Logical, should new data be compared to previously saved 
  #'   dataframe \code{y}.
  #' @param y Name of previously saved table in FishSET database. \code{y} must be 
  #'   defined if \code{compare} is TRUE.
  #' @export
  #' @importFrom jsonlite toJSON
  #' @importFrom DBI dbConnect dbDisconnect dbWriteTable
  #' @details Runs a series of checks on the port data. The function checks that
  #'   each row is unique, that no variables are empty, and that column names are 
  #'   case-insensitive unique. There data issues are resolved before the data is 
  #'   saved to the database. If checks pass, runs the fishset_compare function and
  #'   saves the new data frame to the FishSET database.  The data is saved in the 
  #'   FishSET database as the raw data and the working data. The naming convention 
  #'   for port tables is "projectPortTable".  Date is also attached to the 
  #'   name for the raw data. See \code{\link{table_view}} to view/load port tables 
  #'   into the working environment.
  #' @seealso \code{\link{table_view}}, \code{\link{load_maindata}}, \code{\link{write_dat}}
  #' @examples
  #' \dontrun{
  #' load_port(PortTable, over_write = TRUE, project  ='pollock',
  #'           compare = TRUE, y = 'pollockPortTable01012011')
  #' }
  
  # TODO: have port_lon and lat args to make sure they are correctly identified
  if (project_exists(project) == FALSE) {
    
    stop("Project '", project, "' does not exist. Check spelling or create a",
         " new project with load_maindata().", call. = TRUE)
  }
  
  check <- TRUE
  
  # TODO: change "x" to "port"
  
  x <- data_upload_helper(dat, type = "port")
  
  # TODO: update these lonlat name checks -- make them easier to understand
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
  
  #unique rows
  x <- unique_rows(x)
  
  # TODO: Use name_check() 
  #unique column names
  if(length(toupper(colnames(x))) != length(unique(toupper(colnames(x))))){
    print('Duplicate case-insensitive column names found. Duplicate column names adjusted.')
    colnames(x)[which(duplicated(colnames(x)))] <- paste0(colnames(x)[which(duplicated(colnames(x)))], '.1')
  }
  #empty variables
  x <- empty_vars(x, remove = TRUE)
  
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

load_aux <- function(dat, aux, name, over_write = TRUE, project = NULL) {
  #' Import, parse, and save auxiliary data to FishSET database
  #'
  #' Auxiliary data is additional data that connects the primary dataset.
  #' Function pulls the data, parses it, and then and saves the data to the FishSET 
  #' database. A project must exist before running \code{load_aux()}. See 
  #' \code{\link{load_maindata}} to create a new project. 
  #' 
  #' @param dat Primary data containing information on hauls or trips.
  #'   Table in the FishSET database contains the string 'MainDataTable'.
  #' @param aux File name, including path of auxiliary data.
  #' @param name Name auxiliary data should be saved as in FishSET database.
  #' @param over_write Logical, If TRUE, saves data over previously
  #'   saved data table in the FishSET database.
  #' @param project String, name of project.
  #' @importFrom DBI dbConnect dbDisconnect dbWriteTable
  #' @export
  #' @details Auxiliary data is any additional data beyond the primary data and 
  #'   the port data. Auxiliary data can be any data that can be merged with the 
  #'   primary dataset (ex. prices by date, vessel characteristics, or fishery season). 
  #'   The auxiliary data does not have to be at a haul or trip level but must 
  #'   contain a variable to connect the auxiliary data to the primary dataset. 
  #'   The function checks that at least one column name of the auxiliary data 
  #'   matches a column name in the primary dataset. The function checks that
  #'   each row is unique, that no variables are empty, and that column names are 
  #'   case-insensitive unique. There data issues are resolved before the data is 
  #'   saved to the database. The data is saved in the FishSET database as the raw 
  #'   data and the working data. The naming convention for auxiliary tables is 
  #'   "projectNameAuxTable". Date is also added to the name for the raw data. 
  #'   See \code{\link{table_view}} to view/load auxiliary tables into the working 
  #'   environment.
  #' @seealso \code{\link{table_view}}, \code{\link{load_maindata}}, \code{\link{write_dat}}
  #' @examples
  #' \dontrun{
  #' load_aux(pcodMainDataTable, name = 'FisherySeason', over_write = TRUE, 
  #'          project = 'pcod')
  #' }
  
  if (project_exists(project) == FALSE) {
    
    stop("Project '", project, "' does not exist. Check spelling or create a",
         " new project with load_maindata().", call. = TRUE)
  }
  
  # table name check
  stopifnot("Name cannot contain spaces." = !grepl("\\s", name),
            "Name cannot be empty." = !is_value_empty(name))
  
  # Call in data sets
  check <- TRUE
  
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  aux <- data_upload_helper(aux, "aux")
  
  if (any(colnames(aux) %in% colnames(dataset)) == FALSE) {
    
    warning("No shared columns. Column names do not match between two data sets.")
    check <- FALSE
  }
  
  if (check == FALSE) {
    
    warning("Auxiliary table not saved.")
    invisible(FALSE)
    
  } else {
    
    #unique rows
    aux <- unique_rows(aux)
    
    # TODO: Use name_check() 
    #unique column names
    if(length(toupper(colnames(aux))) != length(unique(toupper(colnames(aux))))){
      print('Duplicate case-insensitive column names found. Duplicate column names adjusted.')
      colnames(aux)[which(duplicated(colnames(aux)))] <- paste0(colnames(aux)[which(duplicated(colnames(aux)))], '.1')
    }
    
    #empty variables
    aux <- empty_vars(aux, remove = TRUE)
    
    if (table_exists(paste0(project, name), project) == FALSE | over_write == TRUE) {
      
      suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), 
                                                    locdatabase(project = project)))
      on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
      
      DBI::dbWriteTable(fishset_db, 
                        paste0(project, name, "AuxTable", 
                               format(Sys.Date(), format = "%Y%m%d")), 
                        aux, overwrite = over_write)
      
      DBI::dbWriteTable(fishset_db, paste0(project, name, "AuxTable"), 
                        aux, overwrite = over_write)
      
      load_aux_function <- list()
      load_aux_function$functionID <- "load_aux"
      load_aux_function$args <- list(deparse_name(dat), deparse_name(aux), name, 
                                     over_write, project)
      log_call(project, load_aux_function)
      
      message("Auxiliary table saved to database.")
      invisible(TRUE)
      
    } else {
      
      warning(paste("Table not saved.", paste0(project, name), 
                    "exists in database, and overwrite is FALSE."))
      invisible(FALSE)
    }
  }
}

load_grid <- function(grid, name, project, over_write = TRUE) {
  #' Import, parse, and save gridded data to FishSET database
  #'
  #' Gridded data is data that varies by two dimensions. Column names must be zone 
  #' names. Load, parse, and save gridded data to FishSET database.  A project must 
  #' exist before running \code{load_grid()}. See \code{\link{load_maindata}}
  #' to create a new project. 
  #' 
  #' @param grid File name, including path, of gridded data. 
  #' @param name Name gridded data should be saved as in FishSET database.
  #' @param project String, name of project.
  #' @param over_write Logical, If TRUE, saves dat over previously saved data table 
  #'   in the FishSET database.
  #' @details Grid data is an optional data frame that contains a variable that 
  #'   varies by the map grid (ex. sea surface temperature, wind speed). Data can 
  #'   also vary by a second dimension (e.g., date/time). Both dimensions in the 
  #'   gridded data file need to be variables included in the primary data set.
  #'   The grid locations (zones) must define the columns and the optional second 
  #'   dimension defines the rows. The row variable must have the exact name as the 
  #'   variable in the primary data frame that it will be linked to. The function DOES 
  #'   NOT check that column and row variables match a variable in the primary data set.
  #'   The function checks that each row is unique, that no variables are empty, 
  #'   and that column names are case-insensitive unique. These data issues are 
  #'   resolved before the data is saved to the database. The data is saved in the 
  #'   FishSET database as the raw data and the working data. In both cases, the 
  #'   table name is the \code{project} and the file name \code{x}. Date is attached 
  #'   to the name for the raw data. The naming convention for gridded tables is 
  #'   "projectNameGridTable". See \code{\link{table_view}} to view/load gridded 
  #'   tables into the working environment.
  #' @export
  #' @seealso \code{\link{table_view}}, \code{\link{load_maindata}},
  #'   \code{\link{write_dat}}
  #' @examples
  #' \dontrun{
  #' load_grid(dat = 'pcodMainDataTable', name = 'SeaSurfaceTemp', 
  #'           over_write = TRUE, project = 'pcod')
  #' }
  #' 
  
  if (project_exists(project) == FALSE) {
    
    stop("Project '", project, "' does not exist. Check spelling or create a",
         " new project with load_maindata().", call. = TRUE)
  }
  
  # table name check
  stopifnot("Name cannot contain spaces." = !grepl("\\s", name),
            "Name cannot be empty." = !is_value_empty(name))
  
  check <- TRUE
  
  if(!is.data.frame(grid)){
    grid <- as.data.frame(grid)
  }
  
  grid <- data_upload_helper(grid, "grid")
  
  # Coerce to tibble
  if(any(is.na(names(grid)))){
    i_rm <- which(is.na(names(grid)))
    grid <- grid[,-i_rm]
  }
  grid <- tibble::as_tibble(grid)
  
  if (check == FALSE) { 
    
    warning("Grid table not saved.")
    invisible(FALSE)
    
  } else {
    
    # TODO: Use name_check() 
    #unique column names
    if(length(toupper(colnames(grid))) != length(unique(toupper(colnames(grid))))){
      print('Duplicate case-insensitive column names found. Duplicate column names adjusted.')
      colnames(grid)[which(duplicated(colnames(grid)))] <- paste0(colnames(grid)[which(duplicated(colnames(grid)))], '.1')
    }
    
    #empty variables
    grid <- empty_vars(grid, remove = TRUE)
    
    # save grid
    if (table_exists(paste0(project, name, "GridTable"), project) == FALSE | over_write == TRUE) {
      
      fishset_db <- suppressWarnings(DBI::dbConnect(RSQLite::SQLite(), 
                                                    locdatabase(project = project)))
      on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
      
      DBI::dbWriteTable(fishset_db, paste0(project, name, "GridTable"), 
                        grid, overwrite = over_write)
      
      load_gridded_function <- list()
      load_gridded_function$functionID <- "load_grid"
      load_gridded_function$args <- list(deparse_name(grid), name, project, over_write)
      log_call(project, load_gridded_function)
      
      message("Grid table saved to database.")
      invisible(TRUE)
      
    } else {
      
      warning(paste("Grid table not saved.", paste0(project, name), 
                    "exists in database, and overwrite is FALSE."))
      invisible(FALSE)
    }
  }
}


load_spatial <- function(spat, name = NULL, over_write = TRUE, project, 
                         data.type = NULL, lon = NULL, lat = NULL, id = NULL, ...) {
  #' Import, parse, and save spatial data 
  #'
  #' Saves a spatial table to the FishSETFolder as a geojson file. A project must 
  #' exist before running \code{load_spatial()}. See \code{\link{load_maindata}} 
  #' to create a new project. 
  #'  
  #' @param spat File name, including path, of spatial data. 
  #' @param name Name spatial data should be saved as in FishSET project folder.
  #' Cannot be empty or contain spaces.
  #' @param over_write Logical, If \code{TRUE}, saves \code{spat} over previously 
  #'   saved data table in the FishSET project folder.
  #' @param project String, name of project.
  #' @param data.type Data type argument passed to \code{\link{read_dat}}. If 
  #'  reading from a shape folder use \code{data.type = "shape"}. 
  #' @param lon Variable or list from \code{spat} containing longitude data. 
  #'    Required for csv files. Leave as \code{NULL} if \code{spat} is a shape or 
  #'    json file.
  #' @param lat Variable or list from \code{spat} containing latitude data. 
  #'    Required for csv files. Leave as \code{NULL} if \code{spat} is a shape or 
  #'    json file
  #' @param id Polygon ID column. Required for csv files. Leave as \code{NULL} if 
  #'   \code{spat} is a shape or json file.
  #' @param ... Additional argument passed to \code{\link{read_dat}}. 
  
  #' @details Function to import, parse, and saved project folder in `FishSETFolder` 
  #'  directory. To export as  shape file, use \code{\link{write_dat}} specifying 
  #'  `type='shp'`. \code{load_spatial()} performs basic quality check before saving 
  #'  spatial tables to the project data folder as a geojson file. To be saved, 
  #'  the spatial must pass the checks in \code{\link{check_spatdat}}. The spatial 
  #'  table is converted to an \code{sf} object, and checked for unique rows and 
  #'  empty columns. The naming convention for spatial tables is "projectNameSpatTable". 
  #'  See \code{\link{table_view}} to view/load spatial tables into the working 
  #'  environment.
  
  #' @export
  #' @importFrom sf st_write
  #' @seealso \code{\link{table_view}}, \code{\link{load_maindata}}, 
  #' \code{\link{write_dat}}
  #' @examples
  #' \dontrun{
  #' # upload from filepath
  #' load_spatial(spat = "FILE/PATH/TO/SPAT", name = 'tenMinSqr', 
  #'              over_write = TRUE, project = 'pcod')
  #' 
  #' # upload from object in working environment
  #' load_spatial(spat = NMFSAreas, name = "NMFS", project = "pcod")
  #' 
  #' # upload from an existing FishSET spatial table
  #' load_spatial(spat = "pcodNMFSSpatTable", name = "NMFS", project = "pcod2020")
  #' }
  
  if (project_exists(project) == FALSE) {
    
    stop("Project '", project, "' does not exist. Check spelling or create a",
         " new project with load_maindata().", call. = TRUE)
  }
  
  stopifnot("Name cannot contain spaces." = !grepl("\\s", name),
            "Name cannot be empty." = !is_value_empty(name))
  
  spat <- data_upload_helper(spat, type = "spat", data.type = data.type, 
                             is.map = TRUE, ...)
  
  # check that spat can be converted to sf
  spat <- check_spatdat(spat, lon = lon, lat = lat, id = id)
  
  # unique names
  # TODO: Use name_check() 
  
  # check for unique rows
  spat <- unique_rows(spat)
  
  # empty variables
  spat <- empty_vars(spat)
  
  # save spatial data
  tab_name <- paste0(project, name, "SpatTable")
  raw_name <- paste0(tab_name, format(Sys.Date(), format = "%Y%m%d"))
  
  file_names <- file.path(loc_data(project), "spat", paste0(c(tab_name, raw_name), ".geojson"))
  
  spat_exists <- file.exists(file_names)
  
  if (sum(spat_exists) > 0) {
    
    if (over_write) {
      
      file.remove(file_names[spat_exists])
      
    } else {
      
      stop("Dataset already exists. Set over_write = TRUE to replace.")
    }
  }
  
  lapply(file_names, function(sfn) sf::st_write(spat, dsn = sfn))
  
  # log call
  load_spatial_function <- list()
  load_spatial_function$functionID <- "load_spatial"
  load_spatial_function$args <- list(deparse_name(spat), name, over_write, project, 
                                     data.type, lon, lat, id)
  load_spatial_function$kwargs <- list(...)
  log_call(project, load_spatial_function)
  
  message("Spatial table saved to project folder as ", tab_name)
  invisible(TRUE)
}





