#' Define alternative fishing choice 
#'
#' Required step. Creates a list identifying how alternative fishing choices should 
#' be defined. Output is saved to the FishSET database. Run this function before 
#' running models. `dat` must have a zone assignment column (see 
#' [column_assignment()]). In certain cases a centroid table must be saved to 
#' the FishSET Database, see `occasion_var` for details.
#'
#' @param dat  Required, main data frame containing data on hauls or trips.
#'   Table in FishSET database should contain the string `MainDataTable`.
#' @param project Required, name of project.
#' @param occasion String, determines the starting point when calculating the 
#'   distance matrix. Options are `"zonal centroid"`, `"fishing centroid"`, 
#'   `"port"`, or `"lon-lat"`. See `occasion_var` for requirements. 
#' @param occaion_var Identifies an ID column or set of lon-lat variables needed 
#'   to create the distance matrix. Possible options depend on the value of 
#'   `occasion`: 
#'   \describe{
#'     \item{Centroid}{When `occasion = zonal/fishing centroid` the possible 
#'       options are `NULL`, the name of a zone ID variable, or a set coordinate 
#'       variables (in Lon-Lat order).
#'       \describe{
#'         \item{NULL}{This will merge centroid lon-lat data to the primary table 
#'           using the column enter in `zoneID`. A centroid table must be saved 
#'           to the FishSET Database.}
#'         \item{Zone ID}{This option specifies the zone ID variable to merge the 
#'           centroid table to. For example, a column containing the previous zonal
#'           area. A centroid table must be saved to the FishSET Database.}
#'         \item{Lon-Lat}{A string vector of length two containing the longitude 
#'           and latitude of an existing set centroid variables in `dat`.}
#'       } 
#'     }
#'     \item{Port}{When `occasion = port` the possible options include the name 
#'       of a port ID variable or a set of lon-lat variables describing the 
#'       location of the port. A value of `NULL` will return an error.
#'       \describe{
#'         \item{Port ID}{The name of a port ID variable in `dat` that will be 
#'           used to join the port table to the primary table. A port table
#'           is required (see [load_port()]) which contains the port name and 
#'           the longitude and latitude of each port.}
#'         \item{Lon-Lat}{A string vector of length two containing a port's 
#'           longitude and latitude in `dat`.}
#'       }
#'     }
#'     \item{Lon-Lat}{When `occasion = lon-lat`, `occasion_var` must contain a 
#'       string vector of length two containing the longitude and latitude of a 
#'       vessel's location in the `dat`. For example, the current or 
#'       previous haul location.} 
#'   }
#' @param alt_var Determines the alternative choices used to calculate the distance
#'   matrix. `alt_var` may be the centroid of zonal assignment (`"zonal centroid"`), 
#'   `"fishing centroid"`, or the closest point in fishing zone 
#'   (`"nearest point"`). The centroid options require that the appropriate 
#'   centroid table has been saved to the project's FishSET Database. See 
#'   [create_centroid()] to create and save centroids. List existing centroid 
#'   tables  by running `list_tables("project", type = "centroid")`.
#' @param dist.unit String, how distance measure should be returned. Choices are 
#'   `"meters"` or `"M"`, `"kilometers"` or `"KM"`, or `"miles"`. Defaults to miles.
#' @param min.haul Required, numeric, minimum number of hauls. Zones with fewer 
#'   hauls than the `min.haul` value will not be included in model data.
#' @param spatID Required, variable in `spat` that identifies the individual 
#'   areas or zones. 
#' @param zoneID Variable in `dat` that identifies the individual zones or 
#'   areas. 
#' @param spat Required, data file or character. `spat` is a spatial data file 
#'   containing information on fishery management or regulatory zones boundaries.
#'   `sf` objects are recommended, but `sp` objects can be used as well. See 
#'   [dat_to_sf()] to convert a spatial table read from a csv file to an `sf` 
#'   object. To upload your spatial data to the FishSETFolder see [load_spatial()].
#'   If `spat` should come from the FishSET database, it should be the name of 
#'   the original file name, in quotes. For example, `"pollockNMFSZonesSpatTable"`.
#'   Use [tables_database()] or `list_tables("project", type = "spat")` to view 
#'   the names of spatial tables in the FishSET database.
#' @importFrom DBI dbExecute
#' @export create_alternative_choice
#' @md
#' @details Defines the alternative fishing choices. These choices are used to develop 
#'   the matrix of distances between observed and alternative fishing choices (where 
#'   they could have fished but did not). The distance matrix is calculated by the 
#'   [make_model_design()] function. `occasion` defines the observed fishing 
#'   location and `alt_var` the alternative fishing location. `occasion_var` 
#'   identifies an ID column or set of lon-lat variables needed to create the 
#'   distance matrix.  
#'   
#'   Parts of the alternative choice list are pulled by [create_expectations()], 
#'   [make_model_design()], and the model run [discretefish_subroutine()]) 
#'   functions. These output include choices of which variable to use for catch and 
#'   which zones to include in analyses based on a minimum number of hauls per trip 
#'   within a zone. Note that if the alternative choice list is modified, the 
#'   [create_expectations()] and [make_model_design()] functions 
#'   should also be updated before rerunning models.
#' @return Saves the alternative choice list to the FishSET database as a list.
#'   Output includes: \cr
#' \tabular{rlll}{
#'         dataZoneTrue: \tab Vector of 0/1 indicating whether the data from that 
#'         zone is to be included in the model\cr
#'         greaterNZ: \tab Zones which pass numofNecessary test\cr
#'         numOfNecessary: \tab Minimum number of hauls for zone to be included\cr
#'         altChoiceUnits: \tab Set to miles\cr
#'         altChoiceType: \tab Set to distance\cr
#'         occasion: \tab Identifies how to find latitude and longitude for starting point\cr
#'         alt_var: \tab Identifies how to find latitude and longitude for alternative choice \cr
#'         zoneRow: \tab Zones and choices array\cr
#'         int: \tab Geographic centroid for each zone. Generated from [find_centroid()]\cr
#'         matrix: \tab Distance matrix is alternative choices comes from gridded dataset
#'         }

create_alternative_choice <- 
  function(dat,
           project,
           occasion = 'zonal centroid',
           occasion_var = NULL,
           alt_var = 'zonal centroid',
           dist.unit = "miles",
           min.haul = 0,
           spat, 
           spatID, 
           zoneID,
           startingloc = NULL) {
    

  # TODO: Let users associate a column from dat w/ area column in spat 
  # Needed if user doesn't have lon/lat data (ex. Fish Tickets)
    
  # TODO: If user imported primary data with zoneID and centroid lon-lat, then
  # spatial table not required. 
  
  # Call in datasets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  spat_out <- data_pull(spat, project)
  spatdat <- spat_out$dataset
  spat <- parse_data_name(spat, "spat", project)
  
  column_check(dataset, cols = c(zoneID, occasion_var))
  column_check(spatdat, cols = spatID)
  
  zone_cent <- NULL
  fish_cent <- NULL
  
  # check args ----
  
  occasion_opts <- c("zonal centroid", "fishing centroid", "port", "lon-lat")
  alt_opts <- c("zonal centroid", "fishing centroid", "nearest point")
  o_len <- length(occasion_var)
  
  if (o_len > 2) stop("Invalid values for 'occasion_var'.", call. = FALSE)
  
  if (!occasion %in% occasion_opts) {
    
    stop("Invalid option for 'occasion'. Options are ", 
         paste0(occasion_opts, collapse = ", "), ".", call. = FALSE)
  }
  
  if (!alt_var %in% alt_opts) {
    
    stop("Invalid option for 'alt_var'. Options are ", 
         paste0(alt_opts, collapse = ", "), ".", call. = FALSE)
  }
  
  # check occasion_var for lon-lat string match
  ll_occ_check <- function(occ) {
    
    ll_check <- grepl("lon|lat", occ, ignore.case = TRUE)
    
    if (!any(ll_check)) {
      
      warning("Check that 'occasion_var' contains longitude and latitude ",
              "variables: ", paste0(occ, collapse = ", "), 
              call. = FALSE)
    }
  }
  
  cent_check <- function(project, cent_exists, spat, type = "zone") {
    
    cent_type <- switch(type, zone = "Zonal", fish = "Fishing")
    
    if (!cent_exists) {
      
      stop(cent_type, " centroid table must be saved to FishSET Database. Run ", 
           "create_centroid().", call. = FALSE)
    }
    
    if (type == "zone") {
      
      cent_tab <- table_view(paste0(spat, "Centroid"), project)
      
    } else {
      
      cent_tab <- table_view(paste0(project, "Centroid"), project)
    }
   
    if (!any(cent_tab$ZoneID %in% dataset[[zoneID]])) {
      
      stop('Zones do not match between centroid table and zonal assignments',
           ' in main data table. Rerun find_centroid() using same spatial data file',
           ' as was using with the assignment_column() function.', call. = FALSE)
    }
  }
  
  # alt_var ----
  ## zonal centroid ----
  z_cent_exists <- table_exists(paste0(spat, 'Centroid'), project)
  
  if (alt_var == "zonal centroid") {
    
    cent_check(project, z_cent_exists, spat, "zone")
  }
  
  ## fishing centroid ----
  f_cent_exists <- table_exists(paste0(project, 'FishCentroid'), project)
  
  if (alt_var == "fishing centroid") {
  
    cent_check(project, f_cent_exists, spat, "fish")
  }
  
  # occasion ----
  ## zonal centroid ----
  if (occasion == "zonal centroid") {
    
    if (is_value_empty(occasion_var) | o_len == 1) {
      
      cent_check(project, z_cent_exists, spat, "zone")
      
    } else if (o_len == 2) {
      
      ll_occ_check(occasion_var)
      
    } else {
      
      stop("Invalid 'occasion_var'.", call. = FALSE)
    }
  ## fishing centroid ----
  } else if (occasion == "fishing centroid") {
    
    if (is_value_empty(occasion_var) | o_len == 1) {
      
      cent_check(project, f_cent_exists, spat, "fish")
      
    } else if (o_len == 2) {
      
      ll_occ_check(occasion_var)
      
    } else {
      
      stop("Invalid 'occasion_var'.", call. = FALSE)
    } ## port ----
    
  } else if (occasion == "port") {
    
    if (is_value_empty(occasion_var)) {
      
      stop("Port column name required for 'occasion = port'.", call. = FALSE)
    }
    
    if (o_len == 2) {
      
      ll_occ_check(occasion_var)
    }
  ## lon-lat ----
  } else if (occasion == "lon-lat") {
    
    if (o_len != 2) {
      
      stop("'occasion_var' must contain a longitude and latitude column.", 
           call. = FALSE)
    }
    
    ll_occ_check(occasion_var)
    
  } else {
    
    stop("Invalid 'occasion' option.", call. = FALSE)
  }
  
  # min hauls ----
  choice <- dataset[[zoneID]]
  
  if (anyNA(choice) == TRUE) {
    
    warning("No zone identified for ", sum(is.na(choice)), " observations. These ", 
            "observations will be removed in future analyses.", call. = FALSE)
  }

  if (is.null(choice)) {
    
    stop("Choice must be defined. Ensure that the zone or area assignment variable",
         " (spatID parameter) is defined.", call. = FALSE)
  }
  
  # count zones
  zoneCount <- agg_helper(dataset, value = zoneID, count = TRUE, fun = NULL)
  # remove zones that don't have enough hauls and unassigned zones
  zoneCount[zoneCount$n < min.haul, zoneID] <- NA

  if (all(is.na(zoneCount[[zoneID]]))) {
    
    stop("No zones meet criteria. No data will be included in further analyses.",
         " Check the 'min.haul' parameter or zone identification.", call. = FALSE)
  }
  
  zoneCount <- zoneCount[!is.na(zoneCount[[zoneID]]), ]

  # zones that meet/exceed min.haul
  greaterNZ <- zoneCount[[zoneID]]
  
  # index of obs to include based on min.haul
  dataZoneTrue <- as.numeric(dataset[[zoneID]] %in% greaterNZ)

  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project=project))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)

  # alt choice list ----
  Alt <- list(
    dataZoneTrue = dataZoneTrue, # index to identify which obs to use in model
    greaterNZ = greaterNZ,
    numOfNecessary = min.haul,
    choice = choice,
    altChoiceUnits = dist.unit,
    altChoiceType = "distance",
    occasion = occasion, 
    occasion_var = occasion_var,
    alt_var = alt_var, 
    zoneHist = zoneCount,
    zoneRow = zoneCount[, zoneID], # zones and choices array
    zoneID = zoneID,
    zone_cent = zone_cent,
    fish_cent = fish_cent
    )
  
  # write Alt to datafile
    
  single_sql <- paste0(project, "altmatrix")
  date_sql <- paste0(project, "altmatrix", format(Sys.Date(), format = "%Y%m%d"))
  
  if (table_exists(single_sql, project)) {
    
    table_remove(single_sql, project)
  }
  
  if (table_exists(date_sql, project)) {
    
    table_remove(date_sql, project)
  }
  # Creates an undated alt matrix table (why?)
  DBI::dbExecute(fishset_db, paste("CREATE TABLE IF NOT EXISTS", single_sql, "(AlternativeMatrix ALT)"))
  DBI::dbExecute(fishset_db, paste("INSERT INTO", single_sql, "VALUES (:AlternativeMatrix)"),
    params = list(AlternativeMatrix = list(serialize(Alt, NULL)))
  )
  # Creates a dated alt matrix table
  DBI::dbExecute(fishset_db, paste("CREATE TABLE IF NOT EXISTS", date_sql, "(AlternativeMatrix ALT)"))
  DBI::dbExecute(fishset_db, paste("INSERT INTO", date_sql, "VALUES (:AlternativeMatrix)"),
    params = list(AlternativeMatrix = list(serialize(Alt, NULL))))
  
  # TODO: add message that altmatrix was created/saved to FishSETDB
  # TODO: return TRUE invisibly if successful, FALSE if not (for testing and app)

  create_alternative_choice_function <- list()
  create_alternative_choice_function$functionID <- "create_alternative_choice"
  create_alternative_choice_function$args <- 
    list(dat, project, occasion, occasion_var, alt_var, 
         dist.unit, min.haul, spat, spatID, zoneID, startingloc)
  
  create_alternative_choice_function$kwargs <- list()
  create_alternative_choice_function$output <- list()

  log_call(project, create_alternative_choice_function)
}
