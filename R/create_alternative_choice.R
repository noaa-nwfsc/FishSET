#' Define alternative fishing choice 
#'
#' Required step. Creates a list identifying how alternative fishing choices should 
#' be defined. Output is saved to the FishSET database. Run this function before 
#' running models.  
#'
#' @param dat  Required, main data frame containing data on hauls or trips.
#'   Table in FishSET database should contain the string `MainDataTable`.
#' @param project Required, name of project
#' @param occasion String, identifies how to find lat/lon for starting point 
#'   (must have a lon/lat associated with it). \code{occasion} may be the ‘centroid 
#'   of zonal assignment’, a port variable or lon/lat variable(s) in the primary 
#'   dataset. If a port variable is defined, a corresponding port table must exist 
#'   which contains the port name and the longitude and latitude of each port. For 
#'   lon/lat variables, longitude must be specified first.
#' @param alt_var Identifies how to find lat/lon for alternative choices. \code{alt_var} 
#'   may be the centroid of zonal assignment \code{"centroid"}, or lon/lat variables 
#'   in the primary dataset. Longitude must be specified first.
# @param grid Data must contain a variable that varies by the spatial dataset 
#   \code{spat}. First variable in \code{grid} should match a column in \code{dat}. 
#   The remaining columns should match the zone IDs in the \code{spat}.
#' @param dist.unit String, how distance measure should be returned. Choices are 
#'   \code{"meters"} or \code{"M"}, \code{"kilometers"} or \code{"KM"}, \code{"miles"}, 
#'   or units of \code{grid}. Defaults to miles.
#' @param min.haul Required, numeric, minimum number of hauls. Zones with fewer 
#'   hauls than the \code{min.haul} value will not be included in model data.
#' @param spatID Required, variable in either \code{dat} or \code{spat} that identifies 
#'   the individual areas or zones. If \code{spatID} is a variable in \code{dat} that 
#'   identifies zone assignments for each occurrence record, set \code{spat} to 
#'   \code{NULL}. Otherwise, if \code{spat} is class \code{sf}, \code{spatID} should be the 
#'   name of the list containing information on zones.
#' @param zoneID Variable in \code{dat} that identifies the individual zones or 
#'   areas. Define if exists in \code{dat} and is not named `ZoneID`. Defaults to 
#'   \code{NULL}. 
#' @param spat Required, data file or character. \code{spat} is a spatial data file 
#'   containing information on fishery management or regulatory zones boundaries.
#'   Shape, json, geojson, and csv formats are supported. geojson is the preferred 
#'   format. json files must be converted into geojson. This is done automatically 
#'   when the file is loaded with \code{\link{read_dat}} with \code{is.map = TRUE}.
#'   \code{lon.dat}, \code{lat.dat}, \code{lon.spat}, and \code{lat.spat} are required 
#'   for specific \code{spat} file formats. \code{spatID} must be specified for all 
#'   file formats. If a zonal centroid table exists in the FishSET database and a 
#'   zonal assignment column exists in \code{dat} then \code{spat} may be a table 
#'   from the FishSET database or a data file. If \code{spat} should come from the 
#'   FishSET database, it should be the name of the original file name, in quotes. 
#'   For example, 'NMFS_zones_polygons'. \code{spatID} would then be the name of the 
#'   column in \code{dat} containing fishing zone assignments. Use \code{tables_database()} 
#'   to view names of tables in the FishSET database.
# @param case Centroid='Centroid of Zonal Assignment', Port, Other
# @param hull.polygon Used in \code{\link{assignment_column}} function. Creates 
#   polygon using convex hull method. Required if zonal assignments in \code{dat} 
#   should be identified and \code{spat} is not \code{NULL}.
# @param lon.dat Longitude variable from \code{dat}. Required if zonal assignments 
#   in \code{dat} should be identified and \code{spat} is not \code{NULL}.
# @param lat.dat Latitude variable from \code{dat}. Required if zonal assignments 
#   in \code{dat} should be identified and \code{spat} is not \code{NULL}.
# @param lon.spat Variable or list from \code{spat} containing longitude data. 
#   Required for csv files. Leave as \code{NULL} if \code{spat} is a shape or json 
#   file, Required if zonal assignments in \code{dat} should be identified and 
#   \code{spat} is not \code{NULL}.
# @param lat.spat Variable or list from \code{spat} containing latitude data. Required 
#   for csv files. Leave as \code{NULL} if \code{spat} is a shape or json file, 
#   Required if zonal assignments in \code{dat} should be identified and \code{spat} 
#   is not \code{NULL}.
# @param closest.pt Logical, if true, zone ID identified as the closest polygon 
#   to the point. Called in \code{\link{assignment_column}}. Required if zonal 
#   assignments for observations in \code{dat} should be identified and \code{spat} 
#   is not \code{NULL}.
#' @importFrom DBI dbExecute
#' @export create_alternative_choice
#' @details Defines the alternative fishing choices. These choices are used to develop 
#'   the matrix of distances between observed and alternative fishing choices (where 
#'   they could have fished but did not). The distance matrix is calculated by the 
#'   \code{\link{make_model_design}} function. The distance matrix can come from 
#'   \code{dat} or from the gridded data frame \code{grid}. If the distance 
#'   matrix is to come from \code{dat}, then \code{occasion} (observed fishing location) 
#'   and \code{alt_var} (alternative fishing location) must be specified. \code{grid}, 
#'   if used, must be a variable that varies by the spatial dataset, such as wind 
#'   speed. Each column must be a unique zone that matches the zones in \code{dat}.
#'   
#'   Parts of the alternative choice list are pulled by \code{\link{create_expectations}}, 
#'   \code{\link{make_model_design}}, and the model run (\code{\link{discretefish_subroutine}}) 
#'   functions. These output include choices of which variable to use for catch and 
#'   which zones to include in analyses based on a minimum number of hauls per trip 
#'   within a zone. Note that if the alternative choice list is modified, the 
#'   \code{\link{create_expectations}} and \code{\link{make_model_design}} functions 
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
#'         int: \tab Geographic centroid for each zone. Generated from \code{\link{find_centroid}}\cr
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
           spat, # keep
           spatID = NULL, # keep (change to spatID?)
           zoneID = NULL) {
    
  # TODO: remove grid feature for now, add later if needed
  # TODO: remove find_centroid() and assignment_column()--check if they are needed
  # based on current args and inform user
  # TODO: Let users associate a column from dat w/ area column in spat 
  # Needed if user doesn't have lon/lat data (ex. Fish Tickets)
  # TODO: add fishing centroid as occasion/alt_var option 
  # TODO: add starting loc arg
    
  # occasion options: 
  # "zonal centroid": pull zone centroid from FSDB and join to primary 
  # "fishing centroid": pull fishing centroid fro FSB and join to primary
  # Port variable name: merge port table to primary with port variable as key
  # c(lon, lat): any vector of length two is assumed to be lon-lat coords. This
  # can be centroid lon-lat, port lon-lat, or haul lon-lat, or previous location
  # But what if previous area? Need to join to centroid
    # add previous area by using startloc arg? 
    # use a third arg ("occasion_var") that contains the variables from primary
    # then use
    
  # alt choice options:
  # "zonal centroid": Pull from FSDB (could get from primary if same as occasion and is lon-lat)
  # "fishing centroid": Pull from FSDB (could get from primary if same as occasion and is lon-lat)
  # "nearest point": have create_distance_matrix calculate nearest point to each zone as alt
  
  # Call in datasets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  spat_out <- data_pull(spat, project)
  spatdat <- spat_out$dataset
  spat <- parse_data_name(spat, "spat", project)
  
  # TODO: column name checks for dat, spat, and grid (make sure columns exist)
  
  column_check(dat, cols = c(zoneID, occasion_var))
  
  
  
  
  
  # Note: Currently only one centroid table can exist for each project
  # this would need to change if multiple spatial scales are being modeled 
  
  # TODO: check if centroid is needed, then see if user has it. If not, 
  # error out.
  cent_exists <- table_exists(paste0(spat, 'Centroid'), project) 
  
  # Note: this will depend on updated arg options. Required if occasion or alt_var
  # are centroid
  cent_required <- TRUE # TODO: update
  case <- "centroid" # this will switch between different types of alt choices

  if (cent_required & cent_exists) {
    
    int <- table_view(paste0(spat, 'Centroid'), project)

  } else if (cent_required & !cent_exists) {
    
    stop("Zonal centroid must be defined. Function not run.", call. = FALSE)
  }
 
 
  if (!any(int$ZoneID %in% dataset[[zoneID]])) {
    
    # if (!is.null(spatdat)) {
    # 
    #   int <- find_centroid(spat = spatdat, project = project, lon.spat = lon.spat,
    #                        lat.spat = lat.spat, spatID = spatID, log.fun = FALSE)
    # 
    # } else {
      
    stop('Zones do not match between centroid table and zonal assignments',
         ' in main data table. Rerun find_centoid using same spatial data file',
         ' as was using with the assignment_column() function.', call. = FALSE)
    # }
  }

  g <- dataset[[zoneID]]
  
  if (anyNA(g) == TRUE) {
    
    warning(paste("No zone identified for", sum(is.na(g)), "observations. 
                  These observations will be removed in future analyses."))
  }
  
  # Note: is this needed?
  choice <- data.frame(g)  
  
  # Q: Is this meant to check whether "startingloc" exists as a column in dat?
  # What if named something else? Add as new arg?  
  startingloc <- if (!"startingloc" %in% colnames(dataset)) {
    
    rep(NA, nrow(dataset))
    
  } else {
    
    data.frame(dataset$startingloc)
  }

  if (is.null(choice)) {
    
    stop("Choice must be defined. Ensure that the zone or area assignment variable",
         " (spatID parameter) is defined.")
  }
  

  # if (case == "centroid") {
  #   
  #   # unique zones w/o NAs 
  #   B <- unique(g[!is.na(g)]) 
  #   
  #   # zone index (of B)
  #   C <- match(g[!is.na(g)], B)
  # 
  # } else {
  #   # TODO: this is an unreliable method for finding area/zone cols
  #   # should use zoneID
  #   a <- colnames(dataset)[grep("zon|area", colnames(dataset), ignore.case = TRUE)] # find data that is zonal type
  # 
  #   temp <- cbind(as.character(g), dataset[[a[1]]]) 
  #   B <- unique(temp) # Correct ->> Needs to be lat/long
  #   C <- match(paste(temp[, 1], temp[, 2], sep = "*"), paste(B[, 1], B[, 2], sep = "*"))
  # }

  # count zones
  zoneCount <- agg_helper(dataset, value = zoneID, count = TRUE, fun = NULL)
  # remove zones that don't have enough hauls and unassigned zones
  zoneCount[zoneCount$n < min.haul, zoneID] <- NA

  if (all(is.na(zoneCount[[zoneID]]))) {
    
    stop("No zones meet criteria. No data will be included in further analyses.",
         " Check the min.haul parameter or zone identification.", call. = FALSE)
  }
  
  zoneCount <- zoneCount[!is.na(zoneCount[[zoneID]]), ]

  # zones that meet/exceed min.haul
  greaterNZ <- zoneCount[[zoneID]]
  
  # index of obs to include based on min.haul
  dataZoneTrue <- as.numeric(dataset[[zoneID]] %in% greaterNZ)
  
  numOfNecessary <- min.haul

  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project=project))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)

  Alt <- list(
    dataZoneTrue = dataZoneTrue, # array of logical values to identify which are to be used in model
    greaterNZ = greaterNZ,
    numOfNecessary = numOfNecessary, # input
    choice = choice,
    altChoiceUnits = dist.unit, # miles
    altChoiceType = "distance",
    occasion = occasion, # altToLocal1
    alt_var = alt_var, # altToLocal2
    startingloc = startingloc,
    zoneHist = zoneCount,
    zoneRow = zoneCount[, zoneID], # zones and choices array
    zoneID = spatID,
    int = int # centroid table
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
    list('dat' = dat, 'project' = project, 'occasion' = occasion, alt_var,  
         dist.unit, min.haul, spat, spatID, zoneID)
  
  create_alternative_choice_function$kwargs <- list()
  create_alternative_choice_function$output <- list()

  log_call(project, create_alternative_choice_function)
}
