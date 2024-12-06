#' Define alternative fishing choice 
#'
#' Required step. Creates a list identifying how alternative fishing choices should 
#' be defined. Output is saved to the FishSET database. Run this function before 
#' running models. `dat` must have a zone assignment column (see 
#' [assignment_column()]). In certain cases a centroid table must be saved to 
#' the FishSET Database, see `occasion_var` for details.
#'
#' @param dat  Required, Primary data frame containing data on hauls or trips.
#'   Table in FishSET database should contain the string `MainDataTable`.
#' @param project Required, name of project.
#' @param occasion String, determines the starting point when calculating the 
#'   distance matrix. Options are `"zonal centroid"`, `"fishing centroid"`, 
#'   `"port"`, or `"lon-lat"`. See `occasion_var` for requirements. 
#' @param occasion_var Identifies an ID column or set of lon-lat variables needed 
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
#'       vessel's location in the `dat`. For example, the current or previous 
#'       haul location.} 
#'   }
#' @param alt_var Determines the alternative choices used to calculate the distance
#'   matrix. `alt_var` may be the centroid of zonal assignment (`"zonal centroid"`), 
#'   `"fishing centroid"`, or the closest point in fishing zone 
#'   (`"nearest point"`). The centroid options require that the appropriate 
#'   centroid table has been saved to the project's FishSET Database. See 
#'   [create_centroid()] to create and save centroids. List existing centroid 
#'   tables  by running `list_tables("project", type = "centroid")`.
#' @param dist.unit String, how distance measure should be returned. Choices are 
#'   `"meters"` or `"m"`, `"kilometers"` or `"km"`, `"miles"`, or `"nmiles"` 
#'   (nautical miles). Defaults to `"miles"`.
#' @param min.haul Required, numeric, minimum number of hauls. Zones with fewer 
#'   hauls than the `min.haul` value will not be included in model data.
#' @param zoneID Variable in `dat` that identifies the individual zones or 
#'   areas.
#' @param zone.cent.name The name of the zonal centroid table to use when 
#'   `occasion` or `alt_var` is set to `zonal centroid`. Use 
#'   `list_tables("project", type = "centroid")` to view existing centroid tables.
#'   See [create_centroid()] to create centroid tables or [centroid_to_fsdb()] to 
#'   create a centroid table from columns found in `dat`.
#' @param fish.cent.name The name of the fishing centroid table to use when 
#'   `occasion` or `alt_var` is set to `fishing centroid`. Use 
#'   `list_tables("project", type = "centroid")` to view existing centroid tables.
#'   See [create_centroid()] to create centroid tables or [centroid_to_fsdb()] to 
#'   create a centroid table from columns found in `dat`.
#' @param spatname Required when `alt_var = 'nearest point'`. `spat` is a spatial 
#'   data file  containing information on fishery management or regulatory zones 
#'   boundaries. `sf` objects are recommended, but `sp` objects can be used as 
#'   well. See [dat_to_sf()] to convert a spatial table read from a csv file to 
#'   an `sf` object. To upload your spatial data to the FishSETFolder see 
#'   [load_spatial()].If `spat` should come from the FishSET database, it should 
#'   be the name of the original file name, in quotes. For example, 
#'   `"pollockNMFSZonesSpatTable"`. Use [tables_database()] or 
#'   `list_tables("project", type = "spat")` to view the names of spatial tables 
#'   in the FishSET database.
#' @param spatID Required when `alt_var = 'nearest point'`. Variable in `spat` 
#'   that identifies the individual zones or areas. 
#' @param grid_sample Logical, TRUE if randomly sampling from all alternatives or 
#'   from a separate point-grid table. If TRUE, then also need to specify the 
#'   sample size (i.e., number of alternatives for each observation).
#' @param grid_sample_n Integer, indicating the sample size for grid sampling
#'   (i.e., number of alternative to sample).
#' @param outsample Logical, indicating whether this is for primary data or out-of
#'   sample data.
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
#'         occasion_var: \tab Identifies how to find latitude and longitude for starting point\cr
#'         alt_var: \tab Identifies how to find latitude and longitude for alternative choice \cr
#'         zoneRow: \tab Zones and choices array\cr
#'         zone_cent: \tab Geographic centroid for each zone. Generated from [find_centroid()]\cr
#'         fish_cent: \tab Fishing centroid for each zone. Generated from [find_fishing_centroid()]\cr
#'         zone_cent_name: \tab Name of the zonal centroid table\cr
#'         fish_cent_name: \tab Name of the fishing centroid table\cr
#'         spat: \tab Spatial data object\cr
#'         spatID: \tab Variable in spat that identifies individuals zones
#'         }

create_alternative_choice <- 
  function(dat,
           project,
           occasion = 'zonal centroid',
           occasion_var = NULL,
           alt_var = 'zonal centroid',
           dist.unit = "miles",
           min.haul = 0,
           zoneID, 
           zone.cent.name = NULL,
           fish.cent.name = NULL,
           spatname = NULL,
           spatID = NULL,
           grid_sample = FALSE, #TODO Need to add an option for when pairwise comparison between zones that are "in" (eg wind lease area, MPA) vs "out"
           grid_sample_n = NULL,
           outsample = FALSE) {
  
  # Call in datasets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  
  # Parse main data table
  if(outsample){ # save dat as out-of-sample dataset
    outsample_dat <- dat
    dat <- parse_data_name(outsample_dat, "outsample", project)
  } else {
    dat <- parse_data_name(dat, "main", project)  
  }
  
  # Parse spatial data object
  spat_out <- data_pull(spatname, project)
  spatdat <- spat_out$dataset
  spat <- parse_data_name(spatname, "spat", project)
  
  # Check args ----
  
  # Run a series of checks for various input args
  # Stop executing code in this function if anything here fails
  column_check(dataset, cols = c(zoneID, occasion_var))
  o_len <- length(occasion_var)
  if (occasion == "zonal centroid") {
    if (o_len != 2 & is_value_empty(zone.cent.name)) {
      stop("'zone.cent.name' is required.", call. = FALSE)
    }
  }
  
  if (occasion == "fishing centroid") {
    if (o_len != 2 & is_value_empty(fish.cent.name)) {
      stop("'fish.cent.name' is required.", call. = FALSE)
    }
  }
  
  if (alt_var == "zonal centroid" & is_value_empty(zone.cent.name)) {
    stop("'zone.cent.name' is required.", call. = FALSE)
  }
  
  if (alt_var == "fishing centroid" & is_value_empty(fish.cent.name)) {
    stop("'fish.cent.name' is required.", call. = FALSE)
  }
 
  occasion_opts <- c("zonal centroid", "fishing centroid", "port", "lon-lat")
  alt_opts <- c("zonal centroid", "fishing centroid", "nearest point")
  
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
              "variables: ", paste0(occ, collapse = ", "), call. = FALSE)
    }
  }
  
  cent_check <- function(project, cent.tab, type = "zone") {
    cent_type <- switch(type, zone = "Zonal", fish = "Fishing")
    cent_exists <- table_exists(cent.tab, project)
    if (!cent_exists) {
      stop(cent_type, " centroid table must be saved to FishSET Database. Run ", 
           "create_centroid().", call. = FALSE)
    }
    
    cent_tab <- table_view(cent.tab, project)
    if (!any(cent_tab$ZoneID %in% unique(dataset[[zoneID]]))) {
      stop('Zones do not match between centroid table and zonal assignments ',
           'in primary data table. Rerun find_centroid() using same spatial data file ',
           'as was using with the assignment_column() function.', call. = FALSE)
    }
    cent_tab
  }
  
  ## units ----
  valid_units <- c('m','meter', 'meters', 'km', 'kilometer', 'kilometers', 
                  'mile', 'miles', 'nmile', 'nmiles')
  
  if (!dist.unit %in% valid_units) {
    stop(dist.unit, " is not an available unit. Unit options are: ", 
         paste0(valid_units, collapse = ", "), call. = FALSE)
  }
  
  # alt_var ----
  # Initialize empty centroid vars
  zone_cent <- NULL
  fish_cent <- NULL
  
  ## zonal centroid ----
  if (alt_var == "zonal centroid") {
    zone_cent <- cent_check(project, zone.cent.name, "zone")
  }
  
  ## fishing centroid ----
  if (alt_var == "fishing centroid") {
    fish_cent <- cent_check(project, fish.cent.name, "fish")
  }
  
  ## nearest point ----
  if (alt_var == "nearest point") {
    if (is_value_empty(spatdat) | is_value_empty(spatID)) {
      stop("'spat' and 'spatID' are required for alt_var = 'nearest point'",
           call. = FALSE)
    }
    
    if (!any(unique(spatdat[[spatID]]) %in% unique(dataset[[zoneID]]))) {
      stop("There are no shared zones between dat and spat. Check that 'spatID' ",
           "and 'zoneID' are correct, or rerun assignment_column().", call. = FALSE)
    }
  }
  
  # occasion ----
  ## zonal centroid ----
  if (occasion == "zonal centroid") {
    
    if (is_value_empty(occasion_var) | o_len == 1) {
      
      if (is.null(zone_cent)) {
        zone_cent <- cent_check(project, zone.cent.name, "zone")
      }
      
    } else if (o_len == 2) {
      ll_occ_check(occasion_var)
      
    } else {
      stop("Invalid 'occasion_var'.", call. = FALSE)
    }
    
  ## fishing centroid ----
  } else if (occasion == "fishing centroid") {
    
    if (is_value_empty(occasion_var) | o_len == 1) {
      
      if (is.null(fish_cent)) {
        fish_cent <- cent_check(project, fish.cent.name, "fish")
      }
      
    } else if (o_len == 2) {
      ll_occ_check(occasion_var)
      
    } else {
      
      stop("Invalid 'occasion_var'.", call. = FALSE)
    } 
  
  ## port ----
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
  
  # Check if spat and spatID are NULL
  if(spat == "NULL"){
    spat_out <- NULL
  } else {
    spat_out <- spat
  }
  
  # min hauls ----
  # if grid sample, then add dummy rows for zones that were not observed in the main data
  # if(grid_sample & !(all(spatdat[[spatID]] %in% dataset[[zoneID]]))){ # when sampling from grid and not all zones are present in the main dataset
  #   zones_not_in_maindat <- unique(spatdat[[spatID]])[which(!(unique(spatdat[[spatID]]) %in% unique(dataset[[zoneID]])))]
  #   dataset[(nrow(dataset)+1):(nrow(dataset)+length(zones_not_in_maindat)),] <- NA
  #   dataset$ZoneID <- replace(dataset$ZoneID, (length(dataset$ZoneID) - length(zones_not_in_maindat) + 1):length(dataset$ZoneID), zones_not_in_maindat)
  # }
  
  choice <- dataset[[zoneID]]
  
  if (anyNA(choice) == TRUE) {
    warning("No zone identified for ", sum(is.na(choice)), " observations. These ", 
            "observations will be removed in future analyses.", call. = FALSE)
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

  # create connection to database
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project=project))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  # Grid-sampling ----
  # TODO - as we build additional options for grid sampling we will want to move this code to its own function
  if(grid_sample){
    # Create a matrix with nrows = observations, and columns = alts. The first column = choice.
    rand_alts_mat <- matrix(choice, nrow = length(choice), ncol = 1)
    
    draw_alts <- function(choice, all_alts, numAlts){
      c(choice, sample(all_alts[!(all_alts %in% choice)], numAlts))
    }
    
    rand_alts_mat <- do.call(rbind, lapply(rand_alts_mat, draw_alts, all_alts = zone_cent$ZoneID, numAlts = grid_sample_n))  
  
  } else {
    rand_alts_mat <- NULL
  }
  
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
    fish_cent = fish_cent,
    zone_cent_name = zone.cent.name,
    fish_cent_name = fish.cent.name,
    spat = spatdat,
    spatID = spatID,
    spatname = spat,
    grid_sample = grid_sample,
    grid_sample_n = grid_sample_n,
    rand_alts_mat = rand_alts_mat)
  
  # write Alt to datafile ----
  # Save table names
  if(!outsample){
    single_sql <- paste0(project, "AltMatrix")
    date_sql <- paste0(project, "AltMatrix", format(Sys.Date(), format = "%Y%m%d"))  
    
  } else {
    single_sql <- paste0(project, "AltMatrixOutSample")
    date_sql <- paste0(project, "AltMatrixOutSample", format(Sys.Date(), format = "%Y%m%d"))  
    
  }
  
  # Remove existing tables
  if (table_exists(single_sql, project)) {
    
    table_remove(single_sql, project)
  }
  
  if (table_exists(date_sql, project)) {
    
    table_remove(date_sql, project)
  }
  
  # Creates an undated alt matrix table (why?)
  DBI::dbExecute(fishset_db, paste("CREATE TABLE IF NOT EXISTS", single_sql, "(AlternativeMatrix ALT)"))
  DBI::dbExecute(fishset_db, paste("INSERT INTO", single_sql, "VALUES (:AlternativeMatrix)"),
                 params = list(AlternativeMatrix = list(serialize(Alt, NULL))))
  # Creates a dated alt matrix table
  DBI::dbExecute(fishset_db, paste("CREATE TABLE IF NOT EXISTS", date_sql, "(AlternativeMatrix ALT)"))
  DBI::dbExecute(fishset_db, paste("INSERT INTO", date_sql, "VALUES (:AlternativeMatrix)"),
                 params = list(AlternativeMatrix = list(serialize(Alt, NULL))))
  
  if(!outsample){
    message('Alternative choice list saved to FishSET database')  
  } else {
    message('Out-of-sample alternative choice list saved to FishSET database')  
  }
  
  # TODO: return TRUE invisibly if successful, FALSE if not (for testing and app)

  create_alternative_choice_function <- list()
  create_alternative_choice_function$functionID <- "create_alternative_choice"
  create_alternative_choice_function$args <- 
    list(dat, project, occasion, occasion_var, alt_var, 
         dist.unit, min.haul, zoneID, zone.cent.name, 
         fish.cent.name, spatname, spatID)
  
  create_alternative_choice_function$kwargs <- list()
  create_alternative_choice_function$output <- list()

  log_call(project, create_alternative_choice_function)
}
