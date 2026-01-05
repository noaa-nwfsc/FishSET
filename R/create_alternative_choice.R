#' Define alternative fishing choice
#'
#' Required step. Creates a list identifying how alternative fishing choices should
#' be defined. Output is saved to the FishSET database. Run this function before
#' running models. `dat` must have a zone assignment column (see
#' [assignment_column()]). In certain cases a centroid table must be saved to
#' the FishSET Database, see `occasion_var` for details.
#'
#' @param dat Required, Primary data frame containing data on hauls or trips.
#'  Table in FishSET database should contain the string `MainDataTable`.
#' @param project Required, name of project.
#' @param alt_name String, **Required**. The name to be assigned to this
#'  alternative choice list within the FishSET database. If a list with this name
#'  already exists, the function will stop.
#' @param zoneID Variable in `dat` that identifies the individual zones or
#'  areas.
#' @param occasion String, determines the starting point when calculating the
#'  distance matrix. Options are `"zonal centroid"`, `"fishing centroid"`,
#'  `"port"`, or `"lon-lat"`. See `occasion_var` for requirements.
#' @param occasion_var Identifies an ID column or set of lon-lat variables needed
#'   to create the distance matrix. Possible options depend on the value of
#'  `occasion`:
#'   \describe{
#'     \item{Centroid}{When `occasion = zonal centroid` the possible
#'       options are `NULL`, the name of a zone ID variable, or a set coordinate
#'       variables (in Lon-Lat order).
#'        \describe{
#'          \item{NULL}{This will merge centroid lon-lat data to the primary table
#'            using the column enter in `zoneID`. A centroid table must be saved
#'            to the FishSET Database.}
#'         \item{Zone ID}{This option specifies the zone ID variable to merge the
#'            centroid table to. For example, a column containing the previous zonal
#'            area. A centroid table must be saved to the FishSET Database.}
#'          \item{Lon-Lat}{A string vector of length two containing the longitude
#'            and latitude of an existing set centroid variables in `dat`.}
#'         }
#'      }
#'    \item{Port}{When `occasion = port` the possible options include the name
#'      of a port ID variable or a set of lon-lat variables describing the
#'      location of the port. A value of `NULL` will return an error.
#'       \describe{
#'          \item{Port ID}{The name of a port ID variable in `dat` that will be
#'            used to join the port table to the primary table. A port table
#'            is required (see [load_port()]) which contains the port name and
#'            the longitude and latitude of each port.}
#'         \item{Lon-Lat}{A string vector of length two containing a port's
#'            longitude and latitude in `dat`.}
#'      }
#'   }
#'    \item{Lon-Lat}{When `occasion = lon-lat`, `occasion_var` must contain a
#'      string vector of length two containing the longitude and latitude of a
#'      vessel's location in the `dat`. For example, the current or previous
#'      haul location.}
#' }
#' @param alt_var Determines the alternative choices used to calculate the distance
#'  matrix. `alt_var` may be the centroid of zonal assignment (`"zonal centroid"`),
#'  `"fishing centroid"`, or the closest point in fishing zone
#'  (`"nearest point"`). The centroid options require that the appropriate
#'  centroid table has been saved to the project's FishSET Database. See
#'  [create_centroid()] to create and save centroids. List existing centroid
#'  tables by running `list_tables("project", type = "centroid")`.
#' @param min_haul Required, numeric, minimum number of hauls. Zones with fewer
#'  hauls than the `min_haul` value will not be included in model data.
#' @param spatname Required when `alt_var = 'nearest point'`. `spat` is a spatial
#'  data file containing information on fishery management or regulatory zones
#'  boundaries. `sf` objects are recommended, but `sp` objects can be used as
#'  well. See [dat_to_sf()] to convert a spatial table read from a csv file to
#'  an `sf` object. To upload your spatial data to the FishSETFolder see
#'  [load_spatial()].If `spat` should come from the FishSET database, it should
#'  be the name of the original file name, in quotes. For example,
#'  `"pollockNMFSZonesSpatTable"`. Use [tables_database()] or
#'  `list_tables("project", type = "spat")` to view the names of spatial tables
#'  in the FishSET database.
#' @param spatID Required when `alt_var = 'nearest point'`. Variable in `spat`
#'  that identifies the individual zones or areas.
#' @param outsample Logical, indicating whether this is for primary data or out-of
#'  sample data.
#' @importFrom DBI dbExecute
#' @export create_alternative_choice
#' @md
#' @details Defines the alternative fishing choices. These choices are used to develop
#'  the matrix of distances between observed and alternative fishing choices (where
#'  they could have fished but did not). The distance matrix is calculated by the
#'  [make_model_design()] function. `occasion` defines the observed fishing
#'  location and `alt_var` the alternative fishing location. `occasion_var`
#'  identifies an ID column or set of lon-lat variables needed to create the
#'  distance matrix.
#'
#'  Parts of the alternative choice list are pulled by [create_expectations()],
#'  [make_model_design()], and the model run [discretefish_subroutine()])
#'  functions. These output include choices of which variable to use for catch and
#'  which zones to include in analyses based on a minimum number of hauls per trip
#'  within a zone. Note that if the alternative choice list is modified, the
#'  [create_expectations()] and [make_model_design()] functions
#'  should also be updated before rerunning models.
#' 
#' @return Function saves a list of alternative choice matrices to the FishSET
#'  database as `projectAlternativeChoice`. The list includes
#'  the alternative choice list from the user-defined choices. Multiple alternative
#'  choice cases can be added to the list by specifying unique names. The list is
#'  automatically saved to the FishSET database and is called in
#'  `make_model_design`.

create_alternative_choice <- function(dat,
                                      project,
                                      alt_name = NULL,
                                      zoneID,
                                      occasion = 'zonal centroid',
                                      occasion_var = NULL,
                                      alt_var = 'zonal centroid',
                                      min_haul = 0,
                                      spatname = NULL,
                                      spatID = NULL,
                                      outsample = FALSE) {
  
  # Setup and Naming -----------------------------------------------------------------------------
  # Define the SQL table name based on whether this is in-sample or out-of-sample
  single_sql <- paste0(project, if (!outsample) "AltMatrix" else "OutSample")
  
  # Generate a default name if none provided, to prevent failures
  if (is_value_empty(alt_name)) {
    alt_name <- paste0("AltMatrix_", format(Sys.Date(), format = "%Y%m%d"))
    warning("No 'alt_name' provided. Using default name: '", alt_name, "'.", call. = FALSE)
  }
  
  # Data Loading and parsing ---------------------------------------------------------------------
  # Pull the main dataset
  out <- data_pull(dat, project)
  dataset <- out$dataset
  
  if(outsample){ 
    outsample_dat <- dat
    dat <- parse_data_name(outsample_dat, "outsample", project)
  } else {
    dat <- parse_data_name(dat, "main", project)
  }
  
  # Pull spatial data if needed
  spat_out <- data_pull(spatname, project)
  spatdat <- spat_out$dataset
  spat <- parse_data_name(spatname, "spat", project)
  
  # Ensure required columns exist in the loaded dataset
  column_check(dataset, cols = c(zoneID, occasion_var))
  o_len <- length(occasion_var)
  
  # Define allowed options
  occasion_opts <- c("zonal centroid", "fishing centroid", "port", "lon-lat")
  alt_opts <- c("zonal centroid", "fishing centroid", "nearest point")
  
  # Validate lengths and option validity
  if (o_len > 2) stop("Invalid values for 'occasion_var'.", call. = FALSE)
  
  if (!occasion %in% occasion_opts) {
    stop("Invalid option for 'occasion'. Options are ", 
         paste0(occasion_opts, collapse = ", "), ".", call. = FALSE)
  }
  
  if (!alt_var %in% alt_opts) {
    stop("Invalid option for 'alt_var'. Options are ",
         paste0(alt_opts, collapse = ", "), ".", call. = FALSE)
  }
  
  # Internal Helper Functions --------------------------------------------------------------------
  # Helper to ensure lat/lon strings appear in variable names
  ll_occ_check <- function(occ) {
    ll_check <- grepl("lon|lat", occ, ignore.case = TRUE)
    if (!any(ll_check)) {
      warning("Check that 'occasion_var' contains longitude and latitude ",
              "variables: ", paste0(occ, collapse = ", "), call. = FALSE)
    }
  }
  
  # Helper to verify centroid tables exist and match the zones in the data
  cent_check <- function(project, type = "zone") {
    cent_type <- switch(type, zone = "Zone", fish = "Fishing")
    cent_exists <- table_exists(paste0(project, cent_type, "Centroid"), project)
    if (!cent_exists) {
      stop(cent_type, " centroid table must be saved to FishSET Database. Run ",
           "create_centroid().", call. = FALSE)
    }
    
    cent_tab <- table_view(paste0(project, cent_type, "Centroid"), project)
    
    if (!any(cent_tab$ZoneID %in% unique(dataset[[zoneID]]))) {
      stop('Zones do not match between centroid table and zonal assignments ',
           'in primary data table. Rerun find_centroid() using same spatial data file ',
           'as was using with the assignment_column() function.', call. = FALSE)
    }
    cent_tab
  }
  
  # Empty tables to hold centroid tables
  zone_cent <- NULL
  fish_cent <- NULL
  
  # Alternative Variable Configuration (alt_var) --------------------------------------------------
  # Load and validate the specific centroid/spatial tables requested
  if (alt_var == "zonal centroid") {
    zone_cent <- cent_check(project,"zone")
    
  }else if (alt_var == "fishing centroid") {
    fish_cent <- cent_check(project, "fish")
    
  }else if (alt_var == "nearest point") {
    if (is_value_empty(spatdat) | is_value_empty(spatID)) {
      stop("'spat' and 'spatID' are required for alt_var = 'nearest point'", call. = FALSE)
    }
    
    if (!any(unique(spatdat[[spatID]]) %in% unique(dataset[[zoneID]]))) {
      stop("There are no shared zones between dat and spat. Check that 'spatID' ",
           "and 'zoneID' are correct, or rerun assignment_column().", call. = FALSE)
    }
  }
  
  # Occasion Variable Configuration --------------------------------------------------------------
  # Validate logic based on the 'occasion' type (zonal vs fishing vs port vs lon-lat)
  if (occasion == "zonal centroid") {
    if (is_value_empty(occasion_var) | o_len == 1) {
      if (is.null(zone_cent)) {
        zone_cent <- cent_check(project, "zone")
      }
    } else if (o_len == 2) {
      ll_occ_check(occasion_var)
    } else {
      stop("Invalid 'occasion_var'.", call. = FALSE)
    }
    
  } else if (occasion == "fishing centroid") {
    if (is_value_empty(occasion_var) | o_len == 1) {
      if (is.null(fish_cent)) {
        fish_cent <- cent_check(project, "fish")
      }
    } else if (o_len == 2) {
      ll_occ_check(occasion_var)
    } else {
      stop("Invalid 'occasion_var'.", call. = FALSE)
    }
    
  } else if (occasion == "port") {
    if (is_value_empty(occasion_var)) {
      stop("Port column name required for 'occasion = port'.", call. = FALSE)
    }
    if (o_len == 2) {
      ll_occ_check(occasion_var)
    }
    
  } else if (occasion == "lon-lat") {
    if (o_len != 2) {
      stop("'occasion_var' must contain a longitude and latitude column.", call. = FALSE)
    }
    ll_occ_check(occasion_var)
    
  } else {
    stop("Invalid 'occasion' option.", call. = FALSE)
  }
  
  # Data Filtering (Minimum Hauls) ----------------------------------------------------------------
  choice <- dataset[[zoneID]]
  
  if (anyNA(choice) == TRUE) {
    warning("No zone identified for ", sum(is.na(choice)), " observations. These ",
            "observations will be removed in future analyses.", call. = FALSE)
  }
  
  # Count hauls per zone to filter out under-represented zones
  zoneCount <- agg_helper(dataset, value = zoneID, count = TRUE, fun = NULL)
  
  # Mark zones with fewer than min_haul as NA
  zoneCount[zoneCount$n < min_haul, zoneID] <- NA
  
  if (all(is.na(zoneCount[[zoneID]]))) {
    stop("No zones meet criteria. No data will be included in further analyses.",
         " Check the 'min_haul' parameter or zone identification.", call. = FALSE)
  }
  
  # Filter zone list to only valid zones
  zoneCount <- zoneCount[!is.na(zoneCount[[zoneID]]), ]
  
  # Get list of valid zone IDs
  greaterNZ <- zoneCount[[zoneID]]
  
  # Create binary index of observations to include (1 = keep, 0 = drop)
  dataZoneTrue <- as.numeric(dataset[[zoneID]] %in% greaterNZ)
  
  # Handle spatial name string for output
  if(spat == "NULL"){
    spat_out_name <- NULL
  } else {
    spat_out_name <- spat
  }
  
  # Construct the Alternative Choice List --------------------------------------------------------
  Alt_current <- list(
    dataZoneTrue = dataZoneTrue, # Index for model inclusions
    greaterNZ = greaterNZ,       # Zones meeting min_haul criteria
    numOfNecessary = min_haul,
    choice = choice,
    altChoiceType = "distance",
    occasion = occasion,
    occasion_var = occasion_var,
    alt_var = alt_var,
    zoneHist = zoneCount,
    zoneRow = zoneCount[, zoneID], 
    zoneID = zoneID,
    zone_cent = zone_cent,
    fish_cent = fish_cent,
    spat = spatdat,
    spatID = spatID,
    spatname = spat_out_name
  )
  
  # Database Connection --------------------------------------------------------------------------
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project))
  # Ensure DB disconnects even if function crashes later
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  AltList <- list()
  
  # If the table exists, load it first to preserve existing lists
  if (table_exists(single_sql, project)) {
    AltList <- unserialize_table(single_sql, project)
    
    # Prevent overwriting an existing named list accidentally
    if (alt_name %in% names(AltList)) {
      stop("An alternative choice list with the name '", alt_name, 
           "' already exists in the database table '", single_sql,
           "'. Please enter a new unique name.", call. = FALSE)
    }
    
    # Safely Remove Old Table if it existed
    table_remove(single_sql, project)
  }
  
  
  # Append this run's configuration to the main list using the unique alt_name
  AltList[[alt_name]] <- Alt_current
  
  
  # Create table with specific column name 'AlternativeMatrix' and type BLOB
  DBI::dbExecute(fishset_db,
                 paste("CREATE TABLE IF NOT EXISTS",
                       single_sql,
                       "(AlternativeMatrix BLOB)") 
  )
  
  # Insert the serialized list into the 'AlternativeMatrix' column
  DBI::dbExecute(fishset_db,
                 paste("INSERT INTO",
                       single_sql,
                       "(AlternativeMatrix)", 
                       "VALUES (:AlternativeMatrix)"),
                 params = list(AlternativeMatrix = list(serialize(AltList, NULL)))
  )
  
  # User Feedback and Logging ---------------------------------------------------------------------
  if(!outsample){
    message("Alternative choice list '", alt_name,
            "' saved to FishSET database under table ", single_sql)
  } else {
    message("Out-of-sample alternative choice list '", 
            alt_name, "' saved to FishSET database under table ", single_sql)
  }
  
  # Log arguments for reproducibility
  create_alternative_choice_function <- list()
  create_alternative_choice_function$functionID <- "create_alternative_choice"
  create_alternative_choice_function$args <- as.list(match.call())[-1]
  create_alternative_choice_function$kwargs <- list()
  create_alternative_choice_function$output <- list()
  
  log_call(project, create_alternative_choice_function)
}