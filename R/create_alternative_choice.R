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
#' @param grid Data must contain a variable that varies by the spatial dataset 
#'   \code{spat}. First variable in \code{grid} should match a column in \code{dat}. 
#'   The remaining columns should match the zone IDs in the \code{spat}.
#' @param dist.unit String, how distance measure should be returned. Choices are 
#'   \code{"meters"} or \code{"M"}, \code{"kilometers"} or \code{"KM"}, \code{"miles"}, 
#'   or units of \code{grid}. Defaults to miles.
#' @param min.haul Required, numeric, minimum number of hauls. Zones with fewer 
#'   hauls than the \code{min.haul} value will not be included in model data.
#' @param cat Required, variable in either \code{dat} or \code{spat} that identifies 
#'   the individual areas or zones. If \code{cat} is a variable in \code{dat} that 
#'   identifies zone assignments for each occurrence record, set \code{spat} to 
#'   \code{NULL}. Otherwise, if \code{spat} is class \code{sf}, \code{cat} should be the 
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
#'   for specific \code{spat} file formats. \code{cat} must be specified for all 
#'   file formats. If a zonal centroid table exists in the FishSET database and a 
#'   zonal assignment column exists in \code{dat} then \code{spat} may be a table 
#'   from the FishSET database or a data file. If \code{spat} should come from the 
#'   FishSET database, it should be the name of the original file name, in quotes. 
#'   For example, 'NMFS_zones_polygons'. \code{cat} would then be the name of the 
#'   column in \code{dat} containing fishing zone assignments. Use \code{tables_database()} 
#'   to view names of tables in the FishSET database.
# @param case Centroid='Centroid of Zonal Assignment', Port, Other
#' @param hull.polygon Used in \code{\link{assignment_column}} function. Creates 
#'   polygon using convex hull method. Required if zonal assignments in \code{dat} 
#'   should be identified and \code{spat} is not \code{NULL}.
#' @param lon.dat Longitude variable from \code{dat}. Required if zonal assignments 
#'   in \code{dat} should be identified and \code{spat} is not \code{NULL}.
#' @param lat.dat Latitude variable from \code{dat}. Required if zonal assignments 
#'   in \code{dat} should be identified and \code{spat} is not \code{NULL}.
#' @param lon.spat Variable or list from \code{spat} containing longitude data. 
#'   Required for csv files. Leave as \code{NULL} if \code{spat} is a shape or json 
#'   file, Required if zonal assignments in \code{dat} should be identified and 
#'   \code{spat} is not \code{NULL}.
#' @param lat.spat Variable or list from \code{spat} containing latitude data. Required 
#'   for csv files. Leave as \code{NULL} if \code{spat} is a shape or json file, 
#'   Required if zonal assignments in \code{dat} should be identified and \code{spat} 
#'   is not \code{NULL}.
#' @param closest.pt Logical, if true, zone ID identified as the closest polygon 
#'   to the point. Called in \code{\link{assignment_column}}. Required if zonal 
#'   assignments for observations in \code{dat} should be identified and \code{spat} 
#'   is not \code{NULL}.
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
           occasion = 'centroid',
           alt_var = 'centroid',
           dist.unit = "miles",
           min.haul = 0,
           spat,
           cat = NULL,
           zoneID = NULL,
           lon.dat = NULL,
           lat.dat = NULL,
           hull.polygon = FALSE,
           closest.pt = FALSE,
           grid = NULL,
           lon.spat = NULL,
           lat.spat = NULL) {
    
  # TODO: remove grid feature for now, add later if needed
  # TODO: remove find_centroid() and assignment_column()--check if they are needed
  # based on current args and inform user
  # TODO: Let users associate a column from dat w/ area column in spat 
  # Needed if user doesn't have lon/lat data (ex. Fish Tickets)
  # TODO: add fishing centroid as occassion/alt_var option 
  case <- "centroid"
  
  # Call in datasets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  spat_out <- data_pull(spat, project)
  spatdat <- spat_out$dataset
  spat <- parse_data_name(dat, "spat", project)
  
  grid_out <- data_pull(grid, project)
  griddat <- grid_out$dataset
  grid <- parse_data_name(grid, "grid", project)
  
  # TODO: column name checks for dat, spat, and grid (make sure columns exist)
  
  
  # Note: Currently, only one centroid table can exist for each project
  # this would need to change if multiple spatial scales are being modeled 
  cent_exists <- 
    table_exists(paste0(spat, 'Centroid'), project) || 
    table_exists("spatCentroid", project)

  # TODO: check whether centroid exists first
  if (!is.null(spatdat)) {
    
      int <- suppressWarnings(
        
        find_centroid(project = project, spat = spatdat, cat = cat, 
                      lon.spat = lon.spat, lat.spat = lat.spat)
        )
      
  } else if (cent_exists) {
    
    if (table_exists(paste0(spat, 'Centroid'), project)) {

      int <- table_view(paste0(spat, 'Centroid'), project)

    } else {
      # Nit: Not sure it makes sense to have two different centroid naming conventions
      int <- table_view('spatCentroid', project)
    }
    
  } else {
    
    stop("Zonal centroid must be defined. Function not run.", call. = FALSE)
  }
 
  # TODO: Simplify this. Remove 'ZoneID' default. Make zoneID required (must exist in dat). 
  # Not clear why cat is changed.
  
  if ('ZoneID' %in% names(dataset) || cat %in% names(dataset) || 
     !is.null(zoneID) && zoneID %in% names(dataset)) {
    
        int.data <- dataset
        
    if ('ZoneID' %in% names(dataset)) {
      
        cat <- 'ZoneID'
        
    } else if (!is.null(zoneID) && zoneID %in% names(dataset)) {
      
        cat <- zoneID
        
    } else if (cat %in% names(dataset)) {
      
        cat <- cat 
    }
        
  } else {
    
  if (!is.null(spatdat) & !is.character(spatdat)) { # is.character(spatdat) shouldn't happen
    
      if (is.null(lon.dat)) {
        
        stop('Observations must be assigned to zones. Function not run.', call. = FALSE)
        
      } else {
        # name will default to "ZoneID" 
        int.data <- assignment_column(
          dat = dataset, project=project, spat = spatdat, hull.polygon = hull.polygon,
          lon.spat = lon.spat, lat.spat = lat.spat, lon.dat = lon.dat,
          lat.dat = lat.dat, cat = cat, closest.pt = closest.pt, log.fun = FALSE
        )
      }
    } # FALSE case?
  }

 
  if (!any(int[,1] %in% int.data[[cat]])) {
    # Note: Not sure what this is checking. Is it, "are any zone IDs from the centroid table not in main data?"
    # Meant to update centroid table, I think. 
    # This statement will return FALSE if any zones from centroid table are in the dataset
    # Nit: should also refer to this column by it's assigned name used in find_centroid(): currently ZoneID

    if (!is.null(spatdat)) {

      int <- find_centroid(spat = spatdat, project = project, lon.spat = lon.spat,
                           lat.spat = lat.spat, cat = cat, log.fun = FALSE)

    } else {
      
      stop('Zones do not match between centroid table and zonal assignments',
           ' in main data table. Rerun find_centoid using same spatial data file',
           ' as was using with the assignment_column function.', call. = FALSE)
    }
  }

    
  if (!is.null(int.data[[cat]])) {
    
    g <- int.data[[cat]]
    
  } else if (!is.null(int.data[['ZoneID']])) {
    
    g <- int.data[['ZoneID']]
  }
  
  if (anyNA(g) == TRUE) {
    
    warning(paste("No zone identified for", sum(is.na(g)), "observations. 
                  These observations will be removed in future analyses."))
  }
  

  choice <- data.frame(g)  
  
  # Q: Is this meant to check whether "startingloc" exists as a column in dat?
  # What if named something else? Add as new arg?  
  startingloc <- if (!"startingloc" %in% colnames(int.data)) {
    
    rep(NA, nrow(int.data))
    
  } else {
    
    data.frame(int.data$startingloc)
  }

  if (is.null(choice)) {
    
    stop("Choice must be defined. Ensure that the zone or area assignment variable",
         " (cat parameter) is defined.")
  }
  
  # TODO: this will always be true (see line 111)
  if (case == "centroid") {
    
    # unique zones w/o NAs
    B <- as.data.frame(unique(g[!is.na(g)])) 
    
    # zone index (of B)
    C <- match(g[!is.na(g)], unique(g[!is.na(g)]))
  
  } else {
    # TODO: this is an unreliable method for finding area/zone cols
    # should use zoneID
    a <- colnames(dataset)[grep("zon|area", colnames(dataset), ignore.case = TRUE)] # find data that is zonal type

    temp <- cbind(as.character(g), dataset[[a[1]]]) 
    B <- unique(temp) # Correct ->> Needs to be lat/long
    C <- match(paste(temp[, 1], temp[, 2], sep = "*"), paste(B[, 1], B[, 2], sep = "*"))
  }

  # Zone counts 
  numH <- accumarray(C, C)
  binH <- 1:length(numH)
  numH <- numH / t(binH)
  # numH = number of obs, binH = zone index, B = name of zone
  zoneHist <- data.frame(numH = as.vector(numH), binH = as.vector(binH), B[, 1])

  # mark zones under min.haul as NA
  zoneHist[which(zoneHist[, 1] < min.haul), 3] <- NA

  # TODO: simplify this check
  if (any(is_empty(which(is.na(zoneHist[, 3]) == FALSE)))) {
    
    stop("No zones meet criteria. No data will be included in further analyses.",
         " Check the min.haul parameter or zone identification.")
  }

  # matrix of which zones to include: first column whether to include (0/1), 
  # second column the index of the assigned zone
  dataZoneTrue <- cbind(g %in% zoneHist[, 3], match(g, zoneHist[, 3], nomatch = 0))
  
  # vector index of zones that meet/exceed min.haul
  greaterNZ <- which(zoneHist[, 1] >= min.haul)
  numOfNecessary <- min.haul

  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project=project))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)

  Alt <- list(
    dataZoneTrue = dataZoneTrue[, 1], # array of logical values to identify which are to be used in model
    greaterNZ = greaterNZ,
    numOfNecessary = numOfNecessary, # input
    choice = choice,
    altChoiceUnits = dist.unit, # miles
    altChoiceType = "distance",
    occasion = occasion, # altToLocal1
    alt_var = alt_var, # altToLocal2
    startingloc = startingloc,
    zoneHist = zoneHist,
    zoneRow = zoneHist[greaterNZ, 3], # zones and choices array
    zoneID = cat,
    int = int # centroid table
    )


  # Q: Why do this after creating Alt list?
  # TODO: Consider making a function that converts gridded datasets to required 
  # matrix output. Handle both wide and long formats. Need additional arguments. 
  ### Add gridded data ###
  if (!is.null(griddat)) {
    # TODO: change gridVar to griddat
    gridVar <- griddat
    
    # removes non-numerical characters from column names
    # grid_names <- noquote(gsub("[^0-9]", "", colnames(gridVar)))
    grid_names <- colnames(gridVar)

    if (!is.null(int.data[[cat]])) {
      
      g <- int.data[[cat]]
      
    } else if (!is.null(int.data[['ZoneID']])) {
      
      g <- int.data[['ZoneID']]
    }
        
    # Note: Assumes that area values can't have non-numeric characters
    # TODO: change this so that long-form data can work
    
    # if (any(noquote(gsub("[^0-9]", "", colnames(gridVar))) %in% g) == FALSE) {
    #   
    #   stop("Cannot use grid. Column names of grid do not match zone",
    #        " IDs in spatial dataset")
    # }

    # If gridded data is not an array, need to create matrix
    
    # Q: is this for cases where gridded data contains a single value for 
    # each area, i.e. does not vary by time etc? An alternative specific constant?
    if (dim(gridVar)[1] == 1) { # (is_empty(gridVar.row.array)){ #1d
      
      biG <- match(Alt[["zoneRow"]], grid_names) # FIXME FOR STRING CONNECTIONS
      numRows <- nrow(dataset)
      
      if (!any(biG)) {
        
        stop("The map associated to the data and the grid information in the",
             " gridded variable do not overlap.")
      }
      # matrix # of obs x Area. Each row is identical. 
      allMat <- matrix(1, numRows, 1) %x% as.matrix(gridVar[1, biG]) 
      
    } else {
      
      # biG <- match(Alt[["zoneRow"]], grid_names[-1]) 
      biG <- Alt$zoneRow %in% grid_names[-1] # TODO: change grid_names[-1] to less faulty approach
      if (!any(biG)) { # if no areas from grid are in dataset
        
        stop("The map associated to the data and the grid information in the",
             " gridded variable do not overlap.")
      }

      # TODO: check if this is necessary
      if (names(gridVar)[1] %in% colnames(dataset) == FALSE) {
        # wrong occurrence variable to connect data
        stop("The data in the workspace and the loaded grid file do not have a", 
             " matching variable for connecting.")
      }

      # Note: believe the "D" in "biD" stands for date
      # Assuming we know which columns in grid are date/id cols -- add args?
      # Match ids between dataset and grid
      biD <- match(dataset[, names(gridVar)[1]], gridVar[, 1])
      
      biD <- dataset[[names(gridVar)[1]]] %in% gridVar[, 1]
      
      if (!any(biD)) {
        # stop?
        message("The data in the workspace and the loaded grid file do not have a",
                " matching variable for connecting.")
      }

      # Note: believe this is suppose to be a matrix of gridded values from areas 
      # and dates that exist in dat (with date col removed)
      # TODO: this repeats the first row of the gridded data by # of obs in data.
      # All other rows are dropped, why? 
      # Q: how many rows should there be? should dim = # obs x # zones? 
      # Join to dataset instead? Need to know what correct format is. 
      allMat <- gridVar[, -1][biD, biG]
    }
    
    if (anyNA(allMat[Alt[["dataZoneTrue"]], ])) {
      
      stop("Problem with loaded matrix, NA found.")
    }
   
    # add grid matrix to Alt choice list
    # Note: Need to change this if adding more than one gridded dataset
    Alt <- c(Alt, matrix = list(allMat[Alt[["dataZoneTrue"]], ]))
  }

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
    list('dat' = dat, 'project' = project, 'occasion' = occasion, alt_var,  dist.unit, 
         min.haul, spat, cat, zoneID, lon.dat, lat.dat, hull.polygon, closest.pt)
  
  create_alternative_choice_function$kwargs <- 
    list("lon.spat" = lon.spat, "lat.spat" = lat.spat, "grid" = grid)
  create_alternative_choice_function$output <- list()

  log_call(project, create_alternative_choice_function)
}
