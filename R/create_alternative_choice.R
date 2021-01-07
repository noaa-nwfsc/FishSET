#' Create alternative choice list
#'
#' Required step. Creates a list defining how alternative fishing choices should be defined. Output is saved to the FishSET database.
#'  Run this function before running models.  
#'
#' @param dat  Required, main data frame containing data on hauls or trips.
#'   Table in FishSET database should contain the string `MainDataTable`.
#' @param project Required, name of project
#' @param alt_var String, identifies how to find lat/lon for starting point (must have a lon/lat associated with it).
#' \code{alt_var} may be the ‘centroid of zonal assignment’, a port variable or lon/lat variable(s) in the primary dataset.
#' If a port variable is defined, a corresponding port table must exist which contains the port name and the longitude and
#' latitude of each port. For lon/lat variables, longitude must be specified first.
#' @param occasion Identifies how to find lat/lon for alternative choices. Occasion may be the centroid of zonal assignment 
#'  \code{"Centroid"}, or lon/lat variables in the primary dataset. Longitude must be specified first.
#' @param griddedDat Data must contain a variable that varies by the spatial dataset \code{gridfile}.
#'  First variable in \code{griddedDat} should match a column in \code{dat}. The remaining columns should match the
#'  zone IDs in the \code{gridfile}.
#' @param dist.unit String, how distance measure should be returned.
#'  Choices are \code{"meters"} or \code{"M"}, \code{"kilometers"} or \code{"KM"}, \code{"miles"}, or units of 
#'  \code{griddedDat}. Defaults to miles.
#' @param min.haul Required, numeric, minimum number of hauls. Zones with fewer hauls than the \code{min.haul}
#' value will not be included in model data.
#' @param cat Required, variable in either \code{dat} or \code{gridfile} that identifies the individual areas or zones.
#'  If \code{cat} is a variable in \code{dat} that identifies zone assignments for each occurrence record,
#'   set \code{gridfile} to NULL. Otherwise, if \code{gridfile} is class sf, \code{cat} should be the name 
#'   of the list containing information on zones.
#' @param gridfile Required, data file or character. 
#'   \code{gridfile} is a spatial data file containing information on fishery management or regulatory zones boundaries.
#'   Shape, json, geojson, and csv formats are supported. geojson is the preferred format. json files must be converted
#'   into geoson. This is done automatically when the file is loaded with \code{\link{read_dat}} with \code{is.map} set to true.
#'   \code{gridfile} cannot, at this time, be loaded from the FishSET database. \cr
#'   \code{lon.dat}, \code{lat.dat}, \code{lon.grid}, and \code{lat.grid} are required for specific \code{gridfile} file formats. \cr
#'   \code{cat} must be specified for all file formats.
#'   If a zonal centroid table exists in the FishSET database and a zonal assignment column 
#'  exists in \code{dat} then \code{gridfile} may be a table from the FishSET database or a data file. If \code{gridfile} should 
#'  come from the FishSET database, it should be the name of the original file name, in quotes. For example, 'NMFS_zones_polygons'.
#'  \code{cat} would then be the name of the column in \code{dat} containing fishing zone assignments.
# @param case Centroid='Centroid of Zonal Assignment', Port, Other
#' @param hull.polygon Used in \code{\link{assignment_column}} function. Creates polygon using convex
#'   hull method. Required if zonal assignments in \code{dat} should be identified
#'   and \code{gridfile} is not NULL.
#' @param lon.dat Longitude variable from \code{dat}. Required if zonal assignments in \code{dat}
#'   should be identified and \code{gridfile} is not NULL.
#' @param lat.dat Latitude variable from \code{dat}. Required if zonal assignments in \code{dat}
#'   should be identified and \code{gridfile} is not NULL.
#' @param lon.grid Variable or list from \code{gridfile} containing longitude data. Required for csv files.
#'  Leave as NULL if \code{gridfile} is a shape or json file, Required if zonal assignments
#'  in \code{dat} should be identified and \code{gridfile} is not NULL.
#' @param lat.grid Variable or list from \code{gridfile} containing latitude data. Required for csv files.
#' Leave as NULL if \code{gridfile} is a shape or json file, Required if zonal assignments
#' in \code{dat} should be identified and \code{gridfile} is not NULL.
#' @param weight.var Variable for calculating weighted centroids. Required if zonal assignments for observations
#' in \code{dat} should be identified and \code{gridfile} is not NULL.
#' @param closest.pt Logical, if true, zone ID identified as the closest polygon to the point.
#'   Called in \code{\link{assignment_column}}. Required if zonal assignments for observations in \code{dat}
#'   should be identified and \code{gridfile} is not NULL.
#' @importFrom DBI dbExecute
#' @export create_alternative_choice
#' @details Defines the alternative fishing choices. These choices are used to develop the matrix of distances 
#'   between observed and alternative fishing choices (where they could have fished but did not). 
#'   The distance matrix is calculated by the \code{\link{make_model_design}} function.
#'   The distance matrix can come from \code{dat} or from the gridded data frame \code{griddedDat}.
#'   If the distance matrix is to come from \code{dat}, then \code{alt_var} (observed fishing location) and 
#'   \code{occasion} (alternative fishing location) must be specified. \code{griddedDat}, if used, must be a variable that varies 
#'   by the spatial dataset, such as wind speed. Each column must be a unique zone that matches the zones in \code{dat}. \cr
#'   
#'   Parts of the alternative choice list are pulled by \code{\link{create_expectations}}, 
#'   \code{\link{make_model_design}}, and the model run (\code{\link{discretefish_subroutine}}) functions. 
#'   These output include choices of which variable to use for catch and 
#'   which zones to include in analyses based on a minimum number of hauls per trip within a zone. 
#'   Note that if the alternative choice list is modified, the \code{\link{create_expectations}}
#'   and \code{\link{make_model_design}} functions should also be updated before rerunning models.
#' @return Saves the alternative choice list to the FishSET database as a list.
#'   Output includes: \cr
#' \tabular{rlll}{
#'         dataZoneTrue: \tab Vector of 0/1 indicating whether the data from that zone is to be included in the model\cr
#'         greaterNZ: \tab Zone which pass numofNecessary test\cr
#'         numOfNecessary: \tab Minimum number of hauls for zone to be included\cr
#'         altChoiceUnits: \tab Set to miles\cr
#'         altChoiceType: \tab Set to distance\cr
#'         alt_var: \tab Identifies how to find latitude and longitude for starting point\cr
#'         occasion: \tab Identifies how to find latitude and longitude for alternative choice \cr
#'         zoneRow: \tab Zones and choices array\cr
#'         int: \tab Centroid for each zone. Generated from \code{\link{find_centroid}}\cr
#'         matrix: \tab Distance matrix is alternative choices comes from gridded dataset
#'         }

create_alternative_choice <- function(dat, project, alt_var='centroid', occasion='centroid', griddedDat = NULL, 
                                      dist.unit = "miles", min.haul=0, gridfile, cat=NULL, 
                                      lon.dat=NULL, lat.dat=NULL, hull.polygon = FALSE,
                                      closest.pt = FALSE, lon.grid = NULL, lat.grid = NULL,
                                      weight.var = NULL) {
  stopanaly <- 0
  case <- "centroid"
  
  # Call in datasets
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset
  
  if(is.null(cat)){
    cat <- 'ZoneID'
  }
  
  
  if(!is.null(gridfile)){
    if(is.character(gridfile)){
      if(table_exists(paste0(gridfile, 'centroid'))==FALSE){
        warning('Table', paste0(gridfile, 'centroid'),'not found in FishSET database. 
                Zonal centroid must be defined from a dataset')
      } else {
      int <- table_view(paste0(gridfile, 'centroid'))
      }
    } else {
  int <- find_centroid(
    dat = dataset, gridfile = gridfile, lon.grid = lon.grid, lat.grid = lat.grid,
    lat.dat = lat.dat, lon.dat = lon.dat, cat = cat, weight.var = weight.var
  )
    }
  } else {
    warning("Zonal centroid must be defined.")
  }
  
  if (!is.null(gridfile) & !is.character(gridfile)) {
    int.data <- assignment_column(
      dat = dataset, gridfile = gridfile, hull.polygon = hull.polygon,
      lon.grid = lon.grid, lat.grid = lat.grid, lon.dat = lon.dat,
      lat.dat = lat.dat, cat = cat, closest.pt = closest.pt
    )
  } else {
    int.data <- dataset
  }
  
  if(!is.null(int.data[[cat]])){
    g <- int.data[[cat]]
  } else if(!is.null(int.data[['ZoneID']])){
    g <- int.data[['ZoneID']]
  }
  if (anyNA(g) == TRUE) {
    warning(paste("No zone identified for", sum(is.na(g)), "observations. 
                  These observations will be removed in future analyses."))
  }

  choice <- data.frame(g)
  startingloc <- if (!"startingloc" %in% int.data) {
    rep(NA, nrow(int.data))
  } else {
    data.frame(int.data$startingloc)
  }

  if (is.null(choice)) {
    warning("Choice must be defined. Ensure that the zone or area assignment variable (cat parameter) is defined.")
    stopanaly <- 1
  }
  
  if (case == "centroid") {
    B <- as.data.frame(unique(g[!is.na(g)])) # unique(unlist(gridInfo['assignmentColumn',,]))
    C <- match(g[!is.na(g)], unique(g[!is.na(g)]))#  match(unlist(gridInfo['assignmentColumn',,]), unique(unlist(gridInfo['assignmentColumn',,])))
  } else {
    a <- colnames(dataset)[grep("zon|area", colnames(dataset), ignore.case = TRUE)] # find(zp)   #find data that is zonal type

    # [B,I,C]=unique([gridInfo.assignmentColumn(~isnan(gridInfo.assignmentColumn)),
    # data(a(v)).dataColumn(~isnan(gridInfo.assignmentColumn),:) ],'rows');%FIXME check that the order of output zones is consistent

   
    temp <- cbind(as.character(g), dataset[[a[1]]]) # cbind(unlist(gridInfo['assignmentColumn',,]), unlist(dataset[[a]]))
    B <- unique(temp) # Correct ->> Needs to be lat/long
    C <- match(paste(temp[, 1], temp[, 2], sep = "*"), paste(B[, 1], B[, 2], sep = "*")) #    C <- data(a(v))[dataColumn,'rows']
  }

  numH <- accumarray(C, C)
  binH <- 1:length(numH)
  numH <- numH / t(binH)
  zoneHist <- data.frame(numH = as.vector(numH), binH = as.vector(binH), B[, 1])

  zoneHist[which(zoneHist[, 1] < min.haul), 3] <- NA

  if (any(is_empty(which(is.na(zoneHist[, 3]) == F)))) {
    warning("No zones meet criteria. No data will be included in further analyses. Check the min.haul parameter or zone identification.")
    stopanaly <- 1
  }

  if (stopanaly == 0) {
    # dataZoneTrue=ismember(gridInfo.assignmentColumn,zoneHist(greaterNZ,3));

    dataZoneTrue <- cbind(g %in% zoneHist[, 3], match(g, zoneHist[, 3], nomatch = 0))
   
    # dataZoneTrue=ismember(gridInfo.assignmentColumn,zoneHist(greaterNZ,3));
    greaterNZ <- which(zoneHist[, 1] >= min.haul) # ifelse(!is.na(zoneHist[, 1]) & zoneHist[, 1] >= 0, 1, 0)
    numOfNecessary <- min.haul

    fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase())

    Alt <- list(
      dataZoneTrue = dataZoneTrue[, 1], # array of logical values to identify which are to be used in model
      greaterNZ = greaterNZ,
      numOfNecessary = numOfNecessary, # input
      choice = choice,
      altChoiceUnits = dist.unit, # miles
      altChoiceType = "distance",
      alt_var = alt_var, # altToLocal1
      occasion = occasion, # altToLocal2
      startingloc = startingloc,
      zoneHist = zoneHist,
      zoneRow = zoneHist[greaterNZ, 3], # zones and choices array
      # assignChoice = gridInfo['dataColumnLink',,],
      # zoneType = ifelse(haul.trip == 'Haul', 'Hauls', 'Trips'),
      int = int # centroid data
    )
  }

  ### Add gridded data ###
  if (!is.null(griddedDat)) {
    gridVar <- griddedDat
    st <- 0
    
    if (DBI::dbExistsTable(fishset_db, griddedDat) == FALSE) {
      DBI::dbWriteTable(fishset_db, griddedDat, gridVar)
    }

    int <- noquote(gsub("[^0-9]", "", colnames(gridVar)))

    if(!is.null(int.data[[cat]])){
      g <- int.data[[cat]]
    } else if(!is.null(int.data[['ZoneID']])) {
      g <- int.data[['ZoneID']]
    }
        
    if (any(noquote(gsub("[^0-9]", "", colnames(gridVar))) %in% g) == FALSE) {
      warning("Cannot use griddedDat. Column names of griddedDat do not match zone ids in gridfile.")
      st <- 1
    }

    # If gridded data is not an array, need to create matrix
    if (st ==0) {
      if(dim(gridVar)[1] == 1) { # (is_empty(gridVar.row.array)){ #1d
        biG <- match(Alt[["zoneRow"]], int) # [aiG,biG] = ismember(Alt.zoneRow, gridVar.col.array) #FIXME FOR STRING CONNECTIONS
        numRows <- nrow(dataset) # size(data(1).dataColumn,1)  #
        if (!any(biG)) {
          warning("The map associated to the data and the grid information in the gridded variable do not overlap.")
          st <- 1
        }
        allMat <- matrix(1, numRows, 1) %x% as.matrix(gridVar[1, biG]) # repmat(gridVar.matrix(1,biG), numRows, 1)
      } else {
        # [aiG,biG] = ismember(Alt.zoneRow, gridVar.col.array)#FIXME FOR STRING CONNECTIONS
        biG <- match(Alt[["zoneRow"]], int[-1]) # gridVar.col.array
        if (!any(biG)) {
          warning("The map associated to the data and the grid information in the gridded variable do not overlap.")
          st <- 1
      }

      if (names(gridVar)[1] %in% colnames(dataset) == FALSE) {
        # wrong occourance variable to connect data
        warning("The data in the workspace and the loaded grid file do not have a matching variable for connecting.")
        st <- 1
      }

      biD <- match(dataset[, names(gridVar)[1]], gridVar[, 1]) # [aiD,biD]=ismember(data(occasVar).dataColumn,gridVar.row.array)

      if (!any(biD)) {
        print("The data in the workspace and the loaded grid file do not have a matching variable for connecting.")
      }

      allMat <- gridVar[, -1][biD, biG]
      }
    }
    if (anyNA(allMat[Alt[["dataZoneTrue"]], ])) {
      warning("Problem with loaded matrix, NA found.")
      st <- 1
    }
    if(st == 0){
    Alt <- c(Alt, matrix = list(allMat[Alt[["dataZoneTrue"]], ])) # allMat[Alt[[dataZoneTrue]],]
    } else {
      Alt = Alt
    }
  }


  # write Alt to datafile
  if (stopanaly == 0) {
    single_sql <- paste0(project, "altmatrix")
    date_sql <- paste0(project, "altmatrix", format(Sys.Date(), format = "%Y%m%d"))
    if (table_exists(single_sql)) {
      table_remove(single_sql)
    }
    if (table_exists(date_sql)) {
      table_remove(date_sql)
    }
    DBI::dbExecute(fishset_db, paste("CREATE TABLE IF NOT EXISTS", single_sql, "(AlternativeMatrix ALT)"))
    DBI::dbExecute(fishset_db, paste("INSERT INTO", single_sql, "VALUES (:AlternativeMatrix)"),
      params = list(AlternativeMatrix = list(serialize(Alt, NULL)))
    )
    DBI::dbExecute(fishset_db, paste("CREATE TABLE IF NOT EXISTS", date_sql, "(AlternativeMatrix ALT)"))
    DBI::dbExecute(fishset_db, paste("INSERT INTO", date_sql, "VALUES (:AlternativeMatrix)"),
      params = list(AlternativeMatrix = list(serialize(Alt, NULL)))
    )
    # DBI::dbExecute (fishset_db, "CREATE TABLE IF NOT EXISTS altmatrix (AlternativeMatrix ALT)")
    # DBI::dbExecute (fishset_db, "INSERT INTO altmatrix VALUES (:AlternativeMatrix)", params = list(AlternativeMatrix = list(serialize(Alt, NULL))))
    DBI::dbDisconnect(fishset_db)
 
    create_alternative_choice_function <- list()
    create_alternative_choice_function$functionID <- "create_alternative_choice"
    create_alternative_choice_function$args <- list(
      dat, project, deparse(substitute(gridfile)), min.haul,
      alt_var, occasion, dist.unit, lon.dat, lat.dat, cat,
      hull.polygon, closest.pt
    )
    create_alternative_choice_function$kwargs <- list("lon.grid" = lon.grid, "lat.grid" = lat.grid, "griddedDat" = griddedDat, "weight.var" = weight.var)
    create_alternative_choice_function$output <- list()

    log_call(create_alternative_choice_function)
  }
}
