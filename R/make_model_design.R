#' Make model design file
#'
#' Create a list containing likelihood function, parameters, and data to be pass to model call function
#'
#' @param dat Primary data containing information on hauls or trips. Table in FishSET database contains the string 'MainDataTable'.
#' @param project String, name of project.
#' @param catchID  String, variable from \code{dat} that contains catch data.
#' @param replace Logical, should the model design file be replaced? If false, appends to existing model design file. 
#'   Defaults to TRUE.
#' @param PortTable Optional. String, name of data table in FishSET database containing the port table with lat/lon for each port. 
#'   Define if \code{alt_var} is a port.
#' @param likelihood String, name of likelihood function. Details on likelihood specific initial parameter specification 
#'   can be found in \code{\link{discretefish_subroutine}} documentation.
#' \tabular{rlll}{
#'  logit_c: \tab  Conditional logit likelihood  \cr
#'  logit_avgcat: \tab Average catch multinomial logit procedure \cr
#'  logit_correction: \tab Full information model with Dahl's correction function  \cr
#'  epm_normal:  \tab  Expected profit model with normal catch function \cr
#'  epm_weibull: \tab Expected profit model with Weibull catch function \cr
#'  epm_lognormal: \tab  Expected profit model with lognormal catch function  \cr
#'  }

#' @param vars1  Character string, additional ‘travel-distance’ variables to include in the model. 
#'   These depend on the likelihood. See the Details section for how to specify for each likelihood function.
#' @param vars2 Character string, additional variables to include in the model. These depend on the likelihood.
#'   See the Details section for how to specify for each likelihood function.
#' @param priceCol Variable in \code{dat} containing price information. Required if specifying an expected profit model 
#'   for the likelihood (epm_normal, epm_weibull, epm_lognormal).
#' @param startloc Variable in \code{dat} identifying the location when choice of where to fish next was made. Required for logit_correction likelihood.
#'   Use the \code{\link{create_startingloc}} function to create the starting location vector.
#' @param polyn Numeric, correction polynomial degree.  Required for logit_correction likelihood.
#' @importFrom geosphere distm
#' @importFrom DBI dbGetQuery dbExecute dbListTables
#' @export make_model_design
#' @details Function creates the model matrix list that contains the data and modeling choices.
#'   The model design list is saved to the FishSET database and called by the
#'   \code{\link{discretefish_subroutine}}. Alternative fishing options come from the
#'   Alternative Choice list, generated from the \code{\link{create_alternative_choice}} function,
#'   and the expected catch matrices from the \code{\link{create_expectations}} function.
#'   The distance from the starting point to alternative choices is calculated. \cr\cr
#'   Variable names details: \cr
#' \tabular{lllllll}{
#' \tab \strong{vars1} \tab \strong{vars2} \tab \cr \cr
#' \strong{logit_c}: \tab 
#'     \preformatted{"travel-distance variables" are
#'     alternative-invariant variables that are
#'     interacted with travel distance to form the cost
#'     portion of the likelihood. Each variable name
#'     therefore corresponds to data with dimensions
#'     (number of observations) by (unity), and returns
#'     a single parameter.} \tab 
#'     \preformatted{"alternative-specific variables"
#'     vary across alternatives, e.g. catch rates.
#'     Each variable name therefore corresponds to data
#'     with dimensions (number of observations) by
#'     (number of alternatives), and returns a single
#'     parameter for each variable (e.g. the marginal
#'     utility from catch).} \cr \cr
#' \strong{logit_avgcat}: \tab 
#'     \preformatted{"travel-distance variables" are
#'     alternative-invariant variables that are
#'     interacted with travel distance to form the cost
#'     portion of the likelihood. Each variable name
#'     therefore corresponds to data with dimensions
#'     (number of observations) by (unity), and returns
#'     a single parameter.} \tab 
#'     \preformatted{"average-catch variables" are
#'     alternative-invariant variables, e.g. vessel
#'     gross tonnage. Each variable name therefore
#'     corresponds to data with dimensions (number of
#'     observations) by (unity), and returns (k-1)
#'     parameters where (k) equals the number of
#'     alternatives, as a normalization of parameters
#'     is needed as the probabilities sum to one.
#'     Interpretation is therefore relative to the
#'     first alternative.} \cr \cr
#' \strong{epm_normal}: \tab 
#'     \preformatted{"travel-distance variables" are
#'     alternative-invariant variables that are
#'     interacted with travel distance to form the
#'     cost portion of the likelihood. Each variable
#'     name therefore corresponds to
#'     data with dimensions (number of observations)
#'     by (unity), and returns a single parameter.} \tab 
#'     \preformatted{"catch-function variables" are
#'     alternative-invariant variables that are
#'     interacted with zonal constants to form the
#'     catch portion of the likelihood. Each variable
#'     name therefore corresponds to data with
#'     dimensions (number of observations) by (unity),
#'     and returns (k) parameters where (k) equals
#'     the number of alternatives.} \cr \cr
#' \strong{epm_lognormal}: \tab 
#'     \preformatted{"travel-distance variables" are
#'     alternative-invariant variables that are
#'     interacted with travel distance to form the
#'     cost portion of the likelihood. Each variable
#'     name therefore corresponds to data with
#'     dimensions (number of observations) by (unity),
#'     and returns a single parameter.} \tab 
#'     \preformatted{"catch-function variables" are
#'     alternative-invariant variables that are
#'     interacted with zonal constants to form the
#'     catch portion of the likelihood. Each variable
#'     name therefore corresponds to data with
#'     dimensions (number of observations) by (unity),
#'     and returns (k) parameters where (k) equals
#'     the number of alternatives.} \cr \cr
#' \strong{epm_weibull}: \tab 
#'     \preformatted{"travel-distance variables" are
#'     alternative-invariant variables that are
#'     interacted with travel distance to form the cost
#'     portion of the likelihood. Each variable name
#'     therefore corresponds to data with dimensions
#'     (number of observations) by (unity), and returns
#'     a single parameter.} \tab 
#'     \preformatted{"catch-function variables" are
#'     alternative-invariant variables that are
#'     interacted with zonal constants to form the catch
#'     portion of the likelihood. Each variable name
#'     therefore corresponds to data with dimensions
#'     (number of observations) by (unity), and returns
#'     (k) parameters where (k) equals the number of
#'     alternatives.} \cr \cr
#' \strong{logit_correction}: \tab 
#'     \preformatted{"travel-distance variables" are
#'     alternative-invariant variables that are
#'     interacted with travel distance to form the cost
#'     portion of the likelihood. Each variable name
#'     therefore corresponds to data with dimensions
#'     (number of observations) by (unity), and returns
#'     a single parameter.} \tab
#'     \preformatted{"catch-function variables" are
#'     alternative-invariant variables that are
#'     interacted with zonal constants to form the catch
#'     portion of the likelihood. Each variable name
#'     therefore corresponds to data with dimensions
#'     (number of observations) by (unity), and returns
#'     (k) parameters where (k) equals the number of
#'     alternatives.} \cr \cr
#' }
#' @return
#' Function creates the model matrix list that contains the data and modeling choices. The model design list is saved to the FishSET database and called by the \code{\link{discretefish_subroutine}}.
#' Alternative fishing options come from the Alternative Choice list, generated from the \code{\link{create_alternative_choice}} function, and the expected catch matrices from the \code{\link{create_expectations}}
#' function. The distance from the starting point to alternative choices is calculated. \cr\cr
#'   Model design list: \cr
#'   \tabular{rlll}{
#'     likelihood: \tab Name of likelihood function\cr
#'     choice: \tab Data corresponding to actual zonal choice\cr
#'     catch: \tab Data corresponding to actual zonal catch\cr
#'     scales: \tab Scale vectors to put catch data, zonal data, and other data on same scale\cr
#'     distance: \tab Data corresponding to distance\cr
#'     instances: \tab Number of observations\cr
#'     alt: \tab Number of alternative zones\cr
#'     epmDefaultPrice: \tab Price data\cr
#'     dataZoneTrue: \tab Vector of 0/1 indicating whether the data from that zone is to be included based on the minimum number of hauls.\cr
#'     numOfNecessary: \tab Minimum number of hauls/trips per zone for data from that zone to be included\cr
#'     typeOfNecessary: \tab Whether data is at haul or trip level\cr
#'     altChoiceType: \tab Function choice. Set to distance\cr
#'     altChoiceUnits: \tab Units of distance\cr
#'     altToLocal: \tab Identifies how to find lat/lon for starting point. Can be zonal centroid, port, etc\cr
#'     altToLocal2: \tab Identifies how to find lat/lon for alternative choices such as 'Centroid of Zonal Assignment'\cr
#'     bCHeader: \tab Variables to include in the model that do not vary by zone. Includes independent variables and interactions\cr
#'     gridVaryingVariables: \tab Variables to include in the model that do vary by zone such as expected catch (from \code{\link{create_expectations}} function)
#'   }
#' @examples
#' \dontrun{
#' make_model_design(pollockMainDataTable ,"pollock", catchID= "OFFICIAL_TOTAL_CATCH", 
#' likelihood='logit_c')
#' }
#'
make_model_design <- function(dat, project, catchID, replace = TRUE, PortTable = NULL, likelihood = NULL, 
                              vars1 = NULL, vars2 = NULL, priceCol = NULL, startloc = NULL, polyn = NULL) {
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase())
  # Call in datasets
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset

  x0 <- 0

  # Script necessary to ensure paramers generated in shiny app are in correct format
  if (is_empty(vars1) || vars1 == "none") {
    indeVarsForModel <- NULL
  } else {
    indeVarsForModel <- vars1
  }
  if (is_empty(vars2) || vars2 == "none") {
    gridVariablesInclude <- NULL
  } else {
    gridVariablesInclude <- vars2
  }
  if (is_empty(priceCol) || priceCol == "none") {
    priceCol <- NULL
  } else {
    priceCol <- priceCol
  }
  if (is_empty(startloc) || startloc == "none") {
    startloc <- NULL
  } else {
    startloc <- startloc
  }
  # lon.dat <- as.character(lon.dat)
  # lat.dat <- as.character(lat.dat)

#  if (any(!is_empty(lonlat))) {
#    if (lonlat[1] == lonlat[2]) {
#      warning("Longitude and Latitude variables are identical.")
#      x0 <- 1
#    }
#  }
  # indeVarsForModel = vars1
  # gridVariablesInclude=vars2

  if (!exists("Alt")) {
    if (!exists("AltMatrixName")) {
      Alt <- unserialize(DBI::dbGetQuery(fishset_db, paste0("SELECT AlternativeMatrix FROM ", project, "altmatrix LIMIT 1"))$AlternativeMatrix[[1]])
      if (!exists("Alt")) {
        warning("Alternative Choice Matrix does not exist. Please run the createAlternativeChoice() function.")
        x0 <- 1
      }
    }
  }

  if (table_exists(paste0(project, "ExpectedCatch"))) {
    ExpectedCatch <- unserialize(DBI::dbGetQuery(fishset_db, paste0("SELECT data FROM ", project, "ExpectedCatch LIMIT 1"))$data[[1]])
  }
  if (!exists("ExpectedCatch")) {
    ExpectedCatch <- ""
    warning("Expected Catch Matrix does not exist. Please run the create_expectations function if expected catch will be included in the model.")
    x0 <- 1
  }

  alt_var <- Alt[["alt_var"]]
  occasion <- Alt[["occasion"]]
  dataZoneTrue <- Alt[["dataZoneTrue"]]
  int <- Alt[["int"]]
  choice <- Alt[["choice"]]
  startingloc <- if (!is.null(startloc) & all(is.na(Alt$startingloc))) {
    dataset[[startloc]]
  } else {
    Alt[["startingloc"]]
  }
  units <- Alt[["altChoiceUnits"]]

  if (any(grepl("Port", alt_var, ignore.case = TRUE) == T)) {
    pt <- data_pull(PortTable)
    ptname <- pt$dat
    port <- pt$dataset
  } else {
    ptname <- NULL
    port <- NULL
  }

  if (is_empty(gridVariablesInclude)) {
    gridVariablesInclude <- as.data.frame(matrix(1, nrow = nrow(choice), ncol = 1)) # max(as.numeric(as.factor(unlist(choice))))))
  } else {
    gridVariablesInclude
  }

  if (is_empty(ExpectedCatch$newDumV)) {
    newDumV <- 1
  } else {
    newDumV <- ExpectedCatch[["newDumV"]]
    # bCHeader <- list(bCHeader, newDumV)
  }
  #
  if (is_empty(indeVarsForModel)) {
    bCHeader <- list(units = units, gridVariablesInclude = gridVariablesInclude, newDumV = newDumV, indeVarsForModel = as.data.frame(rep(1, nrow(choice))))
    bColumnsWant <- ""
    bInterAct <- ""
  } else {
    if (any(indeVarsForModel %in% c("Miles * Miles", "Miles*Miles, Miles x Miles"),
      ignore.case = TRUE
    )) {
      bCHeader <- list(units = units, gridVariablesInclude = gridVariablesInclude, newDumV = newDumV, lapply(indeVarsForModel[-1], function(x) dataset[[x]][which(dataZoneTrue == 1)]))
    } else {
      bCHeader <- list(units = units, gridVariablesInclude = gridVariablesInclude, newDumV = newDumV, lapply(indeVarsForModel, function(x) dataset[[x]][which(dataZoneTrue == 1)]))
    }
  }


  ################### 
  #Steps if alternative matrix come from gridded data file 
   #The distance matrix is the gridded data file
  if (exists(Alt[["matrix"]])) {
    X <- Alt[["matrix"]]

    altChoiceUnits <- Alt[["altChoiceUnits"]]
    
  #Identify if using centroid or other for altToLocal2
    allZP <- dataset[, grep("AREA|zone", colnames(dataset), ignore.case =T)[1]] # get zonal type variables
    if (all(is.null(allZP)) || Alt[["alt_var"]] > length(allZP)) {
      v2 <- 0 # zonal centroid
    } else {
      v2 <- allZP(Alt[["alt_var"]]) #
    }
    if (v2 == 0) {
      altToLocal1 <- ""
      altToLocal2 <- "Centroid of Zonal Assignment"
    } else {
      altToLocal1 <- ""
      altToLocal2 <- alt_var
    }
    altChoiceType <- "loaded grid data matrix"
    B <- Alt[["zoneRow"]]
    choiceZ <- ""
# End Grid Matrix
  } else {

    # steps if alternative matrix comes from loaded data (expectations)
    ##### ---Begin Alt Var--###
    if (any(grepl("zon", alt_var, ignore.case = T))) {
      # (alt_var==c('Zonal centroid')){ #(v1==0){ #Zonal centroid toXY1 <-
      # assignmentColumn()
      # #M.CentroidArcView(grindInfo['assignmentColumn',,][which(dataZoneTrue==1)])
      # #gridInfo.assignmentColumn(dataZoneTrue),:) # toXY1 <-
      # toXY1[,c('cent.long','cen.lat','ID')][which(is.data.frame(dataZoneTrue)==1)]
      temp <- as.matrix(dataset[["ZoneID"]])
      colnames(temp) <- "ZoneID"
      temp <- merge(temp, int)
      toXY1 <- temp[which(dataZoneTrue == 1), 2:3]

      altToLocal1 <- "Centroid of Zonal Assignment"
    } else {
      # Port (isfield(data,'isPort') && data(v1).isPort){ Data from dataframe
      if (any(grepl("Port", alt_var, ignore.case = TRUE) == T)) {
        if (is.data.frame(dataset)) {
          if (any(is_empty(dataset[[alt_var]]))) {
            warning("alt_var does not exist in dataset")
            x0 <- 1
          }

          toXYa <- data.frame(dataset[[alt_var]][which(dataZoneTrue == 1)]) #  data[[altToLocal1]]data(v1).dataColumn(dataZoneTrue,:)      #subset data to when dataZoneTrue==1

          colnames(toXYa) <- c(alt_var)
          toXYa[[alt_var]] <- trimws(toXYa[[alt_var]])
          colnames(port)[1] <- alt_var
          port[[alt_var]] <- trimws(port[[alt_var]])



          if (any(unique(toXYa[[alt_var]]) %in% unique(port[[alt_var]]) == FALSE)) {
       #     if (any(is_empty(lonlat))) {
              warning("At least one port not included in PortTable.") #Specify starting lat/lon in lonlat variable to use mean lat/lon.")
              x0 <- 1
      #      } else {
      #        warning("At least one port not included in PortTable. 
      #                Using vessel lon/lat at", alt_var, "to calculate mean lon/lat of undefined ports.")
      #        unport <- unique(toXYa[[alt_var]])[which(unique(toXYa[[alt_var]]) %in% unique(port[[alt_var]]) == FALSE)]
      #        dataset[[alt_var]] <- trimws(dataset[[alt_var]])
      #        temp <- dataset[dataset[[alt_var]] %in% unport, ]
#
      #        temp <- data.frame(
      #          unique(temp[[alt_var]]), tapply(temp[[lonlat[1]]], temp[[alt_var]], mean),
      #          tapply(temp[[lonlat[2]]], temp[[alt_var]], mean)
     #         )
      #        colnames(temp) <- colnames(port)
     #         port <- rbind(port, temp)
      #      }
          }

          toXY1 <- merge(toXYa, port) # portLL(toXYa,:)


          # toXY1 <- unique(toXY1)
          # Data from list
        } else {
          toXYa <- data.frame(dataset[["data"]][, , which(unlist(dataset[["data"]][, 1, ][3, ]) == alt_var)]$dataColumn[which(dataZoneTrue == 1)]) # data.frame(dataset[[alt_var]][which(dataZoneTrue==1)])#  data[[altToLocal1]]data(v1).dataColumn(dataZoneTrue,:)
          colnames(toXYa) <- c(alt_var)
          # portLL <- data[[alt_var]].codeID[,2] # Extract lat long for selected port variable,
          # cell2mat(data(v1).codeID(:,2)) # convert cell array to an ordinary array
          temp <- data.frame(
            unique(data.frame(dataset[["data"]][, , which(unlist(dataset[["data"]][, 1, ][3, ]) == alt_var)]$dataColumn)),
            tapply(
              data.frame(dataset[["data"]][, , which(unlist(dataset[["data"]][, 1, ][3, ]) == alt_var)]$dataColumn)[, 1],
              data.frame(dataset[["data"]][, , which(unlist(dataset[["data"]][, 1, ][3, ]) == alt_var)]$dataColumn), mean
            ),
            tapply(
              data.frame(dataset[["data"]][, , which(unlist(dataset[["data"]][, 1, ][3, ]) == alt_var)]$dataColumn)[, 2],
              data.frame(dataset[["data"]][, , which(unlist(dataset[["data"]][, 1, ][3, ]) == alt_var)]$dataColumn), mean
            )
          )
          colnames(temp) <- c(alt_var, "LON", "LAT")
          toXY1 <- merge(toXYa, temp)
        }
        # Lat/Lon
      } else {
        # Data is from a dataframe or matrix
        if (is.data.frame(dataset)) {
          if (length(alt_var) < 2) {
            warning("Please define both lat and long in alt_var parameter of create_alternative_choice function.")
            x0 <- 1
          }
            toXY1 <- data.frame(
              dataset[[alt_var[1]]][which(dataZoneTrue == 1)],
              dataset[[alt_var[2]]][which(dataZoneTrue == 1)]
            )
          } else {
           toXY1 <- dataset[[alt_var]][which(dataZoneTrue == 1)]
           
          # Data from a list
        } #else {
          #toXY1 <- data.frame(dataset[["data"]][, , which(unlist(dataset[["data"]][, 1, ][3, ]) == alt_var)]$dataColumn)[which(dataZoneTrue == 1), ] # data.frame(dataset[[alt_var]][which(dataZoneTrue==1)])#  data[[altToLocal1]]data(v1).dataColumn(dataZoneTrue,:)      #subset data to when dataZoneTrue==1
       # }
      }
      altToLocal1 <- alt_var
    }
    ### --End Alt Var---###
    #### ---Begin Occasion Var--##
    if (any(grepl("zon|cent", occasion, ignore.case = T))) {
      # (v2==0){ #Zonal centroid [B,I,choiceZ] <-
      # unique(gridInfo.assignmentColumn(dataZoneTrue))#
      B <- int[int$ZoneID %in% unique(choice[which(dataZoneTrue == 1), ]), 1]
      choiceZ <- match(int$ZoneID[which(dataZoneTrue == 1)], unique(int$ZoneID[which(dataZoneTrue == 1)]))

      centersZone <- int[int$ZoneID %in% unique(choice[which(dataZoneTrue == 1), ]), 2:3] # M.CentroidArcView[B,] #Lat and Long
      altToLocal2 <- "Centroid of Zonal Assignment"
    } else {
      if (is.data.frame(dataset)) {
        if (length(occasion) < 2) {
          warning("Please define both lat and long in parameter variable of create_alternative_choice function.")
          x0 <- 1
        }

        if (any(is_empty(dataset[[occasion[1]]]))) {
          warning("Occasion does not exist in dataset")
          x0 <- 1
        }
        if (any(is_empty(dataset[[occasion[2]]]))) {
          warning("Occasion does not exist in dataset")
          x0 <- 1
        }
        toXY2 <- data.frame(
          dataset[[occasion[1]]][which(dataZoneTrue == 1)],
          dataset[[occasion[2]]][which(dataZoneTrue == 1)]
        )
      } else {
        toXY2 <- dataset[[occasion]][which(dataZoneTrue == 1)] # MUST be a LAT/LONG data(v2).dataColumn(dataZoneTrue,:)
      }
      # [Bb,I,choiceZ] <-
      # unique(cbind(gridInfo['assignmentColumn',,][which(dataZoneTrue==1)],toXY),'rows')
      # #unique([gridInfo.assignmentColumn(dataZoneTrue),toXY],'rows')#
      Bb <- unique(cbind(data.frame(choice[which(dataZoneTrue == 1), ]), data.frame(toXY2)))
      B <- Bb[, 1] # Assignment column
      centersZone <- Bb[, 2:3] # Latitude aned Longitude
      altToLocal2 <- occasion
    } ## -End Occasion Var--##
  } # End From loaded data

  ## ------ Generate Distance Matrix ----##
  # Test for potential issues with data
  if (any(do.call(cbind, lapply(toXY1, is.nan)))) {
    warning(paste("NaN found in ", altToLocal1, ". Design file aborted."))
    x0 <- 1
  }
  if (any(do.call(cbind, lapply(centersZone, is.nan)))) {
    warning(paste("NaN found in ", altToLocal2, ". Design file aborted."))
    x0 <- 1
  }

  if (x0 == 0) {
    # Generate distances using distm function [distAll,!,!] <-
    # #m_idist(toXY1[q,1],toXY1[q,2], centersZone[,1], centersZone[,2])
    if (dim(toXY1)[2] > 2) {
      toXY1[, 2] <- as.numeric(toXY1[, 2])
      toXY1[, 3] <- as.numeric(toXY1[, 3])
      distMatrix <- geosphere::distm(toXY1[, 2:3], centersZone[, 1:2])
    } else {
      toXY1[, 1] <- as.numeric(toXY1[, 1])
      toXY1[, 2] <- as.numeric(toXY1[, 2])
      distMatrix <- geosphere::distm(toXY1[, 1:2], centersZone[, 1:2])
    }

    altChoiceType <- "distance"

    if (Alt[["altChoiceUnits"]] %in% c("meters", "M", "m")) {
      X <- distMatrix
      altChoiceUnits <- "meters"
    } else if (Alt[["altChoiceUnits"]] %in% c("kilometers", "KM", "km")) {
      X <- distMatrix / 1000
      altChoiceUnits <- "kilometers"
    } else if (Alt[["altChoiceUnits"]] == "miles") {
      X <- distMatrix * 0.000621371192237334 # meter* miles/meter
      altChoiceUnits <- "miles"
    }




    ### ---- add special terms: ----### add only for EPM model
    catch <- dataset[which(dataZoneTrue == 1), as.vector(catchID)]
    r <- nchar(sub("\\.[0-9]+", "", max(catch, na.rm = T)))
    yscale <- 10^(r - 1)


    # Some models need price data
    if (is_empty(priceCol) || is.null(priceCol) || priceCol == "") {
      epmDefaultPrice <- ""
    } else {
      epmDefaultPrice <- dataset[which(dataZoneTrue == 1), as.character(priceCol)]
    }

    # scales zonal
    r <- nchar(sub("\\.[0-9]+", "", max(max(X, na.rm = T), na.rm = T)))
    mscale <- 10^(r - 1)

    # scales data r in
    # regexp(arrayfun(@num2str,nanmax(dataPerZone),'UniformOutput',false),'\\.','split')){){
    # dscale <- cellfun(@(x) 10^length(x{1}-1),r)
    dscale <- 1

    ### -- Create output list --- ###
    modelInputData_tosave <- list(
      likelihood = likelihood,
      catch = catch,
      choice = choice[which(dataZoneTrue == 1), ],
      startingloc = startingloc[which(dataZoneTrue == 1)],
      scales = c(catch = yscale, zonal = mscale, data = dscale),
      distance = X,
      instances = dim(X)[1],
      alts = dim(X)[2],
      epmDefaultPrice = epmDefaultPrice,
      dataZoneTrue <- dataZoneTrue,
      typeOfNecessary = Alt[["zoneType"]],
      altChoiceType = altChoiceType,
      altChoiceUnits = altChoiceUnits,
      altToLocal1 = altToLocal1,
      altToLocal2 = altToLocal2,
      bCHeader = bCHeader,
      startloc = startloc,
      polyn = polyn,
      gridVaryingVariables = ExpectedCatch
    )

    fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase())
    single_sql <- paste0(project, "modelinputdata")
    date_sql <- paste0(project, "modelinputdata", format(Sys.Date(), format = "%Y%m%d"))
    if (table_exists(single_sql) & replace == FALSE) {
      # modelInputData <- table_view()
      modelInputData <- unserialize(DBI::dbGetQuery(fishset_db, paste0("SELECT ModelInputData FROM ", project, "modelinputdata LIMIT 1"))$ModelInputData[[1]])
      modelInputData[[length(modelInputData) + 1]] <- modelInputData_tosave
    } else {
      modelInputData <- list()
      modelInputData[[length(modelInputData) + 1]] <- modelInputData_tosave
    }

    single_sql <- paste0(project, "modelinputdata")
    if (table_exists(single_sql)) {
      table_remove(single_sql)
    }
    if (table_exists(date_sql)) {
      table_remove(date_sql)
    }

    DBI::dbExecute(fishset_db, paste("CREATE TABLE IF NOT EXISTS", single_sql, "(ModelInputData MODELINPUTDATA)"))
    DBI::dbExecute(fishset_db, paste("INSERT INTO", single_sql, "VALUES (:ModelInputData)"),
      params = list(ModelInputData = list(serialize(modelInputData, NULL)))
    )
    DBI::dbExecute(fishset_db, paste("CREATE TABLE IF NOT EXISTS", date_sql, "(ModelInputData MODELINPUTDATA)"))
    DBI::dbExecute(fishset_db, paste("INSERT INTO", date_sql, "VALUES (:ModelInputData)"),
      params = list(ModelInputData = list(serialize(modelInputData, NULL)))
    )
    DBI::dbDisconnect(fishset_db)


    make_model_design_function <- list()
    make_model_design_function$functionID <- "make_model_design"
    make_model_design_function$args <- list(
      dat, project, catchID, replace, ptname, likelihood,
      vars1, vars2, priceCol, startloc, polyn
    )
    make_model_design_function$kwargs <- list()

    log_call(make_model_design_function)
  }
}
