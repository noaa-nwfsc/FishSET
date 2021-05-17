#' Create the distance matrix
#'
#' @param dataset Primary dataset
#' @param alt_var Alternative choice location
#' @param occasion Define choice location
#' @param dataZoneTrue Include zone
#' @param int  Zone centroid data frame
#' @param choice Choice zone
#' @param units Distance units
#' @param port Port table
#' @param zoneRow Zone row
#' @param X distace matrix
#' @importFrom geosphere distm
#' @export 
#' @keywords internal
#' @details Function is called by \code{\link{make_model_design}} generate the distance matrix. 
#'   Alternative fishing options come from the Alternative Choice list, generated from the 
#'   \code{\link{create_alternative_choice}} function,
#' @return
#' Distance matrix based on choices made in create_alternative_choice

create_dist_matrix <- function(dataset, alt_var, occasion, dataZoneTrue, int, choice, units, port, zoneRow, X) {
  
    end <- FALSE
    ################### 
    #Steps if alternative matrix come from gridded data file 
    #The distance matrix is the gridded data file
    if (!is.null(X)) {
      altChoiceUnits <- units
      
      #Identify if using centroid or other for altToLocal2
      allZP <- dataset[, grep("AREA|zone", colnames(dataset), ignore.case =T)[1]] # get zonal type variables
      if (all(is.null(allZP)) || alt_var > length(allZP)) {
        v2 <- 0 # zonal centroid
      } else {
        v2 <- allZP(alt_var) #
      }
      if (v2 == 0) {
        altToLocal1 <- ""
        altToLocal2 <- "Centroid of Zonal Assignment"
      } else {
        altToLocal1 <- ""
        altToLocal2 <- alt_var
      }
      altChoiceType <- "loaded grid data matrix"
      B <- zoneRow
      choiceZ <- ""
      # End Grid Matrix
    } else {
      
      # steps if alternative matrix comes from loaded data (expectations)
      ##### ---Begin Alt Var--###
      if (any(grepl("centroid|zon", alt_var, ignore.case = T))) {

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
              end <- TRUE
            }
            
            toXYa <- data.frame(dataset[[alt_var]][which(dataZoneTrue == 1)]) #  data[[altToLocal1]]data(v1).dataColumn(dataZoneTrue,:)      #subset data to when dataZoneTrue==1
            
            colnames(toXYa) <- c(alt_var)
            toXYa[[alt_var]] <- trimws(toXYa[[alt_var]])
            colnames(port)[1] <- alt_var
            port[[alt_var]] <- trimws(port[[alt_var]])
            
            
            
            if (any(unique(toXYa[[alt_var]]) %in% unique(port[[alt_var]]) == FALSE)) {
              #     if (any(is_empty(lonlat))) {
              warning("At least one port not included in PortTable.") #Specify starting lat/lon in lonlat variable to use mean lat/lon.")
              end <- TRUE
            }
            
            toXY1 <- merge(toXYa, port) # portLL(toXYa,:)
            
            
            # toXY1 <- unique(toXY1)
            # Data from list
          } else {
            warning("Primary data must be a data frame.")
            end <- TRUE
           # toXYa <- data.frame(dataset[["data"]][, , which(unlist(dataset[["data"]][, 1, ][3, ]) == alt_var)]$dataColumn[which(dataZoneTrue == 1)]) # data.frame(dataset[[alt_var]][which(dataZoneTrue==1)])#  data[[altToLocal1]]data(v1).dataColumn(dataZoneTrue,:)
            #colnames(toXYa) <- c(alt_var)
            # portLL <- data[[alt_var]].codeID[,2] # Extract lat long for selected port variable,
            # cell2mat(data(v1).codeID(:,2)) # convert cell array to an ordinary array
           # temp <- data.frame(
            #  unique(data.frame(dataset[["data"]][, , which(unlist(dataset[["data"]][, 1, ][3, ]) == alt_var)]$dataColumn)),
             # tapply(
              #  data.frame(dataset[["data"]][, , which(unlist(dataset[["data"]][, 1, ][3, ]) == alt_var)]$dataColumn)[, 1],
               # data.frame(dataset[["data"]][, , which(unlist(dataset[["data"]][, 1, ][3, ]) == alt_var)]$dataColumn), mean
          #    ),
          #    tapply(
          #      data.frame(dataset[["data"]][, , which(unlist(dataset[["data"]][, 1, ][3, ]) == alt_var)]$dataColumn)[, 2],
          #      data.frame(dataset[["data"]][, , which(unlist(dataset[["data"]][, 1, ][3, ]) == alt_var)]$dataColumn), mean
          #    )
          #  )
          #  colnames(temp) <- c(alt_var, "LON", "LAT")
          #  toXY1 <- merge(toXYa, temp)
          }
          # Lat/Lon
        } else {
          # Data is from a dataframe or matrix
          if (is.data.frame(dataset)) {
            if (length(alt_var) < 2) {
              warning("Please define both lat and long in alt_var parameter of create_alternative_choice function.")
              end <- TRUE
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
            warning("Please define lon and lat in occasion argument of create_alternative_choice function.")
            end <- TRUE
          }
          
          if (any(is_empty(dataset[[occasion[1]]]))) {
            warning("Occasion does not exist in dataset")
            end <- TRUE
          }
          if (any(is_empty(dataset[[occasion[2]]]))) {
            warning("Occasion does not exist in dataset")
            end <- TRUE
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
      end <- TRUE
    }
    if (any(do.call(cbind, lapply(centersZone, is.nan)))) {
      warning(paste("NaN found in ", altToLocal2, ". Design file aborted."))
      end <- TRUE
    }
    
    if (end == FALSE) {
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
      
      if (units %in% c("meters", "M", "m")) {
        X <- distMatrix
        altChoiceUnits <- "meters"
      } else if (units %in% c("kilometers", "KM", "km")) {
        X <- distMatrix / 1000
        altChoiceUnits <- "kilometers"
      } else if (units == "miles") {
        X <- distMatrix * 0.000621371192237334 # meter* miles/meter
        altChoiceUnits <- "miles"
      } else {
        X <- distMatrix
        altChoiceUnits <- units
      }
    }
  
  return(list(X=X, altChoiceUnits=altChoiceUnits, altChoiceType=altChoiceType, altToLocal1=altToLocal1,
              altToLocal2=altToLocal2))
}
