#' Trip length
#' 
#' Create a plot or table of vessel trip length
#' 
#' @param dat Main data frame over which to apply function. Table in fishset_db 
#'   database should contain the string `MainDataTable`.
#' @param project name of project.
#' @param start Date variable containing the start of vessel trip.
#' @param end Date variable containing the end of vessel trip.
#' @param units Defaults to "days". Options include "secs", "mins", "hours", 
#'   "days", or "weeks".
#' @param catch Species catch variable for calculating catch per trip. 
#' @param hauls Hauls variable for calculating hauls per trip.  
#' @param output Output results at table or plot
#' @param haul_to_trip Logical, whether to convert \code{dat} from haul level data to 
#'   trip level. See \code{\link{haul_to_trip}} for details.
#' @param ... Additional arguments passed to the haul_to_trip function. These should
#'   be the column(s) that uniquely identify trips. 
#' @return The function calculates vessel trip length given a start and end date, 
#'   and then converts trip length to the desired unit of time (e.g. weeks, days, 
#'   or hours), returning a table or histogram. There is also the option of 
#'   including catch and hauls per trip. 
#' @examples 
#' \dontrun{
#' #' trip_length("pollockMainDataTable", start = "FISHING_START_DATE", end = "HAUL_DATE",
#' units = "days", catch = "OFFICIAL_TOTAL_CATCH", hauls = "HAUL", output = "plot", 
#' haul_to_trip = T, fun.numeric = sum, fun.time = min, "VESSEL", "FISHING_START_DATE")
#' #' }
#' @importFrom lubridate is.Date is.POSIXt 
#' @importFrom purrr map2
#' @importFrom gridExtra arrangeGrob grid.arrange
#' @import ggplot2


trip_length <- function(dat, project, start, end, units = "days", catch = NULL, 
                        hauls = NULL, output = c("table", "plot"), haul_to_trip = FALSE, ...){
  
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset
  
  
  if (lubridate::is.POSIXt(dataset[[start]])) {
    
    dataset[[start]] <- dataset[[start]]
    
  } else if (lubridate::is.Date(dataset[[start]])) {
    
    dataset[[start]] <- dataset[[start]]
    
  } else if (all(grepl("^.*\\s\\d{2}:\\d{2}:\\d{2}$", dataset[[start]])) | all(grepl("^.*\\s\\d{2}:\\d{2}$", dataset[[start]]))) {
    
    dataset[[start]] <- date_time_parser(dataset[[start]])
    
  } else if (all(grepl("^\\d{4}-\\d{2}-\\d{2}$", dataset[[start]]))) {
    
    dataset[[start]] <- date_parser(dataset[[start]])
    
  } else {
    
    warning("Start date format not recognized.")
    
  }
  
  if (lubridate::is.POSIXt(dataset[[end]])) {
    
    dataset[[end]] <- dataset[[end]]
    
  } else if (lubridate::is.Date(dataset[[end]])) {
    
    dataset[[end]] <- dataset[[end]]
    
  } else if (all(grepl("^.*\\s\\d{2}:\\d{2}:\\d{2}$", dataset[[end]])) | all(grepl("^.*\\s\\d{2}:\\d{2}$", dataset[[end]]))) {
    
    dataset[[end]] <- date_time_parser(dataset[[end]])
    
  } else if (all(grepl("^\\d{4}-\\d{2}-\\d{2}$", dataset[[end]]))) {
    
    dataset[[end]] <- date_parser(dataset[[end]])
    
  } else {
    
    warning("end date format not recognized.")
    
  }
  
  if (isTRUE(class(dataset[[start]]) == class(dataset[[end]])) == FALSE) {
    
    warning("Start and end dates have different classes. Suggest converting date variables to same class.")
    
  }
  
  
  if (anyNA(dataset[[start]]) | anyNA(dataset[[end]])) {
    
    warning("NAs detected in dates.")
    
  }
  
  if (haul_to_trip == TRUE) {
    
    dataset <- haul_to_trip(dataset, ...)
    
  }
  
  if (lubridate::is.POSIXt(dataset[[start]]) | lubridate::is.POSIXt(dataset[[end]])) {
    
    trip_length <- as.numeric(difftime(dataset[[end]], dataset[[start]]),
                              units = units)
    
  } else {
    
    trip_length <- as.numeric(dataset[[end]] - dataset[[start]], units = units)
    
  }
  
  df <- data.frame(start = dataset[[start]], end = dataset[[end]], trip_length)
  
  if (any(trip_length[!is.na(trip_length)] < 0)) {
    
    warning(paste(sum(trip_length < 0), "negative values produced."))
    
    if (sum((trip_length < 0)) / length(trip_length) > .05) { 
      
      warning("Negative values exceed 5% of observations.")
      
    }
  }
  
  if (!is.null(hauls)) {
    
    haul_ratio <- (dataset[[hauls]] / df$trip_length)
    
    df <- data.frame(df, haul_ratio)
    
    if (any(is.infinite(haul_ratio))) {

      warning(paste(sum(is.infinite(haul_ratio)),"Inf values produced for haul_ratio. Consider using date-time variables to avoid Inf values."))
      
    }
    
    if (any(is.nan(haul_ratio))) {
      
      warning(paste(sum(is.nan(haul_ratio)),"NaN values produced for haul_ratio. Consider using date-time variables to avoid NaN values. NaNs replaced with zero."))
      
    }
  } 
  
  if (!is.null(catch)) {
    
    if (length(catch) == 1) {
      
      cpue <- (dataset[[catch]] / df$trip_length)
      
      df <- data.frame(df, cpue)
      
      if (any(is.infinite(cpue))) {
        
        warning(paste(sum(is.infinite(cpue)), "Inf values produced for cpue. Consider using date-time variables to avoid Inf values."))
        
      }
      
      if (any(is.nan(cpue))) {
        
        df$cpue[is.nan(df$cpue)] <- 0
        
        warning(paste(sum(is.nan(cpue)), "NaN values produced for cpue. Consider using date-time variables to avoid NaN values."))
        
      }
      
      names(df)[names(df) == "cpue"] <- paste0(catch, "_cpue")
      
    } else if (length(catch) > 1) {
      
      cpue <- lapply(catch, function(s){
        
        dataset[[s]]/df$trip_length
        
        #cpue(df, xTime = "trip_length", xWeight = s, name = paste(s, "cpue", sep = "_"))
        #
      })
      
      cpue_names <- vapply(catch, FUN = function(s) paste(s, "cpue", sep = "_"), FUN.VALUE = "character")
      
      cpue_names <- unname(cpue_names)
      
      names(cpue) <- cpue_names
      
      cpue <- data.frame(cpue)
      
      df <- cbind(df, cpue) 
      
      if (any(apply(df[cpue_names], FUN = is.infinite, MARGIN = 2))) {
        
        warning("Inf values produced for cpue. Consider using date-time variables to avoid Inf values.")
        
      }
      
      if (any(apply(df[cpue_names], FUN = is.nan, MARGIN = 2))) {
        
        warning("NaN values produced for cpue. Consider using date-time variables to avoid NaN values.")
        
      }
    }
  }
  
  
  plots <- lapply(df, function(p) {
    
    ggplot2::ggplot(df, ggplot2::aes(x = p)) + ggplot2::geom_histogram() + 
      fishset_theme
    
  })
  
  names <- names(plots)
  
  plots <- purrr::map2(plots, names, ~.x + ggplot2::labs(x = paste0(.y, " (", units, ")")))
  
  g <- do.call(gridExtra::arrangeGrob, c(plots, nrow = length(plots)))
  
  
  # Log function
  trip_length_function <- list()
  trip_length_function$functionID <- "trip_length"
  trip_length_function$args <- c(dat, project, start, end, units, catch, hauls, 
                                 haul_to_trip, output)
  trip_length_function$kwargs <- ""
  
  log_call(trip_length_function)
  
  # Save output
  
  save_table(df, project, "trip_length")
  save_plot(project, "trip_length", g)
  
  names(df)[names(df) == "trip_length"] <- paste0("trip_length_", units)
  
  if (output == "table") {
    
    df
    
  } else {
    
    do.call(gridExtra::grid.arrange, c(plots, nrow = length(plots)))
    
  }
}

