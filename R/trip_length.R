#' Trip length
#' 
#' Create a plot or table of vessel trip length
#' 
#' @param dat Main data frame over which to apply function. Table in FishSET 
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
#' @param ... Additional arguments passed to the haul_to_trip function. 
#' @return The function calculates vessel trip length given a start and end date, 
#'   and then converts trip length to the desired unit of time (e.g. weeks, days, 
#'   or hours), returning a table or histogram. There is also the option of 
#'   including catch and hauls per trip. 
#' @examples 
#' \dontrun{
#' trip_length("pollockMainDataTable", start = "FISHING_START_DATE", end = "HAUL_DATE",
#' units = "days", catch = "OFFICIAL_TOTAL_CATCH", hauls = "HAUL", output = "plot", 
#' haul_to_trip = T, fun.numeric = sum, fun.time = min, "VESSEL", "FISHING_START_DATE")
#' #' }
#' @importFrom lubridate is.Date 
#' @importFrom purrr map2
#' @importFrom gridExtra arrangeGrob grid.arrange
#' @import ggplot2


trip_length <- function(dat, project, start, end, units = "days", catch = NULL, 
                        hauls = NULL, output = c("table", "plot"), haul_to_trip = FALSE, ...) {
  
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset
  
  if (lubridate::is.Date(dataset[[start]]) == FALSE) {
    
    dataset[[start]] <- date_parser(dataset[[start]])
  }
  
  if (lubridate::is.Date(dataset[[end]]) == FALSE) {
    
    dataset[[end]] <- date_parser(dataset[[end]])
  }
  
  if (any(is.na(dataset[[start]]) | is.na(dataset[[end]]))) {
    
    warning("NAs detected in dates.")
    
  }
  
  if (haul_to_trip == TRUE) {
    
    dataset <- haul_to_trip(dataset, ...)
    
  }
    
  trip_length <- as.numeric(difftime(dataset[[end]], dataset[[start]]),
                            units = units)
  
  if (any(trip_length < 0)) {
    
    warning("Negative values produced.")
    
  }
  
  if (any(is.nan(trip_length) | is.infinite(trip_length))) {
    
    warning("NaN and/or Inf values produced.")
    
  }
  
  if (is.null(catch) & !is.null(hauls)) {
    
    haul_ratio <- (dataset[[hauls]] / trip_length)
    
    df <- data.frame(haul_ratio, trip_length)
    
  } else if (is.null(hauls) & !is.null(catch)) {
    
    catch_ratio <- (dataset[[catch]] / trip_length)
    
    df <- data.frame(catch_ratio, trip_length)
    
  } else if (is.null(catch) & is.null(hauls)) {
    
    df <- data.frame(trip_length)
    
  } else {
    
    haul_ratio <- (dataset[[hauls]] / trip_length)
    
    catch_ratio <- (dataset[[catch]] / trip_length)
    
    df <- data.frame(haul_ratio, catch_ratio, trip_length)
    
  }
  
  if (output == "table") {
  
    df <- data.frame(df, dataset[[start]], dataset[[end]])
    
  } else {
    
    plots <- lapply(df, function(p) {
      
      ggplot2::ggplot(df, ggplot2::aes(x = p)) + ggplot2::geom_histogram() + 
        fishset_theme
      
      })
    
    names <- names(plots)
    
    plots <- purrr::map2(plots, names, ~.x + labs(x = paste0(.y, " (", units, ")")))
    
    g <- do.call(gridExtra::arrangeGrob, c(plots, nrow = length(plots)))
    
  }

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
  
  if (output == "table") {
    
    df
    
  } else {
    
    do.call(gridExtra::grid.arrange, c(plots, nrow = length(plots)))
    
  }
}
