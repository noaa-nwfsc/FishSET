#' Collapse a data frame from haul-level to trip-level
#' 
#' @description
#' Aggregates a data frame from individual observations (hauls) to a summary level (trips).
#' It provides flexible methods for collapsing numeric, character, and temporal data.
#' 
#' @param dat String, the name of the main data containing information on hauls or trips. Note 
#'   that this is the project 'MainDataTable' in the FishSET database.
#' @param project String, name of project.
#' @param trip_id String. Column name that represents the unique trip identifier in \code{dat}.
#' @param zoneID_dat String, the name of the column identifying fishing zones. This column is 
#'   handled separately from other columns.
#' @param zone_fun String, method for collapsing the 'zoneID_dat' variable.
#'   Options are \code{"first"}, \code{"last"}, or \code{"mode"}.
#' @param date_fun String, method for collapsing temporal columns. Options are \code{"mean"}, 
#'   \code{"median"}, \code{"min"}, or \code{"max"}.
#' @param num_fun String, method for collapsing numeric columns. Options are \code{"mean"}, 
#'   \code{"median"}, \code{"mode"}, \code{"min"}, \code{"max"}, or \code{"sum"}.
#' @param char_fun String, method for collapsing character or factor columns. Options are 
#'   \code{"first"}, \code{"last"}, \code{"paste"}, or \code{"mode"}.
#' @param haul_count Logical, If \code{TRUE}, a column name "haul_count" is added,
#'   showing the number of hauls (rows) per trip in the original data.
#' @param log_fun Logical, If \code{TRUE}, the function call is logged for tracking.
#' 
#' @return A data frame where each row represents a single trip, aggregated according to the 
#'   specified methods.
#'   
#' @details 
#'   The function aggregates columns based on their data type per unique trip ID. For columns that 
#'   are not numeric, date, or character, the function defaults to taking the first observation 
#'   for each trip.
#' 
#' @export haul_to_trip
#'
#' @examples
#' \dontrun{
#' # Collapse the data from haul to trip level
#' trip_data <- haul_to_trip(
#'   dat = "pollockMainDataTable",
#'   project = "pollock",
#'   trip_id = "tripID,
#'   zone_col = "ZONE",
#'   zone_fun = "mode", # Use the most common zone for the trip
#'   date_fun = "min",   # Use the earliest haul date as the trip date
#'   num_fun = "sum",    # Sum the fish weight for the trip
#'   char_fun = "first", # Use the first vessel name recorded
#'   haul_count = TRUE
#' )
#' }
#'
haul_to_trip <- function(dat, 
                         project, 
                         trip_id,
                         zoneID_dat,
                         zone_fun = "mode",
                         date_fun = "min",
                         num_fun = "mean", 
                         char_fun = "mode",
                         haul_count = TRUE, 
                         log_fun = TRUE) {
  
  # Data setup and prep ---------------------------------------------------------------------------
  
  # Pull in dataset
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  # Identify columns by type, exclude the ID column -----------------------------------------------
  # Note: using base R in this function to limit dependencies
  all_cols <- names(dataset)[(names(dataset) != trip_id) &
                               (names(dataset) != zoneID_dat)] # EXCLUDE ZONE ID
  
  is_numeric_col <- sapply(dataset[all_cols], is.numeric)
  numeric_cols <- all_cols[is_numeric_col]
  
  is_char_or_factor <- sapply(dataset[all_cols], 
                              function(c) is.character(c) || is.factor(c))
  char_cols <- all_cols[is_char_or_factor]
  
  is_date_or_time <- sapply(dataset[all_cols], 
                            function(c) inherits(c, "Date") || inherits(c, "POSIXt") )
  date_cols <- all_cols[is_date_or_time]
  
  # Aggregate each datatype separately ------------------------------------------------------------
  
  # List to hold the aggregated dataframes
  aggregated_list <- list()
  
  # Base dataframe with just unique IDs
  aggregated_list$ids <- data.frame(trip_id = unique(dataset[[trip_id]]))
  names(aggregated_list$ids) <- trip_id
  
  # Helper function for aggregation
  get_mode <- function(x) {
    ux <- unique(na.omit(x))
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  # Handle zone ID column separately
  zone_formula <- as.formula(paste(". ~", trip_id))
  zone_agg_fun <- switch(zone_fun,
                         "mode" = get_mode,
                         "first" = function(x) x[1],
                         "last" = function(x) x[length(x)])
  aggregated_list$zone <- aggregate(zone_formula, 
                                    data = dataset[,c(trip_id, zoneID_dat)],
                                    FUN = zone_agg_fun)
  
  # Date aggregation
  if (length(date_cols) > 0) {
    date_formula <- as.formula(paste(". ~", trip_id))
    date_agg_fun <- function(x) get(date_fun)(x, na.rm = TRUE)
    # Aggregate converts dates to numeric so need to convert back
    date_agg <- aggregate(date_formula,
                          data = dataset[,c(trip_id, date_cols)],
                          FUN = date_agg_fun)
    original_classes <- sapply(dataset[date_cols], class)
    for (col in date_cols) {
      # Handles both Date and POSIX format
      if (unname(unlist(original_classes[col][1])[1]) == "Date") {
        date_agg[[col]] <- as.Date(date_agg[[col]])
        
      } else if (grepl("POSIX", unname(unlist(original_classes[col][1])[1]))) {
        date_agg[[col]] <- as.POSIXct(date_agg[[col]])
      }
    }
    aggregated_list$date <- date_agg
  }
  
  # Numeric aggregation
  if (length(numeric_cols) > 0) {
    num_formula <- as.formula(paste(". ~", trip_id))
    num_agg_fun <- function(x) get(num_fun)(x, na.rm = TRUE)
    aggregated_list$numeric <- aggregate(num_formula, 
                                         data = dataset[,c(trip_id, numeric_cols)],
                                         FUN = num_agg_fun)
  }
  
  # Character/factor aggregation
  if (length(char_cols) > 0) {
    char_formula <- as.formula(paste(". ~", trip_id))
    char_agg_fun <- switch(char_fun,
                           "paste" = function(x) paste(unique(x), collapse = ","),
                           "mode" = get_mode,
                           "first" = function(x) x[1],
                           "last" = function(x) x[length(x)],
                           function(x) x[1])
    aggregated_list$character <- aggregate(char_formula, 
                                           data = dataset[, c(trip_id, char_cols)], 
                                           FUN = char_agg_fun)
  }
  
  # Haul counter
  if (haul_count) {
    haul_count_df <- data.frame(table(dataset[[trip_id]]))
    names(haul_count_df) <- c(trip_id, "haul_count")
    aggregated_list$haul_count <- haul_count_df
  }
  
  # Merge all data frames together ----------------------------------------------------------------
  aggregated_list <- aggregated_list[!sapply(aggregated_list, is.null)]
  
  df_out <- Reduce(function(x, y) merge(x, y, by = trip_id, all = TRUE), aggregated_list)
  
  # Log function call -----------------------------------------------------------------------------
  if (log_fun) {
    haul_to_trip_function <- list()
    haul_to_trip_function$functionID <- "haul_to_trip"
    haul_to_trip_function$args <- list(dat, 
                                       project, 
                                       trip_id,
                                       zoneID_dat,
                                       zone_fun,
                                       num_fun, 
                                       char_fun,
                                       date_fun,
                                       haul_count, 
                                       log_fun)
    haul_to_trip_function$output <- list(df_out)
    log_call(project, haul_to_trip_function)
  }
  
  return(df_out)
}
