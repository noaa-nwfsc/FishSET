#' Check for common data quality issues that may be present in the data set.
#'
#' Function tests for common data quality issues.
#' @param dat Primary data containing information on hauls or trips.
#' Table in FishSET database contains the string 'MainDataTable'.
#' @param project String, name of project.
#' @param log_fun Logical, whether to log function call (for internal use).
#' @return Statements as to whether data quality issues may exist.
#' @keywords internal
#' @importFrom dplyr distinct
#' @importFrom ggplot2 map_data
#' @importFrom graphics par
#' @export data_verification
#' @details Checks that all columnn names in the data frame are unique, whether any columns in the data frame are empty,
#' whether each row is a unique choice occurrence at the haul or trip level, and that either latitude and longitude or
#' fishing area are included.
#' @examples
#' \dontrun{
#' data_verification(pollockMainDataTable, 'pollock')
#' }
#'
data_verification <- function(dat, project, log_fun = TRUE) {

  # Call in datasets Call in datasets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  check <- 0

  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  cat("Data verification checks for the", project, "project using", dat, "dataset on", 
      format(Sys.Date(), format = "%Y%m%d"), file = tmp)

  # check each row of data is a unique choice occurrence at haul or trip level
  if (nrow(dataset) == nrow(dplyr::distinct(dataset))) {
    cat("\nPass: Each row is a unique choice occurrence.", file = tmp, append = TRUE)
  } else {
    cat("\nEach row in dataset is not a unique choice occurrence at haul or trip level.",
        file = tmp, append = TRUE)
    check <- 1
  }

  # Handling of empty variables
  empty_ind <- qaqc_helper(dataset, function(x) all(is.na(x)))
  
  if (any(empty_ind)) {
    
    cat("\n", names(dataset[empty_ind]), "is empty. Consider removing the column from the dataset.",
      file = tmp, append = TRUE)
    
  } else {
    cat("\nPass: No empty variables exist in the dataset.", file = tmp, append = TRUE)
  }

  if (any(grepl("lat|lon", names(dataset), ignore.case = TRUE))) {
    
    lat_lon <- grep("lat|lon", names(dataset), ignore.case = TRUE)
    num_ll <- !qaqc_helper(dataset[lat_lon], is.numeric)
    deg_ll <- qaqc_helper(dataset[lat_lon], function(x) any(nchar(trunc(abs(x))) > 3)) 
    
    if (any(c(deg_ll, num_ll))) {
      
      cat("At least one lat/lon variable is not in decimal degrees. Use the function degree() to convert to decimal degrees.", 
          file = tmp, append = TRUE)
      
    }  else {
      
      cat("\nPass: lat/lon variables in decimal degrees.",
          file = tmp, append = TRUE)
      
     
      
      lat <- find_lat(dataset)[1]
      lon <- find_lon(dataset)[1]
       pts <- sample(nrow(dataset), nrow(dataset) / 10)
       datatomap <- dataset[pts,c(lat,lon) ]
       datatomap$lon <- datatomap[[lon]]
       datatomap$lat <- datatomap[[lat]]
      
      if(min(datatomap$lon) < 0 & max(datatomap$lon) > 0){
        recenter <- TRUE
        datatomap$lon <- ifelse(datatomap$lon < 0 , datatomap$lon + 360, datatomap$lon)
        worldmap <- ggplot2::map_data("world", wrap = c(0, 360))
        
      } else {
        recenter <- FALSE
        worldmap <- ggplot2::map_data("world")
        
      }
      
      
      g_map <- ggplot2::ggplot() +
        suppressWarnings(ggplot2::geom_map(map = worldmap, ggplot2::aes(x = worldmap$long, y = worldmap$lat,
                                                                     map_id = worldmap$region),
                                           fill = "grey", color = "black", size = 0.375))
      
      
      map_theme <-
        ggplot2::theme(text = ggplot2::element_text(size = 12),
                       axis.title.y = ggplot2::element_text(vjust = 1.5),
                       legend.title = ggplot2::element_blank(),
                       panel.grid.major = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank(),
                       panel.background = ggplot2::element_blank(),
                       panel.border = ggplot2::element_rect(colour = "black",
                                                            fill = NA, size = 1))
      
      
      mapout <-
        g_map +
        ggplot2::geom_point(data = datatomap, ggplot2::aes(x = lon, y = lat),
                            size = 0.975, alpha = 0.25, colour='red') +
        ggplot2::scale_x_continuous(limits=c(min(datatomap$lon),max(datatomap$lon))) +
        ggplot2::ylim(min(datatomap$lat), max(datatomap$lat)) +
        ggplot2::ggtitle("Points") +
        ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 10))) +
        
        map_theme +
        ggplot2::xlab("Longitude") +
        ggplot2::ylab("Latitude")
      
      
       print(mapout)
      print("10% of samples plotted. Verify that points occur in correct geographic area.")
    }
  }

  msg_print(tmp)

  if (log_fun) {
    
    data_verification_function <- list()
    data_verification_function$functionID <- "data_verification"
    data_verification_function$args <- list(dat, project, log_fun)
    data_verification_function$kwargs <- list()
    data_verification_function$msg <- suppressWarnings(readLines(tmp))
    log_call(project, data_verification_function)
  }

  if (check == 1) {
    warning("At least one error exists")
  }
}


### Check that all observations are unique
unique_filter <- function(dat, project, remove = FALSE) {
  #' Check rows are unique
  #'
  #' Check for and remove non-unique rows from primary dataset.
  #'
  #' @param dat Primary data containing information on hauls or trips.
  #'   Table in the FishSET database contains the string 'MainDataTable'.
  #' @param project String, name of project.
  #' @param remove Logical, if \code{TRUE} removes non-unique rows. Defaults to 
  #'   \code{FALSE}.
  #' @details Output is determined by \code{remove}. If \code{remove = TRUE} then
  #' non-unique rows are removed. If \code{remove = FALSE} then only a statement is
  #' returned regarding the number of rows that are not unique.
  #' @return Returns the modified primary dataset with non-unique rows removed if 
  #'   \code{remove = TRUE}.
  #' @export unique_filter
  #' @importFrom dplyr distinct
  #' @examples
  #' \dontrun{
  #' # check for unique rows
  #' unique_filter(pollockMainDataTable)
  #' 
  #' # remove non-unique rows from dataset
  #' mod.dat <- unique_filter(pollockMainDataTable, remove = TRUE)
  #' }

  # Call in datasets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  tmp <- tempfile()
  cat("Unique filter check for", dat, "dataset on", 
      format(Sys.Date(), format = "%Y%m%d"), "\n", file = tmp, append = TRUE)

  if (nrow(dataset) == nrow(dplyr::distinct(dataset))) {
    
    cat("Each row is a unique choice occurrence. No further action required.\n", 
        file = tmp, append = TRUE)
    
  } else {
    
    if (remove == FALSE) {
      
      cat("Each row in data set is not a unique choice occurrence at haul or trip level. \nConsider removing non-unique rows.\n", 
          file = tmp, append = TRUE)
      
    } else {
      
      cat("Each row in data set is not a unique choice occurrence at haul or trip level. \nNon-unique rows removed.\n", 
          file = tmp, append = TRUE)
      dataset <- dplyr::distinct(dataset)
    }
  }

  msg_print(tmp)

  unique_filter_function <- list()
  unique_filter_function$functionID <- "unique_filter"
  unique_filter_function$args <- list(dat, project, remove)
  unique_filter_function$output <- c(dat)
  unique_filter_function$msg <- suppressWarnings(readLines(tmp))
  log_call(project, unique_filter_function)

  unlink(tmp)
  
  if (remove == TRUE) {
    return(dataset)
  }
}


empty_vars_filter <- function(dat, project, remove = FALSE) {
  #' Check variables are not empty
  #'
  #' Check for and remove empty variables from dataset. Empty variables are 
  #' columns in the data that contain all NAs and/or empty strings. 
  #'
  #' @param dat Primary data containing information on hauls or trips.
  #'   Table in the FishSET database contains the string 'MainDataTable'.
  #' @param project String, name of project.
  #' @param remove Logical, whether to remove empty variables. Defaults to \code{FALSE}.
  #' @details Function checks for empty variables and prints an outcome message to 
  #'   the console. If empty variables are present and \code{remove = TRUE}, then 
  #'   empty variables will be removed from the dataset. Empty variables are 
  #'   columns in the dataset that contain all NAs or empty strings. 
  #' @return Returns the dataset with empty variables removed if \code{remove = TRUE}.
  #' @export empty_vars_filter
  #' @examples
  #' \dontrun{
  #' # check for empty vars
  #' empty_vars_filter(pollockMainDataTable)
  #' 
  #' # remove empty vars from data
  #' mod.dat <- empty_vars_filter(pollockMainDataTable, 'pollock', remove = TRUE)
  #' }

  # Call in datasets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  

  tmp <- tempfile()
  cat("Empty vars check for", dat, "dataset on", 
      format(Sys.Date(), format = "%Y%m%d"), "\n", file = tmp, append = TRUE)

  empty_ind <- qaqc_helper(dataset, is_value_empty)
  
  if (any(empty_ind)) {
    
    empty_vars <- names(dataset[empty_ind])

    if (remove == TRUE) {
      
      dataset <- dataset[!empty_ind]
      cat(empty_vars, "is empty and has been removed from the data set.", 
          file = tmp, append = TRUE)
      
    } else {
      
      cat(empty_vars, "is empty. Consider removing from the data set.", 
          file = tmp, append = TRUE)
    }
    
  } else {
    
    cat("No empty variables identified.", file = tmp, append = TRUE)
  }

  msg_print(tmp)

  empty_vars_filter_function <- list()
  empty_vars_filter_function$functionID <- "empty_vars_filter"
  empty_vars_filter_function$args <- list(dat, project, remove)
  empty_vars_filter_function$output <- list(dat)
  empty_vars_filter_function$msg <- suppressWarnings(readLines(tmp))
  log_call(project, empty_vars_filter_function)

  unlink(tmp)
  
  if (remove == TRUE) {
    return(dataset)
  }
}
