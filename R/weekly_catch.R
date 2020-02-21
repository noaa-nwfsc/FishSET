# Weekly catch
weekly_catch <- function(dat, project, species, date, year = NULL, convert_to_tons, output = c("table", "plot")) {
#' 
#' Catch total by week
#'
#' @param dat Main data frame over which to apply function. Table in fishset_db 
#'   database should contain the string `MainDataTable`.
#' @param project name of project.
#' @param species Variable(s) containing species data
#' @param date Dave Variable
#' @param year Optional. If year is supplied then data is subset to that year
#' @param convert_to_tons TRUE or FALSE
#' @param output output as a table or plot
#' @examples 
#' \donrun{
#' weekly_catch("pollockMainDataTable", species = c("HAUL_LBS_270_POLLOCK_LBS", 
#' "HAUL_LBS_110_PACIFIC_COD_LBS",  "HAUL_LBS_OTHER_LBS"), date = "DATE_FISHING_BEGAN", 
#' convert_to_tons = T, year = NULL, output = "plot")
#' }
#' @export weekly_catch
#' @importFrom lubridate is.Date
#' @importFrom stats aggregate
#' @importFrom reshape2 melt
#' @import ggplot2
#'
  
  #Call in datasets
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset
  
  if (lubridate::is.Date(dataset[[date]]) == FALSE) {
    
    warning("Date variable is not in correct format. Converting to date format.")
    
    dataset[[date]] <- date_parser(dataset[[date]])
  }
  
  dataset$week <- format(dataset[[date]], "%W")
  
  if (!is.null(year)) {
    
    dataset <- subset(dataset, format(dataset[[date]], "%Y") == year)
    
  }
  
  if (length(species) == 1) {
    
    df <- stats::aggregate(dataset[[species]], by = list(dataset$week), FUN = sum)
    
    df <- setNames(df, c("week", "catch"))
    
    if (convert_to_tons == TRUE) {
    
    df$catch <- df$catch/2000 
    
    }
    
    ggplot2::ggplot(df, ggplot2::aes(week, catch)) + ggplot2::geom_col() +
      fishset_theme
    
  } else {
    
    df <- lapply(species, function(x) {
      stats::aggregate(dataset[[x]], by = list(dataset$week), FUN = sum) 
    })
    
    df <- setNames(df, species)
    
    df <- reshape2::melt(df)
    
    df$variable <- NULL
    
    df <- setNames(df, c("week", "catch", "species"))
    
    df$species <- as.factor(df$species)
    
    df$species <- factor(df$species, 
                         levels = unique(df$species[order(df$catch, df$species)]), 
                         ordered = TRUE)
    
    if (convert_to_tons == TRUE) {
      
      df$catch <- df$catch/2000 
      
    }
    
    if (output == "plot") {
    
    plot <- ggplot2::ggplot(df, ggplot2::aes(week, catch, fill = species)) + 
              ggplot2::geom_col(position = "stack", col = "black") +
              fishset_theme + 
              ggplot2::theme(legend.position = "bottom", 
                             legend.box.background = ggplot2::element_rect(color = "black", 
                                                                           size = 1, 
                                                                           linetype = 1))
    }
  }
  
  weekly_catch_function <- list()
  weekly_catch_function$functionID <- "weekly_catch"
  weekly_catch_function$args <- c(dat, project, species, date, year, convert_to_tons, output)
  log_call(weekly_catch_function)
  
  save_table(df, project, "weekly_catch")
  
  save_plot(project, "weekly_catch", plot)
  
  if (output == "plot") {
    
    plot
  
    } else {
      
      df
    }  
}
