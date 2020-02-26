#' Catch by Species
#'
#' Creates a table or plot of total species catch by period
#' 
#' @param dat Main data frame over which to apply function. Table in fishset_db 
#'   database should contain the string `MainDataTable`.
#' @param project name of project.
#' @param s A variable containing the species catch or a vector
#'   of species variables (in pounds).
#' @param t time variable containing dates to aggregate by.
#' @param period Time period to count by. Options include "year", "year_abv", 
#'   "month", "month_abv", "month_num", "weeks" (weeks in the year), 
#'   "weekday", "weekday_abv", "weekday_num", "day" (day of the month), 
#'   and "day_of_year".
#' @param convert_to_tons A logical value indicating whether catch variable should 
#'   be converted to tons.
#' @param value Whether to return raw count or percentage of catch. 
#' @param output table or plot.
#' @return This function returns a table or plot of the total species catch by period.
#' @examples 
#' \dontrun{
#' species_catch("pollockMainDataTable", s = c("HAUL_LBS_270_POLLOCK_LBS", 
#' "HAUL_LBS_110_PACIFIC_COD_LBS", "HAUL_LBS_OTHER_LBS"), t = "HAUL_DATE", 
#' value = "count", period = "month_num", output = "plot", convert_to_tons = TRUE)
#' }
#' @export species_catch
#' @import ggplot2
#' @importFrom stats aggregate
#' @importFrom reshape2 dcast melt
#' @importFrom scales percent


species_catch <- function(dat, project, s, t, period = "year", convert_to_tons = TRUE, 
                          value = c("count", "percent"), output = c("table", "plot")){
  
  #Call in datasets
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset
  
  periods <- c("year", "year_abv", "month", "month_abv", "month_num", "weeks", 
               "weekday", "weekday_abv", "weekday_num", "day", "day_of_year")
  
  if (period %in% periods == FALSE) {
    
    stop("Invalid period. Please select a valid period name (see documentation for details).")
    
  } else {
    
    p <- switch(period, "year" = "%Y", "year_abv" = "%y", "month" = "%B",
                "month_abv" = "%b", "month_num" = "%m", "weeks" = "%U", 
                "weekday" = "%A", "weekday_abv" = "%a", "weekday_num" = "%w", 
                "day" = "%d", "day_of_year" = "%j")
  }
  
  if (length(s) == 1) {
    
    count <- stats::aggregate(dataset[[s]], 
                              by = list(format(date_parser(dataset[[t]]), p)), 
                              FUN = sum,
                              drop = FALSE)
    
    names(count) <- c("date", "catch")
    
    if (convert_to_tons == TRUE) {
      
      count$catch <- count$catch/2000
      
    }
    
    if (value == "percent") {
      
      count[["catch"]] <- count[["catch"]] / sum(count[["catch"]])
      
    }
    
    if (p %in% c("%a", "%A", "%b", "%B")) { 
      
      count <- date_factorize(count, "date", p)
      
      if (p == "%b") {
        
        ind <- periods_list$month_abv[which(!(periods_list$month_abv %in% unique(count$date)))]
        
      } else if (p == "%B") {
        
        ind <- periods_list$month[which(!(periods_list$month %in% unique(count$date)))]
        
      } else if (p == "%A") {
        
        ind <- periods_list$weekday[which(!(periods_list$weekday %in% unique(count$date)))]
        
      } else {
        
        ind <- periods_list$weekday_abv[which(!(periods_list$weekday_abv %in% unique(count$date)))]
        
      }
      
      missing_periods <- data.frame(date = ind, catch = rep(0, length(ind)))
      
      count <- bind_rows(count, missing_periods)
      
      count <- date_factorize(count, "date", p)
      
    } else {
      
      count$date <- as.integer(count$date)
      
      if (p == "%U") {
        
        ind <- which(!(1:52 %in% unique(count$date))) 
        
      } else if (p == "%j") {
        
        ind <- which(!(1:365 %in% unique(count$date)))
        
      } else if (p == "%d") {
        
        ind <- which(!(1:31 %in% unique(count$date)))
        
      } else if (p =="%m") {
        
        ind <- which(!(1:12 %in% unique(count$date)))
        
      } else {
        
        ind <- which(!(0:6 %in% unique(count$date)))
        
      }
      
      missing_periods <- data.frame(date = ind, catch = rep(0, length(ind)))
      
      count <- bind_rows(count, missing_periods)
      
      count <- count[order(count$date),]
      
    }
    
    plot <- ggplot2::ggplot(data = count, ggplot2::aes(x = date, y = catch)) + 
      ggplot2::geom_col() + 
      ggplot2::labs(y = if (value == "count" & convert_to_tons == TRUE) "catch (tons)" else "catch") +
      ggplot2::scale_y_continuous(labels = if (value == "percent") scales::percent else waiver()) +
      if (p %in% c("%a", "%A", "%b", "%B")) { ggplot2::scale_x_discrete(breaks = levels(count$date)) }
    else {
      ggplot2::scale_x_continuous(breaks = if (length(count$date) <= 12){seq(1,length(count$date), by = 1)} 
                                  else { seq(1, length(count$date), by = round(length(count$date) * 0.08)) }) } +  
      fishset_theme
    
  } else {
    
    count <- lapply(s, function(x) {
      stats::aggregate(dataset[[x]], 
                       by = list(format(date_parser(dataset[[t]]), p)), 
                       FUN = sum,
                       drop = FALSE)
    })
    
    names(count) <- s
    
    count <- reshape2::melt(count, id.vars = "Group.1")
    
    count$variable <- NULL
    
    colnames(count) <- c("date", "catch", "species")
    
    if (convert_to_tons == TRUE) {
      
      count$catch <- count$catch/2000
      
    }
    
    if (value == "percent") {
      
      count[["catch"]] <- count[["catch"]] / sum(count[["catch"]])
      
    }
    
    if (p %in% c("%a", "%A", "%b", "%B")) { 
      
      count <- date_factorize(count, "date", p)
    }
    
    if (output == "table") {
      
      count <- reshape2::dcast(count, date ~ species, value.var = "catch")
      
    } else {
      
      plot <- ggplot2::ggplot(data = count, ggplot2::aes(x = date, y = catch)) + 
        ggplot2::geom_col() + 
        ggplot2::facet_grid(species ~ ., scales = "free_y") +
        ggplot2::labs(y = if (value == "count" & convert_to_tons == TRUE) "catch (tons)" else "catch") +
        ggplot2::scale_y_continuous(labels = if (value == "percent") scales::percent else waiver()) +
        fishset_theme
      
    }
  }
  
  #Log the function 
  
  species_catch_function <- list()
  species_catch_function$functionID <- "species_catch"
  species_catch_function$args <- c(dat, project, s, t, period, convert_to_tons, value, output)
  log_call(species_catch_function)
  
  # Save output
  
  save_table(count, project, "species_catch")
  
  save_plot(project, "species_catch")
  
  if (output == "table") {
    
    count 
    
  } else {
    
    plot 
  }
}
