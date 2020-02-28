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
#' @param group Up to two variables to group by. For plots, 
#'   the first variable is passed to "fill" and second is passed to "facet_grid".   
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


species_catch <- function(dat, project, s, t, period = "year", group = NULL, convert_to_tons = TRUE, 
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
  
  if (!is.null(group)){
    
    group1 <- group[1]
    
    if (length(group) == 2){
      
      group2 <- group[2] 
      
    } else if (length(group) > 2) {
      
      warning("Too many grouping variables included, selecting first two.")
      
    }
    
  }
  
  if (length(s) == 1) {
    
    if (is.null(group)) {
      
      count <- stats::aggregate(dataset[[s]], 
                                by = list(format(date_parser(dataset[[t]]), p)), 
                                FUN = sum)
      
      names(count) <- c("date", "catch")
      
    } else if (!is.null(group)) {
      
      if(length(group) == 1) {
        
        count <- stats::aggregate(dataset[[s]], 
                                  by = list(format(date_parser(dataset[[t]]), p), dataset[[group1]]), 
                                  FUN = sum)
        
        names(count) <- c("date", "group_1", "catch")
        
      } else {
        
        count <- stats::aggregate(dataset[[s]], 
                                  by = list(format(date_parser(dataset[[t]]), p), 
                                            dataset[[group1]],
                                            dataset[[group2]]), 
                                  FUN = sum)
        
        names(count) <- c("date", "group_1", "group_2", "catch")
        
      }
    }
    
    if (convert_to_tons == TRUE) {
      
      count$catch <- count$catch/2000
      
    }
    
    if (value == "percent") {
      
      count$catch <- count$catch / sum(count$catch)
      
    }
    
    if (p %in% c("%a", "%A", "%b", "%B")) { 
      
      count <- date_factorize(count, "date", p)
      
    } else {
      
      count$date <- as.integer(count$date)
      
    }
    
    ind <- periods_list[[p]][which(!(periods_list[[p]] %in% unique(count$date)))]
    
    if (is.null(group)) {
      
      missing_periods <- data.frame(date = ind)
      
    } else if (!is.null(group) & length(group) == 1) {
      
      missing_periods <- expand.grid(date = ind, 
                                     group_1 = unique(count$group_1))
      
    } else if (!is.null(group) & length(group) > 1) {
      
      missing_periods <- expand.grid(date = ind, 
                                     group_1 = unique(count$group_1), 
                                     group_2 =  unique(count$group_2))
      
    }
    
    missing_periods$catch <- 0
    
    count <- rbind(count, missing_periods)
    
    count <- count[order(count$date), ]
    
    
    plot <- ggplot2::ggplot(data = count, ggplot2::aes(x = date, y = catch, fill = if (!is.null(group)) group_1 else NULL )) + 
      ggplot2::geom_col() + 
      ggplot2::labs(y = if (value == "count" & convert_to_tons == TRUE) "catch (tons)" else "catch") +
      ggplot2::scale_y_continuous(labels = if (value == "percent") scales::percent else waiver()) +
      fishset_theme
    
    if (!is.null(group)) {
      
      plot <- plot + ggplot2::guides(fill = ggplot2::guide_legend(title = group[1], 
                                                                  title.theme = ggplot2::element_text(
                                                                    size = 10),
                                                                  label.theme = ggplot2::element_text(
                                                                    size = 8))) +
        ggplot2::theme(legend.position = "bottom")
      
    }
    
    if (!is.null(group) & length(group) == 2) {
      
      plot <- plot + ggplot2::facet_wrap(~group_2, scales = "free_y")
      
    } 
    
    if (p %in% c("%a", "%A", "%b", "%B")) {
      
      plot <- plot + ggplot2::scale_x_discrete(breaks = levels(count$date))
      
    } else {
      
      plot <- plot + ggplot2::scale_x_continuous(breaks = if (length(unique(count$date)) <= 12){seq(1,length(unique(count$date)), by = 1)
      } else {seq(1, length(unique(count$date)), by = round(length(unique(count$date)) * 0.08))})
    }
    
  } else {
    
    if (is.null(group)) {
      
      count <- lapply(s, function(x) {
        stats::aggregate(dataset[[x]], 
                         by = list(format(date_parser(dataset[[t]]), p)), 
                         FUN = sum)
      })
      
    } else if (!is.null(group)) {
      
      if (length(group) == 1) {
        
        count <- lapply(s, function(x) {
          stats::aggregate(dataset[[x]], 
                           by = list(format(date_parser(dataset[[t]]), p),
                                     dataset[[group1]]), 
                           FUN = sum)
        })
        
      } else {
        
        count <- lapply(s, function(x) {
          stats::aggregate(dataset[[x]], 
                           by = list(format(date_parser(dataset[[t]]), p),
                                     dataset[[group1]],
                                     dataset[[group2]]), 
                           FUN = sum)
        })
      }
    }
    
    names(count) <- s
    
    count <- reshape2::melt(count)
    
    count$variable <- NULL
    
    if (is.null(group)) {
      
      colnames(count) <- c("date", "catch", "species")
      
    }
    
    if (!is.null(group)) {
      
      if (length(group) == 1) {
        
        colnames(count) <- c("date", "group_1", "catch", "species")
        
      } else {
        
        colnames(count) <- c("date", "group_1", "group_2", "catch", "species")
        
      }
    }
    
    if (convert_to_tons == TRUE) {
      
      count$catch <- count$catch/2000
      
    }
    
    if (value == "percent") {
      
      count[["catch"]] <- count[["catch"]] / sum(count[["catch"]])
      
    }
    
    if (p %in% c("%a", "%A", "%b", "%B")) { 
      
      count <- date_factorize(count, "date", p)
      
    } else {
      
      count$date <- as.integer(count$date)  
      
    }
    
    
    ind <- periods_list[[p]][which(!(periods_list[[p]] %in% unique(count$date)))]
    
    if (is.null(group)) {
      
      missing_periods <- expand.grid(date = ind, species = unique(count$species))
      
    } else if (!is.null(group) & length(group) == 1) {
      
      missing_periods <- expand.grid(date = ind, 
                                     group_1 = unique(count$group_1),
                                     species = unique(count$species))
      
    } else if (!is.null(group) & length(group) > 1) {
      
      missing_periods <- expand.grid(date = ind, 
                                     group_1 = unique(count$group_1), 
                                     group_2 =  unique(count$group_2),
                                     species = unique(count$species))
      
    }
    
    missing_periods$catch <- 0
    
    count <- rbind(count, missing_periods)
    
    count <- count[order(count$date), ]
    
    
    plot <- ggplot2::ggplot(data = count, ggplot2::aes(x = date, y = catch, fill = if (!is.null(group)) group_1 else NULL )) + 
      ggplot2::geom_col() + 
      ggplot2::labs(y = if (value == "count" & convert_to_tons == TRUE) "catch (tons)" else "catch") +
      ggplot2::scale_y_continuous(labels = if (value == "percent") scales::percent else waiver()) +
      fishset_theme
    
    if (!is.null(group)) {
      
      plot <- plot + ggplot2::guides(fill = ggplot2::guide_legend(title = group[1], 
                                                                  title.theme = ggplot2::element_text(
                                                                    size = 10),
                                                                  label.theme = ggplot2::element_text(
                                                                    size = 8))) +
        ggplot2::theme(legend.position = "bottom")
      
    }
    
    if (is.null(group) | (!is.null(group) & length(group) == 1)) {
      
      plot <- plot + ggplot2::facet_grid(species ~ ., scales = "free_y")
      
    }
    
    if (!is.null(group) & length(group) == 2) {
      
      plot <- plot + ggplot2::facet_grid(species ~ group_2, scales = "free_y")  
      
    }
    
    if (p %in% c("%a", "%A", "%b", "%B")) { 
      
      plot <- plot + ggplot2::scale_x_discrete(breaks = levels(count$date))
      
    } else {
      
      plot <- plot + ggplot2::scale_x_continuous(breaks = if (length(unique(count$date)) <= 12){seq(1, length(unique(count$date)), by = 1)} 
                                                 else {seq(1, length(unique(count$date)), by = round(length(unique(count$date)) * 0.08))}) }
    
  }
  
  #Log the function 
  
  species_catch_function <- list()
  species_catch_function$functionID <- "species_catch"
  species_catch_function$args <- c(dat, project, s, t, period, group, convert_to_tons, value, output)
  log_call(species_catch_function)
  
  # Save output
  
  save_table(count, project, "species_catch")
  
  save_plot(project, "species_catch")
  
  if (output == "table") {
    
    if(length(s) == 1 & is.null(group)) {
      
      count
      
    } else if (length(s) == 1 & !is.null(group)) {
      
      if (length(group) == 1) {
        
        count <- setNames(count, c("date", group1, "catch"))
        
        count
        
      } else {
        
        count <- setNames(count, c("date", group1, group2, "catch"))
        
        count
        
      }
      
    } else if (length(s) > 1 & is.null(group)) {
      
      count <- reshape2::dcast(count, date ~ species, value.var = "catch", fill = 0)
      
      count 
      
    } else if (length(s) > 1 & !is.null(group)) {
      
      if (length(group) == 1) {
        
        count <- reshape2::dcast(count, date + group_1 ~ species, value.var = "catch", fill = 0)
        
        names(count)[names(count) == "group_1"] <- group1
        
        count 
        
      } else {
        
        count <- reshape2::dcast(count, date + group_1 + group_2 ~ species, value.var = "catch", fill = 0)
        
        names(count)[names(count) == "group_1"] <- group1
        
        names(count)[names(count) == "group_2"] <- group2
        
        count 
        
      }
      
    }
    
  } else {
    
    plot 
  }
}
