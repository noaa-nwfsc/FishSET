#' Weekly catch
#' 
#' Catch total by week
#'
#' @param dat Main data frame over which to apply function. Table in FishSET 
#'   database should contain the string `MainDataTable`.
#' @param project name of project.
#' @param species A variable containing the species catch or a vector
#'   of species variables.
#' @param date A time variable containing dates to aggregate by.
#' @param year A year or vector of years to subset the data by. Plot output is 
#'   faceted by year if multiple years are given. 
#' @param group Up to two categorical variables to group by. For plots, if only one 
#'   species is entered the first group variable is passed to "fill" and the second 
#'   group variable to "facet_grid". If multiple species are entered the species 
#'   variable is passed to "fill" and the first group variable is faceted row-wise 
#'   and the second group variable (if entered) column-wise. If multiple years are 
#'   entered, plots are faceted row-wise by year as well.
#' @param fun Name of function (wrapped in quotes) to aggregate by. Defaults to \code{\link{sum}}. 
#' @param position Bar positioning for plot. Options include "stack", "dodge", 
#'   and "fill". 
#' @param convert_to_tons Whether to convert catch weight to tons. 
#' @param value Whether raw count or percentage of catch should be calculated. 
#' @param output Whether a table or plot should be generated.
#' @param format_tab How table output should be formated. Options include "wide" 
#'   (the default) and "long".  
#' @examples 
#' \dontrun{
#' weekly_catch("pollockMainDataTable", species = c("HAUL_LBS_270_POLLOCK_LBS", 
#' "HAUL_LBS_110_PACIFIC_COD_LBS",  "HAUL_LBS_OTHER_LBS"), date = "DATE_FISHING_BEGAN", 
#' convert_to_tons = T, year = 2011, output = "plot")
#' }
#' @export weekly_catch
#' @importFrom lubridate is.Date
#' @importFrom stats aggregate reformulate
#' @importFrom reshape2 melt dcast
#' @import ggplot2


weekly_catch <- function(dat, project, species, date, year = NULL, group = NULL, fun = "sum", position = "stack",
                         convert_to_tons = FALSE, value = c("count", "percent"), output = c("plot", "table"), format_tab = "wide"){
  
  # Call in datasets
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset
  
  if (lubridate::is.Date(dataset[[date]]) == FALSE) {
    
    warning("Date variable is not in correct format. Converting to type 'date'.")
    
    dataset[[date]] <- date_parser(dataset[[date]])
  }
  
  dataset$week <- format(dataset[[date]], "%U")
  dataset$Year <- format(dataset[[date]], "%Y")
  
  y_val <- year
  
  if (!is.null(year)) { 
    
    dataset <- subset(dataset, Year %in% y_val)
  }
  
  if (!is.null(group)) {
    
    group1 <- group[1]
    
    dataset[[group1]] <- as.factor(dataset[[group1]])
    
    if (length(group) == 2) {
      
      group2 <- group[2]
      
      dataset[[group2]] <- as.factor(dataset[[group2]])
      
    } else if (length(group) > 2) {
      
      warning("Too many grouping variables included, selecting first two.")
    }
  }
  
  if (is.null(group)) {
    
    count <- stats::aggregate(dataset[species], by = list(dataset$Year, dataset$week), FUN = sum)
    
    count <- setNames(count, c("Year", "week", species))
    
  } else if (!is.null(group) & length(group) == 1) {
    
    count <- stats::aggregate(dataset[species], by = list(dataset$Year, 
                                                          dataset$week,
                                                          dataset[[group1]]), 
                              FUN = match.fun(fun))
    
    count <- setNames(count, c("Year", "week", group1, species))
    
  } else {
    
    count <- stats::aggregate(dataset[species], by = list(dataset$Year, 
                                                          dataset$week,
                                                          dataset[[group1]],
                                                          dataset[[group2]]), 
                              FUN = match.fun(fun))
    
    count <- setNames(count, c("Year", "week", group1, group2, species))
  }
  
  if (length(species) > 1) {
    
    count <- reshape2::melt(count, measure.vars = species, variable.name = "species", value.name = "catch")
    
    count$species <- factor(count$species, 
                            levels = unique(count$species[order(count$catch, count$Year, count$species)]), 
                            ordered = TRUE)
  } else {
    
    names(count)[names(count) == species] <- "catch"
  }
  
  if (convert_to_tons == TRUE) {
    
    count$catch <- count$catch/2000
  }
  
  if (value == "percent") {
    
    count$catch <- count$catch / sum(count$catch)
  }
  
  count$week <- as.integer(count$week)
  count$Year <- as.integer(count$Year)
  
  ind <- periods_list[["%U"]][which(!(periods_list[["%U"]] %in% unique(count$week)))]
  
  if (is.null(group)) {
    
    if (length(species) == 1) {
      
      missing_periods <- expand.grid(week = ind, Year = unique(count$Year))
      
    } else {
      
      missing_periods <- expand.grid(week = ind, 
                                     Year = unique(count$Year),
                                     species = unique(count$species))
    }
    
  } else if (!is.null(group) & length(group) == 1) {
    
    if (length(species) == 1) {
      
      missing_periods <- expand.grid(week = ind,
                                     Year = unique(count$Year),
                                     group1 = unique(count[[group1]]))
      
      missing_periods <- setNames(missing_periods, c("week", "Year", group1))
      
    } else {
      
      missing_periods <- expand.grid(week = ind, 
                                     Year = unique(count$Year),
                                     group1 = unique(count[[group1]]),
                                     species = unique(count$species))
      
      missing_periods <- setNames(missing_periods, c("week", "Year", group1, "species"))
    }
    
  } else if (!is.null(group) & length(group) > 1) {
    
    if (length(species) == 1) {
      
      missing_periods <- expand.grid(week = ind,
                                     Year = unique(count$Year),
                                     group1 = unique(count[[group1]]), 
                                     group2 = unique(count[[group2]]))
      
      missing_periods <- setNames(missing_periods, c("week", "Year", group1, group2))
      
    } else {
      
      missing_periods <- expand.grid(week = ind, 
                                     Year = unique(count$Year),
                                     group1 = unique(count[[group1]]),
                                     group2 = unique(count[[group2]]),
                                     species = unique(count$species))
      
      missing_periods <- setNames(missing_periods, c("week", "Year", group1, group2, "species"))
    }
  }
  
  if (nrow(missing_periods) > 0) {
    
    missing_periods$catch <- 0
    
    count <- rbind(count, missing_periods)
  }
  
  count <- count[order(count$Year, count$week), ]
  
  if (length(species) == 1) {
    
    plot <- ggplot2::ggplot(count, ggplot2::aes_string("week", "catch", fill = if (!is.null(group)) group1 else NULL)) + 
      ggplot2::geom_col(position = position) +
      fishset_theme + 
      ggplot2::labs(title = if (!is.null(year) & length(year) == 1) paste(year) else NULL,
                    y = paste(fun, "catch")) +
      ggplot2::theme(plot.title = element_text(hjust = 0.5)) +
      ggplot2::scale_x_continuous(breaks = seq(0, 50, by = 10))
    
    if (!is.null(group)) {
      
      plot <- plot + ggplot2::guides(fill = ggplot2::guide_legend(title = group[1], 
                                                                  title.theme = ggplot2::element_text(
                                                                    size = 10),
                                                                  label.theme = ggplot2::element_text(
                                                                    size = 8))) +
        ggplot2::theme(legend.position = "bottom")
    }
    
    if (!is.null(group) & length(group) > 1) {
      
      if (is.null(year) | (!is.null(year) & length(year) == 1)) {
        
        plot <- plot + ggplot2::facet_grid(stats::reformulate(".", group2), scales = "free_y")
        
      } else if (!is.null(year) & length(year) > 1) {
        
        plot <- plot + ggplot2::facet_grid(stats::reformulate(".", paste("Year +", group2)), scales = "free_y")
      } 
      
    } else {
      
      if (is.null(year) | (!is.null(year) & length(year) == 1)) {
        
        plot <- plot 
        
      } else if (length(year) > 1) {
        
        plot <- plot + ggplot2::facet_grid(Year ~ ., scales = "free_y")
      }
    }
    
  } else if (length(species) > 1) {
    
    plot <- ggplot2::ggplot(count, ggplot2::aes(week, catch, fill = species)) + 
      ggplot2::geom_col(position = if (position == "dodge") ggplot2::position_dodge2(preserve = "single") else position,
                        col = "black") +
      fishset_theme + 
      ggplot2::labs(title = if (!is.null(year) & length(year) == 1) paste(year) else NULL,
                    y = paste(fun, "catch")) +
      ggplot2::theme(plot.title = element_text(hjust = 0.5)) +
      ggplot2::scale_x_continuous(breaks = seq(0, 50, by = 10)) +
      ggplot2::theme(legend.position = "bottom", 
                     legend.box.background = ggplot2::element_rect(color = "black", 
                                                                   size = 1, 
                                                                   linetype = 1)) +
      ggplot2::guides(fill = ggplot2::guide_legend(title = NULL, 
                                                   title.theme = ggplot2::element_text(
                                                     size = 10),
                                                   label.theme = ggplot2::element_text(
                                                     size = 8))) +
      ggplot2::theme(legend.position = "bottom")
    
    if (!is.null(group) & length(group) == 1) {
      
      if (is.null(year) | (!is.null(year) & length(year) == 1)) {
        
        plot <- plot + ggplot2::facet_grid(stats::reformulate(".", group1), scales = "free_y")
        
      } else if (!is.null(year) & length(year) > 1) {
        
        plot <- plot + ggplot2::facet_grid(stats::reformulate(".", paste("Year +", group1)), scales = "free_y")
      } 
      
    } else if (!is.null(group) & length(group) > 1) {
      
      if (is.null(year) | (!is.null(year) & length(year) == 1)) {
        
        plot <- plot + ggplot2::facet_grid(stats::reformulate(group2, group1), scales = "free_y",
                                           labeller = ggplot2::labeller(.cols = label_both))
        
      } else if (!is.null(year) & length(year) > 1) {
        
        plot <- plot + ggplot2::facet_grid(stats::reformulate(group2, paste("Year +", group1)), scales = "free_y",  
                                           labeller = ggplot2::labeller(.cols = label_both))
      } 
      
    } else {
      
      if (is.null(year) | (!is.null(year) & length(year) == 1)) {
        
        plot <- plot 
        
      } else if (length(year) > 1) {
        
        plot <- plot + ggplot2::facet_grid(Year ~ ., scales = "free_y")
      }
    } 
  }
  
  weekly_catch_function <- list()
  weekly_catch_function$functionID <- "weekly_catch"
  weekly_catch_function$args <- c(dat, project, species, date, year, group, fun, position, convert_to_tons, output, format_tab)
  log_call(weekly_catch_function)
  
  # Save plot
  save_plot(project, "weekly_catch", plot)
  
  names(count)[names(count) == "Year"] <- "year"
  
  if (output == "table") {
    
    if (length(species) == 1) {
      
      names(count)[names(count) == "catch"] <- species
      
    } else {
      
      if (format_tab == "wide") {
        
        count <- reshape2::dcast(count, ... ~ species, value.var = "catch", 
                                 fill = 0, fun.aggregate = match.fun(fun))
      }
    }
    # Save table
    save_table(count, project, "weekly_catch")
    
    count
    
  } else {
    
    plot
  }
}

