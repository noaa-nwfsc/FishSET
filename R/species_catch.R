#' Catch by Species
#'
#' Creates a table or plot of total species catch by period
#' 
#' @param dat Main data frame over which to apply function. Table in FishSET 
#'   database should contain the string `MainDataTable`.
#' @param project Name of project.
#' @param species A variable containing the species catch or a vector
#'   of species variables (in pounds).
#' @param date Variable containing dates to aggregate by.
#' @param period Time period to count by. Options include "year", "year_abv", 
#'   "month", "month_abv", "month_num", "weeks" (weeks in the year), 
#'   "weekday", "weekday_abv", "weekday_num", "day" (day of the month), 
#'   and "day_of_year".
#' @param fun Name of function to aggregate by. Defaults to \code{\link[base]{sum}}. 
#' @param group Up to two categorical variables to group by. For plots, if only one 
#'   species is entered the first group variable is passed to "fill" and second is 
#'   passed to "facet_grid". If multiple species are entered the species variable 
#'   is faceted row-wise and the second group variable column-wise. If multiple 
#'   years are entered, plots are faceted row-wise by year as well.
#' @param year A year or vector of years to subset the data by. Plot output is 
#'   faceted by year if multiple years are given.     
#' @param convert_to_tons A logical value indicating whether catch variable should 
#'   be converted to tons.
#' @param value Whether to return raw count or percentage of catch. 
#' @param output Whether function should create a table or plot.
#' @param position Positioning of bar plot. Options include "stack", "dodge", 
#'   and "fill". 
#' @param format_tab How table output should be formated. Options include "wide" 
#'   (the default) and "long".
#' @return Returns a table or plot of the total species catch by period.
#' @examples 
#' \dontrun{
#' species_catch("pollockMainDataTable", species = c("HAUL_LBS_270_POLLOCK_LBS", 
#' "HAUL_LBS_110_PACIFIC_COD_LBS", "HAUL_LBS_OTHER_LBS"), date = "HAUL_DATE", 
#' value = "count", period = "month_num", output = "plot", year = 2011, convert_to_tons = TRUE)
#' }
#' @export species_catch
#' @import ggplot2
#' @importFrom stats aggregate reformulate
#' @importFrom reshape2 dcast melt
#' @importFrom scales percent


species_catch <- function(dat, project, species, date, period = "month_abv", fun = "sum", group = NULL, year = NULL, 
                          convert_to_tons = TRUE, value = c("count", "percent"), 
                          output = c("table", "plot"), position = "stack", format_tab = c("wide", "long")){
  
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
    
    count <- stats::aggregate(dataset[species], 
                              by = list(dataset$Year, 
                                        format(date_parser(dataset[[date]]), p)), 
                              FUN = match.fun(fun))
    
    names(count) <- c("Year", period, species)
    
  } else if (!is.null(group)) {
    
    if (length(group) == 1) {
      
      count <- stats::aggregate(dataset[species], 
                                by = list(dataset$Year, 
                                          format(date_parser(dataset[[date]]), p), 
                                          dataset[[group1]]), 
                                FUN = match.fun(fun))
      
      names(count) <- c("Year", period, group1, species)
      
    } else {
      
      count <- stats::aggregate(dataset[species], 
                                by = list(dataset$Year, 
                                          format(date_parser(dataset[[date]]), p), 
                                          dataset[[group1]],
                                          dataset[[group2]]), 
                                FUN = match.fun(fun))
      
      names(count) <- c("Year", period, group1, group2, species)
    }
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
  
  if (p %in% c("%a", "%A", "%b", "%B")) { 
    
    count <- date_factorize(count, period, p)
    
  } else {
    
    count[[period]] <- as.integer(count[[period]])
  }
  
  ind <- periods_list[[p]][which(!(periods_list[[p]] %in% unique(count[[period]])))]
  
  if (is.null(group)) {
    
    if (length(species) == 1) {
      
      missing_periods <- expand.grid(period = ind, Year = unique(count$Year))
      names(missing_periods)[names(missing_periods) == "period"] <- period
      
    } else {
      
      missing_periods <- expand.grid(period = ind, 
                                     Year = unique(count$Year),
                                     species = unique(count$species))
      
      names(missing_periods)[names(missing_periods) == "period"] <- period
    }
    
  } else if (!is.null(group) & length(group) == 1) {
    
    if (length(species) == 1) {
      
      missing_periods <- expand.grid(period = ind,
                                     Year = unique(count$Year),
                                     group1 = unique(count[[group1]]))
      
      missing_periods <- setNames(missing_periods, c(period, "Year", group1))
      
    } else {
      
      missing_periods <- expand.grid(period = ind, 
                                     Year = unique(count$Year),
                                     group1 = unique(count[[group1]]),
                                     species = unique(count$species))
      
      missing_periods <- setNames(missing_periods, c(period, "Year", group1, "species"))
    }
    
  } else if (!is.null(group) & length(group) > 1) {
    
    if (length(species) == 1) {
      
      missing_periods <- expand.grid(period = ind,
                                     Year = unique(count$Year),
                                     group1 = unique(count[[group1]]), 
                                     group2 = unique(count[[group2]]))
      
      missing_periods <- setNames(missing_periods, c(period, "Year", group1, group2))
      
    } else {
      
      missing_periods <- expand.grid(period = ind, 
                                     Year = unique(count$Year),
                                     group1 = unique(count[[group1]]),
                                     group2 = unique(count[[group2]]),
                                     species = unique(count$species))
      
      missing_periods <- setNames(missing_periods, c(period, "Year", group1, group2, "species"))
    }
  }
  
  if (nrow(missing_periods) > 0) {
    
    missing_periods$catch <- 0
    
    count <- rbind(count, missing_periods)
  }
  
  count <- count[order(count$Year, count[[period]]), ]
  
  if (length(species) == 1) {
    
    plot <- ggplot2::ggplot(data = count, ggplot2::aes_string(x = period, y = "catch", fill = if (!is.null(group)) group1 else NULL )) + 
      ggplot2::geom_col(position = position) + 
      ggplot2::scale_y_continuous(labels = if (value == "percent") scales::percent else waiver()) +
      fishset_theme +
      ggplot2::labs(title = if (!is.null(year) & length(year) == 1) paste(year) else NULL,
                    x = paste0(date," ", "(", period, ")"),
                    y = if (value == "count" & convert_to_tons == TRUE) paste(fun, "catch (tons)") else paste(fun, "catch (lbs)")) +
      ggplot2::theme(plot.title = element_text(hjust = 0.5)) 
    
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
    
    if (p %in% c("%a", "%A", "%b", "%B")) {
      
      plot <- plot + ggplot2::scale_x_discrete(breaks = levels(count[[period]]))
      
    } else {
      
      plot <- plot + ggplot2::scale_x_continuous(breaks = if (length(unique(count[[period]])) <= 12){seq(1,length(unique(count[[period]])), by = 1)
      } else {seq(1, length(unique(count[[period]])), by = round(length(unique(count[[period]])) * 0.08))})
    }
    
  } else if (length(species) > 1) {
    
    plot <- ggplot2::ggplot(data = count, ggplot2::aes_string(x = period, y = "catch", fill = if (!is.null(group)) group1 else NULL )) + 
      ggplot2::geom_col(position = if (position == "dodge") ggplot2::position_dodge2(preserve = "single") else position) + 
      ggplot2::scale_y_continuous(labels = if (value == "percent") scales::percent else waiver()) + 
      fishset_theme +
      ggplot2::labs(title = if (!is.null(year) & length(year) == 1) paste(year) else NULL,
                    x = paste0(date," ", "(", period, ")"),
                    y = if (value == "count" & convert_to_tons == TRUE) paste(fun, "catch (tons)") else paste(fun, "catch (lbs)")) +
      ggplot2::theme(plot.title = element_text(hjust = 0.5))
    
    if (!is.null(group)) {
      
      plot <- plot + ggplot2::guides(fill = ggplot2::guide_legend(title = group[1],
                                                                  title.theme = ggplot2::element_text(size = 10),
                                                                  label.theme = ggplot2::element_text(size = 8))) + 
        ggplot2::theme(legend.position = "bottom")
    }
    
    if (!is.null(group) & length(group) == 1) {
      
      if (is.null(year) | (!is.null(year) & length(year) == 1)) {
        
        plot <- plot + ggplot2::facet_grid(species ~ ., scales = "free_y")
        
      } else if (!is.null(year) & length(year) > 1) {
        
        plot <- plot + ggplot2::facet_grid(Year + species ~ ., scales = "free_y")
      } 
      
    } else if (!is.null(group) & length(group) > 1) {
      
      if (is.null(year) | (!is.null(year) & length(year) == 1)) {
        
        plot <- plot + ggplot2::facet_grid(stats::reformulate(group2, "species"), scales = "free_y", 
                                           labeller = labeller(.cols = label_both))
        
      } else if (!is.null(year) & length(year) > 1) {
        
        plot <- plot + ggplot2::facet_grid(stats::reformulate(group2, "species + Year"), scales = "free_y", 
                                           labeller = labeller(.cols = label_both))
      }
      
    } else if (is.null(group)) {
      
      if (is.null(year) | (!is.null(year) & length(year) == 1)) {
        
        plot <- plot + ggplot2::facet_grid(species ~ ., scales = "free_y")
        
      } else if (length(year) > 1) {
        
        plot <- plot + ggplot2::facet_grid(Year + species ~ ., scales = "free_y")
      }
    } 
    
    if (p %in% c("%a", "%A", "%b", "%B")) { 
      
      plot <- plot + ggplot2::scale_x_discrete(breaks = levels(count[[period]]))
      
    } else {
      
      plot <- plot + ggplot2::scale_x_continuous(breaks = if (length(unique(count[[period]])) <= 12){seq(1, length(unique(count[[period]])), by = 1)} 
                                                 else {seq(1, length(unique(count[[period]])), by = round(length(unique(count[[period]])) * 0.08))}) }
  }
  #Log the function 
  species_catch_function <- list()
  species_catch_function$functionID <- "species_catch"
  species_catch_function$args <- c(dat, project, species, date, period, fun, group, year, convert_to_tons, value, output, position, format_tab)
  log_call(species_catch_function)
  
  # Save output
  save_plot(project, "species_catch")
  
  names(count)[names(count) == "Year"] <- "year"
  
  if (output == "table") {
    
    if (length(species) > 1) {
      
      if (format_tab == "wide") {
        
        count <- reshape2::dcast(count, ... ~ species, value.var = "catch", fill = 0, 
                                 fun.aggregate = match.fun(fun))
      }
    }
    # Save table
    save_table(count, project, "species_catch")
    
    count
    
  } else {
    
    plot 
  }
}
