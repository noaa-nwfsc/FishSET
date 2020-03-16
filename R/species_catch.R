#' Catch by Species
#'
#' Creates a table or plot of total species catch by period
#' 
#' @param dat Main data frame over which to apply function. Table in FishSET 
#'   database should contain the string `MainDataTable`.
#' @param project name of project.
#' @param s A variable containing the species catch or a vector
#'   of species variables (in pounds).
#' @param t time variable containing dates to aggregate by.
#' @param period Time period to count by. Options include "year", "year_abv", 
#'   "month", "month_abv", "month_num", "weeks" (weeks in the year), 
#'   "weekday", "weekday_abv", "weekday_num", "day" (day of the month), 
#'   and "day_of_year".
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
#' @param output table or plot.
#' @param position Positioning of bar plot. Options include "stack", "dodge", 
#'   and "fill". 
#' @param format_tab How table output should be formated. Options include "wide" 
#'   (the default) and "long".
#' @return Returns a table or plot of the total species catch by period.
#' @examples 
#' \dontrun{
#' species_catch("pollockMainDataTable", s = c("HAUL_LBS_270_POLLOCK_LBS", 
#' "HAUL_LBS_110_PACIFIC_COD_LBS", "HAUL_LBS_OTHER_LBS"), t = "HAUL_DATE", 
#' value = "count", period = "month_num", output = "plot", year = 2011, convert_to_tons = TRUE)
#' }
#' @export species_catch
#' @import ggplot2
#' @importFrom stats aggregate reformulate
#' @importFrom reshape2 dcast melt
#' @importFrom scales percent


species_catch <- function(dat, project, s, t, period = "month_abv", group = NULL, year = NULL, 
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
  
  dataset$years <- format(dataset[[t]], "%Y")
  
  y_val <- year
  
  if (!is.null(year)) { 
    
    dataset <- subset(dataset, years %in% y_val)
    
  }
  
  if (!is.null(group)){
    
    group1 <- group[1]
    
    dataset[[group1]] <- as.factor(dataset[[group1]])
    
    if (length(group) == 2){
      
      group2 <- group[2] 
      
      dataset[[group2]] <- as.factor(dataset[[group2]])
      
    } else if (length(group) > 2) {
      
      warning("Too many grouping variables included, selecting first two.")
      
    }
  }
  
  if (length(s) == 1) {
    
    if (is.null(group)) {
      
      count <- stats::aggregate(dataset[[s]], 
                                by = list(dataset$years, 
                                          format(date_parser(dataset[[t]]), p)), 
                                FUN = sum)
      
      names(count) <- c("years", period, "catch")
      
    } else if (!is.null(group)) {
      
      if(length(group) == 1) {
        
        count <- stats::aggregate(dataset[[s]], 
                                  by = list(dataset$years, 
                                            format(date_parser(dataset[[t]]), p), 
                                            dataset[[group1]]), 
                                  FUN = sum)
        
        names(count) <- c("years", period, group1, "catch")
        
      } else {
        
        count <- stats::aggregate(dataset[[s]], 
                                  by = list(dataset$years, 
                                            format(date_parser(dataset[[t]]), p), 
                                            dataset[[group1]],
                                            dataset[[group2]]), 
                                  FUN = sum)
        
        names(count) <- c("years", period, group1, group2, "catch")
        
      }
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
      
      missing_periods <- expand.grid(period = ind, years = unique(count$years))
      
      missing_periods <- setNames(missing_periods, c(period, "years"))
      
    } else if (!is.null(group) & length(group) == 1) {
      
      missing_periods <- expand.grid(period = ind,
                                     years = unique(count$years),
                                     group1 = unique(count[[group1]]))
      
      missing_periods <- setNames(missing_periods, c(period, "years", group1))
      
    } else if (!is.null(group) & length(group) > 1) {
      
      missing_periods <- expand.grid(period = ind,
                                     years = unique(count$years),
                                     group1 = unique(count[[group1]]), 
                                     group2 =  unique(count[[group2]]))
      
      missing_periods <- setNames(missing_periods, c(period, "years", group1, group2))
      
    }
    
    if (nrow(missing_periods) > 0) {
      
      missing_periods$catch <- 0
      
      count <- rbind(count, missing_periods)
      
    }
    
    count <- count[order(count$years, count[[period]]), ]
    
    
    plot <- ggplot2::ggplot(data = count, ggplot2::aes_string(x = period, y = "catch", fill = if (!is.null(group)) group1 else NULL )) + 
      ggplot2::geom_col(position = position) + 
      ggplot2::scale_y_continuous(labels = if (value == "percent") scales::percent else waiver()) +
      fishset_theme +
      ggplot2::labs(title = if (!is.null(year) & length(year) == 1) paste(year) else NULL,
                    x = paste0(t," ", "(", period, ")"),
                    y = if (value == "count" & convert_to_tons == TRUE) "catch (tons)" else "catch (lbs)") +
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
        
        plot <- plot + ggplot2::facet_grid(stats::reformulate(".", paste("years +", group2)), scales = "free_y")
        
      } 
      
    } else {
      
      if (is.null(year) | (!is.null(year) & length(year) == 1)) {
        
        plot <- plot 
        
      } else if (length(year) > 1) {
        
        plot <- plot + ggplot2::facet_grid(years ~ ., scales = "free_y")
        
      }
    } 
    
    if (p %in% c("%a", "%A", "%b", "%B")) {
      
      plot <- plot + ggplot2::scale_x_discrete(breaks = levels(count[[period]]))
      
    } else {
      
      plot <- plot + ggplot2::scale_x_continuous(breaks = if (length(unique(count[[period]])) <= 12){seq(1,length(unique(count[[period]])), by = 1)
      } else {seq(1, length(unique(count[[period]])), by = round(length(unique(count[[period]])) * 0.08))})
    }
    
  } else if (length(s) > 1) {
    
    if (is.null(group)) {
      
      count <- lapply(s, function(x) {
        stats::aggregate(dataset[[x]], 
                         by = list(dataset$years,
                                   format(date_parser(dataset[[t]]), p)), 
                         FUN = sum)
      })
      
    } else if (!is.null(group)) {
      
      if (length(group) == 1) {
        
        count <- lapply(s, function(x) {
          stats::aggregate(dataset[[x]], 
                           by = list(dataset$years,
                                     format(date_parser(dataset[[t]]), p),
                                     dataset[[group1]]), 
                           FUN = sum)
        })
        
      } else {
        
        count <- lapply(s, function(x) {
          stats::aggregate(dataset[[x]], 
                           by = list(dataset$years,
                                     format(date_parser(dataset[[t]]), p),
                                     dataset[[group1]],
                                     dataset[[group2]]), 
                           FUN = sum)
        })
      }
    }
    
    names(count) <- s
    
    count <- reshape2::melt(count,  id.vars = names(count[[1]]))
    
    count$variable <- NULL
    
    if (is.null(group)) {
      
      colnames(count) <- c("years", period, "catch", "species")
      
    }
    
    if (!is.null(group)) {
      
      if (length(group) == 1) {
        
        colnames(count) <- c("years", period, group1, "catch", "species")
        
      } else {
        
        colnames(count) <- c("years", period, group1, group2, "catch", "species")
        
      }
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
      
      missing_periods <- expand.grid(period = ind, 
                                     years = unique(count$years), 
                                     species = unique(count$species))
      
      missing_periods <- setNames(missing_periods, c(period, "years", "species"))
      
        missing_periods <- setNames(missing_periods, c(period, "years", "species"))
      
      missing_periods <- expand.grid(period = ind, 
                                     years = unique(count$years),
                                     group1 = unique(count[[group1]]),
                                     species = unique(count$species))
      
      missing_periods <- setNames(missing_periods, c(period, "years", group1, "species"))
      
    } else if (!is.null(group) & length(group) > 1) {
      
      missing_periods <- expand.grid(period = ind, 
                                     years = unique(count$years),
                                     group1 = unique(count[[group1]]), 
                                     group2 =  unique(count[[group2]]),
                                     species = unique(count$species))
      
      missing_periods <- setNames(missing_periods, c(period, "years", group1, group2, "species"))
      
    }
    
    if (nrow(missing_periods) > 0) {
      
      missing_periods$catch <- 0
      
      count <- rbind(count, missing_periods)
      
    }
    
    count <- count[order(count$years, count[[period]]), ]
    
    
    plot <- ggplot2::ggplot(data = count, ggplot2::aes_string(x = period, y = "catch", fill = if (!is.null(group)) group1 else NULL )) + 
      ggplot2::geom_col(position = if (position == "dodge") ggplot2::position_dodge2(preserve = "single") else position) + 
      ggplot2::scale_y_continuous(labels = if (value == "percent") scales::percent else waiver()) + 
      fishset_theme +
      ggplot2::labs(title = if (!is.null(year) & length(year) == 1) paste(year) else NULL,
                    x = paste0(t," ", "(", period, ")"),
                    y = if (value == "count" & convert_to_tons == TRUE) "catch (tons)" else "catch (lbs)") +
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
        
        plot <- plot + ggplot2::facet_grid(years + species ~ ., scales = "free_y")
        
      } 
      
    } else if (!is.null(group) & length(group) > 1) {
      
      if (is.null(year) | (!is.null(year) & length(year) == 1)) {
        
        plot <- plot + ggplot2::facet_grid(stats::reformulate(group2, "species"), scales = "free_y", 
                                           labeller = labeller(.cols = label_both))
        
      } else if (!is.null(year) & length(year) > 1) {
        
        plot <- plot + ggplot2::facet_grid(stats::reformulate(group2, "species + years"), scales = "free_y", 
                                           labeller = labeller(.cols = label_both))
        
      } 
      
    } else {
      
      if (is.null(year) | (!is.null(year) & length(year) == 1)) {
        
        plot <- plot 
        
      } else if (length(year) > 1) {
        
        plot <- plot + ggplot2::facet_grid(years + species ~ ., scales = "free_y")
        
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
  species_catch_function$args <- c(dat, project, s, t, period, group, year, convert_to_tons, value, output, position, format_tab)
  log_call(species_catch_function)
  
  # Save output
  save_plot(project, "species_catch")
  
  names(count)[names(count) == "years"] <- "year"
  
  if (output == "table") {
    
    if (length(s) > 1) {
      
      if (format_tab == "wide") {
        
        count <- reshape2::dcast(count, ... ~ species, value.var = "catch", fill = 0, 
                                 fun.aggregate = sum)
        
      }
    }
    # Save table
    save_table(count, project, "species_catch")
    
    count
    
  } else {
    
    plot 
  }
}
