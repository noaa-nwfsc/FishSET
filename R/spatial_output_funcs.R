#' Histogram of latitude and longitude by grouping variable
spatial_hist <- function(dat, project, group=NULL) {
  #' @param dat Primary data containing information on hauls or trips.
  #' Table in FishSET database contains the string 'MainDataTable'.
  #' @param project String, name of project.
  #' @param group Column in \code{dat} containing grouping categories.
  #' @import ggplot2
  #' @importFrom tidyr gather
  #' @return Returns histogram of latitude and longitude by grouping variable.
  #'   Output returned to console and saved to Output folder.
  #' @details Returns a histogram of observed lat/lon split by grouping variable.
  #'   Output printed to console and saved to Output folder.  Function is used to
  #'   assess spatial variance/clumping of selected grouping variable.
  #' @export
  #' @examples
  #' \dontrun{
  #' spatial_hist(pollockMainDataTable, 'pollock', 'GEAR_TYPE')
  #' }

  requireNamespace("ggplot2")

  out <- data_pull(dat)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main")
  
  
  if(!is.null(group)){
  dataset <- dataset[, c(grep(group, names(dataset)), grep("lon|lat", names(dataset), ignore.case = TRUE))]
  melt.dat <- tidyr::gather(as.data.frame(dataset)) #reshape2::melt(dataset)
  plot_out <-  ggplot2::ggplot(melt.dat, aes(value, group = group, fill = group)) +
    ggplot2::geom_histogram(position = "identity", alpha = 0.5, binwidth = 0.25) +
    ggplot2::facet_wrap(~key, scales = "free") +
    ggplot2::scale_color_grey() +
    ggplot2::scale_fill_grey() +
    ggplot2::theme_classic()
  } else {
    dataset <- dataset[, c(grep("lon|lat", names(dataset), ignore.case = TRUE))]
    melt.dat <- tidyr::gather(as.data.frame(dataset))
    plot_out <- ggplot2::ggplot(melt.dat, aes(value)) +
      ggplot2::geom_histogram(position = "identity", alpha = 0.5, binwidth = 0.25) +
      ggplot2::facet_wrap(~key, scales = "free") +
      ggplot2::scale_color_grey() +
      ggplot2::scale_fill_grey() +
      ggplot2::theme_classic()
  }

  spatial_hist_function <- list()
  spatial_hist_function$functionID <- "spatial_hist"
  spatial_hist_function$args <- list(dat, project, group)
  spatial_hist_function$kwargs <- list()
  spatial_hist_function$output <- c()
  log_call(project, spatial_hist_function)

  save_plot(project, "spatial_hist")

  print(plot_out)
}


#' Summarize variable over data and time
spatial_summary <- function(dat, project, stat.var = c("length", "no_unique_obs", "perc_total", "mean", "median", "min", "max", "sum"),
                            variable, gridfile, lon.grid = NULL, lat.grid = NULL, 
                            lon.dat = NULL, lat.dat = NULL, cat) {
  #' @param dat Primary data containing information on hauls or trips.
  #'   Table in FishSET database contains the string 'MainDataTable'.
  #' @param project String, name of project.
  #' @param stat.var Options are \code{"length"}, \code{"no_unique_obs"}, \code{"perc_total"},
  #'   \code{"mean"}, \code{"median"}, \code{"min"}, \code{"max"}, and \code{"sum"}.
  #' @param gridfile Spatial data containing information on fishery management or regulatory zones.
  #'   Shape, json, geojson, and csv formats are supported. Leave as NULL if the variable ‘ZoneID’
  #'   assigning observations to zones exists in \code{dat}.
  #' @param variable Variable in \code{dat} to summarize over date and zone.
  #' @param lon.dat Longitude variable in \code{dat}. Leave as NULL if the variable ‘ZoneID’ (zonal 
  #'   assignment) exists in \code{dat}.
  #' @param lat.dat Latitude variable in \code{dat}. Leave as NULL if the variable ‘ZoneID’ (zonal 
  #'   assignments) exists in \code{dat}.
  #' @param lon.grid Variable or list from \code{gridfile} containing longitude data. Required for csv files. 
  #'   Leave as NULL if \code{gridfile} isba shape or json file or if the variable ‘ZoneID’ exists in \code{dat}.
  #' @param lat.grid Variable or list from \code{gridfile} containing latitude data. Required for csv files. Leave as NULL if \code{gridfile}
  #'   is a shape or json file, or if the variable ‘ZoneID’ exists in \code{dat}.
  #' @param cat  Variable or list in \code{gridfile} that identifies the individual areas or zones. If \code{gridfile} is class sf, \code{cat}
  #'   should be name of list containing information on zones. Leave as NULL if the variable ‘ZoneID’ exists in \code{dat}.
  #' @importFrom graphics par lines plot
  #' @export
  #' @description View summary and exploratory statistics of selected variable by date and zone.
  #' @details
  #' \code{stat.var} details:
  #' \tabular{rlll}{
  #' length: \tab Number of observations \cr
  #' no_unique_obs: \tab Number of unique observations \cr
  #' perc_total: \tab Percent of total observations \cr
  #' mean: \tab Mean \cr
  #' median: \tab  Median \cr
  #' min: \tab  Minimum\cr
  #' max: \tab Maximum \cr
  #' sum: \tab Sum \cr
  #' }
  #' @return Returns two plots, the variable aggregated by \code{stat.var} plotted against date and against zone.
  #' @examples
  #' \dontrun{
  #' Example where ZoneID exists in dataset
  #'    spatial_summary(pcodMainDataTable, project = 'pcod', 
  #'       stat.var = "no_unique_obs", variable = 'HAUL')
  #'
  #' Example where obs. have not been assigned to zones
  #'     spatial_summary(pcodMainDataTable, project = 'pcod', stat.var = "no_unique_obs",
  #'        variable = 'HAUL', gridfile = spatdat, lon.dat = 'MidLat', lat.dat = 'MidLat',
  #'        cat = 'NMFS_AREA')
  #' }

  out <- data_pull(dat)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main")
  
  if ("ZoneID" %in% names(dataset) == FALSE) {
    dataset <- assignment_column(
      dat = dataset, project=project, gridfile = gridfile, hull.polygon = TRUE, 
      lon.grid = lon.grid, lat.grid = lat.grid, lon.dat = lon.dat, lat.dat = lat.dat, 
      cat = cat, closest.pt = TRUE, epsg = NULL, log.fun = FALSE
    )
  }

  date.var <- grep("date", names(dataset), ignore.case = TRUE)[1]
  var <- grep(variable, names(dataset), ignore.case = TRUE)[1]

  if (stat.var == "mean") {
    lab <- paste("mean", names(dataset)[var])
  } else if (stat.var == "median") {
    lab <- paste("median", names(dataset)[var])
  } else if (stat.var == "min") {
    lab <- paste("min", names(dataset)[var])
  } else if (stat.var == "max") {
    lab <- paste("max", names(dataset)[var])
  } else if (stat.var == "sum") {
    lab <- paste("sum", names(dataset)[var])
  } else if (stat.var == "length") {
    lab <- paste("No. observations", names(dataset)[var])
  }
  graphics::par(mfrow = c(1, 2))
  
  if (stat.var == "no_unique_obs") {
    plotout <- graphics::plot(aggregate(dataset[[variable]], by = list(as.Date(dataset[, date.var])), 
                                        function(x) length(unique(x))), type = "l", lty = 1,
                              ylab = paste("No. unique obs", names(dataset)[var], "per day"), 
                              xlab = "Date")
    graphics::lines(aggregate(dataset[[variable]], by = list(as.Date(dataset[, date.var])), function(x) length(unique(x))))
    graphics::plot(aggregate(dataset[[variable]], by = list(as.factor(dataset$ZoneID)), 
                             function(x) length(unique(x))), type = "l", lty = 1, 
                   ylab = paste("No. unique obs", names(dataset)[var]), xlab = "Zone")
    lines(aggregate(dataset[[variable]], by = list(as.factor(dataset$ZoneID)), function(x) length(unique(x))))
  } else if (stat.var == "perc_total") {
    plotout <- plot(aggregate(dataset[[variable]], by = list(as.Date(dataset[, date.var])), 
                              function(x) length((x)) / length(dataset[[variable]]) * 100),
      type = "l", lty = 1,
      ylab = paste("Percent total", names(dataset)[var], "per day"), xlab = "Date"
    )
    lines(aggregate(dataset[[variable]], by = list(as.Date(dataset[, date.var])), 
                    function(x) length((x)) / length(dataset[[variable]]) * 100))
    plot(aggregate(dataset[[variable]], by = list(as.factor(dataset$ZoneID)),
                   function(x) length((x)) / length(dataset[[variable]]) * 100),
      type = "l",
      lty = 1, ylab = paste("Percent total", names(dataset)[var]), xlab = "Zone"
    )
    lines(aggregate(dataset[[variable]], by = list(as.factor(dataset$ZoneID)),
                    function(x) length((x)) / length(dataset[[variable]]) * 100))
  } else {
    plotout <- plot(aggregate(dataset[[variable]], by = list(as.Date(dataset[, date.var])), stat.var), 
                    type = "l", lty = 1, ylab = paste(lab, "per day"), xlab = "Date")
    lines(aggregate(dataset[[variable]], by = list(dataset[, date.var]), stat.var))
    plot(aggregate(dataset[[variable]], by = list(as.factor(dataset$ZoneID)), stat.var), type = "l", ylab = lab, xlab = "Zone")
    lines(aggregate(dataset[[variable]], by = list(as.factor(dataset$ZoneID)), stat.var))
  }

  spatial_summary_function <- list()
  spatial_summary_function$functionID <- "spatial_summary"
  spatial_summary_function$args <- list(dat, project, stat.var, variable, gridfile, lon.dat, lat.dat, cat, lon.grid, lat.grid)
  spatial_summary_function$kwargs <- list()
  spatial_summary_function$output <- c()
  log_call(project, spatial_summary_function)

  save_plot(project, "spatial_summary")
  print(plotout)
}
