#' Evaluate outliers in Data
#' 
#' \code{outlier_table()} returns a summary table which shows summary
#' statistics of a variable after applying several outlier filters. 
#'
#' @param dat Primary data containing information on hauls or trips.
#'   Table in the FishSET database contains the string 'MainDataTable'.
#' @param project String, name of project.
#' @param x Variable or column number in \code{dat} to check for outliers.
#' @param sd_val Optional. Number of standard deviations from mean defining 
#'    outliers. For example, \code{sd_val = 4} would mean values outside +/- 4 
#'    SD from the mean would be outliers.
#' @param log_fun Logical, whether to log function call (for internal use).
#' @importFrom stats quantile sd var median
#' @keywords outliers
#' @export outlier_table
#' @return Table for evaluating whether outliers may exist in the selected data 
#'   column.
#' @details Returns a table of summary statistics (mean, median, standard 
#'  deviation, minimum, maximum, number of NAs, and skew of the data) for 
#'  \code{x} after values outside the outlier measure have been removed. 
#'  Outlier measures include 5-95\% quantiles, 25-75\% quantiles, mean +/-2SD, 
#'  mean +/-3SD, median +/-2SD, and median +/-3SD. Only one variable can be 
#'  checked at a time. Table is saved to the Output folder.
#' @examples
#' \dontrun{
#' outlier_table(pollockMainDataTable, 'pollock', x = 'HAUL')
#' }
outlier_table <- function(dat, project, x, sd_val = NULL, log_fun = TRUE) {
  
  # Call in datasets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  # Check if x is not numeric, if not then throw error
  if (!is.numeric(dataset[[x]])) {
    stop("Data is not numeric.", call. = FALSE)
  }
  
  # Initialize empty dataframe
  emptyrow <- data.frame(N = 0, mean = NA, median = NA, SD = NA, 
                         min = NA, max = NA, NAs = NA, skew = NA)
  
  # Helper function for adding to output dataframe
  filledrow <- function(dat, x) {
    data.frame(N = length(temp[[x]]), 
               mean = round(mean(temp[[x]], na.rm = TRUE), 2), 
               median = round(stats::median(temp[[x]], na.rm = TRUE), 2),
               SD = round(stats::sd(temp[[x]], na.rm = TRUE), 2), 
               min = round(min(temp[[x]], na.rm = TRUE), 2), 
               max = round(max(temp[[x]], na.rm = TRUE), 2),
               NAs = sum(length(which(is.na(temp[[x]])))), 
               skew = round(skewness(temp[[x]], na.rm = TRUE), 2))
  }
  
  # Output table of summary statistics Row 1 No data removed
  dat.table <- data.frame(Vector = x, 
                          outlier_check = "None", 
                          N = length(dataset[[x]]), 
                          mean = round(mean(dataset[[x]], na.rm = TRUE), 2),
                          median = round(stats::median(dataset[[x]], na.rm = TRUE), 2), 
                          SD = round(sd(dataset[[x]], na.rm = TRUE), 2), 
                          min = round(min(dataset[[x]], na.rm = TRUE), 2), 
                          max = round(max(dataset[[x]], na.rm = TRUE), 2), 
                          NAs = sum(length(which(is.na(dataset[[x]])))), 
                          skew = round(skewness(dataset[[x]], na.rm = TRUE), 2))
  
  # Row 2 5-95% quantile
  temp <- dataset[
    dataset[[x]] < 
      stats::quantile(dataset[[x]], 0.95, na.rm = TRUE) & dataset[[x]] > 
      stats::quantile(dataset[[x]], 0.05, na.rm = TRUE), ]
  dat.table <- rbind(dat.table, 
                     data.frame(Vector = x, 
                                outlier_check = "5_95_quant", 
                                if (dim(temp)[1] == 0) { emptyrow } else { filledrow(temp, x)}
                     ))
  
  # Row 3 25-75% quantile
  temp <- dataset[
    dataset[[x]] < 
      quantile(dataset[[x]], 0.75, na.rm = TRUE) & dataset[[x]] > 
      quantile(dataset[[x]], 0.25, na.rm = TRUE), ]
  dat.table <- rbind(dat.table, 
                     data.frame(Vector = x, outlier_check = "25_75_quant", 
                                if (dim(temp)[1] == 0) { emptyrow } else { filledrow(temp, x)}
                     ))
  
  # Row 4 Mean +/2SD
  temp <- dataset[
    dataset[[x]] < 
      (mean(dataset[[x]], na.rm = TRUE) + 2 * stats::sd(dataset[[x]], na.rm = TRUE)) & 
      dataset[[x]] > 
      (mean(dataset[[x]], na.rm = TRUE) - 2 * stats::sd(dataset[[x]], na.rm = TRUE)), ]
  dat.table <- rbind(dat.table, 
                     data.frame(Vector = x, outlier_check = "mean_2SD", 
                                if (dim(temp)[1] == 0) { emptyrow} else { filledrow(temp, x)}
                     ))
  
  # Row 5 Mean +/3SD
  temp <- dataset[
    dataset[[x]] < 
      (mean(dataset[[x]], na.rm = TRUE) + 3 * stats::sd(dataset[[x]], na.rm = TRUE)) & 
      dataset[[x]] > 
      (mean(dataset[[x]], na.rm = TRUE) - 3 * stats::sd(dataset[[x]], na.rm = TRUE)), ]
  dat.table <- rbind(dat.table, 
                     data.frame(Vector = x, outlier_check = "mean_3SD", 
                                if (dim(temp)[1] == 0) { emptyrow } else { filledrow(temp, x)}
                     ))
  
  # Row 6 Median +/-2SD
  temp <- dataset[
    dataset[[x]] < 
      (stats::median(dataset[[x]], na.rm = TRUE) + 2 * stats::sd(dataset[[x]], na.rm = TRUE)) & 
      dataset[[x]] > 
      (stats::median(dataset[[x]], na.rm = TRUE) - 2 * stats::sd(dataset[[x]], na.rm = TRUE)), ]
  dat.table <- rbind(dat.table, 
                     data.frame(Vector = x, outlier_check = "median_2SD", 
                                if (dim(temp)[1] == 0) { emptyrow} else { filledrow(temp, x)}
                     ))
  
  # Row 7 Median +/-3SD
  temp <- dataset[
    dataset[[x]] < 
      (stats::median(dataset[[x]], na.rm = TRUE) + 3 * stats::sd(dataset[[x]], na.rm = TRUE)) & 
      dataset[[x]] > 
      (stats::median(dataset[[x]], na.rm = TRUE) - 3 * stats::sd(dataset[[x]], na.rm = TRUE)), ]
  dat.table <- rbind(dat.table, 
                     data.frame(Vector = x, outlier_check = "median_3SD", 
                                if (dim(temp)[1] == 0) { emptyrow} else {filledrow(temp, x)}
                     ))
  
  if(!is.null(sd_val) & is.numeric(sd_val)){
    temp <- dataset[
      dataset[[x]] < 
        (mean(dataset[[x]], na.rm = TRUE) + sd_val * stats::sd(dataset[[x]], na.rm = TRUE)) & 
        dataset[[x]] > 
        (mean(dataset[[x]], na.rm = TRUE) - sd_val * stats::sd(dataset[[x]], na.rm = TRUE)), ]
    dat.table <- rbind(dat.table, 
                       data.frame(Vector = x, outlier_check = paste0("mean_",sd_val,"SD"), 
                                  if (dim(temp)[1] == 0) {emptyrow} else {filledrow(temp, x)}
                       ))
  }
  
  if (log_fun) {
    outlier_table_function <- list()
    outlier_table_function$functionID <- "outlier_table"
    outlier_table_function$args <- list(dat, project, x, log_fun)
    
    log_call(project, outlier_table_function)
  }
  
  save_table(dat.table, project, "outlier_table")
  
  return(dat.table)
}


#' Evaluate outliers in plot format
#'
#' Visualize spread of data and measures to identify outliers.
#' @param dat Primary data containing information on hauls or trips.
#'   Table in the FishSET database contains the string 'MainDataTable'.
#' @param project String, name of project.
#' @param x Variable in \code{dat} to check for outliers.
#' @param dat.remove Outlier measure. Values outside the measure are removed. 
#'   Users can use the predefined values (see below) or user-defined distance 
#'   from the mean. For user-defined values, \code{dat.remove} should be a numeric 
#'   value. For example, \code{dat.remove = 6} would would result in value outside 
#'   6SD from the mean being class as outliers. User-defined standard deviations 
#'   from the mean can also be applied using \code{sd_val}. Pre-defined choices: 
#'   \code{"none"}, \code{"5_95_quant"}, \code{"25_75_quant"}, \code{"mean_2SD"}, 
#'   \code{"median_2SD"}, \code{"mean_3SD"}, \code{"median_3SD"}. See the 
#'   \emph{Details} section for more information.
#'@param sd_val Optional. Number of standard deviations from mean defining outliers. 
#'    Example, \code{sd_val = 6} would mean values outside +/- 6 SD from the mean 
#'    would be outliers.
#' @param x.dist Distribution of the data. Choices include: \code{"normal"}, 
#'   \code{"lognormal"}, \code{"exponential"}, \code{"Weibull"}, \code{"Poisson"}, 
#'   \code{"negative binomial"}.
#' @param date (Optional) date variable to group the histogram by year.
#' @param group (Optional) additional variable to group the histogram by.
#' @param pages Whether to output plots on a single page (\code{"single"}, the 
#'    default) or multiple pages (\code{"multi"}). 
#' @param output.screen Logical, if true, return plots to the screen. If \code{FALSE}, 
#'   returns plot to the 'output' folder as a png file.
#' @param log_fun Logical, whether to log function call (for internal use).
#' @keywords outlier
#' @importFrom stats density dnorm dpois dweibull rnorm dbinom dlnorm dexp dnbinom
#' @importFrom gridExtra grid.arrange
#' @importFrom grid textGrob gpar
#' @importFrom rlang sym
#' @import ggplot2
#' @details  The function returns three plots: the data, a probability plot, 
#'  and a Q-Q plot. The \emph{data plot} returns \code{x} against row number. 
#'  Red points are data points that would be removed based on \code{dat.remove}. 
#'  Blue points are data points within the bounds of \code{dat.remove}. If 
#'  \code{dat.remove} is \code{"none"}, then only blue points will be shown.
#'  The \emph{probability plot} is a histogram of the data, after applying 
#'  \code{dat.remove}, with the fitted probability distribution based on 
#'  \code{x.dist}. \code{group} groups the histogram by a variable from \code{dat}, 
#'  \code{date} groups the histogram by year. The \emph{Q-Q plot} plots are 
#'  sampled quantiles against theoretical quantiles, after applying \code{dat.remove}. 
#'  \cr\cr
#'  The \code{dat.remove} choices are:
#'  \itemize{
#'  \item{numeric value: Remove data points outside +/- `x`SD of the mean}
#'  \item{none:          No data points are removed}
#'  \item{5_95_quant:    Removes data points outside the 5th and 95th quantiles}
#'  \item{25_75_quant:   Removes data points outside the 25th and 75th quantiles}
#'  \item{mean_2SD:      Removes data points outside +/- 2SD of the mean}
#'  \item{median_2SD:    Removes data points outside +/- 2SD of the median}
#'  \item{mean_3SD:      Removes data points outside +/- 3SD of the mean}
#'  \item{median_3SD:    Removes data points outside +/- 3SD of the median}
#'  }
#'  The distribution choices are:
#'  \itemize{
#'   \item{normal}
#'    \item{lognormal}
#'    \item{exponential}
#'    \item{Weibull}
#'    \item{Poisson}
#'    \item{negative binomial}
#'  }
#' @export outlier_plot
#' @return Plot of the data
#' @examples
#' \dontrun{
#' 
#' outlier_plot(pollockMainDataTable, 'pollock', x = 'Haul', dat.remove = 'mean_2SD', 
#'              x.dist = 'normal', output.screen = TRUE)
#' # user-defined outlier        
#' outlier_plot(pollockMainDataTable, 'pollock', x = 'Haul', dat.remove = 6, 
#'              x.dist = 'lognormal', output.screen = TRUE)
#' }
outlier_plot <- function(dat, project, x, dat.remove = 'none', sd_val = NULL, 
                         x.dist = 'normal', date = NULL, group = NULL, 
                         pages = "single", output.screen = FALSE, log_fun = TRUE) {
  
  # Call in datasets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  if (!is.numeric(dataset[[x]])) {
    stop("Data is not numeric.", call. = FALSE)
  }
  
  if (!is.null(date)) {
    dataset[[date]] <- date_parser(dataset[[date]])
    dataset$YEAR <- format(dataset[[date]], "%Y")
  }
  
  if (!is.null(group)) {
    dataset[[group]] <- as.factor(dataset[[group]])
  }
  
  if(!is.null(sd_val) & is.numeric(sd_val)){
    dat.remove <- sd_val
  }
  
  # Begin outlier check
  dataset$row_num <- 1:length(dataset[[x]])
  
  if (dat.remove == "none") {
    dat_sub <- dataset
    
  } else {
    data_mean <- mean(dataset[[x]], na.rm = TRUE)
    data_median <- stats::median(dataset[[x]], na.rm = TRUE)
    data_sd <- stats::sd(dataset[[x]], na.rm = TRUE)
    
    if(is.numeric(dat.remove)){
      dat_sub <- 
        dataset[dataset[[x]] < (data_mean + dat.remove * data_sd) & 
                  dataset[[x]] > (data_mean - dat.remove* data_sd), ]
    } else {
      if (dat.remove == "5_95_quant") {
        data_95 <- stats::quantile(dataset[[x]], 0.95, na.rm = TRUE)
        data_05 <- stats::quantile(dataset[[x]], 0.05, na.rm = TRUE)
        dat_sub <- dataset[dataset[[x]] < data_95 & dataset[[x]] > data_05, ]
        
      } else if (dat.remove == "25_75_quant") {
        data_75 <- stats::quantile(dataset[[x]], 0.75, na.rm = TRUE)
        data_25 <- stats::quantile(dataset[[x]], 0.25, na.rm = TRUE)
        dat_sub <- dataset[dataset[[x]] < data_75 & dataset[[x]] > data_25, ]
        
      } else if (dat.remove == "mean_2SD") {
        dat_sub <- dataset[dataset[[x]] < (data_mean + 2 * data_sd) &
                             dataset[[x]] > (data_mean - 2 * data_sd), ]
        
      } else if (dat.remove == "median_2SD") {
        dat_sub <- dataset[dataset[[x]] < (data_median + 2 * data_sd) &
                             dataset[[x]] > (data_median - 2 * data_sd), ]
        
      } else if (dat.remove == "mean_3SD") {
        dat_sub <- dataset[dataset[[x]] < (data_mean + 3 * data_sd) &
                             dataset[[x]] > (data_mean - 3 * data_sd), ]
        
      } else if (dat.remove == "median_3SD") {
        dat_sub <- dataset[dataset[[x]] < (data_median + 3 * data_sd) &
                             dataset[[x]] > (data_median - 3 * data_sd), ]
      }
    }
  }
  
  tmp_theme <- fishset_theme() + 
    ggplot2::theme(axis.text = ggplot2::element_text(size = 9), 
                   axis.title = ggplot2::element_text(size = 8))
  
  # Data spread plot
  data_spread_plot <- ggplot2::ggplot() +
    ggplot2::geom_point(data = dataset, 
                        aes(x = row_num, y = .data[[x]]),
                        color = "tomato") +
    ggplot2::geom_point(data = dat_sub, 
                        aes(x = row_num, y = .data[[x]]),
                        color = "royalblue") +
    ggplot2::labs(x = "Data row") +
    ggplot2::scale_x_continuous(expand = expansion(mult = c(0.01, 0.05))) +
    ggplot2::scale_y_continuous(expand = expansion(mult = c(0.01, 0.05))) +
    tmp_theme
  
  # Histogram
  x_sym <- rlang::sym(x)
  
  year_sym <- if (is.null(date)) NULL else rlang::sym("YEAR")
  
  group_sym <- if (is.null(group)) NULL else rlang::sym(group)
  
  fill_sym <- if (is.null(year_sym)) "gray" else year_sym
  
  hist_plot <- ggplot2::ggplot(dat_sub, ggplot2::aes(!!x_sym))
  
  # Add bars to histogram
  if (is.null(year_sym)) {
    hist_plot <-
      hist_plot + 
      ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), 
                              fill = "gray", 
                              color = "black",  
                              na.rm = TRUE, 
                              bins = 30) +
      ggplot2::scale_x_continuous(expand = expansion(mult = c(0.01, 0.05))) +
      ggplot2::scale_y_continuous(expand = expansion(mult = c(0.01, 0.05))) +
      tmp_theme
    
  } else {
    hist_plot <-
      hist_plot + 
      ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density), fill = !!fill_sym), 
                              na.rm = TRUE, 
                              bins = 30) +
      ggplot2::scale_x_continuous(expand = expansion(mult = c(0.01, 0.05))) +
      ggplot2::scale_y_continuous(expand = expansion(mult = c(0.01, 0.05))) +
      mytheme
  }
  
  # Add probability density line
  if (x.dist == "normal") {
    hist_plot <- 
      hist_plot + 
      ggplot2::stat_function(fun = dnorm,
                             colour = "royalblue", 
                             args = list(mean = mean(dat_sub[[x]], na.rm = TRUE), 
                                         sd = sd(dat_sub[[x]], na.rm = TRUE)))
    
  } else if (x.dist == "lognormal") {
    # lognormal
    hist_plot <- 
      hist_plot + 
      ggplot2::stat_function(fun = dlnorm, 
                             colour = "royalblue", 
                             args = list(mean = mean(log(dat_sub[[x]]), na.rm = TRUE), 
                                         sd = sd(log(dat_sub[[x]]), na.rm = TRUE)))
    
  } else if (x.dist == "exponential") {
    # Exponential
    hist_plot <- 
      hist_plot + 
      ggplot2::stat_function(fun = dexp, 
                             colour = "royalblue", 
                             args = list(rate = 1 / mean(dat_sub[[x]], na.rm = TRUE)))
    
  } else if (x.dist == "weibull") {
    # Weibull
    hist_plot <- 
      hist_plot + 
      ggplot2::stat_function(fun = dweibull, 
                             colour = "royalblue", 
                             args = list(shape = 1.2 / sqrt(var(log(dat_sub[[x]]), na.rm = TRUE)), 
                                         scale = mean(dat_sub[[x]], na.rm = TRUE) + 0.572 / 
                                           (1.2 / sqrt(var(log(dat_sub[[x]]), na.rm = TRUE)))))
    
  } else if (x.dist == "poisson") {
    # Poisson
    hist_plot <- 
      hist_plot + 
      ggplot2::stat_function(fun = dpois, 
                             colour = "royalblue", 
                             args = list(lambda = mean(dat_sub[[x]], na.rm = TRUE)))
    
  } else if (x.dist == "negative binomial") {
    # Negative Binomial
    dat_sub_mean <- mean(dat_sub[[x]], na.rm = TRUE)
    dat_sub_var <- var(dat_sub[[x]], na.rm = TRUE)
    hist_plot <- 
      hist_plot + 
      ggplot2::stat_function(fun = dnbinom, 
                             colour = "royalblue", 
                             args = list(dat_sub_mean^2 / (dat_sub_var - dat_sub_mean), 
                                         mu = dat_sub_mean))
  }
  
  # Probability plot
  quants <- seq(0, 1, length = length(dat_sub[[x]]) + 2)[2:(length(dat_sub[[x]]) + 1)]
  # normal
  if (x.dist == "normal") {
    fit_quants <- 
      stats::qnorm(quants, mean(dat_sub[[x]], na.rm = TRUE), sd(dat_sub[[x]], na.rm = TRUE))
    
  } else if (x.dist == "lognormal") {
    # lognormal
    fit_quants <- 
      stats::qlnorm(quants, 
                    mean = mean(log(dat_sub[[x]]), na.rm = TRUE), 
                    sd = sd(log(dat_sub[[x]]), na.rm = TRUE))
    
  } else if (x.dist == "exponential") {
    # Exponential
    fit_quants <- 
      stats::qexp(quants, rate = 1 / mean(dat_sub[[x]], na.rm = TRUE))
    
  } else if (x.dist == "weibull") {
    # Weibull
    fit_quants <- 
      stats::qweibull(quants,
                      shape = 1.2 / sqrt(var(log(dat_sub[[x]]), na.rm = TRUE)),
                      scale = mean(dat_sub[[x]], na.rm = TRUE) + 0.572 / 
                        (1.2 / sqrt(var(log(dat_sub[[x]]), na.rm = TRUE))))
    
  } else if (x.dist == "poisson") {
    # Poisson
    fit_quants <- 
      stats::qpois(quants, lambda = mean(dat_sub[[x]], na.rm = TRUE))
    
  } else if (x.dist == "negative binomial") {
    # Negative Binomial
    fit_quants <- 
      stats::qnbinom(quants,
                     size = mean(dat_sub[[x]], na.rm = TRUE)^2 / 
                       (var(dat_sub[[x]], na.rm = TRUE) - mean(dat_sub[[x]], na.rm = TRUE)),
                     mu = mean(dat_sub[[x]], na.rm = TRUE)
      )
  }
  
  data_quants <- stats::quantile(as.numeric(dat_sub[[x]]), quants, na.rm = TRUE)
  
  # create Q-Q plot
  temp <- data.frame(fit_quants, data_quants)
  
  if (!is.null(date)) temp$YEAR <- dat_sub$YEAR
  
  qq_plot <- 
    ggplot2::ggplot(temp, ggplot2::aes(x = fit_quants, y = data_quants)) +
    ggplot2::geom_point(shape = 1) +
    ggplot2::geom_abline() +
    ggplot2::labs(x = "Theoretical Quantiles", y = "Sample Quantiles", 
                  title = paste("Q-Q plot of", x.dist, "fit against data")) +
    tmp_theme
  
  if (is.numeric(dat.remove)) {
    
    p_title <- paste0("Plots for ", x, " with ", x.dist, 
                      " distribution and data removed based on '", 
                      dat.remove, "SD from the mean'. \nBlue: Included points",
                      "   Red: Removed points") 
    
  } else {
    
    p_title <- paste0("Plots for ", x, " with ", x.dist,
                      " distribution and data removed based on '",
                      dat.remove, "'. \nBlue: Included points   Red: Removed points")
  }
  
  if (pages == "single") {
    
    fig <- gridExtra::arrangeGrob(data_spread_plot, hist_plot,
                                  ncol = 2, 
                                  top = grid::textGrob(p_title, gp = grid::gpar(fontsize = 10)))
    
  } else {
    
    fig <- list(data_plot = data_spread_plot, 
                prob_plot = hist_plot)
  }
  
  # Log function
  if (log_fun) {
    outlier_plot_function <- list()
    outlier_plot_function$functionID <- "outlier_plot"
    outlier_plot_function$args <- list(dat, project, x, dat.remove, sd_val,
                                       x.dist, date, group, pages, output.screen, 
                                       log_fun)
    log_call(project, outlier_plot_function)
  }
  
  if (pages == "single") save_plot(project, "outlier_plot", fig)
  else save_nplot(project, "outlier_plot", fig, id = "name")
  
  if (output.screen) fig
  
  return(fig)
}


outlier_remove <- function(dat, project, x, dat.remove = "none", sd_val=NULL, over_write = FALSE) {
  #' Remove outliers from data table
  #'
  #' Remove outliers based on outlier measure.
  #' @param dat Primary data containing information on hauls or trips.
  #'   Table in the FishSET database contains the string 'MainDataTable'.
  #' @param project Project name. 
  #' @param x Variable in \code{dat} containing potential outliers.
  #' @param dat.remove Defines measure to subset the data. Users can use the 
  #'   predefined values (see below) or user-defined standard deviations from the 
  #'   mean. For user-defined values, \code{dat.remove} should be a numeric value. 
  #'   For example, \code{dat.remove=6} would would result in value outside 6SD 
  #'   from the mean being class as outliers. User-defined standard deviations 
  #'   from the mean can also be applied using \code{sd_val}.
  #'   Predefined choices: 
  #'   \code{"none"}, \code{"5_95_quant"}, \code{"25_75_quant"}, \code{"mean_2SD"}, 
  #'   \code{"median_2SD"}, \code{"mean_3SD"}, \code{"median_3SD"}.
  #' @param sd_val Optional. Number of standard deviations from mean defining 
  #'   outliers. For example, \code{sd_val=6} would mean values outside +/- 6 SD 
  #'   from the mean would be outliers.
  #' @param over_write Logical, If \code{TRUE}, saves data over previously saved 
  #'   data table in the FishSET database.
  #' @export outlier_remove
  #' @importFrom stats median quantile sd
  #' @importFrom DBI dbConnect dbWriteTable dbDisconnect
  #' @importFrom RSQLite SQLite
  #' @return Returns the modified primary dataset. Modified dataset will be saved 
  #'   to the FishSET database.
  #' @details   The \code{dat.remove} choices are:
  #'  \itemize{
  #'  \item{numeric value: Remove data points outside +/- `x`SD of the mean}
  #'  \item{none:        No data points are removed}
  #'  \item{5_95_quant:  Removes data points outside the 5th and 95th quantiles}
  #'  \item{25_75_quant: Removes data points outside the 25th and 75th quantiles}
  #'  \item{mean_2SD:    Removes data points outside +/- 2SD of the mean}
  #'  \item{median_2SD:  Removes data points outside +/- 2SD of the median}
  #'  \item{mean_3SD:    Removes data points outside +/- 3SD of the mean}
  #'  \item{median_3SD:  Removes data points outside +/- 3SD of the median}
  #'  }
  #' @examples
  #' \dontrun{
  #' pollockMainDataTable <- outlier_remove(pollockMainDataTable, 'pollock', 'dist', 
  #'    dat.remove = 'mean_2SD', save.output = TRUE)
  #' }
  
  # Call in datasets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  if (!is.numeric(dataset[[x]])) {
    stop("Data is not numeric. Outliers cannot be checked.")
  }
  
  if(!is.null(sd_val) & is.numeric(sd_val)){
    dat.remove <- sd_val
  }
  
  # Begin outlier check
  # summary stats for below calculations
  datmean <- mean(dataset[[x]], na.rm = TRUE)
  datmedian <- stats::median(dataset[[x]], na.rm = TRUE)
  datsd <- stats::sd(dataset[[x]], na.rm = TRUE)
  
  if(is.numeric(dat.remove)){
    dataset <- dataset[dataset[[x]] < (datmean + dat.remove * datsd) & 
                         dataset[[x]] > (datmean - dat.remove * datsd), ]
  } else {
    if (dat.remove == "none") {
      dataset <- dataset
      
    } else if (dat.remove == "5_95_quant") {
      dataset <- dataset[dataset[[x]] < stats::quantile(dataset[[x]], 0.95, na.rm = TRUE) & 
                           dataset[[x]] > stats::quantile(dataset[[x]], 0.05, na.rm = TRUE), ]
      
    } else if (dat.remove == "25_75_quant") {
      dataset <- dataset[dataset[[x]] < stats::quantile(dataset[[x]], 0.75, na.rm = TRUE) & 
                           dataset[[x]] > stats::quantile(dataset[[x]], 0.25, na.rm = TRUE), ]
      
    } else if (dat.remove == "mean_2SD") {
      dataset <- dataset[dataset[[x]] < (datmean + 2 * datsd) & 
                           dataset[[x]] > (datmean - 2 * datsd), ]
      
    } else if (dat.remove == "median_2SD") {
      dataset <- dataset[dataset[[x]] < (datmedian + 2 * datsd) & 
                           dataset[[x]] > (datmedian - 2 * datsd), ]
      
    } else if (dat.remove == "mean_3SD") {
      dataset <- dataset[dataset[[x]] < (datmean + 3 * datsd) & 
                           dataset[[x]] > (datmean - 3 * datsd), ]
      
    } else if (dat.remove == "median_3SD") {
      dataset <- dataset[dataset[[x]] < (datmedian + 3 * datsd) & 
                           dataset[[x]] > (datmedian - 3 * datsd), ]
    }
  }
  
  
  if (dat.remove != "none" & over_write == TRUE) {
    suppressWarnings(
      fishset_db <- DBI::dbConnect(RSQLite::SQLite(), 
                                   locdatabase(project = project))
    )
    on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
    
    DBI::dbWriteTable(fishset_db, deparse(substitute(dat)), dataset, overwrite = over_write)
  }
  
  outlier_remove_function <- list()
  outlier_remove_function$functionID <- "outlier_remove"
  outlier_remove_function$args <- c(dat, project, x, dat.remove, over_write)
  outlier_remove_function$kwargs <- list()
  outlier_remove_function$output <- c("")
  outlier_remove_function$msg <- paste("outliers removed using", dat.remove)
  log_call(project, outlier_remove_function)
  
  return(dataset)
}


outlier_boxplot <- function(dat, project, x = NULL) {
  #' Boxplot to assess outliers
  #' @param dat Primary data containing information on hauls or trips.
  #'   Table in the FishSET database contains the string 'MainDataTable'.
  #' @param project Project name. 
  #' @param x Variables in \code{dat} to check for outliers. Leave as 
  #'   \code{x = NULL} to plot all numeric variables. To specify multiple variables 
  #'   use \code{c('var1', 'var2')}
  #' @details Creates a visual representation of five summary statistics: 
  #'   median, two hinges (first and third quartiles), two whiskers (extends to 
  #'   1.5*IQR where IQR is the distance between the first and third quartiles.
  #'   "Outlying" points, those beyond the two whiskers (1.5*IQR) are shown 
  #'   individually.
  #' @return Box and whisker plot for all numeric variables. Saved to `output` folder.
  #' @export
  #' @import ggplot2
  #' @importFrom tidyr pivot_longer
  
  variable <- value <- id <- NULL
  
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  dataset$id <- 1:nrow(dataset)
  
  if(is.null(x)){
    num_cols <- qaqc_helper(dataset, is.numeric, output = 'names')   
  } else {
    if(!all(x %in% names(dataset))){
      stop('Variables in `x` do not match variables names in data table')
    }
    num_cols <- qaqc_helper(dataset[c(x, 'id')], is.numeric, output = 'names')   
  }
  
  ddm <- tidyr::pivot_longer(dataset[num_cols], 
                             cols = -id, 
                             names_to = 'variable', 
                             values_to = 'value')
  
  p <- 
    ggplot2::ggplot(ddm) +
    ggplot2::geom_boxplot(ggplot2::aes(x = variable, y = value)) +
    fishset_theme() + 
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1))
  
  outlier_boxplot_function <- list()
  outlier_boxplot_function$functionID <- "outlier_boxplot"
  outlier_boxplot_function$args <- list(dat, project, x)
  log_call(project, outlier_boxplot_function)
  
  save_plot(project, "outlier_boxplot", p)
  print(p)
}
