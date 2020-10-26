# Outlier check functions.

outlier_table <- function(dat, project, x) {
  #' Evaluate outliers in a table format
  #'
  #' @param dat Primary data containing information on hauls or trips.
  #'   Table in the FishSET database contains the string 'MainDataTable'.
  #' @param project String, name of project.
  #' @param x Variable or column number in \code{dat} to check for outliers.
  #' @importFrom stats quantile sd var na.pass model.matrix
  #' @importFrom utils file_test
  #' @importFrom grDevices dev.off pdf
  #' @keywords outliers
  #' @export outlier_table
  #' @return Table for evaluating whether outliers may exist in the selected data column.
  #' @details The returned table provides summary statistics (mean, median, standard deviation, minimum,
  #'  maximum, number of NAs, and skew of the data) for the data based on each method to remove
  #'  outliers (data outside the 5 to 95 percent quantiles, data outside the 25-75\% quantile,
  #'  data outside mean +/-2SD, data outside the mean +/-3SD, data outside the median +/-2SD,
  #'  data outside the median +/-3SD).
  #' @examples
  #' \dontrun{
  #' outlier_table(pollockMainDataTable, 'pollock', 'HAUL')
  #' }


  # Call in datasets
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset


  x.name <- x

  emptyrow <- data.frame(N = 0, mean = NA, median = NA, SD = NA, min = NA, max = NA, NAs = NA, skew = NA)

  filledrow <- function(dat, x) {
    data.frame(N = length(temp[, x]), mean = round(mean(temp[, x], na.rm = T), 2), median = round(stats::median(temp[, x], na.rm = T), 2),
               SD = round(stats::sd(temp[ ,x], na.rm = T), 2), min = round(min(temp[, x], na.rm = T), 2), 
               max = round(max(temp[, x], na.rm = T), 2), NAs = sum(length(which(is.na(temp[ ,x])))), skew = round(skewness(temp[, x], na.rm = T), 2))
  }
  # numeric. Cannot check outliers if not.
  if (is.numeric(dataset[, x])) {
    # Output table of summary statistics Row 1 No data removed
    dat.table <- data.frame(Vector = x.name, outlier_check = "None", N = length(dataset[, x]), mean = round(mean(dataset[, x], na.rm = T), 2),
                            median = round(stats::median(dataset[ ,x], na.rm = T), 2), SD = round(sd(dataset[, x], na.rm = T), 2), 
                            min = round(min(dataset[, x], na.rm = T), 2), max = round(max(dataset[, x], na.rm = T), 2), 
                            NAs = sum(length(which(is.na(dataset[, x])))), skew = round(skewness(dataset[, x], na.rm = T), 2))
    # Row 2 5-95% quantile
    temp <- dataset[dataset[, x] < stats::quantile(dataset[, x], 0.95, na.rm = TRUE) & dataset[, x] > stats::quantile(dataset[, x], 0.05, na.rm = TRUE), ]
    dat.table <- rbind(dat.table, data.frame(Vector = x.name, outlier_check = "5_95_quant", if (dim(temp)[1] == 0) {
      emptyrow
    } else {
      filledrow(temp, x)
    }))
    # Row 3 25-75% quantile
    temp <- dataset[dataset[, x] < quantile(dataset[, x], 0.75, na.rm = TRUE) & dataset[, x] > quantile(dataset[, x], 0.25, na.rm = TRUE), ]
    dat.table <- rbind(dat.table, data.frame(Vector = x.name, outlier_check = "25_75_quant", if (dim(temp)[1] == 0) {
      emptyrow
    } else {
      filledrow(temp, x)
    }))
    # Row 4 Mean +/2SD
    temp <- dataset[dataset[, x] < (mean(dataset[, x], na.rm = T) + 2 * stats::sd(dataset[, x], na.rm = T)) & dataset[, x] > (mean(dataset[, x], na.rm = T) -
      2 * stats::sd(dataset[, x], na.rm = T)), ]
    dat.table <- rbind(dat.table, data.frame(Vector = x.name, outlier_check = "mean_2SD", if (dim(temp)[1] == 0) {
      emptyrow
    } else {
      filledrow(temp, x)
    }))
    # Row 5 Mean +/3SD
    temp <- dataset[dataset[, x] < (mean(dataset[, x], na.rm = T) + 3 * stats::sd(dataset[, x], na.rm = T)) & dataset[, x] > (mean(dataset[, x], na.rm = T) -
      3 * stats::sd(dataset[, x], na.rm = T)), ]
    dat.table <- rbind(dat.table, data.frame(Vector = x, outlier_check = "mean_3SD", if (dim(temp)[1] == 0) {
      emptyrow
    } else {
      filledrow(temp, x)
    }))
    # Row 6 Median +/-2SD
    temp <- dataset[dataset[, x] < (stats::median(dataset[, x], na.rm = T) + 2 * stats::sd(dataset[, x], na.rm = T)) & dataset[, x] > (stats::median(dataset[
      ,
      x
    ], na.rm = T) - 2 * stats::sd(dataset[, x], na.rm = T)), ]
    dat.table <- rbind(dat.table, data.frame(Vector = x.name, outlier_check = "median_2SD", if (dim(temp)[1] == 0) {
      emptyrow
    } else {
      filledrow(temp, x)
    }))
    # Row 7 Median +/-3SD
    temp <- dataset[dataset[, x] < (stats::median(dataset[, x], na.rm = T) + 3 * stats::sd(dataset[, x], na.rm = T)) & dataset[, x] > (stats::median(dataset[
      ,
      x
    ], na.rm = T) - 3 * stats::sd(dataset[, x], na.rm = T)), ]
    dat.table <- rbind(dat.table, data.frame(Vector = x.name, outlier_check = "median_3SD", if (dim(temp)[1] == 0) {
      emptyrow
    } else {
      filledrow(temp, x)
    }))
    return(dat.table)
  } else {
    print("Data is not numeric.")
  }

  outlier_table_function <- list()
  outlier_table_function$functionID <- "outlier_table"
  outlier_table_function$args <- list(dat, project, x)

  log_call(outlier_table_function)
  
  if(is.numeric(dataset[,x])){
    save_table(dat.table, project, "outlier_table")
  } else  {
    cat('Table not available. Data is not numeric.')
  }
}

## ---------------------------##
outlier_plot <- function(dat, project, x, dat.remove, x.dist, output.screen = FALSE) {
  #' Evaluate outliers
  #'
  #' Visualize spread of data and impact of outlier removal options.
  #' @param dat Primary data containing information on hauls or trips.
  #'   Table in the FishSET database contains the string 'MainDataTable'.
  #' @param project String, name of project.
  #' @param x Variable in \code{dat} to check for outliers.
  #' @param dat.remove Defines method to subset the data. Choices include: \code{"none"}, \code{"5_95_quant"},
  #' \code{"25_75_quant"}, \code{"mean_2SD"}, \code{"median_2SD"}, \code{"mean_3SD"}, \code{"median_3SD"}.
  #'    See the \emph{Details} section for more information.
  #' @param x.dist Distribution of the data. Choices include: \code{"normal"}, \code{"lognormal"},
  #'  \code{"exponential"}, \code{"Weibull"}, \code{"Poisson"}, \code{"negative binomial"}.
  #' @param output.screen Logical, if true, return plots to the screen. If false, returns plot to the 'inst/output' folder as a png file.
  #' @keywords outlier
  #' @importFrom graphics points
  #' @importFrom stats dnorm dpois dweibull rnorm dbinom dlnorm dexp dnbinom
  # @importFrom ggpubr annotate_figure text_grob
  #' @importFrom  ggplot2 ggplot geom_point aes_string theme geom_histogram labs aes geom_abline geom_abline
  #' @details  The function returns three plots: the data, a probability plot, and a Q-Q plot. The data plot is the value of
  #'  \code{x} against row number. Red points are all the data without any points removed.
  #'  The blue points are the subset of the data. If \code{dat.remove} is \code{"none"}, then only blue points will be shown.
  #'  The probability plot is a histogram of the data with the fitted probability distribution
  #'  based on \code{x.dist}. The Q-Q plot plots are sampled quantiles against theoretical quantiles. \cr\cr
  #'  The \code{dat.remove} choices are:
  #'  \itemize{
  #'  \item{none:        No data points are removed}
  #'  \item{5_95_quant:  Removes data points outside the 5th and 95th quantiles}
  #'  \item{25_75_quant: Removes data points outside the 25th and 75th quantiles}
  #'  \item{mean_2SD:    Removes data points outside +/- 2SD of the mean}
  #'  \item{median_2SD:  Removes data points outside +/- 2SD of the median}
  #'  \item{mean_3SD:    Removes data points outside +/- 3SD of the mean}
  #'  \item{median_3SD:  Removes data points outside +/- 3SD of the median}
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
  #' outlier_plot(pollockMainDataTable, 'Haul', dat.remove = 'mean_2SD', 
  #'    x.dist = 'normal', output.screen = TRUE)
  #' }


  # Call in datasets
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset

  if (is.numeric(dataset[, x]) == T) {
    # Begin outlier check
    dataset$y <- 1:length(dataset[, x])
    if (dat.remove == "none") {
      dataset <- dataset
      dat_sub <- dataset
    } else {
      if (dat.remove == "5_95_quant") {
        dat_sub <- dataset[dataset[, x] < stats::quantile(dataset[, x], 0.95, na.rm = TRUE) &
          dataset[, x] > stats::quantile(dataset[, x], 0.05, na.rm = TRUE), ]
      } else if (dat.remove == "25_75_quant") {
        dat_sub <- dataset[dataset[, x] < stats::quantile(dataset[, x], 0.75, na.rm = TRUE) &
          dataset[, x] > stats::quantile(dataset[, x], 0.25, na.rm = TRUE), ]
      } else if (dat.remove == "mean_2SD") {
        dat_sub <- dataset[dataset[, x] < (mean(dataset[, x], na.rm = T) + 2 * stats::sd(dataset[, x], na.rm = T)) &
          dataset[, x] > (mean(dataset[, x], na.rm = T) - 2 * stats::sd(dataset[, x], na.rm = T)), ]
      } else if (dat.remove == "median_2SD") {
        dat_sub <- dataset[dataset[, x] < (stats::median(dataset[, x], na.rm = T) + 2 * stats::sd(dataset[, x], na.rm = T)) &
          dataset[, x] > (stats::median(dataset[, x], na.rm = T) - 2 * stats::sd(dataset[, x], na.rm = T)), ]
      } else if (dat.remove == "mean_3SD") {
        dat_sub <- dataset[dataset[, x] < (mean(dataset[, x], na.rm = T) + 3 * stats::sd(dataset[, x], na.rm = T)) &
          dataset[, x] > (mean(dataset[, x], na.rm = T) - 3 * stats::sd(dataset[, x], na.rm = T)), ]
      } else if (dat.remove == "median_3SD") {
        dat_sub <- dataset[dataset[, x] < (stats::median(dataset[, x], na.rm = T) + 3 * stats::sd(dataset[, x], na.rm = T)) &
          dataset[, x] > (stats::median(dataset[, x], na.rm = T) - 3 * stats::sd(dataset[, x], na.rm = T)), ]
      }
    } # End Outlier mod

    # graphics::par(mar=c(4,4,4,4)) graphics::par(mfrow = c(2, 2)) points


    mytheme <- ggplot2::theme(
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),
      axis.text = element_text(size = 9), axis.title = element_text(size = 8)
    )
    ## Plot 1!
    distplot <- function(gdf, first, second) {
      ggplot2::ggplot(gdf, ggplot2::aes_string(x = first, y = second)) +
        ggplot2::geom_point(color = "red", na.rm = TRUE) +
        ggplot2::geom_point(
          data = dat_sub,
          ggplot2::aes_string(x = first, y = second), color = "blue", na.rm = TRUE
        ) +
        ggplot2::labs(x = "Data row") +
        mytheme
    }
    p1 <- distplot(dataset, "y", x)


    # Hist Plot 2!
    p2 <- ggplot2::ggplot(dat_sub, aes_string(x)) +
      ggplot2::geom_histogram(ggplot2::aes(y = dat_sub$..density..),
        na.rm = TRUE,
        bins = if (nrow(dataset) < 500) {
          round(nrow(dataset) / 2)
        } else {
          250
        }
      ) +
      mytheme
    if (x.dist == "normal") {
      p2 <- p2 + ggplot2::stat_function(
        fun = dnorm, colour = "blue",
        args = list(mean = mean(dat_sub[, x], na.rm = TRUE), sd = sd(dat_sub[, x], na.rm = TRUE))
      )
    } else if (x.dist == "lognormal") {
      # lognormal
      p2 <- p2 + ggplot2::stat_function(fun = dlnorm, colour = "blue", args = list(mean = mean(log(dat_sub[, x]), na.rm = TRUE), sd = sd(log(dat_sub[
        ,
        x
      ]), na.rm = TRUE)))
    } else if (x.dist == "exponential") {
      # Exponential
      p2 <- p2 + ggplot2::stat_function(fun = dexp, colour = "blue", args = list(rate = 1 / mean(dat_sub[, x], na.rm = TRUE)))
    } else if (x.dist == "weibull") {
      # Weibull
      p2 <- p2 + ggplot2::stat_function(fun = dweibull, colour = "blue", args = list(shape = 1.2 / sqrt(var(log(dat_sub[, x]), na.rm = TRUE)), scale = mean(dat_sub[
        ,
        x
      ], na.rm = TRUE) + 0.572 / (1.2 / sqrt(var(log(dat_sub[, x]), na.rm = TRUE)))))
    } else if (x.dist == "poisson") {
      # Poisson
      p2 <- p2 + ggplot2::stat_function(fun = dpois, colour = "blue", args = list(lambda = mean(dat_sub[, x], na.rm = TRUE)))
    } else if (x.dist == "negative binomial") {
      # Negative Binomial
      p2 <- p2 + ggplot2::stat_function(fun = dnbinom, colour = "blue", args = list(mean(dat_sub[, x], na.rm = TRUE)^2 / (var(dat_sub[, x], na.rm = TRUE) -
        mean(dat_sub[, x], na.rm = TRUE)), mu = mean(dat_sub[, x], na.rm = TRUE)))
    }

    # Plot3 Probability plot
    quants <- seq(0, 1, length = length(dat_sub[, x]) + 2)[2:(length(dat_sub[, x]) + 1)]
    # normal
    if (x.dist == "normal") {
      fit_quants <- stats::qnorm(quants, mean(dat_sub[, x], na.rm = TRUE), sd(dat_sub[, x], na.rm = TRUE))
    } else if (x.dist == "lognormal") {
      # lognormal
      fit_quants <- stats::qlnorm(quants, mean = mean(log(dat_sub[, x]), na.rm = TRUE), sd = sd(log(dat_sub[, x]), na.rm = TRUE))
    } else if (x.dist == "exponential") {
      # Exponential
      fit_quants <- stats::qexp(quants, rate = 1 / mean(dat_sub[, x], na.rm = TRUE))
    } else if (x.dist == "weibull") {
      # Weibull
      fit_quants <- stats::qweibull(quants,
        shape = 1.2 / sqrt(var(log(dat_sub[, x]), na.rm = TRUE)),
        scale = mean(dat_sub[, x], na.rm = TRUE) + 0.572 / (1.2 / sqrt(var(log(dat_sub[, x]), na.rm = TRUE)))
      )
    } else if (x.dist == "poisson") {
      # Poisson
      fit_quants <- stats::qpois(quants, lambda = mean(dat_sub[, x], na.rm = TRUE))
    } else if (x.dist == "negative binomial") {
      # Negative Binomial
      fit_quants <- stats::qnbinom(quants,
        size = mean(dat_sub[, x], na.rm = TRUE)^2 / (var(dat_sub[, x], na.rm = TRUE) - mean(dat_sub[, x], na.rm = TRUE)),
        mu = mean(dat_sub[, x], na.rm = TRUE)
      )
    }

    data_quants <- stats::quantile(as.numeric(dat_sub[, x]), quants, na.rm = TRUE)
    # create Q-Q plot
    temp <- data.frame(fit_quants, data_quants)
    p3 <- ggplot2::ggplot(temp, ggplot2::aes(x = fit_quants, y = data_quants)) +
      ggplot2::geom_point(shape = 1) +
      ggplot2::geom_abline() +
      ggplot2::labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = paste("Q-Q plot of", x.dist, "fit against data")) +
      mytheme

    # Put it all together
    fig <- suppressWarnings(ggpubr::ggarrange(p1, p2, p3, ncol = 2, nrow = 2))
    # labels = c('A', 'B', 'C'),
    fig <- ggpubr::annotate_figure(fig, top = ggpubr::text_grob(paste(
      "Plots for ", x, " with ", x.dist, " distribution and data removed based on '",
      dat.remove, "'. \nBlue: included points   Red: removed points"
    ), size = 10))

    # Log function
    outlier_plot_function <- list()
    outlier_plot_function$functionID <- "outlier_plot"
    outlier_plot_function$args <- list(dat, project, x, dat.remove, x.dist, output.screen)
    log_call(outlier_plot_function)


    plot(fig)
    # Close the pdf file
    if (output.screen == FALSE) {
      save_plot(project, "outlier_plot", fig)
    }
  } else {
    # Actions to take if data is not numeric
    print("Data is not numeric. Plots not generated.")
  }
}

## ---------------------------##
outlier_remove <- function(dat, x, dat.remove = "none", remove = T, over_write = FALSE) {
  #' Remove outliers from dataset
  #'
  #' Remove outliers based on method.
  #' @param dat Primary data containing information on hauls or trips.
  #'   Table in the FishSET database contains the string 'MainDataTable'.
  #' @param x Variable in \code{dat} containing potential outliers.
  #' @param dat.remove Defines method to subset the data. Choices include: \code{"none"},
  #'    \code{"5_95_quant"}, \code{"25_75_quant"}, \code{"mean_2SD"}, \code{"median_2SD"}, \code{"mean_3SD"}, \code{"median_3SD"}.
  #' @param remove Save data with outliers removed. If TRUE, the revised data table,
  #'    with values removed outside the \code{dat.remove} expression, is returned.
  #' @param over_write Logical, If TRUE, saves data over previously saved data table in the FishSET database.
  #' @export outlier_remove
  #' @return Returns the modified primary dataset. Modified dataset will be saved to the FishSET database.
  #' @details   The \code{dat.remove} choices are:
  #'  \itemize{
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
  #' pollockMainDataTable <- outlier_remove(pollockMainDataTable, 'dist', 
  #'    dat.remove = 'mean_2SD', save.output = TRUE)
  #' }

  # Call in datasets
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset


  if (is.numeric(dataset[, x]) == T) {
    # Begin outlier check
    if (remove == TRUE) {
      # log actions


      if (dat.remove == "none") {
        dataset <- dataset
      } else if (dat.remove == "5_95_quant") {
        dataset <- dataset[dataset[, x] < stats::quantile(dataset[, x], 0.95, na.rm = TRUE) & dataset[, x] > stats::quantile(dataset[, x], 0.05,
          na.rm = TRUE
        ), ]
      } else if (dat.remove == "25_75_quant") {
        dataset <- dataset[dataset[, x] < stats::quantile(dataset[, x], 0.75, na.rm = TRUE) & dataset[, x] > stats::quantile(dataset[, x], 0.25,
          na.rm = TRUE
        ), ]
      } else if (dat.remove == "mean_2SD") {
        dataset <- dataset[dataset[, x] < (mean(dataset[, x], na.rm = T) + 2 * stats::sd(dataset[, x], na.rm = T)) & dataset[, x] > (mean(dataset[
          ,
          x
        ], na.rm = T) - 2 * stats::sd(dataset[, x], na.rm = T)), ]
      } else if (dat.remove == "median_2SD") {
        dataset <- dataset[dataset[, x] < (stats::median(dataset[, x], na.rm = T) + 2 * stats::sd(dataset[, x], na.rm = T)) & dataset[, x] >
          (stats::median(dataset[, x], na.rm = T) - 2 * stats::sd(dataset[, x], na.rm = T)), ]
      } else if (dat.remove == "mean_3SD") {
        dataset <- dataset[dataset[, x] < (mean(dataset[, x], na.rm = T) + 3 * stats::sd(dataset[, x], na.rm = T)) & dataset[, x] > (mean(dataset[
          ,
          x
        ], na.rm = T) - 3 * stats::sd(dataset[, x], na.rm = T)), ]
      } else if (dat.remove == "median_3SD") {
        dataset <- dataset[dataset[, x] < (stats::median(dataset[, x], na.rm = T) + 3 * stats::sd(dataset[, x], na.rm = T)) & dataset[, x] >
          (stats::median(dataset[, x], na.rm = T) - 3 * stats::sd(dataset[, x], na.rm = T)), ]
      }


      if (dat.remove != "none" & over_write == "TRUE") {
        suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
        DBI::dbWriteTable(fishset_db, deparse(substitute(dat)), dataset, overwrite = over_write)
        DBI::dbDisconnect(fishset_db)
      }
      outlier_remove_function <- list()
      outlier_remove_function$functionID <- "outlier_remove"
      outlier_remove_function$args <- c(dat, deparse(substitute(x)), dat.remove, remove, over_write)
      outlier_remove_function$kwargs <- list()
      outlier_remove_function$output <- c("")
      outlier_remove_function$msg <- paste("outliers removed using", dat.remove)
      log_call(outlier_remove_function)

      return(dataset)
    } # End Outlier check
    if (remove == FALSE) {
      return(dataset)
      print("No modifications made.")
    }
  } else {
    outlier_remove_function <- list()
    outlier_remove_function$functionID <- "outlier_remove"
    outlier_remove_function$args <- list(dat, x, dat.remove, remove, over_write)

    log_call(outlier_remove_function)
    # Actions to take if data is not numeric
    print("Data is not numeric. Outliers cannot be checked.")
  }
}
