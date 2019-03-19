#' Outlier check function

outlier_table <- function(dataset, x) {
  #' Evaluate outliers in a table output
  #'
  #' @param dataset dataframe or matrix
  #' @param x column in dataframe
  #' @importFrom stats quantile sd var na.pass
  #' @importFrom grDevices dev.off pdf  
  #' @keywords outliers
  #' @export outlier_table
  #' @return Returns table with quantiles for all numeric variables in the dataframe
  # @examples Generate data for example. Inject outliers into cars dataset.  
  # cars1 <- cbind(rbind(cars[1:30, ], data.frame(speed=c(19,19,20,20,20), dist=c(190, 186, 210, 220, 218))), ID=rep(c('a','b','c','d','e'), 7)) 
  # outlier_table(cars1, 'dist') 
   x.name <- x
  # numeric. Cannot check outliers if not.
  if (is.numeric(dataset[, x]) == T) {
    # Output table of summary statistics Row 1 No data removed
    dat.table <- data.frame(Vector = x.name, outlier_check = "None", 
                            N = length(dataset[, x]), mean = mean(dataset[, x], na.rm = T), median = stats::median(dataset[, x], na.rm = T), 
                            SD = sd(dataset[, x]), min = min(dataset[, x], na.rm = T), 
                            max = max(dataset[,x], na.rm = T), NAs = sum(length(which(is.na(dataset[, x])))), 
                            skew = skewness(dataset[,x]))
    # Row 2 5-95% quantile
    temp <- dataset[dataset[, x] < stats::quantile(dataset[, x], 0.95) & dataset[, x] > stats::quantile(dataset[, x], 0.05), ]
    dat.table <- rbind(dat.table, data.frame(Vector = x.name, outlier_check = "5_95_quant", 
                                             N = length(temp[, x]), mean = mean(temp[, x], na.rm = T), 
                                             median = stats::median(temp[, x], na.rm = T), SD = stats::sd(temp[, x]), min = min(temp[, x], na.rm = T), 
                                             max = max(temp[, x], na.rm = T), NAs = sum(length(which(is.na(temp[, x])))), 
                                             skew = skewness(temp[, x])))
    # Row 3 25-75% quantile
    temp <- dataset[dataset[, x] < quantile(dataset[, x], 0.75) & dataset[, x] > quantile(dataset[, x], 0.25), ]
    dat.table <- rbind(dat.table, data.frame(Vector = x.name, outlier_check = "25_75_quant", 
                                             N = length(temp[, x]), mean = mean(temp[, x], na.rm = T), median = stats::median(temp[, x], na.rm = T), 
                                             SD = stats::sd(temp[, x]), min = min(temp[, x], na.rm = T), max = max(temp[, x], na.rm = T), 
                                             NAs = sum(length(which(is.na(temp[, x])))), skew = skewness(temp[, x])))
    # Row 4 Mean +/2SD
    temp <- dataset[dataset[, x] < (mean(dataset[, x], na.rm = T) + 2 * stats::sd(dataset[, x], na.rm = T)) & 
                      dataset[, x] > (mean(dataset[, x], na.rm = T) - 2 * stats::sd(dataset[, x], na.rm = T)), ]
    dat.table <- rbind(dat.table, data.frame(Vector = x.name, outlier_check = "mean_2SD", 
                                             N = length(temp[, x]), mean = mean(temp[, x], na.rm = T), median = stats::median(temp[, x], na.rm = T), 
                                             SD = stats::sd(temp[, x]), min = min(temp[, x], na.rm = T), max = max(temp[, x], na.rm = T), 
                                             NAs = sum(length(which(is.na(temp[, x])))), skew = skewness(temp[, x])))
    # Row 5 Mean +/3SD
    temp <- dataset[dataset[, x] < (mean(dataset[, x], na.rm = T) + 3 * stats::sd(dataset[, x], na.rm = T)) & 
                      dataset[, x] > (mean(dataset[, x], na.rm = T) - 3 * stats::sd(dataset[, x], na.rm = T)), ]
    dat.table <- rbind(dat.table, data.frame(Vector = x, outlier_check = "mean_3SD", 
                                             N = length(temp[, x]), mean = mean(temp[, x], na.rm = T), median = stats::median(temp[, x], na.rm = T), 
                                             SD = stats::sd(temp[, x]), min = min(temp[, x], na.rm = T), max = max(temp[, x], na.rm = T), 
                                             NAs = sum(length(which(is.na(temp[, x])))), skew = skewness(temp[, x])))
    # Row 6 Median +/-2SD
    temp <- dataset[dataset[, x] < (stats::median(dataset[, x], na.rm = T) + 2 * stats::sd(dataset[, x], na.rm = T)) & 
                      dataset[, x] > (stats::median(dataset[, x], na.rm = T) - 2 * stats::sd(dataset[, x], na.rm = T)), ]
    dat.table <- rbind(dat.table, data.frame(Vector = x.name, outlier_check = "median_2SD", 
                                             N = length(temp[, x]), mean = mean(temp[, x], na.rm = T), median = stats::median(temp[, x], na.rm = T), 
                                             SD = stats::sd(temp[, x]), min = min(temp[, x], na.rm = T), max = max(temp[, x], na.rm = T), 
                                             NAs = sum(length(which(is.na(temp[, x])))), skew = skewness(temp[, x])))
    # Row 7 Median +/-3SD
    temp <- dataset[dataset[, x] < (stats::median(dataset[, x], na.rm = T) + 3 * stats::sd(dataset[, x], na.rm = T)) & 
                      dataset[, x] > (stats::median(dataset[, x], na.rm = T) - 3 * stats::sd(dataset[, x], na.rm = T)), ]
    dat.table <- rbind(dat.table, data.frame(Vector = x.name, outlier_check = "median_3SD", 
                                             N = length(temp[, x]), mean = mean(temp[, x], na.rm = T), median = stats::median(temp[, x], na.rm = T),
                                             SD = sd(temp[, x]), min = min(temp[, x], na.rm = T), max = max(temp[, x], na.rm = T), 
                                             NAs = sum(length(which(is.na(temp[, x])))), skew = skewness(temp[, x])))
    return(dat.table)
  } else {
    print("Data is not numeric.")
  }
}



##---------------------------##
outlier_plot <- function(dataset, x, outlier.mod = "none", x.dist = "normal", save.output = FALSE, remove = F) {
  #' Evaluate outliers by plotting the data
  #' @param dataset dataframe or matrix
  #' @param x column in dataframe
  #' @param outlier.mod defines method to ..

  #' Choices include:
  #'                 none: No data points are removed
  #'                 5_95_quant: Removes data points outside the 5th and 95th quantiles
  #'                 25_75_quant: Removes data points outside the 25th and 75th quantiles
  #'                 mean_2SD: Removes data points outside +/- 2SD of the mean
  #'                 median_2SD: Removes data points outside +/- 2SD of the meadian
  #'                 mean_3SD: Removes data points outside +/- 3SD of the mean
  #'                 median_3SD: Removes data points outside +/- 3SD of the meadian
  #' @param x.dist Distribution of the data. Choices include: normal, lognormal, exponential, weibull, poisson, negative binomial
  #' @param save.output Save plots as pdf file. If TRUE, three plots are returned as pdf file. If false, plots are printed to the screen.
  #' @param remove Save data with outliers removed. If TRUE, the revised data tables, based on outlier.mod, is returned.
  #' @keywords outliers
  #' @export outlier_plot
  #' @return Plot of the data
  # @examples Generate data for example. Inject outliers into cars dataset.  
  # cars1 <- cbind(rbind(cars[1:30, ], data.frame(speed=c(19,19,20,20,20), dist=c(190, 186, 210, 220, 218))), ID=rep(c('a','b','c','d','e'), 7)) 
  # outlier_plot(cars1, 'dist', outlier.mod='mean_2SD', x.dist='normal', save.output=FALSE)
  
  x.name <- x
  if (is.numeric(dataset[, x]) == T) {
    # Begin outlier check
    dataset$y <- 1:length(dataset[, x])
    if (outlier.mod == "none") {
      dataset <- dataset
      dat_sub <- dataset
    } else {
      if (outlier.mod == "5_95_quant") {
        dat_sub <- dataset[dataset[, x] < stats::quantile(dataset[, x], 0.95) & dataset[, x] > stats::quantile(dataset[, x], 0.05), ]
      } else if (outlier.mod == "25_75_quant") {
        dat_sub <- dataset[dataset[, x] < stats::quantile(dataset[, x], 0.75) & dataset[, x] > stats::quantile(dataset[, x], 0.25), ]
      } else if (outlier.mod == "mean_2SD") {
        dat_sub <- dataset[dataset[, x] < (mean(dataset[, x], na.rm = T) + 2 * 
                                             stats::sd(dataset[, x], na.rm = T)) & 
                             dataset[, x] > (mean(dataset[, x], na.rm = T) - 2 * stats::sd(dataset[, x], na.rm = T)), ]
      } else if (outlier.mod == "median_2SD") {
        dat_sub <- dataset[dataset[, x] < (stats::median(dataset[, x], na.rm = T) + 2 * stats::sd(dataset[, x], na.rm = T)) & 
                             dataset[, x] > (stats::median(dataset[, x], na.rm = T) - 2 * stats::sd(dataset[, x], na.rm = T)), ]
      } else if (outlier.mod == "mean_3SD") {
        dat_sub <- dataset[dataset[, x] < (mean(dataset[, x], na.rm = T) + 3 * stats::sd(dataset[, x], na.rm = T)) & 
                             dataset[, x] > (mean(dataset[, x], na.rm = T) - 3 * stats::sd(dataset[, x], na.rm = T)), ]
      } else if (outlier.mod == "median_3SD") {
        dat_sub <- dataset[dataset[, x] < (stats::median(dataset[, x], na.rm = T) + 3 * stats::sd(dataset[, x], na.rm = T)) & 
                             dataset[, x] > (stats::median(dataset[, x], na.rm = T) - 3 * stats::sd(dataset[, x], na.rm = T)), ]
      }
    }  #End Outlier mod
    # open a pdf file
    if (save.output == "TRUE") {
      pdf("outlier_plot.pdf")
    }
    graphics::par(mfrow = c(2, 2))
    # points
    graphics::plot(dataset$y, dataset[, x], pch = 19, col = "red", ylab = x.name, xlab = "Data row", main = "")
    graphics::points(dat_sub$y, dat_sub[, x], pch = 19, col = "blue")
    # Hist
    h <- graphics::hist(dat_sub[, x], breaks = length(dat_sub[, x]), col = "red", ylab = "", xlab = x.name, main = "")
    xfit <- seq(min(dat_sub[, x]), max(dat_sub[, x]), length = 40)
    # normal
    if (x.dist == "normal") {
      yfit <- stats::dnorm(xfit, mean = mean(dat_sub[, x]), sd = stats::sd(dat_sub[, x]))
    } else if (x.dist == "lognormal") {
      # lognormal
      yfit <- stats::dlnorm(xfit, mean = mean(log(dat_sub[, x])), sd = stats::sd(log(dat_sub[, x])))
    } else if (x.dist == "exponential") {
      # Exponential
      yfit <- stats::dexp(xfit, rate = 1/mean(dat_sub[, x]))
    } else if (x.dist == "weibull") {
      # Weibull
      yfit <- stats::dweibull(xfit, shape = 1.2/sqrt(var(log(dat_sub[, x]))), 
                       scale = mean(dat_sub[, x]) + 0.572/(1.2/sqrt(var(log(dat_sub[, x])))))
    } else if (x.dist == "poisson") {
      # Poisson
      yfit <- stats::dpois(round(xfit, 0), lambda = mean(dat_sub[, x]))
    } else if (x.dist == "negative binomial") {
      # Negative Binomial
      yfit <- stats::dnbinom(round(xfit, 0), size = mean(dat_sub[, x])^2/(var(dat_sub[, x]) - mean(dat_sub[, x])), 
                             mu = mean(dat_sub[, x]))
    }
    yfit <- yfit * diff(h$mids[1:2]) * length(dat_sub[, x])
    graphics::lines(xfit, yfit, col = "blue", lwd = 2)
    # Probability plot
    quants <- seq(0, 1, length = length(dat_sub[, x]) + 2)[2:(length(dat_sub[, x]) + 1)]
    # normal
    if (x.dist == "normal") {
      fit_quants <- stats::qnorm(quants, mean(dat_sub[, x]), sd(dat_sub[, x]))
    } else if (x.dist == "lognormal") {
      # lognormal
      fit_quants <- stats::qlnorm(quants, mean = mean(log(dat_sub[, x])), sd = sd(log(dat_sub)))
    } else if (x.dist == "exponential") {
      # Exponential
      fit_quants <- stats::qexp(quants, rate = 1/mean(dat_sub[, x]))
    } else if (x.dist == "weibull") {
      # Weibull
      fit_quants <- stats::qweibull(quants, shape = 1.2/sqrt(var(log(dat_sub[, x]))), 
                             scale = mean(dat_sub[, x]) + 0.572/(1.2/sqrt(var(log(dat_sub[, x])))))
    } else if (x.dist == "poisson") {
      # Poisson
      fit_quants <- stats::qpois(round(quants, 0), lambda = mean(dat_sub[, x]))
    } else if (x.dist == "negative binomial") {
      # Negative Binomial
      fit_quants <- stats::qnbinom(round(quants, 0), size = mean(dat_sub[, x])^2/(var(dat_sub[, x]) - mean(dat_sub[, x])), 
                            mu = mean(dat_sub[, x]))
    }
    data_quants <- stats::quantile(dat_sub[, x], quants)
    # create Q-Q plot
    graphics::plot(fit_quants, data_quants, xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
    graphics::title(main = paste("Q-Q plot of", x.dist, "fit against data"))
    graphics::abline(0, 1)
    graphics::mtext(paste("Plots for", x, "with", x.dist, "and", outlier.mod, "outliers removed"), outer = TRUE, cex = 1, line = -1.5)
    # Close the pdf file
    if (save.output == "TRUE") {
      dev.off()
    }
    
  } else {
    # Actions to take if data is not numeric
    print("Data is not numeric. Plots not generated.")
  }
}



##---------------------------##
outlier_remove <- function(dataset, x, outlier.mod = "none", remove = T) {
  #' Evaluate and edit outliers from variable
  #'  Contains functions to evaluate the data through histogram, QQ, and x-y plots.
  #'  Contains functions to evaulate the fit of the data in terms of distributions and analysis
  
  #' @param dataset dataframe or matrix
  #' @param x column in dataframe
  #' @param outlier.mod defines method to ..
  #' Choices include:
  #'                 none: No data points are removed
  #'                 5_95_quant: Removes data points outside the 5th and 95th quantiles
  #'                 25_75_quant: Removes data points outside the 25th and 75th quantiles
  #'                 mean_2SD: Removes data points outside +/- 2SD of the mean
  #'                 median_2SD: Removes data points outside +/- 2SD of the meadian
  #'                 mean_3SD: Removes data points outside +/- 3SD of the mean
  #'                 median_3SD: Removes data points outside +/- 3SD of the meadian
  #' @param remove Save data with outliers removed. If TRUE, the revised data tables, based on outlier.mod, is returned.
  #' @keywords outliers
  #' @export outlier_remove
  #' @return Returns the modified dataframe
  #' @details outlier_check is a series of functions allowing users to evaluate the occurrence of outliers and their effect on the data using summary data and plots. Outliers can onlyl be assessed for numeric vectors.
  
  # @examples Generate data for example. Inject outliers into cars dataset.  
   # MainDataTable <- outlier_remove(MainDataTable, 'dist', outlier.mod='mean_2SD', save.output=TRUE)
  if (is.numeric(dataset[, x]) == T) {
    # Begin outlier check
    if (remove == TRUE) {
      # log actions
      #write(layout.json.ed(trace, "outlier_remove", deparse(substitute(dataset)), deparse(substitute(x)), 
      #                     msg = paste("outliers removed using", outlier.mod)),
      #      paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""), append = T)
      
      if(!exists('logbody')) { 
        logging_code()
      } 
      outlier_remove_function <- list()
      outlier_remove_function$functionID <- 'outlier_remove'
      outlier_remove_function$args <- c(deparse(substitute(dataset)), deparse(substitute(x)), outlier.mod, remove)
      outlier_remove_function$msg <- paste("outliers removed using", outlier.mod)
      functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (outlier_remove_function)
      logbody$fishset_run <- list(infoBodyout, functionBodyout)
      write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE), paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
      assign("functionBodyout", value = functionBodyout, pos = 1)
      
      if (outlier.mod == "none") {
        dataset <- dataset
      } else if (outlier.mod == "5_95_quant") {
        dataset <- dataset[dataset[, x] < stats::quantile(dataset[, x], 0.95) & dataset[, x] > stats::quantile(dataset[, x], 0.05), ]
      } else if (outlier.mod == "25_75_quant") {
        dataset <- dataset[dataset[, x] < stats::quantile(dataset[, x], 0.75) & dataset[, x] > stats::quantile(dataset[, x], 0.25), ]
      } else if (outlier.mod == "mean_2SD") {
        dataset <- dataset[dataset[, x] < (mean(dataset[, x], na.rm = T) + 2 * stats::sd(dataset[, x], na.rm = T)) & 
                             dataset[, x] > (mean(dataset[, x], na.rm = T) - 2 * stats::sd(dataset[, x], na.rm = T)), ]
      } else if (outlier.mod == "median_2SD") {
        dataset <- dataset[dataset[, x] < (stats::median(dataset[, x], na.rm = T) + 2 * stats::sd(dataset[, x], na.rm = T)) & 
                             dataset[, x] > (stats::median(dataset[, x], na.rm = T) - 2 * stats::sd(dataset[, x], na.rm = T)), ]
      } else if (outlier.mod == "mean_3SD") {
        dataset <- dataset[dataset[, x] < (mean(dataset[, x], na.rm = T) + 3 * stats::sd(dataset[, x], na.rm = T)) & 
                             dataset[, x] > (mean(dataset[, x], na.rm = T) - 3 * stats::sd(dataset[, x], na.rm = T)), ]
      } else if (outlier.mod == "median_3SD") {
        dataset <- dataset[dataset[, x] < (stats::median(dataset[, x], na.rm = T) + 3 * stats::sd(dataset[, x], na.rm = T)) & 
                             dataset[, x] > (stats::median(dataset[, x], na.rm = T) - 3 * stats::sd(dataset[, x], na.rm = T)), ]
      }
      return(dataset)
    }  #End Outlier check
    if (remove == FALSE) {
      return(dataset)
      print("No modifications made.")
    }
  } else {
    # Actions to take if data is not numeric
    print("Data is not numeric. Outliers cannot be checked.")
  }
}
