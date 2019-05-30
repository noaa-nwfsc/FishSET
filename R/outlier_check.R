# Outlier check functions. 

outlier_table <- function(dat, x) {
  #' Evaluate outliers in a table output
  #'
  #' @param dat Main data frame over which to apply function. Table in fishet_db database should contain the string `MainDataTable`.
  #' @param x Column in data frame to check for outliers 
  #' @importFrom stats quantile sd var na.pass
  #' @importFrom grDevices dev.off pdf  
  #' @keywords outliers
  #' @export outlier_table
  #' @return Table for evaluating whether outliers may exist in the selected data column.
  #' @details The returned table has dimension 7 x 10. The table allows users to assess outliers by subsetting the data by quantiles or standard deviations, and then
  #' reporting summary stats for the subsetted data.  
  #' Each row is a different subset of the data. In the first row, all data is included. 
  #' In the second row, data within the 5 to 95 percent quantiles are included. Subsequent rows include data within the 25-75\% quantile, the mean of `x` +/2SD, the mean of `x` +/3SD,
  #' the median of `x` +/2SD, and the median of `x` +/3SD.
  #' The second column identifies how data were subset. The remaining columns include the mean, median, standard deviation, minimum, maximum, number of NAs, and skew of the data.
  #' @examples 
  #' \dontrun{
  #' outlier_table(MainDataTable, 'HAUL') 
  #' }
  
  #Call in datasets
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
  if(is.character(dat)==TRUE){
    if(is.null(dat)==TRUE | table_exists(dat)==FALSE){
      print(DBI::dbListTables(fishset_db))
      stop(paste(dat, 'not defined or does not exist. Consider using one of the tables listed above that exist in the database.'))
    } else {
      dataset <- table_view(dat)
    }
  } else {
    dataset <- dat 
  }
  DBI::dbDisconnect(fishset_db)
  
  
    x.name <- x
  # numeric. Cannot check outliers if not.
  if (is.numeric(dataset[, x]) == T) {
    # Output table of summary statistics Row 1 No data removed
    dat.table <- data.frame(Vector = x.name, outlier_check = "None", 
                            N = length(dataset[, x]), mean = mean(dataset[, x], na.rm = T), median = stats::median(dataset[, x], na.rm = T), 
                            SD = sd(dataset[, x]), min = min(dataset[, x], na.rm = T), 
                            max = max(dataset[,x], na.rm = T), NAs = sum(length(which(is.na(dataset[, x])))), 
                            skew = FishSET:::skewness(dataset[,x]))
    # Row 2 5-95% quantile
    temp <- dataset[dataset[, x] < stats::quantile(dataset[, x], 0.95) & dataset[, x] > stats::quantile(dataset[, x], 0.05), ]
    dat.table <- rbind(dat.table, data.frame(Vector = x.name, outlier_check = "5_95_quant", 
                                             N = length(temp[, x]), mean = mean(temp[, x], na.rm = T), 
                                             median = stats::median(temp[, x], na.rm = T), SD = stats::sd(temp[, x]), min = min(temp[, x], na.rm = T), 
                                             max = max(temp[, x], na.rm = T), NAs = sum(length(which(is.na(temp[, x])))), 
                                             skew = FishSET:::skewness(temp[, x])))
    # Row 3 25-75% quantile
    temp <- dataset[dataset[, x] < quantile(dataset[, x], 0.75) & dataset[, x] > quantile(dataset[, x], 0.25), ]
    dat.table <- rbind(dat.table, data.frame(Vector = x.name, outlier_check = "25_75_quant", 
                                             N = length(temp[, x]), mean = mean(temp[, x], na.rm = T), median = stats::median(temp[, x], na.rm = T), 
                                             SD = stats::sd(temp[, x]), min = min(temp[, x], na.rm = T), max = max(temp[, x], na.rm = T), 
                                             NAs = sum(length(which(is.na(temp[, x])))), skew = FishSET:::skewness(temp[, x])))
    # Row 4 Mean +/2SD
    temp <- dataset[dataset[, x] < (mean(dataset[, x], na.rm = T) + 2 * stats::sd(dataset[, x], na.rm = T)) & 
                      dataset[, x] > (mean(dataset[, x], na.rm = T) - 2 * stats::sd(dataset[, x], na.rm = T)), ]
    dat.table <- rbind(dat.table, data.frame(Vector = x.name, outlier_check = "mean_2SD", 
                                             N = length(temp[, x]), mean = mean(temp[, x], na.rm = T), median = stats::median(temp[, x], na.rm = T), 
                                             SD = stats::sd(temp[, x]), min = min(temp[, x], na.rm = T), max = max(temp[, x], na.rm = T), 
                                             NAs = sum(length(which(is.na(temp[, x])))), skew = FishSET:::skewness(temp[, x])))
    # Row 5 Mean +/3SD
    temp <- dataset[dataset[, x] < (mean(dataset[, x], na.rm = T) + 3 * stats::sd(dataset[, x], na.rm = T)) & 
                      dataset[, x] > (mean(dataset[, x], na.rm = T) - 3 * stats::sd(dataset[, x], na.rm = T)), ]
    dat.table <- rbind(dat.table, data.frame(Vector = x, outlier_check = "mean_3SD", 
                                             N = length(temp[, x]), mean = mean(temp[, x], na.rm = T), median = stats::median(temp[, x], na.rm = T), 
                                             SD = stats::sd(temp[, x]), min = min(temp[, x], na.rm = T), max = max(temp[, x], na.rm = T), 
                                             NAs = sum(length(which(is.na(temp[, x])))), skew = FishSET:::skewness(temp[, x])))
    # Row 6 Median +/-2SD
    temp <- dataset[dataset[, x] < (stats::median(dataset[, x], na.rm = T) + 2 * stats::sd(dataset[, x], na.rm = T)) & 
                      dataset[, x] > (stats::median(dataset[, x], na.rm = T) - 2 * stats::sd(dataset[, x], na.rm = T)), ]
    dat.table <- rbind(dat.table, data.frame(Vector = x.name, outlier_check = "median_2SD", 
                                             N = length(temp[, x]), mean = mean(temp[, x], na.rm = T), median = stats::median(temp[, x], na.rm = T), 
                                             SD = stats::sd(temp[, x]), min = min(temp[, x], na.rm = T), max = max(temp[, x], na.rm = T), 
                                             NAs = sum(length(which(is.na(temp[, x])))), skew = FishSET:::skewness(temp[, x])))
    # Row 7 Median +/-3SD
    temp <- dataset[dataset[, x] < (stats::median(dataset[, x], na.rm = T) + 3 * stats::sd(dataset[, x], na.rm = T)) & 
                      dataset[, x] > (stats::median(dataset[, x], na.rm = T) - 3 * stats::sd(dataset[, x], na.rm = T)), ]
    dat.table <- rbind(dat.table, data.frame(Vector = x.name, outlier_check = "median_3SD", 
                                             N = length(temp[, x]), mean = mean(temp[, x], na.rm = T), median = stats::median(temp[, x], na.rm = T),
                                             SD = sd(temp[, x]), min = min(temp[, x], na.rm = T), max = max(temp[, x], na.rm = T), 
                                             NAs = sum(length(which(is.na(temp[, x])))), skew = FishSET:::skewness(temp[, x])))
    return(dat.table)
  } else {
    print("Data is not numeric.")
  }
}



##---------------------------##
outlier_plot <- function(dat, x, dat.remove = "none", x.dist = "normal", output ='screen') {
  #' Evaluate outliers through plots
  #' @param dat Main data frame over which to apply function. Table in fishet_db database should contain the string `MainDataTable`.
  #' @param x Column in dataf rame to check for outliers
  #' @param dat.remove Defines method to subset the data. Choices include: none, 5_95_quant, 25_75_quant, mean_2SD, median_2SD, mean_3SD, median_3SD
  #' @param x.dist Distribution of the data. Choices include: normal, lognormal, exponential, weibull, poisson, negative binomial
  #' @param output Return plots as pdf file or to the screen. If `pdf`, plots are returned as pdf file. If `screen`, plots are printed to the screen.
  #' @keywords outlier
  #' @importFrom graphics points
  #' @details  The function returns three plots, the data, a probability plot, and a Q-Q plot. The data plot is the value of
  #'  x against row number. Red points are all the data without any points removed. The blue points are the subsetted data. If `dat.remove` is `none`, then only blue points will be shown. 
  #'  The probability plot is a histogram of the data with the fitted probability distribution based on `x.dist`. The Q-Q plot plots are
  #'  sampled quantiles against theoretical quantiles. 
  #'  The dat.remove choices are
  #'  \itemize{
  #'  \item{none: No data points are removed}
  #'  \item{5_95_quant: Removes data points outside the 5th and 95th quantiles}
  #'  \item{25_75_quant: Removes data points outside the 25th and 75th quantiles}
  #'  \item{mean_2SD: Removes data points outside +/- 2SD of the mean}
  #'  \item{median_2SD: Removes data points outside +/- 2SD of the median}
  #'  \item{mean_3SD: Removes data points outside +/- 3SD of the mean}
  #'  \item{median_3SD: Removes data points outside +/- 3SD of the median}
  #'  }
  #'  The distribution choices are
  #'  \itemize{
  #'   \item{normal}
  #'    \item{lognormal}
  #'    \item{exponential}
  #'    \item{weibull}
  #'    \item{poisson}
  #'    \item{negative binomial}
  #'  }
  #' @export outlier_plot
  #' @return Plot of the data
  #' @examples 
  #' \dontrun{
  #' outlier_plot(MainDataTable, 'Haul', dat.remove='mean_2SD', x.dist='normal', output='screen')
  #' }
  
  
  #Call in datasets
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
  if(is.character(dat)==TRUE){
    if(is.null(dat)==TRUE | table_exists(dat)==FALSE){
      print(DBI::dbListTables(fishset_db))
      stop(paste(dat, 'not defined or does not exist. Consider using one of the tables listed above that exist in the database.'))
    } else {
      dataset <- table_view(dat)
    }
  } else {
    dataset <- dat 
  }
  DBI::dbDisconnect(fishset_db)
  

  x.name <- x
  if (is.numeric(dataset[, x]) == T) {
    # Begin outlier check
    dataset$y <- 1:length(dataset[, x])
    if (dat.remove == "none") {
      dataset <- dataset
      dat_sub <- dataset
    } else {
      if (dat.remove == "5_95_quant") {
        dat_sub <- dataset[dataset[, x] < stats::quantile(dataset[, x], 0.95) & dataset[, x] > stats::quantile(dataset[, x], 0.05), ]
      } else if (dat.remove == "25_75_quant") {
        dat_sub <- dataset[dataset[, x] < stats::quantile(dataset[, x], 0.75) & dataset[, x] > stats::quantile(dataset[, x], 0.25), ]
      } else if (dat.remove == "mean_2SD") {
        dat_sub <- dataset[dataset[, x] < (mean(dataset[, x], na.rm = T) + 2 * 
                                             stats::sd(dataset[, x], na.rm = T)) & 
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
    }  #End Outlier mod
    # open a pdf file
    if (output == "png") {
      pdf("outlier_plot.png")
    }
    graphics::par(mar=c(4,4,4,4)) 
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
      fit_quants <- stats::qlnorm(quants, mean = mean(log(dat_sub[, x])), sd = sd(log(dat_sub[, x])))
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
    graphics::mtext(paste0("Plots for ", x, " with ", x.dist, " distribution and data removed based on '", dat.remove, "'"), outer = TRUE, cex = 1, line = -1.5)
    # Close the pdf file
    if (output == "pdf") {
      dev.off()
    }
    
  } else {
    # Actions to take if data is not numeric
    print("Data is not numeric. Plots not generated.")
  }
}



##---------------------------##
outlier_remove <- function(dat, x, dat.remove = "none", remove = T) {
  #' Evaluate and edit outliers from variable
  #' @param dat Main data frame over which to apply function. Table in fishet_db database should contain the string `MainDataTable`.
  #' @param x Column in data frame containing potential outliers.
  #' @param dat.remove Defines method to subset the data. Choices include: none, 5_95_quant, 25_75_quant, mean_2SD, median_2SD, mean_3SD, median_3SD
  #' @param remove Save data with outliers removed. If TRUE, the revised data table, with values removed outside the `dat.remove` expression, is returned.
  #' @keywords outliers
  #' @export outlier_remove
  #' @return Returns the modified dataframe
  #' @details   The dat.remove choices are
  #'  \itemize{
  #'  \item{none: No data points are removed}
  #'  \item{5_95_quant: Removes data points outside the 5th and 95th quantiles}
  #'  \item{25_75_quant: Removes data points outside the 25th and 75th quantiles}
  #'  \item{mean_2SD: Removes data points outside +/- 2SD of the mean}
  #'  \item{median_2SD: Removes data points outside +/- 2SD of the median}
  #'  \item{mean_3SD: Removes data points outside +/- 3SD of the mean}
  #'  \item{median_3SD: Removes data points outside +/- 3SD of the median}
  #'  }
  #' @examples 
  #' \dontrun{
   #' MainDataTable <- outlier_remove(MainDataTable, 'dist', dat.remove='mean_2SD', save.output=TRUE)
   #' }
  
  #Call in datasets
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
  if(is.character(dat)==TRUE){
    if(is.null(dat)==TRUE | table_exists(dat)==FALSE){
      print(DBI::dbListTables(fishset_db))
      stop(paste(dat, 'not defined or does not exist. Consider using one of the tables listed above that exist in the database.'))
    } else {
      dataset <- table_view(dat)
    }
  } else {
    dataset <- dat 
  }
  DBI::dbDisconnect(fishset_db)
  
  
  
  if (is.numeric(dataset[, x]) == T) {
    # Begin outlier check
    if (remove == TRUE) {
      # log actions
 
        
      if (dat.remove == "none") {
        dataset <- dataset
      } else if (dat.remove == "5_95_quant") {
        dataset <- dataset[dataset[, x] < stats::quantile(dataset[, x], 0.95) & dataset[, x] > stats::quantile(dataset[, x], 0.05), ]
      } else if (dat.remove == "25_75_quant") {
        dataset <- dataset[dataset[, x] < stats::quantile(dataset[, x], 0.75) & dataset[, x] > stats::quantile(dataset[, x], 0.25), ]
      } else if (dat.remove == "mean_2SD") {
        dataset <- dataset[dataset[, x] < (mean(dataset[, x], na.rm = T) + 2 * stats::sd(dataset[, x], na.rm = T)) & 
                             dataset[, x] > (mean(dataset[, x], na.rm = T) - 2 * stats::sd(dataset[, x], na.rm = T)), ]
      } else if (dat.remove == "median_2SD") {
        dataset <- dataset[dataset[, x] < (stats::median(dataset[, x], na.rm = T) + 2 * stats::sd(dataset[, x], na.rm = T)) & 
                             dataset[, x] > (stats::median(dataset[, x], na.rm = T) - 2 * stats::sd(dataset[, x], na.rm = T)), ]
      } else if (dat.remove == "mean_3SD") {
        dataset <- dataset[dataset[, x] < (mean(dataset[, x], na.rm = T) + 3 * stats::sd(dataset[, x], na.rm = T)) & 
                             dataset[, x] > (mean(dataset[, x], na.rm = T) - 3 * stats::sd(dataset[, x], na.rm = T)), ]
      } else if (dat.remove == "median_3SD") {
        dataset <- dataset[dataset[, x] < (stats::median(dataset[, x], na.rm = T) + 3 * stats::sd(dataset[, x], na.rm = T)) & 
                             dataset[, x] > (stats::median(dataset[, x], na.rm = T) - 3 * stats::sd(dataset[, x], na.rm = T)), ]
      }
      
        if(!exists('logbody')) { 
          logbody <- list()
          infoBodyout <- list()
          functionBodyout <- list()
          infobody <- list()
          
          infobody$rundate <- Sys.Date()
          infoBodyout$info <- list(infobody)
          
          functionBodyout$function_calls <- list()
          
          logbody$fishset_run <- list(infoBodyout, functionBodyout)
        } 
        outlier_remove_function <- list()
        outlier_remove_function$functionID <- 'outlier_remove'
        outlier_remove_function$args <- c(deparse(substitute(dat)), deparse(substitute(x)), dat.remove, remove)
        outlier_remove_function$kwargs <- list()
        outlier_remove_function$output <- c('')
        outlier_remove_function$msg <- paste("outliers removed using", dat.remove)
        functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (outlier_remove_function)
        logbody$fishset_run <- list(infoBodyout, functionBodyout)
        write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE), paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
        assign("functionBodyout", value = functionBodyout, pos = 1)

      
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
