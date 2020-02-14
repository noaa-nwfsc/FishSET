# Outlier check functions. 

outlier_table <- function(dat, x) {
  #' Evaluate outliers in a table output
  #'
  #' @param dat Main data frame over which to apply function. Table in fishet_db database should contain the string `MainDataTable`.
  #' @param x Column in data frame to check for outliers 
  #' @importFrom stats quantile sd var na.pass model.matrix
  #' @importFrom utils file_test
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
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset
  
  
    x.name <- x
    
    emptyrow <- data.frame(
      N = 0, 
      mean = NA, 
      median = NA, 
      SD = NA, 
      min = NA, 
      max = NA, 
      NAs = NA, 
      skew = NA)
    
    filledrow <-  function(dat,x){
      data.frame(
      N = length(temp[, x]), 
      mean = round(mean(temp[, x], na.rm = T),2), 
      median = round(stats::median(temp[, x], na.rm = T),2), 
      SD = round(stats::sd(temp[, x], na.rm=T),2), 
      min = round(min(temp[, x], na.rm = T),2), 
      max = round(max(temp[, x], na.rm = T),2), 
      NAs = sum(length(which(is.na(temp[, x])))), 
      skew = round(skewness(temp[, x], na.rm=T),2)
    )}
  # numeric. Cannot check outliers if not.
  if (is.numeric(dataset[, x]) == T) {
    # Output table of summary statistics Row 1 No data removed
    dat.table <- data.frame(Vector = x.name, outlier_check = "None", 
                            N = length(dataset[, x]), 
                            mean = round(mean(dataset[, x], na.rm = T),2), 
                            median = round(stats::median(dataset[, x], na.rm = T),2), 
                            SD = round(sd(dataset[, x], na.rm=T),2), 
                            min = round(min(dataset[, x], na.rm = T), 2), 
                            max = round(max(dataset[,x], na.rm = T),2), 
                            NAs = sum(length(which(is.na(dataset[, x])))), 
                            skew = round(skewness(dataset[,x], na.rm=T), 2))
    # Row 2 5-95% quantile
    temp <- dataset[dataset[, x] < stats::quantile(dataset[, x], 0.95, na.rm=TRUE) & 
                      dataset[, x] > stats::quantile(dataset[, x], 0.05, na.rm=TRUE), ]
    dat.table <- rbind(dat.table, data.frame(Vector = x.name, outlier_check = "5_95_quant", 
                                             if(dim(temp)[1]==0){
                                               emptyrow
                                             } else {
                                                filledrow(temp,x)
                                             }
                                             ))
    # Row 3 25-75% quantile
    temp <- dataset[dataset[, x] < quantile(dataset[, x], 0.75, na.rm=TRUE) & dataset[, x] > quantile(dataset[, x], 0.25, na.rm=TRUE), ]
    dat.table <- rbind(dat.table, data.frame(Vector = x.name, outlier_check = "25_75_quant", 
                                             if(dim(temp)[1]==0){
                                               emptyrow
                                             } else {
                                              filledrow(temp,x)
                                               }
                                              ))
    # Row 4 Mean +/2SD
    temp <- dataset[dataset[, x] < (mean(dataset[, x], na.rm = T) + 2 * stats::sd(dataset[, x], na.rm = T)) & 
                      dataset[, x] > (mean(dataset[, x], na.rm = T) - 2 * stats::sd(dataset[, x], na.rm = T)), ]
    dat.table <- rbind(dat.table, data.frame(Vector = x.name, outlier_check = "mean_2SD", 
                                             if(dim(temp)[1]==0){
                                               emptyrow
                                             } else {
                                               filledrow(temp,x)
                                             }
                                             ))
    # Row 5 Mean +/3SD
    temp <- dataset[dataset[, x] < (mean(dataset[, x], na.rm = T) + 3 * stats::sd(dataset[, x], na.rm = T)) & 
                      dataset[, x] > (mean(dataset[, x], na.rm = T) - 3 * stats::sd(dataset[, x], na.rm = T)), ]
    dat.table <- rbind(dat.table, data.frame(Vector = x, outlier_check = "mean_3SD", 
                                             if(dim(temp)[1]==0){
                                               emptyrow
                                             } else {
                                               filledrow(temp,x)
                                             }
                                              ))
    # Row 6 Median +/-2SD
    temp <- dataset[dataset[, x] < (stats::median(dataset[, x], na.rm = T) + 2 * stats::sd(dataset[, x], na.rm = T)) & 
                      dataset[, x] > (stats::median(dataset[, x], na.rm = T) - 2 * stats::sd(dataset[, x], na.rm = T)), ]
    dat.table <- rbind(dat.table, data.frame(Vector = x.name, outlier_check = "median_2SD", 
                                             if(dim(temp)[1]==0){
                                               emptyrow
                                             } else {
                                               filledrow(temp,x)
                                             }
                                              ))
    # Row 7 Median +/-3SD
    temp <- dataset[dataset[, x] < (stats::median(dataset[, x], na.rm = T) + 3 * stats::sd(dataset[, x], na.rm = T)) & 
                      dataset[, x] > (stats::median(dataset[, x], na.rm = T) - 3 * stats::sd(dataset[, x], na.rm = T)), ]
    dat.table <- rbind(dat.table, data.frame(Vector = x.name, outlier_check = "median_3SD", 
                                             if(dim(temp)[1]==0){
                                               emptyrow
                                             } else {
                                               filledrow(temp,x)
                                             }
                                              ))
    return(dat.table)
  } else {
    print("Data is not numeric.")
  }
}



##---------------------------##
outlier_plot <- function(dat, x, dat.remove, x.dist, output.screen=FALSE){
  #' Evaluate outliers through plots
  #' @param dat Main data frame over which to apply function. Table in fishet_db database should contain the string `MainDataTable`.
  #' @param x Column in dataf rame to check for outliers
  #' @param dat.remove Defines method to subset the data. Choices include: none, 5_95_quant, 25_75_quant, mean_2SD, median_2SD, mean_3SD, median_3SD
  #' @param x.dist Distribution of the data. Choices include: normal, lognormal, exponential, weibull, poisson, negative binomial
  #' @param output.screen If true, return plots to the screen. If false, returns plot to the inst/output folder as png file.
  #' @keywords outlier
  #' @importFrom graphics points
  #' @importFrom stats dnorm dpois dweibull rnorm dbinom dlnorm dexp dnbinom
  # @importFrom ggpubr annotate_figure text_grob
  #' @importFrom  ggplot2 ggplot geom_point aes_string theme geom_histogram labs aes geom_abline geom_abline
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
  #' outlier_plot(MainDataTable, 'Haul', dat.remove='mean_2SD', x.dist='normal', output.screen=TRUE)
  #' }
  
  requireNamespace("ggplot2")
  
  #Call in datasets
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset
  

  x.name <- x
  if (is.numeric(dataset[, x]) == T) {
    # Begin outlier check
    dataset$y <- 1:length(dataset[, x])
    if (dat.remove == "none") {
      dataset <- dataset
      dat_sub <- dataset
    } else {
      if (dat.remove == "5_95_quant") {
        dat_sub <- dataset[dataset[, x] < stats::quantile(dataset[, x], 0.95, na.rm=TRUE) & dataset[, x] > stats::quantile(dataset[, x], 0.05, na.rm=TRUE), ]
      } else if (dat.remove == "25_75_quant") {
        dat_sub <- dataset[dataset[, x] < stats::quantile(dataset[, x], 0.75, na.rm=TRUE) & dataset[, x] > stats::quantile(dataset[, x], 0.25, na.rm=TRUE), ]
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
  
    #graphics::par(mar=c(4,4,4,4)) 
    #graphics::par(mfrow = c(2, 2))
    # points
    
    
    mytheme <- ggplot2::theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text=element_text(size=9),
                     axis.title=element_text(size=8))                                                                                          
    ##Plot 1!
    distplot <- function(gdf,first, second){        
      ggplot2::ggplot(gdf, ggplot2::aes_string(x=first, y=second)) + 
        ggplot2::geom_point(color = 'red', na.rm=TRUE) + 
        ggplot2::geom_point(data=dat_sub, ggplot2::aes_string(x=first,y=second), color='blue', na.rm=TRUE) +
        ggplot2::labs(x='Data row') + mytheme
    }
    p1 <- distplot(dataset, 'y', x)
    
    
    # Hist
    ##Plot 2!     
    p2 <- ggplot2::ggplot(dat_sub, aes_string(x)) + ggplot2::geom_histogram(ggplot2::aes(y = ..density..), na.rm=TRUE,
                                                                            bins=if(nrow(dataset) < 500) {round(nrow(dataset)/2)
                                                                              } else { 250}) + 
      mytheme  
    if (x.dist == "normal") {    
      p2 <- p2 + ggplot2::stat_function(fun = dnorm, colour = "blue", 
                               args = list(mean = mean(dat_sub[,x], na.rm = TRUE), 
                                           sd = sd(dat_sub[,x], na.rm = TRUE)))
    } else if (x.dist == "lognormal") {
      # lognormal
      p2 <- p2 + ggplot2::stat_function(fun = dlnorm, colour = "blue", 
                               args = list(mean = mean(log(dat_sub[,x]), na.rm = TRUE), 
                                           sd = sd(log(dat_sub[,x]), na.rm = TRUE)))
    } else if (x.dist == "exponential") {
      # Exponential
      p2 <- p2 + ggplot2::stat_function(fun = dexp, colour = "blue", 
                               args = list(rate = 1/mean(dat_sub[, x],na.rm=TRUE)))
    } else if (x.dist == "weibull") {
      # Weibull
      p2 <- p2 + ggplot2::stat_function(fun = dweibull, colour = "blue", 
                               args = list(shape = 1.2/sqrt(var(log(dat_sub[, x]),na.rm=TRUE)), 
                                           scale = mean(dat_sub[, x],na.rm=TRUE) + 0.572/(1.2/sqrt(var(log(dat_sub[, x]),na.rm=TRUE)))))
    } else if (x.dist == "poisson") {
      # Poisson
      p2 <- p2 + ggplot2::stat_function(fun = dpois, colour = "blue", 
                               args = list(lambda = mean(dat_sub[, x],na.rm=TRUE)))
    } else if (x.dist == "negative binomial") {
      # Negative Binomial
      p2 <- p2 + ggplot2::stat_function(fun = dnbinom, colour = "blue", 
                               args = list( mean(dat_sub[, x],na.rm=TRUE)^2/(var(dat_sub[, x],na.rm=TRUE) - mean(dat_sub[, x],na.rm=TRUE)), 
                                            mu = mean(dat_sub[, x],na.rm=TRUE)))
    }
    
#Plot3
    # Probability plot
    quants <- seq(0, 1, length = length(dat_sub[, x]) + 2)[2:(length(dat_sub[, x]) + 1)]
    # normal
    if (x.dist == "normal") {
      fit_quants <- stats::qnorm(quants, mean(dat_sub[, x],na.rm=TRUE), sd(dat_sub[, x],na.rm=TRUE))
    } else if (x.dist == "lognormal") {
      # lognormal
      fit_quants <- stats::qlnorm(quants, mean = mean(log(dat_sub[, x]),na.rm=TRUE), sd = sd(log(dat_sub[, x]),na.rm=TRUE))
    } else if (x.dist == "exponential") {
      # Exponential
      fit_quants <- stats::qexp(quants, rate = 1/mean(dat_sub[, x],na.rm=TRUE))
    } else if (x.dist == "weibull") {
      # Weibull
      fit_quants <- stats::qweibull(quants, shape = 1.2/sqrt(var(log(dat_sub[, x]),na.rm=TRUE)), 
                             scale = mean(dat_sub[, x],na.rm=TRUE) + 0.572/(1.2/sqrt(var(log(dat_sub[, x]),na.rm=TRUE))))
    } else if (x.dist == "poisson") {
      # Poisson
      fit_quants <- stats::qpois(quants, lambda = mean(dat_sub[, x],na.rm=TRUE))
    } else if (x.dist == "negative binomial") {
      # Negative Binomial
      fit_quants <- stats::qnbinom(quants, size = mean(dat_sub[, x],na.rm=TRUE)^2/(var(dat_sub[, x],na.rm=TRUE) - mean(dat_sub[, x],na.rm=TRUE)), 
                            mu = mean(dat_sub[, x],na.rm=TRUE))
    }
    
    data_quants <- stats::quantile(as.numeric(dat_sub[, x]), quants,na.rm=TRUE)
     # create Q-Q plot
    temp <- data.frame(fit_quants, data_quants) 
    p3 <- ggplot2::ggplot(temp, ggplot2::aes(x=fit_quants, y=data_quants)) + ggplot2::geom_point(shape=1) + ggplot2::geom_abline() +
      ggplot2::labs(x='Theoretical Quantiles', y='Sample Quantiles', title=paste('Q-Q plot of', x.dist, 'fit against data'))+
      mytheme
    
#Put it all together
    fig <- suppressWarnings(ggpubr::ggarrange(p1, p2, p3 , ncol = 2, nrow = 2))
    # labels = c("A", "B", "C"),
    fig <- ggpubr::annotate_figure(fig, top = ggpubr::text_grob(paste("Plots for ", x, " with ", x.dist, 
                                                               " distribution and data removed based on '", dat.remove,
                                                               "'. \nBlue: included points   Red: removed points"), size = 10))         
    
    plot(fig)
   # Close the pdf file
    if (output.screen == FALSE) {
      ggplot2::ggsave(file="outlier_plot.png", path=paste0(getwd(),"/inst/output/"))
      dev.off()
    }
    
  } else {
    # Actions to take if data is not numeric
    print("Data is not numeric. Plots not generated.")
  }
}



##---------------------------##
outlier_remove <- function(dat, x, dat.remove = "none", remove = T, over_write=FALSE) {
  #' Evaluate and edit outliers from variable
  #' @param dat Main data frame over which to apply function. Table in fishet_db database should contain the string `MainDataTable`.
  #' @param x Column in data frame containing potential outliers.
  #' @param dat.remove Defines method to subset the data. Choices include: none, 5_95_quant, 25_75_quant, mean_2SD, median_2SD, mean_3SD, median_3SD
  #' @param remove Save data with outliers removed. If TRUE, the revised data table, with values removed outside the `dat.remove` expression, is returned.
  #' @param over_write Save over previous file
  #' @export outlier_remove
  #' @return Returns the modified dataframe. MOdified dataframe will be saved to fishset_db database.
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
        dataset <- dataset[dataset[, x] < stats::quantile(dataset[, x], 0.95, na.rm=TRUE) & dataset[, x] > stats::quantile(dataset[, x], 0.05, na.rm=TRUE), ]
      } else if (dat.remove == "25_75_quant") {
        dataset <- dataset[dataset[, x] < stats::quantile(dataset[, x], 0.75, na.rm=TRUE) & dataset[, x] > stats::quantile(dataset[, x], 0.25, na.rm=TRUE), ]
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
      
      
      if(dat.remove!='none'& over_write=='TRUE'){
      suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
      DBI::dbWriteTable(fishset_db, deparse(substitute(dat)), dataset, overwrite=over_write)
      DBI::dbDisconnect(fishset_db)
      }
        outlier_remove_function <- list()
        outlier_remove_function$functionID <- 'outlier_remove'
        outlier_remove_function$args <- c(dat, deparse(substitute(x)), dat.remove, remove)
        outlier_remove_function$kwargs <- list()
        outlier_remove_function$output <- c('')
        outlier_remove_function$msg <- paste("outliers removed using", dat.remove)
        log_call(outlier_remove_function)
        
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
