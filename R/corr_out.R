# correlation
corr_out <- function(dat, project, variables='all', method = "pearson") {
  #' View correlation coefficients between numeric variables
  #'
  #' @description Correlations coefficients can be displayed between all numeric variables or selected numeric variables. 
  #'   Defaults to pearson correlation coefficient. To change the method, specify \code{'method'} as 
  #'   \code{'kendall'}, or \code{'spearman'}.
  #'    Both a plot and table output are generated and saved to the `output` folder. Correlation plot is generated using \code{\link[ggcorrplot]{ggcorrplot}}.
  #' @param dat Primary data containing information on hauls or trips. Table in FishSET database contains the string 'MainDataTable'.
  #' @param project String, project name.
  #' @param variables A character string of variables to include. Defaults to \code{"all"} numeric variables.
  #' @param method A character string indicating which correlation coefficient is 
  #'   to be computed. One of "pearson" (default), "kendall", or "spearman". 
  #' @export
  #' @import ggplot2
  #' @importFrom ggcorrplot ggcorrplot
  #' @importFrom stats cor
  #' @importFrom rlang sym
  #' @details Returns Pearson's correlation coefficient between numeric variables in plot 
  #'   and table format. Output saved to output folder.
  #' @examples
  #' \dontrun{
  #' corr_out(pollockMainDataTable, 'pollock', 'all')
  #' }

  # Call in data sets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  end <- FALSE
  
  if (length(variables) == 1) {
    
    if (variables == "all") variables <- colnames(dataset)
    else {
      
      stop("At least two variables must be included.")
     
    }
  }
  
  # Subset to only numeric variables
  variables <- numeric_cols(dataset[variables])
  
  if (length(variables) < 2) {
    
    stop("All variables must be numeric.")
   
  }

    if(any(sapply(dataset[,variables], var)==0)){
      cat(paste0("No variance found in ", names(which(sapply(dataset[,variables], var)==0)),
                 ". Removed from correlation test"))
       variables <- variables[-which(sapply(dataset[,variables], var)==0)]
    }
   

    c_tab <- round(stats::cor(dataset[, variables], use = "complete.obs", method = method), 2)
    colnames(c_tab) <- gsub("_", "-", colnames(c_tab))
    
    if (length(variables) == 2) {
      
      x_sym <- rlang::sym(variables[1])
      y_sym <- rlang::sym(variables[2])
      
      c_plot <- 
        ggplot2::ggplot(dataset, ggplot2::aes(x = !!x_sym, y = !!y_sym)) +
        ggplot2::geom_point() +
        ggplot2::geom_smooth(method = lm) +
        ggplot2::labs(subtitle = paste(variables[1], "by", variables[2]), 
                      x = variables[1], y = variables[2]) +
        fishset_theme()
        
    } else if (length(variables) > 2) {
      
      c_plot <- 
        ggcorrplot::ggcorrplot(c_tab, type = "lower", outline.color = "white", 
                               hc.order = TRUE, show.diag = TRUE, 
                               title = paste("Correlation matrix plot for", project, "data"),
                               ggtheme = ggplot2::theme_minimal())
    }

    # Log the function
    corr_out_function <- list()
    corr_out_function$functionID <- "corr_out"
    corr_out_function$args <- list(dat, project, variables, method)
    log_call(project, corr_out_function)

    # Save output
    save_plot(project, "corr_out")
    save_table(c_tab, project, "corr_out")

    list(plot = c_plot, table = c_tab)

}
