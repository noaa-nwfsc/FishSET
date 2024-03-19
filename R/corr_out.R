# correlation
corr_out <- function(dat, project, variables = "all", method = "pearson", 
                     show_coef = FALSE) {
  #' View correlation coefficients between numeric variables
  #'
  #' @description Correlations coefficients can be displayed between all numeric 
  #'   variables or selected numeric variables. Defaults to pearson correlation 
  #'   coefficient. To change the method, specify \code{'method'} as 
  #'   \code{'kendall'}, or \code{'spearman'}.
  #'    Both a plot and table output are generated and saved to the `output` 
  #'    folder.
  #' @param dat Primary data containing information on hauls or trips. Table in 
  #' FishSET database contains the string 'MainDataTable'.
  #' @param project String, project name.
  #' @param variables A character string of variables to include. Defaults to 
  #'   \code{"all"} numeric variables.
  #' @param method A character string indicating which correlation coefficient is 
  #'   to be computed. One of "pearson" (default), "kendall", or "spearman". 
  #' @param show_coef Logical, whether to include the correlation coefficients 
  #'   on the correlation plot. Only coefficients with a p-value of less than or 
  #'   equal to 0.05 are shown. 
  #' @export
  #' @import ggplot2
  #' @importFrom stats cor cor.test
  #' @importFrom rlang sym
  #' @details Returns Pearson's correlation coefficient between numeric variables 
  #'   in plot and table format. Output saved to output folder.
  #' @examples
  #' \dontrun{
  #' corr_out(pollockMainDataTable, 'pollock', 'all')
  #' }

  # Call in data sets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  p.val <- NULL
  
  if (length(variables) == 1) {
    
    if (variables == "all") variables <- colnames(dataset)
    else {
      
      stop("At least two variables must be included.", call. = FALSE)
    }
  }
  
  # Subset to only numeric variables
  variables <- numeric_cols(dataset[variables])
  
  if (length(variables) < 2) {
    # TODO: update this error msg
    stop("All variables must be numeric.", call. = FALSE)
  }
  
  no_var <- qaqc_helper(dataset[variables], 
                        function(x) var(x, na.rm = TRUE) == 0, 
                        output = "names")

  if (length(no_var) > 0) {
    
    warning(paste0("No variance found in ", paste(no_var, collapse = ", "),
               ". Removed from correlation test"), call. = FALSE)
    
     variables <- variables[!variables %in% no_var]
  }

  c_tab <- round(stats::cor(dataset[, variables], use = "complete.obs", 
                            method = method), 2)
  colnames(c_tab) <- gsub("_", "-", colnames(c_tab))
  rownames(c_tab) <- gsub("_", "-", rownames(c_tab))
  
  if (show_coef) {
    
    cor.list <- lapply(dataset[variables], function(x) {
      
      vapply(dataset[variables], 
             function(y) stats::cor.test(x, y, method = method)$p.value, numeric(1))
    })
    
    p.val <- do.call(rbind, cor.list)
  }
  
  
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
    
    c_plot <- corr_plot(c_tab, p.val, show_coef, project)
  }

  # Log the function
  corr_out_function <- list()
  corr_out_function$functionID <- "corr_out"
  corr_out_function$args <- list(dat, project, variables, method, show_coef)
  log_call(project, corr_out_function)

  # Save output
  save_plot(project, "corr_out", c_plot)
  save_table(c_tab, project, "corr_out")

  list(plot = c_plot, table = c_tab)
}


corr_plot <- function(corr, p.val, show_coef, project) {
  #' Correlation plot
  #' @param corr Correlation matrix
  #' @param p.val Correlation matrix of p values. 
  #' @param show_coef Whether to show correlation coefficients. 
  #' @param project Name of project
  #' @export
  #' @keywords internal
  #' @import ggplot2
  #' @importFrom tidyr pivot_longer
  #' @importFrom tibble as_tibble
  
  Var1 <- Var2 <- value <- NULL

  #Get lower triangle of the correlation matrix
  get_lower_tri <- function(cormat) {
    if (is.null(cormat)) {
      return(cormat)
    }
    cormat[upper.tri(cormat)] <- NA
    return(cormat)
  }
  
  if (!is.matrix(corr) & !is.data.frame(corr)) {
    stop("Need a matrix or data frame!")
  }
  
  corr <- as.matrix(corr)
  corr <- round(x = corr, digits = 2)
  
  corr <- get_lower_tri(corr)
  
  melter <- function(mat) {
    
    cn <- colnames(mat)
    rn <- rownames(mat)
    
    value <- c(mat) # column wise 
    c1 <- factor(rep(rn, times = nrow(mat)), levels = rn)
    c2 <- factor(rep(cn, each = nrow(mat)), levels = cn)
    
    data.frame(Var1 = c1, Var2 = c2, value = value)
  }
  
  corr <- melter(corr)
  
  if (show_coef) {
    
    corr$pvalue <- round(c(p.val), 2)
    corr$signif <- ifelse(corr$pvalue < .05, corr$value, NA)
  } 
  
  corr <- corr[!is.na(corr$value), ]
  # corr$abs_corr <- abs(corr$value) * 10
  
  p <- ggplot2::ggplot(data = corr, 
                       mapping = ggplot2::aes(x = Var1, y = Var2, fill = value)) + 
    ggplot2::geom_tile(color = 'white')
  
  p <- p + ggplot2::scale_fill_gradient2(low = "blue", high = "red", 
                                         mid = "white", midpoint = 0, 
                                         limit = c(-1, 1), space = "Lab", 
                                         name = "Corr") + fishset_theme()
  
  if (show_coef) {
    
    p <- p + ggplot2::geom_text(ggplot2::aes(label = signif), size = 2.5, color = "black")
  }
  
  p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, 
                                                              vjust = 1, 
                                                              size = 8, 
                                                              hjust = 1), 
                          axis.text.y = ggplot2::element_text(size = 8)) + 
    ggplot2::coord_fixed()
  
  p <- p + ggplot2::ggtitle(paste("Correlation matrix plot for", project, "data"))
  
  
  p <- p + ggplot2::theme(axis.title.x = ggplot2::element_blank(), 
                          axis.title.y = ggplot2::element_blank())
  p
}
