# correlation
corr_out <- function(dat, project, variables, ...) {
  #' View correlation coefficients between numeric variables
  #'
  #' @description Correlations coefficients can be displayed between all numeric variables or selected numeric variables. 
  #'   Defaults to pearson correlation coefficient. To change the method, specify \code{'method'} as 
  #'   \code{'kendall'}, or \code{'spearman'}.
  #'    Both a plot and table output are generated and saved to the `output` folder. Correlation plot is generated using \code{\link[ggcorrplot]{ggcorrplot}}.
  #' @param dat Primary data containing information on hauls or trips. Table in FishSET database contains the string 'MainDataTable'.
  #' @param project String, project name.
  #' @param variables A character string of variables to include. Defaults to \code{"all"} numeric variables.
  #' @param ... Additional arguments to add
  #' @export
  #' @import ggplot2
  #' @importFrom ggcorrplot ggcorrplot
  #' @importFrom stats cor
  #' @details Returns pearson's correlation coefficient between numeric variables in plot 
  #'   and table format. Output saved to output folder.
  #' @examples
  #' \dontrun{
  #' corr_out(pollockMainDataTable, 'pollock', 'all')
  #' }


  # Call in datasets
  out <- data_pull(dat)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main")
  

  if (variables == "all") {
    variables <- colnames(dataset)
  }

  # Subset to only numeric variables
  variables <- variables[(variables %in% names(which(lapply(dataset, is.numeric) == TRUE))) == TRUE]

  if (length(variables) < 2) {
    warning("At least two variables must be included.")
  } else {
    if (length(variables) == 2) {
      ggplot2::ggplot(dataset, ggplot2::aes_string(x = dataset[[variables[1]]], y = dataset[[variables[2]]])) +
        ggplot2::geom_point() +
        ggplot2::geom_smooth(method = lm) +
        ggplot2::labs(subtitle = paste(variables[1], "by", variables[2]), x = variables[1], y = variables[2]) +
        ggplot2::theme(
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(), panel.background = ggplot2::element_blank(), 
          axis.line = ggplot2::element_line(colour = "black"),
          axis.text = ggplot2::element_text(size = 11), axis.title = ggplot2::element_text(size = 11)
        )
    } else if (length(variables) > 2) {
      ggcorrplot::ggcorrplot(round(stats::cor(dataset[, variables], use = "complete.obs"), 2),
        type = "lower", outline.color = "white", hc.order = TRUE,
        show.diag = TRUE, title = paste("Correlation matrix plot for", project, "data"),
        ggtheme = ggplot2::theme_minimal()
      )
    }


    c1 <- round(stats::cor(dataset[, variables], use = "complete.obs"), 2)
    colnames(c1) <- gsub("_", "-", colnames(c1))


    # Log the function

    corr_out_function <- list()
    corr_out_function$functionID <- "corr_out"
    corr_out_function$args <- list(dat, project, variables)
    log_call(corr_out_function)

    # Save output

    save_plot(project, "corr_out")
    save_table(c1, project, "corr_out")

    plot
    c1
  }
}
