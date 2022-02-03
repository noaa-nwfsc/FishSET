xy_plot <- function(dat, project, var1, var2, regress = FALSE) {
  #' Evaluate relationship of two variables in a plot format
  #' 
  #' @description Evaluate relationship of two variables in a plot format. 
  #'   Plots first variable against second variable.
  #' @param dat Primary data containing information on hauls or trips. Table in FishSET database contains the string 'MainDataTable'.
  #' @param project String, name of project.
  #' @param var1 First variable in \code{dat}.
  #' @param var2 Second variable in \code{dat}.
  #' @param regress Logical, if TRUE, returns plot with fitted linear regression line. Defaults to FALSE.
  #' @keywords xy plot
  #' @description Plot of var1 against var 2
  #' @return Returns plot output to R console and saves plot to Output folder.
  #' @import ggplot2
  #' @importFrom rlang sym expr enexpr current_env parse_expr
  #' @importFrom stats reformulate
  #' @export
  #' @examples
  #' \dontrun{
  #' xy_plot(pollockMainDataTable, 'OFFICIAL_TOTAL_CATCH_MT', 'HAUL', regress=TRUE)
  #' }

  requireNamespace("ggplot2")

  # Call in datasets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  #Empty variables for plotting
  .fitted <- NULL
  .resid <- NULL
  
  x_sym <- rlang::sym(var1)
  y_sym <- rlang::sym(var2)

  if (regress == FALSE) {
    
    x_plot <- ggplot2::ggplot(dataset, ggplot2::aes(x = !!x_sym, y = !!y_sym)) +
      ggplot2::geom_point() +
      ggplot2::labs(subtitle = paste(var1, "by", var2), x = var1, y = var2) +
      fishset_theme()
    
  } else {
    
    x_plot <- 
      ggpubr::annotate_figure(
        ggpubr::ggarrange(
          ggplot2::ggplot(dataset, ggplot2::aes(x = !!x_sym, y = !!y_sym)) +
            ggplot2::geom_point() +
            ggplot2::geom_smooth(method = lm) +
            ggplot2::labs(subtitle = paste(var1, "against", var2), x = var1, y = var2) +
            fishset_theme(), 
          ggplot2::ggplot(lm(dataset[[var1]] ~ dataset[[var2]])) +
            ggplot2::geom_point(ggplot2::aes(x = .fitted, y = .resid)) +
            ggplot2::labs(subtitle = "Residuals against fitted values", 
                          x = "Fitted", y = "Residuals") +
            fishset_theme(),
          ncol = 2, nrow = 1), 
        top = ggpubr::text_grob("Simple linear regression plots", size = 14))

    fm <- stats::reformulate(var1, var2)
    formula <- rlang::enexpr(fm)
    data <- rlang::parse_expr(dat)
    lm_call <- rlang::expr(summary(lm(!!formula, data = !!data)))
  }

  # Log the function
  xy_plot_function <- list()
  xy_plot_function$functionID <- "xy_plot"
  xy_plot_function$args <- list(dat, project, var1, var2, regress)
  log_call(project, xy_plot_function)

  # Save output
  save_plot(project, "xy_plot")

  if (regress == TRUE) {
    
    list(plot = x_plot, 
         refout = eval(lm_call, envir = rlang::current_env()))
  } else x_plot
}
