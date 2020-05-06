xy_plot <- function(dat, project, var1, var2, regress=FALSE){
  #' Plot observed locations on map
  #'
  #' @param dat Main data frame over which to apply function. Table in FishSET database should contain the string `MainDataTable`.
  #' @param project Project name
  #' @param var1 Variable in dat 
  #' @param var2 Variable in dat 
  #' @param regress Defaults to FALSE. If TRUE, returns plot with fitted linear regression line.
  #' @keywords xy plot
  #' @description Plot of var1 against var 2
  #' @return ggplot2 object
  #' @import ggplot2
  #' @export
  #' @examples
  #' \dontrun{
  #' xy_plot('pollockMainDataTable', 'OFFICIAL_TOTAL_CATCH_MT', 'HAUL', regress=TRUE)
  #' }
  
  requireNamespace('ggplot2')
  
  #Call in datasets
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset
  

if(regress==FALSE){  
plot <- ggplot2::ggplot(dataset, ggplot2::aes_string(x=dataset[[var1]], y=dataset[[var2]])) + 
        ggplot2::geom_point()+
        ggplot2::labs(subtitle=paste(var1, 'by', var2), x=var1, y=var2) +
        ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), 
                   panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"),
                   axis.text = ggplot2::element_text(size=11), axis.title=ggplot2::element_text(size=11))
} else {
  
  plot <- ggpubr::annotate_figure(ggpubr::ggarrange(
          ggplot2::ggplot(dataset, ggplot2::aes_string(x=dataset[[var1]], y=dataset[[var2]])) + 
                                            ggplot2::geom_point()+ ggplot2::geom_smooth(method=lm)+
                                            ggplot2::labs(subtitle=paste(var1, 'against', var2), x=var1, y=var2)+
                                            ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), 
                                                           panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"), axis.text=ggplot2::element_text(size=11),
                                                           axis.title = ggplot2::element_text(size=11)),
          ggplot2::ggplot(lm(dataset[[var1]]~dataset[[var2]])) + 
                                            ggplot2::geom_point(ggplot2::aes(x=.fitted, y=.resid)) + 
                                            ggplot2::labs(subtitle = 'Residuals against fitted values', x='Fitted',y='Residuals')+
                                            ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), 
                                                           panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"), 
                                                           axis.text = ggplot2::element_text(size=11),
                                                           axis.title=ggplot2::element_text(size=11)),
                                          ncol=2, nrow=1), top=ggpubr::text_grob('Simple linear regression plots', size=14))

  
          refout <- summary(lm(dataset[[var2]]~dataset[[var1]]))
} 
  
  
  #Log the function 
  
  xy_plot_function <- list()
  xy_plot_function$functionID <- "xy_plot"
  xy_plot_function$args <- c(dat, var1, var2, regress)
  log_call(xy_plot_function)
  
  # Save output
  
  save_plot(project, "xy_plot")
    
  plot
  
  if(regress==TRUE){
    refout
  }
  
}

    