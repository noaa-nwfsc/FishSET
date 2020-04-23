#correlation 
corr_out <- function(dat, project, variables){
#' View correlation table and plot
#' 
#' @param dat Main data frame over which to apply function. Table in FishSET database should contain the string `MainDataTable`.
#' @param project Project name
#' @param variables Variables to include. Defaults to all. TO specify specific variables use variables=c('var1', 'var2', 'var3', ...)
#' @export
#' @details Returns a correlation plot and table. Output saved to output folder.
#' @examples 
#' \dontrun{
#' corr_out('pollockMainDataTable', 'pollock', 'all')
#' }
 

requireNamespace('ggplot2')

#Call in datasets
out <- data_pull(dat)
dat <- out$dat
dataset <- out$dataset

browser()

if(variables=='all') {
  variables <- colnames(dataset)
}

#Subset to only numeric variables
   variables <- variables[(variables %in%  names(which(lapply(dataset, is.numeric)==TRUE)))==TRUE]
  
  if(length(variables)<2) {
    warning('At least two variables must be included.')
  } else {
  if(length(variables)==2){
    ggplot2::ggplot(dataset, ggplot2::aes_string(x=dataset[[variables[1]]], y=dataset[[variables[2]]])) + 
                ggplot2::geom_point()+
                ggplot2::geom_smooth(method=lm)+
                ggplot2::labs(subtitle=paste(variables[1], 'by', variables[2]), x=variables[1], y=variables[2])+
                ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), 
                          panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"),
                          axis.text=ggplot2::element_text(size=11), axis.title=ggplot2::element_text(size=11))
  } else if(length(variables)>2){
    ggcorrplot::ggcorrplot(round(cor(dataset[,variables], use="complete.obs"), 2), 
                           type='lower',outline.color = 'white', hc.order=TRUE,show.diag=TRUE,
                           title = paste("Correlation matrix plot for", project, "data"),
                           ggtheme=ggplot2::theme_minimal())
  } 


  c1 <- round(cor(dataset[,variables], use="complete.obs"), 2)
  colnames(c1)=gsub("_","-", colnames(c1))


#Log the function 

  corr_out_function <- list()
  corr_out_function$functionID <- "corr_out"
  corr_out_function$args <- c(dat, variables)
  log_call(corr_out_function)

# Save output

  save_plot(project, "corr_out")
  save_table(c1, project, "corr_out") 

  plot
  c1

  }

}