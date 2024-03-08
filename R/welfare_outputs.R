#' Welfare plots and tables
#'
#' Generate plots and tables for welfare simulations
#' 
#' @param project Name of project
#' @param closures Closure scenarios
#' @param betadraws Integer indicating the numer of times to run the welfare simulation. Default value is
#'   \code{betadraws = 1000}
#' @details Returns a list with (1) plot showing welfare loss/gain for all scenarios in dollars, (2) plot showing welfare
#'   loss/gain as percentage, (3) dataframe with welfare summary stats in dollars, (4) dataframe with welfare summary
#'   stats as percentages, and (5) dataframe with welfare details such as number of trips, mean loss per trip, and mean of 
#'   the total welfare loss across all trips.
#' @import dplyr
#' @import ggplot2
#' @importFrom plotly ggplotly 
#' @export
#' 

welfare_outputs <- function(project, closures, betadraws = 1000) {
  
  ##
  # Define NULL variables to reduces notes during R CMD checks
  ##
  Scenario<-`2.5%`<-`5%`<-`50%`<-`95%`<-`97.5%`<- NULL
  
  
  ##
  # Load data from welfare simulations ----
  ##
  welfare <- data.table::fread(paste0(locoutput(project), "welfare_output.csv"))
  prc_welfare <- data.table::fread(paste0(locoutput(project), "prcwelfare_output.csv"))
  
  
  ##
  # Summarize data ----
  ##
  welfare_stats <- apply(welfare, MARGIN = 2, FUN = quantile, 
                         probs = c(0.025, 0.05, 0.5, 0.95, 0.975), na.rm = TRUE) %>%
    t() %>%
    as.data.frame() %>%
    mutate(Scenario = unlist(lapply(closures, FUN = function(x){return(x[1]$scenario)}))) %>%
    mutate(mean = apply(welfare, MARGIN = 2, FUN = mean)) %>%
    relocate(Scenario, .before = `2.5%`) %>% relocate(mean, .before = `2.5%`) 
  row.names(welfare_stats) <- NULL
  
  prc_welfare_stats <- apply(prc_welfare, MARGIN = 2, FUN = quantile, 
                             probs = c(0.025, 0.05, 0.5, 0.95, 0.975), na.rm = TRUE) %>%
    t() %>%
    as.data.frame() %>%
    mutate(Scenario = unlist(lapply(closures, FUN = function(x){return(x[1]$scenario)}))) %>%
    mutate(mean = apply(prc_welfare, MARGIN = 2, FUN = mean)) %>%
    relocate(Scenario, .before = `2.5%`) %>% relocate(mean, .before = `2.5%`)
  row.names(prc_welfare_stats) <- NULL
  
  welfare_details <- data.frame(scenarios = unlist(lapply(closures, FUN = function(x){return(x[1]$scenario)})),
                                num_trips = nrow(welfare)/betadraws,
                                mean_loss_per_trip = apply(welfare, MARGIN = 2, mean, na.rm = TRUE),
                                mean_total_welfare_loss = unlist(lapply(welfare,
                                                                        FUN = function(x, draws){
                                                                          tmp <- matrix(x, nrow = length(x) / draws, ncol = draws)
                                                                          tmp <- colSums(tmp)
                                                                          return(mean(tmp))
                                                                        }, draws = betadraws)))
  
  
  ##
  # Plot 1. Welfare loss/gain for all scenarios as dollars ----
  ##
  p1 <- ggplot() +
    geom_bar(data = welfare_stats, aes(x = Scenario, y = mean), stat = "identity") +
    geom_point(data = welfare_stats, aes(x = Scenario, y = `50%`), size = 2) +
    geom_errorbar(data = welfare_stats, aes(x = Scenario, ymin = `97.5%`, ymax = `2.5%`), width = 1) +
    labs(x = "Policy scanarios", y = "Welfare loss[-]/gain[+] ($)") +
    theme_classic() +
    geom_hline(yintercept = 0)
  p1 <- plotly::ggplotly(p1)   
  
  
  ##
  # Plot 2. Welfare loss/gain for all scenarios as percentage
  ##
  p2 <- ggplot() +
    geom_bar(data = prc_welfare_stats, aes(x = Scenario, y = mean), stat = "identity") +
    geom_point(data = prc_welfare_stats, aes(x = Scenario, y = `50%`)) +
    geom_errorbar(data = prc_welfare_stats, aes(x = Scenario, ymin = `97.5%`, ymax = `2.5%`), width = 1) +
    labs(x = "Policy scanarios", y = "Welfare loss[-]/gain[+] (%)") +
    theme_classic() +
    geom_hline(yintercept = 0)
  p2 <- plotly::ggplotly(p2)   
  
  
  # Return list of plots and tables (dataframes)
  return(list(p1, p2, welfare_stats, prc_welfare_stats, welfare_details))
}



