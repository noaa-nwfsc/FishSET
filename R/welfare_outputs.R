#' Welfare plots and tables
#'
#' Generate plots and tables for welfare simulations
#' 
#' @param project Name of project
#' @param mod.name Model name. Argument can be the name of the model or the name 
#'   can be pulled the `modelChosen` table. Leave \code{mod.name} empty to use 
#'   the name of the saved `best` model. If more than one model is saved, 
#'   \code{mod.name} should be the numeric indicator of which model to use.
#'   Use \code{table_view("modelChosen", project)} to view a table of saved models.
#' @param closures Closure scenarios
#' @param betadraws Integer indicating the numer of times to run the welfare simulation. Default value is
#'   \code{betadraws = 1000}
#' @param zone.dat Variable in primary data table that contains unique zone ID.
#' @param group_var Categorical variable from primary data table to group welfare outputs.
#' @details Returns a list with (1) plot showing welfare loss/gain for all scenarios in dollars, (2) plot showing welfare
#'   loss/gain as percentage, (3) dataframe with welfare summary stats in dollars, (4) dataframe with welfare summary
#'   stats as percentages, and (5) dataframe with welfare details such as number of trips, mean loss per trip, and mean of 
#'   the total welfare loss across all trips.
#' 
#' @import dplyr
#' @import ggplot2
#' @importFrom DBI dbConnect dbDisconnect dbGetQuery
#' @importFrom RSQLite SQLite
#' @importFrom data.table fread
#' @importFrom tidyr pivot_longer unnest_wider
#' @importFrom plotly ggplotly config plotly_build
#' @export
#' 

welfare_outputs <- function(project, mod.name, closures, betadraws = 1000, zone.dat = NULL, group_var = NULL) {
  
  ##
  # Define NULL variables to reduces notes during R CMD checks ----
  ##
  Scenario<-`2.5%`<-`5%`<-`50%`<-`95%`<-`97.5%`<-welfare_change<-mean_loss_per_trip<- NULL
  
  ##
  # Load, filter, format main data ----
  ##
  if (shiny::isRunning()) {
    dat <- table_view(paste0(project,"MainDataTable_final"), project)
    
    } else {
   dat <- get(paste0(project,"MainDataTable"))
  }

  # Creat connection to database
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  # Need to get zones included in the model and pull from main data table
  x_temp <- unserialize(DBI::dbGetQuery(fishset_db, paste0("SELECT ModelInputData FROM ", project, "ModelInputData LIMIT 1"))$ModelInputData[[1]])
  
  # Make sure model exists
  if(!any(mod.name %in% sapply(x_temp , "[[" , "mod.name"))){
    stop("Model design file was not found for", mod.name)
  }

  x_new <- x_temp[[which(sapply(x_temp , "[[" , "mod.name") == mod.name)]]

  # Save zones in the model
  zones <- unique(x_new$choice)$choice

  if(!is.null(group_var)){
    # Filter main data table for zones in the model and grouping variables
    dat <- dat %>%
      dplyr::filter((!!rlang::sym(zone.dat)) %in% zones) %>%
      dplyr::select(c((!!rlang::sym(zone.dat)), (!!sym(group_var)))) %>%
      dplyr::mutate_at(zone.dat, as.character)
      
  } else {
    dat <- dat %>%
      dplyr::filter((!!rlang::sym(zone.dat)) %in% zones) %>%
      dplyr::select((!!rlang::sym(zone.dat))) %>%
      dplyr::mutate_at(zone.dat, as.character)
  }

  # Replicate dat by the number of simulations
  dat <- do.call(rbind, replicate(betadraws, as.matrix(dat), simplify = FALSE))


  ##
  # Load, combine data from welfare simulations ----
  ##
  welfare <- data.table::fread(paste0(locoutput(project), "welfare_output.csv"))
  prc_welfare <- data.table::fread(paste0(locoutput(project), "prcwelfare_output.csv"))

  # combine simulation and primary data if grouping by a variable
 # tryCatch() stop and check for NAs
  #flag <- 0
  tryCatch(
    {
      welfare <- data.frame(cbind(welfare, dat))
      prc_welfare <- data.frame(cbind(prc_welfare, dat))
    },
    warning=function(w) {
      stop(paste0('Data frames are differing length, please check for NAs in the ', project,'MainDataTable'), call. = FALSE)

    }
  )


  # closure scenario names for dataframe variable names
  scenario_names <- unlist(lapply(closures, function(x){x$scenario}))

  # Gather data into long format
  welfare_long <- welfare %>%
    dplyr::rename_with(~scenario_names, (names(welfare)[1:length(closures)])) %>% #rename scenarios
    tidyr::pivot_longer(cols = 1:length(closures), names_to = c("Scenario"), values_to = "welfare_change")

  prc_welfare_long <- prc_welfare %>%
    dplyr::rename_with(~scenario_names, (names(prc_welfare)[1:length(closures)])) %>% #rename scenarios
    tidyr::pivot_longer(cols = 1:length(closures), names_to = c("Scenario"), values_to = "welfare_change")

  
  ##
  # Summarize data ----
  ##
  if(is.null(group_var)){ # summarize across closure scenarios
    welfare_summ <- welfare_long %>%
      dplyr::group_by(Scenario) %>%
      dplyr::summarise(mean = round(mean(welfare_change),2),
                q = list(round(quantile(welfare_change, probs = c(0.025,0.05,0.5,0.95,0.975), na.rm = TRUE),2))) %>%
      tidyr::unnest_wider(q) %>%
      dplyr::ungroup()

    prc_welfare_summ <- prc_welfare_long %>%
      dplyr::group_by(Scenario) %>%
      dplyr::summarise(mean = round(mean(welfare_change),2),
                       q = list(round(quantile(welfare_change, probs = c(0.025,0.05,0.5,0.95,0.975), na.rm = TRUE),2))) %>%
      tidyr::unnest_wider(q) %>%
      dplyr::ungroup()


  } else { # summarize across scenarios and grouping variable
    welfare_summ <- welfare_long %>%
      dplyr::group_by(Scenario, !!sym(group_var)) %>%
      dplyr::summarise(mean =round(mean(welfare_change),2),
                       q = list(round(quantile(welfare_change, probs = c(0.025,0.05,0.5,0.95,0.975), na.rm = TRUE),2))) %>%
      tidyr::unnest_wider(q) %>%
      dplyr::ungroup()

    prc_welfare_summ <- prc_welfare_long %>%
      dplyr::group_by(Scenario, !!sym(group_var)) %>%
      dplyr::summarise(mean = round(mean(welfare_change),2),
                       q = list(round(quantile(welfare_change, probs = c(0.025,0.05,0.5,0.95,0.975), na.rm = TRUE),2))) %>%
      tidyr::unnest_wider(q) %>%
      dplyr::ungroup()
  }

  # 
  # Create a supplementary info table
  welfare_details <- data.frame(Scenarios = scenario_names,
                                num_trips = nrow(welfare)/betadraws,
                                mean_loss_per_trip = as.vector(welfare_long %>%
                                                                 dplyr::group_by(Scenario) %>%
                                                                 dplyr::summarise(mean_loss_per_trip=mean(welfare_change)) %>%
                                                                 dplyr::select(mean_loss_per_trip)),
                                mean_total_welfare_loss = unlist(lapply(welfare[ ,1:length(closures), drop = FALSE],
                                                                        FUN = function(x, draws){
                                                                          tmp <- matrix(x, nrow = length(x) / draws, ncol = draws)
                                                                          tmp <- colSums(tmp)
                                                                          return(mean(tmp))
                                                                        }, draws = betadraws))
  )
  

  ##
  # Plot 1. Welfare loss/gain for all scenarios as dollars ----
  ##
  if(is.null(group_var)){
    p1 <- ggplot() +
      geom_bar(data = welfare_summ, aes(x = Scenario, y = mean, fill = Scenario), stat = "identity") +
      geom_point(data = welfare_summ, aes(x = Scenario, y = `50%`), size = 2) +
      geom_errorbar(data = welfare_summ, aes(x = Scenario, ymin = `97.5%`, ymax = `2.5%`), width = 1) +
      labs(x = "Policy scanarios", y = "Welfare loss[-]/gain[+] ($)") +
      theme_classic() +
      nmfspalette::scale_fill_nmfs()+
      geom_hline(yintercept = 0)
    p1 <- plotly::ggplotly(p1)
    

  } else {
    p1 <- ggplot(data = welfare_summ, aes(x = Scenario, y = mean, ymin = `2.5%`, ymax = `97.5%`, fill = !!sym(group_var))) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_point(aes(y = `50%`), size = 2, position = position_dodge(width=0.9)) +
      geom_errorbar(width = nrow(welfare_summ)/15, position = position_dodge(width=0.9)) +
      labs(x = "Policy scanarios", y = "Welfare loss[-]/gain[+] ($)") +
      theme_classic() +
      geom_hline(yintercept = 0) +
      scale_fill_viridis_d()
    p1 <- plotly::ggplotly(p1) %>%
      plotly::config(scrollZoom = TRUE) %>%
      plotly::plotly_build()
    

    # change markers to filled circles
    p1$x$data <- lapply(p1$x$data, function(x_tmp){
      if(x_tmp$type == "scatter" && x_tmp$mode == "markers"){
        x_tmp$marker$color = "black"
        return(x_tmp)
      } else {
        return(x_tmp)
      }
    })
  }


  ##
  # Plot 2. Welfare loss/gain for all scenarios as percentage
  ##
  if(is.null(group_var)){
    p2 <- ggplot() +
      geom_bar(data = prc_welfare_summ, aes(x = Scenario, y = mean, fill = Scenario), stat = "identity") +
      geom_point(data = prc_welfare_summ, aes(x = Scenario, y = `50%`)) +
      geom_errorbar(data = prc_welfare_summ, aes(x = Scenario, ymin = `97.5%`, ymax = `2.5%`), width = 1) +
      labs(x = "Policy scanarios", y = "Welfare loss[-]/gain[+] (%)") +
      theme_classic() +
      nmfspalette::scale_fill_nmfs()+
      geom_hline(yintercept = 0)
    p2 <- plotly::ggplotly(p2)
    

  } else {
    p2 <- ggplot(data = prc_welfare_summ, aes(x = Scenario, y = mean, ymin = `2.5%`, ymax = `97.5%`, fill = !!sym(group_var))) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_point(aes(y = `50%`), size = 2, position = position_dodge(width=0.9)) +
      geom_errorbar(width = nrow(prc_welfare_summ)/15, position = position_dodge(width=0.9)) +
      labs(x = "Policy scanarios", y = "Welfare loss[-]/gain[+] (%)") +
      theme_classic() +
      geom_hline(yintercept = 0) +
      scale_fill_viridis_d()
    p2 <- plotly::ggplotly(p2) %>%
      plotly::config(scrollZoom = TRUE) %>%
      plotly::plotly_build()

    # change markers to filled circles
    p2$x$data <- lapply(p2$x$data, function(x_tmp){
      if(x_tmp$type == "scatter" && x_tmp$mode == "markers"){
        x_tmp$marker$color = "black"
        return(x_tmp)
      } else {
        return(x_tmp)
      }
    })
  }

  # Return list of plots and tables (dataframes)
  return(list(p1, p2, welfare_summ, prc_welfare_summ, welfare_details))
  
}


