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
#' @import ggplot2
#' @importFrom dplyr filter select mutate_at group_by summarise ungroup
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
  
  # Creat connection to database
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  # Need to get zones included in the model and pull from primary data table
  x_temp <- unserialize(DBI::dbGetQuery(fishset_db, paste0("SELECT ModelInputData FROM ", project, "ModelInputData LIMIT 1"))$ModelInputData[[1]])
  
  # Make sure model exists
  if(!any(mod.name %in% sapply(x_temp , "[[" , "mod.name"))){
    stop("Model design file was not found for", mod.name)
  }

  outputs_welf <- list()
  
    
  if (!(shiny::isRunning())) {
     dat1 <- table_view(paste0(project,"MainDataTable"), project)
     
  } else {
     dat1 <- table_view(paste0(project,"MainDataTable_final"), project)
     
  }
  
  scenario_names <- unlist(lapply(closures, function(x){x$scenario}))  
  
  model_list <- list()
  welfare_summ_list <- list()
  prc_welfare_summ_list <- list()
  welfare_details_list <- list()
  
  for (ii in seq_along(mod.name)) { 
     mname <- mod.name[[ii]]
     
     x_new <- x_temp[[which(sapply(x_temp, function(x) x[["mod.name"]]) == mname)]]
    
     # Save zones in the model
     zones <- unique(x_new$choice)$choice
     
     
     if(!is.null(group_var)){
        # Filter primary data table for zones in the model and grouping variables
        dat <- dat1 %>%
           dplyr::filter((!!rlang::sym(zone.dat)) %in% zones) %>%
           dplyr::select(c((!!rlang::sym(zone.dat)), (!!sym(group_var)))) %>%
           dplyr::mutate_at(zone.dat, as.character)
        
     } else {
        dat <- dat1 %>%
           dplyr::filter((!!rlang::sym(zone.dat)) %in% zones) %>%
           dplyr::select((!!rlang::sym(zone.dat))) %>%
           dplyr::mutate_at(zone.dat, as.character)
     }

     # Replicate dat by the number of simulations
     dat <- do.call(rbind, replicate(betadraws, as.matrix(dat), simplify = FALSE))
     
     scenario_list <- list()
 
     for(c in c(scenario_names)){
        pname <- c
      
        ##
        # Load, combine data from welfare simulations ----
        ##
        welfare1 <- data.table::fread(paste0(locoutput(project), "welfare_output_df.csv")) %>% 
           filter(model_name %in% c(mname), policy_name %in% c(pname))
        prc_welfare1 <- data.table::fread(paste0(locoutput(project), "prcwelfare_output_df.csv")) %>% 
           filter(model_name %in% c(mname), policy_name %in% c(pname))
        

        # combine simulation and primary data if grouping by a variable
        # # tryCatch() stop and check for NAs
        flag <- 0
        tryCatch(
           {## view these and see the differing lengths --
              welfare <- data.frame(cbind(welfare1, dat))
              prc_welfare <- data.frame(cbind(prc_welfare1, dat))
           },
           warning=function(w) {
              stop(paste0('Data frames are differing length, please check for NAs in the ', project,'MainDataTable'), call. = FALSE)
              
           }
        )
  
        # Gather data into long format
        welfare_long <- welfare %>%
           rename("Scenario" = policy_name,
                  "welfare_change" = V1)
        
        prc_welfare_long <- prc_welfare%>%
           rename("Scenario" = policy_name,
                  "welfare_change" = V1)

  
        ##
        # Summarize data ----
        ##
        if(is.null(group_var)){ # summarize across closure scenarios
           welfare_summ <- welfare_long %>%
              dplyr::group_by(Scenario, model_name) %>%
              dplyr::summarise(mean = round(mean(welfare_change),2),
                               q = list(round(quantile(welfare_change, probs = c(0.025,0.05,0.5,0.95,0.975), na.rm = TRUE),2))) %>%
              tidyr::unnest_wider(q) %>%
              dplyr::ungroup()
           
           prc_welfare_summ <- prc_welfare_long %>%
              dplyr::group_by(Scenario, model_name) %>%
              dplyr::summarise(mean = round(mean(welfare_change),2),
                               q = list(round(quantile(welfare_change, probs = c(0.025,0.05,0.5,0.95,0.975), na.rm = TRUE),2))) %>%
              tidyr::unnest_wider(q) %>%
              dplyr::ungroup()
           
           
        } else { # summarize across scenarios and grouping variable
           welfare_summ <- welfare_long %>%
              dplyr::group_by(Scenario, !!sym(group_var), model_name) %>%
              dplyr::summarise(mean =round(mean(welfare_change),2),
                               q = list(round(quantile(welfare_change, probs = c(0.025,0.05,0.5,0.95,0.975), na.rm = TRUE),2))) %>%
              tidyr::unnest_wider(q) %>%
              dplyr::ungroup()
           
           prc_welfare_summ <- prc_welfare_long %>%
              dplyr::group_by(Scenario, !!sym(group_var), model_name) %>%
              dplyr::summarise(mean = round(mean(welfare_change),2),
                               q = list(round(quantile(welfare_change, probs = c(0.025,0.05,0.5,0.95,0.975), na.rm = TRUE),2))) %>%
              tidyr::unnest_wider(q) %>%
              dplyr::ungroup()
        }
        
        # 
        # Create a supplementary info table
        welfare_details <- data.frame(#mod.name = mname,
           # Scenarios = closures[[c]]$scenario,
           num_trips = nrow(welfare)/betadraws,
           mean_loss_per_trip = as.vector(welfare_long %>%
                                             dplyr::group_by(Scenario) %>%
                                             dplyr::summarise(mean_loss_per_trip=mean(welfare_change)) %>%
                                             dplyr::select(mean_loss_per_trip)),
           
           ### check this!!!
           mean_total_welfare_loss =  as.vector( welfare %>% 
                                                    group_by(model_name, policy_name) %>% 
                                                    summarise(mean_colsum = {
                                                       # Reshape the values into a matrix
                                                       tmp <- matrix(V1, nrow = n() / betadraws, ncol = betadraws)
                                                       # Calculate column sums and their mean
                                                       mean(colSums(tmp))
                                                    }))
           
        )
        print(paste("Model:", ii, "Scenario:", c))
        welfare_summ_list[[paste(mname, pname, sep = "_")]] <- welfare_summ 
        
        prc_welfare_summ_list[[paste(mname, pname, sep = "_")]] <-  prc_welfare_summ
        
        welfare_details_list[[paste(mname, pname, sep = "_")]] <-  welfare_details
        
     }
  }
 
  welfare_summ_all <- do.call(rbind, lapply(welfare_summ_list, as.data.frame))
  prc_welfare_summ_all <- do.call(rbind, lapply(prc_welfare_summ_list, as.data.frame))
  welfare_details_list_all <- do.call(rbind, lapply(welfare_details_list, as.data.frame))
  
  ##
  # Plot 1. Welfare loss/gain for all scenarios as dollars ----
  ##
  if(is.null(group_var)){
     p1 <- ggplot() +
        geom_bar(data = welfare_summ_all, aes(x = Scenario, y = mean, fill = Scenario), stat = "identity") +
        geom_point(data = welfare_summ_all, aes(x = Scenario, y = `50%`), size = 2) +
        geom_errorbar(data = welfare_summ_all, aes(x = Scenario, ymin = `97.5%`, ymax = `2.5%`), width = 1) +
        facet_wrap(~model_name)+
        
        labs(x = "Policy scanarios", y = "Welfare loss[-]/gain[+] ($)") +
        theme_classic() +
        scale_fill_viridis_d() +
        geom_hline(yintercept = 0)
     p1 <- plotly::ggplotly(p1)
     
     
  } else {
     p1 <- ggplot(data = welfare_summ_all, aes(x = Scenario, y = mean, ymin = `2.5%`, ymax = `97.5%`, fill = !!sym(group_var))) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_point(aes(y = `50%`), size = 2, position = position_dodge(width=0.9)) +
        geom_errorbar(width = nrow(welfare_summ_all)/15, position = position_dodge(width=0.9)) +
        facet_wrap(~model_name)+
        
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
        geom_bar(data = prc_welfare_summ_all, aes(x = Scenario, y = mean, fill = Scenario), stat = "identity") +
        geom_point(data = prc_welfare_summ_all, aes(x = Scenario, y = `50%`)) +
        geom_errorbar(data = prc_welfare_summ_all, aes(x = Scenario, ymin = `97.5%`, ymax = `2.5%`), width = 1) +
        facet_wrap(~model_name)+
        labs(x = "Policy scanarios", y = "Welfare loss[-]/gain[+] (%)") +
        theme_classic() +
        scale_fill_viridis_d() +
        geom_hline(yintercept = 0)
     p2 <- plotly::ggplotly(p2)
     
     
  } else {
     p2 <- ggplot(data = prc_welfare_summ_all, aes(x = Scenario, y = mean, ymin = `2.5%`, ymax = `97.5%`, fill = !!sym(group_var))) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_point(aes(y = `50%`), size = 2, position = position_dodge(width=0.9)) +
        geom_errorbar(width = nrow(prc_welfare_summ_all)/15, position = position_dodge(width=0.9)) +
        labs(x = "Policy scanarios", y = "Welfare loss[-]/gain[+] (%)") +
        facet_wrap(~model_name)+
        
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
  outputs_welf <- list(welfare_summ_all, prc_welfare_summ_all, welfare_details_list_all, p1, p2)
  return(outputs_welf)
}


