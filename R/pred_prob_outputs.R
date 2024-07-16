#' Summarize predicted probabilities
#'
#' Create summary table and figures for the predicted probabilities of fishing per zone for each model and policy scenario. 
#' The table and figures include the base case scenario, which is the proportion of observations in each zone. The table also
#' includes the squared error between the predicted probabilities and base case probabilities. The first figure option displays
#' predicted probabilities for each model, and the second figure option shows predicted probabilities for each model and policy.
#'
#' @param project Name of project
#' @param mod.name Name of model
#' @param zone.dat Variable in primary data table that contains unique zone ID.
#' @param policy.name List of policy scenario names created in zone_closure function
#' @param output_option "table" to return summary table (default); "model_fig" for predicted probabilities; or "policy_fig" 
#' to return predicted probabilities for each model/policy scenario ; "diff_table" to return difference between predicted probabilities between 
#' model and policy scenario for each zone.
#' @return A model prediction summary table (default), model prediction figure, or policy prediction figure. See \code{output_option} argument.
#' @details This function requires that model and prediction output tables exist in the FishSET database. If these tables are not 
#' present in the database to function with terminate and return an error message.
#' 
#'@export
#'@import ggplot2
#'@import tidyr
#'@examples 
#'\dontrun{
#'
#'pred_prob_outputs(project = "scallop")
#'
#'}
pred_prob_outputs <- function(project, mod.name = NULL, zone.dat = NULL, policy.name = NULL, output_option = "table"){
  
  # Create connection to database and remove connection on exit of this function
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  # NULL the ZoneID variable to appease RMD check
  ZoneID <- NULL
  
  # Load model and prediction outputs --------------------------------------------------------------------------------------- 
  # make model and prediction tables exist in the database
  # TODO: If this is running in the Shiny and one or more of the tables above do not exist then show error message and stop running function here
  tryCatch({
  #  model_output <<- model_design_list("scallopMod")[[which(lapply(model_design_list("scallopMod"), "[[", "mod.name") == "lz")]]},
    model_output <- model_design_list(project)[[which(lapply(model_design_list(project), "[[", "mod.name") == mod.name)]]},

    error = function(err){message(paste0("Model output table not found in ", project))}
  )
  tryCatch({
    
    pred_out <- unserialize_table(paste0(project, "predictOutput"), project)
    
    get_mod_pred_out <- lapply(pred_out, function(x){
      mod_name <- x$modelDat$mod.name

      if(mod_name == mod.name){
        return(1)
      } else {
        return(0)
      }
    })
    
    pred_output <- pred_out[which(unlist(get_mod_pred_out) == 1)]
  },
    
    error = function(err){message(paste0("Prediction output table not found in ", project))}  
  )

  # Calculate percent observations per zone
 # prop_obs <- table(as.factor(model_output[[1]]$choice.table$choice)) / sum(table(as.factor(model_output[[1]]$choice.table$choice)))
  prop_obs <- table(as.factor(model_output$choice$choice)) / sum(table(as.factor(model_output$choice$choice)))
  prop_obs <- data.frame(ZoneID = names(prop_obs), Proportion.Observations = as.vector(prop_obs))
  
  # Get a list of model names from prediction output scenario names
  predict_n <- unlist(lapply(pred_output, function(x) x$scenario.name))
  mod_n <- unique(sapply(strsplit(predict_n, split = " "), "[", 1))
  
  # Get the index for the first prediction output for each model name
  predOut_ind <- unlist(lapply(mod_n, function(x) grep(x, predict_n)[1]))
  
  # Get predicted probabilities by zone for each model
  predProbs <- lapply(pred_output[predOut_ind], function(x) x$prob[,1]/100)
  
  # Return table without running code below ---------------------------------------------------------------------------------
  if(output_option == "table"){
    # Calculate squared error between percent observations and predicted probabilities
    predSE<-lapply(predProbs, function(x) (x - prop_obs$Proportion.Observations)^2)
    
    # Combine proportion of observation, predicted probabilities and squared error
    out_tab <- prop_obs
    for(i in 1:length(predProbs)){
      tmp_df<-as.data.frame(cbind(predProbs[[i]], predSE[[i]]))
      names(tmp_df) <- c(mod_n[i], paste0(mod_n[i],"_SE"))
      out_tab <- cbind(out_tab, tmp_df)
    }
    
    return(out_tab)
    
  # Format data for figures -------------------------------------------------------------------------------------------------
  } else if(output_option %in% c("model_fig", "policy_fig", "diff_table")){
    # Save model predicted probabilities for each zone
    tmp_df <- data.frame(ZoneID = prop_obs$ZoneID)
    for(i in 1:length(predProbs)){
      tmp_df1 <- as.data.frame(cbind(predProbs[[i]]))
      names(tmp_df1) <- c(mod_n[i])
      tmp_df <- cbind(tmp_df, tmp_df1)
    }
    
    # Return bargraph with model pred probabilities ------------------------------------------------------------------------
    if(output_option == "model_fig"){
      # Add prop observation and convert to long format dataframe
      fig_df <- tmp_df %>%
        mutate(Proportion.Observations = prop_obs$Proportion.Observations) %>%
        gather(key = "Model", value = "Probability", -ZoneID)
      
      out_fig <- ggplot() +
        geom_col(aes(x = fig_df$ZoneID, y = fig_df$Probability, fill = fig_df$Model), width = 0.6, position = position_dodge(0.6)) +
        labs(x = "Zone ID", y = "Probability", fill = "Model") +
        scale_y_continuous(expand = c(0,0), limits = c(0, max(fig_df$Probability) + 0.05)) +
        scale_fill_viridis_d() +
        theme_classic() +
        theme(text = element_text(size= 16),
              axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.95))
      
      if(!isRunning()){
        return(out_fig)
      }
      
      out_fig <- ggplotly(out_fig) %>% 
        plotly::layout(
        legend =list(
          orientation = "h",
          # show entries horizontally
          bgcolor = "white",
          bordercolor = "black",
          xanchor = "center",
          # use center of legend as anchor
          x = 0.5,
          y = -0.7,
          font = list(
            family = "sans-serif",
            size = 15,
            color = "black"
          )
        )
      )
      
      return(out_fig)
      
    # Return bargraph with policy pred probabilities -----------------------------------------------------------------------  
    } else if (output_option == "policy_fig") {
      # Add predicted probabilities for policy scenarios
      policy_probs <- unlist(lapply(pred_output, function(x){ x$prob[,2] }))
      policy_probs <- matrix(policy_probs/100, ncol = length(pred_output), byrow = FALSE)
      policy_probs <- as.data.frame(policy_probs)
      colnames(policy_probs) <- predict_n
      
      # Combine with base case predictions for each model and convert to long format
      fig_df <- cbind(tmp_df, policy_probs) %>%
        gather(key = "Model", value = "Probability", -ZoneID) %>% 
        filter(Model %in% c(mod_n, paste0(mod_n, " ", policy.name)))
    
      out_fig <- ggplot() +
        geom_col(aes(x = fig_df$Model, y = fig_df$Probability, fill = fig_df$ZoneID), width = 0.6, position = position_dodge(0.6)) +
        labs(x = "Policy Scenario", y = "Probability", fill = "Zone ID") +
        scale_y_continuous(expand = c(0,0), limits = c(0, max(fig_df$Probability) + 0.05)) +
        scale_fill_viridis_d() +
        theme_classic() +
        theme(text = element_text(size= 16),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust = 0.95))
      
      if(!isRunning()){
        return(out_fig)
      }
      
      out_fig <- ggplotly(out_fig)
      
      return(out_fig)
    
    } else if(output_option == "diff_table"){
      policy_probs <- unlist(lapply(pred_output, function(x){ x$prob[,2] }))
      policy_probs <- matrix(policy_probs/100, ncol = length(pred_output), byrow = FALSE)
      policy_probs <- as.data.frame(policy_probs)
      colnames(policy_probs) <- predict_n
      
      diff_df <- cbind(tmp_df, policy_probs)
      
      tm <- diff_df %>% 
        pivot_longer(., -c(any_of("ZoneID"), any_of(mod_n))) %>% 
        group_by(across(any_of("ZoneID")), name) %>% 
        mutate(difference = value- !!rlang::sym(mod_n)) %>% 
        mutate(across(where(is.numeric), ~ round(., 3))) %>%
        filter(name %in% c(paste0(mod_n, " ", policy.name))) %>% 
        pivot_wider(., names_from = c("name"), values_from = c("difference", "value")) %>%
        select(any_of("ZoneID"), any_of(mod.name), starts_with("value"), everything()) %>% 
        mutate(across(c(starts_with("difference")), ~ kableExtra::cell_spec(., color = case_when(. < 0 ~"red",
                                                                                                 . >= 0 ~ "green"))))
      
      diff_output <-  kbl(tm,"html", align = "c", booktabs = T, escape=FALSE) %>% 
        kable_styling(bootstrap_options = c("hover", "responsive", "bordered", "striped"),  full_width = T)
      
      return(diff_output)
      
    } else {
      message("Invalid output option")
      return(NA)
    }
  }
}

