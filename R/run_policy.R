#' Run policy simulations
#'
#' Estimate redistributed fishing effort and welfare loss/gain from changes in policy or change in 
#' other factors that influence fisher location choice.
#' 
#' @param project Name of project
#' @param mod_name  Model name. Argument can be the name of the model or the name 
#'   can be pulled the `modelChosen` table. Leave \code{mod_name} empty to use 
#'   the name of the saved `best` model. If more than one model is saved, 
#'   \code{mod_name} should be the numeric indicator of which model to use.
#'   Use \code{table_view("modelChosen", project)} to view a table of saved models.
#' @param policy_name List of policy scenario names created in zone_closure function
#' @param betadraws Integer indicating the number of times to run the welfare simulation. 
#'   Default value is \code{betadraws = 1000}
#' @param marg_util_income For conditional and zonal logit models. Name of the coefficient to use 
#'   as marginal utility of income.
#' @param income_cost For conditional and zonal logit models. Logical indicating whether the 
#'   coefficient for the marginal utility of income relates to cost (\code{TRUE}) or 
#'   revenue (\code{FALSE}).
#' @param zone_dat Variable in primary data table that contains unique zone ID.
#' @param group_var Categorical variable from primary data table to group welfare outputs.
#' @param enteredPrice Price data. Leave as NULL if using price data from primary 
#'   dataset.
#' @param expected_catch Required for conditional logit (\code{logit_c}) model.
#'   Name of expected catch table to use. Can be the expected catch from the 
#'   short-term scenario (\code{short}), the medium-term scenario (\code{med}), 
#'   the long-term scenario (\code{long}), or the user-defined temporal parameters 
#'   (\code{user}).
#' @param use_scalers Input for \code{create_model_input()}. Logical, should data be normalized? 
#'   Defaults to \code{FALSE}. Rescaling factors are the mean of the numeric vector unless 
#'   specified with \code{scaler_func}.
#' @param scaler_func Input for \code{create_model_input()}. Function to calculate rescaling 
#'   factors.
#' @details \code{run_policy} is a wrapper function for \code{\link{model_prediction}} and 
#'   \code{\link{welfare_predict}}. \code{model_prediction} estimates redistributed fishing 
#'   effort after policy changes, and \code{welfare_predict} simulates welfare loss/gain.
#' @importFrom data.table fread
#' @export

### NOTES: Need to make sure closure areas and fishery zones match
### Have users rerun assignment column function and model.

#model_prediction
#  -create_model_input
#  -logit_predict
#  -epm_predict
#  -mixed_logit_predict
#  -predict_probability
#  welfare_predict
#  sim_welfare

run_policy <- function(project, 
                       mod_name = NULL, 
                       policy_name = NULL, 
                       betadraws = 1000, 
                       marg_util_income = NULL, 
                       income_cost = NULL, 
                       zone_dat = NULL, 
                       group_var = NULL,
                       enteredPrice = NULL, 
                       expected_catch = NULL, 
                       use_scalers = FALSE, 
                       scaler_func = NULL) {
  
  # Connect to SQL database
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  # Check closure file exists ---------------------------------------------------------------------
  # Read in zone closure information
  if (utils::file_test("-f", paste0(locoutput(project), 
                                    pull_output(project, type = 'zone', fun = 'closures')))) {
    
    closures <- yaml::read_yaml(paste0(locoutput(project), 
                                       pull_output(project, type = 'zone', fun = 'closures')))
    
    get_closures_out <- lapply(closures, function(x) {
      pol_nm <-  x$scenario
      
      if(pol_nm %in% policy_name){
        return(1)
      } else {
        return(0)
      }
    })
    
    closures_output <- closures[which(unlist(get_closures_out) == 1)]
    
  } else {
    stop('No policy scenario tables found. Run the zone_closure function.')
  }
  
  # Check that the model can be found -------------------------------------------------------------
  # Check if mod_name from input exists in the model output list
  # If the model does not exist, stop function and return error message
  if (table_exists(paste0(project, "ModelInputData"), project)) {
    
    mod.out <- model_out_view(project)
    
    for (i in seq_along(mod_name)) { # loop through each model
      result <- tryCatch(
        {
          index <- grep(mod_name[i], lapply(mod.out, "[[", "name"))
          if (length(index) == 0) stop(paste("Model output for", mod_name[i], " does not exist."))
          mod.out[[index]]
        },
        error = function(e) {
          message("Error: ", e$message)
          NULL
        }
      )
    } 
    
  } else {
    stop('Model table(s) does not exist. Run model functions.')
    
  }
  
  # Run model_prediction function -----------------------------------------------------------------
  model_prediction(project = project, 
                   mod_name = mod_name,
                   closures = closures_output, 
                   enteredPrice = enteredPrice,
                   use_scalers = use_scalers, 
                   scaler_func = scaler_func)
  
  # Run welfare predict ---------------------------------------------------------------------------
  theta_output <-  welfare_predict(project = project, 
                                   mod_name = mod_name, 
                                   closures = closures_output, 
                                   betadraws = betadraws,
                                   marg_util_income = marg_util_income, 
                                   income_cost = income_cost,
                                   expected_catch = expected_catch, 
                                   enteredPrice = enteredPrice)
  
  # Generate tables and plots ---------------------------------------------------------------------
  outputs_welf <-  welfare_outputs(project = project, 
                                   mod_name = mod_name, 
                                   closures = closures_output,
                                   betadraws = betadraws, 
                                   zone_dat = zone_dat, 
                                   group_var = group_var)
  
  # log run_policy function call ------------------------------------------------------------------
  run_policy_function <- list()
  run_policy_function$functionID <- "run_policy"
  run_policy_function$args <- list(project, 
                                   mod_name, 
                                   enteredPrice, 
                                   expected_catch, 
                                   use_scalers, 
                                   scaler_func)
  run_policy_function$kwargs <- list()
  log_call(project, run_policy_function)
  
  return(list(outputs_welf, theta_output))
}
