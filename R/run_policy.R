#' Runs policy scenarios
#'
#' Estimate redistributed fishing effort and welfare loss/gain from changes in policy or change in other factors that
#' influence fisher location choice.
#' 
#' @param project Name of project
#' @param mod.name  Model name. Argument can be the name of the model or the name 
#'   can be pulled the `modelChosen` table. Leave \code{mod.name} empty to use 
#'   the name of the saved `best` model. If more than one model is saved, 
#'   \code{mod.name} should be the numeric indicator of which model to use.
#'   Use \code{table_view("modelChosen", project)} to view a table of saved models.
#' @param marg_util_income For conditional and zonal logit models. Name of the coefficient to use as 
#'   marginal utility of income.
#' @param income_cost For conditional and zonal logit models. Logical indicating whether the coefficient
#'    for the marginal utility of income relates to cost (\code{TRUE}) or revenue (\code{FALSE}).
#' @param enteredPrice Price data. Leave as NULL if using price data from primary 
#'   dataset.
#' @param expected.catch Required for conditional logit (\code{logit_c}) model.
#'   Name of expected catch table to use. Can be the expected catch from the 
#'   short-term scenario (\code{short}), the medium-term scenario (\code{med}), 
#'   the long-term scenario (\code{long}), or the user-defined temporal parameters 
#'   (\code{user}).
#' @param use.scalers Input for \code{create_model_input()}. Logical, should data be normalized? Defaults to \code{FALSE}. Rescaling factors are the mean of the 
#' numeric vector unless specified with \code{scaler.func}.
#' @param scaler.func Input for \code{create_model_input()}. Function to calculate rescaling factors.
#' @details \code{run_policy} is a wrapper function for \code{\link{model_prediction}} and \code{\link{welfare_predict}}.
#'    \code{model_prediction} estimates redistributed fishing effort after policy changes, and \code{welfare_predict}
#'    simulates welfare loss/gain.
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

run_policy <- function(project, mod.name = NULL, marg_util_income = NULL, income_cost = NULL, enteredPrice = NULL, expected.catch = NULL, use.scalers = FALSE, scaler.func = NULL) {
  
  # Connect to SQL database
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  ##
  # 1. Check closure file exists ----
  ##
  # Read in zone closure information
  if (utils::file_test("-f", paste0(locoutput(project), pull_output(project, type = 'zone', fun = 'closures')))) {
    closures <- yaml::read_yaml(paste0(locoutput(project), pull_output(project, type = 'zone', fun = 'closures')))
    
  } else {
    stop('No policy scenario tables found. Run the zone_closure function.')
    
  }
  
  
  ##
  # 2. Check that the model can be found ----
  ##
  # Get model name
  if (is.null(mod.name) || is.numeric(mod.name)) {
    # check that the model chosen table exists
    if (table_exists('modelChosen', project)) {
      modtemp <- table_view('modelChosen', project)$model
      if (length(modtemp) > 1) {
        if (!is.numeric(mod.name))
          stop(
            'More than one model exists in the modelChosen table. See table_view("modelChosen", project) and rerun function with either the model name or the numeric indicator of the model.'
          )
        if (is.numeric(mod.name))
          modname <- modtemp[mod.name]
      } else
        modname <- modtemp[1]
    } else {
      stop(
        'modelChosen table does not exist. Specify model name or select the model using select_model(project).'
      )
    }
  } else {
    exists <- grep(mod.name, project_files(project))
    
    if (length(exists) > 0){
      modname <- mod.name  
      
    } else {
      stop('modelChosen table does not exist. Specify model name or select the model using select_model(project).')
    }
  }
  
  
  ##
  # 3. Run model_prediction function ----
  ##
  model_prediction(project = project, mod.name = modname,
                   closures = closures, enteredPrice = enteredPrice,
                   use.scalers = use.scalers, scaler.func = scaler.func)
  
  
  ##
  # 4. Run welfare predict ----
  ##
  welfare_predict(project = project, mod.name = modname, closures = closures,
                  marg_util_income = marg_util_income, income_cost = income_cost,
                  expected.catch = expected.catch, enteredPrice = enteredPrice)
  
  
  ##
  # 6. Generate tables and plots
  ## 
  welfare <- fread(paste0(locoutput(project), "welfare_output.csv"))
  prc_welfare <- fread(paste0(locoutput(project), "prcwelfare_output.csv"))
  
  
  ##
  # 7. log run_policy function call ----
  ##
  run_policy_function <- list()
  run_policy_function$functionID <- "run_policy"
  run_policy_function$args <- list(project, mod.name, enteredPrice, expected.catch, use.scalers, scaler.func)
  run_policy_function$kwargs <- list()
  log_call(project, run_policy_function)
}
