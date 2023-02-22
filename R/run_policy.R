#' Runs policy scenario functions
#'
#' Checks policy scenario exists. Runs predict_probability function. Runs 
#' \code{\link{welfare_predict}} function
#' @param project Name of project
#' @param mod.name  Model name. Argument can be the name of the model or the name 
#'   can be pulled the `modelChosen` table. Leave \code{mod.name} empty to use 
#'   the name of the saved `best` model. If more than one model is saved, 
#'   \code{mod.name} should be the numeric indicator of which model to use.
#'   Use \code{table_view("modelChosen", project)} to view a table of saved models.
#' @param enteredPrice Price data. Leave as NULL if using price data from primary 
#'   dataset.
#' @param expected.catch Required for conditional logit (\code{logit_c}) model.
#'   Name of expected catch table to use. Can be the expected catch from the 
#'   short-term scenario (\code{short}), the medium-term scenario (\code{med}), 
#'   the long-term scenario (\code{long}), or the user-defined temporal parameters 
#'   (\code{user}).
#' @details \code{run_policy} is a wrapper function that calls the policy and 
#'   welfare subfunctions. Policy closure scenarios must be defined using the 
#'   \code{\link[FishSET]{zone_closure}} function. The function also requires parameter
#'   estimates and model data from one model.


### NOTES: Need to make sure closure areas and fishery zones match
### Have users rerun assignment column function and model.

#model_prediction
#  -create_model_input
#  -logit_predict
#  -epm_predict
#  -mixed_logit_predict
#  -predict_probability
#welfare_predict
#  sim_welfare

run_policy <-
  function(project,
           mod.name = NULL,
           enteredPrice = NULL,
           expected.catch = NULL) {
    fishset_db <-
      DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project))
    on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
    
    #1. Check closure file exists
    #Read in zone closure information
    if (utils::file_test("-f", paste0(
      locoutput(project),
      pull_output(project, type = 'zone', fun = 'closures')
    ))) {
      closures <-
        yaml::read_yaml(paste0(
          locoutput(project),
          pull_output(project, type = 'zone', fun = 'closures')
        ))
    } else {
      stop('No policy scenario tables found. Run the zone_closure function.')
    }
    
    
    #2 Check that the model can be found
    
    #Get model name
    if (is.null(mod.name) || is.numeric(mod.name)) {
      # check that the model chosen table exists
      if (table_exists('modelChosen', project)) {
        modtemp <- table_view('modelChosen', project)$model
        if (length(modtemp) > 1) {
          if (!is.numeric(mod.name))
            stop(
              'More than one mode exists in the modelChosen table. See table_view("modelChosen", project) and rerun function with either the model name or the numeric indicator of the model.'
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
      modname <- mod.name
    }
    
    #2. Force users to look at closure file
    # This is done in the zone closure tab
    
    #3. Run model_prediction function
    model_prediction(
      project = project,
      mod.name = modname,
      expected.catch = expected.catch,
      enteredPrice
    )
    
    #4. Output should be temporarily saved to pass to the next function
    #pOutput <- unserialize(DBI::dbGetQuery(fishset_db, paste0("SELECT PredictOutput FROM ", project, "predictoutput LIMIT 1"))$PredictOutput[[1]])
    
    #5. Run welfare predict
    welfareout <-
      welfare_predict(
        project = project,
        mod.name = modname,
        expected.catch = expected.catch,
        enteredPrice = enteredPrice
      )
    
    #6. Save output and return tables and plots
  
  

    
  
  
}
