#' Runs policy scenario functions
#' Checks policy scenario exists
#' Runs predict_probability function
#' Runs welfare_predict function
#' @param project Name of project
#' @param mod.name Name of saved model to use
#' @param tac Percent of tac allowed in closure. Value must be between 0 and 1. If more than one closure scenario is 
#'   included, tac should be a string. Percent of allowable tac is assumed to be 0 if not provided.
#' @param expected.catch.name Required for conditonal logit (\code{logit_c}) model. 
#'   Name of expected catch table to use. 
#'    Can be the expected catch from the short-term scenario (\code{short}), the medium-term scenario (\code{med}), the 
#'    long-term scenario (\code{long}), or the user-defined temporal parameters (\code{user}).
#' @param gridfile Required only if \code{zone.closure} is not defined. spatial data file containing information on fishery management or regulatory zones boundaries.
#'   Shape, json, geojson, and csv formats are supported. geojson is the preferred format. json files must be converted
#'   into geoson. This is done automatically when the file is loaded with \code{\link{read_dat}} with \code{is.map} set to true.
#'   \code{gridfile} cannot, at this time, be loaded from the FishSET database. \cr
#' @param cat Required only if \code{zone.closure} is not defined. Variable in \code{gridfile} that identifies the individual areas or zones.
#' @param lon.grid Required only if \code{zone.closure} is not defined and \code{gridfile} is a csv file.
#' @param lat.grid Required only if \code{zone.closure} is not defined and \code{gridfile} is a csv file.


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

run_policy <- function(project, mod.name=NULL, tac = 0, expected.catch.name=NULL, 
                       gridfile = NULL, cat=NULL, lon.grid=NULL, lat.grid=NULL){
  
  #1. Check closure file exists
  #Read in zone closure information
  if(utils::file_test("-f",paste0(locoutput(project), pull_output(project, type='zone', fun='closures')))) {
    closures <- yaml::read_yaml(paste0(locoutput(project), pull_output(project, type='zone', fun='closures')))
  } else {
    # Open zone closure function in file not found  
    if(!is.null(gridfile) && !is.null(cat)){
    zone_closure(project=project, gridfile=gridfile, cat=cat, lon.grid=lon.grid, lat.grid=lat.grid)
    closures <- yaml::read_yaml(paste0(locoutput(project), pull_output(project, type='zone', fun='closures')))
    } else {
      warning('Closure table not found. Run the zone_closure function.')
    }
  }
  
  #2. Force users to look at closure file
    #Need to create a map
    # Message to look at zones and check for match/mismatch
    # Message to inform users what to do about mismatch
  
  #3. Run model_prediction function
  model_prediction(project=project, mod.name=mod.name, expected.catch.name=expected.catch.name)
  
  #4. Output should be temporarily saved to pass to the next function
  
  #5. Run welfare predict
    welfare_predict()
    
  #6. Save output and return tables and plots
  
  

    
  
  
}
