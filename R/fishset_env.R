create_fishset_env <- function() {
  
  #' Create FishSET info environment
  #' 
  #' This function creates an environment named \code{fishset_env} containing project 
  #' information such as table names and confidentiality settings. It is
  #' located in the global environment. 
  #' 
  #' @importFrom rlang new_environment global_env
  #' @details \code{create_fishset_env} is run when the FishSET app is opened. 
  #'   \code{fishset_env} can be reset by running \code{create_fishset_env} as 
  #'   well. 
  #' @seealso \code{\link{delete_fishset_env}}
  #' @export
  
  assign("fishset_env",
         value = rlang::new_environment(data = list(
                                        date = Sys.Date(),
                                        dat_name = NULL, 
                                        port_name = NULL,
                                        spat_name = NULL,
                                        grid_name = NULL,
                                        aux_name = NULL,
                                        confid_check = NULL,
                                        confid_cache = NULL)),
         pos = rlang::global_env())
}

fishset_env_exists <- function() {
  
  #' Check whether fishset_env exists
  #' 
  #' @return `TRUE` if `fishset_env` exists, `FALSE` if not.
  #' @importFrom rlang global_env 
  #' @export
  
  fish <- exists("fishset_env", envir = rlang::global_env(), 
                 mode = "environment")
  if (fish) TRUE
  else {
    #message("fishset_env does not exists, run create_fishset_env().")
    FALSE
  }
}

edit_fishset_env <- function(name, value) {
  
  #' Add or edit an object in fishset_env
  #' 
  #' Create a new object or edit an existing one using the object name (as a string)
  #' and a value.
  #' 
  #' @param name String, the name of the new or existing object.
  #' @param value The value for the new or reassigned object. 
  #' @importFrom rlang env_poke
  #' @seealso \code{\link{show_fishset_env}}
  #' @export
  #' @examples 
  #' \dontrun{
  #' edit_fishset_env("date", Sys.Date())
  #' }
  
  if (fishset_env_exists()) {
    
    rlang::env_poke(fishset_env, name, value)
  }
}

get_fishset_env <- function(name) {
  
  #' Return an object from fishset_env
  #' 
  #' @param name String, the name of a `fishset_env` object.
  #' @importFrom rlang env_get
  #' @export 
  #' @seealso \code{\link{show_fishset_env}}
  #' @examples 
  #' \dontrun{
  #' get_fishset_env("dat_name")
  #' }
  
  if (fishset_env_exists()) {
    
    rlang::env_get(fishset_env, name, default = NULL)
  }
}

show_fishset_env <- function() {
  
  #' Display all objects in fishset_env
  #' 
  #' @description 
  #' The FishSET environment (\code{fishset_env}) contains project information and 
  #' settings. The contents of \code{fishset_env} include: 
  #' \itemize{
  #' \item \code{date}: the date \code{fishset_env} was created.
  #' 
  #' \item \code{dat_name}: the name of the main data table, e.g. `pollockMainDataTable`, 
  #'   currently loaded.
  #' 
  #' \item \code{port_name}: the name of the project port table.
  #' 
  #' \item \code{spat_name}: the name of the spatial data table(s).
  #' 
  #' \item \code{grid_name}: the name of the gridded data table(s).
  #' 
  #' \item \code{aux_name}: the name of the auxiliary data table.
  #' 
  #' \item \code{confid_check}: the confidentiality settings. See 
  #'   \code{\link{set_confid_check}} for details.
  #'   
  #' \item \code{confid_cache}: the suppression conditions, or check tables, used to suppress
  #'   values in summary tables based on the parameters in \code{confid_check}.
  #' }
  #' @return A list of objects in \code{fishset_env}.
  #' @importFrom rlang env_print env_get_list env_names
  #' @export
  
  if (fishset_env_exists()) {
    
    rlang::env_print(fishset_env)
    
    rlang::env_get_list(fishset_env, rlang::env_names(fishset_env), default = NULL)
  }
}

delete_fishset_env <- function() {
  
  #' Delete fishset_env
  #' 
  #' Remove \code{fishset_env} from the global environment.
  #' 
  #' @importFrom rlang global_env
  #' @export
  
  if (fishset_env_exists()) {
    
    rm(fishset_env, envir = rlang::global_env())
  }
}
