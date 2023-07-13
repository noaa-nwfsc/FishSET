delete_models <- function(project, model.names, delete_nested = FALSE) {
  
  mdf <- model_design_list(project)
  # TODO: have model output funcs take single arg (project)
  mot <- model_out_view(paste0(project, 'ModelOut'), project)
  
  mot_n <- vapply(mot, function(x) x$name, character(1))
  mdf_n <- model_names(project)
  
  # check for unlisted models
  
  does_not_exist <- vapply(model.names, function(x) !x %in% c(mdf_n, mot_n), logical(1))
  
  if (any(does_not_exist)) {
    
    stop('The following models could not be found: ', 
         paste(names(does_not_exist)[does_not_exist], collapse = ', '), 
         call. = FALSE)
  }
  
  # check if user supplied specific nested model name (ex. logit_c_mod1.exp1) 
  # or general model name (logit_c_mod1). If general, check if delete_nested = TRUE.
  # If TRUE all nested models will be deleted. If nested model name provided ignore
  # delete_nested. 
  
  is_nested <- vapply(model.names, function(x) (!x %in% mdf_n) & (x %in% mot_n), logical(1))
  # alt
  is_nested <- vapply(model.names, function(x) grepl('\\.', model.names), logical(1))
  
  mot_match <- lapply(model.names[!is_nested], function(x) grep(paste0('^', x), mot_n))
  
  # TODO: check if model has/hasn't been run yet. If not, only delete from MDF
  if (length(mot_match) > 0) {
    
    lapply(mot_match, function(x) {
      
      if (length(x) > 1 & !delete_nested) {
        
        stop('Nested models found. To delete nested models set delete_nested = TRUE.', 
             call. = FALSE)
      }
    })
    # make sure that mixed batch of nested/non-nested can be deleted properly
  } 
    
  mot_match <- c(mot_match, which(mot_n %in% model.names))

  
  # model output table ----
  
  mot <- mot[-unlist(mot_match)]
  
  # model design file ----
  
  # delete specified nested models
  if (any(is_nested)) {
    
    nest_n <- names(is_nested)[is_nested]
    nest_list <- strsplit(nest_n, '\\.')
    n_l <- list()
    # create a list of which nested models to remove
    # TODO: if user wants to delete all nested models then
    # they should use the general name
    for (i in length(nest_list)) {
      
      n_l[[i]] <- nest_list[[i]][-1]
      names(n_l)[[i]] <- nest_list[[i]][1]
    }
    
    # delete from model design file
    for (i in seq_along(n_l)) {
      # model index
      m_ind <- which(mdf_n == names(n_l)[i])
      # nested model index
      n_ind <- vapply(mdf[[m_ind]]$expectcatchmodels, 
                      function(x) identical(x, n_l[[i]]), # all.equal()?
                      logical(1))
      
      # remove nested model from list
      mdf[[m_ind]]$expectcatchmodels <- mdf[[m_ind]]$expectcatchmodels[-which(n_ind)]
    }
  }
  
  # remove non-nested models
  if (any(!is_nested)) {
    
    non_nest <- names(is_nested)[!is_nested]
    m_ind <- which(mdf_n %in% non_nest)
    
    mdf <- mdf[-m_ind]
  }
  
  # save updated tables ----
  
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  ## MDF ----
 
  mdf_tab_nm <- paste0(project, 'ModelInputData')
  table_remove(mdf_tab_nm, project)

  DBI::dbExecute(fishset_db, paste("CREATE TABLE", mdf_tab_nm, "(ModelInputData MODELINPUTDATA)"))
  DBI::dbExecute(fishset_db, paste("INSERT INTO", mdf_tab_nm, "VALUES (:ModelInputData)"),
                 params = list(ModelInputData = list(serialize(mdf, NULL))))

  # MOT ----

  mot_tab_nm <- paste0(project, "ModelOut")

  table_remove(mot_tab_nm, project)

  DBI::dbExecute(fishset_db, paste("CREATE TABLE", mot_tab_nm, "(data ModelOut)"))
  DBI::dbExecute(fishset_db, paste("INSERT INTO", mot_tab_nm, "VALUES (:data)"),
                 params = list(data = list(serialize(mot, NULL))))

  # log function

  delete_models_function <- list()
  delete_models_function$functionID <- "delete_models"
  delete_models_function$args <- list(project, model.names, delete_nested)

  log_call(project, delete_models_function)
  
  message('The following models have been deleted: ', paste(model.names, collapse = ', '))
}