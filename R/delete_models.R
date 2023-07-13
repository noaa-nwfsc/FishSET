delete_models <- function(project, model.names, delete_nested = FALSE) {
  
  # check if user supplied specific nested model name (ex. logit_c_mod1.exp1) 
  # or general model name (logit_c_mod1). If general, check if delete_nested = TRUE.
  # If TRUE all nested models will be deleted. If nested model name provided ignore
  # delete_nested. 
  
  mdf_tab_nm <- paste0(project, 'ModelInputData')
  mot_tab_nm <- paste0(project, "ModelOut")
  mdf_exists <- table_exists(mdf_tab_nm, project)
  mot_exists <- table_exists(mot_tab_nm, project)
  
  if (!mdf_exists) {
    
    stop('Model design table does not exist.', call. = FALSE)
  }
  
  mdf <- mdf_copy <- model_design_list(project)
  mdf_n <- model_names(project)
  # nested names
  mdf_nn <- lapply(mdf, function(x) {
    
    if (!is.null(x$expectcatchmodels)) {
      
      vapply(x$expectcatchmodels, function(y) {
        
        paste0(c(x$mod.name, y), collapse = '.')
      }, character(1))
      
    } else x$mod.name
    
  })
  
  names(mdf_nn) <- mdf_n
  
  is_nested <- vapply(mdf_n, function(x) !x %in% mdf_nn[[x]], logical(1))

  # model output table ----
  
  if (mot_exists) {
    
    # TODO: have model output funcs take single arg (project)
    mot <- mot_copy <- model_out_view(paste0(project, 'ModelOut'), project)
    
    mot_n <- vapply(mot, function(x) x$name, character(1))
    
    # delete unnested models
    unnested <- model.names[model.names %in% mot_n]
    mot_ind <- vector('integer')
    
    if (length(unnested) > 0) {
      
      mot_ind <- c(mot_ind, which(unnested %in% mot_n))
    }
    
    # delete nested models
    if (any(!model.names %in% mot_n)) {
      
      nomatch <- model.names[!model.names %in% mot_n]
      
      # If model name isn't in MOT:
      # 1) user entered general model name 
      # 2) user hasn't run the model yet and is only deleting it from MDF
      # 3) user mistypes model name
      for (i in nomatch) {
        
        if (i %in% mdf_n) {
          # check if nested models were run
          if (all(mdf_nn[[i]] %in% mot_n)) {
            
            if (delete_nested) {
              
              # index of models to remove
              mot_ind <- c(mot_ind, which(mot_n %in% mdf_nn[[i]]))
              
            } else {
              
              stop(i, ' contains nested models. Set delete_nested = TRUE to delete.',
                   call. = FALSE)
            }
          } # do nothing, model not run yet
          
        } else {
          
          stop(i, ' does not exist.', call. = FALSE)
        }
      }
    }
    
    if (length(mot_ind) > 0) {
      
      mot <- mot[-mot_ind]
    }
  }
  
    # Note: this will fail to detect nested models that haven't been run
  # check for unlisted models
  
  # does_not_exist <- vapply(model.names, function(x) !x %in% c(mdf_n, mot_n), logical(1))
  # 
  # if (any(does_not_exist)) {
  #   
  #   stop('The following models could not be found: ', 
  #        paste(names(does_not_exist)[does_not_exist], collapse = ', '), 
  #        call. = FALSE)
  # }
  
  # model design file ----
  
    
    browser()
  
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
  browser()
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  ## MDF ----
 # TODO: save mdf and mot independently
  
  table_remove(mdf_tab_nm, project)

  DBI::dbExecute(fishset_db, paste("CREATE TABLE", mdf_tab_nm, "(ModelInputData MODELINPUTDATA)"))
  DBI::dbExecute(fishset_db, paste("INSERT INTO", mdf_tab_nm, "VALUES (:ModelInputData)"),
                 params = list(ModelInputData = list(serialize(mdf, NULL))))

  # MOT ----

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