delete_models <- function(project, model.names, delete_nested = FALSE) {
  
  # check if user supplied specific nested model name (ex. logit_c_mod1.exp1) 
  # or general model name (logit_c_mod1). If general, check if delete_nested = TRUE.
  # If TRUE all nested models will be deleted. If nested model name provided ignore
  # delete_nested. 
  
  mdf_tab_nm <- paste0(project, 'ModelInputData')
  mot_tab_nm <- paste0(project, "ModelOut")
  mdf_exists <- table_exists(mdf_tab_nm, project)
  mot_exists <- table_exists(mot_tab_nm, project)
  update_mot <- FALSE
  
  if (!mdf_exists) {
    
    stop('Model design table does not exist.', call. = FALSE)
  }
  
  mdf <- model_design_list(project)
  mdf_n <- model_names(project)
  # nested names
  mdf_nn <- lapply(mdf, function(x) {
    
    if (!is.null(x$expectcatchmodels)) {
      # if exp matrices included, created full model name
      vapply(x$expectcatchmodels, function(y) {
        
        paste0(c(x$mod.name, y), collapse = '.')
      }, character(1))
      # otherwise, return unnested name
    } else x$mod.name
    
  })
  
  names(mdf_nn) <- mdf_n
  
  is_nested <- vapply(mdf_n, function(x) !x %in% mdf_nn[[x]] & length(mdf_nn[[x]]) > 1, logical(1))

  # model output table ----
  
  if (mot_exists) {
    
    # TODO: have model output funcs take single arg (project)
    mot <- model_out_view(paste0(project, 'ModelOut'), project)
    
    mot_n <- vapply(mot, function(x) x$name, character(1))
    
    # delete unnested models
    unnested <- model.names[model.names %in% mot_n]
    mot_ind <- vector('integer')
    
    if (length(unnested) > 0) {
      
      mot_ind <- c(mot_ind, which(mot_n %in% unnested))
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
      update_mot <- TRUE
    }
  }
  
  # model design file ----
    
  # possible scenarios:
  # 1) delete unnested model
  # 2) delete specific nested model
  # 3) delete model and all its nested models (check delete_nested = TRUE)
  # 4) typo/non-existing model entered

  # check for nested models
  nest_n <- names(is_nested)[is_nested]
  
  # throw error if delete_nest = FALSE
  if (any(model.names %in% nest_n) & !delete_nested) {
    
    stop('')
  }
  
  # delete specific nested models
  nested_mods_specifc <- model.names[model.names %in% unlist(mdf_nn[nest_n])]
  nested_mods_general <- model.names[model.names %in% nest_n]
  unnested_mods <- model.names[model.names %in% mdf_n[!mdf_n %in% nest_n]]
  
  # delete specified nested models
  if (length(nested_mods_specifc) > 0) {
    
    # split model name by periods, which is used when one or more exp matrices are used 
    nest_list <- strsplit(nested_mods_specifc, '\\.')
    n_l <- list()
    # create a list of which nested models to remove
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
  
  # remove models
  if (length(c(nested_mods_general, unnested_mods))) {
    
    m_ind <- which(mdf_n %in% c(nested_mods_general, unnested_mods))
    mdf <- mdf[-m_ind]
  }
  
  # save updated tables ----
  
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  ## MDF ----
  # TODO: save mdf and mot independently
  
  table_remove(mdf_tab_nm, project)

  DBI::dbExecute(fishset_db, paste("CREATE TABLE", mdf_tab_nm, "(ModelInputData MODELINPUTDATA)"))
  DBI::dbExecute(fishset_db, paste("INSERT INTO", mdf_tab_nm, "VALUES (:ModelInputData)"),
                 params = list(ModelInputData = list(serialize(mdf, NULL))))

  ## MOT ----

  if (update_mot) {
    
    table_remove(mot_tab_nm, project)
    
    DBI::dbExecute(fishset_db, paste("CREATE TABLE", mot_tab_nm, "(data ModelOut)"))
    DBI::dbExecute(fishset_db, paste("INSERT INTO", mot_tab_nm, "VALUES (:data)"),
                   params = list(data = list(serialize(mot, NULL))))
  }

  # log function

  delete_models_function <- list()
  delete_models_function$functionID <- "delete_models"
  delete_models_function$args <- list(project, model.names, delete_nested)

  log_call(project, delete_models_function)
  
  message('The following models have been deleted: ', paste(model.names, collapse = ', '))
}