empty_meta_list <- function() {
  #' Create an empty meta list
  #' Used to create a new metadata entry. 
  #' @keywords internal
  #' @export
  
  list(date_added = Sys.Date(),
       type = character(0),
       meta = list(author = character(0),
                   date_created = character(0),
                   date_modified = character(0),
                   confidentiality = character(0),
                   version = character(0),
                   description = list(about = character(0),
                                      source = character(0),
                                      collection_method = character(0),
                                      intended_use = character(0),
                                      column_desc = character(0),
                                      time = character(0),
                                      geo = character(0),
                                      species = character(0)),
                   contact_info = list(person = character(0),
                                       org = character(0),
                                       address = character(0),
                                       phone = character(0),
                                       email = character(0)),
                   license = character(0),
                   citation = character(0),
                   other = character(0)),
       raw = character(0))
}


col_desc <- function(dat) {
  #' Create a list of column names
  #' Used to create a new metadata entry.
  #' @param dat Dataframe
  #' @keywords internal
  #' @export
  
  if (is.data.frame(dat)) {
    
    out <- lapply(dat, names)
    
  } else if (is.matrix(dat)) {
    
    if (length(colnames(dat))) {
      
      nc <- ncol(dat)
      out <- vector("list", nc)
      names(out) <- colnames(dat)
      
    } else {
      
      out <- character(0)
    }
    
  } else {
    
    nms <- names(dat)
    out <- vector("list", length(nms))
  }
  
  out
}


meta_list <- function(type, dat = NULL) {
  #' Create a meta list with column description section
  #' Used to create a new metadata entry.
  #' @param type String
  #' @param dat Dataset
  #' @keywords internal
  #' @export
  #'

  out <- empty_meta_list()
  
  out$type <- type
  
  if (!is.null(dat)) {
    
    out$meta$description$col_desc <- col_desc(dat)
  } 
  
  out
}


meta_file_exists <- function(project) {
  #' Check if meta file exists for a project
  #' @param project Project name.
  #' @return TRUE if project meta file exists, FALSE if not.
  #' @export
  
  m_dir <- paste0(locproject(), "/", project, "/doc/meta_log.json")
  
  file.exists(m_dir)
}


meta_log_call <- function(project, meta, dataset = NULL, tab_name, tab_type, 
                          overwrite = FALSE, raw = TRUE) {
  #' Log metadata to project folder
  #' Meta data is saved to the metadata `meta_log.json` file in a project's `doc/` directory. 
  #' @param project Project name.
  #' @param meta Meta data list to be saved.
  #' @param dataset A data object. Used to create the column description section
  #'   of the metadata log entry. If \code{NULL}, the section is left empty. This
  #'   may be the case if saving the raw metadata. To add column descriptions, 
  #'   use the \code{meta_form} app.
  #' @param tab_name The table name as it appears in the FishSET Database (e.g.
  #'   "projectMainDataTable" if the main table).
  #' @param tab_type The table type. Options include "main", "spat" (spatial), 
  #'   "port", "grid" (gridded), and "aux" (auxiliary).
  #' @param overwrite Logical, whether to save over existing metadata entry.
  #' @param raw Logical, whether raw metadata file is being saved. 
  #' @keywords internal
  #' @export
  #' @importFrom jsonlite read_json toJSON
  #' 
  
  meta_names <- NULL
  
  if (project_exists(project) == FALSE) {
    
    warning("Project \"", project, "\" does not exist.")
    return(invisible(FALSE))
    
  } else {
    
    m_file <- paste0(locproject(), "/", project, "/doc/meta_log.json")
    
    if (meta_file_exists(project) == FALSE) {
      
      metaBody <- list()
      
      metaBody$rundate <- Sys.Date()
      metaBody$project <- project
      metaBody$table <- list()
      
    } else {
      
      metaBody <- jsonlite::read_json(m_file)
      meta_names <- names(metaBody$table)
    }
    
    tab_exists <- tab_name %in% meta_names
    
    if (raw) {
      
      if (tab_exists == FALSE) {
        
        metaBody$table[[tab_name]] <- meta_list(tab_type, dataset)
        metaBody$table[[tab_name]]$raw <- meta
        
      } else {
        
        raw_exists <- length(metaBody$table[[tab_name]]$raw)
        
        if (raw_exists & overwrite == FALSE) {
          
          warning(paste("Metadata not saved. Raw meta for", tab_name, 
                        "exists and 'overwrite = FALSE'."))
          return(invisible(FALSE))
          
        } else {
          
          metaBody$table[[tab_name]]$raw <- meta
        }
      }
      
    } else {
      
      if (tab_exists == FALSE) {
        
        metaBody$table[[tab_name]] <- meta_list(tab_type, dataset)
        metaBody$table[[tab_name]]$meta <- meta
        
      } else {
        
        meta_exists <- length(metaBody$table[[tab_name]]$meta) > 0
        
        if (meta_exists & overwrite == FALSE) {
          
          warning(paste("Metadata not saved. Metadata for", tab_name, 
                        "exists and 'overwrite = FALSE'."))
          return(invisible(FALSE))
          
        } else {
          
          metaBody$table[[tab_name]]$meta <- meta
        }
      }
    }
    
    meta_log <- jsonlite::toJSON(metaBody, pretty = TRUE, auto_unbox = FALSE, 
                                 null = "null", na = "string")
    
    write(meta_log, m_file)
    message(paste(tab_name, "metadata saved."))
    invisible(TRUE)
  }
}