create_proj_settings <- function(project) {
  #' Create FishSET project settings
  #' 
  #' This function creates the project settings file located in the project/doc folder. 
  #' 
  #' @importFrom grDevices dev.size
  #' @importFrom jsonlite write_json
  #' @details The project settings file contains the confidentiality settings,
  #' the file path to  the user output folder, plot size (width by height in 
  #' inches), and table names used in the shiny app (for logging purposes).  
  #' @seealso \code{\link{edit_proj_settings}}
  #' @export
  #' @keywords internal
  
  if (project_exists(project)) {
    
    set_file <- paste0(loc_doc(project), "project_settings.json")
    
    set_list <- 
      list(date_created = Sys.Date(),
           date_modified = Sys.Date(),
           confid_check = list(check = FALSE,
                               v_id = NULL,
                               rule = "n",
                               value = NULL),
           user_locoutput = locoutput(project),
           plot_size = grDevices::dev.size(),
           save_plot_rds = FALSE,
           tables = list(main = NULL, 
                         port = NULL,
                         spat = NULL,
                         grid = NULL,
                         aux = NULL)
      )
    
    jsonlite::write_json(set_list, set_file, pretty = TRUE, auto_unbox = FALSE,
                         null = "null", na = "string")
    invisible(TRUE)
  }
}


proj_settings_exists <- function(project) {
  
  #' Check if option file exists for a project
  #' @param project Project name.
  #' @return TRUE if project options file exists, FALSE if not.
  #' @export
  
  set_file <- paste0(loc_doc(project), "project_settings.json")
  
  file.exists(set_file)
}


get_proj_settings <- function(project, format = FALSE) {
  #' Retrieve project settings
  #' 
  #' @param project Name of project.
  #' @param format Logical, output project settings using \code{\link{pander}}. Useful
  #' for markdown documents. 
  #' @export
  #' @importFrom jsonlite fromJSON
  #' @importFrom pander pander
  #' @details The project settings file includes confidentiality settings, the 
  #' user output folder directory, and the default plot saving size. 
  
  if (project_exists(project)) {
    
    if (proj_settings_exists(project)) {
      
      set_file <- paste0(loc_doc(project), "project_settings.json")
      
      out <- jsonlite::fromJSON(set_file)
      
      if (format) pander::pander(out)
      else out
      
    } else {
      
      create_proj_settings(project)
      warning("Project settings file did not exists. New project settings file created.")
    }
    
  } else if (is.null(project)){
    
    return(NULL)
    
    } else {
    
    warning(paste("Project", project, "does not exist."))
    return(NULL)
  }
}


edit_proj_settings <- function(project, confid = NULL, user_out = NULL, 
                               tab_name = NULL, tab_type = NULL, plot_size = NULL,
                               save_plot_rds = NULL) {
  #' Edit project settings
  #' 
  #' Edit confidentiality settings, user output folder location, default plot 
  #' size, and tables currently used in the shiny app. 
  #' 
  #' @param project Name of project. 
  #' @param confid List containing new confidentiality settings. See 
  #' \code{\link{set_confid_check}}. 
  #' @param user_out Folder directory containing FishSET output. see 
  #'   \code{\link{set_user_locoutput}}. 
  #' @param tab_name Name of table loaded into shiny app. 
  #' @param tab_type Table type. Options include "main", "port", "spat", "grid",
  #'   and "aux". 
  #' @param plot_size Plot size (width, height) in inches. Must be numeric. 
  #' @param save_plot_rds Logical, whether to save plot as an RDS file in the 
  #' FishSETFolder ouput folder in addition to save as a PNG. This allows users 
  #' to edit plots at a later time. 
  #' @keywords internal
  #' @seealso \code{\link{create_proj_settings}} \code{\link{get_proj_settings}}
  #' @importFrom jsonlite write_json
  #' @export
  #' @examples 
  #' \dontrun{
  #' edit_project_settings("pollock", plot_size = c(5, 4))
  #' }
  
  if (project_exists(project) == FALSE) {
    
    warning(paste("Project", project, "does not exist."))
    
  } else if (proj_settings_exists(project) == FALSE) {
    
    create_proj_settings(project)
    warning("Project settings file did not exists. New project settings file created.")
    
  } else {
    
    p_set <- get_proj_settings(project)
    
    if (!is.null(confid)) {
      
      p_set$confid_check <- confid
    }
    
    if (!is.null(tab_name)) {
      
      p_set$tables[[tab_type]] <- tab_name
    }
    
    if (!is.null(plot_size)) {
      
      if (length(plot_size) == 2 & is.numeric(plot_size)) {
        
        p_set$plot_size <- plot_size
      }
    }
    
    if (!is.null(save_plot_rds) && is.logical(save_plot_rds)) {
      
      p_set$save_plot_rds <- save_plot_rds
    }
    
    if (!is.null(user_out) && dir.exists(user_out)) {
      
      p_set$user_locoutput <- user_out
    }
    
    if (!identical(p_set, get_proj_settings(project))) {
      
      p_set$date_modified <- Sys.Date()
      
      set_file <- paste0(loc_doc(project), "project_settings.json")
      
      jsonlite::write_json(p_set, set_file, pretty = TRUE, auto_unbox = FALSE, 
                           null = "null", na = "string")
    }
  }
}


set_user_locoutput <- function(loc_dir, project) {
  #'
  #' Set user folder directory
  #' @param loc_dir Local user directory
  #' @param project Name of project.
  #' @export
  #' @details This function saves the local user directory to the project settings file
  #'  with a valid folder directory. This directory path is used for inserting plots 
  #'  and tables from a folder outside the FishSET package into the FishSET RMarkdown 
  #'  Template. 
  #' @seealso \code{\link{insert_plot}} \code{\link{insert_table}} \code{\link{get_proj_settings}}

  
  if (project_exists(project)) {
    
    if (!dir.exists(loc_dir)) warning("Invalid directory.")
    else {
      if (!grepl("/$", loc_dir)) loc_dir <- paste0(loc_dir, "/")
      
      edit_proj_settings(project, user_out = loc_dir)
    }
  } else warning(paste("Project", project, "does not exist."))
}


get_user_locoutput <- function(project) {
  #'
  #'Print user folder directory
  #'
  #'@param project Name of project.
  #'@export
  #'@keywords internal
  #'@details This function prints the local user directory saved in the project
  #'  settings file. This directory path is used for inserting plots and tables 
  #'  from a folder outside the FishSET package into the FishSET RMarkdown Template. 
  #'  
  
  if (project_exists(project)) {
    
    locOutput <- get_proj_settings(project)$user_locoutput
    
    if (!is.null(locOutput)) {
      
      if (!dir.exists(locOutput)) warning("Invalid directory.")
      else locOutput
      
    } else {
      
      message("User directory unspecified. Run set_user_locouput() to desired folder directory.")
    }
  }
}