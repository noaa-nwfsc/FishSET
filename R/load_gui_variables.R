#' 
#' @title load_gui_variables
#'
#' @description A helper function to load selected variables from an RDS file in the 
#' FishSET GUI.
#'
#' @param project_name The name of the current project.
#' @param folderpath The file path to the project's root folder.
#'
#' @return A list of saved variables if the file exists, otherwise returns NULL.
load_gui_variables <- function(project_name, folderpath) {
  
  # Construct the file path for the saved variables
  file_name <- paste0(project_name, "SavedVariables.rds")
  file_path <- file.path(folderpath, project_name, "data", file_name)
  
  # Check if the file exists and normalize the path
  if (file.exists(file_path)) {
    file_path <- suppressWarnings(normalizePath(file_path))
    return(readRDS(file_path))
  } else {
    return(NULL)
  }
}