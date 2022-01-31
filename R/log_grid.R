get_grid_log <- function(project) {
  #' Retrieve grid log file
  #' 
  #' Retrieves the grid log file for a project. The grid log shows which grid 
  #' files are currently saved to the project data folder.
  #' @param project Name of project.
  #' @importFrom jsonlite read_json
  #' @export
  #' @details The grid log is a list containing information about the grid files 
  #'   currently saved to the project data folder. Each grid entry contains three 
  #'   fields: \code{grid_name}, \code{closure_name}, and \code{combined_areas}. 
  #'   \code{grid_name} is the name of the original grid object. If the other two
  #'   fields are empty, this means that the grid file has not been altered and 
  #'   is the same as the original. \code{closure_name} is the name of a second 
  #'   grid file containing closure areas that were combined with \code{grid_name}.
  #'    \code{combined_areas} are the names/IDs of the closures areas from the 
  #'    closure grid file that were combined with \code{grid_name}. 
  #' @examples 
  #' \dontrun{
  #' get_grid_log("pollock")
  #' }
  
  filename <- paste0(loc_data(project), "spat/", project, "_grid_info.json")
  
  if (file.exists(filename)) {
    
    jsonlite::read_json(filename, simplifyVector = TRUE)
    
  } else return(NULL)
}


unique_grid <- function(project, grid_info, ind = TRUE) {
  #' Check for unique grid files
  #' 
  #' This function determines whether a grid file should be saved to the project
  #' data folder based on the values in \code{grid_info}. If a match is found, 
  #' indicating that an identical grid file has already been saved, it is deleted.
  #' If no match is found the grid file is saved. 
  #' 
  #' @param project Name of project.
  #' @param grid_info List of grid characteristics used to determine whether a
  #'   grid should be saved to the project data folder. 
  #' @param ind Logical, whether to return an index of unique grid or return a 
  #'   single logical value.
  #' @return \code{TRUE} if a grid is unique (i.e. has no matches in the current
  #'   grid log).  
  #' @keywords internal
  #' 
  
  grid_log <- get_grid_log(project)
  
  if (is.null(grid_log)) TRUE
  else {
    
    no_match <- # make into standalone fun if more conditions needed
      vapply(grid_info, function(gi) {
        
        out <- vapply(grid_log, function(gl) !identical(gl, gi),
                      FUN.VALUE = logical(1))
        
        all(out)
      }, logical(1))
    
    if (ind) no_match
    else any(no_match)
  }
}


grid_lab_helper <- function(project, grid_info, grid_log = NULL) {
  #' Labeling function for saving grid files
  #' 
  #' @param project Name of project.
  #' @param grid_info List containing grid information.
  #' @param grid_log Optional, the grid log. if \code{NULL}, uses names from 
  #' \code{grid_info}. 
  #' @keywords internal
  #' 
  
  # add grid name to label. This will be it's object name when loaded.
  
  if (is.null(grid_log)) names(grid_info)
  else {
    
    # find and remove matching grids
    keep <- unique_grid(project, grid_info, ind = TRUE)
    
    if (sum(keep) > 0) {
      
      grid_info <- grid_info[keep]
      
      # add unique grids to log
      grid_ind <- length(grid_log) + seq(grid_info)
      grid_labs <- paste0("grid_", grid_ind)
      
      grid_labs
    }
  } 
}


log_grid_info <- function(project, grid_info) {
  #' Log grid file 
  #' 
  #' Writes grid information to a JSON file located in the project data directory.
  #' 
  #' @param project Name of project.
  #' @param grid_info List containing grid information. 
  #' @keywords internal
  #' @importFrom stats setNames
  #' @importFrom jsonlite write_json
  #' @seealso \code{\link{save_grid_cache}}
  #' @details The grid log is a list containing information about the grid files 
  #'   currently saved to the project data folder. Each grid entry contains three 
  #'   fields: \code{grid_name}, \code{closure_name}, and \code{combined_areas}. 
  #'   \code{grid_name} is the name of the original grid object. If the other two
  #'   fields are empty, this means that the grid file has not been altered and 
  #'   is the same as the original. \code{closure_name} is the name of a second 
  #'   grid file containing closure areas that were combined with \code{grid_name}.
  #'    \code{combined_areas} are the names/IDs of the closures areas from the 
  #'    closure grid file that were combined with \code{grid_name}. 
  
  save <- TRUE
  
  grid_log <- get_grid_log(project)
  
  if (is.null(grid_log)) grid_log <- grid_info
  
  else {
    
    # find and remove matching grids
    keep <- unique_grid(project, grid_info, ind = TRUE)
    
    if (sum(keep) == 0) save <- FALSE
    else {
      
      grid_info <- grid_info[keep]
      
      # add unique grids to log
      grid_lab <- grid_lab_helper(project, grid_info, grid_log)
      grid_info <- stats::setNames(grid_info, grid_lab)
      
      grid_log <- c(grid_log, grid_info)
    }
  }
  
  if (save) {
    
    filename <- paste0(loc_data(project), "spat/", project, "_grid_info.json")
    
    jsonlite::write_json(grid_log, filename, pretty = TRUE, auto_unbox = TRUE, 
                         null = "null", na = "string", force = TRUE)
  }
}


save_grid_cache <- function(project, grid_list, grid_info) {
  #' Save grid file to project data directory
  #'
  #'@param project Name of project.
  #'@param grid_list List containing grid files.
  #'@param grid_info List containing grid information.
  #'@keywords internal
  #'@details This function references the grid log to determine whether a grid
  #'  file should be saved. If a grid file is unique it is saved, otherwise no
  #'  action is taken. 
  #'@importFrom purrr map2
  #'@importFrom sf st_write
  #'@seealso \code{\link{unique_grid}} \code{\link{log_grid_info}}
  
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  
  grid_log <- get_grid_log(project)
  
  keep <- unique_grid(project, grid_info, ind = TRUE)
  
  if (sum(keep) > 0) {
    
    grid_list <- grid_list[keep]
    # adjust naming convention
    grid_labs <- grid_lab_helper(project, grid_info, grid_log)
    
    filename <- paste0(loc_data(project), "spat/", project, "_", grid_labs, ".geojson")
    
    purrr::map2(grid_list, filename, function(gl, fn) {
      
      sf::st_write(gl, dsn = fn)
    })
    
    cat("Grid saved as geojson", file = tmp)
    
    msg_print(tmp)
    
    # Log function
    save_grid_cache_function <- list()
    save_grid_cache_function$functionID <- "save_grid_cache"
    save_grid_cache_function$args <- list(project, filename, grid_info)
    save_grid_cache_function$msg <- suppressWarnings(readLines(tmp))
    log_call(project, save_grid_cache_function)
  }
}