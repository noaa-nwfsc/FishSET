save_raw_meta <- function(file, project, dataset = NULL, tab.name = NULL, tab.type, 
                          parse = FALSE, overwrite = FALSE, ...) {
  #' Save a meta data file to project folder
  #' 
  #' Raw (i.e. original or pre-existing) meta data can be saved to the project folder. 
  #' To add additional meta data (e.g. column descriptions), see ... 
  #'
  #'@param file String, file path. 
  #'@param project Project name.
  #'@param dataset Optional, the data.frame associated with the meta data. Used 
  #'  to add column names to meta file.
  #'@param tab.name The table name as it appears in the FishSET Database (e.g.
  #'   "projectMainDataTable" if the main table).
  #'@param tab.type The table type. Options include "main", "spat" (spatial), 
  #'  "port", "grid" (gridded), and "aux" (auxiliary).
  #'@param parse Logical, whether to parse meta data from a data file. See 
  #'  \code{\link{parse_meta}}.
  #'@param overwrite Logical, whether to overwrite existing meta table entry. 
  #'@param ... Additional arguments passed to \code{\link{parse_meta}}.
  #'@export
  #'@importFrom tools file_ext
  #'@importFrom readxl read_excel
  #'@importFrom xml2 as_list
  #'@seealso \code{\link{parse_meta}}
  
  if (!(tab.type %in% c("main", "port", "spat", "grid", "aux"))) {
    
    warning("Invalid table type. Choose \"main\", \"port\", \"spat\", \"grid\", or \"aux\".")
    return(invisible(NULL))
  }
  
  table_type <- switch(tab.type, "main" = "MainDataTable", "port" = "PortTable", 
                       "spat" = "SpatialTable", "grid" = "GridTable", "aux" = "AuxTable")
  
  if (tab.type %in% c("main", "port")) tab.name <- NULL
  
  tab_name <- paste0(project, tab.name, table_type)
  
  if (parse) { # extract meta from data file
    
    meta_out <- parse_meta(file, ..., simplify_meta = TRUE)
    
  } else {
    
    m_ext <- tools::file_ext(file)
    
    if (m_ext %in% c("xls", "xlsx", "excel")) { # read_dat coerces to data.frame
      
      meta_out <- readxl::read_excel(path = file, ...)
      
    } else {
      
      meta_out <- read_dat(file, data.type = m_ext, ...) 
    }
    
    if (m_ext == "xml") {
      
      meta_out <- xml2::as_list(meta_out)
    }
  }
  
  if (is.null(meta_out)) {
    
    warning("Meta data not saved.")
    invisible(FALSE)
    
  } else {
    
    meta_log_call(project, meta_out, dataset = dataset, tab_name = tab_name, 
                  tab_type = tab.type, overwrite = overwrite, raw = TRUE)
    invisible(TRUE)
  }
}