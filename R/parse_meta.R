parse_meta_txt <- function(file, sep = ",", comment = "#", d_list = FALSE) {
  #' Parse meta data from a plain text file
  #' 
  #' Use this function if meta data is located in the same file as the data and 
  #' the data is stored in a delimited plain text file (csv, tsv).
  #' 
  #' @param file String, the file path to meta data.
  #' @param sep String, the field separator character. defaults to \code{comment = "#"}.
  #' @param comment String, the comment character used to separate (or "comment-out") 
  #'   the meta data from the data. Only text that has been commented-out will be read.
  #' @param d_list Logical, is meta data stored as a description list (i.e. Field: value, value
  #'   format). If a colon (":") is used after the field name set this to \code{TRUE}.
  #' @export
  #' @importFrom stringi stri_replace_first_fixed
  
  meta <- readLines(file)
  
  com <- paste0('^', comment, '|^\"', comment, '\"')
  com <- gsub("%s", comment, "^%s|^\"%s\"")# excel may add quotes (xls to csv)
  
  m_lines <- grep(com, meta, value = TRUE) # handle length = 0
  
  if (length(m_lines) == 0) {
    
    warning("No meta data found. Check whether comment symbol is correct.")
  
    } else {
    
      m_lines <- gsub("^#\\s*", "", m_lines) # remove any spaces after comment
      
      if (all(grep(":", m_lines)) & d_list) { 
        
        m_lines <- stringi::stri_replace_first_fixed(m_lines, ":", "$$$")
        m_lines <- strsplit(m_lines, split = "$$$", fixed = TRUE)
        
      } else {
        
        m_lines <- strsplit(m_lines, split = sep)
      }
      
      m_lines <- lapply(m_lines, trimws)
      
      m_nm <- vapply(m_lines, function(x) x[[1]], character(1))
      m_lines <- lapply(m_lines, function(x) x[[-1]])
      
      names(m_lines) <- m_nm
      
      m_lines
  }
}


parse_meta_excel <- function(file, range = NULL, ...) {
  #' Parse meta data from an excel file
  #' 
  #' Use this function if meta data is located in the same file as the data.
  #' Correct specification of the \code{range} parameter is key. 
  #' 
  #' @param file String, file path.
  #' @param range String, the cell range to read from (e.g. "A1:C5"). See 
  #'   \code{\link[readxl]{read_excel}} for more details. 
  #' @param ... Additional arguments passed to \code{\link[readxl]{read_excel}}.
  #' @export
  #' @importFrom readxl read_excel
  
  meta <- readxl::read_excel(file, range = range, ...)
  
  meta 
}


parse_meta_json <- function(file, meta_ind = NULL, simplifyVector = TRUE) {
  #' Parse meta data from json file
  #' 
  #' Use this function if meta data is located in the same file as the data and 
  #' the data is stored in a JSON file.
  #' 
  #' @param file String, file path.
  #' @param meta_ind Integer or string. 
  #' @param simplifyVector Logical, simplifies nested lists into vectors and data frames.
  #'   See \code{\link[jsonlite]{fromJSON}}.
  #' @export
  #' @importFrom jsonlite read_json
  
  meta <- jsonlite::read_json(path = file, simplifyVector = simplifyVector)
  
  if (is.null(meta_ind)) meta_ind <- 1
  
  meta_out <- meta[[meta_ind]]
  
  meta_out
}


parse_meta_xml <- function(file, meta_ind = NULL) {
  #' Parse meta data from xml file
  #' 
  #' Use this function if meta data is located in the same file as the data and 
  #' the data is stored in an xml file.
  #' 
  #' @param file String, file path.
  #' @param meta_ind Integer or string. 
  #' @export
  #' @importFrom xml2 read_xml as_list
  
  meta <- xml2::read_xml(x = file)
  meta <- xml2::as_list(meta)
  
  if (is.null(meta_ind)) meta_ind <- 1
  
  meta_out <- meta[[meta_ind]]
  
  meta_out
}


parse_meta <- function(file, ..., simplify_meta = FALSE) {
  #' Parse meta data from a data file
  #' 
  #' General purpose meta parsing function. \code{parse_meta} attempts to parse
  #' a file based on its file extension. 
  #' 
  #' @param file String, file path. 
  #' @param ... Additional arguments passed to a parsing function based on file
  #'   extension. See below.  
  #' @param simplify_meta Logical, attempt to simplify the meta data output. This
  #'   uses \code{\link{simplify_list}}. This can be useful if meta data is not 
  #'   tabular. 
  #' @export
  #' @seealso \code{\link{parse_meta_txt}}, \code{\link{parse_meta_excel}}, 
  #'   \code{\link{parse_meta_json}}, \code{\link{parse_meta_xml}}
  #' @importFrom tools file_ext
  
  ext <- tools::file_ext(file)
  
  if (ext %in% c("xls", "xlsx")) ext <- "excel"
  
  if (ext %in% c("csv", "tsv")) {
    
    meta <- parse_meta_txt(file = file, ...)
  
  } else if (ext == "excel") {
    
    meta <- parse_meta_excel(file = file, ...)
    
  } else if (ext == "json") {
    
    meta <- parse_meta_json(file = file, ...)
    
  } else if (ext == "xml") {
    
    meta <- parse_meta_xml(file = file, ...)
    
  } else {
    
    warning(paste("File type", ext, "is not supported."))
    return(invisible(NULL))
  }
  
  if (simplify_meta) meta <- simplify_list(meta)
    
  meta
}
