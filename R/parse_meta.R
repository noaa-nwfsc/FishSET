parse_meta_delim <- function(file, sep = NULL, comment = "#", is_list = FALSE) {
  #' Parse metadata from a delimited file
  #' 
  #' Use this function if meta data is located in the same file as the data and 
  #' the data is stored in a delimited file (csv, tsv).
  #' 
  #' @param file String, the file path to metadata.
  #' @param sep String, the field separator character. defaults to \code{comment = "#"}.
  #' @param comment String, the comment character used to separate (or "comment-out") 
  #'   the meta data from the data. Only text that has been commented-out will be read.
  #' @param is_list Logical, is meta data stored as a list (i.e. Field: value, Field: value)
  #'   format). If a colon (":") is used after the field name set this to \code{TRUE}.
  #' @export
  #' @keywords internal
  #' @importFrom stringi stri_replace_first_fixed
  
  
  if (is.null(sep)) {
    
    ext <- file_ext(file)
    
    sep <- switch(ext, "csv" = ",", "tsv" = "\t")
  }
  
  meta <- readLines(file)
  
  com <- paste0('^', comment, '|^\"', comment, '\"')
  com <- gsub("%s", comment, "^%s|^\"%s\"")# excel may add quotes (xls to csv)
  
  m_lines <- grep(com, meta, value = TRUE) # handle length = 0
  
  if (length(m_lines) == 0) {
    
    warning("No metadata found. Check whether comment symbol is correct.")
  
    } else {
    
      m_lines <- gsub("^#\\s*", "", m_lines) # remove any spaces after comment
      
      if (all(grep(":", m_lines)) & is_list) { # description list?
        
        m_lines <- stringi::stri_replace_first_fixed(m_lines, ":", "$$$")
        m_lines <- strsplit(m_lines, split = "$$$", fixed = TRUE)
        
        m_lines <- lapply(m_lines, trimws)
        
        m_nm <- vapply(m_lines, function(x) x[[1]], character(1))
        m_lines <- lapply(m_lines, function(x) x[[-1]])
        
        names(m_lines) <- m_nm
        
      } else {
        
        m_lines <- paste0(m_lines, collapse = "\n")
        
        tmp <- tempfile()
        on.exit(unlink(tmp), add = TRUE)
        writeLines(m_lines, con = tmp)
        m_lines <- read.delim(tmp, sep = sep)
      }
      
    m_lines
  }
}


parse_meta_excel <- function(file, range = NULL, ...) {
  #' Parse metadata from an excel file
  #' 
  #' Use this function if metadata is located in the same file as the data.
  #' Correct specification of the \code{range} parameter is key. 
  #' 
  #' @param file String, file path.
  #' @param range String, the cell range to read from (e.g. "A1:C5"). See 
  #'   \code{\link[readxl]{read_excel}} for more details. 
  #' @param ... Additional arguments passed to \code{\link[readxl]{read_excel}}.
  #' @export
  #' @keywords internal
  #' @importFrom readxl read_excel
  
  meta <- readxl::read_excel(file, range = range, ...)
  
  meta 
}


parse_meta_json <- function(file, meta_ind = NULL, simplifyVector = TRUE) {
  #' Parse metadata from json file
  #' 
  #' Use this function if metadata is located in the same file as the data and 
  #' the data is stored in a JSON file.
  #' 
  #' @param file String, file path.
  #' @param meta_ind Integer or string. 
  #' @param simplifyVector Logical, simplifies nested lists into vectors and data frames.
  #'   See \code{\link[jsonlite]{fromJSON}}.
  #' @export
  #' @keywords internal
  #' @importFrom jsonlite read_json
  
  meta <- jsonlite::read_json(path = file, simplifyVector = simplifyVector)
  
  if (is.null(meta_ind)) meta_ind <- 1
  
  meta_out <- meta[[meta_ind]]
  
  meta_out
}


parse_meta_xml <- function(file, meta_ind = NULL) {
  #' Parse metadata from xml file
  #' 
  #' Use this function if metadata is located in the same file as the data and 
  #' the data is stored in an xml file.
  #' 
  #' @param file String, file path.
  #' @param meta_ind Integer or string. 
  #' @export
  #' @keywords internal
  #' @importFrom xml2 read_xml as_list
  
  meta <- xml2::read_xml(x = file)
  meta <- xml2::as_list(meta)
  
  if (is.null(meta_ind)) meta_ind <- 1
  
  meta_out <- meta[[meta_ind]]
  
  meta_out
}


parse_meta <- function(file, ..., simplify_meta = FALSE) {
  #' Parse metadata from a data file
  #' 
  #' General purpose meta parsing function. \code{parse_meta} attempts to parse
  #' a file based on its file extension. 
  #' 
  #' @param file String, file path. 
  #' @param ... Additional arguments passed to a parsing function based on file
  #'   extension. See below.  
  #' @param simplify_meta Logical, attempt to simplify the metadata output. This
  #'   uses \code{\link{simplify_list}}. This can be useful if metadata is not 
  #'   tabular. 
  #' @export
  #' @seealso \code{\link{parse_meta_txt}}, \code{\link{parse_meta_excel}}, 
  #'   \code{\link{parse_meta_json}}, \code{\link{parse_meta_xml}}
  #' @importFrom tools file_ext
  #' @details Function supports xls, xlsx, csv, tsv, excel, json, and xlm extensions.
  #' #' Extension-specific notes: \cr
  #' 
  #'  txt: \cr
  #'  \verb{  }sep Field separator character. defaults to \code{comment = "#"}.\cr
  #'   \verb{  }comment The comment character used to separate (or "comment-out") 
  #'   the metadata from the data. Only text that has been commented-out will be read.\cr
  #'   \verb{ }d_list Logical, is metadata stored as a description list (i.e. Field: value, value
  #'   format). If a colon (":") is used after the field name set this to \code{TRUE}.
  #'
  #'  xls, xlsx: \cr
  #'   \verb{    }range The cell range to read from (e.g. "A1:C5"). See 
  #'   \code{\link[readxl]{read_excel}} for more details. 
  #'     
  #' 
  
  
  ext <- tools::file_ext(file)
  
  if (ext %in% c("xls", "xlsx")) ext <- "excel"
  
  if (ext %in% c("csv", "tsv")) {
    
    meta <- parse_meta_delim(file = file, ...)
  
  } else if (ext == "excel") {
    
    meta <- parse_meta_excel(file = file, ...)
    
  } else if (ext == "json") {
    
    meta <- parse_meta_json(file = file, ...)
    
  } else if (ext == c("xml", "html", "xhtml")) {
    
    meta <- parse_meta_xml(file = file, ...)
    
  } else if (ext == "txt") {
    
    meta <- utils::read.table(file = file, ...)
    
  } else {
    
    warning(paste("File type", ext, "is not supported."))
    return(invisible(NULL))
  }
  
  if (simplify_meta) meta <- simplify_list(meta)
    
  meta
}
