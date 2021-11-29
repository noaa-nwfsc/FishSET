list_depth <- function(l) {
  #' Find list depth
  #' 
  #' Iterates \code{purrr::vec_depth()} over each object in a list. 
  #' 
  #' @param l A list.
  #' 
  #' @keywords internal
  #' @importFrom purrr vec_depth
  #' @return A numeric vector of list depths.
  
  vapply(l, purrr::vec_depth, numeric(1))
}


list_length <- function(l) {
  #' Find list length
  #' 
  #' Iterates \code{length()} over each object in a list. 
  #' 
  #' @param l A list.
  #' 
  #' @keywords internal
  #' @return A numeric vector of list lengths.
  
  vapply(l, length, numeric(1))
}


is_leaf_table <- function(x) {
  #' Detect potential tables in list node
  #'
  #' Determines whether an object in a list can be converted into a dataframe.
  #' @param x An object in a list.
  #' @keywords internal
  #' @importFrom purrr vec_depth
  #' @return \code{TRUE} if leaf node can be converted to dataframe, \code{FALSE} if not.
  #'
  
  dep <- purrr::vec_depth(x)
  len <- list_length(x) 
  
  if (length(len) == 1) {
    
    len <- c(len, len)
  }
  
  if (dep == 2) {
    
    if (var(len) == 0) TRUE
    else FALSE
    
  } else FALSE
}


is_leaf <- function(x) {
  #' Is list object a leaf node
  #'
  #' Determines whether an object in a list is a leaf node.
  #'
  #' @param x An object in a list.
  #' @keywords internal
  #' @importFrom purrr vec_depth
  #' @return \code{TRUE} if list object is a leaf node, \code{FALSE} if not.
  
  dep <- purrr::vec_depth(x) 
  
  dep == 1 | is.data.frame(x)
}


collapse_leaf <- function(x) {
  #' Convert vector to scalar
  #' 
  #' Collapses a vector to a scalar. This is needed to convert an R list to
  #' an html list.
  #' 
  #' @param x A vector or object in a list. 
  #' @keywords internal 
  #' @return a character vector of length 1. 
  
  
  if (is_leaf(x)) {
    
    if (!is.matrix(x) & !is.data.frame(x) & length(x) > 1) {
      
      paste(x, collapse = ", ")
      
    } else x
    
  } else x
}


collapse_leaf_r <- function(l) {
  #' Recursively apply \code{collapse_leaf()} to a list object
  #' 
  #' @param l A list
  #' @keywords internal
  #' @importFrom purrr map2
  #' @return A list.
  
  l <- lapply(l, collapse_leaf)
  
  leaf <- vapply(l, is_leaf, logical(1))
  
  purrr::map2(l, leaf, function(x, y) {

    if (y) x

    else collapse_leaf_r(x)
  })
}


lame_list <- function(l) {
  #' Detect an unnecessary nested list
  #' 
  #' If a list object contains an unnamed list containing another list or a single 
  #' object, it is a "lame list". For example: \code{list(list(1:10)))}. The
  #' nested list in this example is unnecessary since it contains a single vector, 
  #' and therefore can be removed. Pruning lame lists can help simplify a list object.
  #' 
  #' @param l A list.
  #' @keywords internal
  #' @seealso \code{\link{clean_list}}
  #' @return \code{TRUE} if list object is a lame list, \code{FALSE} if not.
  #' @examples 
  #' \dontrun{
  #' lame_list(list(list(1:10)))
  #' lame_list(list(A = list(1:10)))
  #' lame_list(list(list(1:10), list(11:20)))
  #' }
  
  len <- length(l)
  nm <- names(l) 
  is_l <- is.list(l)
  
  len == 1 & is.null(nm) & is_l
}


clean_list <- function(l) { 
  #' Remove unnecessary nested lists
  #' 
  #' Recursively removes unnecessary lists ("lame lists") from a list. A lame list
  #' is a unnamed list containing a single list object (see \code{\link{lame_list}}). 
  #' 
  #' @param l A list.
  #' 
  #' @keywords internal
  #' @importFrom purrr vec_depth
  #' @seealso \code{\link{simplify_list}} \code{\link{lame_list}}
  #' @return a list.
  #' @examples 
  #' \dontrun{
  #' clean_list(list(list(1:10)))
  #' clean_list(list(A = list(1:10)))
  #' clean_list(list(list(1:10), list(11:20)))
  #' }
  
  dep <- purrr::vec_depth(l) 
  
  if (dep > 1) {
    
    if (lame_list(l)) unlist(l, recursive = FALSE)
    
    else lapply(l, clean_list)
    
  } else l
}


list_df <- function(l) {
  #' Convert leaf node to a dataframe
  #' 
  #' Determines whether a leaf node contains an object that can be converted to
  #' a dataframe.
  #' @param l A list.
  #' @keywords internal
  #' @importFrom purrr vec_depth
  #' @seealso \code{\link{is_leaf_table}} \code{\link{simplify_list}}
  #' @return A list.
  #' @examples 
  #' \dontrun{
  #' list_df(list(A = list(X = 1:10, Y = letters[1:10])))
  #' list_df(list(A = 1:10, B = "Text", C = c("text", "text"))) # no conversion
  #' }
  
  dep <- purrr::vec_depth(l)
  
  if (dep > 1) {
    
    if (is_leaf_table(l)) as.data.frame(l)
    
    else lapply(l, list_df)
    
  } else l
}


simplify_list <- function(l, format = FALSE) {
  #' Simplify a list
  #' 
  #' Cleans a list and converts applicable leaf nodes to dataframes.
  #' 
  #' @param l A list.
  #' @param format Logical, whether to print list using pandoc markdown.
  #' @export
  #' @keywords internal
  #' @importFrom dplyr bind_rows
  #' @importFrom pander pander
  #' @seealso \code{\link{clean_list}} \code{\link{list_df}}  \code{\link[pander]{pander}}
  #' @return A list
  #' @examples 
  #' \dontrun{
  #' simplify_list(list(A = 1:10, B = 11:20))
  #' simplify_list(list(A = list(X = 1:10, Y = letters[1:10])))
  #' simplify_list(list(A = 1:10, B = "Text", C = c("text", "text")))
  #' }
  
  if (rlang::is_bare_list(l)) {
    
    out <- clean_list(l)
    
    len <- list_length(out)
    dep <- list_depth(out)
    
    if (!is.na(var(len)) & !is.na(var(dep))) {
      
      if (var(len) == 0 & var(dep) == 0) { # list entries have same length and depth
        
        out <- dplyr::bind_rows(out)
        
      } else {
        
        out <- list_df(out)
      }
      
    } else {
      
      out <- list_df(out)
    }
  } else out <- l
  
  if (format) pander::pander(out)
  else out
}
