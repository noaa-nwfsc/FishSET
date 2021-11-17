to_html_table <- function(x, rownames = FALSE) {
  #' Convert dataframe/matrix to html table
  #' 
  #' @param x A vector or list object.
  #' @param rownames Logical, whether to show rownames. 
  #' @export
  #' @importFrom htmltools HTML
  #' @importFrom shiny renderTable
  #' @keywords internal
  #' @return Returns an HTML table or (if the object is not a matrix or dataframe)
  #'   the original object unmodified. 
  #' 
  
  if (is.matrix(x)) {
    
    if (is.null(colnames(x))) {
      
      colnames(x) <- paste0("col_", seq(ncol(x)))
    }
    
    x <- as.data.frame(x)
  }
  
  if (is.data.frame(x)) {
    
    htmltools::HTML(shiny::renderTable(x, rownames = rownames)())
    
  }  else x
}


list_html <- function(l) {
  #' Create an HTML list item
  #' 
  #' Converts a list object to an HTML list item (<li>). If an object is a dataframe
  #' or matrix, it is converted to an HTML table first.
  #' 
  #' @param l A list.
  #' @importFrom shiny tags
  #' @keywords internal
  #' @return An HTML string of a list item.
  
  html_tab <- is.data.frame(l) | is.matrix(l)
  
  l <- to_html_table(l)
  
  if (html_tab) {
    
    shiny::tags$li(l)
    
  } else {
    
    shiny::tags$li(as.character(l))
  }
}


list_html_r <- function(l) {
  #' Create an un-ordered HTML list recursively
  #' 
  #' Applies \code{\link{list_html}} to every level of a list object.
  #' 
  #' @param l A list.
  #' @importFrom purrr map2
  #' @importFrom shiny tags
  #' @keywords internal
  #' @return A list of HTML list item strings. 
  
  nms <- names(l) # what if no names?
  
  purrr::map2(l, nms, function(x, y) {

    if (is_leaf(x)) {

      shiny::tags$li(y, shiny::tags$ul(list_html(x)))

    } else {

      shiny::tags$li(y, shiny::tags$ul(list_html_r(x)))
    }
  })
}


list_to_html <- function(l) {
  #' Convert list to HTML
  #' 
  #' Converts a list to an un-ordered HTML list (<ul>). 
  #' 
  #' @param l A list.
  #' @importFrom shiny tags
  #' @importFrom rlang is_bare_list
  #' @export
  #' @return An un-ordered HTML list.
  #' @seealso \code{\link{simplify_list}} \code{\link{collapse_leaf_r}} 
  #'   \code{\link{list_html_r}}
  #' @examples 
  #' \dontrun{
  #'   l <- list(A = 'a text', B = list(b1 = 'b1 text', b2 = 'b2 text'),
  #'             C = list(data.frame(A = 1:10, B = letters[1:10])))
  #'   list_to_html(l)
  #' }
  
  # if (is.list(l) & !is.data.frame(l)) {
  if (rlang::is_bare_list(l)) {
    
    l <- simplify_list(l)
    
    l <- collapse_leaf_r(l)
    
    l <- list_html_r(l)
    
    shiny::tags$ul(l)
  
  } else {
    
    warning("Object is not a list.")
  }
}
