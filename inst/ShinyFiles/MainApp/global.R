requireNamespace('shiny')
requireNamespace('ggplot2')
requireNamespace('DT')
#----
#Helper functions
#----
enableBookmarking(store = "server")



simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}


#' A column of delete buttons for each row in the data frame for the first column
#'
#' @param df data frame
#' @param id id prefix to add to each actionButton. The buttons will be id'd as id_INDEX.
#' @return A DT::datatable with escaping turned off that has the delete buttons in the first column and \code{df} in the other
deleteButtonColumn <- function(df, id, ...) {
  # function to create one action button as string
  f <- function(i) {
    # https://shiny.rstudio.com/articles/communicating-with-js.html
    as.character(actionButton(paste(id, i, sep="_"), label = NULL, icon = icon('trash'),
                              onclick = 'Shiny.setInputValue(\"deletePressed\",  this.id, {priority: "event"})'))
  }
  
  deleteCol <- unlist(lapply(seq_len(nrow(df)), f))
  
  # Return a data table
  DT::datatable(cbind(delete = deleteCol, df),
                # Need to disable escaping for html as string to work
                escape = FALSE,
                options = list(
                  # Disable sorting for the delete column
                  columnDefs = list(list(targets = 1, sortable = FALSE))
                ))
}

#' Extracts the row id number from the id string
#' @param idstr the id string formated as id_INDEX
#' @return INDEX from the id string id_INDEX
parseDeleteEvent <- function(idstr) {
  res <- as.integer(sub(".*_([0-9]+)", "\\1", idstr))
  if (! is.na(res)) res
}

find_lon <- function(dat) {
  
  cols <- colnames(dat)
  lon_match <- stringi::stri_count_regex(cols, '(?=LON|Lon|lon)')
  lon_cols <- which(lon_match %in% max(lon_match))
  
  cols[lon_cols]
}

find_lat <- function(dat) {
  
  cols <- colnames(dat)
  lat_match <- stringi::stri_count_regex(cols, '(?=LAT|Lat|lat)')
  lat_cols <- which(lat_match %in% max(lat_match))
  
  cols[lat_cols]
}

find_lonlat <- function(dat) {
  
  grep("lon|lat", colnames(dat), ignore.case = TRUE, value = TRUE)
}