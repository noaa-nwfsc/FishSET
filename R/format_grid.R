#' Format Gridded Data
#' 
#' Change the format of a gridded dataset from wide to long (or vice versa). 
#' \code{format_grid} will also remove any unmatched area/zones from \code{grid}.
#' 
#' @param grid Gridded dataset to format.
#' @param dat Primary data containing information on hauls or trips. Table in 
#'   the FishSET database contains the string 'MainDataTable'.
#' @param project Name of project.
#' @param area.dat String, the name of the area or zone column in \code{dat}.
#' @param area.grid String, the name of the area or zone column in \code{dat} if
#'   \code{from.format = "long"}. Ignored if \code{from.format = "wide"}.
#' @param id.cols String, the names of all non-area columns if \code{from.format = "wide"},
#'   such as date variables.\code{id.cols} should contain the name of the value
#'   variable if \code{from.format = "long"}. 
#' @param from.format The original format of \code{grid}. Options include
#'   \code{"long"} or \code{"wide"}. Use \code{"long"} if a single area column
#'   exists in \cide{grid}. Use \code{"wide"} if each area has its own column in 
#'   \code{grid}.
#' @param to.format The desired format of \code{grid}. Options include
#'   \code{"long"} or \code{"wide"}.  Use \code{"long"} if you want a single area 
#'   column with a corresponding value column. Use \code{"wide"} if you would 
#'   like each area to have its own column.
#' @param val.name Required if \code{from.format = "wide"} and \code{to.format = "long"}.
#'   This will be the name of the new value variable associated with the area 
#'   column.
#' @param save Logical, whether to save formatted \code{grid}. 
#' @export
#' @importFrom tidyr pivot_longer pivot_wider

format_grid <-
  function(grid,
           dat,
           project,
           area.dat,
           area.grid = NULL,
           id.cols, 
           from.format = "wide",
           to.format = "wide", 
           val.name = NULL,
           save = FALSE
           ) {
    
    out <- data_pull(dat, project)
    dataset <- out$dataset
    dat <- parse_data_name(dat, "main", project)
    
    grid_out <- data_pull(grid, project)
    griddat <- grid_out$dataset
    grid <- parse_data_name(grid, "grid", project)
    
    # Note: if no date var, allow format?
    
    column_check(dat, area.dat)
    column_check(griddat, c(area.grid, id.cols))
    
    if (from.format == "wide") {
      
      # filter out unmatched areas
      
      g_names <- colnames(griddat)
      g_areas <- g_names[!g_names %in% id.cols]
      d_areas <- unique(as.character(dat[[area.dat]]))
      a_ind <- g_areas %in% d_areas
      
      if (sum(a_ind) == 0) {
        
        stop("No matches between gridded table and primary table. Check gridded ",
             "table format or use data with matching zones.", call. = FALSE)
      }
      
      # filter out unmatched areas
      griddat <- griddat[c(id.cols, g_areas[a_ind])]
      
      if (to.format == "long") {
        
        if (is_value_empty(val.name)) {

          warning("'val.name' missing. defaulting to 'value'.", call. = FALSE)
        }
        
        griddat <- tidyr::pivot_longer(griddat, !id.cols, 
                                       names_to = area.dat, 
                                       values_to = val.name) 
      }
      
    } else if (from.format == "long") {
      
      # Note: check/coerce type? 
      # filter unmatched areas from grid
      g_areas <- unique(griddat[[area.grid]])
      d_areas <- unique(dat[[area.dat]])
      g_areas <- g_areas[g_areas %in% d_areas] 
      
      if (length(g_areas) == 0) {
        
        stop("No matches between gridded table and primary table. Check gridded ",
             "table format or use data with matching zones.", call. = FALSE)
      }
      
      griddat <- griddat[griddat[[area.grid]] %in% g_areas, ]
      
      if (to.format == "wide") {
        
        griddat <- tidyr::pivot_wider(griddat, 
                                      values_from = id.cols, 
                                      names_from = all_of(area.grid))
      }
    }
    
    # TODO: add save feature
    
    
    griddat
}