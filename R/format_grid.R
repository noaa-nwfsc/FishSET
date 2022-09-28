#' Format Gridded Data
#' 
#' Change the format of a gridded dataset from wide to long (or vice versa) 
#' and remove any unmatched area/zones from `grid`.
#' 
#' @param grid Gridded dataset to format.
#' @param dat Primary data containing information on hauls or trips. Table in 
#'   the FishSET database contains the string 'MainDataTable'.
#' @param project Name of project.
#' @param area.dat String, the name of the area or zone column in `dat`.
#' @param area.grid String, the name of the area or zone column in `dat` if
#'   `from.format = "long"`. Ignored if `from.format = "wide"`.
#' @param id.cols String, the names of all non-area columns if `from.format = "wide"`,
#'   such as date variables.`id.cols` should contain the name of the value
#'   variable if `from.format = "long"`. 
#' @param from.format The original format of `grid`. Options include
#'   `"long"` or `"wide"`. Use `"long"` if a single area column
#'   exists in `grid`. Use `"wide"` if each area has its own column in 
#'   `grid`.
#' @param to.format The desired format of `grid`. Options include
#'   `"long"` or `"wide"`.  Use `"long"` if you want a single area 
#'   column with a corresponding value column. Use `"wide"` if you would 
#'   like each area to have its own column.
#' @param val.name Required if `from.format = "wide"` and `to.format = "long"`.
#'   This will be the name of the new value variable associated with the area 
#'   column.
#' @param save Logical, whether to save formatted `grid`. 
#' @export
#' @importFrom tidyr pivot_longer pivot_wider
#' @seealso [merge_dat()]
#' @md

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
    
    # TODO: warning if gridded data does not cover all zones in primary data
    
    out <- data_pull(dat, project)
    dataset <- out$dataset
    dat <- parse_data_name(dat, "main", project)
    
    grid_out <- data_pull(grid, project)
    griddat <- grid_out$dataset
    grid <- parse_data_name(grid, "grid", project)
    
    # Note: if no date var, allow format?
    
    stopifnot("'area.dat' is required" = !is_value_empty(area.dat),
              "'id.cols' is requierd" = !is_value_empty(id.cols))
    
    column_check(dataset, area.dat)
    column_check(griddat, c(area.grid, id.cols))
    
    if (from.format == "wide") {
      
      # filter out unmatched areas
      g_names <- colnames(griddat)
      g_areas <- g_names[!g_names %in% id.cols]
      d_areas <- unique(as.character(dataset[[area.dat]]))
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
      
      if (is_value_empty(area.grid)) {
        
        stop("'area.grid' is required.", call. = FALSE)
      }
      
      # Note: check/coerce type? 
      # filter unmatched areas from grid
      g_areas <- unique(griddat[[area.grid]])
      d_areas <- unique(dataset[[area.dat]])
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
    
    if (save) {
      
      if (table_exists(grid, project)) {
        
        fishset_db <- suppressWarnings(DBI::dbConnect(RSQLite::SQLite(), 
                                                      locdatabase(project = project)))
        on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
        
        DBI::dbWriteTable(fishset_db, grid, griddat, overwrite = TRUE)
        
      } else {
        
        warning("Gridded table '", grid, "' does not exists, table not saved. ", 
                "Use load_grid() to save gridded tables to the FishSET Database.",
                call. = FALSE)
      }
    }
    
    # log function
    format_grid_function <- list()
    format_grid_function$functionID <- "format_grid"
    format_grid_function$args <- 
      list(grid, dat, project, area.dat, area.grid, id.cols,
           from.format, to.format, val.name, save)
    
    griddat
}