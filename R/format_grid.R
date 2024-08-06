#' Format Gridded Data
#' 
#' Change the format of a gridded dataset from wide to long (or vice versa) 
#' and remove any unmatched area/zones from `grid`. This is a necessary step
#' for including gridded variables in the conditional logit ([logit_c()]) model.
#' 
#' @param grid Gridded dataset to format.
#' @param dat Primary data containing information on hauls or trips. Table in 
#'   the FishSET database contains the string 'MainDataTable'.
#' @param project Name of project.
#' @param dat.key String, name of column(s) in MainDataTable to join by. The 
#'   number of columns must match `id.cols`.
#' @param area.dat String, the name of the area or zone column in `dat`.
#' @param area.grid String, the name of the area or zone column in `dat` if
#'   `from.format = "long"`. Ignored if `from.format = "wide"`.
#' @param id.cols String, the names of columns from `grid` that are neither area 
#'   (`area.grid`) or value (`val.name`) columns, for example date or period 
#'   column(s).
#' @param from.format The original format of `grid`. Options include `"long"` or 
#'   `"wide"`. Use `"long"` if a single area column exists in `grid`. Use `"wide"` 
#'   if `grid` contains a column for each area.
#' @param to.format The desired format of `grid`. Options include `"long"` or 
#'   `"wide"`. Use `"long"` if you want a single area column with a corresponding 
#'   value column. Use `"wide"` if you would like each area to have its own column.
#' @param val.name Required if converting from wide to long or long to wide format.
#'  When `from.format = "wide"` and `to.format = "long"`, `val.name` will be the 
#'  name of the new value variable associated with the area column.  When 
#'  `from.format = "long"` and `to.format = "wide"`, `val.name` will be the 
#'  name of the existing value variable associated with the area column. 
#' @param save Logical, whether to save formatted `grid`. When `TRUE`, the 
#'   table will be saved with the string `"Wide"` or `"Long"` appended depending 
#'   on the value of `to.format`.
#' @export
#' @importFrom tidyr pivot_longer pivot_wider
#' @seealso [merge_dat()]
#' @md

format_grid <-
  function(grid,
           dat,
           project,
           dat.key,
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
    stopifnot("'area.dat' is required" = !is_value_empty(area.dat),
              "'id.cols' is required" = !is_value_empty(id.cols))
    
    column_check(dataset, area.dat)
    column_check(griddat, c(area.grid, id.cols))
    
    # from wide ----
    if (from.format == "wide") {
      
      # filter out unmatched areas
      g_names <- colnames(griddat)
      g_areas <- g_names[!g_names %in% id.cols]
      d_areas <- unique(as.character(dataset[[area.dat]]))
      a_ind <- g_names %in% d_areas
      
      if (sum(a_ind) == 0) {
        
        stop("No matches between gridded table and primary table. Check gridded ",
             "table format or use data with matching zones.", call. = FALSE)
      }
      
      if (!all(d_areas %in% g_areas)) {
        
        stop("Gridded data does not include all zones from 'dat'.", call. = FALSE)
      }
      
      # filter out unmatched areas
      griddat <- griddat[c(id.cols, g_areas[a_ind])]
      
      if (to.format == "long") {
        
        if (is_value_empty(val.name)) {

          warning("'val.name' missing. defaulting to 'value'.", call. = FALSE)
        }
        
        
        dat.area.class <- class(dataset[[area.dat]])
        class.type <- switch(dat.area.class, 
                             "numeric" = as.numeric, "integer" = as.integer,
                             "character" = as.character, "factor" = as.factor)
        area.tran <- stats::setNames(list(class.type), area.dat)
        
        # match class of area.dat
        griddat <- tidyr::pivot_longer(griddat, !id.cols, 
                                       names_to = area.dat, 
                                       values_to = val.name,
                                       names_transform = area.tran) 
        
        # join to primary data
        dat_grid <- merge_dat(dataset, griddat, project = project,
                              main_key = c(dat.key, area.dat),
                              other_key = c(id.cols, area.dat),
                              other_type = "grid",
                              merge_type = "left")
        
      } else {
        
        dat_grid <- merge_dat(dataset, griddat, project = project,
                              main_key = dat.key,
                              other_key = id.cols,
                              other_type = "grid",
                              merge_type = "left")
      }
      
      # from long ----
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
      
      if (!all(d_areas %in% g_areas)) {
        
        stop("Gridded data does not include all zones from 'dat'.", call. = FALSE)
      }
      
      griddat <- griddat[griddat[[area.grid]] %in% g_areas, ]
      
      if (to.format == "wide") {

        griddat <- tidyr::pivot_wider(griddat, 
                                      values_from = val.name, 
                                      names_from = area.grid)
        
        # id.cols should contain the grid ids needed to join, change to grid.id
        dat_grid <- merge_dat(dataset, griddat, project = project,
                              main_key = dat.key,
                              other_key = id.cols,
                              other_type = "grid",
                              merge_type = "left")
        
      } else {
      
        # join to primary data
        dat_grid <- merge_dat(dataset, griddat, project = project,
                              main_key = c(dat.key, area.dat),
                              other_key = c(id.cols, area.dat),
                              other_type = "grid",
                              merge_type = "left")
      }
    }
    
    # drop dat and ID columns
    griddat <- dat_grid[-seq(ncol(dataset))]
    
    # save ----
    if (save) {
      
      if (table_exists(grid, project)) {
        
        fishset_db <- suppressWarnings(DBI::dbConnect(RSQLite::SQLite(), 
                                                      locdatabase(project = project)))
        on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
        
        g_name <- paste0(grid, simpleCap(to.format))
        
        DBI::dbWriteTable(fishset_db, g_name, griddat, overwrite = TRUE)
        
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
      list(grid, dat, project, dat.key, area.dat, area.grid, id.cols,
           from.format, to.format, val.name, save)
    
    griddat
}