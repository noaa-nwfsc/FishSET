
fleet_table <- function(dat, project, cond = NULL, fleet_val = NULL, table = NULL, save = TRUE) {
  #' Define and store fleet expressions
  #'
  #' @description \code{fleet_table} saves a table of fleet expression to the FishSET database which
  #'   can then be applied to a dataset with fleet_assign. The table must contain a 'condition'
  #'   and 'fleet' column with each row corresponding to a set of expressions that will be used to
  #'   assign observations to fleets. A table can be created with the \code{cond} and \code{fleet_val}
  #'   arguments, or by uploading an existing table that matches the format requirements. See 'Details'
  #'   below for examples of how table can be formatted.
  #' @param dat Primary data containing information on hauls or trips. Table in FishSET database contains the string 'MainDataTable'.
  #' @param project String, name of project.
  #' @param cond String; use a vector if creating a single condition column or a
  #'   list if creating multiple condition columns. Must be used with the \code{fleet_val} argument.
  #'   Each expression should be in quotes (double or single) with nested quotes indicated
  #'   with escaped quotes (\') or with the opposite quote used to contain the expression. For example,
  #'   "species == 'cod'" and "species == \'cod\'" are both valid.
  #' @param fleet_val String; a vector of fleet names to be assigned. Must be used with the
  #'   \code{cond} argument.
  #' @param table A data frame that has at least one condition column and one fleet column. See
  #'   'Details' for table formatting.
  #' @param save Logical; whether to save fleet_table to the FishSET database. Defaults to TRUE.
  #'   Tables are saved in the format of 'projectFleetTable'. If a table with the same name exists
  #'   the date is appended to the table name. See \code{\link{table_remove}} to delete a table.
  #' @importFrom DBI dbConnect dbWriteTable dbDisconnect
  #' @importFrom RSQLite SQLite
  #' @export fleet_table
  #' @return Returns a table of fleet conditions that is saved to the FishSET database with the name
  #'   'projectFleetTable'.
  #' @details
  #'   Below is a simple example of a fleet table. For a fleet table to be created, it must contain
  #'   at least one condition column, a fleet column, and an 'other' category which can be
  #'   indicated with an NA in the condition column. Users can add as many condition columns
  #'   as they like, and each condition column can have as many expressions as needed. For
  #'   example, the first expression in the condition column example could also be
  #'   \code{"GEAR == 8 & species == 'pollock'"}. When multiple condition columns exist the function
  #'   uses the '&' operator when combining expressions.
  #' \tabular{lllll}{
  #' condition                 \tab fleet\cr
  #' 'GEAR == 8'               \tab 'A'\cr
  #' 'species == 'cod''        \tab 'B'\cr
  #' 'area \%in\% c(640, 620)' \tab 'C'\cr
  #' NA                        \tab 'Other'
  #' }
  #'
  #' \code{fleet_table} can also be paired with \code{\link{sum_catch}} to aggregate total
  #' catch by unique vessel without the need to create an additional variable in advance:
  #'
  #' \tabular{ll}{
  #' condition:  'sum_catch(pollockMainDataTable, 'myProject', catch = 'COD', v_id = 'v_id', exp = 'COD >= 3', val = 'raw')'      \cr\cr                                                                                       \tab fleet\cr
  #' fleet: 'Cod'
  #' }
  #'
  # It may be useful to run \dontrun{subset(df, eval(parse(text = 'exp')))} to test that expressions have been formatted correctly.  @seealso
  # \code{\link{fleet_assign}} \code{\link{sum_catch}} @examples \dontrun{ fleet_table('MainDataTable', 'myProject', nb/';-==#' cond = c('GEAR ==
  # 8', 'species == 'cod'', 'area %in% c(640, 620)', NA), fleet_val = c('A', 'B', 'C', 'Other'), save = TRUE) }

  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset
  
  x <- 0
  
  if (!is.null(table) & (!is.null(cond) | !is.null(fleet_val))) {
    warning("Error: Either a table or cond and fleet_val arguments should be given.")
    x <- 1
  } else if ((!is.null(cond) & is.null(fleet_val)) | (!is.null(fleet_val) & is.null(cond))) {
    warning("Missing argument: both cond and fleet_val must be provided.")
    x <- 1
  }
  
  if (x == 0) {
    if (!is.null(cond)) {
      if (is.list(cond)) {
        if (any(sapply(cond, length) != length(fleet_val))) {
          warning("Fleet condition and fleet value lengths do not match.")
          x <- 1
        }
      } else if (length(cond) != length(fleet_val)) {
        warning("Fleet condition and fleet value lengths do not match.")
        x <- 1
      }
      
      if (x == 0) {
        f_tab <- data.frame(cond, fleet = fleet_val, stringsAsFactors = FALSE)
        
        if (is.null(names(cond)) | any(grepl("^cond", names(cond))) == FALSE) {
          if (is.list(cond)) {
            nm <- sapply(seq_along(cond), function(x) paste0("cond_", x))
            
            f_tab <- setNames(f_tab, c(nm, "fleet"))
          } else {
            f_tab <- setNames(f_tab, c("cond", "fleet"))
          }
        }
      }
    }
    
    if (!is.null(table)) {
      if (!is.data.frame(table)) {
        table <- as.data.frame(table)
      }
      
      f_tab <- table
      
      nm <- sapply(seq_along(f_tab[-1]), function(x) paste0("cond_", x))
      
      f_tab <- setNames(f_tab, c(nm, "fleet"))
    }
    
    nm <- names(f_tab)[grep("^cond", names(f_tab))]
    
    if (any(apply(f_tab[nm], 1, function(x) grepl("^\\s*$", x)))) {
      f_tab <- data.frame(apply(f_tab, 2, function(x) gsub("^\\s*$", NA, x)))
      
      warning("Empty strings found, replacing with NAs.")
    }
    
    # if (sum(apply(f_tab[nm], 1, function(x) all(is.na(x)))) != 1) {
    #   warning("One row of the fleet table must contain all NAs with the exception of \n
    #           the fleet column. This row will be for designating an 'other' category.")
    #   x <- 1
    #}
    
    if (x == 0) {
      if (save == TRUE) {
        # save to fishset_db
        if (table_exists(paste0(project, "FleetTable")) == FALSE) {
          fishset_db <- suppressWarnings(DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
          DBI::dbWriteTable(fishset_db, paste0(project, "FleetTable"), f_tab, overwrite = FALSE)
          DBI::dbDisconnect(fishset_db)
          cat("Table saved to fishset_db database")
        } else {
          fishset_db <- suppressWarnings(DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
          DBI::dbWriteTable(fishset_db, paste0(project, "FleetTable", Sys.Date()), f_tab, overwrite = FALSE)
          DBI::dbDisconnect(fishset_db)
          cat("Table saved to fishset_db database")
        }
      }
      
      fleet_table_function <- list()
      fleet_table_function$functionID <- "fleet_table"
      fleet_table_function$args <- list(dat, project, cond, fleet_val, table, save)
      fleet_table_function$msg <- f_tab
      log_call(fleet_table_function)
      
      save_table(f_tab, project, "fleet_table")
      
      f_tab
    }
  }
}




fleet_assign <- function(dat, project, fleet_tab, overlap = FALSE, format_tab = "long") {
  #' Create fleet variable based on stored fleet expressions
  #'
  #' Creates a fleet variable using a fleet table from the FishSET database.
  #'
  #' @param dat Primary data containing information on hauls or trips. Table in FishSET database contains the string 'MainDataTable'.
  #' @param project String, name of project.
  #' @param fleet_tab String, name of the fleet table stored in FishSET database. Should contain the
  #'    string `FleetTable`.
  #' @param overlap Logical; whether overlapping fleet assignments are allowed. Defaults to
  #'   FALSE.
  #' @param format_tab String; \code{"long"} outputs a single 'fleet' column. If overlap = TRUE,
  #'   observations with multiple fleet assignments are duplicated. \code{"wide"} outputs a column
  #'   for each fleet in the fleet table.
  #' @importFrom DBI dbConnect dbGetQuery dbDisconnect
  #' @importFrom RSQLite SQLite
  #' @importFrom reshape2 melt
  #' @export fleet_assign
  #' @return Returns the primary dataset with added fleet variable(s).
  #' @examples
  #' \dontrun{
  #' fleet_assign(pollockMainDataTable, 'myProject', fleet_tab = 'myProjectFleetTable', overlap = TRUE)
  #' }
  #' @seealso \code{\link{fleet_table}}

  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset
  
  x <- 0
  
  if (table_exists(fleet_tab) == FALSE) {
    warning("Table name does not exist in fishset_db. Check spelling or create a fleet assignment table with fleet_table().")
    x <- 1
  }
  
  if (x == 0) {
    fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase())
    f_tab <- DBI::dbGetQuery(fishset_db, paste0("SELECT * FROM", paste0("'", noquote(fleet_tab), "'")))
    DBI::dbDisconnect(fishset_db)
    
    nm <- names(f_tab)[grep("^cond", names(f_tab))]
    fleet_names <- f_tab$fleet
    
    if (any(grepl("other", fleet_names, ignore.case = TRUE)) == FALSE) {
      
      fleet_names <- c(fleet_names, "other") 
    }
    
    if (any(apply(f_tab, 2, FUN = function(x) grepl("sum_catch", x)))) {
      sc_cond <- apply(f_tab, 2, FUN = function(x) grepl("sum_catch", x))
      
      sc_rows <- data.frame(exp = f_tab[sc_cond], row = row(sc_cond)[sc_cond == TRUE])
      
      f_tab[apply(f_tab, 2, FUN = function(x) grepl("sum_catch", x))] <- NA
    }
    
    nm2 <- which(apply(f_tab[nm], 1, function(x) !is.na(x)))
    
    if (length(nm) > 1) {
      if (any(is.na(f_tab[nm]))) {
        cond_list <- apply(f_tab[nm], 1, function(x) {
          x[!is.na(x)]
        })
        
        if (!is.null(dim(cond_list))) {
          cond_vec <- vapply(cond_list, function(x) paste(x, collapse = " & "), 
                             FUN.VALUE = "character")
        } else if (is.null(dim(cond_list))) {
          cond_vec <- NULL
        }
      } else {
        cond_vec <- do.call(paste, c(f_tab[nm], sep = " & "))
      }
      
      if (!is.null(cond_vec)) {
        cond_tab <- do.call(cbind, c(lapply(seq_along(cond_vec), function(x) {
          with(dataset, eval(parse(text = cond_vec[x])))
        })))
      } else if (is.null(cond_vec)) {
        cond_tab <- NULL
      }
    } else if (length(nm) == 1) {
      cond_vec <- f_tab[!is.na(f_tab[nm]), -which(names(f_tab) == "fleet")]
      
      cond_tab <- do.call(cbind, c(lapply(cond_vec, FUN = function(x) {
        with(dataset, eval(parse(text = x)))
      })))
    }
    
    if (exists("sc_rows")) {
      sc_matrix <- apply(sc_rows["exp"], 1, function(x) {
        eval(parse(text = x))
      })
      
      if (anyNA(sc_matrix)) {
        # Treating NaNs produced by sum_catch when calculating percentage
        sc_matrix[is.na(sc_matrix)] <- FALSE
      }
      
      colnames(sc_matrix) <- sc_rows$row
      
      if (!is.null(cond_tab)) {
        colnames(cond_tab) <- nm2
        
        if (any(colnames(sc_matrix) %in% colnames(cond_tab))) {
          for (i in (colnames(sc_matrix)[colnames(sc_matrix) %in% colnames(cond_tab)])) {
            cond_tab[, colnames(cond_tab) == i] <- sc_matrix[, i] & cond_tab[, colnames(cond_tab) == i] # allow for ' | '
          }
        }
        
        if (any(!(colnames(sc_matrix) %in% colnames(cond_tab)))) {
          cond_tab <- cbind(cond_tab, sc_matrix[, colnames(sc_matrix)[which(!(colnames(sc_matrix) %in% colnames(cond_tab)))]])
          
          cond_tab <- cond_tab[, order(colnames(cond_tab))]
        }
      } else if (is.null(cond_tab)) {
        cond_tab <- sc_matrix
      }
    }
    # check for overlapping fleet assignments
    ovrlp <- which(apply(cond_tab, 1, function(x) sum(x) > 1))
    
    if (overlap == FALSE & length(ovrlp) > 0) {
      warning(paste(length(ovrlp), "overlapping fleet assingments detected."))
      x <- 1
    }
    
    if (x == 0) {
      # create 'other' category
      cond_tab <- cbind(cond_tab, apply(cond_tab, 1, function(x) ifelse(sum(x) == 0, 1, 0)))
      
      colnames(cond_tab) <- fleet_names
      
      cond_tab <- apply(cond_tab, 2, as.numeric)
      
      if (any(colnames(cond_tab) %in% colnames(dataset))) {
        colnames(cond_tab)[colnames(cond_tab) %in% colnames(dataset)] <- toupper(colnames(cond_tab)[colnames(cond_tab) %in% colnames(dataset)])
      }
      
      dataset <- cbind(dataset, cond_tab)
      
      if (format_tab == "long") {
        dataset <- reshape2::melt(dataset, measure.vars = colnames(cond_tab), variable.name = "fleet")
        
        dataset <- subset(dataset, value > 0)
        
        dataset$value <- NULL
        
        row.names(dataset) <- 1:nrow(dataset)
        
        dup <- which(duplicated(dataset[, -which(names(dataset) %in% "fleet")]))
        
        if (length(dup) > 0) {
          warning(paste(length(dup), "overlapping fleet assignments detected."))
        }
      } else if (format_tab == "wide" & length(ovrlp) > 0) {
        warning(paste(length(ovrlp), "overlapping fleet assingments detected."))
      }
      
      fleet_assign_function <- list()
      fleet_assign_function$functionID <- "fleet_assign"
      fleet_assign_function$args <- list(dat, project, fleet_tab, overlap, format_tab)
      log_call(fleet_assign_function)
      
      save_table(f_tab, project, "fleet_assign")
      
      dataset
    }
  }
}
