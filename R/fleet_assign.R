
fleet_table <- function(dat, project, cond = NULL, fleet_val = NULL, table = NULL, save = TRUE) {
  #' Define and store fleet expressions
  #'
  #' @description \code{fleet_table} saves a table of fleet expression to the FishSET database which
  #'   can then be applied to a dataset with fleet_assign. The table must contain a 'condition'
  #'   and 'fleet' column with each row corresponding to a set of expressions that will be used to
  #'   assign observations to fleets. A table can be created with the \code{cond} and \code{fleet_val}
  #'   arguments or by uploading an existing table that matches the format requirements. See 'Details'
  #'   below for examples of how tables can be formatted.
  #' @param dat Primary data containing information on hauls or trips. Table in the 
  #'   FishSET database contains the string 'MainDataTable'.
  #' @param project String, name of project.
  #' @param cond String; a vector containing valid R expressions saved as strings. 
  #'   Must be used with the \code{fleet_val} argument.
  #'   Each expression should be in quotes (double or single) with nested quotes indicated
  #'   with escaped quotes (\') or with the opposite quote used to contain the expression. For example,
  #'   "species == 'cod'" and "species == \'cod\'" are both valid.
  #' @param fleet_val String; a vector of fleet names to be assigned. Must be used with the
  #'   \code{cond} argument.
  #' @param table A data frame that has one condition column and one fleet column. See
  #'   'Details' for table formatting.
  #' @param save Logical; whether to save the current fleet_table to the FishSET database. Defaults to \code{TRUE}.
  #'   Tables are saved in the format of 'projectFleetTable'. Each project can only have one fleet table.
  #'   New fleet definitions are appended to the exiting fleet table. See \code{\link{table_remove}} to delete a table.
  #' @importFrom DBI dbConnect dbWriteTable dbDisconnect
  #' @importFrom RSQLite SQLite
  #' @importFrom stats setNames
  #' @export fleet_table
  #' @return Returns a table of fleet conditions that is saved to the FishSET database with the name
  #'   'projectFleetTable'.
  #' @details
  #'   Below is a simple example of a fleet table. For a fleet table to be created, it must contain
  #'   one "condition" column and one "fleet" column. Each fleet definition can be as long 
  #'   as necessary. For example, the first expression in the condition column example could also be
  #'   \code{"GEAR == 8 & species == 'pollock'"}. Use the '&' operator when combining expressions.
  #' \tabular{lllll}{
  #' condition                 \tab fleet\cr
  #' 'GEAR == 8'               \tab 'A'\cr
  #' 'species == 'cod'         \tab 'B'\cr
  #' 'area \%in\% c(640, 620)' \tab 'C'\cr
  #' }
  #'
  #' @examples 
  #' \dontrun{ 
  #' fleet_table("MainDataTable", "myProject", 
  #'             cond = c("GEAR == 8", "species == 'cod'", "area %in% c(640, 620)"),
  #'             fleet_val = c("A", "B", "C"), save = TRUE
  #'             ) 
  #' }
  #' 
  
  out <- data_pull(dat)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main")
  
  
  if (!is.null(table) & (!is.null(cond) | !is.null(fleet_val))) {
   
     warning("Error: Either a table or cond and fleet_val arguments should be given.")
    end <- TRUE
    
  } else if ((!is.null(cond) & is.null(fleet_val)) | (!is.null(fleet_val) & is.null(cond))) {
   
     warning("Missing argument: both cond and fleet_val must be provided.")
    end <- TRUE
  }
  
  if (end == FALSE) {
    if (!is.null(cond)) {
      if (length(cond) != length(fleet_val)) {
        warning("Fleet condition and fleet value lengths do not match.")
        end <- TRUE
      }
    }
      
    if (end == FALSE) {
      
      if (is.null(table)) {
        
        f_tab <- data.frame(condition = cond, fleet = fleet_val, stringsAsFactors = FALSE)
      
      } else {
        
        if (!is.data.frame(table)) {
          table <- as.data.frame(table)
        }
        
        if (ncol(table) != 2) {
          warning("Fleet table must have two columns.")
          end <- TRUE
        }
        
        if (all(colnames(table) %in% c("condition", "fleet")) == FALSE) {
          
          warning("Fleet table columns must be 'condition' and 'fleet'.")
          table <- stats::setNames(table, c("condition", "fleet"))
        }
        
        f_tab <- table
      }
    }
  }
    
  # check for incomplete rows
  unused_row <- lapply(f_tab, function(x) grepl("enter [condition|fleet|value]", x))
  empty_row <- lapply(f_tab, function(x) grepl("^\\s*$", x)) # empty/only whitespace
  drop <- NULL
    
  if (sum(unlist(unused_row)) > 0) {
    
    drop <- c(drop, unlist(lapply(unused_row, which)))
  }

  if (sum(unlist(empty_row)) > 0) {
    
    drop <- c(drop, unlist(lapply(empty_row, which)))
  }
  
  drop <- unique(drop)
  
  # drop unused rows
  if (!is.null(drop))  {
    
    f_tab <- f_tab[-drop, ]
    warning("Incomplete or empty rows detected and removed.")
  }

  if (end == FALSE) {
    if (save == TRUE) {
      # save to fishset_db
      fishset_db <- suppressWarnings(DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
      
      if (table_exists(paste0(project, "FleetTable")) == FALSE) {
        
        DBI::dbWriteTable(fishset_db, paste0(project, "FleetTable"), f_tab, overwrite = FALSE)
        cat("Table saved to fishset_db database\n")
        
      } else {
        
        db_tab <- table_view(paste0(project, "FleetTable"))
        f_tab <- rbind(db_tab, f_tab)
        DBI::dbWriteTable(fishset_db, paste0(project, "FleetTable"), f_tab, overwrite = TRUE)
        
        cat("Table saved to fishset_db database\n")
      }
      
      DBI::dbDisconnect(fishset_db)
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


fleet_assign <- function(dat, project, fleet_tab, assign = NULL, overlap = FALSE, format_var = "string") {
  #' Create fleet variable using fleet definition table
  #'
  #' Creates a fleet variable using a fleet table from the FishSET database.
  #'
  #' @param dat Primary data containing information on hauls or trips. Table in FishSET database contains the string 'MainDataTable'.
  #' @param project String, name of project.
  #' @param fleet_tab String, name of the fleet table stored in FishSET database. Should contain the
  #'    string `FleetTable`.
  #' @param assign Integer, a vector of row numbers from \code{fleet_tab}. Only 
  #'   fleet definitions in these rows will be used and added to `MainDataTable`. 
  #'   If \code{assign = NULL} (the default), all fleet definitions in the table will be used.  
  #' @param overlap Logical; whether overlapping fleet assignments are allowed. Defaults to
  #'   \code{FALSE}.
  #' @param format_var String. If \code{format_var = "string"}, a single column named 
  #' "fleet" will be added to `MainDataTable`. If \code{overlap = TRUE},
  #'   observations with multiple fleet assignments are duplicated. \code{format_var ="dummy"} 
  #'   outputs a binary column for each fleet in the fleet table. Defaults to \code{"string"}.
  #' @importFrom tidyr pivot_longer
  #' @importFrom shiny isRunning
  #' @export fleet_assign
  #' @return Returns the primary dataset with added fleet variable(s).
  #' @examples
  #' \dontrun{
  #' fleet_assign(pollockMainDataTable, 'pollock', fleet_tab = 'pollockFleetTable', overlap = TRUE)
  #' }
  #' @seealso \code{\link{fleet_table}}

  out <- data_pull(dat)
  dataset <- out$dataset
  
  if (shiny::isRunning()) {
    if (deparse(substitute(dat)) == "values$dataset") dat <- get("dat_name")
  } else { 
    if (!is.character(dat)) dat <- deparse(substitute(dat)) }
  
  end <- FALSE
  
  if (table_exists(fleet_tab) == FALSE) {
    warning("Table name does not exist in fishset_db. Check spelling or create a fleet definition table with fleet_table().")
    end <- TRUE
  }
  
  if (end == FALSE) {
    
    f_tab <- table_view(fleet_tab)

    # check if all conditions should be ran
    if (is.null(assign)) {
      
      fleet_names <- f_tab$fleet
      # boolean matrix: cols = conditions, rows = dataset row
      cond_tab <- do.call(cbind, c(lapply(f_tab$condition, FUN = function(x) {
        eval(parse(text = x), envir = dataset)
      })))
    
    } else {
      
      fleet_names <- f_tab$fleet[assign]
      
      # use selected conditions
      cond_tab <- do.call(cbind, c(lapply(f_tab$condition[assign], FUN = function(x) {
        eval(parse(text = x), envir = dataset)
      })))
    }
    
    if (any(grepl("other", fleet_names, ignore.case = TRUE)) == FALSE) {
      
      fleet_names <- c(fleet_names, "other") 
    }
     
    # check for overlapping fleet assignments
    ovrlp <- which(apply(cond_tab, 1, function(x) sum(x) > 1))
    
    if (length(ovrlp) > 0) {
      warning(paste(length(ovrlp), "overlapping fleet assingments detected."))
      
      if (overlap == FALSE) end <- TRUE
    }
    
    if (end == FALSE) {
      # create 'other' category
      cond_tab <- cbind(cond_tab, apply(cond_tab, 1, function(x) ifelse(sum(x) == 0, 1, 0)))
      
      if (format_var == "dummy") {
        
        fleet_names <- trimws(fleet_names)
        fleet_names <- gsub(" ", "_", fleet_names)
      }
      
      colnames(cond_tab) <- fleet_names
      
      # check if any fleet names are identical to dataset colnames
      if (any(colnames(cond_tab) %in% colnames(dataset))) {
        colnames(cond_tab)[colnames(cond_tab) %in% colnames(dataset)] <- toupper(colnames(cond_tab)[colnames(cond_tab) %in% colnames(dataset)])
      }
      
      dataset <- cbind(dataset, cond_tab)
      
      if (format_var == "string") {
        
        fleet_nm <- "fleet"
        
        # check if fleet colname exists
        if ("fleet" %in% colnames(dataset)) {
          col_num <- sum(grepl("fleet", colnames(dataset)))
          fleet_nm <- paste0("fleet_", col_num + 1)
        }
        
        value <- NULL
        dataset <- tidyr::pivot_longer(dataset, cols = !!fleet_names, names_to = fleet_nm)
        
        dataset <- subset(dataset, value > 0)
        
        dataset$value <- NULL
      }
      
      fleet_assign_function <- list()
      fleet_assign_function$functionID <- "fleet_assign"
      fleet_assign_function$args <- list(dat, project, fleet_tab, assign, overlap, format_var)
      log_call(fleet_assign_function)
      
      save_table(f_tab, project, "fleet_assign")
      
      dataset
    }
  }
}
