list_logs <- function(project = NULL, chron = FALSE, modified = FALSE) {
  #' View list of all log files
  #' @param project Project name. Displays all logs if NULL.
  #' @param chron Logical, whether to display logs in chronological order (TRUE) or
  #'   reverse chronological order (FALSE). 
  #' @param modified Logical, whether to include date modified. 
  #' @export
  
  if (is.null(project)) {
    
    logs <- lapply(projects(), function(x) list.files(loclog(project = x)))
    logs <- unlist(logs)
    
    if (modified) {
      
      log_mod <- lapply(projects(), function(x) {
        
        file.mtime(list.files(loclog(project = x), full.names = TRUE))
      })

      log_mod <- unlist(lapply(log_mod, as.character))
      log_df <- data.frame(log = logs, modified = log_mod)
    } 
    
  } else {
    
    logs <- list.files(loclog(project = project))
    
    if (modified) {
      
      log_mod <- file.mtime(paste0(loclog(project = project), logs))
      log_df <- data.frame(log = logs, modified = log_mod)
    } 
  }
  
  ord <- gsub("[^0-9]", "", logs)
  
  if (chron) {
    
    if (modified) log_df <- log_df[order(as.numeric(ord)), ]
    else logs <- logs[order(as.numeric(ord))]
    
  } else {
    
    if (modified) log_df <- log_df[order(-as.numeric(ord)), ]
    else logs <- logs[order(-as.numeric(ord))]
  }
  
  if (modified) log_df
  else logs
}

project_logs <- function(project, modified = FALSE) {
  #' List logs by project
  #' @param project Name of project.
  #' @param modified Logical, whether to show modification date. Returns a data
  #'  frame.
  #' @export
  
  logs <- list_logs(project, modified = modified)
  
  if (length(logs) == 0) message("Project \"", project, "\" empty or not found")
  else logs
}


current_log <- function(project) {
  #'
  #' Lists most recent log file
  #' @param project Project name. 
  #' @keywords internal
  #' @export
  #' @details Prints the name of the most recent log file, but not the filepath.
  
  if (!is.null(project)) logs <- project_logs(project)
  else logs <- list.files(loclog(project = project))

  if (!is.null(logs)) {
    
    g <- gsub("[^0-9]", "", logs)
    log <- logs[which(g == max(g, na.rm = TRUE))]
    
    log
    
  } else invisible(NULL)
}


pull_log <- function(project, log_date = NULL) {
  #'
  #'  pull log file
  #'
  #' @param project Project name. 
  #' @param log_date Date of log to be pulled. If \code{NULL}, then most recent 
  #'   log file is retrieved.
  #' @importFrom jsonlite fromJSON
  #' @keywords internal
  #' @export

  end <- FALSE

  if (is.null(log_date)) {
    
    log <- jsonlite::fromJSON(paste0(loclog(project = project), current_log(project)), 
                              simplifyVector = FALSE)
  
    } else {
      
      log_file <- grep(log_date, project_logs(project), value = TRUE)
      
    if (length(log_file) > 0) {
      
      log <- jsonlite::fromJSON(paste0(loclog(project = project), log_file), 
                                simplifyVector = FALSE)
      
    } else {
      
      warning("Log file does not exist.")
      end <- TRUE
    }
  }

  if (end == FALSE) log
}


current_out <- function(project) {
  #'
  #' Lists most recent output files
  #' @param project Project name
  #' @keywords internal
  #' @export
  #' @importFrom stringi stri_extract_first_regex
  #' @details Prints the name of the most recent output files.
  #' @examples
  #' \dontrun{
  #' current_out("pollock")
  #' }

  outs <- list.files(locoutput(project = project))

  cur <- stringi::stri_extract_first_regex(outs, "\\d{4}-\\d{2}-\\d{2}")
  
  if (is_value_empty(cur)) return(NULL)

  cur <- gsub("[^0-9]", "", cur)

  outs <- outs[which(cur == max(cur, na.rm = TRUE))]

  outs
}

project_files <- function(project) {
  #' List output files by project name
  #' 
  #' @param project Project name
  #' @export
  #' @examples 
  #' \dontrun{
  #' project_files("pollock")
  #' }
  
  outs <- list.files(locoutput(project = project))
  
  proj <- grep(paste0("^", project), outs, value = TRUE)
  
  if (length(proj) == 0) {
    
    message("No output files found for project ", "'", project, "'. ", sep = "")
    invisible(NULL)
  
  } else proj
}


pull_output <- function(project, fun = NULL, date = NULL, type = "plot", conf = TRUE) {
  #'
  #' Retrieve output file name by project, function, and type
  #'
  #' @param project Name of project
  #' @param fun Name of function.
  #' @param date Output file date in "%Y-%m-%d" format to retrieve. If \code{NULL}
  #'   the most recent output file is pulled.
  #' @param type Whether to return the \code{"plot"} (.png), \code{"table"} (.csv),
  #'  "notes" (.txt) or \code{"all"} files matching the project name, function, 
  #'  and date.
  #' @param conf Logical, whether to return suppressed confidential data. 
  #'    Unsuppressed output will be pulled if suppressed output is not available. 
  #' @importFrom stringi stri_extract_first_regex
  #' @export
  #' @examples
  #' \dontrun{
  #' pull_output("pollock", "species_catch", type = "plot")
  #' }
  end <- FALSE
  
  outs <- project_files(project)
  ext <- switch(type, "plot" = ".*\\.png$", "table" = ".*\\.csv$", 
                "notes" = ".*\\.txt$", "zone" = ".*\\.yaml")
  
  out <- grep(ext, outs, value = TRUE)
  
  if (length(out) == 0) {
    
    message("No ", type, " found for project '", project,"'.", sep = "")
    end <- TRUE
  }
  
  if (!end) {
    
    if (!is.null(fun)) {
      
      out <- grep(paste0("_", fun, "_"), out, value = TRUE)
      
      if (length(out) == 0) {
        
        message("No ", type, " output for function ", fun, " exists for project ",
                "'", project, "'. ", sep = "")
        end <- TRUE
      }
      
    } else out <- grep("_notes_", out, value = TRUE)
    
    if (!end) {
      
      if (is.null(date)) {
        
        dates <- stringi::stri_extract_first_regex(out, "\\d{4}-\\d{2}-\\d{2}")
        dates <- gsub("[^0-9]", "", dates)
        out <- out[which(dates == max(dates, na.rm = TRUE))]
       
      } else {
        
        out <- grep(date, out, value = TRUE)
        
        if (length(out) == 0) {
          
          message("No ", type, " output from ", date, " exists for project ", "'", 
                  project, "'. ", sep = "")
          end <- TRUE
        }
      }
      
      if (!end) {
        # check for suppressed output
        conf_ind <- grepl("_confid_", out)
        
        if (conf) {
          
          if (sum(conf_ind) > 0) out <- out[conf_ind]
          
        } else {
          
          if (sum(!conf_ind) > 0) out <- out[!conf_ind]
        }
        
        out
      }
    }
  }
}

pull_table <- function(project, fun, date = NULL, conf = TRUE) {
  #' 
  #' Import and format table to notebook file
  #' 
  #' @param project Project name.
  #' @param fun String, the name of the function that created the table. 
  #' @param date the date the table was created. If NULL, then the most recent 
  #'   version is retrieved. 
  #' @param conf Logical, whether to return suppressed confidential data. 
  #'    Unsuppressed output will be pulled if suppressed output is not available.
  #' @export
  #' @examples
  #' \dontrun{
  #' pull_table("pollock", "vessel_count")
  #' }
  #' 
  out <- pull_output(project = project, fun = fun, date = date, type = "table",
                     conf = conf)
  
  if (!is.null(out)) {
    
    if (fun == "summary_stats") summary_table(project)
    else table_format(out, project)
  }
}


pull_plot <- function(project, fun, date = NULL, conf = TRUE) {
  #' 
  #' Import and format plots to notebook file
  #' 
  #' @param project Project name.
  #' @param fun String, the name of the function that created the plot. 
  #' @param date the date the plot was created. If NULL, then the most recent 
  #'   version is retrieved. 
  #' @param conf Logical, whether to return suppressed confidential data. 
  #'   Unsuppressed output will be pulled if suppressed output is not available.
  #' @export
  #' @importFrom knitr include_graphics
  #' @examples
  #' \dontrun{
  #' pull_plot("pollock", "density_plot")
  #' }
  
  out <- pull_output(project = project, fun = fun, date = date, type = "plot", 
                     conf = conf)
  
  if (!is.null(out)) {
    
    knitr::include_graphics(paste0(locoutput(project = project), out))
  } 
}


current_db_table <- function(project, table) {
  #'
  #' Retrieve name of the most recent table from a project
  #'
  #' @param project Name of project.
  #' @param table Name of table, e.g. "MainDataTable".
  #' @export
  #' @keywords internal
  
  tab <- tables_database(project=project)
  
  proj_tab <- grep(paste0(project, table), tab, value = TRUE)
  exists <- TRUE
  
  if (table == "MainDataTable") {
    proj_tab <- proj_tab[!grepl("Info", proj_tab)]
  } else if (table == "MainDataTableInfo") {
    proj_tab <- grep("Info", proj_tab, value = TRUE)
  }
  
  if (length(proj_tab) == 0) {
    
    exists <- FALSE
    tab_out <- paste0(project, table," not found.")
    
  } else if (length(proj_tab) == 1) {
    
    tab_out <- proj_tab
    
  } else if (length(proj_tab) > 1) {
    # pull most recent table
    new_tab <- gsub("[^0-9\\.]", "", proj_tab)
    tab_out <- proj_tab[which(new_tab == max(new_tab, na.rm = TRUE))]
    
    if (!table_exists(tab_out, project)) {
      
      exists <- FALSE
      tab_out <- paste0(project, table," not found.")
    }
    
  } else {
    
    exists <- FALSE
    tab_out <- paste0(project, table," not found.")
  }
  
  list(table = tab_out,
       exists = exists)
}


table_format <- function(x, project) {
  #' 
  #' Import and format saved tables to notebook file
  #' 
  #' @param x Name of table saved in output
  #' @param project project name
  #' @keywords internal
  #' @export
  #' @importFrom pander panderOptions pander
  #' @seealso \code{\link{pull_output}}
  #' @examples
  #' \dontrun{
  #' table_format("pollock_species_catch_2020-05-29.csv", 'pollock')
  #' table_format(pull_output("pollock", "species_catch", type = "table"), 'pollock')
  #' }

  if (length(x) == 1) {
    
    tab_int <- read.csv(paste0(locoutput(project = project), x))
  
  } else {
    
    tab_int <- lapply(x, function(i) {
      
      read.csv(file = paste0(locoutput(project = project), i))
    })
    
  }
  
  pander::panderOptions("table.alignment.default", function(df) {
    ifelse(sapply(df, is.numeric), "right", "left")
  })
  pander::panderOptions("table.emphasize.rownames", TRUE)
  pander::panderOptions("table.split.table", Inf)
  pander::panderOptions("graph.fontsize", 8)
  pander::panderOptions("table.style", "multiline")
  
  if (all(grepl("summary", x))) {
    colnames(tab_int)[1] <- "Variable"
    pander::pander(tab_int)
    
  } else {
    
    pander::pander(tab_int)
  }
}


plot_format <- function(x, project) {
  #' 
  #' Import and format plots to notebook file
  #' 
  #' @param x Name of plot saved in output
  #' @param project Name of project
  #' @keywords internal
  #' @export
  #' @importFrom knitr include_graphics
  #' @examples
  #' \dontrun{
  #' plot_format("pollock_species_catch_2020-05-29.png")
  #' plot_format(pull_output("pollock", "species_catch", type = "plot"))
  #' }
  
  project <- project
  knitr::include_graphics(paste0(locoutput(project = project), x))
}

insert_plot <- function(out, project) {
  #' 
  #' Insert plot from user folder
  #' 
  #' @param out String, plot file name.
  #' @param project Name of project.
  #' @export
  #' @importFrom knitr include_graphics
  #' @examples
  #' \dontrun{
  #' insert_plot("pollock_plot.png")
  #' }
  
  if (!is.null(get_user_locoutput(project))){
    
    if (file.exists(paste0(get_user_locoutput(project), out))) {
      
      knitr::include_graphics(paste0(get_user_locoutput(project), out))
      
    } else message("Plot not found.")
  }
}

insert_table <- function(out, project) {
  #' 
  #' Insert table from user folder
  #' 
  #' @param out String, table file name.
  #' @param project Name of project. 
  #' @export
  #' @examples
  #' \dontrun{
  #' insert_table("pollock_table.csv")
  #' }
  if (!is.null(get_user_locoutput(project))) {
    
    if (!file.exists(paste0(get_user_locoutput(project), out))) {
      
      message("Table not found.")
      
    } else {
      
      tab <- read.csv(paste0(get_user_locoutput(project), out))
      tab
    }
  }
}

pull_notes <- function(project, date = NULL, output = "print") {
  #' 
  #' Pull notes from output folder
  #'
  #' @param project String, the project name.
  #' @param date String, date to pull notes from. If NULL, most recent note file is 
  #'   retrieved.
  #' @param output Output type. "print" returns formatted notes. "string" returns a 
  #'   character vector of the notes. "print" is recommended for displaying notes in a report.  
  #' @export
  #' @importFrom stringi stri_omit_empty
  #' @importFrom stringi stri_extract_first_regex
  #' @details Notes are saved to the output folder by project name and date. If date is not
  #'   specified then the most recent notes file with the project name is pulled. Notes are
  #'   are also saved by FishSET app session; if more than one session occurred in the same day, each 
  #'   session's notes are pulled and listed in chronological order. 
  
  out <- pull_output(project = project, date = date, type = "notes")
  
  note_names <- c("Data quality evaluation: ", "Simple analysis: ",
                  "Data exploration: ", "Upload data: ", "Fleet functions: ",
                  "Create new variable: ", "Alternative choice: ", 
                  "Expected catch/revenue: ", "Models: ", "Bookmark URL: ")
  
  if (length(out) == 1) {
    
    notes <- readLines(paste0(locoutput(project = project), out))
    notes <- stringi::stri_omit_empty(notes)
    ind <- which(notes %in% note_names)
    
    for (i in ind) notes[i] <- paste0("\n", notes[i])
    
    if (output == "string") {
      paste(notes, collapse = "\n")
      
    } else if (output == "print") {
      
      cat(notes, sep = "\n")
    }
  } else if (length(out) > 1) { # if users has multiple notes from different sessions
    # order notes chronologically
    n_id <- stringi::stri_extract_first_regex(out, "\\d{4}-\\d{2}-\\d{2}.*")
    n_id <- gsub("[^0-9]", "", n_id)
    n_id <- order(n_id)
    
    notes <- lapply(out, function(n) readLines(paste0(locoutput(project = project), n)))
    notes <- lapply(n_id, function(x) notes[[x]])
    notes <- lapply(notes, stringi::stri_omit_empty)
    
    for (n in seq_along(notes)) {
      
      ind <- which(notes[[n]] %in% note_names)
      for (i in ind) {
        notes[[n]][[i]] <- paste0("\n(", n, ") ", notes[[n]][[i]])
      }
    }
    
    if (output == "string") paste(unlist(notes), collapse = "\n")
    else if (output == "print") cat(unlist(notes), sep = "\n")
  }
}

parse_notes <- function(project, date = NULL, section, output = "print") {
  #'
  #' Selectively display a note section
  #' 
  #' @param project The project name.
  #' @param date Date to pull notes from. If NULL then the most recent version of notes
  #'   from the project are retrieved. 
  #' @param section The note section to display. Options include "upload" for Upload data,
  #'   "quality" for Data quality evaluation, "explore" for Data exploration, "fleet" for
  #'   Fleet functions, "analysis" for Simple analysis, "new_variable" for Create new variable,
  #'   "alt_choice" for Alternative choice, "models", and "bookmark". 
  #' @param output Output type. "print" returns formatted notes. "string" returns a 
  #'   character vector of the notes. "print" is recommended for displaying notes in a report.
  #' @export
  #' @examples 
  #' \dontrun{
  #' parse_notes("pollock", type = "explore")
  #' }
  
  notes <- pull_notes(project = project, date = date, output = "string")
  
  if (length(notes) == 0) {
    
    message("No notes found for project", project, date)
  
  } else {
  
    split <- unlist(strsplit(notes, "\n\n"))
    
    note_type <-  switch(section, "upload" = "Upload data: ", "quality" = "Data quality evaluation: ", 
                         "explore" = "Data exploration: ", "fleet" = "Fleet functions: ", 
                         "analysis" =  "Simple analysis: ","new_variable" = "Create new variable: ", 
                         "alt_choice" =  "Alternative choice: ", "expected_catch" = "Expected catch/revenue: ", 
                         "models" = "Models: ", "bookmark" = "Bookmark URL: ")
    
    notes <- grep(note_type, split, value = TRUE)
    
    if (output == "string") notes
    else if (output == "print") cat(notes, sep = "\n")
  }
}

summary_table <- function(project, output = "print") {
  #'
  #' Display dataset summary table
  #'
  #' @param project Name of project.
  #' @param output Output type. "print" returns formatted notes. "table" returns a 
  #'   dataframe. "print" is recommended for displaying summary table in a report.
  #' @export
  #' @keywords internal
  #' @importFrom tibble rownames_to_column
  #' @importFrom pander pander pandoc.table
  #' @details Displays the most recent table created by \code{\link{summary_stats}}
  #' as a dataframe. Can be used in console or notebook.

  #date <- gsub(".json", "", current_log())
  sum_out <- pull_output(project = project, fun = "summary_stats", type = "table")
  
  if (length(sum_out) == 0) {
    
    message("Summary table for project '", project, "' not found.", sep = "")
  
  } else {

    sum_tab <- read.csv(paste0(locoutput(project = project), sum_out),
      strip.white = TRUE, check.names = FALSE)
  
    rownames(sum_tab) <- c("Min", "Median", "Mean", "Max", "Missing", "Unique Obs.", 
                           "No. 0's")
  
    sum_tab <- apply(sum_tab, 2, function(x) gsub(".*:", "", x))
  
    sum_tab <- apply(sum_tab, 2, function(x) trimws(x))
  
    sum_tab <- as.data.frame(t(sum_tab))
  
    sum_tab <- tibble::rownames_to_column(sum_tab, "Variable")
  
    sum_tab <- sum_tab[-1, ]
  
    if (output == "print") {
      
      pander::pander(
        pander::pandoc.table(sum_tab, style = "simple", row.names = FALSE, split.tables = Inf)
      )
    
    } else if (output == "table") sum_tab
  }
}


function_summary <- function(project, date = NULL, type = "dat_load", show = "all") {
  #'
  #' Display summary of function calls
  #'
  #' @param project Project name. 
  #' @param date Character string; the date of the log file ("%Y-%m-%d" format) to
  #'   retrieve. If \code{NULL} the most recent log is pulled.
  #' @param type The type of function to display. "dat_load", "dat_quality", "dat_create",
  #'   "dat_exploration", "fleet", and "model".
  #' @param show Whether to display \code{"all"} calls, the \code{"last"} (most recent) call, or
  #' the \code{"first"} (oldest) function call from the log file.
  #' @importFrom dplyr bind_rows
  #' @details Displays a list of functions by type and their arguments from a log file.
  #'   If no date is entered the most recent log file is pulled.
  #' @export
  #' @seealso \code{\link{filter_summary}}
  #' @examples
  #' \dontrun{
  #' function_summary("pollock")
  #' }

  dat_load <- c(
    "load_data", "load_maindata", "load_port", "load_aux","load_gridded", 
    "load_spatial", "merge_dat", "split_dat", "write_dat"
  )

  dat_quality <- c(
    "data_verification", "data_check", "nan_identify", "nan_filter", "na_filter",
    "outlier_table", "outlier_plot", "outlier_plot_int", "outlier_remove", "degree", 
    "unique_filter", "empty_vars_filter", "check_model_data", "filter_table", 
    "filter_dat", "add_vars", "changeclass", "summary_stats", "spatial_qaqc"
  )

  dat_create <- c(
    "temporal_mod", "ID_var", "create_seasonal_ID", "cpue", "dummy_var",
    "dummy_num", "dummy_matrix", "set_quants", "bin_var", "create_var_num",
    "create_mid_haul", "create_trip_centroid", "create_dist_between",
    "create_duration", "create_startingloc", "haul_to_trip", 
    "randomize_value_row", "randomize_value_range", "jitter_lonlat", 
    "randomize_lonlat_zone", "lonlat_to_centroid", "assignment_column",
    "create_dist_between_for_gui", "group_perc", "group_diff", "group_cumsum",
    "create_trip_distance" 
  )

  dat_exploration <- c(
    "map_plot", "map_kernel", "getis_ord_stats", "moran_stats", "temp_plot", 
    "xy_plot", "corr_out", "map_viewer", "spatial_hist", "spatial_summary",
    "view_grid_dat"
  )

  fleet <- c(
    "fleet_table", "density_plot", "fleet_assign", "vessel_count", "species_catch", "bycatch",
    "sum_catch", "weekly_catch", "weekly_effort", "trip_length", "roll_catch"
  )

  alt_choice <- c("create_alternative_choice")

  model <- c(
    "sparsetable", "sparsplot", "create_expectations", "make_model_design",
    "discretefish_subroutine", "check_model_data", "log_func_model",
    "create_alternative_choice", "temp_obs_table"
  )

  fun_vector <- switch(type, "dat_load" = dat_load, "dat_quality" = dat_quality,
    "dat_create" = dat_create, "dat_exploration" = dat_exploration,
    "fleet" = fleet, "alt_choice" = alt_choice, "model" = model
  )

  end <- FALSE

  p_log <- pull_log(project, log_date = date)
  
  if (is.null(p_log)) end <- TRUE
  
  log_date <- p_log[[1]][[1]]$info[[1]]$rundate

  # grab all function calls
  fun_calls <- lapply(
    seq_along(p_log$fishset_run[[2]]$function_calls),
    function(x) p_log$fishset_run[[2]]$function_calls[[x]]$functionID
  )

  c_vars <- unique(unlist(fun_calls[unlist(lapply(fun_vector, function(x) grep(x, fun_calls)))]))

  ind <- lapply(c_vars, function(x) grep(x, fun_calls))

  if (length(ind) == 0) {
    
    message("No functions of type \"", type, "\" found in log.")
    end <- TRUE
  } 
  
  if (end == FALSE) {
    
    fun_list <- lapply(ind, function(x) {
      
      lapply(x, function(i) p_log$fishset_run[[2]]$function_calls[[i]])
    })

    names(fun_list) <- c_vars

    # determine if all function args are present in log
    args <- lapply(c_vars, function(x) names(formals(x))[names(formals(x)) != "..."])

    arg_len <- lapply(args, length)

    names(arg_len) <- c_vars

    arg_match <- sapply(names(arg_len), function(x) {
      
      sapply(seq_along(fun_list[[x]]), function(i) {
        
        arg_len[x] == length(fun_list[[x]][[i]]$args)
      }, USE.NAMES = TRUE, simplify = FALSE)
    }, USE.NAMES = TRUE, simplify = FALSE)
    
    # find functions w/ kwargs
    f_args <- sapply(c_vars, function(x) names(formals(x)), 
                     USE.NAMES = TRUE, simplify = FALSE)
    
    kwargs_nm <- names(f_args)[which(vapply(f_args, function(x) any(x == "..."), 
                                            FUN.VALUE = logical(1)))]

    if (all(unlist(arg_match)) == TRUE) {
      for (n in names(fun_list)) {
        for (i in seq_along(fun_list[[n]])) {
          names(fun_list[[n]][[i]]$args) <- names(formals(n))[names(formals(n)) != "..."]
        }
      }
    } else {
      arg_match <- sapply(arg_match, unlist, USE.NAMES = TRUE, simplify = FALSE)

      match <- sapply(arg_match, which, USE.NAMES = TRUE, simplify = FALSE)

      no_match <- sapply(arg_match, function(x) which(x == FALSE),
        USE.NAMES = TRUE, simplify = FALSE
      )

      if (length(unlist(match)) > 0) {
        for (n in names(match)) {
          for (i in match[[n]]) {
            names(fun_list[[n]][[i]]$args) <- names(formals(n))[names(formals(n)) != "..."]
          }
        }
      }
      # create generic names for unmatched arguments
      for (n in names(no_match)) {
        for (i in no_match[[n]]) {
          names(fun_list[[n]][[i]]$args) <- sapply(seq_along(fun_list[[n]][[i]]$args), function(x) {
            paste("arg", x, sep = "_")
          })
        }
      }
    }
    
    # add kwargs to list
    if (length(kwargs_nm) > 0) {
      
      for (n in kwargs_nm) {
        for(i in seq_along(fun_list[[n]])) {
          if (!is.null(unlist(fun_list[[n]][[i]]$kwargs))) {
            
            fun_list[[n]][[i]]$args$kwargs <- unlist(fun_list[[n]][[i]]$kwargs)
            
          } else {
            
            fun_list[[n]][[i]]$args$kwargs <- "NULL"
          }
        }
      }
    }

    # replace NULLs and zero length args with string
    for (n in names(fun_list)) {
      for (i in seq_along(fun_list[[n]])) {
        for (a in seq_along(fun_list[[n]][[i]]$args)) {
          if (is.null(fun_list[[n]][[i]]$args[[a]])) {
            fun_list[[n]][[i]]$args[[a]] <- "NULL"
          } else if (length(fun_list[[n]][[i]]$args[[a]]) == 0) {
            fun_list[[n]][[i]]$args[[a]] <- "NULL"
          }
        }
      }
    }

    # Collaspe vectors, lists, and tables
    for (n in names(fun_list)) {
      for (i in seq_along(fun_list[[n]])) {
        for (a in seq_along(fun_list[[n]][[i]]$args)) {
          if (length(fun_list[[n]][[i]]$args[[a]]) > 1) {
            fun_list[[n]][[i]]$args[[a]] <- paste(fun_list[[n]][[i]]$args[[a]],
              collapse = ", "
            )
          } else {
            fun_list[[n]][[i]]$args[[a]] <- fun_list[[n]][[i]]$args[[a]]
          }
        }
      }
    }

    # create list of dataframes
    df_list <- list()

    for (n in names(fun_list)) {
      for (i in seq_along(fun_list[[n]])) {
        df_list[[n]][[i]] <- as.data.frame(fun_list[[n]][[i]]$args)

        df_list[[n]][[i]]$function_name <- n
      }
    }
    
    # convert any column that isn't a character vector to character
    for (n in names(df_list)) {
      for (i in seq_along(df_list[[n]])) {
        if (any(vapply(df_list[[n]][[i]], FUN = function(x) !is.character(x), FUN.VALUE = logical(1)))) {
          
          chr_ind <- which(vapply(df_list[[n]][[i]], FUN = function(x) !is.character(x), FUN.VALUE = logical(1)))
          for (nc in chr_ind) {
            
            df_list[[n]][[i]][[nc]] <- as.character(df_list[[n]][[i]][[nc]])
          }
        }
      }
    }

    df_list2 <- list()

    for (i in seq_along(df_list)) {
      df_list2[[i]] <- suppressWarnings(dplyr::bind_rows(df_list[[i]]))

      n <- colnames(df_list2[[i]])[colnames(df_list2[[i]]) != "function_name"]

      df_list2[[i]][["id"]] <- 1:nrow(df_list2[[i]])

      df_list2[[i]] <- df_list2[[i]][, c("id", "function_name", n)]
    }

    if (show == "first") {
      for (i in seq_along(df_list2)) {
        df_list2[[i]] <- df_list2[[i]][1, ]
      }
    } else if (show == "last") {
      for (i in seq_along(df_list2)) {
        df_list2[[i]] <- df_list2[[i]][nrow(df_list2[[i]]), ]
      }
    } else if (show == "all") {
      df_list2 <- df_list2
    }

    names(df_list2) <- names(fun_list)

    df_list2$date <- log_date

    df_list2 <- df_list2[c("date", c_vars)]

    df_list2
  }
}


filter_summary <- function(sum_tab, filter_list) {
  #'
  #' Select function calls to display
  #'
  #' @param sum_tab Summary table to filter.
  #' @param filter_list A named list of integers. Each list entry should
  #'  contain the name of the function and the row number(s) to filter by.
  #'  For example, \code{list(temporal_mod = 2)} will display the second row of
  #'   the temporal_mod dataframe in a summary list.
  #' @export
  #' @keywords internal
  #' @seealso \code{\link{function_summary}}
  #' @examples
  #' \dontrun{
  #' filter_summary(function_summary(),
  #'               filter_list = list(set_quants = 2, temporal_mod = 2))
  #' }


  for (i in names(filter_list)) {
    sum_tab[[i]] <- sum_tab[[i]][filter_list[[i]], ]
  }

  sum_tab
}

view_model_design <- function(project, date = NULL) {
  #' View model design file in database
  #' 
  #' @param project Project name.
  #' @param date String, date model design file was created. 
  #' @export
  
  tab_name <- paste0(project, "modelDesignTable")
  
  if (!is.null(date)) tab_name <- paste0(tab_name, gsub("-", "", date))
  
  if (!table_exists(tab_name, project)) message(tab_name, "not found.")
  else table_view(tab_name, project)
}

model_out_summary <- function(project, output = "print") {
  #'
  #' Retrieve most recent summary of model output
  #'
  #' @param project Name of project
  #' @param output Output type. "print" returns formatted notes. "table" returns a 
  #'   dataframe. "print" is recommended for displaying summary table in a report.
  #' @export
  #' @importFrom pander pander pandoc.table
  #' @keywords internal
  #' @examples
  #' \dontrun{
  #' model_out_summary("pollock")
  #' }

  pull_out <- current_db_table(project, "modelOut")
  
  if (pull_out$exists == FALSE) {
    
    message(project, "modelOut not found.")
  
  } else {
    
    p_mod <- pull_out$table
    
    results <- model_out_view(p_mod, project)
    
    modeltab <- data.frame(
      Model_name = rep(NA, length(results)),
      covergence = rep(NA, length(results)),
      Stand_Errors = rep(NA, length(results)),
      Hessian = rep(NA, length(results))
    )
    
    for (i in seq_along(results)) {
      modeltab[i, 1] <- results[[i]]$name
      modeltab[i, 2] <- results[[i]]$optoutput$convergence
      modeltab[i, 3] <- toString(round(results[[i]]$seoutmat2, 3))
      modeltab[i, 4] <- toString(round(results[[i]]$H1, 5))
    }
    
    if (output == "print") {
      
      pander::pander(
        pander::pandoc.table(modeltab, style = "simple", row.names = FALSE, split.table = Inf)
      )
      
    } else if (output == "table") modeltab
  }
}


model_error_summary <- function(project, output = "print") {
  #'
  #' Retrieve most recent summary of model error
  #'
  #' @param project Name of project
  #' @param output Output type. "print" returns formatted notes. "table" returns a 
  #'   dataframe. "print" is recommended for displaying summary table in a report.
  #' @export
  #' @importFrom pander pander pandoc.table
  #' @keywords internal
  #' @examples
  #' \dontrun{
  #' model_error_summary("pollock")
  #' }

  pull_out <- current_db_table(project, "modelOut")

  if (pull_out$exists == FALSE) {
    
    message(project, "modelOut not found.")
    
  } else {
    
    p_mod <- pull_out$table 
    results <- model_out_view(p_mod, project)
  
    error_out <- data.frame(
      Model_name = rep(NA, length(results)),
      Model_error = rep(NA, length(results)),
      Optimization_error = rep(NA, length(results))
    )
  
    for (i in seq_along(results)) {
      error_out[i, 1] <- results[[i]]$name
      error_out[i, 2] <- ifelse(is.null(results[[i]]$errorExplain), "No error reported",
        toString(results[[i]]$errorExplain)
      )
      error_out[i, 3] <- ifelse(is.null(results[[i]]$optoutput$optim_message), "No message reported",
        toString(results[[i]]$optoutput$optim_message)
      )
    }
  
    if (output == "print") {
      
      pander::pander(
        pander::pandoc.table(error_out, style = "simple", row.names = FALSE, split.table = Inf)
      )
      
    } else if (output == "table") error_out
  }
}


model_fit_summary <- function(project, output = "print") {
  #'
  #' Retrieve most recent summary of model fit
  #'
  #' @param project Name of project
  #' @param output Output type. "print" returns formatted notes. "table" returns a 
  #'   dataframe. "print" is recommended for displaying summary table in a report.
  #' @export
  #' @importFrom pander pander pandoc.table
  #' @keywords internal
  #' @examples
  #' \dontrun{
  #' model_fit_summary("pollock")
  #' }

  pull_out <- current_db_table(project, "modelOut")
  
  if (pull_out$exists == FALSE) {
    
    message(project, "modelOut not found.")
  
  } else {
    
    p_mod <- pull_out$table 
    results <- model_out_view(p_mod, project)
  
    fit_tab <- data.frame(
      Model_name = rep(NA, length(results)),
      AIC = rep(NA, length(results)), AICc = rep(NA, length(results)),
      BIC = rep(NA, length(results)), PseudoR2 = rep(NA, length(results))
    )
  
    for (i in seq_along(results)) {
      fit_tab[i, 1] <- results[[i]]$name
      fit_tab[i, 2] <- results[[i]]$MCM$AIC
      fit_tab[i, 3] <- results[[i]]$MCM$AICc
      fit_tab[i, 4] <- results[[i]]$MCM$BIC
      fit_tab[i, 5] <- results[[i]]$MCM$PseudoR2
    }
  
    if (output == "print") {
      
      pander::pander(
        pander::pandoc.table(fit_tab, style = "simple", row.names = FALSE, split.table = Inf)
      )
      
    } else if (output == "table") fit_tab
  }
}

view_fleet_table <- function(project) {
  #'
  #'View the most recent fleet table by project
  #'
  #' @param project The name of project.
  #' @export
  #' @importFrom pander pander pandoc.table
  #' 
  #' @examples 
  #' \dontrun{
  #' view_fleet_table("pollock")
  #' }
  
  fleet_tab <- current_db_table(project, table = "FleetTable")
  
  if (fleet_tab$exists == TRUE) {
    pander::pander(
      pander::pandoc.table(
        table_view(fleet_tab$table, project),
        style = "simple", row.names = FALSE, split.table = Inf)
    )
    
  } else message("No fleet table found.")
}


pull_meta <- function(project, tab.name = NULL, tab.type = NULL, format = FALSE) {
  #' Retrieve/display meta data by project
  #' 
  #' @param project Project name.
  #' @param tab.name String, table name. Optional, used to filter output to a specific
  #'   table. 
  #' @param tab.type String, table type. Optional, used to filter output. Options 
  #'   include "main", "spat" (spatial), "port", "grid" (gridded), and "aux" (auxiliary).
  #' @param format Logical, whether to format output using \code{\link[pander]{pander}}.
  #'   Useful for displaying in reports. 
  #' @export
  #' @importFrom jsonlite read_json
  #' @importFrom pander pander
  
  if (!is.null(tab.type) && !(tab.type %in% c("main", "port", "spat", "grid", "aux"))) {
    
    warning("Invalid table type. Choose \"main\", \"port\", \"spat\", \"grid\", or \"aux\".")
    return(invisible(NULL))
  }
  
  if (project_exists(project) == FALSE) {
    
    warning("Project \"", project, "\" does not exist.")
    invisible(NULL)
    
  } else {
    
    m_file <- paste0(locproject(), "/", project, "/doc/meta_log.json")
    
    if (!file.exists(m_file)) {
      
      warning("A meta log does not exist for project \"", project, ".\"")
      invisible(NULL)
      
    } else {
      
      meta_log <- jsonlite::read_json(m_file, simplifyVector = TRUE)
      
      if (!is.null(tab.name)) {
        
        if (tab.name %in% names(meta_log$table)) {
          
          meta_log <- meta_log$table[[tab.name]]
          
        } else {
          
          warning(paste0("Table '", tab.name, "' not found." ))
          return(invisible(NULL))
        }
        
      } else if (!is.null(tab.type)) {
        
        ind <- vapply(meta_log$table, function(x) x$type == tab.type, logical(1))
        
        meta_log <- meta_log$table[ind]
      } 
      
      if (format) pander::pander(meta_log)
      else meta_log
    }
  }
}


delete_meta <- function(project, tab.name = NULL, delete_file = FALSE) {
  #' Delete table meta data or project meta file
  #' 
  #' @param project Project name.
  #' @param tab.name String, table name. 
  #' @param delete_file Logical, whether to delete project meta file. 
  #' @export
  #' @importFrom jsonlite read_json toJSON
  
  if (project_exists(project) == FALSE) {
    
    warning("Project \"", project, "\" does not exist.")
    invisible(FALSE)
    
  } else {
    
    m_file <- paste0(locproject(), "/", project, "/doc/meta_log.json")
    
    if (!file.exists(m_file)) {
      
      warning("A meta log does not exist for project\"", project, ".\"")
      invisible(FALSE)
      
    } else {
      
      if (is.null(tab.name) & delete_file) { # remove meta file
        
        unlink(m_file)
        message("metadata removed.")
        invisible(TRUE)
        
      } else {
        
        if (is.null(tab.name)) {
          
          warning("Please specific table.")
          return(invisible(FALSE))
        }
        
        meta_log <- jsonlite::read_json(m_file)
        
        if (!(tab.name %in% names(meta_log$table))) {
          
          warning(paste0("Table '", tab.name, "' not found"))
          invisible(FALSE)
          
        } else {
          
          if (length(meta_log$table) == 1) {
            
            unlink(m_file)
            message(paste(tab.name, "metadata removed."))
            invisible(TRUE)
            
          } else {
            
            meta_log$table[[tab.name]] <- NULL
            meta_log <- jsonlite::toJSON(meta_log, pretty = TRUE, auto_unbox = TRUE, 
                                         null = "null", na = "string")
            
            write(meta_log, m_file)
            message(paste(tab.name, "metadata removed."))
            invisible(TRUE)
          }
        }
      }
    } 
  }
}


meta_tables <- function(project, tab.type = NULL) {
  #' Print meta tables by project and/or type
  #' 
  #' @param project Name of project.
  #' @param tab.type String, table type. Optional, used to filter output. Options 
  #'   include "main", "spat" (spatial), "port", "grid" (gridded), and "aux" 
  #'   (auxiliary).
  #' @export
  #' 
  
  meta_log <- pull_meta(project)
  
  if (!is.null(meta_log)) {
    
    if (!is.null(tab.type)) {
      
      ind <- vapply(meta_log$table, function(x) x$type == tab.type, logical(1))
      
      if (sum(ind) == 0) {
        
        warning(paste0("No tables of type '", tab.type, "' found"))
        return(invisible(NULL))
        
      } else {
        
        out <- names(meta_log$table[ind])
      }
      
    } else {
      
      out <- names(meta_log$table)
    }
    
    out
  }
}


table_type <- function(tab) {
  #' Detect table type
  #' 
  #' @param tab FishSET table.
  #' @importFrom stringi stri_extract_first_regex
  #' @keywords internal
  #' @examples
  #' \dontrun{
  #' table_type(pollockMainDataTable) # returns "main"
  #' }
  
  if (!is.character(tab)) {
    
    warning("Invalid input. Table name must be a string.")
    
  } else if (length(tab) > 1) {
    
    warning("Table name must be a vector of length 1.")
    
  } else {
    
    db_type <- c("MainDataTableInfo", "MainDataTable_raw", "MainDataTable_final", 
                 "MainDataTable", "ExpectedCatch", "altmatrix", "PortTable", "port", 
                 "ldglobalcheck", "FleetTable", "modelOut", "modelfit", "modelinputdata", 
                 "modelDesignTable", "FilterTable", "GridTable", "AuxTable", "SpatTable")
    
    t_regex <- paste0(db_type, collapse = "|")
    t_str <- stringi::stri_extract_first_regex(tab, t_regex)
    t_str[is.na(t_str)] <- "other"
    
    out <- switch(t_str, 
                  "MainDataTable" = "main", "MainDataTable_final" = "final table", 
                  "MainDataTable_raw" = "raw table", "ExpectedCatch" = "expected catch matrix", 
                  "altmatrix" = "alt choice matrix", "PortTable" = "port", 
                  "port" = "port", "MainDataTableInfo" = "info table",
                  "FilterTable" = "filter table", "ldglobalcheck" = "global check", 
                  "FleetTable" = "fleet table", "modelOut" = "model output", 
                  "modelfit" = "model fit", "modelinputdata" = "model data", 
                  "modelDesignTable" = "model design", "other" = "other",
                  "GridTable" = "grid", "AuxTable" = "aux", "SpatTable" = "spatial")
    
    out
  }
}
