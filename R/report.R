
user_locoutput <- function() {
  #'
  #'Print user folder directory
  #'
  #'@export
  #'@keywords internal
  #'@details This function looks for an object named "locuser" with a valid folder directory. 
  #'  if it doesn't find one, it asks user to set a valid directory. This is used for
  #'  inserting plots and tables from a folder outside the FishSET package. 
  #'  
  
  if (exists("locuser")) {
    
    if (!dir.exists(locuser)) {
      
      warning("Invalid directory.")
      
    } else {
      
      locuser
    }
    
  } else {
    
    cat("User directory unspecified. Please set object 'locuser' to desired folder directory.")
  }
}

current_log <- function() {
  #'
  #' Lists most recent log file
  #'
  #' @keywords internal
  #' @export
  #' @details Prints the name of the most recent log file, but not the filepath.

  logs <- list.files(loclog())

  g <- gsub("[^0-9]", "", logs)

  log <- logs[which(g == max(g))]

  log
}


pull_log <- function(log_date = NULL) {
  #'
  #'  pull log file
  #'
  #' @param log_date Date of log to be pulled. If \code{NULL}, then most recent log file
  #'   is retrieved.
  #' @importFrom jsonlite fromJSON
  #' @keywords internal
  #' @export

  x <- 0

  if (is.null(log_date)) {
    log <- jsonlite::fromJSON(paste0(loclog(), current_log()), simplifyVector = FALSE)
  } else {
    if (file.exists(paste0(loclog(), log_date, ".json"))) {
      log <- jsonlite::fromJSON(paste0(loclog(), log_date, ".json"),
        simplifyVector = FALSE
      )
    } else {
      warning("Log file does not exist.")
      x <- 1
    }
  }

  if (x == 0) {
    log
  }
}


current_out <- function() {
  #'
  #' Lists most recent output files
  #'
  #' @keywords internal
  #' @export
  #' @importFrom stringr str_extract_all
  #' @details Prints the name of the most recent output files.
  #' @examples
  #' \dontrun{
  #' current_out()
  #' }

  outs <- list.files(locoutput())

  c <- stringr::str_extract_all(outs, "\\d{4}-\\d{2}-\\d{2}", simplify = TRUE)

  c <- gsub("[^0-9]", "", c)

  outs <- outs[which(c == max(c))]

  outs
}

pull_output <- function(project, fun = NULL, date = NULL, type = "plot") {
  #'
  #' Retrieve output file name by project, function, and type.
  #'
  #' @param project Name of project
  #' @param fun Name of function.
  #' @param date Output file date in "%Y-%m-%d" format to retrieve. If \code{NULL}
  #'   the most recent output file is pulled.
  #' @param type Whether to return the \code{"plot"} (.png), \code{"table"} (.csv),
  #'  "notes" (.txt) or \code{"all"} files matching the project name, function, and date.
  #' @export
  #' @examples
  #' \dontrun{
  #' pull_output("pollock", "species_catch", type = "plot")
  #' }
  end <- FALSE
  outs <- list.files(locoutput())
  ext <- switch(type, "plot" = ".*\\.png$", "table" = ".*\\.csv$", "notes" = ".*\\.txt$")
  
  out <- grep(paste0("^", project, "_", ext), outs, value = TRUE)
  
  if (length(out) == 0) {
    cat("No ", type, " found for project '", project,"'.", sep = "")
    end <- TRUE
  }
  
  if (!end) {
    
    if (!is.null(fun)) {
      out <- grep(paste0("_", fun, "_"), out, value = TRUE)
      
      if (length(out) == 0) {
        cat("No", type, "output for function", fun, "exists for project", paste0("'", project, "'. "))
        end <- TRUE
      }
    } else {
      out <- grep("_notes_", out, value = TRUE)
    }
    
    if (!end) {
      
      if (is.null(date)) {
        dates <- stringr::str_extract_all(out, "\\d{4}-\\d{2}-\\d{2}", simplify = TRUE)
        dates <- gsub("[^0-9]", "", dates)
        out <- out[which(dates == max(dates))]
       
      } else {
        
        out <- grep(date, out, value = TRUE)
        
        if (length(out) == 0) {
          cat("No", type, "output from", date, "exists for project", paste0("'", project, "'. "))
          end <- TRUE
        }
      }
      if (!end) {
        out
      }
    }
  }
}

pull_table <- function(project, fun, date = NULL) {
  #' 
  #' Import and format table to notebook file
  #' 
  #' @param project Project name.
  #' @param fun String, the name of the function that created the table. 
  #' @param date the date the table was created. If NULL, then the most recent version
  #'   is retrieved. 
  #' @export
  #' @examples
  #' \dontrun{
  #' pull_table("pollock", "vessel_count")
  #' }
  #' 
  out <- pull_output(project = project, fun = fun, date = date, type = "table")
  
  # formatting here
  
  table_format(out)
}


pull_plot <- function(project, fun, date = NULL) {
  #' 
  #' Import and format plots to notebook file
  #' 
  #' @param project Project name.
  #' @param fun String, the name of the function that created the plot. 
  #' @param date the date the plot was created. If NULL, then the most recent version
  #'   is retrieved. 
  #' @export
  #' @importFrom knitr include_graphics
  #' @examples
  #' \dontrun{
  #' pull_plot("pollock", "density_plot")
  #' }
  
  out <- pull_output(project = project, fun = fun, date = date, type = "plot")
  
  if (!is.null(out)) {
    
    knitr::include_graphics(paste0(locoutput(), out))
    
  } else {
    
    cat("Plot not found.")
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
  
  tab <- tables_database()
  
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
    tab_out <- proj_tab[which(new_tab == max(new_tab))]
    
    if (!table_exists(tab_out)) {
      
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


table_format <- function(x) {
  #' 
  #' Import and format saved tables to notebook file
  #' 
  #' @param x Name of table saved in inst/output
  #' @keywords internal
  #' @export
  #' @importFrom pander panderOptions pander
  #' @seealso \code{\link{pull_output}}
  #' @examples
  #' \dontrun{
  #' table_format("pollock_species_catch_2020-05-29.csv")
  #' table_format(pull_output("pollock", "species_catch", type = "table"))
  #' }

  tab_int <- read.csv(paste0(locoutput(), x))
  pander::panderOptions("table.alignment.default", function(df) {
    ifelse(sapply(df, is.numeric), "right", "left")
  })
  pander::panderOptions("table.emphasize.rownames", TRUE)
  pander::panderOptions("table.split.table", Inf)
  pander::panderOptions("graph.fontsize", 8)
  pander::panderOptions("table.style", "multiline")
  if (grepl("summary", x)) {
    colnames(tab_int)[1] <- "Variable"
    pander::pander(tab_int)
  } else {
    pander::pander(tab_int)
  }
}


plot_format <- function(x) {
  #' 
  #' Import and format plots to notebook file
  #' 
  #' @param x Name of plot saved in inst/output
  #' @keywords internal
  #' @export
  #' @importFrom knitr include_graphics
  #' @examples
  #' \dontrun{
  #' plot_format("pollock_species_catch_2020-05-29.png")
  #' plot_format(pull_output("pollock", "species_catch", type = "plot"))
  #' }
  knitr::include_graphics(paste0(locoutput(), x))
}

insert_plot <- function(out) {
  #' 
  #' Insert plot from user folder
  #' 
  #' @param out String, plot file name.
  #' @export
  #' @importFrom knitr include_graphics
  #' @examples
  #' \dontrun{
  #' insert_plot("pollock_plot.png")
  #' }
  
  if (!is.null(user_locoutput())){
    
    if (file.exists(paste0(user_locoutput(), out))) {
      
      knitr::include_graphics(paste0(user_locoutput(), out))
      
    } else {
      
      cat("Plot not found.")
    }
  }
}

insert_table <- function(out) {
  #' 
  #' Insert table from user folder
  #' 
  #' @param out String, table file name.
  #' @export
  #' @examples
  #' \dontrun{
  #' insert_table("pollock_table.csv")
  #' }
  if (!is.null(user_locoutput())) {
    
    if (!file.exists(paste0(user_locoutput(), out))) {
      
      cat("Table not found.")
      
    } else {
      
      tab <- read.csv(paste0(user_locoutput(), out))
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
  #'   character vector of the notes. "print" is reccomended for displaying notes in a report.  
  #' @export
  #' @importFrom stringi stri_omit_empty
  #' @importFrom stringr str_extract
  #' @details Notes are saved to the output folder by project name and date. If date is not
  #'   specified then the most recent notes file with the project name is pulled. Notes are
  #'   are also saved by FishSET app session; if more than one session occured in the same day, each 
  #'   session's notes are pulled and listed in chronological order. 
  
  out <- pull_output(project = project, date = date, type = "notes")
  
  note_names <- c("Data quality evaluation: ", "Simple analysis: ", "Data exploration: ", "Upload data: ", "Fleet functions: ",
                  "Create new variable: ", "Zone definition: ", "Expected catch/revenue: ", "Models: ", "Bookmark URL: ")
  
  if (length(out) == 1) {
    
    notes <- readLines(paste0(locoutput(), out))
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
    n_id <- stringr::str_extract(out, "\\d{4}-\\d{2}-\\d{2}.*")
    n_id <- gsub("[^0-9]", "", n_id)
    n_id <- order(n_id)
    
    notes <- lapply(out, function(n) readLines(paste0(locoutput(), n)))
    notes <- lapply(n_id, function(x) notes[[x]])
    notes <- lapply(notes, stringi::stri_omit_empty)
    
    for (n in seq_along(notes)) {
      
      ind <- which(notes[[n]] %in% note_names)
      for (i in ind) {
        notes[[n]][[i]] <- paste0("\n(", n, ") ", notes[[n]][[i]])
      }
    }
    
    if (output == "string") {
      paste(unlist(notes), collapse = "\n")
      
    } else if (output == "print") {
      
      cat(unlist(notes), sep = "\n")
    }
  }
}

parse_notes <- function(project, date = NULL, section, output = "print") {
  #'
  #' Selectively display a note section
  #' 
  #' @param project The project name.
  #' @param date Date to pull notes from. If NULL then the most recent version of notes
  #'   from the project are retreived. 
  #' @param section The note section to display. Options include "upload" for Upload data,
  #'   "quality" for Data quality evaluation, "explore" for Data exploration, "fleet" for
  #'   Fleet functions, "analysis" for Simple analysis, "new_variable" for Create new variable,
  #'   "zone" for Zone definition, "models", and "bookmark". 
  #' @param output Output type. "print" returns formatted notes. "string" returns a 
  #'   character vector of the notes. "print" is reccomended for displaying notes in a report.
  #' @export
  #' @examples 
  #' \dontrun{
  #' parse_notes("pollock", type = "explore")
  #' }
  
  notes <- pull_notes(project = project, date = date, output = "string")
  
  split <- unlist(strsplit(notes, "\n\n"))
  
  note_type <-  switch(section, "upload" = "Upload data: ", "quality" = "Data quality evaluation: ", 
                       "explore" = "Data exploration: ", "fleet" = "Fleet functions: ", 
                       "analysis" =  "Simple analysis: ","new_variable" = "Create new variable: ", 
                       "zone" =  "Zone definition: ", "expected_catch" = "Expected catch/revenue: ", 
                       "models" = "Models: ", "bookmark" = "Bookmark URL: ")
  
  notes <- grep(note_type, split, value = TRUE)
  
  if (output == "string") {
    
    notes
    
  } else if (output == "print") {
    
    cat(notes, sep = "\n")
  }
}

summary_table <- function(project, output = "print") {
  #'
  #' Display dataset summary table
  #'
  #' @param project Name of project.
  #' @param output Output type. "print" returns formatted notes. "table" returns a 
  #'   dataframe. "print" is reccomended for displaying summary table in a report.
  #' @export
  #' @keywords internal
  #' @importFrom tibble rownames_to_column
  #' @importFrom pander pander pandoc.table
  #' @details Displays the most recent table created by \code{\link{summary_stats}}
  #' as a dataframe. Can be used in console or notebook.

  date <- gsub(".json", "", current_log())

  sum_tab <- read.csv(paste0(locoutput(), project, "_summary_stats_", date, ".csv"),
    strip.white = TRUE, check.names = FALSE
  )

  rownames(sum_tab) <- c(
    "Min", "Median", "Mean", "Max", "Missing",
    "Unique Obs.", "No. 0's"
  )

  sum_tab <- apply(sum_tab, 2, function(x) gsub(".*:", "", x))

  sum_tab <- apply(sum_tab, 2, function(x) trimws(x))

  sum_tab <- as.data.frame(t(sum_tab))

  sum_tab <- tibble::rownames_to_column(sum_tab, "Variable")

  sum_tab <- sum_tab[-1, ]

  if (output == "print") {
    
    pander::pander(
      pander::pandoc.table(sum_tab, style = "simple", row.names = FALSE, split.tables = Inf)
    )
  } else if (output == "table") {
    
    sum_tab
  }
}


function_summary <- function(date = NULL, type = "dat_load", show = "all") {
  #'
  #' Display summary of function calls
  #'
  #' @param date Character string; the date of the log file ("%Y-%m-%d" format) to
  #'   retrieve. If \code{NULL} the most recent log is pulled.
  #' @param type The type of function to display. "dat_load", "dat_quality", "dat_create",
  #'   "dat_exploration", "fleet", "zonal_def", and "model".
  #' @param show Whether to display \code{"all"} calls, the \code{"last"} (most recent) call, or
  #' the \code{"first"} (oldest) function call from the log file.
  #' @importFrom dplyr bind_rows
  #' @details Displays a list of functions by type and their arguments from a log file.
  #'   If no date is entered the most recent log file is pulled.
  #' @export
  #' @keywords internal
  #' @seealso \code{\link{filter_summary}}
  #' @examples
  #' \dontrun{
  #' function_summary()
  #' }

  dat_load <- c(
    "load_data", "load_maindata", "main_mod", "load_port", "load_aux",
    "load_gridded", "merge_dat", "split_dat"
  )

  dat_quality <- c(
    "data_verification", "data_check", "nan_identify", "nan_filter", "na_filter",
    "outlier_table", "outlier_plot", "outlier_remove", "degree", "unique_filter",
    "empty_vars_filter", "check_model_data", "filter_table", "filter_dat",
    "add_vars", "changeclass"
  )

  dat_create <- c(
    "temp_mod", "ID_var", "create_seasonal_ID", "cpue", "dummy_var",
    "dummy_num", "dummy_matrix", "set_quants", "bin_var", "create_var_num",
    "create_mid_haul", "create_trip_centroid", "create_dist_between",
    "create_duration", "create_startingloc", "haul_to_trip", "create_TD",
    "randomize_value_row", "randomize_value_range", "jitter_lonlat", 
    "randomize_lonlat_zone", "lonlat_to_centroid"
  )

  dat_exploration <- c(
    "map_plot", "map_kernel", "getis_ord_stats", "moran_stats", "temp_plot", 
    "xy_plot", "corr_out"
  )

  fleet <- c(
    "fleet_table", "density_plot", "fleet_assign", "vessel_count", "species_catch", "bycatch",
    "sum_catch", "weekly_catch", "weekly_effort", "trip_length", "roll_catch"
  )

  zonal_def <- c("create_alternative_choice", "find_centroid", "assignment_column")

  model <- c(
    "sparsetable", "sparsplot", "create_expectations", "make_model_design",
    "discretefish_subroutine", "check_model_data", "log_func_model"
  )

  fun_vector <- switch(type, "dat_load" = dat_load, "dat_quality" = dat_quality,
    "dat_create" = dat_create, "dat_exploration" = dat_exploration,
    "fleet" = fleet, "zonal_def" = zonal_def, "model" = model
  )


  log <- pull_log(log_date = date)
  log_date <- log[[1]][[1]]$info[[1]]$rundate


  # grab all function calls
  fun_calls <- lapply(
    seq_along(log$fishset_run[[2]]$function_calls),
    function(x) log$fishset_run[[2]]$function_calls[[x]]$functionID
  )

  c_vars <- unique(unlist(fun_calls[unlist(lapply(fun_vector, function(x) grep(x, fun_calls)))]))

  ind <- lapply(c_vars, function(x) grep(x, fun_calls))

  if (length(ind) == 0) {
    cat("No functions of type", type, "found in log.")
  } else if (length(ind) > 0) {
    fun_list <- lapply(ind, function(x) {
      lapply(x, function(i) {
        log$fishset_run[[2]]$function_calls[[i]]
      })
    })

    if (any(c_vars == "temp_mod")) {
      c_vars[c_vars == "temp_mod"] <- "temporal_mod"
    }

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


model_out_summary <- function(project, output = "print") {
  #'
  #' Retrieve most recent summary of model output
  #'
  #' @param project Name of project
  #' @param output Output type. "print" returns formatted notes. "table" returns a 
  #'   dataframe. "print" is reccomended for displaying summary table in a report.
  #' @export
  #' @importFrom pander pander pandoc.table
  #' @keywords internal
  #' @examples
  #' \dontrun{
  #' model_out_summary("pollock")
  #' }

  pull_out <- current_db_table(project, "modelOut")
  
  if (pull_out$exists == TRUE) {
    
    p_mod <- pull_out$table
    
    results <- model_out_view(p_mod)
    
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
      
    } else if (output == "table") {
      
      modeltab
    }
  }
  paste0(project, "modelOut", "not found.")
}


model_error_summary <- function(project, output = "print") {
  #'
  #' Retrieve most recent summary of model error
  #'
  #' @param project Name of project
  #' @param output Output type. "print" returns formatted notes. "table" returns a 
  #'   dataframe. "print" is reccomended for displaying summary table in a report.
  #' @export
  #' @importFrom pander pander pandoc.table
  #' @keywords internal
  #' @examples
  #' \dontrun{
  #' model_error_summary("pollock")
  #' }

  pull_out <- current_db_table(project, "modelOut")

  if (pull_out$exists == TRUE) {
    
    p_mod <- pull_out$table 
    results <- model_out_view(p_mod)
  
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
      
    } else if (output == "table") {
      
      error_out
    }
  }
  paste0(project, "modelOut", " not found.")
}


model_fit_summary <- function(project, output = "print") {
  #'
  #' Retrieve most recent summary of model fit
  #'
  #' @param project Name of project
  #' @param output Output type. "print" returns formatted notes. "table" returns a 
  #'   dataframe. "print" is reccomended for displaying summary table in a report.
  #' @export
  #' @importFrom pander pander pandoc.table
  #' @keywords internal
  #' @examples
  #' \dontrun{
  #' model_fit_summary("pollock")
  #' }

  pull_out <- current_db_table(project, " modelOut")
  
  if (pull_out$exists == TRUE) {
    
    p_mod <- pull_out$table 
    results <- model_out_view(p_mod)
  
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
      
    } else if (output == "table") {
      
      fit_tab
    }
  }
  paste0(project, "modelOut", " not found.")
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
        table_view(fleet_tab$table),
        style = "simple", row.names = FALSE, split.table = Inf)
    )
    
  } else {
    cat("No fleet table found.")
  }
}