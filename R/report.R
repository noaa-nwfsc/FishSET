
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
                                simplifyVector = FALSE)
      
    } else {
      
      warning("Log file does not exist.")
      x <- 1
    }
  }
  
  if (x == 0) {
    
    log
  }
}


pull_table <- function(project, table) {
  #' 
  #' Retrieve name of the most recent table from a project
  #' 
  #' @param project Name of project.
  #' @param table Name of table, e.g. "MainDataTable". 
  #' @export
  #' @keywords internal
  
  tab <- tables_database()
  
  tab <- grep(paste0(project, table), tab, value = TRUE)
  
  if (table == "MainDataTable") {
    
    tab <- tab[!grepl("Info", tab)]
    
  } else if (table == "MainDataTableInfo") {
    
    tab <- grep("Info", tab, value = TRUE)
  }
  
  tab <- gsub("[^0-9\\.]", "", tab)
  
  tab <- tab[tab == max(tab)]
  
  tab <- paste0(project, table, tab)
  
  if (table_exists(tab)) {
    
    tab
    
  } else {
    
    warning(tab, " does not exist.")
  }
}


table_format <- function(x) {
  #' Import and format saved tables to notebook file
  #' @param x Name of table saved in inst/output
  #' @keywords internal
  #' @export
  #' @importFrom pander panderOptions pander 
  
  tab_int <- read.csv(paste0(getwd(),'/inst/output/', x, '.csv'))
  pander::panderOptions('table.alignment.default', function(df)
    ifelse(sapply(df, is.numeric), 'right', 'left'))
  pander::panderOptions('table.emphasize.rownames',TRUE)
  pander::panderOptions('table.split.table', Inf)
  pander::panderOptions('graph.fontsize',8)
  pander::panderOptions('table.style', 'multiline')
  if(grepl('summary', x)){
    colnames(tab_int)[1] <- 'Variable'  
    pander::pander(tab_int)
  } else {
    pander::pander(tab_int)
  }
}


plot_format <- function(x){
  #' Import and format plots to notebook file
  #' @param x Name of plot saved in inst/output
  #' @keywords internal
  #' @export
  #' @importFrom knitr include_graphics
  #' 
  knitr::include_graphics(paste0(getwd(), '/inst/output/',x,'.png'))
}


summary_table <- function(project) {
  #' 
  #' Display dataset summary table
  #'
  #' @param project Name of project.
  #' @export
  #' @keywords internal
  #' @importFrom tibble rownames_to_column 
  #' @details Displays the most recent table created by \code{\link{summary_stats}} 
  #' as a dataframe. Can be used in console or notebook. 
  
  date <- gsub(".json", "", current_log())
  
  sum_tab <- read.csv(paste0(locoutput(), project, "_summary_stats_", date, ".csv"), 
                      strip.white = TRUE, check.names = FALSE)
  
  rownames(sum_tab) <- c("Min", "Median", "Mean", "Max", "Missing", 
                         "Unique Obs.", "No. 0's")
  
  sum_tab <- apply(sum_tab, 2, function(x) gsub(".*:", "", x))
  
  sum_tab <- apply(sum_tab, 2, function(x) trimws(x))
  
  sum_tab <- as.data.frame(t(sum_tab))
  
  sum_tab <- tibble::rownames_to_column(sum_tab, "Variable")
  
  sum_tab <- sum_tab[-1, ]
  
  sum_tab
}


dat_create_summary <- function(date = NULL, show = "all"){
  #' 
  #' Display summary of data creation function calls
  #'
  #' @param date Character string; the date of the log file ("%Y-%m-%d" format) to 
  #'   retrieve. If \code{NULL} the most recent log is pulled. 
  #' @param show Whether to display \code{"all"} calls, the \code{"last"} (most recent) call, or 
  #' the \code{"first"} (oldest) function call from the log file.  
  #' @importFrom dplyr bind_rows
  #' @details Displays a list of data creation functions and their arguments ran by date. 
  #'   If no date is entered the most recent log file is pulled. 
  #' @export
  #' @keywords internal
  #' @seealso \code{\link{filter_summary}}
  #' @examples 
  #' \dontrun{
  #' dat_create_summary()
  #' }
  
  fun_vector <- c("temp_mod", "ID_var", "create_seasonal_ID", "cpue", "dummy_var",
                  "dummy_num", "dummy_matrix", "set_quants", "bin_var", "create_var_num",
                  "create_mid_haul", "create_trip_centroid", "create_dist_between", 
                  "create_duration")
  
  log <- pull_log(log_date = date)
  log_date <- log[[1]][[1]]$info[[1]]$rundate
  
  # grab all function calls
  fun_calls <- lapply(seq_along(log$fishset_run[[2]]$function_calls), 
                      function(x) log$fishset_run[[2]]$function_calls[[x]]$functionID)
  
  c_vars <- unique(unlist(fun_calls[unlist(lapply(fun_vector, function(x) grep(x, fun_calls)))]))
  
  ind <- lapply(c_vars, function(x) grep(x, fun_calls))
  
  if (length(ind) == 0) {
    
    cat("No data creation function found in log.")
    
  }  else if (length(ind) > 0) {
    
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
    args <- lapply(c_vars, function(x) names(formals(x)))
    
    arg_len <- lapply(args, length)
    
    names(arg_len) <- c_vars 
    
    arg_match <- lapply(names(arg_len), function(x) {
      
      lapply(seq_along(fun_list[[x]]), function(i) {
        
        arg_len[x] == length(fun_list[[x]][[i]]$args) 
      })
    })
    
    if (all(unlist(arg_match)) == TRUE) { # update 
      
      for (i in names(fun_list)) {
        
        for (k in seq_along(fun_list[[i]])) {
          
          names(fun_list[[i]][[k]]$args) <- names(formals(i))
        }
      }  
      
    } else {
      
      match <- suppressWarnings(which(unlist(lapply(arg_match, any))))
      no_match <- suppressWarnings(which(!unlist(lapply(arg_match, any))))
      
      if (length(match) > 0) {
        
        for (i in match) {
          
          for (k in seq_along(fun_list[[i]])) {
            
            names(fun_list[[i]][[k]]$args) <- names(formals(c_vars[i]))
          } 
        }
      }
      
      for (i in no_match) {
        
        for (k in seq_along(fun_list[[i]])) {
          
          names(fun_list[[i]][[k]]$args) <- sapply(seq_along(fun_list[[i]][[k]]$args), function(x) {
            
            paste("arg", x, sep = "_")
          })
        }
      }
    }
    
    df_list <- list()
    
    for (i in names(fun_list)) {
      
      for (k in seq_along(fun_list[[i]])) {
        
        df_list[[i]][[k]] <- as.data.frame(fun_list[[i]][[k]]$args) 
        
        df_list[[i]][[k]]$function_name <- i
      }
    }
    
    df_list2 <- list()
    
    for (i in seq_along(df_list)) {
      
      df_list2[[i]] <- suppressWarnings(dplyr::bind_rows(df_list[[i]]))
      
      n <- colnames(df_list2[[i]])[colnames(df_list2[[i]]) != "function_name"]
      
      df_list2[[i]] <- df_list2[[i]][ , c("function_name", n)]
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
  }
  
  df_list2
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
  #' @seealso \code{\link{dat_create_summary}}
  #' @examples
  #' \dontrun{
  #' filter_summary(dat_create_summary(), 
  #'               filter_list = list(set_quants = 2, temporal_mod = 2))
  #' }
 
  
  for (i in names(filter_list)) {
    
    sum_tab[[i]] <- sum_tab[[i]][filter_list[[i]], ]
  }
  
  sum_tab
}


model_out_summary <- function(project) {
  #' 
  #' Retrieve most recent summary of model output  
  #' 
  #' @param project Name of project
  #' @export
  #' @keywords internal
  #' @examples 
  #' \dontrun{
  #' model_out_summary("pollock")
  #' } 
  
  p_mod <- pull_table(project, "modelOut")
  
  results <- model_out_view(p_mod)
  
  modeltab <- data.frame(Model_name = rep(NA, length(results)), 
                         covergence = rep(NA, length(results)), 
                         Stand_Errors = rep(NA, length(results)), 
                         Hessian = rep(NA, length(results)))
  
  for (i in seq_along(results)) {
    
    modeltab[i, 1] <- results[[i]]$name
    modeltab[i, 2] <- results[[i]]$optoutput$convergence
    modeltab[i, 3] <- toString(round(results[[i]]$seoutmat2, 3))
    modeltab[i, 4] <- toString(round(results[[i]]$H1, 5))
  }
  
  modeltab
}


model_error_summary <- function(project) {
  #' 
  #' Retrieve most recent summary of model error  
  #' 
  #' @param project Name of project
  #' @export
  #' @keywords internal
  #' @examples 
  #' \dontrun{
  #' model_error_summary("pollock")
  #' }
  
  p_mod <- pull_table(project, "modelOut")
  
  results <- model_out_view(p_mod)
  
  error_out <- data.frame(Model_name = rep(NA, length(results)), 
                          Model_error = rep(NA, length(results)), 
                          Optimization_error = rep(NA, length(results)))
  
  for (i in seq_along(results)) {
    
    error_out[i, 1] <- results[[i]]$name
    error_out[i, 2] <- ifelse(is.null(results[[i]]$errorExplain), 'No error reported', 
                              toString(results[[i]]$errorExplain))
    error_out[i, 3] <- ifelse(is.null(results[[i]]$optoutput$optim_message), 'No message reported', 
                              toString(results[[i]]$optoutput$optim_message))
  }
  
  error_out
}


model_fit_summary <- function(project) {
  #' 
  #' Retrieve most recent summary of model fit  
  #' 
  #' @param project Name of project
  #' @export
  #' @keywords internal
  #' @examples 
  #' \dontrun{
  #' model_fit_summary("pollock")
  #' }
  
  p_mod <- pull_table(project, "modelOut")
  
  results <- model_out_view(p_mod)
  
  fit_tab <- data.frame(Model_name = rep(NA, length(results)), 
                        AIC = rep(NA, length(results)), AICc = rep(NA, length(results)),
                        BIC = rep(NA, length(results)), PseudoR2 = rep(NA, length(results)))
  
  for (i in seq_along(results)) {
    
    fit_tab[i, 1] <- results[[i]]$name
    fit_tab[i, 2] <- results[[i]]$MCM$AIC
    fit_tab[i, 3] <- results[[i]]$MCM$AICc
    fit_tab[i, 4] <- results[[i]]$MCM$BIC
    fit_tab[i, 5] <- results[[i]]$MCM$PseudoR2
  }
  
  fit_tab
}