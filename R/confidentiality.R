check_confid_par <- function(rule, value) {
  #' Check confidentiality parameters
  #' 
  #' @param rule String, \code{"n"} for rule of n, \code{"k"} for n/k. 
  #' @param value Numeric, for \code{rule = "n"} must be an integer of at least 
  #'   2. For \code{rule = "k"} any numeric value from 0 to 100. 
  #' @keywords internal
  #' @return \code{TRUE} if confidentiality parameters are valid, \code{FALSE} 
  #'   if not. 
  
  if (rule == "n") {
    
    if (!is.integer(value)) {
      
      warning("Value must be an integer for rule = n")
      FALSE
      
    } else if (value < 2) {
      
      warning("Value cannot be below 2 vessels")
      FALSE
      
    } else TRUE
    
  } else if (rule == "k") {
    
    if (!is.numeric(value)) {
      
      warning("Value must be numeric")
      FALSE
      
    } else if (value > 100 | value < 0) {
      
      warning("Value must be between 0 and 100")
      FALSE
      
    } else TRUE
    
  } else {
    
    warning("Rule type not recognized")
    FALSE
  }
}


set_confid_check <- function(project, check = TRUE, v_id = NULL, rule = "n", 
                             value = NULL) {
  #' Set confidentiality parameters
  #' 
  #' This function specifics whether to check for confidentiality and which 
  #' rule should be applied.
  #' 
  #' @param project Name of project. 
  #' @param check Logical, whether to check for confidentiality.
  #' @param v_id String, the column name containing the vessel identifier. 
  #' @param rule String, the confidentiality rule to apply. See "Details" below. 
  #'   \code{rule = "n"} suppresses values containing fewer than n vessels. 
  #'   \code{rule = "k"} suppresses values where a single vessel contains k percent 
  #'   or more of the total catch. 
  #' @param value The threshold for confidentiality. for \code{rule = "n"} must 
  #'   be an integer of at least 2. For \code{rule = "k"} any numeric value from 
  #'   0 to 100. 
  #' @export
  #' @details \code{rule = "n"} counts the number of vessel in each strata 
  #'   and suppresses values where fewer than n vessels are present. For
  #'   \code{rule = "k"}, or the "Majority allocation rule", each vessel's share of 
  #'   catch is calculated by strata. If any vessel's total catch share is greater 
  #'   than or equal to k percent the value is suppressed.
  #' @examples 
  #' \dontrun{
  #' set_confid_check("pollock", check = TRUE, v_id = "PERMIT", rule = "n", value = 3L)
  #' }
  
  if (check) {
    
    if (!is.null(v_id) & !is.null(value)) {
      
      if (rule == "n") value <- as.integer(value)
      
      pass_checks <- check_confid_par(rule, value)
      
    } else {
      
      warning("Arguments v_id and/or value are missing")
      pass_checks <- FALSE
      invisible(FALSE)
    } 
    
  } else pass_checks <- TRUE
  
  if (pass_checks) {
    
    confid_list <- list(check = check, v_id = v_id, rule = rule, value = value)
    
    edit_proj_settings(project, confid = confid_list)
    
    message("Confidentiality settings saved.")
    invisible(TRUE)
    
  } else invisible(FALSE)
}


run_confid_check <- function(project) {
  #' Check whether confidentiality rules should be applied
  #' @param project Name of project. 
  #' @keywords internal
  #' @return \code{TRUE} if confidentiality settings exists and \code{check = TRUE}, 
  #'   \code{FALSE} if settings do not exists yet or \code{check = FALSE}.
  
  if (proj_settings_exists(project)) {
    
    out <- get_confid_check(project)
    
    if (!is.null(out)) out$check
    else FALSE
    
  } else FALSE
}


get_confid_check <- function(project) {
  #' Return the confidentiality settings 
  #' 
  #' This function returns the confidentiality settings from project settings
  #' file.
  #' @param project Name of project
  #' @return A list containing the confidentiality parameters: \code{check}, 
  #'   \code{v_id}, \code{rule}, and \code{value}.
  #' @seealso \code{\link{set_confid_check}} \code{\link{get_proj_settings}}
  #' @export
  
  proj_set <- get_proj_settings(project)
  
  if (!is.null(proj_set)) proj_set$confid
  else return(NULL)
}


confid_cache_exists <- function(project) {
  #' Confidentialy cache exists
  #' 
  #' Returns \code{TRUE} if confidentiality cache file is found in the project
  #' output folder.
  #' @param project Name of project.
  #' @export
  #' @examples 
  #' \dontrun{
  #' confid_cache_exists("pollock")
  #' }
  
  cache_file <- paste0(locoutput(project), "confid_cache.json")
  file.exists(cache_file)
}


get_confid_cache <- function(project, show = "all") {
  
  #' Return cached confidentiality tables
  #' 
  #' This function lists the confidentiality "check" tables used to suppress values.
  #' @param project Name of project
  #' @param show Output \code{"all"} tables, \code{"last"} table, or \code{"first"}
  #'   table. 
  #' @importFrom jsonlite read_json
  #' @return A list of tables containing suppression conditions. 
  #' @export
  #' @seealso \code{\link{reset_confid_cache}}
  
  if (project_exists(project) == FALSE) {
    
    warning(paste("Project", project, "does not exist."))
    
  } else {
    
    cache_file <- paste0(locoutput(project), "confid_cache.json")
    
    if (file.exists(cache_file)) {
      
      cache <- jsonlite::read_json(cache_file, simplifyVector = TRUE)
      
      if (show == "first") cache <- cache[[1]]
      else if (show == "last") cache <- cache[[length(cache)]]
      
      cache
      
    } else return(NULL)
  }
}


cache_check_table <- function(check, project) {
  
  #' Add check table to confidentiality cache list
  #' 
  #' @param check Dataframe, check table to be added to confidentiality cache.
  #' @param project Name of project. 
  #' @importFrom jsonlite write_json
  #' @keywords internal
  
  cache <- get_confid_cache(project)
  
  if (is.null(cache)) cache <- vector("list", length = 0)
  
  ind <- length(cache) + 1
  
  cache[[ind]] <- check
  
  cache_file <- paste0(locoutput(project), "confid_cache.json")
  jsonlite::write_json(cache, cache_file)
}


reset_confid_cache <- function(project) {
  #' Reset confidentiality cache tables
  #' 
  #' This function deletes all confidentiality check tables stored in 
  #' the \code{"confid_cache.json"} file located in the project output folder. 
  #' Resetting this cache is recommended after a long period of use as check 
  #' tables can accumulate over time.
  #' @param project Project name
  #' @export
  #' @seealso \code{\link{get_confid_cache}}
  
  if (confid_cache_exists(project)) {
    
    file.remove(paste0(locoutput(project), "confid_cache.json"))
  }
}


check_confidentiality <- function(dataset, project, v_id, value_var, group = NULL, 
                                  rule = c("n", "k"), value, names_to = "name", 
                                  values_to = "value") {
  
  #' Create confidentiality check table
  #' 
  #' This function checks for confidential values in a summary table and creates 
  #' a table of suppression conditions, or "check table".
  #' 
  #' @param dataset The dataset used to create a summary table. This must include 
  #'   the vessel identifier column. 
  #' @param project Name of project. 
  #' @param v_id String, the name of the vessel identifier column.
  #' @param value_var String, the name(s) of the value variable(s). 
  #' @param group String, the name(s) of the grouping variable(s). This should 
  #'   include the `period` name if summarizing over time. 
  #' @param rule String, the confidentiality rule to apply. \code{rule = "n"} 
  #'   suppresses values containing fewer than n vessels. \code{rule = "k"} (the 
  #'   "majority allocation rule") suppresses values where a single vessel contains 
  #'   k percent or more of the total catch. 
  #' @param value The threshold for confidentiality. for \code{rule = "n"} must 
  #'   be an integer of at least 3. For \code{rule = "k"} any double value from
  #'   0 to 100. 
  #' @param names_to String, the name for the column containing the names of value 
  #'   variables when \code{value_var} has two or more columns.
  #' @param values_to String, the name for the column containing the values from the
  #'   variables listed in \code{names_to}.
  #' @keywords internal
  #' @importFrom tidyr pivot_longer
  #' @importFrom dplyr distinct
  #' @importFrom tibble as_tibble
  #'
  
  # stack value vars and filter for non-zero values
  if (length(value_var) > 1) {
    
    dataset <- tidyr::pivot_longer(dataset, cols = !!value_var, 
                                   names_to = names_to, values_to = values_to)
    value_var <- values_to
    group <- c(group, names_to)
    include_val <- FALSE
    
  } else include_val <- TRUE
  
  dataset <- dataset[dataset[[value_var]] > 0, ]
  
  if (rule == "n") {
    
    if (value < 2) value <- 2 
    
    check <- agg_helper(dataset, v_id, group, fun = function(x) length(unique(x)))
    
    # filter for obs below threshold
    ind <- check[[v_id]] < value
    
  } else if (rule == "k") {
    
    check <- agg_helper(dataset, value_var, group = c(v_id, group), fun = sum)
    
    check <- perc_of_total(check, value_var = value_var, group = group)
    
    val_perc <- paste0(value_var, "_perc")
    
    ind <- check[[val_perc]] >= value
  }
  
  supr_vals <- sum(ind) > 0
  
  if (supr_vals) {
    
    if (is.null(group)) group <- v_id
    
    if (include_val) {
      
      check$value_var <- value_var
      group <- c(group, "value_var")
    }
    
    check <- tibble::as_tibble(check)
    
    check <- check[ind, group]
    
    check <- dplyr::distinct(check)
    
    cache_check_table(check, project)
    warning("Confidential data detected.")
  }
  
  list(table = if (supr_vals) check else NULL,
       suppress = supr_vals) 
}


suppress_table <- function(check, output, value_var, group, rule, type = "code",
                           as_vector = FALSE) {
  
  #' Suppress confidential values in summary table
  #' 
  #' This function suppresses values in a summary table based on suppression 
  #' conditions found in the check table (see \code{link{check_confidentiality}})
  #' 
  #' @param check The check table containing suppression conditions.
  #' @param output The summary table to be edited based on check table.
  #' @param value_var String, value variable name(s).
  #' @param group String, grouping variable name(s). This includes
  #'   `period` and `facet_by` from summary function. 
  #' @param rule String, the confidentiality rule to apply. \code{rule = "n"} 
  #'   suppresses values containing fewer than n vessels. \code{rule = "k"} (the 
  #'   "majority allocation rule") suppresses values where a single vessel contains 
  #'   k percent or more of the total catch. 
  #' @param type String, the value used to replace confidential data. \code{"code"} 
  #'   replaces values with \code{-999}, \code{"NA"} (with quotes) replaces with 
  #'   \code{NA}, and \code{"zero"} replaces with 0. 
  #' @param as_vector Logical, whether to return the suppressed values as a vector.
  #'   If \code{as_vector == FALSE} the output table is returned. 
  #' @keywords internal
  #' @importFrom rlang parse_exprs
  
  # convert check table to expression list
  #group <- names(output)[names(output) != value_var]
  
  #if (is.null(group)) group <- get_confid_check()$v_id # throws error for roll_catch
  
  len <- length(group) - 1
  
  # check for factors
  check[group] <- lapply(check[group], function(x) {
    
    if ("factor" %in% class(x)) {
      
      if (any(grepl("\\D", levels(x)))) as.character(x)
      else x
      
    } else x
  })
  
  paste_quote <- function(vec) {
    if (any(c("character", "Date") %in% class(vec))) paste0("'", vec, "'") 
    else vec
  }
  
  check_str <- 
    lapply(names(check[group]), function(col) {
      
      vapply(check[[col]], function(i) paste(col, "==", paste_quote(i)), 
             character(1))
    })
  
  if (len > 0) {
    
    check_str[seq(len)] <- lapply(seq(len), function(i) paste(check_str[[i]], "&"))
  }
  
  check_exp <- do.call(paste, check_str)
  
  # convert strings to list of exprs
  check_exp <- rlang::parse_exprs(check_exp)
  
  # eval exprs and create index
  check_ind <- lapply(check_exp, function(x) which(eval(x, envir = output)))
  
  check_ind <- unique(sort(unlist(check_ind)))
  
  supr_code <- switch(type, "NA" = NA, "code" = -999, "zero" = 0)
  
  output[check_ind, value_var] <- supr_code
  
  if (as_vector) output[[value_var]]
  else output
}


check_and_suppress <- function(dat, output, project, v_id, value_var, group = NULL, 
                               rule, value, type = "code", names_to = "name", 
                               values_to = "value") {
  
  #'Check and suppress data
  #'
  #' @param dat The dataset used to create a summary table. This must include 
  #'   the vessel identifier column. 
  #' @param output The output table to be suppressed. If \code{output = NULL}, 
  #'   \code{dat} is used. 
  #'   @param project Name of project. 
  #' @param v_id String, the name of the vessel identifier column.
  #' @param value_var String, the name(s) of the value variable(s). 
  #' @param group String, the name(s) of the grouping variable(s). This should 
  #'   include the `period` name if summarizing over time. 
  #' @param rule String, the confidentiality rule to apply. \code{rule = "n"} 
  #'   suppresses values containing fewer than n vessels. \code{rule = "k"} (the 
  #'   "majority allocation rule") suppresses values where a single vessel contains 
  #'   k percent or more of the total catch. 
  #' @param value The threshold for confidentiality. for \code{rule = "n"} must 
  #'   be an integer of at least 3. For \code{rule = "k"} any double value from
  #'   0 to 100. 
  #' @param type String, the value used to replace confidential data. \code{"code"} 
  #'   replaces values with \code{-999}, \code{"NA"} (with quotes) replaces with 
  #'   \code{NA}, and \code{"zero"} replaces with 0. 
  #' @param names_to String, the name for the column containing the names of value 
  #'   variables when `value_var` has two or more columns.
  #' @param values_to String, the name for the column containing the values from the
  #'   variables listed in `names_to`.
  #' @keywords internal
  
  check <- 
    check_confidentiality(dat, project, v_id, value_var, group, rule, value, 
                          names_to, values_to)
  
  if (check$suppress) {
    
    if (!is.null(output)) {
      
      dat <- suppress_table(check$table, output, value_var = value_var, group, rule, 
                            type = type)
      
    } else {
      
      dat <- suppress_table(check$table, dat, value_var = value_var, group, rule,
                            type = type)
    }
    
    list(table = dat, suppress = TRUE)
  } else check
}


window_cc <- function(x, k) {
  #' Unique values window function
  #' 
  #' Used by \code{roll_catch()} to count unique vessels within a rolling window. 
  #' 
  #' @param x List of vessel IDs to count
  #' @param k Window width.
  #' @keywords internal
  #' @seealso \code{\link{roll_catch}} \code{\link{roll_cc}}
  
  vec_out <- numeric(length(x) - k)
  
  i <- 1
  
  for (i in 1:(length(x) - k + 1)) {
    
    vec_out[i] <- length(unique(unlist(x[i:(i + (k - 1))])))
  }
  
  vec_out
}


roll_cc <- function(dat, k, align) {
  #' Apply window_cc 
  #' 
  #' Used by \code{roll_catch()} to count unique vessels within a rolling window 
  #' for multiple columns of catch.
  #' 
  #' @param dat Dataframe used to calculate rolling catch.
  #' @param k Window width.
  #' @param align Window alignment.
  #' @keywords internal
  #' @seealso \code{\link{roll_catch}} \code{\link{window_cc}}
  
  # names excluding date var
  nm <- names(dat[-1])  
  date <- names(dat)[1]
  nr <- nrow(dat)
  
  if (align == "left") {
    
    Index <- dat[[date]][1:(nr - k + 1)]
    
  } else if (align == "center") {
    
    if (k %% 2 == 0) {
      
      Index <- dat[[date]][(.5 * k):(nr - (.5 * k))]
      
    } else {
      
      Index <- dat[[date]][ceiling((.5 * k)):(nr - floor((.5 * k)))]
    }
    
  } else if (align == "right") {
    
    Index <- dat[[date]][k:nr]
  }
  
  roll_out <- data.frame(Index = Index)
  names(roll_out)[1] <- date
  
  roll_out[nm] <- lapply(dat[nm], function(x) window_cc(x, k))
  
  roll_out
}


check_conf_rc <- function(dat, roll_tab, project, catch, date, group, k, full_dates, 
                          align) {
  #' Check and suppress roll_catch output
  #' 
  #' @param dat Dataset used to create \code{roll_tab} dataframe. 
  #' @param roll_tab Unsuppressed table from \code{roll_catch}.
  #' @param project Name of project. 
  #' @param catch String, name of catch variable(s).
  #' @param date String, name of date variable.
  #' @param group String, name of group variable(s). 
  #' @param k Integer, width of window. 
  #' @param full_dates Vector of full dates.
  #' @param align String, align argument for \code{rollapply()}. 
  #' @keywords internal
  #' @importFrom zoo zoo merge.zoo rollapply fortify.zoo
  #' @importFrom tidyr pivot_longer pivot_wider nest
  #' @importFrom dplyr group_by across arrange
  #' @importFrom magrittr %>% 
  
  cc_par <- get_confid_check(project)
  
  dat <- dat[unique(c(date, cc_par$v_id, group, catch))]
  
  if (length(catch) > 1) {
    
    dat <- tidyr::pivot_longer(dat, cols = !!catch, 
                               names_to = "species", values_to = "catch")
    #value_var <- values_to
    group <- c(group, "species")
    include_val <- FALSE
    
  } else include_val <- TRUE
  
  # remove catch values before nesting 
  if (length(catch) == 1) {
    
    dat[[catch]] <- NULL
    
  } else {
    
    dat$catch <- NULL
  }
  
  # nested df with v_id list for each date
  dat_nest <- 
    dat %>% 
    dplyr::group_by(dplyr::across(c(date, group))) %>% 
    tidyr::nest(v_id = cc_par$v_id) %>% 
    dplyr::arrange(dplyr::across(date))
  
  names(dat_nest)[names(dat_nest) == "v_id"] <- cc_par$v_id
  
  if (!is.null(group)) {
    
    dat_nest[group] <- lapply(dat_nest[group], trimws)
    
    dat_nest <- tidyr::pivot_wider(dat_nest, id_cols = !!date, names_from = !!group,
                                   values_from = !!cc_par$v_id, names_sep = "__")
  }
  
  conf_tab <- zoo::zoo(dat_nest[-which(names(dat_nest) == date)], dat_nest[[date]])
  conf_tab <- zoo::merge.zoo(conf_tab, full_dates, all = TRUE, fill = list(NULL))
  
  conf_tab$full_dates <- NULL
  
  # fortify table for plotting
  conf_roll_tab <- zoo::fortify.zoo(conf_tab)
  names(conf_roll_tab)[names(conf_roll_tab) == "Index"] <- date
  
  
  conf_roll_tab <- roll_cc(conf_roll_tab, k, align)
  
  # omits zero catch events
  if (!is.null(group)) { # return list of dates to suppress for each column
    
    ind <- lapply(conf_roll_tab[-1], function(x) x > 0 & x < cc_par$value)
    
    null_ind <- vapply(ind, is.null, logical(1))
    
    supr_vals <- any(!null_ind)
    
    if (supr_vals) {
      
      ind <- ind[!null_ind]
      
      names(ind) <- trimws(names(ind))
      
      roll_tab[names(ind)] <- 
        
        lapply(names(ind), function(n) {
          
          roll_tab[ind[[n]], n] <- -999
          roll_tab[[n]]
        })
      
      check_ind <- lapply(ind, function(x) roll_tab[[date]][x])
      
      # cache check table
      cache_check_table(check_ind, project)
      warning("Confidential data detected.")
    }
    
  } else {
    
    ind <- conf_roll_tab[[cc_par$v_id]] > 0 & conf_roll_tab[[cc_par$v_id]] < cc_par$value
    supr_vals <- any(ind)
    
    if (supr_vals) {
      
      if (is.null(group)) group <- cc_par$v_id
      
      if (include_val) {
        
        conf_roll_tab$value_var <- catch
        group <- c(group, "value_var")
      }
      
      check <- conf_roll_tab[c(date, group)]
      
      check <- check[ind, ]
      
      roll_tab <- 
        suppress_table(check, roll_tab, catch, group = unique(date, group), "n")
      
      cache_check_table(check, project)
      warning("Confidential data detected.")
    }
  }
  
  list(table = if (supr_vals) roll_tab else NULL,
       suppress = supr_vals) 
} 


replace_sup_code <- function(output, code = NA) {
  
  #' Replace suppression code
  #' 
  #' This function replaces the default suppression code in a table.
  #' 
  #' Suppressed values are represented as `-999` by default. This isn't ideal 
  #' for plotting. NAs -- the default in `replace_sup_code()`-- are a better 
  #' alternative for plots as they can easily be removed. 
  #' 
  #' @param output Table containing suppressed values.
  #' @param code The replacement suppression code. \code{code = NA} by default; 
  #'   this is ideal for plotting as ggplot automatically removes NAs. 
  #' @export
  #' @examples 
  #' \dontrun{
  #' summary_tab <- replace_sup_code(summary_tab, code = NA)
  #' }
  
  ind <- which(output == -999, arr.ind = TRUE)
  
  if (nrow(ind) > 0) output[unique(ind[, 1]), unique(ind[, 2])] <- code
  
  output
}