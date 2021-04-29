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

set_confid_check <- function(check = TRUE, v_id = NULL, rule = "n", value = NULL) {
  #' Set confidentiality parameters
  #' 
  #' This function specifics whether to check for confidentiality and which 
  #' rule should be applied.
  #' 
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
  #' set_confid_check(check = TRUE, v_id = "PERMIT", rule = "n", value = 3L)
  #' }
  
  if (check == TRUE) {
    
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
    
    edit_fishset_env("confid_check", 
                     value = list(check = check, v_id = v_id, 
                                  rule = rule, value = value))
    message("Confidentiality settings saved.")
    invisible(TRUE)
  
  } else invisible(FALSE)
}

run_confid_check <- function() {
  #' Check whether confidentiality rules should be applied
  #' @keywords internal
  #' @importFrom rlang env_get
  #' @return \code{TRUE} if confidentiality settings exists and \code{check = TRUE}, 
  #'   \code{FALSE} if settings do not exists yet or \code{check = FALSE}.
  
  if (fishset_env_exists()) {
    
    out <- rlang::env_get(fishset_env, "confid_check", default = NULL)
    
    if (!is.null(out)) out$check
    else FALSE
    
  } else FALSE
}

get_confid_check <- function() {
  #' Return the confidentiality settings 
  #' 
  #' This function returns the confidentiality settings from the FishSET environment.
  #' 
  #' @return A list containing the confidentiality parameters: \code{check}, 
  #'   \code{v_id}, \code{rule}, and \code{value}.
  #' @seealso \code{\link{set_confid_check}}
  #' @export

    get_fishset_env("confid_check")
}

get_confid_cache <- function() {
  
  #' Return cached confidentiality tables
  #' 
  #' This function lists the confidentiality "check" tables used to suppress values.
  #' 
  #' @return A list of tables containing suppression conditions. 
  #' @export
  
  get_fishset_env("confid_cache")
}

cache_check_table <- function(check) {
  
  #' Add check table to confidentiality cache list
  #' 
  #' @param check Dataframe, check table to be added to confidentiality cache.
  #' @keywords internal
  
  cache <- get_confid_cache()
  
  if (is.null(cache)) cache <- list()
  
  ind <- length(cache) + 1
  
  cache[[ind]] <- check
  
  edit_fishset_env("confid_cache", cache)
}

check_confidentiality <- function(dataset, v_id, value_var, group = NULL, 
                                  rule = c("n", "k"), value, names_to = "name", 
                                  values_to = "value") {
  
  #' Create confidentiality check table
  #' 
  #' This function checks for confidential values in a summary table and creates 
  #' a table of suppression conditions, or "check table".
  #' 
  #' @param dataset The dataset used to create a summary table. This must include 
  #'   the vessel identifier column. 
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
  #'   variables when `value_var` has two or more columns.
  #' @param values_to String, the name for the column containing the values from the
  #'   variables listed in `names_to`.
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
      
    } else {
      
      check <- agg_helper(dataset, value_var, group = c(v_id, group), fun = sum)
      
      check <- perc_of_total(check, value_var = value_var, group = group)
      
      val_perc <- paste0(value_var, "_perc")
    
      ind <- check[[val_perc]] >= value
    }
  
  supr_vals <- ifelse(sum(ind) > 0, TRUE, FALSE)
    
  if (supr_vals) {
    
    if (is.null(group)) group <- v_id
    
    if (include_val) {
      
      check$value_var <- value_var
      group <- c(group, "value_var")
    }
    
    check <- tibble::as_tibble(check)
    
    check <- check[ind, group]
    
    check <- dplyr::distinct(check)
    
    cache_check_table(check)
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
  
  #if (is.null(group)) group <- get_confid_check()$v_id
  
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

check_and_suppress <- function(dat, v_id, value_var, group = NULL, rule, value,
                               names_to = "name", values_to = "value") {
  
  #'Check and suppress data
  #'
  #' @param dataset The dataset used to create a summary table. This must include 
  #'   the vessel identifier column. 
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
  #'   variables when `value_var` has two or more columns.
  #' @param values_to String, the name for the column containing the values from the
  #'   variables listed in `names_to`.
  #' @keywords internal
  
  check <- 
    check_confidentiality(dat, v_id, value_var, group, rule, value, names_to, 
                          values_to)
  
  if (check$suppress) {
    
    dat <- suppress_table(check$table, dat, value_var, group, rule)
    
    dat
  } else check
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
