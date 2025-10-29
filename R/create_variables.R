cpue <- function(dat, project, xWeight = NULL, xTime, price = NULL, name = NULL) {
  #' Create catch or revenue per unit effort variable
  #' 
  #' @description Add catch per unit effort (CPUE) or revenue per unit effort 
  #'   variable to the primary dataset. Catch should be a weight variable but can 
  #'   be a count. Effort should be in a duration of time, such as days, hours, 
  #'   or minutes.
  #' @param dat Primary data containing information on hauls or trips. Table in 
  #'   FishSET database contains the string 'MainDataTable'.
  #' @param project Project name. 
  #' @param xWeight Catch variable in \code{dat}. Variable should be a measure of 
  #'   weight (pounds, metric tons, etc) but can also be count. If calculating
  #'   revenue per unit effort (RPUE) and a revenue column exists in \code{dat}, 
  #'   then add the revenue column to \code{price} and set \code{xWeight = NULL}.
  #' @param xTime Duration of time variable in \code{dat} representing effort, 
  #'   such as weeks, days, hours, or minutes.
  #' @param price Optional, variable from \code{dat} containing price/value data. 
  #'   Price is multiplied against the catch variable, \code{xWeight}, to generated 
  #'   revenue. If revenue exists in \code{dat} and you wish to use this revenue 
  #'   instead of price, then \code{xWeight} must be \code{NULL}. Defaults to 
  #'   \code{NULL}.
  #' @param name String, name of created variable. Defaults to "cpue" or "rpue" 
  #'   if \code{price} is not \code{NULL}.
  #' @export cpue
  #' @details Creates the catch or revenue per unit effort variable. Catch variable 
  #'   should be in weight (lbs, mts). Effort variable should be a measurement of 
  #'   duration in time. New variable is added to the primary dataset with the 
  #'   column name defined by the \code{name} argument. CPUE for individual species
  #'   should be calculated separately.
  #' @return Returns primary dataset with CPUE variable added.
  #' @examples
  #' \dontrun{
  #' pollockMainDataTable <- cpue(pollockMainDataTable, 'pollock', 
  #'                              xWeight = 'OFFICIAL_TOTAL_CATCH_MT', 
  #'                              xTime = 'DURATION_IN_MIN', name = 'cpue')
  #' }
  
  # Call in datasets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  if (is_value_empty(xWeight) & is_value_empty(price)) {
    
    stop("'xWeight' and/or 'price' argument must be provided.", call. = FALSE)
  }
  
  column_check(dataset, cols = c(xWeight, xTime, price))
  
  if (is_value_empty(name)) {
    
    if (!is.null(price)) name = 'rpue'
    else name = 'cpue'
    
    warning("'name' empty, using '", name, "'.", call. = FALSE)
  }
  
  name <- name_check(dataset, name, repair = TRUE)
  
  # TODO: Deal with revenue issue
  if (!is.null(price)) {
    
    if (!is.null(xWeight)) {
      
      stopifnot("xWeight must be numeric" = is.numeric(dataset[[xWeight]]))
      stopifnot("price must be numeric" = is.numeric(dataset[[price]]))
      
      if (!grepl("LB|Pounds|MT", xWeight, ignore.case = TRUE)) {
        
        warning("xWeight must a measurement of mass. RPUE calculated.", call. = FALSE)
      }
      
      weight <- dataset[[xWeight]] * dataset[[price]]
      
    } else {
      
      weight <- dataset[[price]]
    }
    
  } else {
    
    stopifnot("xWeight must be numeric" = is.numeric(dataset[[xWeight]]))
    
    if (!grepl("LB|Pounds|MT", xWeight, ignore.case = TRUE)) {
      
      warning("xWeight must a measurement of mass. CPUE calculated.", call. = FALSE)
    }
    
    weight <- dataset[[xWeight]]
  }
  
  stopifnot("xTime must be numeric" = is.numeric(dataset[[xTime]])) 
  
  # Check that Weight variable is indeed a weight variable
  if (!grepl("Duration", xTime, ignore.case = TRUE)) {
    
    warning("xTime should be a measurement of time. Use the create_duration ", 
            "function. CPUE calculated.", call. = FALSE)
  }
  
  # TODO: safely test
  dataset[[name]] <- weight / dataset[[xTime]]
  
  if (qaqc_helper(dataset[name], "Inf")) {
    
    warning("Inf values detected in '", name, "'.", call. = FALSE)
  }
  
  cpue_function <- list()
  cpue_function$functionID <- "cpue"
  cpue_function$args <- list(dat, project, xWeight, xTime, price, name)
  cpue_function$kwargs <- list()
  cpue_function$output <- list(dat)
  
  log_call(project, cpue_function)
  
  return(dataset)
}

## ---- Dummy  Variables ----##
# dummy_num
dummy_num <- function(dat, project, var, value, opts = "more_less", name = "dummy_num") {
  #' Create a binary vector from numeric, date, and character or factor vectors.
  #' @param dat Primary data containing information on hauls or trips.
  #'   Table in the FishSET database contains the string 'MainDataTable'.
  #' @param project Project name. 
  #' @param var Variable in \code{dat} to create dummy variable from.
  #' @param value String, value to set dummy variable by. If \code{var} is a date, value should be a year,
  #'   If \code{var} is a factor, value should be a factor level. If \code{var} is numeric, value should be a single
  #'   number or range of numbers [use c(1,5)].
  #' @param opts String, how dummy variable should be defined. Choices are \code{"x_y"} and \code{"more_less’"}. For \code{"x_y"}, each
  #'   element of \code{var} is set to 1 if the element matches \code{value}, otherwise 0.
  #'   For \code{"more_less"}, each element of \code{var} less than \code{value} is set to 0 and all elements greater than
  #'   \code{value} set to 1. If \code{var} is a factor, then elements that match value will be set to 1 and all other
  #'   elements set to 0. Default is set to \code{"more_less"}.
  #' @param name String, name of created dummy variable. Defaults to name of the function if not defined.
  #' @importFrom lubridate origin as_date
  #' @details For date variables, the dummy variable is defined by a date (year) and may be either year \code{x} versus all
  #'   other years (\code{"x_y"}) or before vs after year \code{x} (\code{"more_less"}). Use this function to create a variable defining whether
  #'   or not a policy action had been implemented. \cr
  #'   Example: before vs. after a 2008 amendment: \cr
  #'   \code{dummy_num('pollockMainDataTable', 'Haul_date', 2008, 'more_less', 'amend08')} \cr\cr
  #'
  #'  For factor variables, both choices in \code{opts} compare selected factor level(s) against all other factor levels.\cr
  #'  Example: Fishers targeting pollock vs. another species:  \cr
  #'  \code{dummy_num('pollockMainDataTable', 'GF_TARGET_FT', c('Pollock - bottom', 'Pollock - midwater'), 'x_y', 'pollock_target')}  \cr\cr
  #'
  #'  For numeric variables, \code{value} can be a single number or a range of numbers. The dummy variable is the
  #'  selected value(s) against all others (\code{x_y}) or less than the selected value versus more than the selected value
  #'  (\code{more_less}). For \code{more_less}, the mean is used as the critical value if a range of values is provided.
  #' @return Returns primary dataset with dummy variable added.
  #' @export
  #' @examples
  #' \dontrun{
  #' pollockMainDataTable <- dummy_num(pollockMainDataTable, 'pollock', 'Haul_date', 2008, 
  #'   'more_less', 'amend80')
  #' }
  
  
  # Pull in data
  out <- data_pull(dat, project)
  dataset <- out$dataset
  
  dat <- parse_data_name(dat, "main", project)
  
  # name <- ifelse(is_empty(name), "dummy_num", name)
  name <- name_check(dataset, name, repair = TRUE)
  
  if (grepl("dat|year", var, ignore.case = TRUE)) {
    if (length(value) == 6) {
      dataset[[var]] <- format(lubridate::as_date(dataset[[var]]), "%Y%m")
    } else if (length(value) == 4) {
      dataset[[var]] <- format(lubridate::as_date(dataset[[var]]), "%Y")
    } else {
      dataset[[var]] <- format(lubridate::as_date(dataset[[var]]), "%m")
    }
  } 
  
  if (is.numeric(dataset[[var]])) {
    if (opts == "x_y") {
      newvar <- ifelse(dataset[[var]] >= min(value) & dataset[[var]] <= max(value), 0, 1)
    } else {
      newvar <- ifelse(dataset[[var]] < mean(value), 0, 1)
    }
  } else if (is.factor(dataset[[var]]) | is.character(dataset[[var]])) {
    newvar <- ifelse(trimws(dataset[[var]], "both") == trimws(value, "both"), 0, 1)
  } else {
    (warning("variable is not recognized as being a date, factor, or numeric. Function not run."))
  }
  
  g <- cbind(dataset, newvar)
  colnames(g)[dim(g)[2]] = name
  
  dummy_num_function <- list()
  dummy_num_function$functionID <- "dummy_num"
  dummy_num_function$args <- list(dat, project, var, value, opts, name)
  dummy_num_function$kwargs <- list()
  dummy_num_function$output <- list(dat)
  
  log_call(project, dummy_num_function)
  return(g)
}

#' Create dummy variable
dummy_var <- function(dat, project, DumFill = 1, name = "dummy_var") {
  #' @param dat Primary data containing information on hauls or trips.
  #' Table in the FishSET database contains the string 'MainDataTable'.
  #' @param project Project name. 
  #' @param DumFill Fill the dummy variable with 1 or 0
  #' @param name String, name of created dummy variable. Defaults to name of the function if not defined.
  #' @return Primary dataset with dummy variable added.
  #' @export dummy_var
  #' @details Creates a dummy variable of either 0 or 1 with length of the number of rows of the data set.
  #' @examples
  #' \dontrun{
  #' pollockMainDataTable <- dummy_var(pollockMainDataTable, 'pollock', DumFill=1, 'dummyvar')
  #' }
  
  # Pull in data
  out <- data_pull(dat, project)
  dataset <- out$dataset
  
  dat <- parse_data_name(dat, "main", project)
  
  # name <- ifelse(is_empty(name), "dummy_var", name)
  name <- name_check(dataset, name, repair = TRUE)
  
  newvar <- as.vector(rep(DumFill, nrow(dataset)))
  
  g <- cbind(dataset, newvar)
  colnames(g)[dim(g)[2]] = name
  
  dummy_var_function <- list()
  dummy_var_function$functionID <- "dummy_var"
  dummy_var_function$args <- list(dat, project, DumFill, name)
  dummy_var_function$kwargs <- list()
  dummy_var_function$output <- list(dat)
  
  log_call(project, dummy_var_function)
  
  return(g)
}

#' Create dummy matrix from a coded ID variable
dummy_matrix <- function(dat, project, x) {
  #' @param dat Primary data containing information on hauls or trips.
  #' Table in FishSET database contains the string 'MainDataTable'.
  #' @param project Project name.
  #' @param x Variable in \code{dat} used to generate dummy matrix.
  #' @export dummy_matrix
  #' @details Creates a dummy matrix of 1/0 with dimensions \emph{[(number of observations in dataset) x
  #' (number of factors in x)]} where each column is a unique factor level. Values are 1 if the value in the
  #' column matches the column factor level and 0 otherwise.
  #' @examples
  #' \dontrun{
  #' PortMatrix <- dummy_matrix(pollockMainDataTable, 'pollock', 'PORT_CODE')
  #' }
  
  
  out <- data_pull(dat, project)
  dataset <- out$dataset
  
  dat <- parse_data_name(dat, "main", project)
  
  
  # create the matrix
  factor.levels <- levels(as.factor(dataset[[x]]))
  int <- data.frame(matrix(rep(dataset[[x]], length(factor.levels)), ncol = length(factor.levels)))
  colnames(int) <- factor.levels
  
  # change matrix to TRUE/FALSE
  int <- data.frame(lapply(1:length(factor.levels), function(x) ifelse(int[, x] == colnames(int)[x], 1, 0)))
  colnames(int) <- paste(x, "_", levels(as.factor(dataset[[x]])))
  
  dummy_matrix_function <- list()
  dummy_matrix_function$functionID <- "dummy_matrix"
  dummy_matrix_function$args <- list(dat, project, x)
  dummy_matrix_function$kwargs <- list()
  dummy_matrix_function$output <- list()
  log_call(project, dummy_matrix_function)
  
  return(int)
}

## ---- Coded variables ----##
#' Create factor variable from quantiles
#'
#' Create a factor variable from numeric data.  Numeric variable is split into categories based on quantile categories.
#'
set_quants <- function(dat, project, x, quant.cat = c(0.1, 0.2, 0.25,0.33, 0.4), custom.quant = NULL, name = "set_quants") {
  #' @param dat Primary data containing information on hauls or trips.
  #' Table in FishSET database contains the string 'MainDataTable'.
  #' @param project Project name.
  #' @param x Variable to transform into quantiles.
  #' @param quant.cat Quantile options: \code{"0.2"}, \code{"0.25"}, \code{"0.33"}, and \code{"0.4"}
  #' \itemize{
  #'   \item{0.1:  (0\%, 10\%, 20\%, 30\%, 40\%, 50\%, 60\%, 70\%, 80\%, 90\%, 100\%)}
  #'   \item{0.2:  (0\%, 20\%, 40\%, 60\%, 80\%, 100\%)}
  #'   \item{0.25: (0\%, 25\%, 50\%, 75\%, 100\%)}
  #'   \item{0.33: (0\%, 33\%, 66\%, 100\%)}
  #'   \item{0.4:  (0\%, 10\%, 50\%, 90\%, 100\%)}
  #'   }
  #' @param custom.quant Vector, user defined quantiles.
  #' @param name String, name of created vector. Defaults to name of the function if not defined.
  #' @return Primary dataset with quantile variable added.
  #' @export set_quants  
  #'
  #' @examples
  #' \dontrun{
  #' pollockMainDataTable <- set_quants(pollockMainDataTable, 'pollock', 'HAUL', 
  #'    quant.cat=.2, 'haul.quant')
  #' }
  #
  out <- data_pull(dat, project)
  dataset <- out$dataset
  
  dat <- parse_data_name(dat, "main", project)
  
  name <- ifelse(is_empty(name), "set_quants", name)
  
  tmp <- 0
  
  if (!is.numeric(dataset[[x]])) {
    tmp <- 1
    warning("Variable must be numeric. Function not run.")
  }
  
  if (tmp == 0) {
    if (quant.cat == 0.1) {
      prob.def <- seq(0, 1, by = .1)
    } else if (quant.cat == 0.2) {
      prob.def <- seq(0, 1, by = .2)
    } else if (quant.cat == 0.25) {
      prob.def <- seq(0, 1, by = .25)
    } else if(quant.cat == 0.33) {
      prob.def <- c(0, .33, .66, 1)
    }  else if (quant.cat == 0.4) {
      prob.def <- c(0, 0.1, 0.5, 0.9, 1)
    }
    
    if (!is.null(custom.quant) & is.numeric(custom.quant)) {
      prob.def <- custom.quant
    }
    # var.name <- paste('TRIP_OTC_MT', 'quantile', sep = '.')
    newvar <- as.integer(cut(dataset[[x]], quantile(dataset[[x]], probs = prob.def), include.lowest = TRUE))
    
    g <- cbind(dataset, newvar)
    colnames(g)[dim(g)[2]] = name
    
    set_quants_function <- list()
    set_quants_function$functionID <- "set_quants"
    set_quants_function$args <- list(dat, project, x, quant.cat, custom.quant, name)
    set_quants_function$kwargs <- list()
    set_quants_function$output <- list(dat)
    
    log_call(project, set_quants_function)
    return(g)
  }
}


bin_var <- function(dat, project, var, br, name = "bin", labs = NULL, ...) {
  #'
  #' Creates numeric variables divided into equal sized groups
  #'
  #' @param dat Primary data containing information on hauls or trips. Table in FishSET database contains the string 'MainDataTable'.
  #' @param project String, name of project.
  #' @param var Numeric variable in \code{dat} to bin into a factor.
  #' @param br Numeric vector. If a single number, the range of \code{var} is divided into \code{br} even groups. If two or more values are given, \code{var} is divided into intervals.
  #' @param name Variable name to return. Defaults to `bin`.
  #' @param labs A character string of category labels.
  #' @param ... Additional arguments passed to \code{\link{cut}}.
  #' @details Function adds a new factor variable, labeled by name, to the primary dataset. The numeric variable 
  #' is divided into equal sized groups if the length of \code{br} is equal to one and into intervals if the 
  #' length of \code{br} is greater than one.
  #' @export
  #' @return Returns the primary dataset with binned variable added.
  #' @examples
  #' \dontrun{
  #'  pollockMainDataTable <- bin_var(pollockMainDataTable, 'pollock', 'HAUL', 10, 'HAULCAT')
  #'  pollockMainDataTable <- bin_var(pollockMainDataTable, 'pollock', 'HAUL', c(5,10), 'HAULCAT')
  #' }
  
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  # name <- ifelse(is_empty(name), "bin", name)
  name <- name_check(dataset, name, repair = TRUE)
  
  tmp <- 0
  
  if (!is.numeric(dataset[[var]])) {
    tmp <- 1
    warning("Variable must be numeric.")
  }
  
  if (tmp == 0) {
    newvar <- cut(dataset[[var]], breaks = br, labels = labs, ...)
    
    g <- cbind(dataset, newvar)
    colnames(g)[dim(g)[2]] = name
    
    # Log function
    bin_var_function <- list()
    bin_var_function$functionID <- "bin_var"
    bin_var_function$args <- c(dat, project, var, project, br, name, labs)
    bin_var_function$kwargs <- list()
    bin_var_function$output <- list(dat)
    log_call(project, bin_var_function)
    
    return(g)
  }
}

# within group ----

group_perc <- function(dat, project, id_group, group = NULL, value, name = "group_perc", 
                       create_group_ID = FALSE, drop_total_col = FALSE) {
  #' Create a within-group percentage variable 
  #'
  #' @param dat Primary data frame over which to apply function. Table in FishSET 
  #'   database should contain the string `MainDataTable`.
  #' @param project String, project name.
  #' @param id_group String, primary grouping variable(s). Used to create the "total_value" 
  #'   variable which sums \code{value} by \code{id_group}. If \code{group = NULL}, then 
  #'   \code{value} is divided by "total_value".
  #' @param group String, secondary grouping variable(s). Used to create the "group_total" 
  #'   variable which sums \code{value} by \code{id_group} and \code{group}. Percentage 
  #'   is calculated by dividing "group_total" by "total_value". Defaults to \code{NULL}.
  #' @param value String, the value variable used to calculate percentage. Must be numeric. 
  #' @param name String, the name for the new variable. Defaults to "group_perc". 
  #' @param create_group_ID Logical, whether to create a group ID variable using \code{\link{ID_var}}.
  #'   Defaults to \code{FALSE}.
  #' @param drop_total_col Logical, whether to remove the "total_value" and "group_total" variables
  #'   created to calculate percentage. Defaults to \code{FALSE}.
  #' @export
  #' @importFrom dplyr across mutate group_by select ungroup rename_with
  #' @importFrom shiny isRunning
  #' @details \code{group_perc} creates a within-group percentage variable using a primary
  #'   group ID (\code{id_group}) and secondary group (\code{group}). The total value of 
  #'   \code{id_group} is stored in the "total_value" variable, and the within-group total
  #'   stored in "group_total". The group percentage is calculated using these two function-created
  #'   variables. "total_value" and "group_total" can be dropped by setting \code{drop_total_col = TRUE}.
  #'   A group ID column can be created using the variables in\code{id_group} and \code{group} by setting 
  #'   \code{create_group_ID = TRUE}. 
  #' @examples
  #' \dontrun{
  #' group_perc(pollockMainDataTable, "pollock", id_group = "PERMIT", group = NULL, 
  #'            value = "OFFICIAL_TOTAL_CATCH_MT")
  #'            
  #' group_perc(pollockMainDataTable, "pollock", id_group = "PERMIT",
  #'            group = "DISEMBARKED_PORT", value = "HAUL")
  #' }
  
  out <- data_pull(dat, project)
  dataset <- out$dataset
  
  dat <- parse_data_name(dat, "main", project)
  
  # name <- ifelse(is_empty(name), "group_perc", name)
  name <- name_check(dataset, name, repair = TRUE)
  
  if (create_group_ID) dataset <- ID_var(dataset, project, vars = c(id_group, group), 
                                         log_fun = FALSE)
  
  . <- group_total <- total_value <- NULL
  
  if (is.null(group)) {
    
    dataset <- dataset %>% 
      dplyr::group_by(dplyr::across(dplyr::all_of(id_group))) %>% 
      dplyr::mutate(dplyr::across(dplyr::all_of(value), sum, .names = "total_value")) %>% # calc. total value by id_group
      dplyr::ungroup() %>% 
      dplyr::mutate(dplyr::across(dplyr::all_of(value), .fns = ~ (.x/total_value) * 100, .names = name)) %>% # calc. percent of total value
      { if (drop_total_col) dplyr::select(., -total_value) else . } # drop total column if desired
    
  } else {
    
    dataset <- dataset %>% 
      dplyr::group_by(dplyr::across(dplyr::all_of(id_group))) %>% 
      dplyr::mutate(dplyr::across(dplyr::all_of(value), sum, .names = "total_value")) %>% # calc. total value by id_group
      dplyr::group_by(dplyr::across(dplyr::all_of(group)), .add = TRUE) %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(value), sum, .names = "group_total")) %>% # calc. group total
      dplyr::mutate(temporary_col_name = (group_total/total_value) * 100) %>%
      dplyr::rename_with(~ name, .cols = "temporary_col_name") %>%
      dplyr::ungroup() %>% 
      { if (drop_total_col) dplyr::select(., -c(group_total, total_value)) else . }
  }
  
  group_perc_function <- list()
  group_perc_function$functionID <- "group_perc"
  group_perc_function$args <- list(dat, project, id_group, group, value, name, 
                                   create_group_ID, drop_total_col) 
  log_call(project, group_perc_function)
  
  dataset
}


group_diff <- function(dat, project, group, sort_by, value, name = "group_diff", 
                       lag = 1, create_group_ID = FALSE, drop_total_col = FALSE) {
  #' Create a within-group lagged difference variable
  #' 
  #' @param dat Primary data frame over which to apply function. Table in FishSET 
  #'   database should contain the string `MainDataTable`.
  #' @param project String, project name.
  #' @param group String, the grouping variable(s) to sum \code{value} by. Used to create the 
  #'   "group_total" variable.  
  #' @param sort_by String, a date variable to order `MainDataTable` by. 
  #' @param value String, the value variable used to calculate lagged difference. Must be numeric. 
  #' @param name String, the name for the new variable. Defaults to "group_diff". 
  #' @param lag Integer, adjusts lag length. Defaults to 1. 
  #' @param create_group_ID Logical, whether to create a group ID variable using \code{\link{ID_var}}.
  #'   Defaults to \code{FALSE}.
  #' @param drop_total_col Logical, whether to remove the "group_total" variable
  #'   created to calculate percentage. Defaults to \code{FALSE}.
  #' @export
  #' @importFrom dplyr across arrange left_join mutate group_by select summarize ungroup rename_with
  #' @importFrom shiny isRunning
  #' @details \code{group_diff} creates a grouped lagged difference variable. \code{value}
  #'   is first summed by the variable(s) in \code{group}, then the difference within-group is 
  #'   calculated. The "group_total" variable gives the total value by group and can
  #'   be dropped by setting \code{drop_total_col = TRUE}. A group ID column can be 
  #'   created using the variables in \code{group} by setting \code{create_group_ID = TRUE}. 
  #' @examples 
  #' \dontrun{
  #' group_diff(pollockMainDataTable, "pollock", group = c("PERMIT", "TRIP_ID"),
  #'            sort_by = "HAUL_DATE", value = "HAUL")
  #' }
  
  out <- data_pull(dat, project)
  dataset <- out$dataset
  
  dat <- parse_data_name(dat, "main", project)
  
  # name <- ifelse(is_empty(name), "group_diff", name)
  name <- name_check(dataset, name, repair = TRUE)
  
  
  . <- group_total <- NULL
  
  
  if (create_group_ID) dataset <- ID_var(dataset, project, vars = group, 
                                         log_fun = FALSE)
  
  alt_diff <- function(x, lag) c(0, diff(x, lag = lag))
  
  if (all(!(class(dataset[[sort_by]]) %in% c("Date","POSIXct", "POSIXt")))) {
    
    dataset[[sort_by]] <- date_parser(dataset[[sort_by]])
  }
  
  tab <- dataset %>% 
    dplyr::arrange(dplyr::across(dplyr::all_of(sort_by))) %>% 
    dplyr::group_by(dplyr::across(dplyr::all_of(group))) %>% 
    { if (length(group) == 1)
      dplyr::mutate(., dplyr::across(dplyr::all_of(value), sum, .names = "group_total")) %>% 
        dplyr::mutate(., dplyr::across(dplyr::all_of(value), alt_diff, lag = lag, .names = name))
      else 
        dplyr::summarize(., dplyr::across(dplyr::all_of(value), sum, .names = "group_total")) %>% 
        dplyr::mutate(., temporary_col_name = alt_diff(group_total, lag = lag)) } %>% 
    
    dplyr::rename_with(~ name, .cols = "temporary_col_name") %>%
    dplyr::ungroup() %>% 
    { if (drop_total_col) dplyr::select(., -group_total) else . }
  
  if (length(group) > 1) {
    dataset <- dplyr::left_join(dataset, tab, by = group)
  } else { 
    dataset <- tab 
  }
  
  group_diff_function <- list()
  group_diff_function$functionID <- "group_diff"
  group_diff_function$args <- list(dat, project, group, sort_by, value, name, lag,
                                   create_group_ID, drop_total_col) 
  log_call(project, group_diff_function)
  
  dataset
}

group_cumsum <- function(dat, project, group, sort_by, value, name = "group_cumsum", 
                         create_group_ID = FALSE, drop_total_col = FALSE) {
  #' Create a within-group running sum variable
  #' 
  #' @param dat Primaryy data frame over which to apply function. Table in FishSET 
  #'   database should contain the string `MainDataTable`.
  #' @param project String, project name.
  #' @param group String, the grouping variable(s) to sum \code{value} by. Used to create the 
  #'   "group_total" variable.  
  #' @param sort_by String, a date variable to order `MainDataTable` by. 
  #' @param value String, the value variable used to calculate cumulative sum. Must be numeric. 
  #' @param name String, the name for the new variable. Defaults to "group_cumsum". 
  #' @param create_group_ID Logical, whether to create a group ID variable using \code{\link{ID_var}}.
  #'   Defaults to \code{FALSE}.
  #' @param drop_total_col Logical, whether to remove the "group_total" variable
  #'   created to calculate percentage. Defaults to \code{FALSE}.
  #' @export
  #' @importFrom dplyr across arrange left_join mutate group_by select summarize ungroup %>% rename_with
  #' @importFrom shiny isRunning
  #' @details \code{group_cumsum} sums \code{value} by \code{group}, then cumulatively
  #'   sums within groups. For example, a running sum by trip variable can be made 
  #'   by entering
  #'   variables that identify unique vessels and trips into \code{group} and a numeric
  #'   variable (such as catch or # of hauls) into \code{value}. Each vessel's
  #'   trip total is calculated then cumulatively summed. The "group_total" variable 
  #'   gives the total value by group and can be dropped by setting \code{drop_total_col = TRUE}.
  #'   A group ID column can be created using the variables in \code{group} by setting
  #'   \code{create_group_ID = TRUE}. 
  #' @examples 
  #' \dontrun{
  #' group_cumsum(pollockMainDataTable, "pollock", group = c("PERMIT", "TRIP_ID"),
  #'              sort_by = "HAUL_DATE", value = "OFFICIAL_TOTAL_CATCH")
  #' }
  
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  # name <- ifelse(is_empty(name), "group_cumsum", name)
  name <- name_check(dataset, name, repair = TRUE)
  
  . <- group_total <- NULL
  
  if (create_group_ID) dataset <- ID_var(dataset, project, vars = group, 
                                         log_fun = FALSE)
  
  if (all(!(class(dataset[[sort_by]]) %in% c("Date","POSIXct", "POSIXt")))) {
    
    dataset[[sort_by]] <- date_parser(dataset[[sort_by]])
  }
  
  tab <- 
    dataset %>% 
    dplyr::arrange(dplyr::across(sort_by)) %>% 
    dplyr::group_by(dplyr::across(group)) %>% 
    { if (length(group) == 1) 
      dplyr::mutate(., dplyr::across(value, sum, .names = "group_total")) %>% 
        dplyr::mutate(., dplyr::across(value, cumsum, .names = name))
      else 
        dplyr::summarize(., dplyr::across(value, sum, .names = "group_total")) %>% 
        dplyr::mutate(., temporary_col_name = cumsum(group_total)) } %>% 
    dplyr::rename_with(~ name, .cols = "temporary_col_name") %>%
    dplyr::ungroup() %>% 
    { if (drop_total_col) dplyr::select(., -group_total) else . }
  
  if (length(group) > 1) {
    dataset <- dplyr::left_join(dataset, tab, by = group)
  } else { 
    dataset <- tab 
  }
  
  group_cumsum_function <- list()
  group_cumsum_function$functionID <- "group_cumsum"
  group_cumsum_function$args <- list(dat, project, group, sort_by, value, name,
                                     create_group_ID, drop_total_col) 
  log_call(project, group_cumsum_function)
  
  dataset
}

## ---- Numeric  Variables ----##
#' Create numeric variable using arithmetic expression
create_var_num <- function(dat, project, x, y, method, name = "create_var_num") {
  #' @description Creates a new variable based on the arithmetic operation between two variables.  
  #'   Function is useful for creating rate variables or the summation of two related variables.
  #' @param dat Primary data containing information on hauls or trips.
  #'   Table in the FishSET database contains the string 'MainDataTable'.
  #' @param project Project name. 
  #' @param x Variable in \code{dat}. Variable will be the numerator if \code{method} is division.
  #' @param y Variable  in \code{dat} or numeric value. Variable will be the denominator if \code{method} is division.
  #' @param method String, arithmetic expression. Options include: \code{"sum"}, addition (\code{"add"}),
  #' subtraction (\code{"sub"}), multiplication (\code{"mult"}), and division (\code{"div"}).
  #' @param name String, name of created vector. Defaults to name of the function if not defined.
  #' @export create_var_num
  #' @details Creates a new numeric variable based on the defined arithmetic expression \code{method}. 
  #'   New variable is added to the primary dataset.
  #' @return Returns primary dataset with new variable added.
  #' @examples
  #' \dontrun{
  #' pollockMainDataTable <- create_var_num(pollockMainDataTable, 'pollock', x = 'HAUL_CHINOOK',
  #'     y = 'HAUL_CHUM', method = 'sum', name = 'tot_salmon')
  #' }
  
  out <- data_pull(dat, project)
  dataset <- out$dataset
  
  dat <- parse_data_name(dat, "main", project)
  
  # name <- ifelse(is_empty(name), "create_var_num", name)
  name <- name_check(dataset, name, repair = TRUE)
  
  if(is.character(y)){
    if (is.numeric(dataset[[x]]) == FALSE | is.numeric(dataset[[y]]) == FALSE) {
      stop("Variables must be numeric")
    }
    y_new <- dataset[[y]]
    
  } else if(is.numeric(y)){
    y_new <- y
    
  }
  
  if (grepl("add|sum", method, ignore.case = TRUE)) {
    newvar <- dataset[[x]] + y_new
  } else if (grepl("sub", method, ignore.case = TRUE)) {
    newvar <- dataset[[x]] - y_new
  } else if (grepl("mult", method, ignore.case = TRUE)) {
    newvar <- dataset[[x]] * y_new
  } else if (grepl("div", method, ignore.case = TRUE)) {
    newvar <- dataset[[x]] / y_new
  }
  
  g <- cbind(dataset, newvar)
  colnames(g)[dim(g)[2]] = name
  
  create_var_num_function <- list()
  create_var_num_function$functionID <- "create_var_num"
  create_var_num_function$args <- list(dat, project, x, y, method, name)
  create_var_num_function$kwargs <- list()
  create_var_num_function$output <- list(dat)
  log_call(project, create_var_num_function)
  
  return(g)
}

## ---- Spatial  Variables ----##

#' Creates haul midpoint latitude and longitude variables
create_mid_haul <- function(dat, project, start = c("lon", "lat"), end = c("lon", "lat"), 
                            name = "mid_haul") {
  #' @description Calculates latitude and longitude of the haul midpoint and adds two variables
  #' to the primary data set: the midpoint latitude and the midpoint longitude.
  #'
  #' @param dat Primary data containing information on hauls or trips.
  #' Table in the FishSET database contains the string 'MainDataTable'.
  #' @param project Project name. 
  #' @param start Character string, variables in \code{dat} defining the longitude and latitude of
  #' the starting location of haul. Must be in decimal degrees.
  #' @param end Character string, variables in \code{dat} defining the longitude and latitude of the
  #' ending location of haul.  Must be in decimal degrees.
  #' @param name String, name of new variable. Defaults to `mid_haul`.
  #' @details Each row of data must be a unique haul. Requires a start and end point for each observation.
  #' @return Returns primary dataset with two new variables added: latitude and longitude of haul midpoint.
  #' @importFrom geosphere distGeo midPoint
  #' @export
  #' @examples
  #' \dontrun{
  #' pollockMainDataTable <- create_mid_haul(pollockMainDataTable, 'pollock', 
  #'     start = c('LonLat_START_LON', 'LonLat_START_LAT'), 
  #'    end = c('LonLat_END_LON', 'LonLat_END_LAT'), name = 'mid_haul')
  #' }
  #
  
  out <- data_pull(dat, project)
  dataset <- out$dataset
  
  dat <- parse_data_name(dat, "main", project)
  
  # name <- ifelse(is_empty(name), "mid_haul" , name)
  name <- name_check(dataset, name, repair = TRUE)
  
  if (is_empty(start) || is_empty(end)) {
    stop("Starting and end locations must both be specified. Function not run.")
  }
  
  # this checks the length of the vars, but dataframes already must have same length
  # for each column. Not sure this check is fleshed out. 
  if (dim(dataset[, c(start)])[1] != dim(dataset[, c(end)])[1]) {
    stop("Starting and ending locations are of different lengths. Function not run.")
  }
  
  if (any(abs(dataset[, c(start)][1]) > 180) | any(abs(dataset[, c(end)][1]) > 180)) {
    stop("Longitude is not valid (outside -180:180). Function not run")
    # stop('Longitude is not valid (outside -180:180.')
    
  }
  
  if (any(abs(dataset[, c(start)][2]) > 90) | any(abs(dataset[, c(end)][2]) > 90)) {
    stop("Latitude is not valid (outside -90:90. Function not run")
    
    # stop('Latitude is not valid (outside -90:90.')
  }
  
  # TODO: check whether this can be done in sf; if so remove geosphere package
  distBetween <- geosphere::midPoint(dataset[, c(start)], dataset[, c(end)])
  colnames(distBetween) <- c(paste0(name, "Lon"), paste0(name, "Lat"))
  out <- cbind(dataset, distBetween)
  
  create_mid_haul_function <- list()
  create_mid_haul_function$functionID <- "create_mid_haul"
  create_mid_haul_function$args <- list(dat, project, start, end, name)
  create_mid_haul_function$kwargs <- list()
  create_mid_haul_function$output <- list(dat)
  log_call(project, create_mid_haul_function)
  
  return(out)
  
}

create_trip_centroid <- function(dat, project, lon, lat, tripID, weight.var = NULL) {
  ## ----trip centroid-----#
  #' Create trip centroid variable
  #'
  #' Create latitude and longitude variables containing the centroid of each trip
  #'
  #' @param dat Primary data containing information on hauls or trips. Table in the FishSET database contains the string 'MainDataTable'.
  #' @param project Project name. 
  #' @param lat Variable in \code{dat} containing latitudinal data.
  #' @param lon Variable in \code{dat} containing longitudinal data.
  #' @param tripID Variable in \code{dat} containing trip identifier. If trip identifier should be defined by more than one variable then list as \code{c('var1', 'var2')}.
  #' @param weight.var Variable in \code{dat} for computing the weighted average.
  #' @details Computes the average longitude and latitude for each trip. Specify \code{weight.var} to calculate the weighted centroid.
  #'   Additional arguments can be added that define unique trips. If no additional arguments are added, each row will be treated as a unique trip.
  #' @return Returns the primary dataset with centroid latitude and centroid longitude variables added.
  #' @importFrom stats ave
  #' @export
  #' @examples
  #' \dontrun{
  #' pollockMainDataTable <- create_trip_centroid(pollockMainDataTable, 'pollock', 'LonLat_START_LON', 
  #'   'LonLat_START_LAT', weight.var = NULL, 'DISEMBARKED_PORT', 'EMBARKED_PORT')
  #' }
  
  out <- data_pull(dat, project)
  dataset <- out$dataset
  
  dat <- parse_data_name(dat, "main", project)
  
  x <- 0
  if (any(abs(dataset[[lon]]) > 180)) {
    stop("Longitude is not valid (outside -180:180). Function not run")
    # stop('Longitude is not valid (outside -180:180.')
    
  }
  if (any(abs(dataset[[lat]]) > 90)) {
    stop("Latitude is not valid (outside -90:90. Function not run")
    
    # stop('Latitude is not valid (outside -90:90.')
  }
  
  
  #    if (grepl("input", as.character(match.call(expand.dots = FALSE)$...)[1]) == TRUE) {
  #      argList <- eval(...)
  #    } else {
  #     argList <- (as.character(match.call(expand.dots = FALSE)$...))
  #   }
  
  
  idmaker <- function(vec) {
    return(paste(sort(vec), collapse = ""))
  }
  
  
  int <- as.data.frame(cbind(dataset, rowID = as.numeric(factor(apply(as.matrix(dataset[, tripID]), 1, idmaker)))))
  # int <- int[, c(colnames(sapply(dataindex[[varnameindex]], grepl, colnames(int))), 'rowID')]
  cat(length(unique(int$rowID)), "unique trips were identified using", tripID, "\n")
  # Handling of empty variables
  if (any(apply(int, 2, function(x) all(is.na(x))) == TRUE)) {
    int <- int[, -which(apply(int, 2, function(x) all(is.na(x))) == TRUE)]
  } else {
    int <- int
  }
  
  if (is_empty(weight.var)) {
    int$cent.lon <- stats::ave(int[[lon]], int[["rowID"]])
    int$cent.lat <- stats::ave(int[[lat]], int[["rowID"]])
  } else {
    # weighted centroid
    int$cent.lon <- stats::ave(int[c(lon, weight.var)], int[["rowID"]], FUN = function(x) stats::weighted.mean(x[[lon]], x[[weight.var]]))[[1]]
    int$cent.lat <- stats::ave(int[c(lat, weight.var)], int[["rowID"]], FUN = function(x) stats::weighted.mean(x[[lat]], x[[weight.var]]))[[1]]
  }
  
  create_trip_centroid_function <- list()
  create_trip_centroid_function$functionID <- "create_trip_centroid"
  create_trip_centroid_function$args <- list(dat, project, lon, lat, tripID, weight.var)
  create_trip_centroid_function$kwargs <- list()
  create_trip_centroid_function$output <- list(dat)
  log_call(project, create_trip_centroid_function)
  
  return(int)
  
}


#' Interactive application to create distance between points variable
create_dist_between <- function(dat, project, start, end, 
                                units = c("miles", "meters", "km", "midpoint"), 
                                zoneid=NULL, name = "distBetween") {
  #' @param dat Primary data frame over which to apply function.
  #'   Table in FishSET database should contain the string `MainDataTable`.
  #' @param project Project name. 
  #' @param start,end  Starting and ending location. Should be a port, lat/lon location, or the fishery management zone/area centroid.
  #'   or area. If port is desired, start should be the column name in the \code{dat} containing the port
  #'   Latitude and longitude for the port are extracted from the port table. If a lat/lon location is
  #'   desired then start should be a character string of column names from \code{dat}. The order must be lon,
  #'   lat. If fishery management centroid is used then set \code{start="centroid"} or \code{end="centroid"}.
  #'   \code{\link{find_centroid}} and \code{\link{assignment_column}} will be called to identify the latitude and longitude 
  #'   if the centroid table does not exist in the FishSET database.
  #' @param units  Unit of measurement for calculated distance between start and ending points.
  #'   Can be in \code{"miles"}, \code{"meters"}, \code{"kilometers"}, or \code{"midpoint"} location.
  #' @param zoneid Variable in \code{dat} that identifies the individual zones or areas. Define if exists in \code{dat} and is not names `ZoneID`.
  #' @param name String, output variable name. Defaults to `distBetween`.
  #' @export
  #' @return Returns primary data set with distance between variable.
  #' @importFrom geosphere distGeo midPoint
  #' @description Adds a variable for distance between two points to the primary dataset. There are two versions of this
  #'   function. The difference between the two versions is how additional arguments specific to start and end locations are added.
  #'   This version requires only five arguments to be specified before running. Additional arguments specific to identifying
  #'   the lat/lon of start or end points are added through prompts. This function is designed for an interactive session.
  #'   The \code{\link{create_dist_between_for_gui}} function requires all necessary arguments to be specified before running
  #'   and is best used in a non-interactive session. Both versions of the distance between function require that the start
  #'   and end points be different vectors. If the start or ending points are from a port then \code{PortTable} must be specified
  #'   to obtain lat/lons. If the start or ending points are the center of a fishing zone or area then \code{spat}, \code{lon.dat},
  #'   \code{lat.dat}, \code{cat}, \code{lon.spat}, and \code{lat.spat} must be specified to obtain latitude and longitude.
  #' @examples
  #' \dontrun{
  #' pollockMainDataTable <- create_dist_between(pollockMainDataTable, 'pollock', 'centroid',
  #'  'EMBARKED_PORT', units = 'miles', 'DistCentPort')
  #' 
  #' pollockMainDataTable <- create_dist_between(pollockMainDataTable, 'pollock', c('LonLat_START_LON',
  #'  'LonLat_START_LAT'), c('LonLat_END_LON','LonLat_END_LAT'), units='midpoint', 'DistLocLock')
  #'  
  #' pollockMainDataTable <- create_dist_between(pollockMainDataTable, 'pollock', 'DISEMBARKED_PORT',
  #'   'EMBARKED_PORT', units='meters', 'DistPortPort')
  #' }
  #' @details
  #' Additional arguments. \cr
  #' Further arguments are required to identify the latitude and longitude of the starting or ending location if \code{start} or \code{end}
  #' is defined as zonal centroid or a column from primary dataset containing port information, such as departing or embarking port.
  #' Prompts will appear asking for required arguments. \cr\cr
  #' Port arguments required:
  #' \tabular{rlll}{
  #' portTable: \tab Port table from FishSET database. Required if \code{start} or \code{end} is a port vector.
  #' }
  #' \cr\cr
  #' Centroids arguments required:
  #' \tabular{rlll}{
  #' spat: \tab Spatial data set containing information on fishery management or regulatory zones.
  #' Can be shape file, json, geojson, data frame, or list data frame or list. Required if \code{start} or \code{end} is centroid. \cr
  #' lon.dat: \tab Longitude variable from \code{dat}. \cr
  #' lat.dat: \tab Latitude variable from \code{dat}. \cr
  #' lon.spat: \tab Variable or list from \code{spat} containing longitude data. Required if \code{start} or \code{end} is centroid. Leave as NULL if \code{spat} is a shape or json file. \cr
  #' lat.spat: \tab Variable or list from \code{spat} containing latitude data. Required if \code{start} or \code{end} is centroid. Leave as NULL if \code{spat} is a shape or json file. \cr
  #' cat: \tab Variable or list in \code{spat} that identifies the individual areas or zones. If \code{spat} is class sf, \code{cat} should be the name of list containing information on zones.
  #' }
  
  
  # \tabular{AddPromptparams}{
  
  # head(create_dist_between(dat, 'pollock', 'centroid','EMBARKED_PORT', units='miles'))
  # head(create_dist_between(dat, 'pollock', c('LonLat_START_LON','LonLat_START_LAT'),c('LonLat_END_LON','LonLat_END_LAT'), units='midpoint'))
  # head(create_dist_between(dat, 'pollock', DISEMBARKED_PORT','EMBARKED_PORT', units='meters'))
  
  # Call in data sets
  if (start[1] == end[1]) {
    warning("Starting and ending vectors are identical.")
  } else {
    fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project))
    on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
    
    # Call in data sets
    out <- data_pull(dat, project)
    dataset <- out$dataset
    
    dat <- parse_data_name(dat, "main", project)
    
    # if(is_empty(name)){ "distBetween" } else {name}
    name <- name_check(dataset, name, repair = TRUE)
    
    vars <- NULL
    
    if (any(grepl("port", c(start[1], end[1]), ignore.case = TRUE))) {
      # in port table
      fun <- function() {
        readline("What is the table name in fishset_db containing port data?")
        # return(PortTable)
      }
      vars <- if (interactive()) {
        fun()
      }
      if (table_exists(gsub("'|\"", "", vars[1]), project) == FALSE) {
        print(DBI::dbListTables(fishset_db))
        stop(paste("PortTable", "not defined or does not exist. Consider using one of the tables listed above that exist in the database."))
      } else {
        port.table <- table_view(gsub("'|\"", "", vars[1]), project)
      }
    }
    
    x <- 0
    
    if (any(grepl("centroid", c(start[1], end[1]), ignore.case = TRUE))) {
      fun <- function() {
        spat <- readline("What is the name of the spatial data set? Can be shape file, data frame, or list?")
        long.spat <- readline("What is the name of the vector containing longitude of points from spatial data set?")
        lat.spat <- readline("What is the name of the vector containing latitude of points from spatial data set?")
        lon.dat <- readline("What is the name of the vector containing longitude of points from data set?")
        lat.dat <- readline("What is the name of the vector containing latitude of points from data set?")
        cat <- readline("What is the name of the vector defining the individual areas or zones from the spatial data set?")
        out <- c(spat, long.spat, lat.spat, lon.dat, lat.dat, cat)
      }
      vars <- if (interactive()) {
        fun()
      }
      
      ##Find centroid
      if(table_exists('spatCentroid', project)){
        int <- table_view('spatCentroid', project)
      } else {
        int <- find_centroid(spat = eval(parse(text = vars[1])), project=project, 
                             spatID = gsub("\"|'", "", vars[6]), lon.spat = gsub("\"|'", "", vars[2]),
                             lat.spat = gsub("\"|'", "", vars[3]), log.fun = FALSE)
      }
      
      
      ##Assignment column
      if("ZoneID" %in% names(dataset) == TRUE){
        zoneid <- 'ZoneID'
      } else if(!is.null(zoneid) & zoneid %in% names(dataset)){
        colnames(dataset)[colnames(dataset)==zoneid] <- 'ZoneID'
      } else {
        dataset <- assignment_column(
          dat = dataset, project = project, spat = eval(parse(text = vars[1])), hull.polygon = FALSE, lon.spat = gsub("\"|'", "", vars[2]),
          lat.spat = gsub("\"|'", "", vars[3]), lon.dat = gsub("\"|'", "", vars[4]), lat.dat = gsub("\"|'", "", vars[5]), cat = gsub(
            "\"|'", "",
            vars[6]
          ), closest.pt = TRUE, log.fun = FALSE
        )
      }
    }
    
    if (grepl("port", start[1], ignore.case = TRUE)) {
      start.lat <- as.numeric(sapply(trimws(dataset[[start]]), function(x) port.table[which(port.table[["Port_Name"]] == x), "Port_Lat"]))
      
      start.long <- as.numeric(sapply(trimws(dataset[[start]]), function(x) port.table[which(port.table[["Port_Name"]] == x), "Port_Long"]))
    } else if (start[1] == "centroid") {
      start.lat <- as.numeric(sapply(trimws(dataset[["ZoneID"]]), function(x) int[which(int[["ZoneID"]] == x), "cent.lat"]))
      start.long <- as.numeric(sapply(trimws(dataset[["ZoneID"]]), function(x) int[which(int[["ZoneID"]] == x), "cent.lon"]))
    } else {
      start.long <- dataset[[start[1]]]
      start.lat <- dataset[[start[2]]]
      
      if (any(abs(start.long) > 180)) {
        stop("Longitude is not valid (outside -180:180). Function not run")
      }
      if (any(abs(start.lat) > 90)) {
        stop("Latitude is not valid (outside -90:90. Function not run")
      }
    }
    
    if (grepl("port", end[1], ignore.case = TRUE)) {
      end.lat <- as.numeric(sapply(trimws(dataset[[end]]), function(x) port.table[which(port.table[["Port_Name"]] == x), "Port_Lat"]))
      end.long <- as.numeric(sapply(trimws(dataset[[end]]), function(x) port.table[which(port.table[["Port_Name"]] == x), "Port_Long"]))
    } else if (end[1] == "centroid") {
      end.lat <- as.numeric(sapply(trimws(dataset[["ZoneID"]]), function(x) int[which(int[["ZoneID"]] == x), "cent.lat"]))
      end.long <- as.numeric(sapply(trimws(dataset[["ZoneID"]]), function(x) int[which(int[["ZoneID"]] == x), "cent.lon"]))
    } else {
      end.lat <- dataset[[end[2]]]
      end.long <- dataset[[end[1]]]
      if (any(abs(end.long) > 180)) {
        stop("Longitude is not valid (outside -180:180). Function not run")
      }
      if (any(abs(end.lat) > 90)) {
        stop("Latitude is not valid (outside -90:90. Function not run")
      }
    }
    
    # Get distance between points
    if (units == "midpoint") {
      newvar <- geosphere::midPoint(cbind(start.long, start.lat), cbind(end.long, end.lat))
    } else {
      newvar <- geosphere::distGeo(cbind(start.long, start.lat), cbind(end.long, end.lat), a = 6378137, f = 1 / 298.257223563)
    }
    
    if (units == "miles") {
      newvar <- newvar * 0.000621371192237334
    } else if (units == "kilometers") {
      newvar <- newvar / 1000
    }
    
    g <- cbind(dataset, newvar)
    colnames(g)[dim(g)[2]] = name
    
    # Log the function
    create_dist_between_function <- list()
    create_dist_between_function$functionID <- "create_dist_between"
    create_dist_between_function$args <- list(dat, project, start, end, units, zoneid, name)
    create_dist_between_function$kwargs <- list(vars)
    create_dist_between_function$output <- list(dat)
    
    log_call(project, create_dist_between_function)
    return(g)
    
  }
}

## ---- Temporal  Variables ----##
#' Create duration of time variable
create_duration <- function(dat, project, start, end, 
                            units = c("week", "day", "hour", "minute"),
                            name = "create_duration") {
  #' @description  Create duration of time variable based on start and ending dates in desired temporal units.
  #' @param dat Primary data containing information on hauls or trips.
  #' Table in FishSET database contains the string 'MainDataTable'.
  #' @param project Project name.
  #' @param start Date variable from \code{dat} indicating start of time period.
  #' @param end Date variable from \code{dat} indicating end of time period.
  #' @param units String, unit of time for calculating duration. Must be \code{"week"}, \code{"day"}, \code{"hour"}, or \code{"minute"}.
  #' @param name String, name of created vector. Defaults to name of the function if not defined.
  #' @importFrom lubridate interval as.duration dweeks ddays dhours dminutes
  #' @export create_duration
  #' @return Returns primary dataset with duration of time variable added.
  #' @details Calculates the duration of time between two temporal variables based on defined time unit.
  #' The new variable is added to the dataset.
  #' A duration of time variable is required for other functions, such as \code{\link{cpue}}.
  #' @examples
  #' \dontrun{
  #' pollockMainDataTable <- create_duration(pollockMainDataTable, 'pollock', 'TRIP_START', 'TRIP_END',
  #'   units = 'minute', name = 'TripDur')
  #' }
  
  # Call in datasets
  out <- data_pull(dat, project = project)
  dataset <- out$dataset
  
  dat <- parse_data_name(dat, "main", project)
  
  # name <- if(is_empty(name)){ "create_duration" } else {name}
  name <- name_check(dataset, name, repair = TRUE)
  
  if (any(grepl("date|min|hour|week|month|TRIP_START|TRIP_END", start, ignore.case = TRUE)) == FALSE) {
    warning("Function is designed for temporal variables")
  }
  if (any(grepl("date|min|hour|week|month|TRIP_START|TRIP_END", end, ignore.case = TRUE)) == FALSE) {
    warning("Function is designed for temporal variables")
  }
  
  elapsed.time <- lubridate::interval(date_parser(dataset[[start]]), date_parser(dataset[[end]]))
  if (units == "week") {
    newvar <- lubridate::as.duration(elapsed.time) / lubridate::dweeks(1)
  } else if (units == "day") {
    newvar <- lubridate::as.duration(elapsed.time) / lubridate::ddays(1)
  } else if (units == "hour") {
    newvar <- lubridate::as.duration(elapsed.time) / lubridate::dhours(1)
  } else if (units == "minute") {
    newvar <- lubridate::as.duration(elapsed.time) / lubridate::dminutes(1)
  }
  
  g <- cbind(dataset, newvar)
  colnames(g)[dim(g)[2]] = name
  
  
  create_duration_function <- list()
  create_duration_function$functionID <- "create_duration"
  create_duration_function$args <- list(dat, project, start, end, units, name)
  create_duration_function$kwargs <- list()
  create_duration_function$output <- list(dat)
  log_call(project, create_duration_function)
  
  return(g)
}


# Confidentiality functions ----

randomize_value_row <- function(dat, project, value) {
  #'
  #' Randomize value between rows
  #'
  #' @param dat Primary data frame over which to apply function. Table in FishSET 
  #'   database should contain the string `MainDataTable`.
  #' @param project Project name. 
  #' @param value String, variable name to be randomly distributed between rows. 
  #' @export 
  #' @details This is one of the FishSET confidentiality functions. It is useful
  #'   for randomly assigning ID values between observations. 
  #' @examples
  #' \dontrun{
  #' randomize_value_row(pollockMainDataTable, "pollock", "PERMIT")
  #' }
  
  out <- data_pull(dat, project)
  dataset <- out$dataset
  
  dat <- parse_data_name(dat, "main", project)  
  
  dataset[[value]] <- sample(dataset[[value]], size = nrow(dataset), replace = FALSE)
  
  randomize_value_row_function <- list()
  randomize_value_row_function$functionID <- "randomize_value_row"
  randomize_value_row_function$args <- list(dat, project, value) 
  log_call(project, randomize_value_row_function)
  
  dataset
}


randomize_value_range <- function(dat, project, value, perc = NULL) {
  #' 
  #' Randomize variable value by percentage range
  #' 
  #' @param dat Primary data frame over which to apply function. Table in FishSET 
  #'   database should contain the string `MainDataTable`.
  #' @param project Project name. 
  #' @param value String, name of variable to jitter. 
  #' @param perc Numeric, a vector of percentages to randomly adjust a column of values by. 
  #'   Defaults to a range of 0.05 - 0.15 (i.e. 5-15 percent of original value). 
  #' @export
  #' @details This is one of the FishSET confidentiality functions. It adjusts a 
  #'   value by adding or substracting (chosen at random for each value) a percentage of the value. The percentage is
  #'   randomly sampled from a range of percentages provided in the "perc" argument. 
  #' @examples 
  #' \dontrun{
  #' randomize_value_range(pollockMainDataTable, "pollock", "LBS_270_POLLOCK_LBS")
  #' } 
  
  out <- data_pull(dat, project)
  dataset <- out$dataset
  
  dat <- parse_data_name(dat, "main", project)
  
  end <- FALSE
  
  if (!is.numeric(dataset[[value]])) {
    
    warning("Variable must be numeric.")
    end <- TRUE
  }
  
  if (end == FALSE) {
    
    if (is.null(perc)) perc <- seq(.05, .15, by = .01)
    
    r_val <- function(v, prc) {
      
      r_perc <- sample(prc, 1)
      add_sub <- sample(c("add", "subtract"), 1)
      
      if (add_sub == "add") {
        v <- v + (v * r_perc)
      } else {
        v <- v - (v * r_perc)
      }
      v
    }
    
    new_vals <- vapply(dataset[[value]], FUN = r_val, FUN.VALUE = numeric(1), prc = perc)
    
    if (is.integer(dataset[[value]])) new_vals <- round(new_vals) 
    
    dataset[[value]] <- new_vals
    
    randomize_value_range_function <- list()
    randomize_value_range_function$functionID <- "randomize_value_range"
    randomize_value_range_function$args <- list(dat, project, value, c(perc[1], perc[length(perc)])) 
    log_call(project, randomize_value_range_function)
    
    dataset
  }
}


jitter_lonlat <- function(dat, project, lon, lat, factor = 1, amount = NULL) {
  #'
  #' Jitter longitude and latitude variables
  #'
  #' @param dat Primary data frame over which to apply function. Table in FishSET 
  #'   database should contain the string `MainDataTable`.
  #' @param project Project name. 
  #' @param lon String, variable name containing longitude.
  #' @param lat String, variable name containing latitude.
  #' @param factor Numeric, see \code{\link[base]{jitter}} for details. 
  #' @param amount Numeric, see \code{\link[base]{jitter}} for details. 
  #'     Default (NULL): factor * d/5 where d is about the smallest difference between x values.
  #' @export
  #' @details This is one of the FishSET confidentiality functions. It "jitters" 
  #'   longitude and latitude using the base R function \code{\link[base]{jitter}}. 
  #' @examples 
  #' \dontrun{
  #' jitter_lonlat(pollockMainDataTable, "pollock",
  #'               lon = "LonLat_START_LON", lat = "LonLat_START_LAT")
  #' }
  
  out <- data_pull(dat, project)
  dataset <- out$dataset
  
  dat <- parse_data_name(dat, "main", project) 
  
  j_list <- 
    lapply(c(lon, lat), function(x) {
      
      jitter(dataset[[x]], factor = factor, amount = amount)
    })
  
  names(j_list) <- c(lon, lat)
  
  dataset[c(lon, lat)] <- as.data.frame.list(j_list)
  
  jitter_lonlat_function <- list()
  jitter_lonlat_function$functionID <- "jitter_lonlat"
  jitter_lonlat_function$args <- list(dat, project, lon, lat, factor, amount) 
  log_call(project, jitter_lonlat_function)
  
  dataset
}


randomize_lonlat_zone <- function(dat, project, spat, lon, lat, zone) {
  #' 
  #' Randomize latitude and longitude points by zone
  #' 
  #' @param dat Primary data frame over which to apply function. Table in FishSET 
  #'   database should contain the string `MainDataTable`.
  #' @param project Project name. 
  #' @param spat Spatial data table containing regulatory zones. This can be 
  #'   a "spatial feature" or sf object.
  #' @param lon String, variable name containing longitude.
  #' @param lat String, variable name containing latitude.
  #' @param zone String, column name contain the assigned zone. Must be the same 
  #'   for both the spatial data table and MainDataTable. 
  #' @export
  #' @importFrom dplyr across arrange count %>% 
  #' @importFrom sf st_coordinates st_sample st_as_sf
  #' @details This is one of the FishSET confidentiality functions. It replaces 
  #'   longitude and latitude values with randomly sampled coordinates from the
  #'   regulatory zone the observation occurred in. 
  #' @examples 
  #' \dontrun{
  #' randomize_lonlat_zone(pollockMainDataTable, "pollock", spatdat, 
  #'                    lon = "LonLat_START_LON", lat = "LonLat_START_LAT",
  #'                    zone = "NMFS_AREA")
  #' }
  
  out <- data_pull(dat, project)
  dataset <- out$dataset
  
  dat <- parse_data_name(dat, "main", project)
  
  spat_out <- data_pull(spat, project)
  spatdat <- spat_out$dataset
  
  spat <- parse_data_name(spat, 'spat', project)
  
  
  if (!("sf" %in% class(spatdat))) {
    spatdat <- sf::st_as_sf(x = spatdat, crs = "+proj=longlat +datum=WGS84")
  }
  
  # arrange spatdat and dataset by zone
  spatdat <- 
    spatdat %>% 
    dplyr::arrange(dplyr::across(zone))
  
  temp_row_id <- NA
  
  # temporary row id to preserve order
  dataset$temp_row_id <- 1:nrow(dataset)
  
  dataset <- 
    dataset %>% 
    dplyr::arrange(dplyr::across(zone))
  
  # count the n_obs in each zone
  zone_tab <- 
    dataset %>% 
    dplyr::count(dplyr::across(zone)) 
  
  # index of which zones have obs
  z_ind <- which(spatdat[[zone]] %in% zone_tab[[zone]]) 
  
  # empty vector to be filled by zone sample size
  # st_sample's size arg accepts a vector (sample size for each polygon)
  size_vec <- rep(0, length(spatdat[[zone]]))
  
  # add sample size from zone_tab to size_vec, positioned by zone index
  for (i in 1:nrow(zone_tab)) {
    
    size_vec[z_ind[i]] <- zone_tab$n[i]
  }
  
  # generate random lonlat points 
  rm_pnts <- sf::st_sample(spatdat, size = size_vec, type = "random")
  
  # fetch coordinates (matrix, array)
  pts <- sf::st_coordinates(rm_pnts)
  
  colnames(pts) <- c(lon, lat)
  
  pts <- as.data.frame(pts)
  
  # create zone id vector
  z_id <- rep(spatdat[[zone]], times = size_vec)
  
  pts[[zone]] <- z_id
  
  dataset[c(lon, lat)] <- pts[c(lon, lat)]
  
  dataset <- dataset %>% dplyr::arrange(temp_row_id)
  dataset$temp_row_id <- NULL
  
  randomize_lonlat_zone_function <- list()
  randomize_lonlat_zone_function$functionID <- "randomize_lonlat_zone"
  randomize_lonlat_zone_function$args <- list(dat, project, spat, lon, lat, zone) 
  log_call(project, randomize_lonlat_zone_function)
  
  dataset
}


lonlat_to_centroid <- function(dat, project, lon, lat, spat, zone) {
  #'
  #' Assign longitude and latitude points to zonal centroid
  #' 
  #' @param dat Primary data frame over which to apply function. Table in FishSET 
  #'   database should contain the string `MainDataTable`.
  #' @param project Project name. 
  #' @param lon String, variable name containing longitude.
  #' @param lat String, variable name containing latitude.
  #' @param spat Spatial data table containing regulatory zones. This can be 
  #'   a "spatial feature" or sf object. 
  #' @param zone String, column name contain the assigned zone. Must be the same 
  #'   for both the spatial data table and MainDataTable. 
  #' @export
  #' @importFrom dplyr left_join
  #' @importFrom sf st_as_sf
  #' @details This is one of the FishSET confidentiality functions. It replaces the 
  #'   selected longitude and latitude columns with the zonal centroid derived 
  #'   from a spatial data table. 
  #' @examples 
  #' \dontrun{
  #' lonlat_to_centroid(pollockMainDataTable, "pollock", spatdat, 
  #'                   lon = "LonLat_START_LON", lat = "LonLat_START_LAT",
  #'                   zone = "NMFS_AREA")
  #' }
  #' 
  
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  spat_out <- data_pull(spat, project)
  spat <- spat_out$dat
  spatdat <- parse_data_name(spat, 'spat', project)
  
  # TODO: update with spatial checks, check_spatdat()
  if (!("sf" %in% class(spatdat))) {
    
    spatdat <- sf::st_as_sf(x = spatdat, crs = 4326) #WGS 84 default
  }
  
  pts <- find_centroid(spat = spatdat, project=project, spatID = zone, log.fun = FALSE)
  
  colnames(pts) <- c(zone, lon, lat)
  
  c_names <- colnames(dataset)
  
  dataset[c(lon, lat)] <- NULL
  
  dataset <- dplyr::left_join(dataset, pts, by = zone)
  
  dataset <- dataset[, c_names] # preserve original column order
  
  lonlat_to_centroid_function <- list()
  lonlat_to_centroid_function$functionID <- "lonlat_to_centroid"
  lonlat_to_centroid_function$args <- list(dat, project, spat, lon, lat, zone) 
  log_call(project, lonlat_to_centroid_function)
  
  dataset
}

