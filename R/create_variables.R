# Create variables or matrix.

## --- CPUE ----##
#' Create catch per unit effort varaiable
#'
#' Add catch per unit effort variable to the primary dataset. Catch should be a weight variable 
#' but can be a count. Effort should be in a duration of time, such as days, hours, or minutes.
#'
cpue <- function(dat, xWeight, xTime, name = "cpue") {
  #' @param dat Primary data containing information on hauls or trips.
  #'   Table in FishSET database contains the string 'MainDataTable'.
  #' @param xWeight Catch variable in \code{dat}. Variable should be a measure of weight 
  #'   (pounds, metric tons, etc) but can also be count.
  #' @param xTime Duration of time variable in \code{dat} representing effort, such as
  #'   weeks, days, hours, or minutes.
  #' @param name String, name of created variable. Defaults to name of the function if not defined.
  #' @export cpue
  #' @details Creates the catch per unit effort variable. Catch variable should be in weight (lbs, mts).
  #'   Effort variable should be a measurement of duration in time. New variable is added to the primary dataset
  #'   with the column name defined by the \code{name} argument. CPUE for individual species should be
  #'   calculated separately.
  #' @return Returns primary dataset with CPUE variable added.
  #' @examples
  #' \dontrun{
  #' pollockMainDataTable <- cpue(pollockMainDataTable, 'OFFICIAL_TOTAL_CATCH_MT', 
  #'     'DURATION_IN_MIN', 'cpue')
  #' }

  # Call in datasets
  dataset <- dat
  dat <- deparse(substitute(dat))

  tmp <- 0

  if (!is.numeric(dataset[[xTime]]) | !is.numeric(dataset[[xWeight]])) {
    tmp <- 1
    warning("Data must be numeric. CPUE not calculated")
  }

  if (tmp == 0) {
    # Check that Weight variable is indeed a weight variable
    if (!grepl("Duration", xTime, ignore.case = TRUE)) {
      warning("xTime should be a measurement of time. Use the create_duration function. CPUE calculated.")
    }
    if (!grepl("LB|Pounds|MT", xWeight, ignore.case = TRUE)) {
      warning("xWeight must a measurement of mass. CPUE calculated.")
    }

    name <- dataset[[xWeight]] / dataset[[xTime]]

    create_var_cpue_function <- list()
    create_var_cpue_function$functionID <- "cpue"
    create_var_cpue_function$args <- list(dat, xWeight, xTime, deparse(substitute(name)))
    create_var_cpue_function$kwargs <- list()
    create_var_cpue_function$output <- list(dat)

    log_call(create_var_cpue_function)
    return(cbind(dataset, name))
  }
}

## ---- Dummy  Variables ----##
# dummy_num
dummy_num <- function(dat, var, value, opts = "more_less", name = "dummy_num") {
  #' Create a dummy vector from numeric characterization of variable
  #' @param dat Primary data containing information on hauls or trips.
  #' Table in FishSET database contains the string 'MainDataTable'.
  #' @param var Variable in \code{dat} to create dummy variable from.
  #' @param value String, value to set dummy variable by. If \code{var} is a date date, value should be a year,
  #' If \code{var} is a factor, value should be a factor level. If \code{var} is numeric, value should be a single
  #' number or range of numbers [use c(1,5)].
  #' @param opts String, how dummy variable should be defined. Choices are \code{"x_y"} and \code{"more_less’"}. For \code{"x_y"}, each
  #'   element of \code{var} is set to 1 if the elemant matches \code{value}, otherwise 0.
  #'   For \code{"more_less"}, each element of \code{var} less than \code{value} is set to 0 and all elements greater than
  #'   \code{varlue} set to 1. If \code{var} is a factor, then elements that match value will be set to 1 and all other
  #'   elements set to 0. Default is set to \code{"more_less"}.
  #' @param name String, name of created dummy variable. Defaults to name of the function if not defined.
  #' @details For date variables, the dummy variable is defined by a date (year) and may be either year \code{x} versus all
  #'   other years (\code{"x_y"}) or before vs after year \code{x} (\code{"more_less"}). Use this function to create a variable defining whether
  #'   or not a policy action had been implemented. \cr
  #'   Example: before vs. after a 2008 ammendment: \cr
  #'   ‘dummy_num(’MainDataTable', 'Haul_date', 2008, 'more_less', 'ammend08')' \cr\cr
  #'
  #'  For factor variables, both choices in \code{opts} compare selected factor level(s) against all other factor levels.\cr
  #'  Example: Fishers targeting pollock vs. another species:  \cr
  #'  ‘dummy_num(’MainDataTable', 'GF_TARGET_FT', c('Pollock - bottom', 'Pollock - midwater'), 'x_y', 'pollock_target')'  \cr\cr
  #'
  #'  For numeric variables, \code{value} can be a single number or a range of numbers. The dummy variable is the
  #'  selected value(s) against all others (\code{x_y}) or less than the selected value versus more than the selected value
  #'  (\code{more_less}). For \code{more_less}, the mean is used as the critical value if a range of values is provided.
  #' @return Returns primary dataset with dummy variable added.
  #' @export
  #' @examples
  #' \dontrun{
  #' MainDataTable <- dummy_num(pollockMainDataTable, 'Haul_date', 2008, 'more_less', 'ammend80')
  #' }


  # Pull in data
  dataset <- dat
  dat <- deparse(substitute(dat))

  if (grepl("dat|year", var, ignore.case = TRUE)) {
    if (length(value) == 6) {
      dataset[[var]] <- format(as.Date(dataset[[var]]), "%Y%m")
    } else if (length(value) == 4) {
      dataset[[var]] <- format(as.Date(dataset[[var]]), "%Y")
    } else {
      dataset[[var]] <- format(as.Date(dataset[[var]]), "%m")
    }
  } else if (is.numeric(dataset[[var]])) {
    if (opts == "x_y") {
      name <- ifelse(dataset[[var]] >= min(value) & dataset[[var]] <= max(value), 0, 1)
    } else {
      name <- ifelse(dataset[[var]] < mean(value), 0, 1)
    }
  } else if (is.factor(dataset[[var]]) | is.character(dataset[[var]])) {
    name <- ifelse(trimws(dataset[[var]], "both") == trimws(value, "both"), 0, 1)
  } else {
    (warning("variable is not recognized as being a date, factor, or numeric. Function not run."))
  }

  create_var_dummy_num_function <- list()
  create_var_dummy_num_function$functionID <- "dummy_num"
  create_var_dummy_num_function$args <- list(dat, var, value, opts, deparse(substitute(name)))
  create_var_dummy_num_function$kwargs <- list()
  create_var_dummy_num_function$output <- list(dat)

  log_call(create_var_dummy_num_function)
  return(cbind(dataset, name))
}

#' Create dummy variable
dummy_var <- function(dat, DumFill = "TRUE", name = "dummy_var") {
  #' @param dat Primary data containing information on hauls or trips.
  #' Table in FishSET database contains the string 'MainDataTable'.
  #' @param DumFill Fill the dummy variable with TRUE or FALSE
  #' @param name String, name of created dummy variable. Defaults to name of the function if not defined.
  #' @return Primary dataset with dummy variable added.
  #' @export dummy_var
  #' @details Creates a dummy variable of either FALSE or TRUE with length of the number of rows of the data set.
  #' @examples
  #' \dontrun{
  #' pollockMainDataTable <- dummy_var(pollockMainDataTable, DumFill=TRUE, 'dummyvar')
  #' }

  # Pull in data
  dataset <- dat
  dat <- deparse(substitute(dat))

  name <- as.vector(rep(DumFill, nrow(dataset)))

  create_var_dummy_var_function <- list()
  create_var_dummy_var_function$functionID <- "dummy_var"
  create_var_dummy_var_function$args <- list(dat, DumFill, deparse(substitute(name)))
  create_var_dummy_var_function$kwargs <- list()
  create_var_dummy_var_function$output <- list(dat)

  log_call(create_var_dummy_var_function)

  return(cbind(dataset, name))
}

#' Create dummy matrix from a coded ID variable
dummy_matrix <- function(dat, x) {
  #' @param dat Primary data containing information on hauls or trips.
  #' Table in FishSET database contains the string 'MainDataTable'.
  #' @param x Variable in \code{dat} used to generate dummy matrix.
  #' @export dummy_matrix
  #' @details Creates a dummy matrix of TRUE/FALSE with dimensions \emph{[(number of observations in dataset) x
  #' (number of factors in x)]} where each column is a unique factor level. Values are TRUE if the value in the
  #' column matches the column factor level and FALSE otherwise.
  #' @examples
  #' \dontrun{
  #' PortMatrix <- dummy_matrix(pollockMainDataTable, 'PORT_CODE')
  #' }


  dataset <- dat
  dat <- deparse(substitute(dat))

  # create the matrix
  factor.levels <- levels(as.factor(dataset[[x]]))
  int <- data.frame(matrix(rep(dataset[[x]], length(factor.levels)), ncol = length(factor.levels)))
  colnames(int) <- factor.levels

  # change matrix to TRUE/FALSE
  int <- data.frame(lapply(1:length(factor.levels), function(x) ifelse(int[, x] == colnames(int)[x], TRUE, FALSE)))
  colnames(int) <- paste(x, "_", levels(as.factor(dataset[[x]])))

  create_var_dummy_matrix_function <- list()
  create_var_dummy_matrix_function$functionID <- "dummy_matrix"
  create_var_dummy_matrix_function$args <- list(dat, x)
  create_var_dummy_matrix_function$kwargs <- list()
  create_var_dummy_matrix_function$output <- list()
  log_call(create_var_dummy_matrix_function)

  return(int)
}

## ---- Coded variables ----##
#' Create factor variable from quantiles
#'
#' Create a factor variable from numeric data.  Numeric variable is split into categories based on quantile categories.
#'
set_quants <- function(dat, x, quant.cat = c(0.2, 0.25, 0.4), name = "set_quants") {
  #' @param dat Primary data containing information on hauls or trips.
  #' Table in FishSET database contains the string 'MainDataTable'.
  #' @param x Variable to transform into quantiles.
  #' @param quant.cat Quantile options: \code{"0.2"}, \code{"0.25"}, and \code{"0.4"}
  #' \itemize{
  #'   \item{.2:  (0\%, 20\%, 40\%, 60\%, 80\%, 100\%)}
  #'   \item{.25: (0\%, 25\%, 50\%, 75\%, 100\%)}
  #'   \item{.4:  (0\%, 10\%, 50\%, 90\%, 100\%)}
  #'   }
  #' @param name String, name of created vector. Defaults to name of the function if not defined.
  #' @return Primary dataset with quantile variable added.
  #' @export set_quants
  #'
  #' @examples
  #' \dontrun{
  #' pollockMainDataTable <- set_quants(pollockMainDataTable, 'HAUL', quant.cat=.2, 'haul.quant')
  #' }
  #

  dataset <- dat
  dat <- deparse(substitute(dat))

  tmp <- 0

  if (!is.numeric(dataset[[x]])) {
    tmp <- 1
    warning("Variable must be numeric. Function not run.")
  }

  if (tmp == 0) {
    if (quant.cat == 0.2) {
      prob.def <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
    } else if (quant.cat == 0.25) {
      prob.def <- c(0, 0.25, 0.5, 0.75, 1)
    } else if (quant.cat == 0.4) {
      prob.def <- c(0, 0.1, 0.5, 0.9, 1)
    }
    # var.name <- paste('TRIP_OTC_MT', 'quantile', sep = '.')
    name <- as.integer(cut(dataset[[x]], quantile(dataset[[x]], probs = prob.def), include.lowest = TRUE))

    create_var_set_quants_function <- list()
    create_var_set_quants_function$functionID <- "set_quants"
    create_var_set_quants_function$args <- list(dat, x, quant.cat, deparse(substitute(name)))
    create_var_set_quants_function$kwargs <- list()
    create_var_set_quants_function$output <- list(dat)

    log_call(create_var_set_quants_function)
    return(cbind(dataset, name))
  }
}


bin_var <- function(dat, project, var, br, name, labs = NULL, ...) {
  #'
  #' Wrapper for \code{\link{cut}}
  #'
  #' @param dat Primary data containing information on hauls or trips. Table in FishSET database contains the string 'MainDataTable'.
  #' @param project String, name of project.
  #' @param var Numeric variable in \code{dat} to bin into a factor.
  #' @param br Numeric vector. If a single number, the range of \code{var} is divided into \code{br} even groups. If two or more values are given, \code{var} is divided into intervals.
  #' @param name Variable name to return. Defaults to `bin`.
  #' @param labs A character string of category labels.
  #' @param ... Additional arguments passed to \code{\link{cut}}.
  #' @details Function adds a new factor variable, labeled by name, to the primary dataset. The numeric variable is divided into equal sized groups if the length of \code{br} is one and into intervals if the length of \code{br} is greater than one.
  #' @export
  #' @return Returns the primary dataset with binned variable added.
  #' @examples
  #' \dontrun{
  #'  pollockMainDataTable <- bin_var(pollockMainDataTable, 'pollock', 'HAUL', 10, 'HAULCAT')
  #'  pollockMainDataTable <- bin_var(pollockMainDataTable, 'pollock', 'HAUL', c(5,10), 'HAULCAT')
  #' }

  dataset <- dat
  dat <- deparse(substitute(dat))

  tmp <- 0

  if (!is.numeric(dataset[[var]])) {
    tmp <- 1
    warning("Variable must be numeric.")
  }

  if (tmp == 0) {
    name <- cut(dataset[[var]], breaks = br, labels = labs, ...)

    # Log function
    bin_var_function <- list()
    bin_var_function$functionID <- "bin_var"
    bin_var_function$args <- c(dat, project, var, project, br, deparse(substitute(name)), labs)
    bin_var_function$kwargs <- list()
    bin_var_function$output <- list(dat)
    log_call(bin_var_function)

    return(cbind(dataset, name))
  }
}


## ---- Numeric  Variables ----##
#' Create numeric variable using arithmetic expression
create_var_num <- function(dat, x, y, method, name = "create_var_num") {
  #' @description Creates a new variable based on the arithmetic operation between two variables.  Function is useful for creating rate variables or the summation of two related variables.
  #' @param dat Primary data containing information on hauls or trips.
  #' Table in FishSET database contains the string 'MainDataTable'.
  #' @param x Variable in \code{dat}. Variable will be the numerator if \code{method} is division.
  #' @param y Variable  in \code{dat}. Variable will be the denominator if \code{method} is division.
  #' @param method String, arithmetic expression. Options include: \code{"sum"}, addition (\code{"add"}),
  #' subtraction (\code{"sub"}), multiplication (\code{"mult"}), and division (\code{"div"}).
  #' @param name String, name of created vector. Defaults to name of the function if not defined.
  #' @export create_var_num
  #' @details Creates a new numeric variable based on defined arithmetic expression \code{method}. New variable is added to the primary dataset.
  #' @return Returns primary dataset with new variable added.
  #' @examples
  #' \dontrun{
  #' pollockMainDataTable <- create_var_num(pollockMainDataTable, x='HAUL_CHINOOK',
  #' y='HAUL_CHUM', method='sum', name='tot_salmon')
  #' }

  dataset <- dat
  dat <- deparse(substitute(dat))

  tmp <- 0

  if (is.numeric(dataset[[x]]) == FALSE | is.numeric(dataset[[y]]) == FALSE) {
    tmp <- 1
    warning("Variables must be numeric")
  }

  if (tmp == 0) {
    if (grepl("add|sum", method, ignore.case = TRUE)) {
      name <- dataset[[x]] + dataset[[y]]
    } else if (grepl("sub", method, ignore.case = TRUE)) {
      name <- dataset[[x]] - dataset[[y]]
    } else if (grepl("mult", method, ignore.case = TRUE)) {
      name <- dataset[[x]] * dataset[[y]]
    } else if (grepl("div", method, ignore.case = TRUE)) {
      name <- dataset[[x]] / dataset[[y]]
    }

    create_var_num_function <- list()
    create_var_num_function$functionID <- "create_var_num"
    create_var_num_function$args <- list(dat, x, y, method, deparse(substitute(name)))
    create_var_num_function$kwargs <- list()
    create_var_num_function$output <- list(dat)
    log_call(create_var_num_function)

    return(cbind(dataset, name))
  }
}

## ---- Spatial  Variables ----##
#' Calculate haul midpoint latitude and longitude variables
create_mid_haul <- function(dat, start = c("lon", "lat"), end = c("lon", "lat"), name = "mid_haul") {
  #' @description Calculate latitude and longitude of the haul midpoint and adds two variables
  #' to the primary dataset, the midpoint latitude and the midpoint longitude.
  #'
  #' @param dat Primary data containing information on hauls or trips.
  #' Table in FishSET database contains the string 'MainDataTable'.
  #' @param start Character string, variables in \code{dat} defining the longitude and latitude of
  #' the starting location of haul. Must be in decimal degrees.
  #' @param end Character string, variables in \code{dat} defining the longitude and latitude of the
  #' ending location of haul.  Must be in decimal degrees.
  #' @param name String, name of new variable. Defaults to `mid_haul`.
  #' @details Each row of data must be a unique haul. Requires a start and end point for each observations.
  #' @return Returns primary dataset with two new variables added, latitude and longitude of haul midpoint.
  #' @importFrom geosphere distGeo midPoint
  #' @export
  #' @examples
  #' \dontrun{
  #' pollockMainDataTable <- create_mid_haul(pollockMainDataTable, start = c('LonLat_START_LON',
  #' 'LonLat_START_LAT'), end = c('LonLat_END_LON', 'LonLat_END_LAT'), name='mid_haul')
  #' }
  #

  dataset <- dat
  dat <- deparse(substitute(dat))

  tmp <- 0

  if (is_empty(start) || is_empty(end)) {
    tmp <- 1
    warning("Starting and end locations must both be specified. Function not run.")
  }

  if (dim(dataset[, c(start)])[1] != dim(dataset[, c(end)])[1]) {
    tmp <- 1
    warning("Starting and ending locations are of different lengths. Function not run.")
  }

  if (any(abs(dataset[, c(start)][1]) > 180) | any(abs(dataset[, c(end)][1]) > 180)) {
    warning("Longitude is not valid (outside -180:180). Function not run")
    # stop('Longitude is not valid (outside -180:180.')
    tmp <- 1
  }
  if (any(abs(dataset[, c(start)][2]) > 90) | any(abs(dataset[, c(end)][2]) > 90)) {
    warning("Latitude is not valid (outside -90:90. Function not run")
    tmp <- 1
    # stop('Latitude is not valid (outside -90:90.')
  }

  if (tmp == 0) {
    distBetween <- geosphere::midPoint(dataset[, c(start)], dataset[, c(end)])
    colnames(distBetween) <- c(paste0(name, "Lon"), paste0(name, "Lat"))
    out <- cbind(dataset, distBetween)

    create_mid_haul_function <- list()
    create_mid_haul_function$functionID <- "create_mid_haul"
    create_mid_haul_function$args <- list(dat, start, end, name)
    create_mid_haul_function$kwargs <- list()
    create_mid_haul_function$output <- list(dat)
    log_call(create_mid_haul_function)

    return(out)
  }
}

create_trip_centroid <- function(dat, lon, lat, weight.var = NULL, ...) {
  ## ----trip centroid-----#
  #' Create trip centroid variable
  #'
  #' Create latitude and longitude variables containing the centroid of each trip
  #'
  #' @param dat Primary data containing information on hauls or trips. Table in FishSET database contains the string 'MainDataTable'.
  #' @param lat Variable in \code{dat} containing latitudinal data.
  #' @param lon Variable in \code{dat} containing longitudinal data.
  #' @param weight.var Variable in \code{dat} for computing the weighted average.
  #' @param ... Optional, one or more variable in \code{dat} that identify individual trips. If not defined, each row is treated as a unique trip.
  #' @details Computes the average longitude and latitude for each trip. Centroid can be weighted using the \code{weight.var}.
  #' Additional arguments can be added that define unique trips. If no additional arguments are added, each row will be treated as a unique trip.
  #' @return Returns the primary dataset with centroid latitude and centroid longitude variables added.
  #' @importFrom geosphere distGeo midPoint
  #' @export
  #' @examples
  #' \dontrun{
  #' pollockMainDataTable <- create_trip_centroid(pollockMainDataTable, 'LonLat_START_LON', 
  #'   'LonLat_START_LAT', weight.var=NULL, 'DISEMBARKED_PORT', 'EMBARKED_PORT')
  #' }

  dataset <- dat
  dat <- deparse(substitute(dat))

  x <- 0
  if (any(abs(dataset[[lon]]) > 180)) {
    warning("Longitude is not valid (outside -180:180). Function not run")
    # stop('Longitude is not valid (outside -180:180.')
    x <- 1
  }
  if (any(abs(dataset[[lat]]) > 90)) {
    warning("Latitude is not valid (outside -90:90. Function not run")
    x <- 1
    # stop('Latitude is not valid (outside -90:90.')
  }

  if (x == 1) {
    if (grepl("input", as.character(match.call(expand.dots = FALSE)$...)[1]) == TRUE) {
      argList <- eval(...)
    } else {
      argList <- (as.character(match.call(expand.dots = FALSE)$...))
    }

    idmaker <- function(vec) {
      return(paste(sort(vec), collapse = ""))
    }

    int <- as.data.frame(cbind(dataset, rowID = as.numeric(factor(apply(as.matrix(dataset[, eval(substitute(argList))]), 1, idmaker)))))
    # int <- int[, c(colnames(sapply(dataindex[[varnameindex]], grepl, colnames(int))), 'rowID')]
    cat(length(unique(int$rowID)), "unique trips were identified using", argList, "\n")
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
    create_trip_centroid_function$args <- list(dat, dat, lon, lat, weight.var, argList)
    create_trip_centroid_function$kwargs <- list()
    create_trip_centroid_function$output <- list(dat)
    log_call(create_trip_centroid_function)

    return(int)
  }
}


#' Create distance between points variable - interactive version
create_dist_between <- function(dat, start, end, units = c("miles", "meters", "km", "midpoint"), name = "distBetween") {
  #' @param dat Main data frame over which to apply function.
  #' Table in FishSET database should contain the string `MainDataTable`.
  #' @param start Starting location. Should be a port, lat/lon location, or the centroid of fishing zone
  #' or area. If port is desired, start should be the column name in the \code{dat} containing the port
  #' names. Latitude and longitude for the port are extracted from the port table. If a lat/lon location is
  #' desired then start should be a character string of column names from \code{dat}. The order must be lon,
  #' lat. If the centroid of the fishing zone or area is to be used then \code{start} should be \code{"centroid"} and
  #' \code{\link{find_centroid}} and \code{\link{assignment_column}} will be called to identify the latitude and longitude.
  #' @param end Ending location. Should be a port, lat/lon location, or the centroid of the fishing zone or area.
  #' If port is desired, end should be a variable in \code{dat} containing the port names. Latitude
  #' and longitude for the port are extracted from the port table. If a lat, long location is desired then end
  #' should be a character string of column names specifying first longitude then latitude. If the centroid of the
  #' fishing zone or area is to be used then \code{end} should be \code{"centroid"} and \code{\link{find_centroid}} and
  #' \code{\link{assignment_column}} will be called to identify the latitude and longitude.
  #' @param units  Unit of measurement for calculated distance between start and ending points.
  #' Can be in \code{"miles"}, \code{"meters"}, \code{"kilometers"}, or \code{"midpoint"} location.
  #' @param name String, output variable name. Defaults to `distBetween`.
  #' @export
  #' @return Returns primary dataset with distance between variable.
  #' @importFrom geosphere distGeo midPoint
  #' @description Adds distance between two points variable to the primary dataset. There are two versions of this
  #'   function. The difference between the two versions is how additional arguments specific to start and end locations are added.
  #'   This version requires only five arguments to be specified before running. Additional arguments specific to identifying
  #'   the lat/lon of start or end points are added through prompts. This function is designed for an interactive session.
  #'   The \code{\link{create_dist_between_for_gui}} function requires all necessary arguments to be specified before running
  #'   and is best used in a non-interactive session. Both versions of the distance between function require that the start
  #'   and end points be different vectors. If the start or ending points are from a port then \code{PortTable} must be specified
  #'   to obtain lat/lons. If the start or ending points are the center of a fishing zone or area then \code{gridfile}, \code{lon.dat},
  #'   \code{lat.dat}, \code{cat}, \code{lon.grid}, and \code{lat.grid} must be specified to obtain latitude and longitude.
  #' @examples
  #' \dontrun{
  #' pollockMainDataTable <- create_dist_between(pollockMainDataTable, 'centroid',
  #'  'EMBARKED_PORT', units = 'miles', 'DistCentPort')
  #' 
  #' pollockMainDataTable <- create_dist_between(pollockMainDataTable, c('LonLat_START_LON',
  #'  'LonLat_START_LAT'), c('LonLat_END_LON','LonLat_END_LAT'), units='midpoint', 'DistLocLock')
  #'  
  #' pollockMainDataTable <- create_dist_between(pollockMainDataTable,'DISEMBARKED_PORT',
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
  #' gridfile: \tab Spatial data set containing information on fishery management or regulatory zones.
  #' Can be shape file, json, geojson, data frame, or list data frame or list. Required if \code{start} or \code{end} is centroid. \cr
  #' lon.dat: \tab Longitude variable from \code{dat}. \cr
  #' lat.dat: \tab Latitude variable from \code{dat}. \cr
  #' lon.grid: \tab Variable or list from \code{gridfile} containing longitude data. Required if \code{start} or \code{end} is centroid. Leave as NULL if \code{gridfile} is a shape or json file. \cr
  #' lat.grid: \tab Variable or list from \code{gridfile} containing latitude data. Required if \code{start} or \code{end} is centroid. Leave as NULL if \code{gridfile} is a shape or json file. \cr
  #' cat: \tab Variable or list in \code{gridfile} that identifies the individual areas or zones. If \code{gridfile} is class sf, \code{cat} should be the name of list containing information on zones.
  #' }


  # \tabular{AddPromptparams}{

  # head(create_dist_between(dat,'centroid','EMBARKED_PORT', units='miles'))
  # head(create_dist_between(dat,c('LonLat_START_LON','LonLat_START_LAT'),c('LonLat_END_LON','LonLat_END_LAT'), units='midpoint'))
  # head(create_dist_between(dat,'DISEMBARKED_PORT','EMBARKED_PORT', units='meters'))

  # Call in datasets
  if (start[1] == end[1]) {
    warning("Starting and ending vectors are identical.")
  } else {
    fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase())

    # Call in datasets
    dataset <- dat
    dat <- deparse(substitute(dat))

    if (any(grepl("port", c(start[1], end[1]), ignore.case = TRUE))) {
      # in port table
      fun <- function() {
        readline("What is the table name in fishset_db containing port data?")
        # return(PortTable)
      }
      vars <- if (interactive()) {
        fun()
      }
      if (table_exists(gsub("'|\"", "", vars[1])) == FALSE) {
        print(DBI::dbListTables(fishset_db))
        stop(paste("PortTable", "not defined or does not exist. Consider using one of the tables listed above that exist in the database."))
      } else {
        port.table <- table_view(gsub("'|\"", "", vars[1]))
      }
    }
    DBI::dbDisconnect(fishset_db)

    x <- 0

    if (any(grepl("centroid", c(start[1], end[1]), ignore.case = TRUE))) {
      fun <- function() {
        gridfile <- readline("What is the name of the spatial data set? Can be shape file, data frame, or list?")
        long.grid <- readline("What is the name of the vector containing longitude of points from spatial data set?")
        lat.grid <- readline("What is the name of the vector containing latitude of points from spatial data set?")
        lon.dat <- readline("What is the name of the vector containing longitude of points from data set?")
        lat.dat <- readline("What is the name of the vector containing latitude of points from data set?")
        cat <- readline("What is the name of the vector defining the individual areas or zones from the spatial data set?")
        out <- c(gridfile, long.grid, lat.grid, lon.dat, lat.dat, cat)
      }
      vars <- if (interactive()) {
        fun()
      }

      dataset <- assignment_column(
        dat = dataset, gridfile = eval(parse(text = vars[1])), hull.polygon = FALSE, lon.grid = gsub("\"|'", "", vars[2]),
        lat.grid = gsub("\"|'", "", vars[3]), lon.dat = gsub("\"|'", "", vars[4]), lat.dat = gsub("\"|'", "", vars[5]), cat = gsub(
          "\"|'", "",
          vars[6]
        ), closest.pt = TRUE
      )
      int <- find_centroid(dataset, gridfile = eval(parse(text = vars[1])), lon.grid == gsub("\"|'", "", vars[2]), lat.grid == gsub(
        "\"|'", "",
        vars[3]
      ), lon.dat = gsub("\"|'", "", vars[4]), lat.dat = gsub("\"|'", "", vars[5]), cat = gsub("\"|'", "", vars[6]), weight.var = NULL)
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
        warning("Longitude is not valid (outside -180:180). Function not run")
        x <- 1
      }
      if (any(abs(start.lat) > 90)) {
        warning("Latitude is not valid (outside -90:90. Function not run")
        x <- 1
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
        warning("Longitude is not valid (outside -180:180). Function not run")
        x <- 1
      }
      if (any(abs(end.lat) > 90)) {
        warning("Latitude is not valid (outside -90:90. Function not run")
        x <- 1
      }
    }

    if (x == 1) {
      # Get distance between points
      if (units == "midpoint") {
        name <- geosphere::midPoint(cbind(start.long, start.lat), cbind(end.long, end.lat))
      } else {
        name <- geosphere::distGeo(cbind(start.long, start.lat), cbind(end.long, end.lat), a = 6378137, f = 1 / 298.257223563)
      }

      if (units == "miles") {
        name <- name * 0.000621371192237334
      } else if (units == "kilometers") {
        name <- name / 1000
      }

      # Log the function
      create_dist_between_function <- list()
      create_dist_between_function$functionID <- "create_dist_between"
      create_dist_between_function$args <- list(dat, start, end, units, deparse(substitute(name)))
      create_dist_between_function$kwargs <- list(vars)
      create_dist_between_function$output <- list(dat)

      log_call(create_dist_between_function)
      return(cbind(dataset, name))
    }
  }
}

## ---- Temporal  Variables ----##
#' Create vector of duration of time
create_duration <- function(dat, start, end, units = c("week", "day", "hour", "minute"), name = "create_duration") {
  #' @description  Create vector of duration of time based on start and ending dates in desired temporal units.
  #' @param dat Primary data containing information on hauls or trips.
  #' Table in FishSET database contains the string 'MainDataTable'.
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
  #' MainDataTable <- create_duration(MainDataTable, 'TRIP_START', 'TRIP_END',
  #'  units='minute', name='TripDur')
  #' }

  # Call in datasets
  dataset <- dat
  dat <- deparse(substitute(dat))


  if (any(grepl("date|min|hour|week|month|TRIP_START|TRIP_END", start, ignore.case = TRUE)) == FALSE) {
    warning("Function is designed for temporal variables")
  }
  if (any(grepl("date|min|hour|week|month|TRIP_START|TRIP_END", end, ignore.case = TRUE)) == FALSE) {
    warning("Function is designed for temporal variables")
  }

  elapsed.time <- lubridate::interval(date_parser(dataset[[start]]), date_parser(dataset[[end]]))
  if (units == "week") {
    name <- lubridate::as.duration(elapsed.time) / lubridate::dweeks(1)
  } else if (units == "day") {
    name <- lubridate::as.duration(elapsed.time) / lubridate::ddays(1)
  } else if (units == "hour") {
    name <- lubridate::as.duration(elapsed.time) / lubridate::dhours(1)
  } else if (units == "minute") {
    name <- lubridate::as.duration(elapsed.time) / lubridate::dminutes(1)
  }

  create_var_temp_function <- list()
  create_var_temp_function$functionID <- "create_duration"
  create_var_temp_function$args <- list(dat, start, end, units, deparse(substitute(name)))
  create_var_temp_function$kwargs <- list()
  create_var_temp_function$output <- list(dat)
  log_call(create_var_temp_function)

  return(cbind(dataset, name))
}
