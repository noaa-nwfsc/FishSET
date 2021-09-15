# Data selection tool

# add_vars
#' Add removed variables back into dataset - non-interactive version
#' 
#' Add columns that have been removed from the primary dataset back into the primary dataset. 
#'
#' @param working_dat Primary data containing information on hauls or trips. Table in FishSET database contains the string `MainDataTable`.
#' @param raw_dat Unmodified raw version of the primary dataset. Should be a character specifying a table from the FishSET database
#'   containing the string ‘MainDataTable’ and date table was created.
#' @param vars Character string, variables from \code{raw_dat} to add back into \code{working_dat}.
#' @param project Character, name of project. Parameter is used to generate meaningful table names in FishSET database.
#' @importFrom DBI  dbDisconnect dbConnect dbListTables dbWriteTable
#' @import shiny
#' @export add_vars
#' @details  Add variables back into the dataset that were removed.
#' The removed variables are obtained from the \code{raw_dat} and merged into the working data based on a row identifier.
#' The row identifier is created when a variable is removed using the \code{\link{select_vars}} function.
#' The row identifier is used to match the raw data variables to \code{working_dat}.

#' @examples
#' \dontrun{
#' add_vars(pcodMainDataTable, "pcodMainDataTable20200410", "pollock")
#' }
#'
# Selectbox is column names in raw data that are not in working data
# Show top five rows of working data
# Drop rows that are not in linkID
add_vars <- function(working_dat, raw_dat, vars, project) {
  x <- 0


  # Pull in data
  out <- data_pull(working_dat, project)
  work_dat_name <- parse_data_name(working_dat, "main")
  working_dat <- out$dataset

  out <- data_pull(raw_dat, project)
  raw_dat_name <- out$dat
  raw_dat <- out$dataset

  if ("linkID" %in% names(working_dat) == FALSE) {
    warning("Function could not run. Must use select_vars function to subset data set before adding variables back into working data set.")
    x <- 1
  }
  if (length(colnames(raw_dat)[-which(names(raw_dat) %in% names(working_dat))]) == 0) {
    warning("Function could not run. No new variables to add.")
    x <- 1
  }

  if (x == 0) {
    # Keep the selected columns
    raw_dat <- raw_dat[, c(vars, "linkID"), drop = FALSE]
    dataset <- merge(raw_dat, working_dat, by = "linkID")

    # Save the data
    # Connect to the database
    suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project)))
    on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
    DBI::dbWriteTable(fishset_db, paste0(project, "MainDataTable", format(Sys.Date(), format = "%Y%m%d")), dataset, overwrite = TRUE)

    print(paste0("Table saved to database as ", project, "MainDataTable", format(Sys.Date(), format = "%Y%m%d")))
    DBI::dbDisconnect(fishset_db)

    # Log the function
    add_vars_function <- list()
    add_vars_function$functionID <- "add_vars"
    add_vars_function$args <- list(work_dat_name, raw_dat_name, vars, project)
    add_vars_function$output <- work_dat_name
    log_call(project, add_vars_function)

    return(dataset)
  }
}
