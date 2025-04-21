# run_fishset_gui
#' Guided user interface for FishSET functions
#'
#' Runs functions associated with loading data, exploring data, checking for data quality issues, generating new variables, and basic data analysis function.
#'
#' @import shiny
# @import ggplot2
# @importFrom DT DTOutput renderDT
# @importFrom ggpubr ggarrange
# @importFrom ggcorrplot ggcorrplot
# @importFrom DBI  dbDisconnect dbConnect dbListTables dbWriteTable
# @importFrom gridExtra grid.table
# @importFrom stringi stri_count_regex
# @importFrom shinycssloaders withSpinner
#' @export run_fishset_gui
#' @details
#' Opens an interactive page that allows users to select which functions to run by clicking check boxes.
#' Data can be modified and saved. Plot and table output are saved to the output folder. Functions calls are logged in the log file.
#' @examples
#' \dontrun{
#' run_fishset_gui()
#' }
#'
run_fishset_gui <- function() {
  appDir <- system.file("ShinyFiles", "MainApp", package = "FishSET")
  if (appDir == "") {
        stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  # shiny app call
  shiny::runApp(appDir, display.mode = "normal")

}
