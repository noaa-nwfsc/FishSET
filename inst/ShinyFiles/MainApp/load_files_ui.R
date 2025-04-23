# =================================================================================================
# File: load_files_ui.R
# Description: Defines the UI layout for the load files subtab in the FishSET Shiny app, which
#              is pared with 'load_files_server.R' and sourced in 'lite_app.R'.
#
# Package: FishSET
# Authors: Paul Carvalho, Anna Abelman
# Date created: 4/23/2025
#
# Notes: - Keep UI layout and input definitions modular and clean.
#        - Use 'bslib' package for UI.
#        - Use unique namespaced IDs for modules
#
# =================================================================================================

# UI for sidebar
load_files_sidebar_ui <- function(id){
  
}

# UI for main panel -------------------------------------------------------------------------------

## Change folder path -----------------------------------------------------------------------------
folder_path_ui <- function(id){
  tagList(
    fluidRow(
      column(3, actionButton(inputId = NS(id, "change_fs_folder"), 
                             label = 'Change FishSET Folder',
                             width = '100%',
                             class = "btn-primary")),
      column(9, p("TEST"))
    )
  )
}