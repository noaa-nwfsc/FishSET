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

# UI for sidebar ----------------------------------------------------------------------------------


# UI for main panel -------------------------------------------------------------------------------

## Change folder path -----------------------------------------------------------------------------
## Description: returns a button for changing folder path and displays the selected folderpath
##              in the same bslib card
folder_path_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3, actionButton(inputId = NS(id, "change_fs_folder_btn"), 
                             label = 'Change FishSET Folder',
                             width = '100%',
                             class = "btn-primary")),
      column(9, verbatimTextOutput(ns("display_folderpath")))
    )
  )
}

## Select project ---------------------------------------------------------------------------------
## Description: displays a checkbox, if FALSE (default) then the users inputs a new project name.
##              If TRUE, the user gets a drop down menu of projects in the fishset folder 
##              directory.
select_project_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3, checkboxInput(inputId = ns("load_existing_proj_input"),
                              label = "Load existing project",
                              value = FALSE)),
      
      column(9,
             # select project container - initially hidden with CSS
             div(id = ns("proj_select_container"), 
                 style = "display: none;",
                 selectInput(ns("proj_select_input"), "Choose a project", choices = NULL)),
             
             # project name input container - initially visible
             div(id = ns("proj_name_container"),
                 textInput(ns("proj_name_input"), "Enter project name"))
      )
    )
  )
}

## Load primary data ------------------------------------------------------------------------------
## Description: Provide user with a dropdown menu of primary tables if loading an existing 
##              project, but if this is a new project have the user upload a new file.
load_primary_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      # select existing primary data table - initially hidden with CSS
      div(id = ns("primary_select_container"), 
          style = "display: none;",
          selectInput(ns("primary_select_input"), "Choose primary data table", 
                      choices = NULL)),
      
      # load new primary data file - initially visible
      div(id = ns("primary_upload_container"),
          fileInput(ns("primary_upload_input"), 
                    "Choose primary data file", 
                    multiple = FALSE,
                    placeholder = "No file selected"))
    )
  )
}

## Load spatial data ------------------------------------------------------------------------------
## Description: 
##
load_spatial_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
    )
  )
}
