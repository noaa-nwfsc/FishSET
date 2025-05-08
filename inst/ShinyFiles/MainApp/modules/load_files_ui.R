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
## Description: UI module function for spatial data upload and selection. Initially show file
##              input, but if the user selects existing project show existing spatial tables.
load_spatial_ui <- function(id){
  ns <- NS(id)  
  tagList(
    # Hidden container for selecting an already uploaded spatial table
    div(id = ns("spat_select_container"), style = "display: none;",
        selectInput(ns("spat_select_input"), 
                    "Choose spatial table:", 
                    choices = NULL)  # Choices will be populated dynamically
    ),
    
    # Visible container for uploading spatial data
    div(id = ns("spat_upload_container"),
        fluidRow(
          # File input for general spatial data files (e.g., CSV, GeoJSON)
          div(id = ns('spat_file_container'), 
              fileInput(ns("spat_file_input"), 
                        "Choose spatial data file",
                        multiple = FALSE, 
                        placeholder = 'No file selected')
          ),
          # File input for shapefile components
          div(id = ns("spat_shp_container"),
              fileInput(ns("spat_shp_input"), 
                        "Choose spatial shape data file",
                        accept = c('.shp', '.dbf', '.sbn', '.sbx', '.shx', '.prj', '.cpg'),
                        multiple = TRUE, 
                        placeholder = 'No file selected')
              
          ), 
          #Checkbox to change file upload to shape files
          checkboxInput(inputId = ns("spat_shp_chk_input"),
                        label = "Uploading shape file instead?",
                        value = FALSE)),
    )
  )
}

## Load gridded data ------------------------------------------------------------------------------
## Description: UI module for uploading or selecting gridded (1D/2D) data. Initially show file
##              input, but if user selects an existing project show existing gridded tables.
load_grid_ui <- function(id){
  ns <- NS(id)  
  tagList(
    # Hidden container to select a previously uploaded gridded table
    div(id = ns("grid_select_container"), style = "display: none;",
        selectInput(ns("grid_select_input"), 
                    "Choose gridded table:", 
                    choices = NULL)  # Choices will be populated dynamically
    ),
    # Visible container for uploading gridded data
    div(id = ns("grid_upload_container"),
        fluidRow(
          # File input for uploading a single data file (e.g., CSV, TSV)
          fileInput(ns("grid_file_input"), 
                    "Choose data file that varies over one or two dimensions (gridded)",
                    multiple = FALSE, 
                    placeholder = 'No file selected')
        )
        
    )
  )
}
