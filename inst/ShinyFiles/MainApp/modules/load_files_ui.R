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

## UI for sidebar ---------------------------------------------------------------------------------
## Description: various action buttons for users to explore as popups including refreshing data, 
##              setting confidentiality rules, resetting the log, saving notes as txt output, and
##              closing app
load_sidebar_ui <- function(id){
  ns <- NS(id)
  
  refresh_btn_container <- tagList(
    actionButton(ns("refresh_data_btn"), HTML("Complete refresh <br> of data"),
                 class = "btn-secondary",
                 style = "width: 100%;",
                 icon = icon('sync', verify_fa = FALSE),
    ) ,
    # Overlay spinner for refreshing data
    div(id = ns("refresh_data_spinner_container"),
        style = "display: none;",
        spinner_ui(ns("refresh_data_spinner"),
                   spinner_type = "circle",
                   size = "large",
                   message = "Refreshing data...",
                   overlay = TRUE)
    )
  )
  
  tagList(
    actionButton(ns("confid_modal_btn"), "Confidentiality settings",
                 class = "btn-secondary", disable = TRUE
    ),
    splitLayout(cellWidths = c("50%", "50%"),
                refresh_btn_container,
                actionButton(ns("reset_log_modal_btn"), "Reset log",
                             style = "width: 100%; height: 58px",
                             icon = icon('file-lines', verify_fa = FALSE),
                             class = "btn-secondary",  disable = TRUE
                )
    )
  )
}

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

## Select data ------------------------------------------------------------------------------------
## Description: Provide user with a dropdown menu if loading an existing project AND the data 
##              table exists in the project. Otherwise, give the user the file input option, and 
##              include option to load shape files for spatial data.
select_data_ui <- function(id, data_type){
  ns <- NS(id)
  tagList(
    fluidRow(
      # select existing data table - initially hidden with CSS
      div(id = ns(paste0(data_type, "_select_container")), 
          style = "display: none;", # hide this input to start
          selectInput(inputId = ns(paste0(data_type, "_select_input")),
                      label = 
                        switch(data_type,
                               "main" = HTML("Select main data table <b>(required)</b>:"),
                               "port" = HTML("Select port data table <b>(optional)</b>:"),
                               "aux" = HTML("Select auxiliary data table <b>(optional)</b>:"),
                               "spat" = HTML("Select spatial table <b>(required)</b>:"),
                               "grid" = HTML("Select gridded data table <b>(optional)</b>:")),
                      choices = NULL)
      ),
      
      # select new data file - initially visible
      if(data_type != "spat"){
        div(id = ns(paste0(data_type, "_upload_container")),
            fileInput(ns(paste0(data_type, "_upload_input")),
                      label = 
                        switch(data_type,
                               "main" = HTML("Select main data file <b>(required)</b>:"),
                               "port" = HTML("Select port file <b>(optional)</b>:"),
                               "aux" = HTML("Select auxiliary data file <b>(optional)</b>:"),
                               "grid" = HTML("Select gridded data file <b>(optional)</b>:")),
                      multiple = FALSE,
                      placeholder = "No file selected")
        )
        
      } else {
        # Visible container for selecting spatial file(s)
        div(id = ns("spat_upload_container"),
            fluidRow(
              # File input for general spatial data files (e.g., CSV, GeoJSON)
              div(id = ns('spat_file_container'), 
                  fileInput(ns("spat_file_input"), 
                            label = HTML("Select spatial file <b>(required)</b>:"),
                            multiple = FALSE, 
                            placeholder = 'No file selected')
              ),
              
              # File input for shapefile components
              div(id = ns("spat_shp_container"),
                  fileInput(ns("spat_shp_input"), 
                            label = HTML("Select shape files <b>(required)</b>:"),
                            accept = c('.shp', '.dbf', '.sbn', '.sbx', '.shx', '.prj', '.cpg'),
                            multiple = TRUE, 
                            placeholder = 'No file selected')
              ),
              
              #Checkbox to change file upload to shape files
              checkboxInput(inputId = ns("spat_shp_chk_input"),
                            label = "Uploading shape file instead?",
                            value = FALSE)),
        )
      }
    )
  )
}

## Load data --------------------------------------------------------------------------------------
## Description: Action button to load all of the selected data. Users are also notified if data
##              loaded successfully, or if an error occured (with a specific error message).
load_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(inputId = ns("load_data_btn"),
                 label = "Load data",
                 width = "50%",
                 icon = icon(name="upload", 
                             lib="font-awesome")
    ),
    
    # Overlay spinner for this section
    div(id = ns("load_data_spinner_container"),
        style = "display: none;",
        spinner_ui(ns("load_data_spinner"),
                   spinner_type = "circle",
                   size = "large",
                   message = "Loading data...",
                   overlay = TRUE)
    ),
    
    # Error message - see specific error messages in load_data_server()
    div(id = ns("load_error_message"), 
        style = "color: red; display: none; font-size: 20px;", 
        textOutput(ns("load_error_message_out"))
    ),
    
    # Success message
    div(id = ns("load_success_message"), 
        style = "color: green; display: none; font-size: 20px;",
        textOutput(ns("load_success_message_out"))
    ),
  )
}

