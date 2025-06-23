# =================================================================================================
# File: select_variables_ui.R
# Description: Defines the UI layout for the selecting variables subtab in the FishSET Shiny app, which
#              is pared with 'select_variables_server.R' and sourced in 'lite_app.R'.
#
# Package: FishSET
# Authors: Paul Carvalho, Anna Abelman
# Date created: 6/9/2025
#
# Notes: - Keep UI layout and input definitions modular and clean.
#        - Use 'bslib' package for UI.
#        - Use unique namespaced IDs for modules
#
# =================================================================================================

select_main_var_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(id = ns("main_variables_container"),
        style = "display: none;",
        selectizeInput(ns("main_zone_id_input"), 
                       "Select zone ID from primary data",
                       choices = NULL, multiple = FALSE),
        selectizeInput(ns("main_zone_lon_input"),
                       "Select fishing location longitude from primary data",
                       choices = NULL, multiple = FALSE, 
                       options = list(create = TRUE)),
        selectizeInput(ns("main_zone_lat_input"), 
                       "Select fishing location latitude from primary data",
                       choices = NULL, multiple = FALSE, 
                       options = list(create = TRUE)),
        selectizeInput(ns("main_zone_date_input"),
                       "Select date variable", 
                       choices = NULL, multiple = FALSE)
    ),
    div(id = ns("select_error_message"), 
        style = "color: red; display: none; font-size: 20px;", 
        p(" ⚠️ Main data not found. Return to Upload data tab and ensure data is
            loaded correctly. ")
    )
  )
}

select_spat_var_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(id = ns("spat_variables_container"),
        style = "display: none;",
        selectizeInput(ns('spat_zone_id_input'),
                       'Select column containing zone ID in spatial data table',
                       choices = NULL, multiple = FALSE)
    ),
    div(id = ns("select_error_message"), 
        style = "color: red; display: none; font-size: 20px;", 
        p(" ⚠️ Spatial data not found. Return to Upload data tab and ensure data is
            loaded correctly. ")
    )
  )
}

select_port_var_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    div(id = ns("port_variables_container"),
        style = "display: none;",
        selectizeInput(ns("port_name_input"),
                       "Select variable from port table with port names",
                       choices =NULL,
                       multiple = FALSE),
        selectizeInput(ns("port_lon_input"),
                       "Select variable from port table with port longitude",
                       choices = NULL,
                       multiple = FALSE),
        selectizeInput(ns("port_lat_input"),
                       "Select variable from port table with port latitude",
                       choices = NULL,
                       multiple = FALSE)),
    div(id = ns("select_error_message"), 
        style = "color: red; display: none; font-size: 20px;", 
        p(" ⚠️ Port data not found. Return to Upload data tab and ensure data is
            loaded correctly. ")
    )
    
  )
}

select_aux_var_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(id = ns("aux_variables_container"),
        style = "display: none;",
        selectizeInput(ns('aux_id_input'),
                       'Select column containing ID in aux data table that matches with main table',
                       choices = NULL, multiple = FALSE)
    ),
    div(id = ns("select_error_message"), 
        style = "color: red; display: none; font-size: 20px;", 
        p(" ⚠️ Aux data not found. Return to Upload data tab and ensure data is
            loaded correctly. ")
    )
  )
}

saving_sel_var_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(inputId = ns("save_vars_btn"),
                 label = "Save selected variables",
                 width = "50%",
                 icon = icon(name="upload", 
                             lib="font-awesome")  ),
    
    # Error message - see specific error messages in load_data_server()
    div(id = ns("var_error_message"), 
        style = "color: red; display: none; font-size: 20px;", 
        textOutput(ns("var_error_message_out"))
    ),
    
    # Success message
    div(id = ns("var_success_message"), 
        style = "color: green; display: none; font-size: 20px;",
        textOutput(ns("var_success_message_out"))
    ),
  )
}