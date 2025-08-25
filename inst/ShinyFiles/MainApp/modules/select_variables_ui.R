# =================================================================================================
# File: select_variables_ui.R
# Description: Defines the UI layout for the selecting variables subtab in the FishSET Shiny app, 
#              which is pared with 'select_variables_server.R' and sourced in 'lite_app.R'.
#
# Package: FishSET
# Authors: Anna Abelman, Paul Carvalho
# Date created: 6/9/2025
#
# Notes: - Keep UI layout and input definitions modular and clean.
#        - Use 'bslib' package for UI.
#        - Use unique namespaced IDs for modules
#
# =================================================================================================

# UI for sidebar ----------------------------------------------------------------------------------

# UI for main panel -------------------------------------------------------------------------------

## Select variables from main data table ----------------------------------------------------------
## Description: Users can select variables from main data table where they can then be used 
##              throughout the app; error message will show if main data does not exist
select_main_var_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(id = ns("main_variables_container"),
        style = "display: none;",
        
        selectizeInput(ns("main_unique_obs_id_input"),
                       tagList(
                         span(style = "white-space: nowrap; display: inline-flex; 
                                       align-items: center;",
                              HTML("Select trip/haul ID from primary data: &nbsp;"),
                              bslib::tooltip(
                                shiny::icon("circle-info", `aria-label` = "More information"),
                                HTML("If unique trip/haul ID is not available for your dataset, 
                                      use the optional input below to create an ID variable. 
                                      Return to this input after the variable is created and 
                                      saved."),
                                options = list(delay = list(show = 0, hide = 850))
                              )
                         )
                       ),
                       choices = NULL, multiple = FALSE),
        
        selectizeInput(ns("main_zone_id_input"), 
                       "Select zone ID from primary data",
                       choices = NULL, multiple = FALSE),
        
        selectizeInput(ns("main_lon_input"),
                       "Select fishing location longitude from primary data",
                       choices = NULL, multiple = FALSE, 
                       options = list(create = TRUE)),
        
        selectizeInput(ns("main_lat_input"), 
                       "Select fishing location latitude from primary data",
                       choices = NULL, multiple = FALSE, 
                       options = list(create = TRUE)),
        
        selectizeInput(ns("main_date_input"),
                       "Select date variable", 
                       choices = NULL, multiple = FALSE)
    ),
    
    div(id = ns("select_error_message"), 
        style = "color: red; display: none; font-size: 20px;", 
        p("⚠️ Main data not found and is required. Return to load files and ensure data is
          loaded correctly.")
    )
  )
}

## Select variables from port data table ----------------------------------------------------------
## Description: Users can select variables from port data table where they can then be used 
##              throughout the app
select_port_var_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(id = ns("port_variables_container"),
        style = "display: none; width: 100%;",
        
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
                       multiple = FALSE)
    ),
    
    div(id = ns("select_error_message"), 
        style = "display: none; font-size: 20px;", 
        p("Port data not found. If you supplied this data, return to load files and ensure data is
          loaded correctly.")
    )
  )
}

## Select variables from aux data table -----------------------------------------------------------
## Description: Users can select variables from aux data table where they can then be used 
##              throughout the app
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
        style = "display: none; font-size: 20px;", 
        p("Aux data not found. If you supplied this data, return to load files and ensure data is
          loaded correctly.")
    )
  )
}

## Select variables from spat data table ----------------------------------------------------------
## Description: Users can select variables from spat data table where they can then be used 
##              throughout the app; error message will show if spat data does not exist
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
        p(" ⚠️ Spatial data not found and is required. Return to load files and ensure data is
          loaded correctly.")
    )
  )
}

## Create trip/haul level ID ----------------------------------------------------------------------
## Description: Users can select whether or not they need to create a trip/haul level id in the 
##              main data table and which method
create_nominal_id_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(id = ns("nominal_id_chk_container"),
        style = "display: none;",
        checkboxInput(ns("nominal_id_chk_input"), 
                      "Do you need to create a trip-level ID for your data?",
                      value = FALSE)
    ),
    div(id = ns("chk_error_message"), 
        style = "display: none; font-size: 12px;", 
        p("Data needs to be loaded in first.")
    ),
    
    div(id = ns("nominal_id_container"),
        style = "display: none;",
        
        selectInput(
          ns('select_nominal_id_input'),
          'Functions', 
          choices = c('Create haul or trip ID based on variables' = 'create_id_input',
                      'Create haul or trip ID based on row numbers' = 'create_id_seq_input'),
          multiple = FALSE, 
          selected = 'create_id_input'),
        
        textInput(ns('create_id_varname_input'),
                  list('Name of new variable',  
                       bslib::tooltip(  
                         bsicons::bs_icon("info-circle"),
                         "If left empty, default names will be supplied.", 
                         id = "tip", 
                         placement = "right")
                  )
        )
    )
  )
}

## Create trip/haul level ID  Continued -----------------------------------------------------------
## Description: Users can select how they want to create the ID either by using a row number or by
##              combining values of two or more selected variables
create_nominal_id_inputs_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(id = ns("create_id_container"),
        style = "display: none;",
        
        selectInput(ns("create_id_vars_input"), 
                    "Select 2 or more variables",
                    choices = NULL,
                    multiple = TRUE),
        
        selectizeInput(ns('create_id_type_input'), 
                       "Select ID column class type",
                       choices = c("string", "integer"))
    ),
    
    div(id = ns("create_id_btn_container"),
        style = "display:none;",
        actionButton(ns("create_nominal_id_btn"), 
                     "Create ID")
    ),
    
    div(id = ns("id_success_message"), 
        style = "color: green; display: none; font-size: 20px;",
        textOutput(ns("id_success_message_out"))
    )
  )
}

## Save variables to project folder ---------------------------------------------------------------
## Description: Users can save variables from all data tables so they can be used in future 
##              sessions
save_var_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 12,
             bslib::card(
               fill = FALSE,
               
               bslib::card_header(
                 "1. Primary data variables",
                 class = "bg-secondary"),
               
               bslib::card_body(
                 bslib::layout_column_wrap(
                   fill = TRUE,
                   width = 1/3,
                   bslib::card(fill = FALSE,
                               h6("Primary data"),
                               select_main_var_ui(ns("selecting_main"))),
                   
                   bslib::card(fill = FALSE,
                               h6("Port data"),
                               select_port_var_ui(ns("selecting_port"))),
                   
                   bslib::card(fill = FALSE,
                               h6("Aux data"),
                               select_aux_var_ui(ns("selecting_aux")))
                 )
               ),
               
               fluidRow(
                 column(width = 8,
                        bslib::card(
                          fill = FALSE,
                          
                          bslib::card_header(h6("Creating haul/trip level ID (optional)")),
                          
                          bslib::card_body(
                            bslib::layout_column_wrap(
                              fill = TRUE,
                              width = 1/2,
                              bslib::card(
                                class = "border-0 shadhow-none",
                                create_nominal_id_ui(ns("nominal_id"))),
                              
                              bslib::card(
                                class = "border-0 shadhow-none",
                                create_nominal_id_inputs_ui(ns("nominal_id_vars"))
                              )
                            )
                          )
                        )
                 )
               )
             )
      )
    ),
    
    fluidRow(
      column(
        width = 12,
        bslib::card(
          fill = FALSE,
          
          bslib::card_header(
            "2. Spatial data variables",
            class = "bg-secondary"),
          
          bslib::card_body(
            fill = FALSE,
            h6("Spatial data"),
            select_spat_var_ui(ns("selecting_spat"))
          )
        )
      )
    ),
    
    fluidRow(
      actionButton(inputId = ns("save_vars_btn"),
                   label = "Save selected variables",
                   width = "50%",
                   icon = icon(name="upload",
                               lib="font-awesome")),
      
      # Overlay spinner for this section
      div(id = ns("save_var_spinner_container"),
          style = "display: none;",
          spinner_ui(ns("save_var_spinner"),
                     spinner_type = "circle",
                     size = "large",
                     message = "Saving variables...",
                     overlay = TRUE)
      ),
      
      # Error message - see specific error messages
      div(id = ns("var_error_message"),
          style = "color: red; display: none; font-size: 20px;",
          textOutput(ns("var_error_message_out"))
      ),
      
      # Success message
      div(id = ns("var_success_message"),
          style = "color: green; display: none; font-size: 20px;",
          textOutput(ns("var_success_message_out"))
      )
    ),
    
    # Next button to move to quality checks tab
    actionButton(inputId = ns("select_var_next_btn"),
                 label = "Next",
                 width = "15%",
                 style = "float:right",
                 icon = icon(name="circle-chevron-right", 
                             lib="font-awesome"))
  )
}


