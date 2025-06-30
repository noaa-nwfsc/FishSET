# =================================================================================================
# File: other_actions_ui.R
# Description: Defines the UI layout for the other action functions found in the sidebar including 
#              adding and saving notes in text output, create R expression to test reactive
#              functions with output, and close app button
#
# Package: FishSET
# Authors: Anna Abelman, Paul Carvalho
# Date created: 4/23/2025
#
# Notes: - Keep UI layout and input definitions modular and clean.
#        - Use 'bslib' package for UI.
#        - Use unique namespaced IDs for modules
#
# =================================================================================================


other_actions_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::accordion(open = FALSE,
                     bslib::accordion_panel(
                       "Other actions", icon = bsicons::bs_icon("menu-app") ,
                       actionButton(inputId =ns("manage_tables_btn"),
                                    icon = icon('table', verify_fa = FALSE),
                                    label ="Manage Tables", 
                                    class = "btn-secondary",
                                    disable = TRUE),
                       
                       textInput(inputId = ns('add_notes_input'),
                                 label = "Add notes",
                                 value=NULL,
                                 placeholder = 'Write notes to store in text output file.'),
                       
                       actionButton(inputId = ns('download_notes_btn'),
                                    icon = icon('comments', verify_fa = FALSE),
                                    label = 'Save notes',
                                    class = "btn-success"),
                       
                       div(id = ns("notes_error_container"), 
                           style = "color: red; display: none; font-size: 16px;", 
                           p(paste0("⚠️ Input cannot be empty or null."))
                       ),
                       
                       div(id = ns("notes_success_container"), 
                           style = "color: green; display: none; font-size: 16px;",
                           p(paste0("Data notes saved."))
                       ),
                       
                       tags$br(),
                       
                       actionButton(inputId =ns("close_app_btn"), "Close app",
                                    icon = icon("circle-xmark"),
                                    width = "100%",
                                    class = "btn-danger", 
                                    onclick = "setTimeout(function(){window.close();},500);")
                     )
    )
  )
}