# =================================================================================================
# File: other_actions_ui.R
# Description: Defines the UI layout for the other action functions found in the sidebar including 
#              adding and saving notes in text output, create R expression to test reactive
#              functions with output, and close app button
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


other_actions_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::accordion(open = FALSE,
                     bslib::accordion_panel(
                        "Other actions", icon = bsicons::bs_icon("menu-app"), 
                       # textInput('notes_input',
                       #           label = "Notes", 
                       #           value=NULL,
                       #           placeholder = 'Write notes to store in text output file. Text can 
                       #           be inserted into report later.'),
                       # actionButton('save_notes_btn',
                       #              label = 'Save notes',
                       #              class = "btn-success"),
                       # tags$br(), tags$br(),
                       textInput(ns("r_expr_input"), 
                                 label = "Enter an R expression",
                                 value = "values$dataset"),
                       actionButton(ns("run_r_btn"), "Run", class = "btn-success"),
                       div(style = "margin-top: 2em;",
                           uiOutput(ns('r_expr_result'))
                           )
                     ),
                     # tags$br(),
                     # actionButton("close_app_btn", "Close app",
                     #              icon = icon("circle-xmark"),
                     #              width = "100%",                                                           
                     #              class = "btn-danger", 
                     #              onclick = "setTimeout(function(){window.close();},500);")
    )
    
  )
}