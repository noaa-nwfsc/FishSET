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
                        "Other actions", icon = bsicons::bs_icon("menu-app"),
                        
                        # Run R expressions from the shiny app
                        textInput(ns("r_expr_input"), 
                                  label = "Enter an R expression",
                                  value = "values$data$main"),
                        
                        actionButton(ns("run_r_expr_btn"), "Run", class = "btn-success"),
                        
                        div(id = ns("r_expr_container"), 
                            style = "margin-top: 2em; display: none;", 
                            verbatimTextOutput(ns("r_expr_result"))
                        )
                     
    )
  )
  )
}