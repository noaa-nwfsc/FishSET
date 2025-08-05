# =================================================================================================
# File: qaqc_module.R
# Description: 
#
# Authors: Paul Carvalho, Anna Abelman
# Date created: 8/4/2025
# Dependencies: 
# Notes: 
# =================================================================================================

# Source module scripts ---------------------------------------------------------------------------
source("modules/qaqc/preview_data_module.R", local = TRUE) # Preview data in table format

# UI for the sidebar controls in the QAQC tab
qaqc_sidebar_ui <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("qaqc_options"), 
                 "Data quality checks:",
                 choices = c("Preview data" = "preview",
                             "Simple message" = "message"),
                 selected = "preview")
  )
}


# 
qaqc_server <- function(id, rv_project_name, rv_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    preview_data_server("preview_data", rv_project_name, rv_data)
    
    output$message <- renderText({
      "This is a simple message displayed when the 'Simple message' radio button is selected."
    })
  })
}

#
qaqc_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    # Conditionally display the preview data UI
    conditionalPanel(
      condition = "input.qaqc_options == 'preview'",
      ns = ns,
      preview_data_ui(ns("preview_data"))
    ),
    
    # Conditionally display the simple message
    conditionalPanel(
      condition = "input.qaqc_options == 'message'",
      ns = ns,
      h4("A Simple Message"),
      wellPanel(
        textOutput(ns("message"))
      )
    )
  )
}
