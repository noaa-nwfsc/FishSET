# =================================================================================================
# File: other_actions_server.R
# Description: Defines the server-side logic for the other action functions found in the sidebar 
#              including adding and saving notes in text output, create R expression to test 
#              reactive functions with output, and close app button
#
# Package: FishSET
# Authors: Anna Abelman, Paul Carvalho
# Date created: 4/23/2025
# Dependencies: - Input/output bindings defined in load_files_ui.RQ
#               - Additional input/output bindings defined in modules.
# Notes: - Avoid using hard-coded file paths; use system.file() when accessing
#          package resources.
#
# =================================================================================================


other_actions_server <- function(id, values = NULL){
  moduleServer(id, function(input, output, session){
    
    # Initialize reactives
    rv_r_expr <- reactiveValues(output = "")
    rv_r_expr_output <- reactiveVal("")
    rv_r_expr_status <- reactiveVal(FALSE)
    
    # Output for results from running R expression - initially hidden
    output$r_expr_result <- renderText({
      rv_r_expr_output()
    })
    
    # Observer run R expression
    observeEvent(input$run_r_expr_btn, {
      req(input$r_expr_input) # ensure access to code input
      
      rv_r_expr_status(FALSE)
      
      tryCatch(
        {
          rv_r_expr$output <- isolate(
            paste(utils::capture.output(eval(parse(text = input$r_expr_input))), collapse = '\n')
          )
          
          rv_r_expr_status(TRUE)
        },
        error = function(e) {rv_r_expr$output <- e$message}
      )
    })
    
    # Generate output for R expression
    observe({
      req(rv_r_expr$output)
      
      rv_r_expr_output(
        paste(paste(">", isolate(input$r_expr_input)), rv_r_expr$output, sep = '\n')
      )
      
      shinyjs::show("r_expr_container")
      
      # Add or remove "error-text" class on the container div based on status
      if (rv_r_expr_status()) {
        shinyjs::removeClass(session$ns("r_expr_container"), "error-text")
      } else {
        shinyjs::addClass(session$ns("r_expr_container"), "error-text")
      }
    })
  })
}