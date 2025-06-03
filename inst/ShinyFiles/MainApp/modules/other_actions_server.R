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


other_actions_server <- function(id){
  moduleServer(id, function(input, output, session){
    
    # Initialize reactive
    rv_r_expr <- reactiveValues(output = "")
    
    # Observer run R expression
    observeEvent(input$run_r_expr_btn, {
      req(input$r_expr_input) # ensure access to code input
      
      tryCatch(
        {
          rv_r_expr$output <- isolate(
            paste(utils::capture.output(eval(parse(text = input$r_expr_input))), collapse = '\n')
          )
          # rv_r_expr$ok <- TRUE
        },
        error = function(e) {rv_r_expr$output <- e$message}
      )
    })
    
    # Generate output for R expression
    output$r_expr_result <- renderUI({
      if(rv_r_expr$done > 0 ) {
        content <- paste(paste(">", isolate(input$r_expr_input)), rv_r_expr$output, sep = '\n')
        if(rv_r_expr$ok) {
          pre(content)
        } else {
          pre( style = "color: red; font-weight: bold;", content)
        }
      }
    })
    

    
  })
}