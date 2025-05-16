# =================================================================================================
# File: other_actions_server.R
# Description: Defines the server-side logic for the other action functions found in the sidebar 
#              including adding and saving notes in text output, create R expression to test 
#              reactive functions with output, and close app button
#
# Package: FishSET
# Authors: Paul Carvalho, Anna Abelman
# Date created: 4/23/2025
# Dependencies: - Input/output bindings defined in load_files_ui.RQ
#               - Additional input/output bindings defined in modules.
# Notes: - Avoid using hard-coded file paths; use system.file() when accessing
#          package resources.
#
# =================================================================================================


other_actions_server <- function(id, rv_r_expr, rv_project_name){
  moduleServer(id, function(input, output, session){
    
    
    observeEvent(input$run_r_btn, {
      shinyjs::hide("error")
      rv_r_expr$ok <- FALSE
      tryCatch(
        {
          rv_r_expr$output <- isolate(
            paste(utils::capture.output(eval(parse(text = input$r_expr_input))), collapse = '\n')
          )
          rv_r_expr$ok <- TRUE
        },
        error = function(err) {rv_r_expr$output <- err$message}
      )
      rv_r_expr$done <- rv_r_expr$done + 1
    })
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