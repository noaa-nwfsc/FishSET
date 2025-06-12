# =================================================================================================
# File: select_variables_server.R
# Description: Defines the server-side logic for the selecting variables subtab in the FishSET Shiny app.
#              This function is sourced into lite_app.R and passed to shinyApp().
#
# Package: FishSET
# Authors: Paul Carvalho, Anna Abelman
# Date created: 6/9/2025
# Dependencies: - Input/output bindings defined in select_variables_ui.R
#               - Additional input/output bindings defined in modules.
# Notes: - Avoid using hard-coded file paths; use system.file() when accessing
#          package resources.
#
# =================================================================================================

select_main_var_server <- function(id, rv_data){
   moduleServer(
      id,
      function(input, output, session){
         
         ns <- session$ns
         
         # Initialize reactives
         
         observe({
            req(rv_data) # Ensure data is not null
            main_data <- rv_data$main # Save static copy of main data from reactive input
            
            if(!is.null(main_data)){
               shinyjs::show("main_variables_container")  # Show single file upload
               updateSelectInput(session, 
                                 'main_zone_id_input',
                                 choices = colnames(main_data))
               updateSelectInput(session, 
                                 'main_zone_lon_input',
                                 choices = find_lon(main_data))
               updateSelectInput(session, 
                                 'main_zone_lat_input',
                                 choices = find_lat(main_data))
               updateSelectInput(session, 
                                 'main_zone_date_input',
                                 choices = date_cols(main_data)) 
               shinyjs::hide("select_error_message")
               
            } else {
               shinyjs::hide("main_variables_container")  
               shinyjs::show("select_error_message")
               
            }
         })
         
         reactive({
            list(
               main_zone_id = input$main_zone_id_input,
               main_zone_lon = input$main_zone_lon_input,
               main_zone_lat = input$main_zone_lat_input,
               main_zone_date = input$main_zone_date_input
            )
         })
      })
   
} 

select_spat_var_server <- function(id, rv_data){
   moduleServer(
      id,
      function(input, output, session){
         
         ns <- session$ns
         
         observe({
            req(rv_data) # Ensure data is not null
            spat_data <- rv_data$spat # Save static copy of spat data from reactive input
            
            
            if(!is.null(spat_data)){
               shinyjs::show("spat_variables_container")  
               updateSelectInput(session,
                                 'spat_zone_id_input',
                                 choices = names(as.data.frame(spat_data)))
               shinyjs::hide("select_error_message")
               
            } else {
               shinyjs::hide("spat_variables_container")  
               shinyjs::show("select_error_message")
               
            }
         })
         reactive({
            list(
               spat_zone_id = input$spat_zone_id_input,
            )
         })
      })
}

select_port_var_server <- function(id, rv_data){
   moduleServer(
      id,
      function(input, output, session){
         
         ns <- session$ns
         
         observe({    
            req(rv_data) # Ensure data is not null
            port_data <- rv_data$port
            
            if(!is.null(port_data)){
               shinyjs::hide("select_error_message")
               shinyjs::show("port_variables_container")  
               tagList(
                  updateSelectInput(session,
                                    "port_name_input",
                                    choices =colnames(port_data)),
                  
                  updateSelectInput(session,
                                    "port_lon_input",
                                    choices = find_lon(port_data)),
                  
                  updateSelectInput(session,
                                    "port_lat_input",
                                    choices =  find_lat(port_data))
               )
               
            } else {
               shinyjs::show("select_error_message")
               shinyjs::hide("port_variables_container")  
               
               
            }
            
         })
         
         reactive({
            list(
               port_name = input$port_name_input,
               port_lon = input$port_lon_input,
               port_lat = input$port_lat_input
            )
         })
      })
}

