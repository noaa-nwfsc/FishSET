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
         return(
            reactive({
               list(
                  main_zone_id = input$main_zone_id_input,
                  main_zone_lon = input$main_zone_lon_input,
                  main_zone_lat = input$main_zone_lat_input,
                  main_zone_date = input$main_zone_date_input
               )
               
            })
         )
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
                                 choices = colnames(spat_data))
               shinyjs::hide("select_error_message")
               
            } else {
               shinyjs::hide("spat_variables_container")  
               shinyjs::show("select_error_message")
            }
         })
         reactive({
            list(
               spat_zone_id = input$spat_zone_id_input
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

select_aux_var_server <- function(id, rv_data){
   moduleServer(
      id,
      function(input, output, session){
         
         ns <- session$ns
         
         observe({    
            req(rv_data) # Ensure data is not null
            aux_data <- rv_data$aux
            
            if(!is.null(aux_data)){
               shinyjs::hide("select_error_message")
               shinyjs::show("aux_variables_container")  
               tagList(
                  updateSelectInput(session,
                                    "aux_id_input",
                                    choices =colnames(aux_data))
               )
            } else {
               shinyjs::show("select_error_message")
               shinyjs::hide("aux_variables_container")  
            }
         })
         
         reactive({
            list(
               aux_id = input$aux_id_input,
            )
         })
      })
}

save_var_server <- function(id, rv_project_name, rv_selected_variables){
   moduleServer(
      id,
      function(input, output, session){
         
         ns <- session$ns
         
         # Initialize reactives
         rv_var_error_message <- reactiveVal("") # Store error message
         rv_var_success_message <- reactiveVal("") # Store success message
         
         # Outputs for error and success messages - initially hidden
         output$var_error_message_out <- renderText({
            rv_var_error_message()
         })
         output$var_success_message_out <- renderText({
            rv_var_success_message()
         })
         
         observeEvent(input$save_vars_btn, {
            
            if(input$save_vars_btn){
               req(rv_project_name()) # Check to ensure reactive is available
               project_name <- rv_project_name()$value
               req(rv_selected_variables)
               
               saved_variables_main <- rv_selected_variables$main()
               saved_variables_port <- rv_selected_variables$port()
               saved_variables_spat <- rv_selected_variables$spat()
               
               saved_variables <- list(main = saved_variables_main,
                                       spat = saved_variables_spat,
                                       port = saved_variables_port)
               
               tab_name <- paste0(project_name, "SavedVariables")
               raw_name <- paste0(tab_name, format(Sys.Date(), format = "%Y%m%d"))
               
               file_names <- paste0(loc_data(project_name),raw_name, ".rds")
               
               saveRDS(saved_variables, file = file_names)
               
               rv_var_success_message("Variables are loaded and saved in project data folder.")
               shinyjs::show("var_success_message")
               shinyjs::hide("var_error_message")
               
            }
            
            
         })
      }
   )
}

create_nominal_id_server <- function(id, rv_project_name, rv_selected_variables){
   moduleServer(
      id,
      function(input, output, session){
         
         ns <- session$ns
         
         
      }
   )
}