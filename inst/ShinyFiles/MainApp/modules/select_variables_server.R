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

# Server for sidebar ------------------------------------------------------------------------------

# Server for main panel ---------------------------------------------------------------------------

## Select variables from main data table ----------------------------------------------------------
## Description: Users can select variables from main data table where they can then be used 
##              throughout the app
select_main_var_server <- function(id,rv_project_name, rv_data){
   moduleServer(
      id,
      function(input, output, session){
         
         ns <- session$ns
         
         observe({
            req(rv_project_name()) # Check to ensure reactive is available
            project_name <- rv_project_name()$value
            req(rv_data) # Ensure data is not null
            main_data <- rv_data$main # Save static copy of main data from reactive input
            
            # if saved variables already exist in project folder
            tab_name <- paste0(project_name, "SavedVariables")
            folder_path <- paste0(loc_data(project_name), "/", tab_name, ".rds")
            
            # if exists update the selectInput selections to show the existing variable
            if (file.exists(folder_path) & !is.null(main_data)) {
               existing_variables <- readRDS(folder_path)
               
               shinyjs::show("main_variables_container")  # Show single file upload
               updateSelectInput(session, 
                                 'main_zone_id_input',
                                 choices = colnames(main_data),
                                 selected = existing_variables$main$main_zone_id)
               updateSelectInput(session, 
                                 'main_zone_lon_input',
                                 choices = find_lon(main_data),
                                 selected = existing_variables$main$main_zone_lon)
               updateSelectInput(session, 
                                 'main_zone_lat_input',
                                 choices = find_lat(main_data),
                                 selected = existing_variables$main$main_zone_lat)
               updateSelectInput(session, 
                                 'main_zone_date_input',
                                 choices = date_cols(main_data),
                                 selected = existing_variables$main$main_zone_date) 
               shinyjs::hide("select_error_message")
               
               # if doesn't exist, just show variables in main data
            } else if(!file.exists(folder_path) & !is.null(main_data)){
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
               
               # if main data does not exist, show error message
            } else {
               shinyjs::hide("main_variables_container")  
               shinyjs::show("select_error_message")
               
            }
         })
         # return selected variables 
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

## Select variables from spat data table ----------------------------------------------------------
## Description: Users can select variables from spat data table where they can then be used 
##              throughout the app
select_spat_var_server <- function(id,rv_project_name, rv_data){
   moduleServer(
      id,
      function(input, output, session){
         
         ns <- session$ns
         
         observe({
            req(rv_project_name()) # Check to ensure reactive is available
            project_name <- rv_project_name()$value
            req(rv_data) # Ensure data is not null
            spat_data <- rv_data$spat # Save static copy of spat data from reactive input
            
            # if saved variables already exist in project folder
            tab_name <- paste0(project_name, "SavedVariables")
            folder_path <- paste0(loc_data(project_name), "/", tab_name, ".rds")
            
            # if exists update the selectInput selections to show the existing variable
            if (file.exists(folder_path) & !is.null(spat_data)) {
               existing_variables <- readRDS(folder_path)
               
               shinyjs::show("spat_variables_container")  
               updateSelectInput(session,
                                 'spat_zone_id_input',
                                 choices = colnames(spat_data), 
                                 selected = existing_variables$spat$spat_zone_id)
               shinyjs::hide("select_error_message")
               
               # if doesn't exist, just show variables in spat data
            } else if(!file.exists(folder_path) & !is.null(spat_data)){
               shinyjs::show("spat_variables_container")  
               updateSelectInput(session,
                                 'spat_zone_id_input',
                                 choices = colnames(spat_data))
               shinyjs::hide("select_error_message")
               
               # if spat data does not exist, show error message
            } else {
               shinyjs::hide("spat_variables_container")  
               shinyjs::show("select_error_message")
            }
         })
         # return selected variables 
         reactive({
            list(
               spat_zone_id = input$spat_zone_id_input
            )
         })
      })
}

## Select variables from port data table ----------------------------------------------------------
## Description: Users can select variables from port data table where they can then be used 
##              throughout the app
select_port_var_server <- function(id,rv_project_name, rv_data){
   moduleServer(
      id,
      function(input, output, session){
         
         ns <- session$ns
         
         observe({  
            req(rv_project_name()) # Check to ensure reactive is available
            project_name <- rv_project_name()$value
            req(rv_data) # Ensure data is not null
            port_data <- rv_data$port
            
            # if saved variables already exist in project folder
            tab_name <- paste0(project_name, "SavedVariables")
            folder_path <- paste0(loc_data(project_name), "/", tab_name, ".rds")
            
            # if exists update the selectInput selections to show the existing variable
            if (file.exists(folder_path) & !is.null(port_data)) {
               existing_variables <- readRDS(folder_path)
               
               shinyjs::hide("select_error_message")
               shinyjs::show("port_variables_container")  
               tagList(
                  updateSelectInput(session,
                                    "port_name_input",
                                    choices =colnames(port_data),
                                    selected = existing_variables$port$port_name),
                  
                  updateSelectInput(session,
                                    "port_lon_input",
                                    choices = find_lon(port_data),
                                    selected = existing_variables$port$port_lon),
                  
                  updateSelectInput(session,
                                    "port_lat_input",
                                    choices =  find_lat(port_data),
                                    selected = existing_variables$port$port_lat)
               )
               
               # if doesn't exist, just show variables in port data
            } else if(!file.exists(folder_path) & !is.null(port_data)){
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
               
               # if port data does not exist, show error message
            } else {
               shinyjs::show("select_error_message")
               shinyjs::hide("port_variables_container")  
            }
         })
         # return selected variables 
         reactive({
            list(
               port_name = input$port_name_input,
               port_lon = input$port_lon_input,
               port_lat = input$port_lat_input
            )
         })
      })
}
## Select variables from aux data table ----------------------------------------------------------
## Description: Users can select variables from aux data table where they can then be used 
##              throughout the app
select_aux_var_server <- function(id,rv_project_name, rv_data){
   moduleServer(
      id,
      function(input, output, session){
         
         ns <- session$ns
         
         observe({    
            req(rv_project_name()) # Check to ensure reactive is available
            project_name <- rv_project_name()$value
            req(rv_data) # Ensure data is not null
            aux_data <- rv_data$aux
            
            # if saved variables already exist in project folder
            tab_name <- paste0(project_name, "SavedVariables")
            folder_path <- paste0(loc_data(project_name), "/", tab_name, ".rds")
            
            # if exists update the selectInput selections to show the existing variable
            if (file.exists(folder_path) & !is.null(aux_data)) {
               existing_variables <- readRDS(folder_path)
               
               shinyjs::hide("select_error_message")
               shinyjs::show("aux_variables_container")  
               tagList(
                  updateSelectInput(session,
                                    "aux_id_input",
                                    choices =colnames(aux_data),
                                    selected = existing_variables$aux$aux_id)
               )
               
               # if doesn't exist, just show variables in aux data
            } else if(!file.exists(folder_path) & !is.null(aux_data)){
               shinyjs::hide("select_error_message")
               shinyjs::show("aux_variables_container")  
               tagList(
                  updateSelectInput(session,
                                    "aux_id_input",
                                    choices =colnames(aux_data))
               )
               # if aux data does not exist, show error message
            } else {
               shinyjs::show("select_error_message")
               shinyjs::hide("aux_variables_container")  
            }
         })
         # return selected variables 
         reactive({
            list(
               aux_id = input$aux_id_input,
            )
         })
      })
}
## Save variables to project folder ----------------------------------------------------------
## Description: Users can save variables from all data tables so they can be used in future 
##              sessions
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
               
               file_names <- paste0(loc_data(project_name), "/", tab_name, ".rds")
               
               saveRDS(saved_variables, file = file_names)
               
               rv_var_success_message("Variables are loaded and saved in project data folder.")
               shinyjs::show("var_success_message")
               shinyjs::hide("var_error_message")
               
               ### Zonal centroid -----------------------------------------------------------------
               q_test_centroid <- quietly_test(create_centroid, show_msg = FALSE)
               q_test_centroid(project  )
               
               
            }
            
            
         })
      }
   )
}
## Create trip/haul level ID ----------------------------------------------------------------------
## Description: Users can select whether or not they need to create a trip/haul level id in the 
##              main data table
create_nominal_id_server <- function(id, rv_project_name, rv_selected_variables){
   moduleServer(
      id,
      function(input, output, session){
         
         ns <- session$ns
         
         # if users check box, show options and can type new variable name
         observeEvent(input$nominal_id_chk_input,{
            if(input$nominal_id_chk_input) {
               shinyjs::show("nominal_id_container") 
            } else{
               shinyjs::hide("nominal_id_container")
            }
         })
         # return values to be used in the create_nominal_id_inputs_server
         reactive({
            list(
               id_chk = input$nominal_id_chk_input,
               id_type = input$select_nominal_id_input,
               create_id_varname = input$create_id_varname_input
            )
         })
      }
   )
}

## Create trip/haul level ID  Continued -----------------------------------------------------------
## Description: Users can select how they want to create the ID either by using a row number or by
##              combining values of two or more selected variables; a modal will open for users to
##              preview the new IDs before saving to the Fishset database.
create_nominal_id_inputs_server <- function(id, rv_project_name, rv_data, 
                                            rv_selected_variables, rv_nominal_id_type){
   moduleServer(
      id,
      function(input, output, session){
         
         ns <- session$ns
         
         # Initialize reactives
         rv_create_id_table <- reactiveValues() # reactive value for table with new ID 
         rv_id_success_message <- reactiveVal("") # Store success message
         
         
         observe({    
            req(rv_nominal_id_type())
            id_type <- rv_nominal_id_type()$id_type # type of nominal id user selected
            req(rv_data) # Ensure data is not null
            main_data <- rv_data$main # save as static value
            
            # only show if user checks the nominal id checkbox
            if(rv_nominal_id_type()$id_chk == TRUE){
               if(id_type == 'create_id_input'){
                  shinyjs::show("create_id_container")
                  updateSelectizeInput(session, "create_id_vars_input",
                                       choices = colnames(main_data))
               }  else {
                  shinyjs::hide("create_id_container")
               }
               
               shinyjs::show("create_id_btn_container")
               
            } else{
               shinyjs::hide("create_id_container")  
            }
            
         })
         
         observeEvent(input$create_nominal_id_btn, {
            
            req(rv_nominal_id_type())
            id_type <- rv_nominal_id_type()$id_type
            id_varname <- rv_nominal_id_type()$create_id_varname
            req(rv_project_name()) # Check to ensure reactive is available
            project_name <- rv_project_name()$value
            req(rv_data) # Ensure data is not null
            main_data <- rv_data$main
            req(input$create_id_type_input)
            
            # if user selects: Create haul or trip ID based on variables
            if(id_type == 'create_id_input'){
               req(input$create_id_vars_input)
               
               vars_in <- input$create_id_vars_input # save as static value
               
               # user ID_var function to create ID
               q_test <- quietly_test(ID_var)
               rv_create_id_table$output <- q_test(main_data,
                                                   project = project_name,
                                                   name = id_varname, 
                                                   vars =vars_in, 
                                                   type = input$create_id_type_input)
               # if user selects: Create haul or trip ID based on row numbers 
            } else if(id_type == 'create_id_seq_input'){
               
               # user ID_var function to create ID
               q_test <- quietly_test(ID_var)
               rv_create_id_table$output <- q_test(main_data,
                                                   project = project_name,
                                                   name = id_varname, 
                                                   vars = NULL, 
                                                   type = input$create_id_type_input)
            }
            
            # creating table with initial values for user to get a glance at new variable created
            output$create_id_table <- DT::renderDT(
               head(rv_create_id_table$output)
            )
            
            # Popup to view new table and confirmation button
            showModal(
               modalDialog(
                  title = "Are you sure you would like to add this new variable?",
                  style = "overflow-x: auto; white-space: nowrap;",
                  size = "l",
                  DT::DTOutput(ns("create_id_table")),
                  footer = tagList(
                     modalButton("Close"),
                     actionButton(ns("confirm_nominal_id_btn"), "Confirm & Save", 
                                  class = "btn-secondary")),
                  easyClose = TRUE)
            )
         })
         
         
         output$id_success_message_out <- renderText({
            rv_id_success_message()
         })
         
         observeEvent(input$confirm_nominal_id_btn,{
            req(rv_project_name()) # Ensure rv_project_name is not NULL
            req(rv_data) # Ensure data is not null
            project_name <- rv_project_name() # Retrieve current project info
            req(rv_create_id_table$output)
            
            # save new table with new variables
            rv_data$main <- rv_create_id_table$output
            
            # reset main table in fishset database
            load_maindata(dat = rv_data$main,
                          project = project_name$value,
                          over_write = TRUE)
            
            removeModal()
            
            rv_id_success_message("table saved")
            shinyjs::show("id_success_message")
         } )
      }
   )
}