# =================================================================================================
# File: select_variables_server.R
# Description: Defines the server-side logic for the selecting variables subtab in the FishSET 
#              Shiny app. This function is sourced into lite_app.R and passed to shinyApp().
#
# Package: FishSET
# Authors: Anna Abelman, Paul Carvalho
# Date created: 6/9/2025
# Dependencies: - Input/output bindings defined in select_variables_ui.R
#               - Additional input/output bindings defined in modules.
# Notes: - Avoid using hard-coded file paths; use system.file() when accessing
#          package resources.
#
# =================================================================================================

# Servers for sidebar -----------------------------------------------------------------------------

# Servers for main panel --------------------------------------------------------------------------

## Select variables from main data table ----------------------------------------------------------
## Description: Users can select variables from main data table where they can then be used 
##              throughout the app
select_main_var_server <- function(id, rv_project_name, rv_data){
  moduleServer(
    id,
    function(input, output, session){
      
      ns <- session$ns
      
      observe({
        req(rv_project_name()) # Check to ensure reactive is available
        project_name <- rv_project_name()$value
        req(rv_data) # Ensure data is not null
        main_data <- rv_data$main # Save static copy of main data from reactive input
        
        # if main data does not exist, show error message
        if (is.null(main_data)) {
          shinyjs::hide("main_variables_container")
          shinyjs::show("select_error_message")
          
        } else {
          # if saved variables already exist in project folder
          saved_var_file <- paste0(project_name, "SavedVariables.rds")
          saved_var_filepath <- file.path(loc_data(project_name), saved_var_file)
          saved_var_filepath <- suppressWarnings(normalizePath(saved_var_filepath)) 
          
          
          # if exists update the selectInput selections to show the existing variable
          if (file.exists(saved_var_filepath) & !is.null(main_data)) {
            existing_variables <- readRDS(saved_var_filepath)
            
            shinyjs::show("main_variables_container") # Show variable inputs for main data
            shinyjs::hide("select_error_message")
            
            updateSelectInput(session,
                              'main_unique_obs_id_input',
                              choices = colnames(main_data),
                              selected = existing_variables$main$main_unique_obs_id)
            
            updateSelectInput(session,
                              'main_zone_id_input',
                              choices = colnames(main_data),
                              selected = existing_variables$main$main_zone_id)
            
            updateSelectInput(session,
                              'main_lon_input',
                              choices = find_lon(main_data),
                              selected = existing_variables$main$main_lon)
            
            updateSelectInput(session,
                              'main_lat_input',
                              choices = find_lat(main_data),
                              selected = existing_variables$main$main_lat)
            
            updateSelectInput(session,
                              'main_date_input',
                              choices = date_cols(main_data),
                              selected = existing_variables$main$main_date)
            
            # if doesn't exist, just show variables in main data
          } else if(!file.exists(saved_var_filepath) & !is.null(main_data)){
            shinyjs::show("main_variables_container")  # Show single file upload
            shinyjs::hide("select_error_message")
            
            updateSelectInput(session,
                              'main_unique_obs_id_input',
                              choices = colnames(main_data))
            
            updateSelectInput(session,
                              'main_zone_id_input',
                              choices = colnames(main_data))
            
            updateSelectInput(session,
                              'main_lon_input',
                              choices = find_lon(main_data))
            
            updateSelectInput(session,
                              'main_lat_input',
                              choices = find_lat(main_data))
            
            updateSelectInput(session,
                              'main_date_input',
                              choices = date_cols(main_data))
          }
        }
      })
      
      create_trip_haul_id_server("create_trip_haul_id",
                                 rv_project_name = rv_project_name,
                                 rv_data = rv_data)
      
      create_zone_id_server("create_zone_id",
                            rv_project_name = rv_project_name,
                            rv_data = rv_data)
      
      # return selected variables 
      return(
        reactive({
          list(
            main_unique_obs_id = input$main_unique_obs_id_input,
            main_zone_id = input$main_zone_id_input,
            main_lon = input$main_lon_input,
            main_lat = input$main_lat_input,
            main_date = input$main_date_input
          )
        })
      )
    })
} 

## Select variables from port data table ----------------------------------------------------------
## Description: Users can select variables from port data table where they can then be used 
##              throughout the app
select_port_var_server <- function(id, rv_project_name, rv_data) {
  moduleServer(
    id,
    function(input, output, session){
      
      ns <- session$ns
      
      observe({  
        req(rv_project_name()) # Check to ensure reactive is available
        project_name <- rv_project_name()$value
        req(rv_data) # Ensure data is not null
        port_data <- rv_data$port
        
        if (is.null(port_data)) {
          shinyjs::show("select_error_message")
          shinyjs::hide("port_variables_container")
          
        } else {
          # if saved variables already exist in project folder
          saved_var_file <- paste0(project_name, "SavedVariables.rds")
          saved_var_filepath <- file.path(loc_data(project_name), saved_var_file)
          saved_var_filepath <- suppressWarnings(normalizePath(saved_var_filepath))
          
          # if exists update the selectInput selections to show the existing variable
          if (file.exists(saved_var_filepath) & !is.null(port_data)) {
            existing_variables <- readRDS(saved_var_filepath)
            
            shinyjs::hide("select_error_message") # Shop inputs for port data
            shinyjs::show("port_variables_container")
            
            updateSelectInput(session,
                              "port_name_input",
                              choices =colnames(port_data),
                              selected = existing_variables$port$port_name)
            
            updateSelectInput(session,
                              "port_lon_input",
                              choices = find_lon(port_data),
                              selected = existing_variables$port$port_lon)
            
            updateSelectInput(session,
                              "port_lat_input",
                              choices =  find_lat(port_data),
                              selected = existing_variables$port$port_lat)
            
            # if doesn't exist, just show variables in port data
          } else if (!file.exists(saved_var_filepath) & !is.null(port_data)) {
            shinyjs::hide("select_error_message")
            shinyjs::show("port_variables_container")  
            
            updateSelectInput(session,
                              "port_name_input",
                              choices =colnames(port_data))
            
            updateSelectInput(session,
                              "port_lon_input",
                              choices = find_lon(port_data))
            
            updateSelectInput(session,
                              "port_lat_input",
                              choices =  find_lat(port_data))
          }
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
select_aux_var_server <- function(id, rv_project_name, rv_data) {
  moduleServer(
    id,
    function(input, output, session){
      
      ns <- session$ns
      
      observe({    
        req(rv_project_name()) # Check to ensure reactive is available
        project_name <- rv_project_name()$value
        req(rv_data) # Ensure data is not null
        aux_data <- rv_data$aux
        
        if (is.null(aux_data)) {
          shinyjs::show("select_error_message")
          shinyjs::hide("aux_variables_container")  
          
        } else {
          # if saved variables already exist in project folder
          saved_var_file <- paste0(project_name, "SavedVariables.rds")
          saved_var_filepath <- file.path(loc_data(project_name), saved_var_file)
          saved_var_filepath <- suppressWarnings(normalizePath(saved_var_filepath))
          
          # if exists update the selectInput selections to show the existing variable
          if (file.exists(saved_var_filepath) & !is.null(aux_data)) {
            existing_variables <- readRDS(saved_var_filepath)
            
            shinyjs::hide("select_error_message")
            shinyjs::show("aux_variables_container")
            
            updateSelectInput(session,
                              "aux_id_input",
                              choices =colnames(aux_data),
                              selected = existing_variables$aux$aux_id)
            
            # if doesn't exist, just show variables in aux data
          } else if (!file.exists(saved_var_filepath) & !is.null(aux_data)) {
            shinyjs::hide("select_error_message")
            shinyjs::show("aux_variables_container")
            
            updateSelectInput(session,
                              "aux_id_input",
                              choices =colnames(aux_data))
          } 
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

## Select variables from spat data table ----------------------------------------------------------
## Description: Users can select variables from spat data table where they can then be used 
##              throughout the app
select_spat_var_server <- function(id, rv_project_name, rv_data) {
  moduleServer(
    id,
    function(input, output, session){
      
      ns <- session$ns
      
      observe({
        req(rv_project_name()) # Check to ensure reactive is available
        project_name <- rv_project_name()$value
        req(rv_data) # Ensure data is not null
        spat_data <- rv_data$spat # Save static copy of spat data from reactive input
        
        if (is.null(spat_data)) {
          shinyjs::hide("spat_variables_container")  
          shinyjs::show("select_error_message")
          
        } else {
          # if saved variables already exist in project folder
          saved_var_file <- paste0(project_name, "SavedVariables.rds")
          saved_var_filepath <- file.path(loc_data(project_name), saved_var_file)
          saved_var_filepath <- suppressWarnings(normalizePath(saved_var_filepath)) 
          
          # if exists update the selectInput selections to show the existing variable
          if (file.exists(saved_var_filepath) & !is.null(spat_data)) {
            existing_variables <- readRDS(saved_var_filepath)
            
            shinyjs::show("spat_variables_container") # Show inputs for spat data 
            shinyjs::hide("select_error_message")
            
            updateSelectInput(session,
                              'spat_zone_id_input',
                              choices = colnames(spat_data), 
                              selected = existing_variables$spat$spat_zone_id)
            
            # if doesn't exist, just show variables in spat data
          } else if(!file.exists(saved_var_filepath) & !is.null(spat_data)) {
            shinyjs::show("spat_variables_container")  
            shinyjs::hide("select_error_message")
            
            updateSelectInput(session,
                              'spat_zone_id_input',
                              choices = colnames(spat_data))
          }
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

## Create trip/haul level ID ----------------------------------------------------------------------
## Description: Modal popup for users to create a trip/haul ID column.
create_trip_haul_id_server <- function(id, rv_project_name, rv_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Initialize reactive values
    rv_create_id_table <- reactiveVal() # For preview data
    
    # Show the main modal for creating an ID
    observeEvent(input$create_trip_haul_id_btn, {
      req(rv_data$main)
      
      showModal(
        modalDialog(
          title = "Create Trip/Haul ID Column",
          size = "m",
          
          selectInput(ns('select_nominal_id_input'),
                      'How would you like to create the ID?', 
                      choices = c('Based on variables' = 'create_id_input',
                                  'Based on row numbers' = 'create_id_seq_input'),
                      multiple = FALSE, 
                      selected = 'create_id_input'),
          
          textInput(ns('create_id_varname_input'),
                    'Name for new ID column',
                    placeholder = "e.g., trip_id"),
          
          # This part is shown/hidden based on the selection above
          shinyjs::hidden(
            div(id = ns("create_id_vars_container"),
                selectInput(ns("create_id_vars_input"), 
                            "Select 2 or more variables to combine",
                            choices = colnames(rv_data$main),
                            multiple = TRUE),
                
                selectizeInput(ns('create_id_type_input'), 
                               "Select ID column class type",
                               choices = c("string", "integer"))
            )
          ),
          
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("preview_id_btn"), "Preview & Save", class = "btn-primary")
          ),
          easyClose = TRUE
        )
      )
    })
    
    # Dynamically show/hide the variable selector in the modal
    observe({
      req(input$select_nominal_id_input)
      if (input$select_nominal_id_input == 'create_id_input') {
        shinyjs::show("create_id_vars_container")
      } else {
        shinyjs::hide("create_id_vars_container")
      }
    })
    
    # Handle the "Preview & Save" button click
    observeEvent(input$preview_id_btn, {
      id_type <- input$select_nominal_id_input
      id_varname <- input$create_id_varname_input
      main_data <- rv_data$main
      
      # Basic validation
      if (id_type == 'create_id_input' && (is.null(input$create_id_vars_input) || length(input$create_id_vars_input) < 2)) {
        showNotification("Please select 2 or more variables to combine.", type = "error")
        return()
      }
      
      # Create data
      new_data_output <- NULL
      if (id_type == 'create_id_input') {
        vars_in <- input$create_id_vars_input
        q_test <- quietly_test(ID_var)
        new_data_output <- q_test(main_data, project = rv_project_name()$value, name = id_varname, 
                                  vars = vars_in, type = input$create_id_type_input)
      } else if (id_type == 'create_id_seq_input') {
        q_test <- quietly_test(ID_var)
        # Row number ID is always integer, no type input needed for this choice
        new_data_output <- q_test(main_data, project = rv_project_name()$value, name = id_varname, 
                                  vars = NULL, type = "integer")
      }
      
      if (!is.null(new_data_output)) {
        rv_create_id_table(new_data_output)
        
        # Show preview modal
        showModal(
          modalDialog(
            title = "Preview New ID: Confirm & Save",
            style = "overflow-x: auto; white-space: nowrap;",
            size = "l",
            DT::DTOutput(ns("create_id_table_preview")),
            footer = tagList(
              modalButton("Back"),
              actionButton(ns("confirm_save_id_btn"), "Confirm & Save", class = "btn-secondary")
            ),
            easyClose = TRUE
          )
        )
      }
    })
    
    # Render the preview table
    output$create_id_table_preview <- DT::renderDT(
      head(rv_create_id_table())
    )
    
    # Handle the final "Confirm & Save" button
    observeEvent(input$confirm_save_id_btn, {
      req(rv_project_name(), rv_data, rv_create_id_table())
      
      rv_data$main <- rv_create_id_table()
      
      load_maindata(dat = rv_data$main, project = rv_project_name()$value, over_write = TRUE)
      
      removeModal() # Closes preview modal
      removeModal() # Closes main modal
      
      showNotification("Trip/Haul ID created and saved successfully.", type = "message")
    })
    
  })
}

## Create zone ID column --------------------------------------------------------------------------
## Description: Modal popup for users to create a zone ID column by merging the main data with 
##              spatial grid.
create_zone_id_server <- function(id, rv_project_name, rv_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input$create_zone_id_btn, {
      req(rv_data$main, rv_data$spat)
      
      showModal(
        modalDialog(
          title = "Create Zone ID Column",
          size = "m",
          
          selectInput(ns("modal_main_lon"),
                      HTML("Select longitude from <strong>main data</strong>"),
                      choices = find_lon(rv_data$main),
                      selected = find_lon(rv_data$main)[1]),
          
          selectInput(ns("modal_main_lat"),
                      HTML("Select latitude from <strong>main data</strong>"),
                      choices = find_lat(rv_data$main),
                      selected = find_lat(rv_data$main)[1]),
          
          selectInput(ns("modal_spat_zone"),
                      HTML("Select zone ID from <strong>spatial data</strong>"),
                      choices = colnames(rv_data$spat)),
          
          textInput(ns("modal_new_name"),
                    "Enter name for new zone ID column",
                    value = ""),
          
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_create_zone_btn"), "Create ID", class = "btn-primary")
          ),
          easyClose = TRUE
        )
      )
    })
    
    observeEvent(input$confirm_create_zone_btn, {
      req(rv_project_name(), rv_data$main, rv_data$spat)
      
      project_name <- rv_project_name()$value
      
      tryCatch({
        # Call the function to assign zones and create the new column
        updated_main_data <- assignment_column(
          dat = rv_data$main,
          project = project_name,
          spat = rv_data$spat,
          lon.dat = input$modal_main_lon,
          lat.dat = input$modal_main_lat,
          cat = input$modal_spat_zone,
          name = input$modal_new_name,
        )
        
        # Update the reactive data frame with the new data
        rv_data$main <- updated_main_data
        
        # Save the updated data frame back to the project's database
        load_maindata(dat = rv_data$main,
                      project = project_name,
                      over_write = TRUE)
        
        # Close the modal and show success/failure notifications
        removeModal()
        
        # Show success message
        showNotification("Zone ID created and saved successfully.", type = "message")
        
      }, error = function(e) {
        # If an error occurs
        removeModal()
        
        # Show a new modal with the error message
        showModal(
          modalDialog(
            title = tagList(shiny::icon("circle-xmark", style = "color: red;"), " Error"),
            p("An error occurred while creating the Zone ID column:"),
            tags$b(e$message),
            footer = modalButton("Close"),
            easyClose = TRUE
          )
        )
      })
    })
  })
}

## Save variables to project folder --------------------------------------------------------------
## Description: Users can save variables from all data tables so they can be used in future 
##              sessions
save_var_server <- function(id, rv_project_name, rv_data, parent_session){
  moduleServer(
    id,
    function(input, output, session){
      
      ns <- session$ns
      
      # Initialize reactives
      rv_var_error_message <- reactiveVal("") # Store error message
      rv_var_success_message <- reactiveVal("") # Store success message
      rv_selected_variables <- reactiveValues() # All selected variables
      rv_nominal_id_type <- reactiveValues() # type of trip/haul id
      
      # Outputs for error and success messages - initially hidden
      output$var_error_message_out <- renderText({
        rv_var_error_message()
      })
      output$var_success_message_out <- renderText({
        rv_var_success_message()
      })
      
      #### Select main data variables
      rv_selected_variables$main <- select_main_var_server("selecting_main",
                                                           rv_project_name = rv_project_name,
                                                           rv_data = rv_data)
      #### Select spat data variables
      rv_selected_variables$spat <- select_spat_var_server("selecting_spat",
                                                           rv_project_name = rv_project_name,
                                                           rv_data = rv_data)
      #### Select port data variables (optional)
      rv_selected_variables$port <- select_port_var_server("selecting_port",
                                                           rv_project_name = rv_project_name,
                                                           rv_data = rv_data)
      #### Select aux data variables (optional)
      rv_selected_variables$aux <-  select_aux_var_server("selecting_aux",
                                                          rv_project_name = rv_project_name,
                                                          rv_data = rv_data)
      
      # #### Create haul/trip level ID (if needed)
      # rv_nominal_id_type <- create_nominal_id_server(
      #   "nominal_id",
      #   rv_project_name = rv_project_name,
      #   rv_data = rv_data,
      #   rv_selected_variables = rv_selected_variables)
      # 
      # create_nominal_id_inputs_server(
      #   "nominal_id_vars",
      #   rv_project_name = rv_project_name,
      #   rv_data = rv_data,
      #   rv_selected_variables = rv_selected_variables,
      #   rv_nominal_id_type = rv_nominal_id_type)
      
      observeEvent(input$save_vars_btn, {
        req(rv_project_name()) # Check to ensure reactive is available
        project_name <- rv_project_name()$value
        req(rv_selected_variables)
        
        # Overlay spinner
        shinyjs::show("save_var_spinner_container")
        
        saved_variables_main <- rv_selected_variables$main()
        saved_variables_port <- rv_selected_variables$port()
        saved_variables_spat <- rv_selected_variables$spat()
        
        saved_variables <- list(main = saved_variables_main,
                                spat = saved_variables_spat,
                                port = saved_variables_port)
        
        # Save .rds file
        tab_name <- paste0(project_name, "SavedVariables.rds")
        file_names <- file.path(loc_data(project_name), tab_name)
        saveRDS(saved_variables, file = file_names)
        
        rv_var_success_message("Variables are loaded and saved in project data folder.")
        shinyjs::show("var_success_message")
        shinyjs::hide("var_error_message")
        
        ### Zonal centroid ------------------------------------------------------------------------
        cent_table_name <- paste0(project_name, "_ZoneCentroid")
        
        # Create centroid table if it does not exist
        if (!table_exists(cent_table_name, project_name)) {
          q_test_centroid <- quietly_test(create_centroid, show_msg = FALSE)
          q_test_centroid(spat = rv_data$spat,
                          dat = rv_data$main,
                          project = project_name,
                          spatID = saved_variables_spat$spat_zone_id,
                          cent.name = "_",
                          output = "centroid table")
        }
        
        # Hide local spinner
        shinyjs::hide("save_var_spinner_container")
      })
      
      # Next button to move user to quality checks sub-tab
      observeEvent(input$select_var_next_btn, {
        bslib::nav_show(
          id = "tabs", 
          target = "quality_checks", 
          select = TRUE,
          session = parent_session
        )
      })
    }
  )
}




