# =================================================================================================
# File: model_design_module.R
# Description: This module defines the UI and server logic for creating Model Design objects.
#              It constructs the design matrices (X) and choice vector (y) required for
#              discrete choice modeling.
#              
# Dependencies: shiny, DT, shinyjs, bslib
# Notes: This module interacts with [Project]LongFormatData (input) and 
#        [Project]ModelDesigns (output).
# =================================================================================================

# model design server -------------------------------------------------------------------------
#' model_design_server
#'
#' @param id A character string that is unique to this module instance.
#' @param rv_project_name A reactive value containing the current project name.
#'
#' @return This module does not return a value.
model_design_server <- function(id, rv_project_name) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive value to store names of existing designs
    rv_existing_design_names <- reactiveVal(character(0))
    
    # Reactive to store the currently loaded formatted dataframe (for column extraction)
    rv_current_formatted_data <- reactiveVal(NULL)
    
    # 1. Load Manage Table Data ----------------------------------------------------------------
    load_designs <- function() {
      req(rv_project_name())
      project <- rv_project_name()$value
      table_name <- paste0(project, "ModelDesigns")
      
      just_names <- character(0)
      
      if (table_exists(table_name, project)) {
        # Load the object from DB
        full_designs <- tryCatch({
          unserialize_table(table_name, project)
        }, error = function(e) {
          return(list()) 
        })
        
        if (length(full_designs) > 0) {
          just_names <- names(full_designs)
        }
      }
      
      # Update reactive value
      rv_existing_design_names(just_names)
      
      # Update removal dropdown
      updateSelectizeInput(session, "design_to_remove", choices = just_names, selected = "")
    }
    
    # Load data on init
    observeEvent(rv_project_name(), {
      load_designs()
    })
    
    # 2. Input Updates (Dropdowns) -------------------------------------------------------------
    
    # Update Formatted Data Dropdown
    observeEvent(rv_project_name(), {
      req(rv_project_name())
      project <- rv_project_name()$value
      
      # Look for available formatted datasets
      data_table_name <- paste0(project, "LongFormatData")
      choices <- c()
      
      if (table_exists(data_table_name, project)) {
        data_list <- tryCatch({
          unserialize_table(data_table_name, project)
        }, error = function(e) list())
        
        # Filter keys ending in "_settings" to get just the data names
        all_keys <- names(data_list)
        choices <- all_keys[!grepl("_settings$", all_keys)]
      }
      
      updateSelectizeInput(session, "formatted_data_input", choices = choices, selected = "")
    })
    
    # Update Column Selectors based on chosen Formatted Data
    observeEvent(input$formatted_data_input, {
      req(rv_project_name(), input$formatted_data_input)
      project <- rv_project_name()$value
      data_name <- input$formatted_data_input
      
      # Load the specific dataframe to get column names
      # We do this lightly just to get colnames
      tryCatch({
        full_lf_list <- unserialize_table(paste0(project, "LongFormatData"), project)
        if (data_name %in% names(full_lf_list)) {
          df <- full_lf_list[[data_name]]
          rv_current_formatted_data(df)
          
          cols <- colnames(df)
          
          # Attempt to guess IDs based on common names
          guess_obs <- cols[grepl("haul|trip|id|obs", tolower(cols)) & !grepl("zone", tolower(cols))][1]
          guess_zone <- cols[grepl("zone", tolower(cols))][1]
          
          updateSelectizeInput(session, "unique_obs_id_input", choices = cols, selected = guess_obs)
          updateSelectizeInput(session, "zone_id_input", choices = cols, selected = guess_zone)
          
          # Also help the user by listing available variables for the formula
          output$avail_vars_list <- renderText({
            paste("Available variables:", paste(cols, collapse = ", "))
          })
        }
      }, error = function(e) {
        showNotification("Error loading dataset columns.", type = "error")
      })
    })
    
    # 3. Execution Logic (Run Design) ----------------------------------------------------------
    observeEvent(input$run_design_btn, {
      req(rv_project_name())
      project_name <- rv_project_name()$value
      
      # Validation
      if (input$model_name_input == "") {
        showNotification("Please enter a Model Name.", type = "warning")
        return()
      }
      if (input$formatted_data_input == "") {
        showNotification("Please select a Formatted Dataset.", type = "warning")
        return()
      }
      if (input$formula_input == "") {
        showNotification("Please enter a Formula.", type = "warning")
        return()
      }
      
      # UI State
      shinyjs::hide("design_success_message")
      shinyjs::hide("design_error_message")
      shinyjs::show("run_design_spinner_container")
      shinyjs::disable("run_design_btn")
      
      tryCatch({
        # Convert string input to formula
        form_str <- input$formula_input
        # Basic check to ensure it's a formula string
        if (!grepl("~", form_str)) {
          stop("Invalid formula format. Must contain '~'.")
        }
        
        # Run the main function
        fishset_design(
          formula = as.formula(form_str),
          project = project_name,
          model_name = input$model_name_input,
          formatted_data_name = input$formatted_data_input,
          unique_obs_id = input$unique_obs_id_input,
          zone_id = input$zone_id_input
        )
        
        # Success Feedback
        output$design_success_out <- renderText({
          paste0("Success: Design '", input$model_name_input, "' created and saved.")
        })
        shinyjs::show("design_success_message")
        
        # Reload Table
        load_designs()
        
      }, error = function(e) {
        output$design_error_out <- renderText({
          paste("Error:", e$message)
        })
        shinyjs::show("design_error_message")
        
      }, finally = {
        shinyjs::hide("run_design_spinner_container")
        shinyjs::enable("run_design_btn")
      })
    })
    
    # 4. Manage Table Logic --------------------------------------------------------------------
    output$existing_designs_table <- DT::renderDataTable({
      d_names <- rv_existing_design_names()
      
      if (length(d_names) == 0) {
        return(DT::datatable(
          data.frame(Name = character(0), Actions = character(0)),
          caption = "No Design Objects found."
        ))
      }
      
      actions <- sapply(d_names, function(name) {
        as.character(
          tags$button(
            class = "btn btn-secondary btn-sm",
            onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", 
                              ns("view_design_trigger"), name),
            "View Details"
          )
        )
      })
      
      df <- data.frame(
        Name = d_names,
        Actions = actions,
        stringsAsFactors = FALSE
      )
      
      DT::datatable(df,
                    options = list(pageLength = 5, searching = TRUE, dom = 'tp',
                                   columnDefs = list(list(orderable = FALSE, targets = 1))),
                    rownames = FALSE,
                    escape = FALSE,
                    selection = 'none',
                    caption = "Existing Design Objects")
    })
    
    # View Details Modal
    observeEvent(input$view_design_trigger, {
      selected_name <- input$view_design_trigger
      project_name <- rv_project_name()$value
      
      # Load Designs
      designs <- tryCatch({
        unserialize_table(paste0(project_name, "ModelDesigns"), project_name)
      }, error = function(e) return(NULL))
      
      if (is.null(designs) || is.null(designs[[selected_name]])) {
        showNotification("Could not load details.", type = "error")
        return()
      }
      
      d_obj <- designs[[selected_name]]
      
      # Extract info for display
      f_text <- deparse(d_obj$formula)
      dims <- d_obj$settings
      
      # UI for Modal
      details_ui <- tagList(
        tags$b("Formula:"), pre(paste(f_text, collapse = " ")),
        hr(),
        tags$div(
          style = "overflow-x: auto;",
          tags$table(class = "table table-sm table-striped",
                     tags$thead(tags$tr(tags$th("Metric"), tags$th("Value"))),
                     tags$tbody(
                       tags$tr(tags$td("Formatted Data Source"), tags$td(dims$formatted_data_name)),
                       tags$tr(tags$td("Observations (N)"), tags$td(dims$N_obs)),
                       tags$tr(tags$td("Alternatives (J)"), tags$td(dims$J_alts)),
                       tags$tr(tags$td("Parameters (K)"), tags$td(dims$K_vars)),
                       tags$tr(tags$td("Obs ID Column"), tags$td(dims$unique_obs_id)),
                       tags$tr(tags$td("Zone ID Column"), tags$td(dims$zone_id))
                     )
          )
        )
      )
      
      showModal(modalDialog(
        title = paste("Design Details:", selected_name),
        details_ui,
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
    
    # Remove Design Logic
    observeEvent(input$remove_design_btn, {
      req(input$design_to_remove)
      showModal(modalDialog(
        title = "Confirm Removal",
        paste("Are you sure you want to permanently remove the design:", 
              input$design_to_remove, "?"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_remove_btn"), "Remove", class = "btn-danger")
        )
      ))
    })
    
    observeEvent(input$confirm_remove_btn, {
      removeModal()
      target_name <- input$design_to_remove
      project_name <- rv_project_name()$value
      table_name <- paste0(project_name, "ModelDesigns")
      
      tryCatch({
        fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project_name))
        on.exit({ if(DBI::dbIsValid(fishset_db)) DBI::dbDisconnect(fishset_db) })
        
        if(table_exists(table_name, project_name)){
          existing <- unserialize_table(table_name, project_name)
          if(target_name %in% names(existing)){
            existing[[target_name]] <- NULL
          }
          
          # Rewrite table
          table_remove(table_name, project_name)
          
          if(length(existing) > 0){
            DBI::dbExecute(fishset_db, paste("CREATE TABLE", table_name, "(data BLOB)"))
            DBI::dbExecute(fishset_db, paste("INSERT INTO", table_name, "VALUES (:data)"),
                           params = list(data = list(serialize(existing, NULL))))
          }
          
          load_designs()
          showNotification("Design removed.", type = "message")
        }
      }, error = function(e) {
        showNotification(paste("Error removing:", e$message), type = "error")
      })
    })
    
  })
}

# model design UI -----------------------------------------------------------------------------
#' model_design_ui
#'
#' @param id A character string that is unique to this module instance.
#' @return A tagList containing the sidebar UI elements.
model_design_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    
    div(id = ns("main_container"),
        
        # CARD 1: Create Design Object
        bslib::card(
          class = "card-overflow",
          bslib::card_header('Create Model Design'),
          bslib::card_body(
            class = "card-overflow",
            p("This module constructs the design matrices required for discrete choice modeling using
               formatted FishSET data."),
            
            bslib::layout_column_wrap(
              fill = FALSE,
              width = 1/2, 
              gap = "1rem",
              
              # 1. Source and Naming
              bslib::card(
                class = "card-overflow",
                bslib::card_header(h5("1. Source Data & Naming", class = "mb-0")),
                bslib::card_body(
                  class = "card-overflow d-flex flex-column gap-3",
                  
                  textInput(ns("model_name_input"), 
                            label = tags$span("Model Design Name ", 
                                              bslib::tooltip(shiny::icon("info-circle"),
                                                             "Unique name for this model design configuration.")), 
                            placeholder = "e.g., clogit_base_design", width = "100%"),
                  
                  selectizeInput(ns("formatted_data_input"), 
                                 label = tags$span("Input Formatted Data ", 
                                                   bslib::tooltip(shiny::icon("info-circle"),
                                                                  "Select a dataset created in the Format Model Data module.")),
                                 choices = NULL, width = "100%"),
                  
                  div(class = "row",
                      div(class = "col-md-6",
                          selectizeInput(ns("unique_obs_id_input"), "Observation ID Column", choices = NULL, width = "100%")
                      ),
                      div(class = "col-md-6",
                          selectizeInput(ns("zone_id_input"), "Zone/Alternative ID Column", choices = NULL, width = "100%")
                      )
                  )
                )
              ),
              
              # 2. Model Specification
              bslib::card(
                class = "card-overflow",
                bslib::card_header(h5("2. Model Specification", class = "mb-0")),
                bslib::card_body(
                  class = "card-overflow d-flex flex-column gap-3",
                  
                  textAreaInput(ns("formula_input"), 
                                label = tags$span("Formula ", 
                                                  bslib::tooltip(shiny::icon("info-circle"),
                                                                 "Use '|' to separate alternative-specific vars from individual-specific vars.")),
                                placeholder = "chosen ~ catch + distance | income", 
                                rows = 3, width = "100%"),
                  
                  helpText(tags$small(
                    "Format: ", tags$code("LHS ~ Alt_Vars | Ind_Vars"), br(),
                    "Note: LHS is usually 'chosen'. Ind_Vars are interacted with zones automatically."
                  )),
                  
                  div(class = "text-muted", style = "font-size: 0.8rem;",
                      textOutput(ns("avail_vars_list")))
                )
              )
            ),
            
            # Run Button
            fluidRow(
              column(6, style = "margin-top: 25px;",
                     actionButton(ns("run_design_btn"), "Create Design Object", 
                                  icon = icon("layer-group"), class = "btn-secondary", width = "100%")
              )
            ),
            
            # Spinner & Messages
            div(id = ns("run_design_spinner_container"),
                style = "display: none; margin-top: 15px;",
                spinner_ui(ns("run_design_spinner"), spinner_type = "circle", 
                           message = "Generating Design Matrices...", overlay = TRUE)
            ),
            div(id = ns("design_success_message"), 
                style = "color: green; display: none; margin-top: 10px;",
                textOutput(ns("design_success_out"))),
            div(id = ns("design_error_message"), 
                style = "color: red; display: none; margin-top: 10px;", 
                textOutput(ns("design_error_out")))
          )
        ),
        
        # CARD 2: Manage Designs
        bslib::card(
          class = "card-overflow",
          bslib::card_header("Manage Design Objects"),
          bslib::card_body(
            class = "card-overflow",
            DT::dataTableOutput(ns("existing_designs_table"), fill = FALSE),
            hr(),
            fluidRow(
              column(8,
                     selectizeInput(ns("design_to_remove"), "Select design to remove:", choices = NULL, width = "100%")
              ),
              column(4, style = "margin-top: 25px;",
                     actionButton(ns("remove_design_btn"), "Remove Selected",
                                  icon = icon("trash"), class = "btn-danger w-100")
              )
            )
          )
        )
    )
  )
}