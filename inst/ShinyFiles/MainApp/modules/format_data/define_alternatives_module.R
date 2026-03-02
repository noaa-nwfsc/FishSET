# =================================================================================================
# File: define_alternatives_module.R
# Description: This module defines the UI and server logic for defining alternative fishing choices.
#              It manages a list of alternative choice sets stored in a single database table.
#
# Authors: Anna Abelman,  Paul Carvalho
# Updated: 11/20/2025
# Dependencies: shiny, DT, RSQLite, DBI, shinyjs, bslib
# =================================================================================================

# define alternatives server ----------------------------------------------------------------------
#' define_alt_server
#'
#' @param id A character string that is unique to this module instance.
#' @param rv_project_name A reactive value containing the current project name.
#' @param rv_data A reactiveValues object containing the loaded data frames.
#' @param rv_shared_alt_names A reactiveVal passed from the main server to share alt matrix names.
#'
#' @return This module does not return a value.
define_alt_server <- function(id, rv_folderpath, rv_project_name, rv_data, 
                              rv_shared_alt_names = NULL){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Reactive value to store strictly the NAMES (Character Vector)
    rv_existing_matrix_names <- reactiveVal(character(0))
    rv_selected_vars <- reactiveValues(vars = NULL)
    
    # Load List Names from DB ---
    load_alt_data <- function() {
      req(rv_project_name())
      project <- rv_project_name()$value
      table_name <- paste0(project, "AltMatrix")
      
      # Default to empty
      just_names <- character(0)
      
      if (table_exists(table_name, project)) {
        # Load the object from DB
        full_data <- tryCatch({
          unserialize_table(table_name, project)
        }, error = function(e) {
          # Log to console but don't crash app
          message(paste("Error loading table:", e$message)) 
          return(list()) 
        })
        
        # Extract NAMES only
        if (length(full_data) > 0) {
          just_names <- names(full_data)
        }
      }
      
      # Update reactive value with NAMES only
      rv_existing_matrix_names(just_names)
      
      # Update SHARED value (for the other module)
      if (!is.null(rv_shared_alt_names)) {
        rv_shared_alt_names(just_names) 
      }
      
      # Update removal dropdown
      updateSelectizeInput(session, "matrix_to_remove", choices = just_names, selected = "")
    }
    
    # Load data on init
    observeEvent(rv_data$main, {
      load_alt_data()
    }, once = TRUE)
    
    # Update dropdowns based on loaded data columns
    observe({
      req(rv_data$main)
      updateSelectInput(session, "occ_lon_input", choices = find_lon(rv_data$main))
      updateSelectInput(session, "occ_lat_input", choices = find_lat(rv_data$main))
      updateSelectInput(session, "occ_lag_zone_id_input", choices = colnames(rv_data$main))
      updateSelectInput(session, "occ_port_name_input", choices = colnames(rv_data$main))
      
    })
    
    
    # Save choice set
    observeEvent(input$altc_save_btn, {
      project_name <- rv_project_name()$value
      folderpath <- rv_folderpath()
      req(rv_data$main)
      req(rv_data$spat)
      
      alt_name_input <- input$altc_name_input
      
      # Validation
      if (alt_name_input == "") {
        showNotification("Please provide a name for the new Alternative Choice matrices",
                         type = "error")
        return()
      }
      if (alt_name_input %in% rv_existing_matrix_names()) {
        showNotification("Name already exists. Please choose a unique name.", type = "error")
        return()
      }
      
      # Show Spinner
      shinyjs::show("define_alt_spinner_container")
      on.exit(shinyjs::hide("define_alt_spinner_container"), add = TRUE)
      
      
      # Load selected variables
      selected_vars <- load_gui_variables(project_name, folderpath)
      if (is.null(selected_vars)) {
        # Handle the case where the RDS file does not exist.
        shinyjs::hide("define_alt_spinner_container")
        showModal(modalDialog(
          title = "Error: Missing Data",
          "The selected variables file for the current project could not be found.
          Please ensure you have selected and saved variables in the 'Select variables' tab.",
          easyClose = TRUE
        ))
        return() # Stop execution of the observer
      }
      rv_selected_vars$vars <- selected_vars
      
      # Helper for occasion type mapping
      occ_type <- switch(input$altc_occasion_input,
                         'port' = 'port',
                         'zonal centroid' = 'zonal centroid',
                         'lon-lat' = 'lon-lat')
      
      # Run Function Wrapper
      q_test <- quietly_test(create_alternative_choice, show_msg = TRUE)
      
      # Add specific occasion variables based on selection
      if(input$altc_occasion_input == "zonal centroid"){
        occasion_var_input <- input$occ_lag_zone_id_input
      } else if(input$altc_occasion_input == "lon-lat"){
        occasion_var_input <- c(input$occ_lon_input, input$occ_lat_input)
      } else if(input$altc_occasion_input == "port"){
        occasion_var_input <- input$occ_port_name_input
      }
      
      q_test(
        dat = rv_data$main,
        project = project_name,
        alt_name = alt_name_input,
        zoneID = rv_selected_vars$vars$main$main_zone_id,
        occasion = occ_type,
        occasion_var=occasion_var_input,
        alt_var = "zonal centroid",
        min_haul = input$altc_min_haul_input,
        spatID = rv_selected_vars$vars$spat$spat_zone_id)
      
      # Refresh UI (reloads names from DB)
      load_alt_data()
      
      # Reveal the hidden plot container
      shinyjs::show("altc_zone_plot_wrapper")
    })
    
    # zone freq table
    zone_freq <- reactive({
      req(input$altc_save_btn)
      req(rv_data$main)
      req(input$altc_min_haul_input)
      req(rv_selected_vars$vars$main$main_zone_id)
      
      freq_tab <- agg_helper(rv_data$main, value = rv_selected_vars$vars$main$main_zone_id,
                             count = TRUE, fun = NULL)
      freq_tab$include <- freq_tab$n >= input$altc_min_haul_input
      freq_tab
    })
    
    # barplot of zone freq
    zoneIDNumbers_dat <- reactive({
      req(rv_data$main)
      req(zone_freq())
      req(input$altc_min_haul_input)
      req(rv_selected_vars$vars$main$main_zone_id)
      
      dat <- zone_freq()
      
      z_sym <- rlang::sym(rv_selected_vars$vars$main$main_zone_id)
      
      tmp <- dat[which(dat$include),]
      
      # Sort by 'n' in descending order
      tmp <- tmp[order(tmp$n, decreasing = TRUE), ]
      
      # Slice the top 20 (head handles cases < 20 gracefully)
      tmp <- head(tmp, 20)
      
      
      ggplot2::ggplot(tmp) +
        ggplot2::geom_col(ggplot2::aes(x=stats::reorder(as.factor(!!z_sym), n), y = n),
                          fill = "darkgreen") +
        ggplot2::labs(x = "Zone ID", y = "n") +
        ggplot2::scale_y_continuous(expand = c(0,0)) +
        fishset_theme() +
        ggplot2::coord_flip()+
        ggplot2::labs(
          caption= paste0("Only showing the 20 largest groups.")
        )
    })
    
    output$altc_zone_plot <- renderPlot(zoneIDNumbers_dat())
    
    # render table with names of choice sets 
    output$existing_matrices_table <- DT::renderDataTable({
      # 1. Retrieve the character vector of names ---------------
      alt_names <- rv_existing_matrix_names()
      
      # 2. Handle Empty Case ----------------------
      if (length(alt_names) == 0) {
        return(DT::datatable(
          data.frame(Name = character(0), Actions = character(0)),
          caption = "No Alternative Choice Matrices found."
        ))
      }
      
      # 3. Create buttons with embedded JS onclick events ----------------
      # We send the specific 'name' to input$view_settings_trigger
      actions <- sapply(alt_names, function(name) {
        as.character(
          tags$button(
            class = "btn btn-secondary btn-sm",
            onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", 
                              ns("view_settings_trigger"), name),
            "View Settings"
          )
        )
      })
      
      # 4. Force structure into a Data Frame -----------
      # This ensures 'Name' is a column, preventing the list-display bug
      df <- data.frame(
        Name = alt_names,
        Actions = actions,
        stringsAsFactors = FALSE
      )
      
      # 5. Render ----------
      DT::datatable(df,
                    options = list(pageLength = 3, searching = TRUE, dom = 'tp',
                                   scrollX = TRUE,
                                   columnDefs = list(list(orderable = FALSE, targets = 1))
                    ),
                    rownames = FALSE,
                    escape = FALSE, # Essential for HTML buttons
                    selection = 'none',
                    caption = "Existing Alternative Choice Matrices")
    })
    
    # Triggered by the JS in the table buttons
    observeEvent(input$view_settings_trigger, {
      selected_name <- input$view_settings_trigger
      project_name <- rv_project_name()$value
      table_name_db <- paste0(project_name, "AltMatrix")
      
      # 1. Load the master list from DB  ---------------------
      master_list <- tryCatch({
        unserialize_table(table_name_db, project_name)
      }, error = function(e) return(NULL))
      
      if (is.null(master_list) || is.null(master_list[[selected_name]])) {
        showNotification("Could not load settings for this item.", type = "error")
        return()
      }
      
      # 2. Extract specific sub-list ---------------------
      settings_list <- master_list[[selected_name]]
      
      # 3. Format values for display (handle lists/dfs/vectors) --------------------
      format_val <- function(x) {
        if (is.data.frame(x)) {
          return(paste0("[Data Frame] ", nrow(x), " rows"))
        } else if (is.list(x)) {
          return(paste0("[List] ", length(x), " elements"))
        } else if (is.null(x)) {
          return("NULL")
        } else if (length(x) > 10) {
          return(paste0("[Vector] ", length(x), " items"))
        } else {
          return(paste(as.character(x), collapse = ", "))
        }
      }
      
      # 4. Construct Table UI ------------------------
      settings_ui <- tags$div(
        style = "overflow-x: auto;",
        tags$table(class = "table table-sm table-striped",
                   tags$thead(tags$tr(tags$th("Parameter"), tags$th("Value"))),
                   tags$tbody(
                     lapply(names(settings_list), function(p_name) {
                       val <- settings_list[[p_name]]
                       tags$tr(
                         tags$td(tags$b(p_name)),
                         tags$td(format_val(val))
                       )
                     })
                   )
        )
      )
      
      # 5. Show Modal ----------------
      showModal(modalDialog(
        title = paste("Settings for:", selected_name),
        settings_ui,
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
    
    # remove matrix option
    observeEvent(input$remove_matrix_btn, {
      req(input$matrix_to_remove)
      showModal(modalDialog(
        title = "Confirm Removal",
        paste("Are you sure you want to permanently remove the matrix:", 
              input$matrix_to_remove, "?"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_remove_btn"), "Remove", class = "btn-danger")
        )
      ))
    })
    
    observeEvent(input$confirm_remove_btn, {
      removeModal()
      target_name <- input$matrix_to_remove
      project_name <- rv_project_name()$value
      table_name_db <- paste0(project_name, "AltMatrix")
      
      tryCatch({
        fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project_name))
        on.exit({ if(DBI::dbIsValid(fishset_db)) DBI::dbDisconnect(fishset_db) })
        
        if(table_exists(table_name_db, project_name)){
          master_list <- unserialize_table(table_name_db, project_name)
          
          if(target_name %in% names(master_list)){
            # Remove item from list
            master_list[[target_name]] <- NULL
            
            # Remove old table
            table_remove(table_name_db, project_name)
            
            # Only recreate table if list is not empty
            if(length(master_list) > 0){
              DBI::dbExecute(fishset_db, paste("CREATE TABLE", table_name_db,
                                               "(AlternativeMatrix BLOB)"))
              DBI::dbExecute(fishset_db, paste("INSERT INTO", table_name_db, 
                                               "(AlternativeMatrix) VALUES (:AlternativeMatrix)"),
                             params = list(AlternativeMatrix = list(serialize(master_list, NULL))))
            }
            
            load_alt_data()
            showNotification("Removed successfully.", type = "message")
          }
        }
      }, error = function(e) {
        showNotification(paste("Error removing:", e$message), type = "error")
      })
    })
  })
}


# Define alternative sidebar ---------------------------------------------------------------------
#' define_alt_ui
#'
#' @param id A character string that is unique to this module instance.
#' @return A tagList containing the sidebar UI elements.
define_alt_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    div(id = ns("main_container"),
        bslib::card(
          class="card-overflow", 
          bslib::card_header('Create Alternative Fishing Choices'),
          bslib::card_body(
            class="card-overflow", 
            p("For more information on defining alternative location matrices, see ",
              tags$a(
                href = "https://noaa-nwfsc.github.io/FishSET/articles/FishSET_User_Manual.html",
                "section 8.2.2"),
              "in the FishSET User Manual."),
            
            # --- Inputs Card ---
            bslib::layout_column_wrap( 
              width = 1/2,
              bslib::card(
                class="card-overflow",
                height = "100%", # Make card fill its parent column
                bslib::card_header(h5("1. Starting location", class = "mb-0")),
                bslib::card_body(
                  class="card-overflow d-flex flex-column",
                  selectInput(
                    ns('altc_occasion_input'),
                    tagList(
                      span(
                        style = 
                          "white-space: wrap; display: inline-flex; align-items: center;",
                        HTML("Select occasion type: &nbsp;"),
                        bslib::tooltip(
                          shiny::icon("circle-info", `aria-label` = "More information"),
                          HTML("For trip-level data, select port as starting location. For 
                          haul-level data, select lagged haul location. Using lat/lon coordinates 
                          for starting location could either be trip or haul-level data. For 
                          trip-level give coordinates for starting location, while for haul-level 
                          data coordinates should be previous/lagged."),
                          options = list(delay = list(show = 0, hide = 850))
                        )
                      )
                    ),
                    choices=c('Port' = 'port', 
                              'Lagged haul location' = 'zonal centroid', 
                              'Lon-Lat Coordinates' = 'lon-lat')),
                  
                  conditionalPanel("input.altc_occasion_input=='lon-lat'", ns = ns,
                                   selectInput(ns("occ_lon_input"), "Select longitude:", 
                                               choices = NULL),
                                   selectInput(ns("occ_lat_input"), "Select latitude:", 
                                               choices = NULL)),
                  
                  conditionalPanel("input.altc_occasion_input=='zonal centroid'", ns = ns,
                                   selectInput(ns("occ_lag_zone_id_input"), 
                                               "Select lagged location for multi-haul trips:",
                                               choices = NULL)),
                  
                  conditionalPanel(
                    "input.altc_occasion_input=='port'", ns = ns,
                    selectInput(
                      ns("occ_port_name_input"), 
                      tagList(
                        span(
                          style = 
                            "white-space: wrap; display: inline-flex; align-items: center;",
                          HTML("Select port name: &nbsp;"),
                          bslib::tooltip(
                            shiny::icon("circle-info", `aria-label` = "More information"),
                            HTML("Must select a port name variable in main data table that 
                               identified port of departure"),
                            options = list(delay = list(show = 0, hide = 850))
                          )
                        )
                      ),
                      choices = NULL))
                )
              ),
              
              bslib::card(
                class="card-overflow",
                height = "100%", # Make card fill its parent column
                bslib::card_header(h5("2. Set threshold for inclusion of alternatives", 
                                      class = "mb-0")),
                bslib::card_body(
                  class="card-overflow d-flex flex-column",
                  numericInput(ns('altc_min_haul_input'), 
                               'Include zones with more observations than:', 
                               min = 1, max = 1000, value = 1)
                )
              )
            )
          ),
          
          # Save Section ---
          bslib::layout_column_wrap( 
            width = 1/2,
            textInput(ns("altc_name_input"), 
                      "Name for new Alternative Choice Matrix:",
                      placeholder = "e.g., Alt_Run1"),
            
            tags$div(class = "form-group",
                     tags$label(HTML("&nbsp;")), # Empty label for spacing
                     actionButton(ns('altc_save_btn'), 'Create alternative location matrix',
                                  icon = icon("cogs"),
                                  class = "btn-primary",
                                  width = "100%")
            )
          )
          
        )
    ),
    
    #  Manage Table Card -----------
    bslib::layout_column_wrap(
      width = 1/2,
      heights_equal = "row", # Keeps left and right cards the same height
      bslib::card(
        full_screen = TRUE, 
        bslib::card_header("Manage Alternative Choice Matrices"),
        
        # Explicit CSS max-height and scrolling on the BODY only.
        # This cages the table so it cannot grow over the footer.
        bslib::card_body(
          style = "max-height: 800px; overflow-y: auto; min-height: 300px;",
          
          # Set fill = FALSE. Let the div above handle the scrollbar, not the JS.
          DT::dataTableOutput(ns("existing_matrices_table"), fill = FALSE)
        ),
        
        # Footer stays legally outside the scrollable body area
        bslib::card_footer(
          bslib::layout_columns(
            col_widths = c(8, 4),
            gap = "10px",
            selectizeInput(
              ns("matrix_to_remove"), 
              "Select matrix to remove:", 
              choices = NULL, 
              width = "100%"
            ),
            
            div(
              style = "margin-top: 25px;",
              actionButton(
                ns("remove_matrix_btn"), 
                icon = icon("trash"),
                "Remove Selected", 
                class = "btn-danger w-100"
              )
            )
          )
        )
      ),
      
      div(
        id = ns("altc_zone_plot_wrapper"), 
        style = "display: none; height: 100%;", 
        bslib::card(
          full_screen = TRUE,
          height = "100%", 
          bslib::card_header("Analysis of Saved Matrix"),
          bslib::card_body(
            # Standard plot output
            plotOutput(ns('altc_zone_plot'), height = "800px")
          )
        )
      )
    ),
    
    # Spinner container
    div(id = ns("define_alt_spinner_container"),
        style = "display: none;",
        spinner_ui(ns("define_alt_spinner"),
                   spinner_type = "circle",
                   size = "large",
                   message = "Creating alternative choice matrix...",
                   overlay = TRUE)
    )
  )
}