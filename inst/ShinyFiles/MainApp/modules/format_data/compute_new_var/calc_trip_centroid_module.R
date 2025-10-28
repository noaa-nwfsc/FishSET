# =================================================================================================
# File: calc_trip_centroid_module.R
# Description: This module provides the UI and server logic for calculating trip centroids.
#              It loads required lat/lon variables, asks for a trip_id, allows for an 
#              optional weighting variable, previews the result, and displays a map.
#
# Authors: Paul Carvalho, Anna Abelman
# Date created: 10/28/2025
# Dependencies: shiny, DT, shinyjs, dplyr, ggplot2, maps
# Notes: This module interacts with the main reactive data values (rv_data) and saved
#        project variables. It assumes lat/lon are set in 'Select variables'.
#        This code was adapted by Gemini.
# =================================================================================================


# Server ------------------------------------------------------------------------------------------
#' calc_trip_centroid_server
#'
#' @description Server logic for the trip centroid module.
#'
#' @param id id A character string that is unique to this module instance.
#' @param rv_folderpath A reactive value containing the path to the project folder.
#' @param rv_project_name A reactive value containing the current project name.
#' @param rv_data A reactiveValues object containing the loaded data frames.
#'
#' @return This module does not return a value but modifies the main data frame (rv_data$main).
calc_trip_centroid_server <- function(id, rv_folderpath, rv_project_name, rv_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Reactive value to store the dataset with new centroid columns for preview
    rv_centroid_dat <- reactiveVal()
    # Reactive value to store the selected variables (lat, lon)
    rv_selected_vars <- reactiveVal()
    
    # Update input choices based on the main data
    observe({
      req(rv_data$main)
      
      all_cols <- colnames(rv_data$main)
      numeric_cols <- names(which(sapply(rv_data$main, is.numeric)))
      
      # Add "None" option for weight
      weight_choices <- c("None (unweighted)" = "", numeric_cols)
      
      updateSelectInput(session, 
                        "weight_var_input", 
                        choices = weight_choices,
                        selected = "")
      
      updateSelectizeInput(session,
                           "trip_id_input",
                           choices = all_cols)
    })
    
    # Handle the 'Calculate trip centroid' button click 
    observeEvent(input$calculate_btn, {
      req(rv_project_name(), rv_data$main)
      project_name <- rv_project_name()$value
      folderpath <- rv_folderpath()
      
      # Show spinner
      shinyjs::show("centroid_spinner_container")
      on.exit(shinyjs::hide("centroid_spinner_container"), add = TRUE)
      
      # Load selected variables
      selected_vars <- load_gui_variables(project_name, folderpath)
      
      # Check specifically for lat and lon
      if (is.null(selected_vars$main$main_lon) ||
          is.null(selected_vars$main$main_lat)) {
        
        shinyjs::hide("centroid_spinner_container")
        showModal(modalDialog(
          title = "Error: Missing Variables",
          "The main longitude or latitude variables have not been 
          identified in the 'Select variables' tab. Please ensure these are selected 
          for the main data table.",
          easyClose = TRUE
        ))
        return() # Stop execution
      } 
      
      # Input validation for trip_id
      if (is.null(input$trip_id_input) || input$trip_id_input == "") {
        showNotification("Please select a unique trip ID variable.", type = "error")
        return()
      }
      
      # Store selected vars (lat/lon) for the plot render
      rv_selected_vars(selected_vars)
      
      # Get weight_var, set to NULL if "None" is selected
      weight_var <- if (input$weight_var_input == "") NULL else input$weight_var_input
      
      # Run calc_trip_centroid function and show preview
      tryCatch({
        dat_with_centroids <- calc_trip_centroid(
          dat = rv_data$main,
          project = project_name,
          lon = selected_vars$main$main_lon,
          lat = selected_vars$main$main_lat,
          trip_id = input$trip_id_input,
          weight_var = weight_var
        )
        
        rv_centroid_dat(dat_with_centroids)
        
        # Show the plot output area
        shinyjs::show("plot_card_wrapper")
        
        # Show modal with preview
        showModal(modalDialog(
          title = "Preview: Data with Trip Centroids",
          size = "l",
          DT::dataTableOutput(ns("preview_table")),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_save_btn"), "Confirm and Save", class = "btn-primary")
          )
        ))
      }, error = function(e) {
        showNotification(paste("An error occurred:", e$message), type = "error", duration = 10)
      })
    })
    
    # Render the preview table in the modal
    output$preview_table <- DT::renderDataTable({
      req(rv_centroid_dat())
      # Show new columns plus original key columns
      vars <- rv_selected_vars()
      cols_to_show <- c(input$trip_id_input, 
                        vars$main$main_lon, 
                        vars$main$main_lat, 
                        "cent_lon", "cent_lat")
      # Include weight var if used
      if (!is.null(input$weight_var_input) && input$weight_var_input != "") {
        cols_to_show <- c(cols_to_show, input$weight_var_input)
      }
      
      preview_data <- rv_centroid_dat() %>%
        dplyr::select(dplyr::any_of(unique(cols_to_show)))
      
      DT::datatable(head(preview_data, 20),
                    options = list(scrollX = TRUE, pageLength = 5),
                    rownames = FALSE)
    })
    
    # Handle the 'Confirm and Save' button click
    observeEvent(input$confirm_save_btn, {
      # Update the main reactive data frame
      rv_data$main <- rv_centroid_dat()
      
      # Save changes to SQLite database
      table_save(table = rv_data$main,
                 project = rv_project_name()$value,
                 type = "main")
      
      # Remove the modal
      removeModal()
      
      # Show a success message
      showNotification("Trip centroids successfully calculated and saved.", type = "message")
    })
    
    # Generate and render the centroid map
    output$centroid_plot <- renderPlot({
      req(rv_centroid_dat())
      
      dat <- rv_centroid_dat()
      trip_id_col <- input$trip_id_input
      
      # Get unique centroids for plotting
      centroid_data <- dat %>%
        dplyr::select(!!sym(trip_id_col), cent_lon, cent_lat) %>%
        dplyr::distinct()
      
      req(nrow(centroid_data) > 0)
      
      # Get world map data
      world <- maps::map_data("world")
      
      # Calculate plot limits
      lon_range <- range(centroid_data$cent_lon, na.rm = TRUE) + c(-2, 2)
      lat_range <- range(centroid_data$cent_lat, na.rm = TRUE) + c(-2, 2)
      
      ggplot2::ggplot() +
        ggplot2::geom_polygon(data = world, 
                              ggplot2::aes(x = long, y = lat, group = group),
                              fill = "grey80", color = "white", linewidth = 0.2) +
        ggplot2::geom_point(data = centroid_data, 
                            ggplot2::aes(x = cent_lon, y = cent_lat),
                            color = "blue", alpha = 0.5, size = 2) +
        ggplot2::coord_quickmap(xlim = lon_range, ylim = lat_range) +
        ggplot2::labs(title = "Calculated Trip Centroids",
                      x = "Longitude",
                      y = "Latitude") +
        ggplot2::theme_minimal(base_size = 14)
    })
    
  })
}

# UI ----------------------------------------------------------------------------------------------
#' calc_trip_centroid_ui
#'
#' @description UI for the trip centroid module.
#'
#' @param id A character string that is unique to this module instance.
#'
#' @return A tagList containing the UI elements for the trip centroid module.
calc_trip_centroid_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    div(
      id = ns("main_container"),
      bslib::card(
        class = "card_overflow",
        bslib::card_header("Calculate Trip Centroid"),
        bslib::card_body(
          class = "card_overflow",
          p("This function calculates the geographic centroid (average latitude and longitude) 
            for each unique trip. It uses the 'Latitude' and 'Longitude' variables set in the 
            'Select variables' tab. You must select the 'Trip ID' column below. You can also 
            optionally select a numeric column (e.g., catch amount) to calculate a weighted 
            centroid."),
          hr(),
          
          fluidRow(
            column(4,
                   h5(tagList(
                     "1. Select Trip ID",
                     bslib::tooltip(
                       shiny::icon("circle-info"),
                       HTML("Select the variable that identifies unique trips. Note
                             that hauls/sets within the same trip should share trips IDs.")
                     )
                   )),

                   selectizeInput(ns("trip_id_input"),
                                  label = NULL,
                                  choices = NULL,
                                  multiple = FALSE)
            ),
            
            column(4,
                   h5("2. Select Weight (Optional)"),
                   selectInput(ns("weight_var_input"),
                               label = "Choose 'None' for a simple average:",
                               choices = NULL, # Populated by server
                               selected = "")
            ),
            
            column(4,
                   style = "margin-top: 36px;",
                   actionButton(ns("calculate_btn"),
                                "Calculate Trip Centroid",
                                icon = icon("calculator"),
                                class = "btn-secondary",
                                width = "100%")
            )
          ),
          
          # Spinner container
          div(id = ns("centroid_spinner_container"),
              style = "display: none;",
              spinner_ui(ns("centroid_spinner"),
                         spinner_type = "circle",
                         size = "large",
                         message = "Calculating centroids...",
                         overlay = TRUE)
          )
        )
      ),
      
      shinyjs::hidden(
        div(id = ns("plot_card_wrapper"),
            bslib::card(
              bslib::card_header("Centroid Map"),
              bslib::card_body(
                plotOutput(ns("centroid_plot"))
              )
            )
        )
      )
    )
  )
}