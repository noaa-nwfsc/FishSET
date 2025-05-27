library(shiny)
library(shinyjs)

# =============================================================================
# REUSABLE SPINNER MODULE
# =============================================================================

# CSS for all spinner types (defined here so it's self-contained)
spinner_css <- "
  /* Base spinner container styles */
  .spinner-inline {
    text-align: center;
    padding: 20px;
  }
  
  .spinner-overlay {
    position: fixed;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    background-color: rgba(255, 255, 255, 0.9);
    z-index: 9999;
    display: flex;
    justify-content: center;
    align-items: center;
    flex-direction: column;
  }
  
  .spinner-content {
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 15px;
  }
  
  .spinner-message {
    margin: 0;
    font-size: 16px;
    color: #666;
    font-weight: 500;
  }
  
  /* Circle Spinner */
  .spinner-circle {
    border: 4px solid #f3f3f3;
    border-radius: 50%;
    border-top: 4px solid #3498db;
    animation: spin 1s linear infinite;
  }
  
  .spinner-circle.spinner-small { width: 20px; height: 20px; border-width: 2px; }
  .spinner-circle.spinner-medium { width: 40px; height: 40px; border-width: 4px; }
  .spinner-circle.spinner-large { width: 60px; height: 60px; border-width: 6px; }
  
  /* Dots Spinner */
  .spinner-dots {
    display: flex;
    gap: 5px;
  }
  
  .spinner-dots div {
    border-radius: 50%;
    background-color: #3498db;
    animation: dots-bounce 1.4s ease-in-out infinite both;
  }
  
  .spinner-dots div:nth-child(1) { animation-delay: -0.32s; }
  .spinner-dots div:nth-child(2) { animation-delay: -0.16s; }
  
  .spinner-dots.spinner-small div { width: 8px; height: 8px; }
  .spinner-dots.spinner-medium div { width: 12px; height: 12px; }
  .spinner-dots.spinner-large div { width: 16px; height: 16px; }
  
  /* Pulse Spinner */
  .spinner-pulse {
    border-radius: 50%;
    background-color: #3498db;
    animation: pulse-scale 1s ease-in-out infinite;
  }
  
  .spinner-pulse.spinner-small { width: 30px; height: 30px; }
  .spinner-pulse.spinner-medium { width: 50px; height: 50px; }
  .spinner-pulse.spinner-large { width: 70px; height: 70px; }
  
  /* Bars Spinner */
  .spinner-bars {
    display: flex;
    gap: 3px;
    align-items: center;
  }
  
  .spinner-bars div {
    background-color: #3498db;
    animation: bars-stretch 1.2s ease-in-out infinite;
  }
  
  .spinner-bars div:nth-child(2) { animation-delay: -1.1s; }
  .spinner-bars div:nth-child(3) { animation-delay: -1.0s; }
  .spinner-bars div:nth-child(4) { animation-delay: -0.9s; }
  
  .spinner-bars.spinner-small div { width: 3px; height: 20px; }
  .spinner-bars.spinner-medium div { width: 4px; height: 30px; }
  .spinner-bars.spinner-large div { width: 6px; height: 40px; }
  
  /* Animations */
  @keyframes spin {
    0% { transform: rotate(0deg); }
    100% { transform: rotate(360deg); }
  }
  
  @keyframes dots-bounce {
    0%, 80%, 100% { transform: scale(0); }
    40% { transform: scale(1); }
  }
  
  @keyframes pulse-scale {
    0%, 100% { transform: scale(0); opacity: 1; }
    50% { transform: scale(1); opacity: 0.5; }
  }
  
  @keyframes bars-stretch {
    0%, 40%, 100% { transform: scaleY(0.4); }
    20% { transform: scaleY(1.0); }
  }
"

# Spinner Module UI
spinnerUI <- function(id, 
                      spinner_type = "circle",
                      size = "medium", 
                      color = "#3498db",
                      message = "Loading...",
                      overlay = FALSE) {
  
  ns <- NS(id)
  
  # Define spinner types
  spinner_html <- switch(spinner_type,
                         "circle" = tags$div(class = paste("spinner-circle", paste0("spinner-", size)), 
                                             style = paste0("border-top-color: ", color, ";")),
                         
                         "dots" = tags$div(class = paste("spinner-dots", paste0("spinner-", size)),
                                           tags$div(style = paste0("background-color: ", color, ";")),
                                           tags$div(style = paste0("background-color: ", color, ";")),
                                           tags$div(style = paste0("background-color: ", color, ";"))),
                         
                         "pulse" = tags$div(class = paste("spinner-pulse", paste0("spinner-", size)), 
                                            style = paste0("background-color: ", color, ";")),
                         
                         "bars" = tags$div(class = paste("spinner-bars", paste0("spinner-", size)),
                                           tags$div(style = paste0("background-color: ", color, ";")),
                                           tags$div(style = paste0("background-color: ", color, ";")),
                                           tags$div(style = paste0("background-color: ", color, ";")),
                                           tags$div(style = paste0("background-color: ", color, ";")))
  )
  
  # Container class based on overlay option
  container_class <- ifelse(overlay, "spinner-overlay", "spinner-inline")
  
  tagList(
    # Include CSS automatically when module is used (only once per session)
    singleton(tags$head(tags$style(HTML(spinner_css)))),
    
    # The actual spinner UI
    
    tags$div(
      id = ns("spinner_container"),
      class = container_class,
      tags$div(class = "spinner-content",
               spinner_html,
               if(nchar(message) > 0) {
                 tags$p(class = "spinner-message", message)
               }
      )
    )
    
  )
}

# Spinner Module Server
spinnerServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Return functions to control the spinner
    list(
      show = function() {
        shinyjs::show("spinner_container")
      },
      
      hide = function() {
        shinyjs::hide("spinner_container")
      },
      
      toggle = function() {
        shinyjs::toggle("spinner_container")
      },
      
      update_message = function(new_message) {
        # Update the message text
        shinyjs::html(id = "spinner_container .spinner-message", html = new_message)
      }
    )
  })
}

# # CSS for all spinner types (removed from here - now in module)
# 
# # =============================================================================
# # DEMO APP USING THE SPINNER MODULE
# # =============================================================================
# 
# # Demo Module 1: Data Loader
# dataLoaderUI <- function(id) {
#   ns <- NS(id)
#   
#   tagList(
#     h4("Data Loader Demo"),
#     actionButton(ns("load_data"), "Load Data", class = "btn-primary"),
#     br(), br(),
#     
#     # Inline spinner for this section
#     spinnerUI(ns("data_spinner"), 
#               spinner_type = "circle", 
#               size = "medium", 
#               message = "Loading data..."),
#     
#     br(),
#     
#     # Results area
#     div(id = ns("results"),
#         hidden(
#           div(id = ns("success_result"), class = "alert alert-success",
#               strong("Success! "), "Data loaded successfully.",
#               br(), br(),
#               tableOutput(ns("data_table"))
#           )
#         ),
#         hidden(
#           div(id = ns("error_result"), class = "alert alert-danger",
#               strong("Error! "), span(id = ns("error_text"), "")
#           )
#         )
#     )
#   )
# }
# 
# dataLoaderServer <- function(id, global_spinner) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#     
#     # Initialize local spinner
#     local_spinner <- spinnerServer("data_spinner")
#     loaded_data <- reactiveVal(NULL)
#     
#     observeEvent(input$load_data, {
#       # Hide previous results
#       shinyjs::hide("success_result")
#       shinyjs::hide("error_result")
#       
#       # Show local spinner
#       local_spinner$show()
#       
#       # Simulate data loading
#       invalidateLater(sample(2000:4000, 1), session)
#       
#       observe({
#         if (input$load_data > 0) {
#           # Simulate success/failure
#           if (runif(1) > 0.3) {
#             # Success
#             data <- data.frame(
#               ID = 1:5,
#               Product = paste("Product", 1:5),
#               Price = round(runif(5, 10, 100), 2),
#               Stock = sample(1:50, 5)
#             )
#             loaded_data(data)
#             shinyjs::show("success_result")
#           } else {
#             # Error
#             shinyjs::html("error_text", "Failed to connect to database")
#             shinyjs::show("error_result")
#           }
#           
#           # Hide local spinner
#           local_spinner$hide()
#         }
#       }, once = TRUE)
#     })
#     
#     output$data_table <- renderTable({
#       req(loaded_data())
#       loaded_data()
#     })
#   })
# }
# 
# # Demo Module 2: File Processor
# fileProcessorUI <- function(id) {
#   ns <- NS(id)
#   
#   tagList(
#     h4("File Processor Demo"),
#     actionButton(ns("process_file"), "Process Files", class = "btn-success"),
#     actionButton(ns("show_global"), "Show Global Overlay", class = "btn-warning"),
#     br(), br(),
#     
#     # Different spinner type
#     spinnerUI(ns("file_spinner"), 
#               spinner_type = "bars", 
#               size = "large", 
#               color = "#e74c3c",
#               message = "Processing files..."),
#     
#     br(),
#     textOutput(ns("process_result"))
#   )
# }
# 
# fileProcessorServer <- function(id, global_spinner) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#     
#     # Initialize local spinner
#     local_spinner <- spinnerServer("file_spinner")
#     
#     observeEvent(input$process_file, {
#       local_spinner$show()
#       
#       # Update message during processing
#       local_spinner$update_message("Step 1: Validating files...")
#       
#       invalidateLater(1500, session)
#       observe({
#         local_spinner$update_message("Step 2: Processing data...")
#         
#         invalidateLater(1500, session)
#         observe({
#           local_spinner$update_message("Step 3: Saving results...")
#           
#           invalidateLater(1000, session)
#           observe({
#             local_spinner$hide()
#             output$process_result <- renderText({
#               paste("Processing completed at", Sys.time())
#             })
#           }, once = TRUE)
#         }, once = TRUE)
#       }, once = TRUE)
#     })
#     
#     # Demo global overlay spinner
#     observeEvent(input$show_global, {
#       global_spinner$show()
#       
#       invalidateLater(3000, session)
#       observe({
#         global_spinner$hide()
#       }, once = TRUE)
#     })
#   })
# }
# 
# # Main App UI
# ui <- fluidPage(
#   useShinyjs(),
#   
#   # No need to manually add spinner CSS - it's included in the module!
#   tags$head(
#     tags$style(HTML("
#       .alert {
#         border-radius: 5px;
#         padding: 15px;
#         margin: 15px 0;
#         border: 1px solid transparent;
#       }
#       .alert-success {
#         color: #3c763d;
#         background-color: #dff0d8;
#         border-color: #d6e9c6;
#       }
#       .alert-danger {
#         color: #a94442;
#         background-color: #f2dede;
#         border-color: #ebccd1;
#       }
#       .btn { margin: 5px; padding: 8px 16px; }
#     "))
#   ),
#   
#   titlePanel("Reusable Spinner Module Demo"),
#   
#   # Global overlay spinner
#   spinnerUI("global_spinner", 
#             spinner_type = "pulse", 
#             size = "large", 
#             overlay = TRUE,
#             message = "Processing your request..."),
#   
#   fluidRow(
#     column(6, dataLoaderUI("data_loader")),
#     column(6, fileProcessorUI("file_processor"))
#   ),
#   
#   br(),
#   div(class = "alert alert-info",
#       strong("Demo Features:"),
#       tags$ul(
#         tags$li("Local inline spinners for each module"),
#         tags$li("Global overlay spinner that covers the entire page"),
#         tags$li("Different spinner types: circle, bars, pulse"),
#         tags$li("Dynamic message updates"),
#         tags$li("Configurable sizes and colors")
#       )
#   )
# )
# 
# # Main App Server
# server <- function(input, output, session) {
#   # Initialize global spinner
#   global_spinner <- spinnerServer("global_spinner")
#   
#   # Initialize modules
#   dataLoaderServer("data_loader", global_spinner)
#   fileProcessorServer("file_processor", global_spinner)
# }
# 
# # Run the app
# shinyApp(ui = ui, server = server)