# =================================================================================================
# File: spinner.R
# Description: Reusable loading spinner module. This file contains css settings and the ui module
#              for the spinner. To use, create a spinner container the show/hide the ui.
#
# Package: FishSET
# Authors: Paul Carvalho, Anna Abelman
# Date created: 5/27/2025
# Dependencies: - shiny and shinyjs packages
#
# =================================================================================================

# CSS settings ------------------------------------------------------------------------------------
# Description: CSS code that defines styling for different types of spinners in the spinner shiny 
#              module. This supports both inline and full-screen (overlay) spinners. Types of 
#              spinners include circle, dots, pulse, and bars.
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

# Spinner module ui -------------------------------------------------------------------------------
# Description: This ui module defines a customizable loading spinner UI. It supports circle, dots,
#              pulse, and bar spinners, various colors/sizes, optional loading messages, and
#              a overlay and inline display modes. 
spinner_ui <- function(id, 
                      spinner_type = "circle",
                      size = "medium", 
                      color = "#3498db",
                      message = "Loading...",
                      overlay = FALSE) {
  
  ns <- NS(id)
  
  # Define spinner types
  spinner_html <- switch(
    spinner_type,
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
    div(
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