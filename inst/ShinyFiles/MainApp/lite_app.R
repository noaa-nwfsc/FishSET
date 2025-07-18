# =============================================================================
# File: app.R
# Description: Main entry point for the FishSET GUI Shiny app.
#              This file initializes the app by sourcing the UI and 
#              server logic from separate files ('ui.R' and 'server.R').
#
# Package: FishSET
# Authors: Paul Carvalho, Anna Abelman, et al. from previous Shiny app
# Date created: 4/18/2025
# Requirements: - R with Shiny package installed
#               - ui.R and server.R in the same directory or proper paths
# Usage: Run this file to launch the Shiny application
#        > shiny::runApp()
#
# =============================================================================
library(shiny)

# Create and run the Shiny app
shinyApp(ui = ui, server = server, session, enableBookmarking = "server")