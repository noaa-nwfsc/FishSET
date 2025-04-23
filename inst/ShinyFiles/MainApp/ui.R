# =================================================================================================
# File: ui.R
# Description: Defines the UI layout for the FishSET Shiny app, which is bundled with the
#              FishSET R package. This UI is pared with 'server.R' and sourced in 'app.R'.
#
# Package: FishSET
# Authors: Paul Carvalho, Anna Abelman, et al. from previous Shiny app
# Date created: 4/18/2025
#
# Notes: - Keep UI layout and input definitions modular and clean.
#        - Use 'bslib' package for UI.
#        - Use unique namespaced IDs for modules
#
# =================================================================================================

# Source module scripts ---------------------------------------------------------------------------


# UI function definition
ui <- function(request){
  bslib::page_navbar(
    theme = bslib::bs_theme(
      primary = "#41729F", 
      secondary = "#AACDE5", 
      info = "#274472",
      font_scale = 0.9,
      preset = "cerulean"),
    id = "tabs",
    
    # UPLOAD DATA ---------------------------------------------------------------------------------
    bslib::nav_menu(
      title = "Upload Data",
      
      ## Load files subtab ------------------------------------------------------------------------
      bslib::nav_panel(
        title = "Load files", 
        id = "load_files",
        bslib::page_fillable(
          bslib::layout_sidebar(
            fillable = TRUE, 
            fill = TRUE,
            sidebar = bslib::sidebar( # SIDE BAR
              fillable = TRUE, 
              fill = TRUE, 
              width = 400
              
            )
          )
        )
      ),
      
      ## Select variables subtab ------------------------------------------------------------------
      bslib::nav_panel(
        title = "Select variables", 
        id = "select_variables"
      )
    )
  )
}

