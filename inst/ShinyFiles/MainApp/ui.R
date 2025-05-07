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
source("modules/load_files_ui.R", local = TRUE) # Upload data - load files subtab

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
    
    # Upload data ---------------------------------------------------------------------------------
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
            includeCSS("styles.css"), # Line needs to be placed on same level as bslib::card() 
            
            ### Sidebar
            sidebar = bslib::sidebar( 
              fillable = TRUE, 
              fill = TRUE, 
              width = 400,
            ),
            
            ### Change folder path
            bslib::card(fill = FALSE,
                        bslib::card_header("1. Change folder path"),
                        bslib::card_body(
                          folder_path_ui("folderpath")
                        )
            ),
            
            ### Select project
            bslib::card(fill = FALSE,
                        bslib::card_header("2. Add or select a project"),
                        bslib::card_body(
                          shinyjs::useShinyjs(),
                          select_project_ui("select_project")
                        )
            ),
            
            bslib::layout_column_wrap(
              width = 1/2,
              
              ### Load primary data
              bslib::card(fill = FALSE,
                          bslib::card_header("3. Primary data"),
                          bslib::card_body(
                            load_primary_ui("load_primary")
                          )
              ),
              
              ### Load spatial and optional gridded data
              bslib::card(fill = FALSE,
                          bslib::card_header("4. Spatial data"),
                          bslib::card_body(
                            bslib::card(fill = FALSE,
                                        bslib::card_body(
                                          h5("Spatial data:"),
                                          load_spatial_ui("load_spatial")
                                        )
                            ), 
                            
                            bslib::card(fill = FALSE,
                                        bslib::card_body(
                                          h5(tags$strong(tags$i("Optional")), "gridded data:"),
                                          load_grid_ui("load_grid")
                                        )
                            )
                          )
              )
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

