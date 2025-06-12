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
source("modules/spinner.R", local = TRUE) # Reusable spinner
source("modules/load_files_ui.R", local = TRUE) # Upload data - load files subtab
source("modules/other_actions_ui.R", local = TRUE) # Other actions in sidebar 
source("modules/select_variables_ui.R", local = TRUE) # Other actions in sidebar 


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
              load_sidebar_ui("upload_data_sidebar"),
              other_actions_ui("upload_data_actions"),
              
              textInput("r_expr_input", 
                        label = "Enter an R expression",
                        value = "values$dataset"),
              actionButton("run_r_btn", "Run", class = "btn-success"),
              div(style = "margin-top: 2em;",
                  uiOutput('r_expr_result')
              ),
            ),
            
            ### Change folder path
            bslib::card(fill = FALSE,
                        bslib::card_header("1. Set folder path"),
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
              fill = FALSE,
              width = 1/2,
              ### Select main data 
              bslib::card(fill = FALSE,
                          bslib::card_header("3. Primary data"),
                          bslib::card_body(
                            bslib::card(
                              bslib::card_body(
                                select_data_ui("select_main", data_type = "main")
                              )
                            ),
                            bslib::card(
                              bslib::card_body(
                                select_data_ui("select_port", data_type = "port")
                              )
                            ),
                            bslib::card(
                              bslib::card_body(
                                select_data_ui("select_aux", data_type = "aux")
                              )
                            )
                          )
              ),
              
              ### Select spatial data
              bslib::card(fill = FALSE,
                          bslib::card_header("4. Spatial data"),
                          bslib::card_body(
                            bslib::card(fill = FALSE,
                                        bslib::card_body(
                                          select_data_ui("select_spatial", data_type = "spat")
                                        )
                            ), 
                            bslib::card(fill = FALSE,
                                        bslib::card_body(
                                          select_data_ui("select_grid", data_type = "grid")
                                        )
                            )
                          )
              )
            ),
            
            fluidRow(
              column(7, load_data_ui("load_data"))
            )
          )
        )
      ),
      
      ## Select variables subtab ------------------------------------------------------------------
      bslib::nav_panel(
        title = "Select variables", 
        id = "select_variables",
        bslib::layout_column_wrap( fill = TRUE,
                                   width = 1/2,
                                   bslib::card(fill = TRUE,fillable = TRUE,
                                               bslib::card_header(strong("Primary data variables"), class = "bg-info"),
                                               bslib::card_body( 
                                                 bslib::card(
                                                   h6("Primary data"),
                                                   select_main_var_ui("selecting_main")),
                                                 bslib::card(
                                                   h6("Port data"),
                                                   select_port_var_ui("selecting_port"))
                                               )
                                   ),
                                   bslib::card(fill = TRUE,fillable = TRUE,
                                               bslib::card_header(strong('Spatial data variables'),  class = "bg-info"),
                                               bslib::card_body(
                                                 h6("Spatial data"),
                                                 select_spat_var_ui("selecting_spat")
                                               )
                                   )
        )
      )
    )
  )
}

