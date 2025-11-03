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
source("modules/qaqc_module.R", local = TRUE)
source("modules/explore_data_module.R", local = TRUE)
source("modules/format_data/compute_new_var_module.R", local = TRUE)
source("modules/format_data/create_expectations_module.R", local = TRUE)

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
        value = "load_files",
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
              
              checklist_ui("load_checklist"),
              load_sidebar_ui("upload_data_sidebar"),
              other_actions_ui("upload_data_actions"),
            ),
            
            ### Main panel
            #### Change folder path
            bslib::card(fill = FALSE,
                        bslib::card_header("1. Set folder path"),
                        bslib::card_body(
                          folder_path_ui("folderpath")
                        )
            ),
            
            #### Select project
            bslib::card(fill = FALSE,
                        height = 200,
                        bslib::card_header("2. Add or select a project"),
                        bslib::card_body(
                          shinyjs::useShinyjs(),
                          select_project_ui("select_project")
                        )
            ),
            
            #### Load data
            fluidRow(
              column(12, load_data_ui("load_data")),
            )
          )
        )
      ),
      
      ## Select variables subtab ------------------------------------------------------------------
      bslib::nav_panel(
        title = "Select variables", 
        id = "select_variables",
        value = "select_variables",
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
              checklist_ui("select_var_checklist"),
              other_actions_ui("selecting_variables_actions")
            ),
            
            ### Main panel
            fluidRow(
              column(12, save_var_ui("saving_all_variables"))
            )
          )
        )
      )
    ),
    
    # QAQC ----------------------------------------------------------------------------------------
    bslib::nav_menu(
      title = "QAQC",
      
      ## Quality checks subtab --------------------------------------------------------------------
      bslib::nav_panel(
        title = "Data quality checks", 
        id = "quality_checks",
        value = "quality_checks",
        bslib::page_fillable(
          bslib::layout_sidebar(
            fillable = TRUE,
            fill = TRUE,
            
            ### Sidebar
            sidebar = bslib::sidebar( 
              fillable = TRUE, 
              fill = TRUE, 
              width = 400,
              
              checklist_ui("quality_check_checklist"),
              hr(),
              qaqc_sidebar_ui("qaqc_checks"),
              other_actions_ui("quality_check_actions")
            ),
            
            ### Main panel
            qaqc_ui("qaqc_checks")    
          )
        )
      ),
      
      ## Data exploration subtab ------------------------------------------------------------------
      bslib::nav_panel(
        title = "Explore the data", 
        id = "explore_data",
        value = "explore_data",
        bslib::page_fillable(
          bslib::layout_sidebar(
            fillable = TRUE,
            fill = TRUE,
            
            ### Sidebar
            sidebar = bslib::sidebar( 
              fillable = TRUE, 
              fill = TRUE, 
              width = 400,
              
              checklist_ui("explore_data_checklist"),
              hr(),
              explore_data_sidebar_ui("explore_data"),
              other_actions_ui("explore_data_actions")
            ),
            
            ### Main panel
            explore_data_ui("explore_data")    
          )
        )
      )
    ),
    # Format data ---------------------------------------------------------------------------------
    bslib::nav_menu(
      title = "Format Data",
      
      ## Compute new variables subtab -------------------------------------------------------------
      bslib::nav_panel(
        title = "Compute new variables", 
        id = "compute_new_variables",
        value = "compute_new_variables",
        bslib::page_fillable(
          bslib::layout_sidebar(
            fillable = TRUE,
            fill = TRUE,
            
            ### Sidebar
            sidebar = bslib::sidebar( 
              fillable = TRUE, 
              fill = TRUE, 
              width = 400,
              
              checklist_ui("compute_new_var_checklist"),
              hr(),
              compute_new_var_sidebar_ui("compute_new_var"),
              other_actions_ui("compute_new_var_actions")
            ),
            
            ### Main panel
            compute_new_var_ui("compute_new_var")

          )
        )
      ),
      
      ## Create expectations subtab ---------------------------------------------------------------
      bslib::nav_panel(
        title = "Create expected catch matrix", 
        id = "create_exp",
        value = "create_exp",
        bslib::page_fillable(
          bslib::layout_sidebar(
            fillable = TRUE,
            fill = TRUE,
            
            ### Sidebar
            sidebar = bslib::sidebar( 
              fillable = TRUE, 
              fill = TRUE, 
              width = 400,
              
              checklist_ui("create_expectations_checklist"),
              # compute_new_var_sidebar_ui("compute_new_var"),
              other_actions_ui("create_expectations_actions")
            ),
            
            ### Main panel
            create_expectations_ui("create_expectations")
            
          )
        )
      )
    )
  )
}

