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

# Source libraries --------------------------------------------------------------------------------
library(shinycssloaders)
library(ggplot2)
library(bslib)
library(DT)

# Source module scripts ---------------------------------------------------------------------------
source("modules/spinner.R", local = TRUE) # Reusable spinner
source("modules/load_files_ui.R", local = TRUE) # Upload data - load files subtab
source("modules/other_actions_ui.R", local = TRUE) # Other actions in sidebar 
source("modules/select_variables_ui.R", local = TRUE) # Other actions in sidebar 
source("modules/qaqc_module.R", local = TRUE)
source("modules/explore_data_module.R", local = TRUE)
source("modules/format_data/compute_new_var_module.R", local = TRUE)
source("modules/format_data/define_alternatives_module.R", local = TRUE)
source("modules/format_data/create_expectations_module.R", local = TRUE)
source("modules/format_data/format_model_data_module.R", local = TRUE)
source("modules/model_design_module.R", local = TRUE)
source("modules/model_fit_module.R", local = TRUE)
source("modules/model_cv_module.R", local = TRUE)
source("modules/policy/zone_closure_module.R", local = TRUE)
source("modules/policy/policy_sim_module.R", local = TRUE)
source("modules/policy/policy_effort_module.R", local = TRUE)
source("modules/policy/policy_welfare_module.R", local = TRUE)

quickstart_guide_ui <- function() {
  bslib::page_fillable(
    tags$style(HTML("
      .quickstart-hero {
        background: linear-gradient(135deg, #f7fbff 0%, #edf4fb 100%);
        border-left: 6px solid #41729F;
        border-radius: 0.75rem;
        padding: 1.25rem 1.5rem;
        margin-bottom: 1rem;
      }
      .quickstart-card {
        height: 100%;
      }
      .quickstart-list {
        padding-left: 1.2rem;
        margin-bottom: 0;
      }
      .quickstart-list li {
        margin-bottom: 0.4rem;
      }
      .quickstart-accordion .accordion-item {
        border-color: #d9e6f2;
      }
    ")),
    tags$div(
      class = "quickstart-hero",
      tags$h2("Quickstart guide", style = "color: #274472; margin-top: 0;"),
      tags$p(
        "This guide gives new users a fast path through the FishSET GUI, from loading data ",
        "to running models and reviewing policy outputs."
      ),
      tags$p(
        tags$strong("Recommended path: "),
        "Upload Data → Select variables → QAQC → Explore the data → Format Data → ",
        "Modeling → Policy"
      )
    ),
    bslib::layout_column_wrap(
      width = 1 / 2,
      bslib::card(
        class = "quickstart-card",
        bslib::card_header("Before you start"),
        bslib::card_body(
          tags$ul(
            class = "quickstart-list",
            tags$li("Have your main data file ready. Port, spatial, gridded, and auxiliary files are optional."),
            tags$li("Choose a FishSET folder first so the project database, output, and log files can be saved."),
            tags$li("The GUI saves changes locally in the project folder; nothing is shared automatically."),
            tags$li("Use the video tutorial and the User Manual if you want a slower walkthrough.")
          )
        )
      ),
      bslib::card(
        class = "quickstart-card",
        bslib::card_header("What gets saved automatically"),
        bslib::card_body(
          tags$ul(
            class = "quickstart-list",
            tags$li("Loaded data and edited tables are stored in the FishSET database."),
            tags$li("Plots and tables are written to the project output folder."),
            tags$li("Function calls and inputs are written to the project logs."),
            tags$li("The report template in the doc folder can be used to document results.")
          )
        )
      )
    ),
    bslib::accordion(
      class = "quickstart-accordion",
      open = FALSE,
      bslib::accordion_panel(
        "1. Upload Data",
        tags$p(
          "Set your FishSET folder, create or select a project, then load the main and optional supporting files."
        ),
        tags$ul(
          class = "quickstart-list",
          tags$li("Use the Upload Data tab to set the project folder and choose or create a project."),
          tags$li("Load the main data first; add port, spatial, gridded, or auxiliary data if your workflow needs them."),
          tags$li("If you already have an existing project, the database tables can be reloaded from the FishSET database.")
        )
      ),
      bslib::accordion_panel(
        "2. Select variables",
        tags$p(
          "Map the required IDs, dates, coordinates, and zone variables so later tabs know how to interpret the data."
        ),
        tags$ul(
          class = "quickstart-list",
          tags$li("Assign the main and spatial variables used throughout the app."),
          tags$li("Create trip, haul, or zone IDs if they do not already exist."),
          tags$li("Save the variable mapping once; you only need to redo it when a new project is started.")
        )
      ),
      bslib::accordion_panel(
        "3. QAQC and Explore the data",
        tags$p(
          "Check the data for missing values, duplicate observations, outliers, and spatial issues before modeling."
        ),
        tags$ul(
          class = "quickstart-list",
          tags$li("Run the QAQC checks that match your dataset; do not assume every check is needed."),
          tags$li("Use Explore the data to view tables, plots, and spatial summaries."),
          tags$li("Save any corrected data back to the database before moving on.")
        )
      ),
      bslib::accordion_panel(
        "4. Format Data",
        tags$p(
          "Build the derived variables, alternative fishing choices, and expected catch inputs used for modeling."
        ),
        tags$ul(
          class = "quickstart-list",
          tags$li("Use Compute New Variables to create new fields such as CPUE, trip-level variables, or spatial summaries."),
          tags$li("Define alternative fishing choices before generating expected catch/revenue."),
          tags$li("Run Format Model Data after the supporting matrices are ready.")
        )
      ),
      bslib::accordion_panel(
        "5. Modeling",
        tags$p(
          "Create the model design, fit the model, and then validate the results with cross-validation."
        ),
        tags$ul(
          class = "quickstart-list",
          tags$li("Model Design builds the specification needed for fitting."),
          tags$li("Model Fit estimates the parameters and saves the model objects."),
          tags$li("Model Cross Validation checks out-of-sample performance.")
        )
      ),
      bslib::accordion_panel(
        "6. Policy",
        tags$p(
          "Use the policy tabs to create closures, simulate effort redistribution, and summarize welfare impacts."
        ),
        tags$ul(
          class = "quickstart-list",
          tags$li("Zone Closure lets you define closed or restricted zones."),
          tags$li("Run Policy Simulations estimates the effects of a closure scenario."),
          tags$li("Summarize Effort Redistribution and Summarize Welfare Impacts present the outputs.")
        )
      ),
      bslib::accordion_panel(
        "Need more help?",
        tags$ul(
          class = "quickstart-list",
          tags$li(tags$a("FishSET video tutorial", href = "https://bcove.video/3WCSb5N", target = "_blank")),
          tags$li(tags$a("FishSET User Manual", href = "https://noaa-nwfsc.github.io/FishSET/articles/FishSET_User_Manual.html", target = "_blank")),
          tags$li("Questions: nmfs.fishset@noaa.gov")
        )
      )
    )
  )
}

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
    header = tags$head(
      includeCSS("styles.css"),
      tags$style(HTML("
        .navbar-nav .nav-link {
          font-weight: 600;
        }
      "))
    ),
    
    # Quickstart guide ---------------------------------------------------------------------------
    bslib::nav_panel(
      title = "Quickstart Guide",
      quickstart_guide_ui()
    ),
    
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
                        class="card-overflow",
                        height = 200,
                        bslib::card_header("2. Add or select a project"),
                        bslib::card_body(
                          class="card-overflow d-flex flex-column",
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
      ## Define alternatives subtab ---------------------------------------------------------------
      bslib::nav_panel(
        title = "Define alternative fishing choices", 
        id = "define_alternatives_id",
        value = "define_alternatives_id",
        bslib::page_fillable(
          bslib::layout_sidebar(
            fillable = TRUE,
            fill = TRUE,
            
            ### Sidebar
            sidebar = bslib::sidebar( 
              fillable = TRUE, 
              fill = TRUE, 
              width = 400,
              
              checklist_ui("define_alt_checklist"),
              hr(),
              other_actions_ui("define_alt_actions")
            ),
            
            ### Main panel
            define_alt_ui("define_alternatives")
            
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
              other_actions_ui("create_expectations_actions")
            ),
            
            ### Main panel
            create_expectations_ui("create_expectations")
            
          )
        )
      ),
      
      ## Format model data subtab ------------------------------------------------------------------
      bslib::nav_panel(
        title = "Format model data", 
        id = "format_model_data",
        value = "format_model_data",
        bslib::page_fillable(
          bslib::layout_sidebar(
            fillable = TRUE,
            fill = TRUE,
            ### Sidebar
            sidebar = bslib::sidebar( 
              fillable = TRUE, 
              fill = TRUE, 
              width = 400,
              checklist_ui("format_mod_data_checklist"),
              other_actions_ui("format_mod_data_actions")
            ),
            
            ### Main panel
            format_model_data_ui("format_mod_data")
            
          )
        )
      )
    ),
    
    
    # Modeling ------------------------------------------------------------------------------------
    bslib::nav_menu(
      title = "Modeling",
      
      ## Model design -------------------------------------------------------------------------------
      bslib::nav_panel(
        title = "Design Model", 
        id = "model_design",
        value = "model_design",
        bslib::page_fillable(
          bslib::layout_sidebar(
            fillable = TRUE,
            fill = TRUE,
            
            ### Sidebar
            sidebar = bslib::sidebar( 
              fillable = TRUE, 
              fill = TRUE, 
              width = 400,
              
              checklist_ui("model_design_checklist"),
              hr(),
              other_actions_ui("model_design_actions")
            ),
            
            ### Main panel
            model_design_ui("model_design_data")    
          )
        )
        
      ),
      ## Model fit -------------------------------------------------------------------------------
      bslib::nav_panel(
        title = "Model Fit", 
        id = "model_fit",
        value = "model_fit",
        bslib::page_fillable(
          bslib::layout_sidebar(
            fillable = TRUE,
            fill = TRUE,
            
            ### Sidebar
            sidebar = bslib::sidebar( 
              fillable = TRUE, 
              fill = TRUE, 
              width = 400,
              
              checklist_ui("model_fit_checklist"),
              hr(),
              other_actions_ui("model_fit_actions")
            ),
            
            ### Main panel
            model_fit_ui("model_fit_data")    
          )
        )
      ),
      ## Model Cross Validation -------------------------------------------------------------------
      bslib::nav_panel(
        title = "Model Cross Validation", 
        id = "model_cv_id",
        value = "model_cv_id",
        bslib::page_fillable(
          bslib::layout_sidebar(
            fillable = TRUE,
            fill = TRUE,
            
            ### Sidebar
            sidebar = bslib::sidebar( 
              fillable = TRUE, 
              fill = TRUE, 
              width = 400,
              
              checklist_ui("model_cv_checklist"),
              hr(),
              other_actions_ui("model_cv_actions")
            ),
            
            ### Main panel
            model_cv_ui("model_cv")    
          )
        )
      )
    ),
    # Policy ------------------------------------------------------------------------------------
    bslib::nav_menu(
      title = "Policy",
      
      ## Zone closure -----------------------------------------------------------------------------
      bslib::nav_panel(
        title = "Zone Closures", 
        id = "zone_closures",
        value = "zone_closures",
        bslib::page_fillable(
          bslib::layout_sidebar(
            fillable = TRUE,
            fill = TRUE,
            
            ### Sidebar
            sidebar = bslib::sidebar( 
              fillable = TRUE, 
              fill = TRUE, 
              width = 400,
              
              checklist_ui("zone_closure_checklist"),
              hr(),
              other_actions_ui("zone_closure_actions")
            ),
            
            ### Main panel
            zone_closure_ui("zone_closure")    
          )
        )
        
      ),
      ## Run policy simulation --------------------------------------------------------------------
      bslib::nav_panel(
        title = "Run Policy Simulation", 
        id = "policy_sim",
        value = "policy_sim",
        bslib::page_fillable(
          bslib::layout_sidebar(
            fillable = TRUE,
            fill = TRUE,
            
            ### Sidebar
            sidebar = bslib::sidebar( 
              fillable = TRUE, 
              fill = TRUE, 
              width = 400,
              
              checklist_ui("policy_sim_checklist"),
              hr(),
              other_actions_ui("policy_sim_actions")
            ),
            
            ### Main panel
            policy_sim_ui("policy_simulation")    
          )
        )
      ),
      
      # Policy Effort ----------------------------------------------------------------------------
      bslib::nav_panel(
        title = "Summarize Effort Redistribution", 
        id = "policy_effort",
        value = "policy_effort",
        bslib::page_fillable(
          bslib::layout_sidebar(
            fillable = TRUE,
            fill = TRUE,
            
            ### Sidebar
            sidebar = bslib::sidebar( 
              fillable = TRUE, 
              fill = TRUE, 
              width = 400,
              
              checklist_ui("policy_effort_checklist"),
              hr(),
              other_actions_ui("policy_effort_actions")
            ),
            
            ### Main panel
            policy_effort_ui("policy_effort")    
          )
        )
        
      ),
      # Policy Welfare Impacts --------------------------------------------------------------------
      bslib::nav_panel(
        title = "Summarize Welfare Impacts", 
        id = "welfare_impact",
        value = "welfare_impact",
        bslib::page_fillable(
          bslib::layout_sidebar(
            fillable = TRUE,
            fill = TRUE,
            
            ### Sidebar
            sidebar = bslib::sidebar( 
              fillable = TRUE, 
              fill = TRUE, 
              width = 400,
              
              checklist_ui("policy_welfare_checklist"),
              hr(),
              other_actions_ui("policy_welfare_actions")
            ),
            
            ### Main panel
            policy_welfare_ui("policy_welfare")    
          )
        )
        
      )
    )
  )
}
