source("fleetUI.R", local = TRUE)
source("fleet_helpers.R", local = TRUE)
source("map_viewer_app.R", local = TRUE)
source("zone_outsample_UI.R", local = TRUE)
source("zone_outsample_server.R", local = TRUE)
source("zone_closure_UI.R", local = TRUE)
source("run_policy_UI.R", local = TRUE)
source("run_policy_server.R", local = TRUE)
source("checklist_module.R", local = TRUE)


## USER INTERFACE    
ui = function(request){
  bslib::page_navbar(
    theme = bslib::bs_theme(primary = "#41729F", secondary = "#AACDE5", 
                            info = "#274472",
                            font_scale = 0.9,
                            preset = "cerulean"),
    id = "tabs",
    
    
    # Pop up information icons
    header= tags$head(
      shinyjs::useShinyjs(),
      use_prompter(),
      tags$style(".fa-info-circle {color:#0066FF}"),
      
      tags$style(".fa-exclamation-circle {color:#FF0066}"),
      
      
      #--
      # Formatting ----
      #--
      tags$style(HTML("
                                .multicol { 
                                  height: 150px;
                                -webkit-column-count: 3; /* Chrome, Safari, Opera */ 
                                -moz-column-count: 3;    /* Firefox */ 
                                column-count: 3; 
                                -moz-column-fill: auto;
                                -column-fill: auto;
                                } 
                              .checkbox { 
                                margin-left: 0px;
                                margin-right: 10px;
                                }
                              .checkbox+.checkbox {
                              margin-left: 0px;
                              margin-right: 10px;
                              }
                              .sidebar { height: 90vh; overflow-y: auto; }
                              .dataTables_wrapper { overflow-x: scroll; }
                               
                              .fa-check {
                                 color:green;
                                 font-size: 1.5em;
                              }     
                              .fa-times {
                                 color:red;
                                 font-size: 1.5em;
                              }
                              .fa-exclamation-triangle {
                                 color: #FFCC00;
                                 font-size: 1.5em;
                              }
                              #cl-unorList {
                                 list-style-type: none;
                                 text-align: justify;
                              }
                              .modal-xl {
                                 width: 100%;
                              }
                              .modal-body {
                                 overflow-x: auto;
                              }
                    " )),
      tags$script(HTML('Shiny.addCustomMessageHandler("jsCode",
                                 function(message) {
                                 eval(message.value);
                                 });')),
      tags$style(HTML("#shiny-notification-panel {
                                position: fixed;
                                bottom: 350px;
                                right: 250px;
                                font-size: 25px;
                              }
                              .shiny-notification {
                                position:relative;
                                top: calc(50% - 150px);
                                left: calc(50% - 150px);
                                width: 500px;
                              }"
      )),
      
      tags$style(HTML(".shiny-notification {
                      background-color:#AACDE5;
                      color:#000000;
                      }"
      )),
      
      tags$style(HTML(".shiny-notification-message {
                      background-color:#B5E26F;
                      color:#000000;
                      }"
      )),
      
      tags$style(HTML(".shiny-notification-warning {
                      background-color:#FCE205;
                      color:#000000;
                      }"
      )),
      
      tags$style(HTML(".shiny-notification-error {
                      background-color:#BA110C;
                      color:#000000;
                      }"
      ))
    ),
    
    
    #---
    # Information ----
    #---
    bslib::nav_menu(title = "Information",header = NULL, footer = NULL,
                    
                    bslib::nav_panel( title = "Background",
                                      fluidRow(
                                        column(width = 12,
                                               tags$div(tags$p( tags$h2('FishSET - Spatial Economic Toolbox for Fisheries', 
                                                                                  style="color:darkblue; text-align:center; font-size: 32px;")),
                                                        tags$div(style="display: inline-block; align:right; border: 3px solid black;", img(src="SandPoint_Boats.JPG", height='10%', width='100%', align='center')),
                                                        tags$br(),tags$br(),
                                                        tags$p('FishSET is a set of statistical programming and data management tools 
                                                              developed to improve fishery modeling. The tools standardize data management and organization,
                                                               enable the use of location choice models to provide input into fishery management,
                                                                 and provide various other modeling and visualization tools.'),
                                                        tags$p('The FishSET toolbox is provided as a set of R functions that can be run in an R console 
                                                               or here in this FishSET Graphical User Interface (FishSET GUI).'),
                                                        tags$p('The FishSET GUI is divided into tabs to guide users through the steps of creation, 
                                                                from uploading and exploring data to developing and evaluating models.
                                                                Tabs can be navigated in any order.  All data is automatically saved to a SQL database called the 
                                                               FishSET database when first loaded. The database is housed in the projects directory withing the FishSETFolder directory. 
                                                               The FishSETFolder directory is created the first time data is is loaded into FishSET.
                                                                Modified versions of the data can be saved to the FishSET database in the', tags$em('Data Quality Evaluation'), 'and', 
                                                               tags$em('Compute New Variables'), 'tabs.   
                                                                Plots and table outputs are saved in an output folder of the project directory. 
                                                              Function calls, including chosen parameters, are saved to the', tags$em('src'), 'folder in the projects directory. 
                                                              The', tags$em('Quickstart Guide'), 'subtab provides further assistance on using the FishSET GUI.',
                                                              "The", tags$em('User Manual'), "provides greater detail and background on functions."),
                                                        tags$p('For questions and comments please contact: nmfs.fishset@noaa.gov'))
                                        )
                                      )
                    ),
                    bslib::nav_panel(title ='Quickstart guide',
                                 bslib::page_fluid(
                                   tags$h2("Quickstart guide", style="color:darkblue; text-align:center;"),
                                     tags$div(
                                       selectizeInput('QuickStartChoices',"Select an option below for a brief guide for each tab.", 
                                                      choices = c('Information across all tabs'='AcrossTabs',
                                                                                        'Upload Data tab'='UploadTab',
                                                                                        'Data Quality Evaluation tab'='DQTab',
                                                                                        'Data Exploration tab'='ExplorTab',
                                                                                        'Fleet Analysis and Assignment tab' = 'FleetTab',
                                                                                        'Simple Analyses tab'='AnalTab',
                                                                                        'Compute New Variables tab'='NewVarsTab',
                                                                                        'Map Viewer tab' = 'MapTab',
                                                                                        'Define Alternative Fishing Choices'='ZonalTab',
                                                                                        'Expected Catch/Revenue tab'='ExpectedTab',
                                                                                        'Models tab'='ModelTab',
                                                                                        'Bookmark Choices tab'='BookmarkTab'
                                       ), width = '50%', selected='AcrossTabs'), style="font-size:100%; font-weight:bold;"
                                     
                                     ),
                                   hr(),
                                     uiOutput('AcrossTabsText'), 
                                     uiOutput('UploadTabsText'),
                                     uiOutput('ExploreTabsText'),
                                     uiOutput('DQTabsText'),
                                     uiOutput('FleetText'),
                                     uiOutput('AnalTabsText'),
                                     uiOutput('NewVarsTabsText'),
                                     uiOutput('MapTabsText'),
                                     uiOutput('ZonalTabsText'),
                                     uiOutput('ExpectedTabsText'),
                                     uiOutput('ModelTabsText'),
                                     uiOutput('BookmarkTabsText')
                                 )
                    ),
                    bslib::nav_panel(title ="Alaska Details",
                                     fluidRow(
                                       column(width = 6,
                                              tags$div(
                                                tags$p(
                                                  tags$br(), 
                                                  'This page includes information on AK catch and management data with some links and tables.'
                                                ), 
                                                tags$p('Relevant papers', 
                                                       tags$br(),
                                                       tags$a('North Pacific Fleet Profiles (2012)', href='FleetProfiles2012.pdf', target="_blank"),
                                                       tags$br(),
                                                       tags$a('Fisheries Catch and Landings Reports in Alaska', href='https://www.fisheries.noaa.gov/alaska/commercial-fishing/fisheries-catch-and-landings-reports-alaska', target="_blank")
                                                )
                                              )
                                       ) 
                                     )
                    ),
                    bslib::nav_panel(title ="FishSET Background",
                                     fluidRow(
                                       column(width = 6,
                                              tags$div(
                                                tags$p(
                                                  tags$br(),
                                                  'The Spatial Economics Toolbox for Fisheries (FishSET) is a collaborative effort by economists and data scientists at the NOAA Alaska 
                                                        Fisheries Science Center, NOAA Fisheries Office of Science and Technology, other NOAA Fisheries Science Centers, the Pacific States 
                                                        Marine Fisheries Commission (PacStates), the Alaska Fisheries Information Network (AKFIN), and academic partners at many universities, ICES, 
                                                        PICES, IIFET, NAAFE, and others.'),
                                                tags$p('Partner links', tags$br(),
                                                       tags$a('National Marine Fisheries Science Service (NMFS)', href='https://www.fisheries.noaa.gov/', target="_blank"), 
                                                       tags$br(),
                                                       tags$a('NOAA Office of Science and Technology Economics Program', href='https://www.st.nmfs.noaa.gov/economics/', target="_blank"),
                                                       tags$br(),
                                                       tags$a("NOAA Alaska Fisheries Science Center", href = 'https://www.fisheries.noaa.gov/about/alaska-fisheries-science-center', target="_blank"),
                                                       tags$br(),
                                                       tags$a("Other NOAA Fisheries Science Centers", href = 'https://www.fisheries.noaa.gov/', target="_blank"),
                                                       tags$br(),
                                                       tags$a('Pacific Stats Marine Fisheries Commission', href = 'https://www.psmfc.org/psmfc-info', target="_blank"),
                                                       tags$br(),
                                                       tags$a('AKFIN', href = 'https://akfin.psmfc.org/', target="_blank")
                                                )
                                              )
                                       ) 
                                     )
                    ) 
    ),
    
    
    #---
    # Upload data tabset panel ---- 
    #---
    bslib::nav_panel(title = "Upload Data", id = "upload", 
                     bslib::page_fillable(
                       #tags$style(type='text/css', "#uploadMain { width:100%; margin-top: 24px;margin-left:-20px;padding-left:2px; padding-right:5px}"),
                       bslib::layout_sidebar(fillable = TRUE, fill = TRUE,
                       sidebar = bslib::sidebar( fillable = TRUE, fill = TRUE, width = 400,
                                                 h5(strong("Steps for uploading data:")),
                                                 
                                                 p("1. Select folder that contains 'FishSETFolder'."),
                                                 
                                                 actionButton('change_fs_folder', 'Change FishSET Folder',
                                                              class = "btn-primary"), 
                                                 p("2. Enter name of project. If uploading from FishSET database, projects will appear after 'FishSET database' is selected."),
                                                 p("3. Select files to upload."),
                                                 p("4. Click on 'Load data' to create FishSET database and upload files into tables."),
                                                # splitLayout(
                                                 actionButton(inputId = 'loadDat',label ="Load data", class = "btn-primary"#,width = "100%"
                                                              ),
                                                 actionButton("refresh", "Refresh data",
                                                              class = "btn-primary",
                                                              icon = icon('sync', verify_fa = FALSE),
                                                              #width = "100%"
                                                              ) ,

                                                 tags$br(), 
                                                
                                               # splitLayout(
                                                 actionButton(inputId ="meta_modal", label ="Metadata",  class = "btn-secondary", 
                                                              #style = "padding-left: 40px; padding-right: 42px;"
                                                              ),
                                                 actionButton(inputId ="delete_tabs_bttn", label ="Manage Tables", class = "btn-secondary", 
                                                            #  style = "padding-left: 27px; padding-right: 25px;"
                                                            ),
                                                 uiOutput('load_manage_proj_ui'),
                                                 
                                                conditionalPanel("input.loadDat > 0", # TODO: update to a more reliable method
                                                                # splitLayout(
                                                                   actionButton("confid_modal", "Confidentiality",
                                                                                class = "btn-secondary", 
                                                                               # style = "padding-left: 25px; padding-right: 25px;"
                                                                               ),
                                                                   actionButton("reset_modal", "Reset log",
                                                                                class = "btn-secondary", 
                                                                               # style = "padding-left: 45px; padding-right: 45px;"
                                                                               ),
                                                                   actionButton("plot_set", "Plot settings", 
                                                                                class = "btn-secondary", 
                                                                                #style = "padding-left: 35px; padding-right: 35px;"
                                                                               # )
                                                                )),
                                                
                                                
                                               
                                                bslib::accordion(open = FALSE,
                                                  bslib::accordion_panel(
                                                    "Other actions", icon = bsicons::bs_icon("menu-app"), 
                                                    textInput('notesUp', "Notes", value=NULL,
                                                              placeholder = 'Write notes to store in text output file. Text can be inserted into report later.'),

                                                    actionButton('callTextDownloadUp','Save notes', class = "btn-success"),

                                                    tags$br(), tags$br(),
                                                    textInput("exprUp", label = "Enter an R expression",
                                                              value = "values$dataset"),
                                                    actionButton("runUp", "Run", class = "btn-success"),
                                                    div(style = "margin-top: 2em;",
                                                        uiOutput('resultUp')                                                  )
                                                 ),
                                                 tags$br(),
                                                 actionButton("closeUp", "Close app", icon = icon("circle-xmark"),
                                                              width = "100%",                                                           
                                                              class = "btn-danger", 
                                                              onclick = "setTimeout(function(){window.close();},500);")
                                                )
                                                 
                       
                       ),
                       
                       
                       tags$br(),
                         bslib::card(fill = FALSE,
                          # height = 200,
                          bslib::card_body( 
                                   uiOutput('fish_folder_path'), 
                                   uiOutput("projects"))
                         ), # define project name
                         
                       bslib::layout_column_wrap( fill = FALSE,
                         width = 1/3,
                         bslib::card(
                          # height = 400,
                           bslib::card_body( 
                           radioButtons('load_main_src', tags$strong("Source primary data from:"),
                                             choices=c('Upload new file','FishSET database'), 
                                             selected='Upload new file', inline=TRUE),
                           uiOutput('main_upload'),

                         uiOutput('ui.action2'))
                         ),

                     bslib::card(
                     #  height = 400,
                            radioButtons('load_port_src', tags$strong("Source port data from:"), 
                                                choices=c('Upload new file','FishSET database'), 
                                                inline=TRUE),

                         uiOutput('port_upload'),
                         uiOutput('ui.actionP2'),
                         uiOutput("portAddTable"),
                         uiOutput("PortAddtableMerge")),
                       
                     bslib::card(
                      # height = 400,
                                radioButtons('load_spat_src', tags$strong("Source spatial data from:"), 
                                                choices=c('Upload new file', 'FishSET database'), 
                                                selected='Upload new file', inline=TRUE),
                         radioButtons('filefolder', "", choices=c("Upload single file", "Upload shape files"), 
                                      selected="Upload single file", inline = TRUE),
                         uiOutput('spatial_upload'),
                         uiOutput('spatial_upload2'))
                     ),

                     bslib::layout_column_wrap( fill = FALSE,
                       width = 1/2,
                     bslib::card(
                      # height = 300,
                                radioButtons('load_grid_src', tags$strong("Source gridded data from:"), 
                                                choices=c('Upload new file','FishSET database'), 
                                                selected='Upload new file', inline=TRUE),
                         uiOutput('grid_upload'),
                         uiOutput('grid_upload2'),
                         uiOutput('gridded_uploaded')),

                     bslib::card(
                      # height = 300,
                                radioButtons('load_aux_src', tags$strong("Source auxiliary data from:"), 
                                                choices=c('Upload new file','FishSET database'), 
                                                selected='Upload new file', inline=TRUE),
                         uiOutput('aux_upload'),

                         mergeUI("aux", dat_type = "aux"))
                       
                     )
                     )
                     )
                   ),
    
    
    #---
    # Data quality evaluation tabset panel ----
    #---
    bslib::nav_panel("Data Quality Evaluation", value = "qaqc",
                     bslib::page_fillable(
                       bslib::layout_sidebar(fillable = TRUE, fill = TRUE,
                                      sidebar = bslib::sidebar( fillable = TRUE, fill = TRUE, width = 400,
                                                                checklist_ui("checklist_1"),
                                                                
                                                                actionButton("mod_check", "FishSET checklist",
                                                                             class = "btn-primary"), 
                                                                
                                                                actionButton('saveData','Save data to FishSET database',
                                                                             width = "100%",
                                                                             class = "btn-primary"),
                                                                splitLayout(
                                                                   tabPlotUI("qaqc", type = "tab_plot")
                                                ),
                                                 
                                                 radioButtons("checks", "Select data quality check function to run:", 
                                                              choices = c('Variable class', 'Summary table', 'Outliers', 
                                                                          'NAs', 'NaNs', 'Remove variables', 'Unique observations', 
                                                                          'Empty variables', 'Latitude and Longitude'='Lat_Lon units', 
                                                                          'Spatial data')),
                                                 
                                                 conditionalPanel(condition = "input.checks == 'Variable class'",
                                                   uiOutput('change_var_inputs'),
                                                   actionButton('rchclass', 'Change variable classes', class = "btn-secondary")
                                                 ),
                                                 conditionalPanel("input.checks == 'NAs'",
                                                                  actionButton('NA_Filter_all', 'Remove all NAs',class = "btn-secondary"),
                                                                  actionButton('NA_Filter_mean', 'Replace NAs with mean value', class = "btn-secondary")
                                                 ),
                                                 conditionalPanel("input.checks == 'NaNs'",
                                                                  actionButton('NAN_Filter_all', 'Remove all NaNs', class = "btn-secondary"),
                                                                  actionButton('NAN_Filter_mean', 'Replace NaNs with mean value', class = "btn-secondary")
                                                 ),
                                                 uiOutput('outlier_column'),
                                                 uiOutput('outlier_subset_method'),
                                                 uiOutput('outlier_subset'),
                                                 uiOutput('outlier_dist'),
                                                 conditionalPanel("input.checks=='Outliers'",
                                                                  actionButton('Outlier_Filter', 'Remove outliers', class = "btn-secondary"),
                                                                  uiOutput("hover_info1")
                                                 ),
                                                 conditionalPanel("input.checks=='Unique observations'",
                                                                  actionButton('Unique_Filter', 'Remove non-unique rows', class = "btn-secondary")
                                                 ),
                                                 conditionalPanel("input.checks=='Empty variables'",
                                                                  actionButton('Empty_Filter', 'Remove empty variables', class = "btn-secondary")
                                                 ),
                                                 conditionalPanel("input.checks=='Lat_Lon units'",
                                                                  uiOutput('LatLonDir'),
                                                                  selectInput('LatLon_Filter_Lat', 'Change sign for latitude direction', 
                                                                              choices=c('None', 'All values'='all', 'Positve to negative'='neg', 
                                                                                        'Negative to positive'='pos'), selected='None'),
                                                                  selectInput('LatLon_Filter_Lon', 'Change sign for longitude direction', 
                                                                              choices=c('None', 'All values'='all', 'Positve to negative'='neg', 
                                                                                        'Negative to positive'='pos'), selected='None'),
                                                                  actionButton('LatLon_Filter', 'Convert data', 
                                                                               value=FALSE, class = "btn-secondary")
                                                 ),
                                                 
                                                 conditionalPanel("input.checks=='Spatial data'",
                                                                  
                                                                  conditionalPanel("input.spat_qaqc_tab == 'checks'",         
                                                                                   uiOutput("spatQAQC_checkUI")),
                                                                  
                                                                  conditionalPanel("input.spat_qaqc_tab == 'corrections'",
                                                                                   uiOutput("spatQAQC_correctUI"))
                                                 ),
                                                 bslib::accordion(open = FALSE,
                                                   bslib::accordion_panel(
                                                     "Other actions", icon = bsicons::bs_icon("menu-app"), open = FALSE,
                                                     
                                                     textInput('notesQAQC', "Notes", value=NULL,
                                                               placeholder = 'Write notes to store in text output file. Text can be inserted into report later.'),
                                                     actionButton('callTextDownloadQAQC','Save notes', class = "btn-success"),
                                                     
                                                     tags$br(), tags$br(),
                                                     ##Inline scripting 
                                                     

                                                     textInput("exprQA", label = "Enter an R expression",
                                                               value = "values$dataset"),
                                                     actionButton("runQA", "Run", class = "btn-success"),
                                                     div(style = "margin-top: 2em;",
                                                         uiOutput('resultQA')
                                                     ),
                                                     
                                                   )  
                                                     
                                                ),
                                                 actionButton("closeQAQC", "Close app", icon = icon("circle-xmark"),
                                                              width = "50%",
                                                              class = "btn-danger",
                                                              onclick = "setTimeout(function(){window.close();},500);")
                     
                       ),#END SIDEBAR LAYOUT             
                       tags$br(),
                       htmlOutput("Case"),
                       conditionalPanel("input.checks == 'Remove variables'",
                                        uiOutput('remove_vars'),
                                          actionButton("removeVars", "Remove variables", class = "btn-secondary", width = "25%")
                                          
                                        
                       ),
                       conditionalPanel("input.checks == 'Variable class'",
                                        DT::dataTableOutput('changetable') ),
                       conditionalPanel("input.checks=='Summary table'",
                                        DT::DTOutput("output_table_summary")),
                       conditionalPanel("input.checks=='Outliers' && input.column_check == ''",
                                        plotOutput('outlierbox')),  
                       
                       conditionalPanel("input.checks=='Outliers'",
                                        
                                        shinycssloaders::withSpinner(DT::DTOutput("output_table_outlier"), type = 6),
                                        
                                        tags$br(),
                                        htmlOutput("outlier_fig_title"),
                                        
                                        splitLayout(cellWidths = c('33%','33%','33%'),
                                                    shinycssloaders::withSpinner(plotOutput('plot1',
                                                                                            hover = hoverOpts("plot1_hover", delay = 100, delayType = "debounce"),
                                                                                            dblclick = "plot1_dblclick", 
                                                                                            brush = brushOpts(id = "plot1_brush",resetOnNew = TRUE)
                                                    ), type = 6),
                                                    shinycssloaders::withSpinner(plotOutput('plot2', dblclick = "plot2_dblclick", 
                                                                                            brush = brushOpts(id = "plot2_brush",resetOnNew = TRUE)), type = 6),
                                                    shinycssloaders::withSpinner(plotOutput('plot3',
                                                                                            hover = hoverOpts("plot3_hover", delay = 100, delayType = "debounce"),
                                                                                            dblclick = "plot3_dblclick", 
                                                                                            brush = brushOpts(id = "plot3_brush",resetOnNew = TRUE)
                                                    ), type = 6))
                       ),
                       
                       conditionalPanel("input.checks=='NAs'",
                                        DT::DTOutput('missingtable')),
                       DT::DTOutput('output_table_latlon'),
                       
                       conditionalPanel("input.checks=='Spatial data'",
                                        
                                        bslib::navset_tab(id = "spat_qaqc_tab", selected = "checks",
                                                          bslib::nav_panel(title = "Spatial Checks", value = "checks",
                                                                           shinycssloaders::withSpinner(uiOutput("spatQAQC_checkOut"), type = 6)      
                                                          ),
                                                          bslib::nav_panel(title = "Spatial Corrections", value = "corrections", 
                                                                           uiOutput("spat_correct_msg"),
                                                                           radioButtons("select_spat_tab", "Show:",
                                                                                        choices = c("all", "points outside zone" = "out_zone")),
                                                                           tags$div(shinycssloaders::withSpinner(DT::DTOutput("spat_correct_tab"), type = 6), 
                                                                                    style = "font-size: 75%; width: 100%")
                                                          )))
                     )
                     )
    ),
    
    #---
    # Data exploration tabset panel ----
    #---
    bslib::nav_menu(title = "Explore the Data",header = NULL, footer = NULL,
                    bslib::nav_panel(title = "Data Exploration", id = "explore",
                                     bslib::page_fillable(
                                       bslib::layout_sidebar(fillable = TRUE, fill = TRUE,
                                                      sidebar = bslib::sidebar( fillable = TRUE, fill = TRUE, width = 400,
                                                                 actionButton('saveDataExplore','Save data to FishSET database',
                                                                              width = "100%",
                                                                              class = "btn-primary"),

                                                                 conditionalPanel("input.plot_table=='Plots'",
                                                                                  splitLayout(tabPlotUI("explore"))
                                                                 
                                                                 ),
                                                                 conditionalPanel("input.SelectDatasetExplore=='main' || input.SelectDatasetExplore=='grid'",
                                                                                  selectInput('plot_table', 'View data table or plots', 
                                                                                              choices=c('Table','Plots'), selected='Table')
                                                                 ),
                                                                 
                                                                 uiOutput("SelectDatasetExploreUI"), 
                                                                 
                                                                 conditionalPanel("input.SelectDatasetExplore=='main' && input.plot_table=='Plots'",
                                                                                  selectInput('plot_type', 'Select Plot Type', 
                                                                                              choices=c('Temporal','Spatial-autocorrelation', 'Spatial-zone summary','x-y plot'))
                                                                 ),
                                                                 
                                                                 conditionalPanel("input.SelectDatasetExplore=='main' && input.plot_table=='Plots' && input.plot_type=='Temporal'",
                                                                                  actionButton("run_temporal_plot", "Run temporal plots", width = "100%",
                                                                                               class = "btn-secondary")
                                                                 ),
                                                                 
                                                                 conditionalPanel("input.SelectDatasetExplore=='main' && input.plot_table=='Plots' && input.plot_type=='Spatial-autocorrelation'",
                                                                                  uiOutput("mtgt_output"),
                                                                                  uiOutput('mtgt_output_secondary'),
                                                                                  uiOutput('mtgt_out2'),
                                                                                  uiOutput("location_info_spatial")),
                                                                 
                                                                 conditionalPanel("input.SelectDatasetExplore=='main' && input.plot_table=='Plots' && input.plot_type=='Spatial-zone summary'",
                                                                                  uiOutput("zone_summary_out1"),
                                                                                  actionButton("run_zone_summ", "Run zone summary", width = "100%",
                                                                                               class = "btn-secondary")
                                                                 ),
                                                                 
                                                                 conditionalPanel("input.SelectDatasetExplore=='main' && input.plot_table=='Table'",
                                                                                  verbatimTextOutput('editText')
                                                                 ),
                                                                 
                                                                 conditionalPanel("input.SelectDatasetExplore == 'grid' && input.plot_table == 'Plots'",
                                                                                  uiOutput("plot_grid_args"),
                                                                 ),
                                                                 
                                                                 bslib::accordion(open = FALSE,
                                                                                  bslib::accordion_panel(
                                                                                    "Other actions", icon = bsicons::bs_icon("menu-app"), 
                                                                                    
                                                                                    textInput('notesExplore', "Notes", value=NULL, 
                                                                                              placeholder = 'Write notes to store in text output file. Text can be inserted into report later.'),
                                                                                    
                                                                                    actionButton('callTextDownloadExplore','Save notes', class = "btn-success"),
                                                                                    
                                                                                    tags$br(), tags$br(),
                                                                                    
                                                                                    textInput("exprExplore", label = "Enter an R expression",
                                                                                              value = "values$dataset"),
                                                                                    actionButton("runExplore", "Run", class = "btn-success"),
                                                                                    div(style = "margin-top: 2em;",
                                                                                        uiOutput('resultExplore')
                                                                                        
                                                                                    )
                                                                                  )
                                                                 ),
                                                                 actionButton("closeExplore", "Close app", icon = icon("circle-xmark"),
                                                                              width = "50%",                                                           
                                                                              class = "btn-danger", 
                                                                              onclick = "setTimeout(function(){window.close();},500);")
                                                                 
                                       ),
                                       
                                       tags$br(),
                                       tags$div(shinycssloaders::withSpinner(DT::DTOutput("output_table_exploration"), type = 6), 
                                                style = "font-size: 75%; width: 100%"),
                                       
                                       conditionalPanel(
                                         "input.SelectDatasetExplore=='main' && input.plot_table=='Plots' && input.plot_type=='Temporal'", 
                                         uiOutput('column_select'), 
                                         tagList(
                                           tags$br(),tags$br(),
                                           fluidRow(column(12, align='right',
                                                           div(style="display: inline-block;vertical-align:top;  width: 33%;",
                                                               selectInput("p2fun", label = "y-axis function:",
                                                                           c("No. observations",'No. unique observations',
                                                                             '% of total observations'), 
                                                                           selected='No. of observations')),
                                                           
                                                           div(style="display: inline-block; vertical-align:top; width: 33%;",
                                                               selectInput("p3fun", "y-axis function:",
                                                                           c('mean','median','min','max','sum'), 
                                                                           selected = 'mean'))))
                                         ),
                                         shinycssloaders::withSpinner(plotOutput('plot_time'), type = 6),
                                       ),
                                       
                                       conditionalPanel(
                                         "input.SelectDatasetExplore=='main' && input.plot_table=='Plots' && input.plot_type=='Spatial-autocorrelation'",
                                         
                                         column(shinycssloaders::withSpinner(plotOutput('plot_spatial',
                                                                                        click = "plot_spatial_click",
                                                                                        dblclick = "plot_spatial_dblclick",
                                                                                        brush = brushOpts(id = "plot_spatial_brush", resetOnNew = FALSE)), type = 6), width=7),
                                         column(shinycssloaders::withSpinner(plotOutput('map_kernel'), type = 6), width=7),
                                         column(shinycssloaders::withSpinner(DT::DTOutput('output_table_gt_mt'), type = 6), width=6)
                                         
                                       ),
                                       
                                       conditionalPanel(
                                         "input.SelectDatasetExplore=='main' && input.plot_table=='Plots' && input.plot_type=='Spatial-zone summary'",
                                         fluidRow(
                                           column(shinycssloaders::withSpinner(plotly::plotlyOutput('plot_zone_summary', width = '850px', height = '550px'),
                                                                               type = 6), width=10)
                                         )
                                       ),
                                       
                                       conditionalPanel(
                                         "input.SelectDatasetExplore=='main' && input.plot_table=='Plots' && input.plot_type=='x-y plot'",
                                         uiOutput('xy_selectUI'),
                                         shinycssloaders::withSpinner(plotOutput('plot_xy'), type = 6)
                                       ),
                                       
                                       # gridded data plot 
                                       conditionalPanel(
                                         "input.SelectDatasetExplore == 'grid' && input.plot_table == 'Plots'",
                                         shinycssloaders::withSpinner(plotOutput("grid_plot"), type = 6)
                                       )
                                     ))
                                     ),
                    
                    
                    #---
                    ## Basic analyses tabset panel ----
                    #---
                    bslib::nav_panel(title = "Simple Analyses", id = "analysis",#value
                                     bslib::page_fillable(
                                       bslib::layout_sidebar(fillable = TRUE, fill = TRUE,
                                                      sidebar = bslib::sidebar( fillable = TRUE, fill = TRUE, width = 400,
                                                                 splitLayout(
                                                                 tabPlotUI("anal")),
                                                                 
                                                                   modSaveUI("anal"),
                                                              
                                                                 selectInput('corr_reg','Show correlations or simple linear regression', 
                                                                             choices=c('Correlation','Regression'), selected='Correlation'),
                                                                 
                                                                 conditionalPanel("input.corr_reg == 'Correlation'", 
                                                                                  
                                                                         actionButton("run_corr", "Run function", width = "50%", class = "btn-primary")
                                                                 ),
                                                                 
                                                                 conditionalPanel("input.corr_reg == 'Regression'", 
                                                                                  
                                                                                  actionButton("run_reg", "Run function",  class = "btn-primary")
                                                                 ),
                                                                 bslib::accordion(open = FALSE,
                                                                                  bslib::accordion_panel(
                                                                                    "Other actions", icon = bsicons::bs_icon("menu-app"), 
                                                                                    
                                                                                    textInput('notesAnal', "Notes", value=NULL, 
                                                                                              placeholder = 'Write notes to store in text output file. Text can be inserted into report later.'),
                                                                                    
                                                                                    actionButton('callTextDownloadAnal','Save notes', class = "btn-success"),
                                                                                    
                                                                                    tags$br(), tags$br(),
                                                                                    
                                                                                    textInput("exprAnal", label = "Enter an R expression",
                                                                                              value = "values$dataset"),
                                                                                    actionButton("runAnal", "Run", class = "btn-success"),
                                                                                    div(style = "margin-top: 2em;",
                                                                                        uiOutput('resultA')
                                                                                        
                                                                                    )
                                                                                  )
                                                                 ),
                                                                 actionButton("closeAnalysis", "Close app", icon = icon("circle-xmark"),
                                                                              width = "50%",                                                           
                                                                              class = "btn-danger", 
                                                                              onclick = "setTimeout(function(){window.close();},500);")
                                       ),
                                       tags$br(),
                                       conditionalPanel("input.corr_reg=='Correlation'",
                                                        uiOutput('corr_out'),
                                                        # verbatimTextOutput('output_text_corr'), # This part doesn't show in the Shiny
                                                        div(DT::DTOutput('output_table_corr'), style = "font-size: 75%; width: 100%"),
                                                        tags$br(), tags$br(),
                                                        shinycssloaders::withSpinner(plotOutput('output_plot_corr', width='100%', height = "600px"), type = 6)
                                       ),
                                       conditionalPanel("input.corr_reg=='Regression'",
                                                        uiOutput('reg_resp_out'),
                                                        uiOutput('reg_exp_out'),
                                                        shinycssloaders::withSpinner(verbatimTextOutput("output_text_reg"), type= 6),
                                                        tags$br(),
                                                        shinycssloaders::withSpinner(plotOutput("output_plot_reg", width='100%', height = "600px"), type = 6)
                                       ))
                    )
                     
    )),
    
    
    #---
    # Map Viewer ----
    #---
    bslib::nav_panel(title = "Map Viewer",
                     map_viewerUI("map")
                     
    ),
    
    
    #---
    # Create new variables tabset panel ----
    #---
    bslib::nav_panel(title = 'Compute New Variables', id='new',#value
                     bslib::page_sidebar(
                       sidebar = bslib::sidebar(  width = 400,
                                                   actionButton('saveDataNewVars','Save data to FishSET database',
                                                                width = "100%",
                                                                class = "btn-primary"),
                                                 selectInput('VarCreateTop', "Create variables based on", multiple=FALSE,  
                                                             choices=c('Nominal ID', 'Arithmetic functions', 
                                                                       'Dummy variables', 'Temporal functions',
                                                                       'Spatial functions', 'Trip-level functions', 
                                                                       'Data transformations'),
                                                             selected = 'Spatial functions'),
                                                 
                                                 ## Function options              
                                                 conditionalPanel("input.VarCreateTop=='Data transformations'",
                                                                  selectInput('trans','Functions', 
                                                                              choices = c('Coded variable based on quantiles'='set_quants',
                                                                                          'Within-group percentage' = 'group_perc',
                                                                                          'Within-group lagged difference' = 'group_diff',
                                                                                          'Within-group running sum' = 'group_cumsum'),
                                                                              selected = 'set_quants'), multiple = FALSE),
                                                 conditionalPanel("input.VarCreateTop=='Temporal functions'",
                                                                  selectInput('tempfunc', 'Functions', 
                                                                              choices = c("Change time unit"='temp_mod',
                                                                                          'Duration of time between two temporal variables'='create_duration'),
                                                                              selected = 'temp_mod', multiple=FALSE)),
                                                 conditionalPanel("input.VarCreateTop=='Nominal ID'",
                                                                  selectInput('ID','Functions', 
                                                                              choices = c('Create haul or trip ID based on variables'='ID_var',
                                                                                          'Create haul or trip ID based on row numbers'='ID_seq_var',
                                                                                          'Create binary fishery season identifier'='binary_seasonID',
                                                                                          'Create location, gear, species-specific fishery season identifier'='create_seasonal_ID'),
                                                                              multiple = FALSE, selected='ID_var')),
                                                 conditionalPanel("input.VarCreateTop=='Arithmetic functions'",
                                                                  selectInput('numfunc','Functions', 
                                                                              choices = c('Numeric functions'='create_var_num',
                                                                                          'Catch or revenue per unit effort'='cpue',
                                                                                          'Scale catch variable'='scale_catch'),
                                                                              selected = 'create_var_num', multiple = FALSE)),
                                                 conditionalPanel("input.VarCreateTop=='Dummy variables'",
                                                                  selectInput('dummyfunc','Functions',
                                                                              choices = c('From variable', 'From policy dates', 'From area closures'),
                                                                              selected = 'From variable', multiple=FALSE)),
                                                 conditionalPanel("input.VarCreateTop=='Spatial functions'",
                                                                  selectInput('dist','Functions', 
                                                                              choices = c('Assign observations to zones'='zone', 
                                                                                          'Zonal centroid' = 'zone_cent',
                                                                                          'Fishery centroid' = 'fish_cent',
                                                                                          'Distance between two points'='create_dist_between',
                                                                                          'Midpoint location (lon/lat) for each haul'='create_mid_haul',
                                                                                          'Zone when choice of where to go next was made'='create_startingloc'),
                                                                              selected='zone', multiple = FALSE)),
                                                 conditionalPanel("input.VarCreateTop=='Trip-level functions'",
                                                                  selectInput('trip','Functions', 
                                                                              choices = c('Collapse haul to trip'='haul_to_trip','Calculate trip distance'='trip_distance',
                                                                                          'Calculate trip centroid'='trip_centroid'),
                                                                              selected='haul_to_trip', multiple = FALSE)),
                                                 conditionalPanel("!((input.VarCreateTop=='Spatial functions' && input.dist=='zone_cent')||
                                               (input.VarCreateTop=='Trip-level functions' && (input.trip=='haul_to_trip'||input.trip=='trip_centroid')))", 
                                               add_prompter(
                                                 textInput('varname', list('Name of new variable', icon('info-circle', verify_fa = FALSE)), 
                                                           value = '', placeholder = ''),
                                                 position = "bottom", type='info', size='medium', 
                                                 message = "If left empty, default names will be supplied.")),
                                               
                                               #More sub choices Data Transformations     
                                               
                                               conditionalPanel("input.VarCreateTop=='Temporal functions'&&input.tempfunc=='temp_mod'",
                                                                style = "margin-left:19px;", 
                                                                uiOutput('trans_time_out'),
                                                                selectInput('define_format','Temporal units to return data in',
                                                                            choices=c(NULL, 'year', "month","day", 'hour', 'minute'), 
                                                                            selected = 'year'),
                                                                textInput('timezone', "Optional: Define timezone", value=NULL, placeholder = c("Examples: 'UTC', 'America/New_York'"))),
                                               
                                               conditionalPanel("input.VarCreateTop=='Data transformations'&&input.trans=='set_quants'",
                                                                style = "margin-left:19px;", 
                                                                uiOutput('trans_quant_name'),
                                                                selectInput('quant_cat','Quantile categories',
                                                                            choices=c('0%, 20%, 40%, 60%, 80%, 100%'='0.2', 
                                                                                      '0%, 25%, 50%, 75%, 100%'='0.25', 
                                                                                      '0%, 10%, 50%, 90%, 100%'='0.4'))),
                                               #More sub choices Nominal IDS  
                                               conditionalPanel("input.VarCreateTop=='Nominal ID'&&input.ID=='ID_var'",
                                                                style = "margin-left:19px;", 
                                                                uiOutput('unique_col_id')
                                               ),
                                               
                                               conditionalPanel("input.VarCreateTop=='Nominal ID'&&input.ID=='binary_seasonID'",
                                                                style = "margin-left:19px;", 
                                                                fileInput("seasonal.dat", "Choose data file containing data on fishery seasons",
                                                                          multiple = FALSE),
                                                                textInput('seasonstart', 'Fishery season start date', value='', placeholder = 'Type variable name or date'),
                                                                textInput('seasonend', 'Fishery season end date', value='', placeholder = 'Type variable name or date'),
                                                                checkboxInput('overlap', 'Include fishing dates that extend beyond season dates', value=FALSE)
                                               ),
                                               
                                               conditionalPanel("input.VarCreateTop=='Nominal ID'&&input.ID=='create_seasonal_ID'",
                                                                style = "margin-left:19px;", 
                                                                fileInput("seasonal.dat", "Choose data file containing data on fishery seasons",
                                                                          multiple = FALSE),
                                                                
                                                                uiOutput('sp_col.select'),
                                                                checkboxInput('sp_collocation', "Optional: Do fishery season dates depend on fishery location?", 
                                                                              value=FALSE), # note: not used by any function in server
                                                                checkboxInput('sp_colgeartype', "Optional: Do fishery season dates depend on fishery geartype?",
                                                                              value=FALSE),# note: not used by any function in server
                                                                textInput('target', "Optional: Name of target species", value=NULL)
                                               ),
                                               
                                               #More sub choices Arithmetic functions  
                                               
                                               conditionalPanel("input.VarCreateTop=='Arithmetic functions'&&input.numfunc=='create_var_num'",
                                                                style = "margin-left:19px;", 
                                                                uiOutput('var_xy_select'),
                                                                selectInput('create_method', 'Arithmetic expression', 
                                                                            choices=c('addition', 'subtraction', 'multiplication', 'division'))
                                               ),
                                               
                                               conditionalPanel("input.VarCreateTop=='Arithmetic functions'&&input.numfunc=='scale_catch'",
                                                                style = "margin-left:19px;",
                                                                uiOutput('scale_catch_select')
                                               ),
                                               
                                               uiOutput('grp_perc'),
                                               uiOutput('grp_diff'),
                                               uiOutput('grp_cumsum'),
                                               
                                               conditionalPanel("input.VarCreateTop=='Temporal functions'&&input.tempfunc=='create_duration'",
                                                                style = "margin-left:19px;", 
                                                                uiOutput('input_dur'),
                                                                selectInput('dur_units', 'Unit of time for calculating duration', 
                                                                            choices = c("week", "day", "hour", "minute"), selected='week')
                                               ),
                                               
                                               conditionalPanel("input.VarCreateTop=='Arithmetic functions'&&input.numfunc=='cpue'",
                                                                style = "margin-left:19px;",
                                                                uiOutput("input_cpue")
                                               ),
                                               
                                               
                                               #dummy_var        Vector of TRUE or FALSE the length  (rows) of the dataset. 
                                               #dummy_matrix     Matrix with same dimensions at the data set filled with TRUE or FALSE.
                                               #More sub choices for dummy functions
                                               conditionalPanel("input.VarCreateTop=='Dummy variables'",
                                                                style = "margin-left:19px;",
                                                                uiOutput('dummy_select'),
                                                                conditionalPanel("input.dummyfunc=='From variable'",
                                                                                 uiOutput('dummy_sub'))
                                               ),
                                               
                                               #More sub choices Spatial functions 
                                               
                                               ## runs assignment column
                                               conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='zone'",
                                                                uiOutput('zone_assign_1'),
                                                                uiOutput('zone_assign_2')
                                               ),
                                               
                                               conditionalPanel(condition = "input.VarCreateTop=='Spatial functions'&input.dist=='zone_cent'",
                                                                uiOutput('zone_cent_ui')
                                               ),
                                               
                                               conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='fish_cent'",
                                                                uiOutput('fish_weight_cent'), 
                                                                uiOutput('fish_weight_cent_2'), 
                                                                uiOutput('fish_weight_cent_3')
                                               ), 
                                               
                                               
                                               conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_dist_between'",
                                                                uiOutput('dist_between_input'),
                                                                uiOutput('dist_betwn_opts'),
                                                                tags$div(style = "margin-left:19px;", 
                                                                         selectInput('units', 'Distance unit',
                                                                                     choices = c('miles','meters','km')))
                                               ),
                                               
                                               conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&&input.dist=='create_mid_haul'",
                                                                style = "margin-left:19px;",
                                                                uiOutput('mid_haul_input')
                                               ),
                                               
                                               conditionalPanel("input.VarCreateTop=='Spatial functions'&&input.dist=='create_startingloc'",
                                                                
                                                                uiOutput('input_startingloc'),
                                               ),
                                               
                                               conditionalPanel("input.VarCreateTop=='Trip-level functions'&input.trip=='haul_to_trip'",
                                                                style = "margin-left:19px;",
                                                                selectInput('fun_numeric','Numeric function to transform data', 
                                                                            choices = c('min','mean','max','median','sum'), 
                                                                            selected = 'mean'),
                                                                selectInput('fun_time','Numeric function to transform temporal data',
                                                                            choices = c('min','mean','max','median'), 
                                                                            selected = 'mean'),
                                                                uiOutput('input_IDVAR')),
                                               
                                               conditionalPanel("input.VarCreateTop=='Trip-level functions'&&input.trip=='trip_distance'" ,
                                                                style = "margin-left:19px;",
                                                                uiOutput('input_trip_dist_vars')),
                                               
                                               conditionalPanel("input.VarCreateTop=='Trip-level functions'&&input.trip=='trip_centroid'",
                                                                style = "margin-left:19px;",
                                                                uiOutput('input_tri_cent')),
                                               
                                               actionButton('runNew',"Run function",class = "btn-secondary"),
                                               
                                               
                                               bslib::accordion(open = FALSE,
                                                                bslib::accordion_panel(
                                                                  "Other actions", icon = bsicons::bs_icon("menu-app"), 
                                                                  
                                                                  textInput('notesNew', "Notes", value=NULL, 
                                                                            placeholder = 'Write notes to store in text output file. Text can be inserted into report later.'),
                                                                  
                                                                  actionButton('callTextDownloadNew','Save notes', class = "btn-success"),
                                                                  
                                                                  tags$br(), tags$br(),
                                                                  
                                                                  textInput("exprN", label = "Enter an R expression",
                                                                            value = "values$dataset"),
                                                                  actionButton("runN", "Run", class = "btn-success"),
                                                                  div(style = "margin-top: 2em;",
                                                                      uiOutput('resultN')
                                                                      
                                                                  )
                                                                )
                                               ),
                                               actionButton("closeNew", "Close app", icon = icon("circle-xmark"),
                                                            width = "50%",                                                           
                                                            class = "btn-danger", 
                                                            onclick = "setTimeout(function(){window.close();},500);")
                                               ),
                       tags$br(),
                       
                       DT::DTOutput("output_table_create"),
                       downloadButton("exportData", "Download data")
                       
                       # TODO: Get the download button working                             
                       # selectInput("export_type", "Export data as:",
                       #             choices = c("csv", "txt", "rdata", "xlsx", 
                       #                         "json", "stata", "sas", "spss", "matlab")),
                       #)
                     )
    ),
    
    
    #---
    # Fleet functions ----
    #---
    bslib::nav_panel(title = "Fleet Assignment and Summary", value = "fleet",
                     bslib::navset_tab(id = 'select_fleet',
                                       bslib::nav_panel(title = "Fleet Assignment", value = "fleet_assignment",
                                                        bslib::page_sidebar(
                                                          sidebar = bslib::sidebar( width = 400,
                                                                                   saveDataTableUI("fleet"), # "Save data to FishSET database"
                                                                                   splitLayout(uiOutput("fleetSaveOutput1")), # "Save table..." and "Save plot..."
                                                                                   tags$br(), tags$br(),
                                                                                   
                                                                                   selectInput("assign_fun", label = "Select task",
                                                                                               choices = c("Define fleets", "Fleet assignment"),
                                                                                               selected = "Define fleets"),
                                                                                   
                                                                                   conditionalPanel("input.assign_fun == 'Define fleets'",
                                                                                                    fleet_tableUI("f_table")),
                                                                                   
                                                                                   conditionalPanel("input.assign_fun == 'Fleet assignment'",
                                                                                                    fleet_assignUI("f_assign")),
                                                                                   
                                                                                   bslib::accordion(open = FALSE,
                                                                                                    bslib::accordion_panel(
                                                                                                      "Other actions", icon = bsicons::bs_icon("menu-app"), 
                                                                                                      
                                                                                                      noteUI("fleet"),

                                                                                                      tags$br(), tags$br(),
                                                                                                      
                                                                                                      textInput("exprFleet", label = "Enter an R expression",
                                                                                                                value = "values$dataset"),
                                                                                                      actionButton("runFleet", "Run", class = "btn-success"),
                                                                                                      div(style = "margin-top: 2em;",
                                                                                                          uiOutput('resultFleet')
                                                                                                          
                                                                                                      )
                                                                                                    )
                                                                                   ),


                                                                                   closeAppUI("fleet")
                                                          ),
                                                          bslib::page_fillable(
                                                          
                                                          conditionalPanel("input.assign_fun == 'Define fleets'",
                                                                           fleet_exprUI("f_table"),
                                                                           fleet_tableOut("f_table")),
                                                          
                                                          conditionalPanel("input.assign_fun == 'Fleet assignment'",
                                                                           fleet_assignOut("f_assign"))
                                                        )
                                                        )
                                       ),
                                       
                                       bslib::nav_panel(title = "Fleet Summary", id = "fleet_summary",
                                                        bslib::page_sidebar(
                                                          sidebar = bslib::sidebar( width = 400,
                                                                                   saveDataTableUI("fleet_summary"), # "Save data to FishSET database"
                                                                                   splitLayout(uiOutput("fleetSaveOutput2")), # "Save table..." and "Save plot..."
                                                                                   tags$br(),
                                                                                   
                                                                                   tags$br(),
                                                                                   
                                                                                   selectInput("fleet_fun", "Select function",
                                                                                               choices = c("vessel count" = "vessel_count", "species catch" = "species_catch",
                                                                                                           "rolling catch" = "roll_catch", "weekly catch" = "weekly_catch",
                                                                                                           "weekly effort" = "weekly_effort", "bycatch", "trip duration" = "trip_dur_out",
                                                                                                           "density plot" = "density_plot"), 
                                                                                               multiple = FALSE,
                                                                                               selected = "vessel_count"),
                                                                                   tags$br(),
                                                                                   
                                                                                   conditionalPanel("input.fleet_fun == 'vessel_count'",
                                                                                                    vessel_countUI("ves")),
                                                                                   
                                                                                   conditionalPanel("input.fleet_fun == 'species_catch'",
                                                                                                    species_catchUI("spec")),
                                                                                   
                                                                                   conditionalPanel("input.fleet_fun == 'roll_catch'",
                                                                                                    roll_catchUI("roll")),
                                                                                   
                                                                                   conditionalPanel("input.fleet_fun == 'weekly_catch'",
                                                                                                    weekly_catchUI("wc")),
                                                                                   
                                                                                   conditionalPanel("input.fleet_fun == 'weekly_effort'",
                                                                                                    weekly_effortUI("we")),
                                                                                   
                                                                                   conditionalPanel("input.fleet_fun == 'bycatch'",
                                                                                                    bycatchUI("by")),
                                                                                   
                                                                                   conditionalPanel("input.fleet_fun == 'trip_dur_out'",
                                                                                                    trip_durUI("trip")),
                                                                                   
                                                                                   conditionalPanel("input.fleet_fun == 'density_plot'",
                                                                                                    density_plotUI("den")),
                                                                                   
                                                                                   uiOutput("run_fleet_fun"), # run function
                                                                                   
                                                                                   bslib::accordion(open = FALSE,
                                                                                                    bslib::accordion_panel(
                                                                                                      "Other actions", icon = bsicons::bs_icon("menu-app"),
                                                                                                      
                                                                                                      noteUI("fleet_summary"),
                                                                                                      
                                                                                                      tags$br(), tags$br(),
                                                                                                      
                                                                                                      textInput("exprFleetSummary", label = "Enter an R expression",
                                                                                                                value = "values$dataset"),
                                                                                                      actionButton("runFleetSummary", "Run", class = "btn-success"),
                                                                                                      div(style = "margin-top: 2em;",
                                                                                                          uiOutput('resultFleetSummary')
                                                                                                      )
                                                                                                    )  
                                                                              ),
                                                              closeAppUI("fleet_summary"),
                                                          ),
                                                          
                                                          conditionalPanel("input.fleet_fun == 'vessel_count'",
                                                                           h2("Vessel count"),
                                                                           fleetOut("ves")),
                                                          
                                                          conditionalPanel("input.fleet_fun == 'species_catch'",
                                                                           fleetOut("spec")),
                                                          
                                                          conditionalPanel("input.fleet_fun == 'roll_catch'",
                                                                           fleetOut("roll")),
                                                          
                                                          conditionalPanel("input.fleet_fun == 'weekly_catch'",
                                                                           fleetOut("wc")),
                                                          
                                                          conditionalPanel("input.fleet_fun == 'weekly_effort'",
                                                                           fleetOut("we")),
                                                          
                                                          conditionalPanel("input.fleet_fun == 'bycatch'",
                                                                           fleetOut("by")),
                                                          
                                                          conditionalPanel("input.fleet_fun == 'trip_dur_out'",
                                                                           fleetOut("trip")),
                                                          
                                                          conditionalPanel("input.fleet_fun == 'density_plot'",
                                                                           fleetOut("den"))
                                                          
                                                        )
                                       )
                     )
    ),
    
    
    #---
    # Alternative Choice tabset panel ----
    #---
    bslib::nav_panel(title = 'Define Alternative Fishing Choices', id = "altc",
                     bslib::page_sidebar(
                       sidebar = bslib::sidebar(  width = 400,
                                                 uiOutput("disableMsg1"),
                                                 
                                                 actionButton("save_final_modal", "Save final table to FishSET database",
                                                              class= "btn-primary"),
                                                 
                                                 actionButton('altc_save','Save choices', class= "btn-primary"), 


                                                 bslib::accordion(open = FALSE,
                                                                  bslib::accordion_panel(
                                                                    "Other actions", icon = bsicons::bs_icon("menu-app"), 
                                                 
                                                   textInput('notesAltc', "Notes", value = NULL, 
                                                             placeholder = 'Write notes to store in text output file. Text can be inserted into report later.'),
                                                   
                                                   actionButton('callTextDownloadAlt','Save notes',class = "btn-success"),
                                                   
                                                   tags$br(), tags$br(),
                                                   
                                                   # Inline scripting 
                                                   textInput("exprZ", label = "Enter an R expression",
                                                             value = "values$dataset"),
                                                   actionButton("runZ", "Run", class = "btn-success"),
                                                   div(style = "margin-top: 2em;",
                                                       uiOutput('resultZ')
                                                   )
                                                  )
                                             ),
                                             
                                             actionButton("altc_close", "Close app", icon = icon("circle-xmark"),
                                                          width = "50%",                                                           
                                                          class = "btn-danger", 
                                                          onclick = "setTimeout(function(){window.close();},500);")
                       ),
                       
                       uiOutput('altc_ui'),
                       
                       div(style="display: inline-block;vertical-align:top; width: 500px;",
                           
                           plotOutput('altc_zone_plot')),
                       
                       div(style="display: inline-block;vertical-align:top; width: 160px;",
                           
                           textOutput('zoneIDText')),
                       
                       plotOutput('zone_include_plot')
                     )
    ),
    
    
    #---
    # Expected catch tabset panel ----
    #---
    bslib::nav_panel(title ="Expected Catch/Revenue", id = "expectedCatch",
                     bslib::navset_tab(id = 'exp_tab', 
                                       bslib::nav_panel(title = 'Create expected catch', id = 'exp_create', 
                                                        bslib::page_sidebar(
                                                          sidebar = bslib::sidebar( width = 400,
                                                                                   
                                                                                   tags$br(),
                                                                                   
                                                                                   uiOutput('exp_ui'),
                                                                                   
                                                                                   add_prompter(div(
                                                                                     div(style="display:inline-block; width: 200px", 
                                                                                         h4('Temporal options')), 
                                                                                     div(style="display:inline-block; margin-left:-10px", icon('info-circle', verify_fa = FALSE))),
                                                                                     position = "above", type='info', size='large', 
                                                                                     message = 'Use the entire temporal record of catch or take the timeline of catch into account. 
                                                                                                When timeline is considered, catch for a given day is the average for the defined number of days (window), 
                                                                                                shifted to the past by the defined number of days (lag). For example, a window of 3 days and lag of 1 day means we take the 
                                                                                                average catch over three days starting one day prior to the given date.'),
                                  
                                  div(style = "margin-left:19px;font-size: 12px", 
                                      selectInput('exp_temporal', 'Method to sort time:', 
                                                  choices = c('Daily timeline'='daily', 
                                                              'Sequential order'='sequential'),
                                                  selected = 'daily')),
                                  
                                  uiOutput('exp_temp_var_ui'),
                                  
                                  conditionalPanel("input.exp_temporal!='Entire record of catch (no time)'",
                                                   style = "margin-left:19px;font-size: 12px",
                                                   
                                                   numericInput('exp_temp_year', 'No. of years to go back if expected catch based on previous year(s) catch ', 
                                                                value=0, min=0, max=''),
                                                   
                                                   numericInput('exp_temp_window', 'Window size (days) to average over', value = 7, min=0),
                                                   
                                                   numericInput('exp_temp_lag', 'Time lag (in days) ', value = 0, min=0, max=''),
                                                   
                                                   selectInput('exp_calc_method','Expectation calculation:', 
                                                               choices = c("Standard average"="standardAverage", 
                                                                           "Simple lag regression of means"="simpleLag")
                                                   )), #"Weights of regressed groups"="weights"
                                  
                                  conditionalPanel("input.exp_temporal!='Entire record of catch (no time)'", 
                                                   selectInput('exp_lag_method', 'Method to average across time steps', 
                                                               choices= c("Entire time period"="simple", "Grouped time periods"="grouped")),
                                                   
                                                   h4('Averaging options')),
                                  
                                  conditionalPanel("input.exp_temporal!='Entire record of catch (no time)'", 
                                                   div(style = "margin-left:19px; font-size: 12px", 
                                                       selectInput('exp_empty_catch', 'Replace empty catch with:', 
                                                                   choices = c("NA: NA's removed when averaging"='NA', '0', 
                                                                               'Mean of all catch' ="allCatch", 
                                                                               'Mean of grouped catch' = "groupedCatch"))),
                                                   add_prompter(div(
                                                     div(style = "display:inline-block; width: 200px; margin-left:19px",
                                                         checkboxInput('weight_avg', 'Weighted average', value = FALSE)),
                                                     div(style="display:inline-block; margin-left:-45px", icon('info-circle', verify_fa = FALSE))),
                                                     position = "right", type='info', size='large', 
                                                     message = 'Select to give more weight to days with multiple observations for a given zone. 
                                                     See user manual for more detail.'),
                                  ),
                                  
                                  h4('Expected Catch/Dummy options'), 
                                  
                                  div(style = "margin-left:19px; font-size: 12px",
                                      selectInput('empty_expectation', 'Replace empty expected catch with:', 
                                                  choices = c(1e-04, 0))),  
                                  
                                  div(style = "margin-left:19px; font-size: 14px",
                                      checkboxInput('exp_dummy', 'Output dummy variable for originally missing values?', 
                                                    value = FALSE)),
                                  
                                  checkboxInput('exp_replace_output', 'Replace previously saved expected catch output with new output', 
                                                value = FALSE),
                                  
                                  actionButton("exp_submit", "Run expected catch/revenue function", 
                                               class = "btn-secondary"), 
                                  
                                  ##Inline scripting 
                                  bslib::accordion(open = FALSE,
                                                   bslib::accordion_panel(
                                                     "Other actions", icon = bsicons::bs_icon("menu-app"), 
                                                     textInput('notesEC', "Notes", value=NULL, 
                                                               placeholder = 'Write notes to store in text output file. Text can be inserted into report later.'),
                                                     
                                                     actionButton('callTextDownloadEC','Save notes',class = "btn-success"),
                                                     
                                                     tags$br(),tags$br(),
                                                     
                                                      textInput("exprEC", label = "Enter an R expression",
                                                                value = "values$dataset"),
                                                      actionButton("runEC", "Run", class = "btn-success"),
                                                      div(style = "margin-top: 2em;",
                                                          uiOutput('resultEC'))
                                                      )
                                      ),
                                        
                                  actionButton("closeEC", "Close app", icon = icon("circle-xmark"),
                                               width = "50%",                                                           
                                               class = "btn-danger", 
                                               onclick = "setTimeout(function(){window.close();},500);")
                                                          ),

                                  
                                  tags$br(),tags$br(),
                                  h4(tags$b('Compute expected catch for each observation and zone')),
                                  
                                  uiOutput('exp_altc_check'),
                                  
                                  tags$p(
                                    tags$br(), 
                                    'This tab creates the expected catch or expected revenue data frame based on selected parameters.',
                                    'Output saved in FishSET database. Previously saved expected catch/revenue output will be written over if the', 
                                    tags$i('Replace previously saved'), 'box is checked. Leaving the box unchecked will add new output to existing output.',
                                    
                                    tags$br(), tags$br(),
                                    
                                    h4(tags$b('Default Matrices')), tags$br(),
                                    'There are four optional matrix settings:', 
                                    tags$br(), tags$br(),
                                    tags$b('Recent expected catch:'), 'Expected catch/revenue based on catch of the previous two days (two day window, no lag).', 
                                    'In this case, there is no grouping, and catch for entire fleet is used.',
                                    tags$br(), tags$br(), 
                                    tags$b('Older expected catch:'), 'Expected catch/revenue based on catch for the previous seven days', 
                                    ' (seven day window) starting two days previously (two day lag). Vessels are grouped (or not) based on group widget.',
                                    tags$br(), tags$br(),
                                    tags$b('Oldest expected catch:'), 'Expected catch/revenue based on catch for the previous seven days',
                                    'starting eight days previously (eight day lag). Vessels are grouped (or not) based on group widget.',
                                    tags$br(), tags$br(),
                                    tags$b('Logbook expected catch:'), 'Expected catch/revenue based on catch in the previous 14 days', 
                                    '(14 day window) starting one year and seven days previously. Can only be used if a group variable is provided.',
                                  ),
                                  
                                  checkboxGroupInput('exp_default', 'Select default matrices to include',
                                                     choices = c('Recent' = 'recent', 'Older' = 'older', 
                                                                 'Oldest' = 'oldest', 'Logbook' = 'logbook')),
                                  
                                  tags$br(), tags$br(),
                                  
                                  conditionalPanel("input.exp_temp_var!='none'",
                                                   tagList(
                                                     h4(tags$b('Sparsity of observations by time period and zone')),
                                                     h5('Higher values indicate greater sparsity.'),
                                                     DT::DTOutput('spars_table'),
                                                     shinycssloaders::withSpinner(plotOutput('spars_plot'), type = 6)
                                                   ))
                                                        )
                                  
                                       ),
                                                                              
                                  bslib::nav_panel(title ='Merge expected catch', id = 'exp_merge',
                                                   bslib::page_sidebar(
                                                     sidebar = bslib::sidebar( width = 400,
                                                                               actionButton('exp_merge_reload', 'Refresh expected catch list', 
                                                                                            class = "btn-primary"),
                                                                               
                                                                               tags$br(),
                                                                               
                                                                               p(strong('Merge an expected catch matrix to the primary data')),
                                                                               
                                                                               uiOutput('exp_merge_ui'),
                                                                               
                                                                               actionButton('exp_merge_run', 'Merge expected catch', 
                                                                                            class = "btn-secondary"),
                                                     ),
                                                     
                                                     fluidRow(
                                                       tags$br(),
                                                       column(12,
                                                              DT::dataTableOutput('exp_merge_tab')))
                                                   )
                                  )
                     )
    ),
    
    
    #---
    # Model design and Run tabset panel ----
    #---
    bslib::nav_panel(title = "Models", id = "models",
                     bslib::navset_tab(id = 'mod_sub',
                                       bslib::nav_panel(title="Run model(s)", id = 'model_run',
                                                        bslib::page_sidebar(
                                                          sidebar = bslib::sidebar( width = 400,
                                                                                   # Models can't be run if final dataset not detected
                                                                                   uiOutput("disableMsg"),

                                                                                   # tags$br(),
                                                                                   
                                                                                   actionButton("mod_check", "Run model checks",
                                                                                                class = "btn-primary"), 
                                                                                   
                                                                                   uiOutput('mod_add_run_bttn'),
                                                                                   
                                                                                   tags$br(),tags$br(),
                                                                                   
                                                                                   tags$p(tags$strong("More information"), tags$br(),
                                                                                          "Model parameter table is editable. Double click a cell to edit."),
                                                                                   
                                                                                   bslib::accordion(open = FALSE,
                                                                                                    bslib::accordion_panel(
                                                                                                      "Other actions", icon = bsicons::bs_icon("menu-app"),
                                                                                                      
                                                                                         textInput('notesModel', "Notes", value=NULL, 
                                                                                                   placeholder = 'Write notes to store in text output file. Text can be inserted into report later.'),
                                                                                         
                                                                                         actionButton('callTextDownloadModels','Save notes', class = "btn-success"),
                                                                                         
                                                                                         tags$br(), tags$br(),
                                                                                         
                                                                                         
                                                                                         ##Inline scripting 
                                                                                         textInput("exprM", label = "Enter an R expression",
                                                                                                   value = "values$dataset"),
                                                                                         actionButton("runM", "Run", class = "btn-success"),
                                                                                         div(style = "margin-top: 2em;", uiOutput('resultM')),
                                                                                                    )
                                                                                   ),
                                                                                   actionButton("closeModel", "Close app", icon = icon("circle-xmark"),
                                                                                                width = "50%",                                                           
                                                                                                class = "btn-danger", 
                                                                                                onclick = "setTimeout(function(){window.close();},500);")
                                                          ),
                                                                                
                                                          
                                                          
                                                          div(id = "form",
                                                              
                                                              h4('Likelihood function'),
                                                              
                                                              selectInput("model", label = "",
                                                                          choices = list("Conditional logit" = 'logit_c', 
                                                                                         "Zonal logit" = "logit_zonal", 
                                                                                         "Logit Dahl correction" = "logit_correction",
                                                                                         'EPM normal'='epm_normal', 
                                                                                         'EPM lognormal'='epm_lognormal', 
                                                                                         'EPM Weibull'='epm_weibull'),
                                                                          selected = 'logit_c'),
                                                              
                                                              uiOutput('mod_name_ui'), 
                                                              
                                                              selectInput('mod_optmeth', 'Optimization method', 
                                                                          choices=c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN"), 
                                                                          selected='BFGS'),
                                                              
                                                              h4('Select variables to include in model'),
                                                              
                                                              div(style="display: inline-block;vertical-align:top; width: 250px;", 
                                                                  uiOutput('mod_ind_var_ui')),
                                                              
                                                              div(style="display: inline-block;vertical-align:top; width: 250px;", 
                                                                  uiOutput('mod_grid_var_ui')),
                                                              
                                                              uiOutput('mod_catch_out'),
                                                              uiOutput('mod_logit_correction'),
                                                              
                                                              conditionalPanel(condition="input.model=='logit_c'",
                                                                               
                                                                               add_prompter(
                                                                                 
                                                                                 h4(list('Select Expected Catch Matrix'), icon('info-circle', verify_fa = FALSE)),
                                                                                 position = "bottom", type='info', size='medium',
                                                                                 message = 'Required for conditional logit. 
                                                      A model will be run for each additional entry.
                                                      Do not leave any entries blank. Click "Reset"
                                                      button to clear the list.
                                                      '
                                                                               ),
                                                      
                                                      tags$br(),
                                                      
                                                      actionButton('mod_add_exp', 'Add expected catch entry',
                                                                   class = "btn-secondary"),
                                                      
                                                      actionButton('mod_add_exp_reset', 'Reset',
                                                                   class = "btn-secondary"),
                                                      
                                                      uiOutput('mod_select_exp_ui'))
                                                    
                                                              ),
                                                      
                                                      uiOutput('mod_spat_ui'),
                                                      
                                                      add_prompter(
                                                        
                                                        textInput('mod_spat_crs', label = list('EPSG code', icon('info-circle', verify_fa = FALSE)),
                                                                  value = '', placeholder = 'e.g. 4326'),
                                                        position = "bottom", type='info', size='medium', 
                                                        message = 'The EPSG code that determines the coordinate 
                                     reference system (CRS) to use when creating the distance 
                                     matrix. Defaults to WGS 84 (EPSG 4326).'
                                                      ),
                                     
                                     h3('Model parameters'),
                                     
                                     fluidRow(
                                       h4("Optimization options"),
                                       splitLayout(cellWidths = c("22%", "22%", "22%", "22%"),
                                                   numericInput("mod_iter", "max iterations", value = 100),
                                                   numericInput("mod_relTolX", "tolerance of x", value = 0.00000001),
                                                   numericInput("mod_report_freq", "report frequency", value = 1),
                                                   numericInput("mod_detail_report", "detailed report", value = 1, max=6)
                                       )
                                     ),
                                     
                                     fluidRow(
                                       add_prompter(div(
                                         div(style="display: inline-block; width: 145px ;", h4('Initial parameters')), 
                                         div(style="display: inline-block; width: 5px ;", icon('info-circle', verify_fa = FALSE))),
                                         position = "bottom", type='info', size='large', 
                                         message = tags$p("Starting values can be changed. 
                                        The function will explore the parameter space to find better starting values. 
                                        The order of starting parameter values differ between likelihood functions.
                                        Conditional logit and zonal logit: alternative-specific parameters (betas),  travel-distance parameters (gammas)
                                        Logit correction:  marginal utility from catch, catch-function parameters, polynomial starting parameters, travel-distance parameters, catch sigma
                                        EPM likelihood functions:  catch-function parameters, travel-distance parameters, catch sigma(s), scale parameter 
                                        See Likelihood section of Help Manual for more details.")),
                                       
                                       uiOutput('mod_param_choose'),
                                       
                                       conditionalPanel(condition = "input.initchoice=='prev'",
                                                        uiOutput("mod_param_table_ui")),
                                       
                                       conditionalPanel(condition = "input.model == 'epm_weibull'",
                                                        checkboxInput("alt_spec_epm1", label = "Set alternative-specific shape parameters")),
                                       
                                       conditionalPanel(condition = "input.model == 'epm_lognormal' || input.model == 'epm_normal'",
                                                        checkboxInput("alt_spec_epm2", label = "Set alternative-specific standard deviation")),
                                       
                                       fluidRow(
                                         column(6, DT::dataTableOutput("param_tab"))
                                       )
                                     ),
                                     
                                     DT::DTOutput('mod_param_table')
                                     
                                                          )
                                     #)
                                     ),
                                     
                                     
                                     bslib::nav_panel(title = "Compare models", id = 'model_compare',
                                                      bslib::page_sidebar(
                                                        sidebar = bslib::sidebar(  width = 400,
                                                                                  actionButton("mod_reload", "Reload model output",  class = "btn-primary"),
                                                                                  actionButton("mod_compare_delete", "Delete row", class = "btn-primary"),
                                                                                #  h3(''),
                                                                                  actionButton("mod_save_table", "Save table", class = "btn-primary"),
                                                                                  # tags$br(),tags$br(),
                                                                                  # tags$button(
                                                                                  #   id = 'closeCM',
                                                                                  #   type = "button",
                                                                                  #   style="color: #fff; background-color: #FF6347; border-color: #800000;",
                                                                                  #   class = "btn action-button",
                                                                                  #   onclick = "setTimeout(function(){window.close();},500);",  # close browser
                                                                                  #   "Close window"
                                                                                  # ) 
                                                        ),
                                                        fluidRow(
                                                          column(12,
                                                                 h4('Model Output'),
                                                                 DT::dataTableOutput('mod_model_tab'),
                                                                 
                                                                 tags$br(),
                                                                 h4("Measures of fit"),
                                                                 DT::DTOutput("mod_fit_out"),
                                                                 tags$script(HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
                                                                  Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());})")),
                                                                 
                                                                 tags$br(),
                                                                 h4('Error messages'),
                                                                 DT::DTOutput('mod_error_msg')
                                                          )
                                                        )
                                                      )),
                                     
                                     bslib::nav_panel(title = 'Manage models', id = 'model_man',
                                                      bslib::page_sidebar(
                                                        sidebar = bslib::sidebar(  width = 400,
                                                                                  actionButton("mod_reload", "Reload model output",
                                                                                               class = "btn-primary"),
                                                                                  
                                                                                  actionButton('mod_delete', 'Delete model(s)',
                                                                                               class = "btn-primary")
                                                                                  
                                                        ),
                                                        
                                                        
                                                        fluidRow(
                                                          column(6, 
                                                                 div('Note: deleting a parent model will delete its nested models
                                            and output', style = 'background-color: yellow;
                                            border-style: solid; border-color: black; border-width: thin;')
                                                          )
                                                        ),
                                            
                                            selectInput('mod_man_mod_type', 'Select model type', 
                                                        choices = c('parent model', 'nested model')),
                                            
                                            uiOutput('mod_man_ui')
                                            
                                                      )
                                            #  )
                                     ),
                                     bslib::nav_panel(title = 'View model list', id = 'model_list',
                                                      bslib::page_sidebar(
                                                        sidebar = bslib::sidebar(  width = 400,
                                                                                  actionButton('mod_list_reload', 'Reload model list',
                                                                                               class = "btn-primary"),
                                                                                  
                                                        ),
                                                        
                                                        
                                                        verbatimTextOutput('mod_list_ui')
                                                      )
                                     ),
                                     bslib::nav_panel(title='Cross-validation', id = 'cross_validation',
                                                      bslib::page_sidebar(
                                                        sidebar = bslib::sidebar(  width = 400,
                                                                                  actionButton('run_cv', "Run cross-validation",
                                                                                               class = "btn-primary"),
                                                                
                                                                                  actionButton('reload_cv', "Reload output",
                                                                                               class = "btn-primary"),
                                                                                  
                                                                                  tags$br(),
                                                                                  tags$br(),
                                                                                  
                                                                                  uiOutput('cv_ui'),
                                                                                  
                                                        ),
                                                        
                                                        h4('K-fold Cross Validation Prediction Performance'),
                                                        DT::DTOutput('cv_perf_tab', width = "75%"),
                                                        
                                                        tags$br(),
                                                        
                                                        h4('Model fits for each iteration'),
                                                        DT::DTOutput('cv_modfit_tab', width = "100%"),
                                                        
                                                        tags$br(),
                                                        
                                                        h4('Model estimates for each iteration'),
                                                        DT::DTOutput('cv_modout_tab', width = "75%")
                                                        
                                                      ),
                                     ),
                                     bslib::nav_panel(title = 'Out-of-sample prediction', id = 'outsample_predict',
                                                      bslib::page_sidebar(
                                                        sidebar = bslib::sidebar(  width = 400,
                                                                                  h5(strong('Step 1: Upload out-of-sample data')),
                                                                                  uiOutput("load_outsample"),
                                                                                  
                                                                                  tags$div(style = "margin-top: -30px;",
                                                                                           h5(strong('Step 2: Filter out-of-sample data'))),
                                                                                  uiOutput("filter_outsample"),
                                                                                  
                                                                                  tags$br(),
                                                                                  
                                                                                  h5(strong('Step 3: Make model design')),
                                                                                  uiOutput("mod_design_outsample"),
                                                                                  
                                                                                  tags$br(),
                                                                                  
                                                                                  h5(strong('Step 4: Out-of-sample prediction')),
                                                                                  p("Important: check settings in previous steps before running prediction."),
                                                                                  uiOutput("run_outsample_prediction"),   
                                                        ),
                                                        
                                                        uiOutput("outsample_pred_err"),
                                                        
                                                        tags$br(),
                                                        
                                                        h4('Predicted out-of-sample fishing probabilities'),
                                                        DT::DTOutput('outsample_preds', width = "75%"),
                                                        
                                                        tags$br(),
                                                        
                                                        h4("Map of predicted out-of-sample fishing probabilities"),
                                                        plotOutput("map_outsample")
                                                      )
                                     ),
                                     
                     )),
    
    
    #---
    # Policy tab ----
    #---
    bslib::nav_menu(title ='Policy', 
                    bslib::nav_panel(title = "Zone Closure", id = "zone_closures",
                                     bslib::page_sidebar(
                                       sidebar = bslib::sidebar(   width = 400,
                                         "Click on one or more zones to select closed zones.",
                                         "\nPress the 'Add closure' button to record choices.",
                                         "Repeat to add another closure.",
                                         "When done, press the 'Save closures' button.",
                                         zone_closure_sidebarUI("policy")),
                                       bslib::page_fluid(
                                         zone_closure_mapUI("policy"),
                                         zone_closure_tableUI("policy")
                                       )
                                      )
                    
    ),
                    bslib::nav_panel(title = "Run Policy", id = "run_policy",
                                     bslib::page_sidebar(
                                       sidebar = bslib::sidebar(  width = 400,
                                       bslib::accordion(
                                            bslib::accordion_panel(
                                              "To run policy scenarios", icon = bsicons::bs_icon("sliders"),
                                              run_policyUI("run_policy")),
  
                                            bslib::accordion_panel(
                                              "To create predicted probabilities map", icon = bsicons::bs_icon("map"),
                                              predict_map_sidebarUI("run_policy")
                                            ),
                                            bslib::accordion_panel(
                                              "Other actions", icon = bsicons::bs_icon("menu-app"),
                                              
                                            textInput('notesPolicy', "Notes", value=NULL,
                                                      placeholder = 'Write notes to store in text output file. Text can be inserted into report later.'),

                                            actionButton('callTextDownloadPolicy','Save notes', class = "btn-success"),

                                            tags$br(), tags$br(),
                                            
                                            ##Inline scripting 
                                            textInput("exprP", label = "Enter an R expression",
                                                      value = "values$dataset"),
                                            actionButton("runP", "Run", class = "btn-success"),
                                            div(style = "margin-top: 2em;", uiOutput('resultP')),
                                            
                                            
                                            )
                                 ),
                                 actionButton("closePolicy", "Close app", icon = icon("circle-xmark"),
                                              width = "50%",                                                           
                                              class = "btn-danger", 
                                              onclick = "setTimeout(function(){window.close();},500);")
                                       
                                       ),
                                       bslib::page_fillable(
                                         bslib::accordion(
                                           pred_plotsUI("run_policy"),
                                           predict_map_mainUI("run_policy"),
                                           welfare_outputsUI("run_policy")
                                     )
                             )
                         )
                      )                
                    ),
    
    
    #--- 
    # Bookmark tabset panel ----
    #---
    bslib::nav_panel(title ='Bookmark Choices', id = "book",
                     bslib::navset_tab(id = "boomark_tab", 
                                       bslib::nav_panel(title ="Bookmark", id = "bookmark_page",
                                                        
                                                        tags$br(),
                                                        tags$br(),tags$br(),
                                                        h4('Bookmark selected choices'),
                                                        bookmarkButton(width = "25%"),
                                                        tags$br(),
                                                        textInput('notesBook', "Notes", value=NULL, placeholder = 'Paste bookmarked URL here.'),
                                                        actionButton('callTextDownloadBook','Save notes', width = "25%", class = "btn-primary"),
                                                        downloadLink("downloadTextBook", label=''),
                                                        tags$br(),tags$br(),
                                                        h4('Upload previously saved bookmarked state (.rds file)'),
                                                        fileInput('uploadbookmark', paste0('File should be located in ', system.file(package='FishSET'),'/ShinyFiles/MainApp/shiny_bookmarks folder'), 
                                                                  multiple=FALSE, accept='.rds'),
                                                        #h4(paste0("File should be a .rds file and would be located in a shiny_bookmarks folder under ",  system.file(package='FishSET'), " directory")),
                                                        #textInput('notesBook', "Notes", value=NULL, 
                                                        #          placeholder = 'Write notes to store in text output file. Text can be inserted into report later.'),
                                                        tags$br(),
                                                        tags$p('This application can be reopened at a saved state by following the following steps:'),
                                                        tags$ul('Saving current state:'),
                                                        tags$ul(tags$ul(tags$li('Click bookmark button and close the pop-up window.'))),
                                                        tags$ul('Reopen saved state:'),
                                                        tags$ul(tags$ul(tags$li('Reopen the app'))),
                                                        tags$ul(tags$ul(tags$li('Return to the bookmark tab'))),
                                                        tags$ul(tags$ul(tags$li('Browse to the saved bookmark file (ends in .rds)'))),
                                                        tags$ul(tags$ul(tags$li('Double click on input.rds'))),
                                                        tags$ul(tags$ul(tags$li('Reload the data in the Upload Data tab.'))),
                                                        tags$ul(tags$ul(tags$li('Selections and inputs will be reloaded but function calls will not be
                                                   run. Go to the', tags$em('log rerun'),
                                                   'subtab to rerun logged function calls.'))),
                                    actionButton("closeB", "Close app", icon = icon("circle-xmark"),
                                                 width = "25%",                                                           
                                                 class = "btn-danger", 
                                                 onclick = "setTimeout(function(){window.close();},500);"),
                                       ),
                                       bslib::nav_panel(title = "Rerun logged function calls", id = "log_rerun",
                                                        bslib::page_sidebar(
                                                          sidebar = bslib::sidebar(  width = 400,
                                                                                    
                                                                                    actionButton("run_log", "Rerun log",
                                                                                                 class = "btn-secondary"),
                                                                                    
                                                                                    
                                                                                    uiOutput('new_dat_cb_choices'),
                                                                                    
                                                                                    
                                                                                    p("Click on the table rows to run specific function calls."),
                                                                                    actionButton("closeRerun", "Close app", icon = icon("circle-xmark"),
                                                                                                 width = "50%",                                                           
                                                                                                 class = "btn-danger", 
                                                                                                 onclick = "setTimeout(function(){window.close();},500);")
                                                            
                                                          ),
                                                          
                                                          bslib::page_fillable(
                                                          shinycssloaders::withSpinner(DT::DTOutput("log_table"), type = 6)
                                                          )
                                                        )
                                       )
                     )
    )
  )
  
  
  # )
}
