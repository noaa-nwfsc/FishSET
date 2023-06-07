source("fleetUI.R", local = TRUE)
source("fleet_helpers.R", local = TRUE)
source("map_viewer_app.R", local = TRUE)

   ## USER INTERFACE    
    ui = function(request){
      fluidPage(
      shinyjs::useShinyjs(),
      use_prompter(),
      # Pop up information icons
      tags$style(".fa-info-circle {color:#0066FF}"),
      tags$style(".fa-exclamation-circle {color:#FF0066}"),
      
      #--
      # Formatting ----
      #--
      tags$head(tags$style(HTML("
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
                              }
                              .shiny-notification {
                                position:relative;
                                top: calc(50% - 150px);
                                left: calc(50% - 150px);
                                width: 500px;
                              }
                               "))
                ), #  position:fixed;
      
      #---

      tabsetPanel(id = "tabs",
      # Landing page ----
                  
                  tabPanel("Information",
                           tabsetPanel(
                             tabPanel("Background",
                                      fluidRow(
                                        column(width = 10,
                                               tags$div(tags$p(tags$br(), tags$h2('FishSET - Spatial Economic Toolbox for Fisheries', 
                                                                                  style="color:darkblue; text-align:center; font-size: 32px;")),
                                                        tags$div(style="display: inline-block; align:right", img(src="SandPoint_Boats.JPG", height='10%', width='100%', align='center')),
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
                                                        tags$p('For questions and comments please contact: FishSET@noaa.gov'))
                                        )
                                      )
                             ),
                             tabPanel('Quickstart guide',

                                        tags$br(),
                                      tags$p('Select a radio button for a brief guide for each tab.'),
                                      tags$div(
                                      radioButtons('QuickStartChoices',"", choices = c('Information across all tabs'='AcrossTabs',
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
                                                                                       ), selected='AcrossTabs', inline = TRUE), style="font-size:115%; font-weight:bold;"
                                      ),
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
                             ),
                             tabPanel("Alaska Details",
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
                             tabPanel("FishSET Background",
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
                           )
                          # )
                           ),
                  
                  #---
      # Upload data tabset panel ---- 
                  #---
                  tabPanel("Upload Data", value = "upload",
                           tags$style(type='text/css', "#uploadMain { width:100%; margin-top: 24px;margin-left:-20px;padding-left:2px; padding-right:5px}"),
                           
                           mainPanel(
                             #div(style="display:inline-block;vertical-align:bottom;",
                             tags$button(
                               id = 'closeDat',
                               type = "button",
                               style="color: #fff; background-color: #FF6347; border-color: #800000;",
                               class = "btn action-button",
                               onclick = "setTimeout(function(){window.close();},500);",  # close browser
                               "Close app"
                             ),
                             
                             tags$button(
                               id = 'loadDat',
                               type = "button",
                               style = "color: white; background-color: blue;",
                               class = "btn action-button",
                               "Load data"),
                             
                             actionButton("delete_tabs_bttn", "Manage Tables",
                                          style = "color: white; background-color: blue;"),
                             
                             actionButton("meta_modal", "Metadata",
                                          style = "color: white; background-color: blue;"),
                             
                             conditionalPanel("input.loadDat > 0", # update to a more reliable method
                                actionButton("confid_modal", "Confidentiality",
                                             style = "color: white; background-color: blue;"),
                                actionButton("reset_modal", "Reset log",
                                             style = "color: white; background-color: blue;"),
                                actionButton("plot_set", "Plot settings", 
                                             style = "color: white; background-color: blue;")),
                             
                             tags$br(), tags$br(),
                             fluidRow(
                               
                               actionButton('change_fs_folder', 'Change FishSET Folder',
                                            style = "color: white; background-color: blue;"), 
                               uiOutput('fish_folder_path'), 
                              
                                uiOutput("projects"), # define project name
                                
                               column(4, 
                                      radioButtons('loadmainsource', "Source primary data from:",
                                                   choices=c('Upload new file','FishSET database'), 
                                                   selected='Upload new file', inline=TRUE)
                                      # uiOutput('main1')
                                      ),
                               uiOutput('main_upload')),
                           
                             fluidRow( 
                               column(width = 8, offset = 2,
                                      uiOutput('ui.action2'))),
                             fluidRow(
                               column(4, radioButtons('loadportsource', "Source port data from:", 
                                                      choices=c('Upload new file','FishSET database'), 
                                                       inline=TRUE)),
                               uiOutput('port_upload')
                             ),
                             fluidRow(
                               column(width=8, offset=2,
                                      uiOutput('ui.actionP2'))
                             ),
                             # combining port tables
                             uiOutput("portAddTable"),
                             
                             uiOutput("PortAddtableMerge"),
                             
                             fluidRow(
                               column(4, radioButtons('loadspatialsource', "Source spatial data from:", 
                                                      choices=c('Upload new file', 'FishSET database'), 
                                                      selected='Upload new file', inline=TRUE)),
                               radioButtons('filefolder', "", choices=c("Upload single file", "Upload shape files"), 
                                            selected="Upload single file", inline = TRUE),
                               uiOutput('spatial_upload')
                             ),
                             
                             fluidRow(
                                 column(4, radioButtons('loadgridsource', "Source gridded data from:", 
                                                        choices=c('Upload new file','FishSET database'), 
                                                        selected='Upload new file', inline=TRUE)),
                                 uiOutput('grid_upload')
                               ),
                             
                             uiOutput('gridded_uploaded'),
                             
                             tags$br(),
                            
                             fluidRow(
                               column(4, radioButtons('loadauxsource', "Source auxiliary data from:", 
                                                      choices=c('Upload new file','FishSET database'), 
                                                      selected='Upload new file', inline=TRUE)),
                               uiOutput('aux_upload')
                             ), 
                             
                             mergeUI("aux", dat_type = "aux"),
                             
                             actionButton('callTextDownloadUp','Save notes'),
                             textInput('notesUp', "Notes", value=NULL, 
                                       placeholder = 'Write notes to store in text output file. Text can be inserted into report later.'),
                             
                             textInput("exprUp", label = "Enter an R expression",
                                       value = "values$dataset"),
                             actionButton("runUp", "Run", class = "btn-success"),
                             div(style = "margin-top: 2em;",
                                 uiOutput('resultUp')
                             ) 
                           )),
                  #---
      # Data quality evaluation tabset panel ----
                  #---
                  tabPanel("Data Quality Evaluation", value = "qaqc",
                           sidebarLayout(
                             sidebarPanel(width=3,
                                          tags$br(),tags$br(),
                                          
                                          tabPlotUI("qaqc", type = "tab_plot"),
                                          actionButton('saveData','Save data to FishSET database'),
                                          tags$br(),
                                          tags$button(
                                            id = 'closeQAQC',
                                            type = "button",
                                            style="color: #fff; background-color: #FF6347; border-color: #800000;",
                                            class = "btn action-button",
                                            onclick = "setTimeout(function(){window.close();},500);",  # close browser
                                            "Close app"
                                          ),
                                          
                                          actionButton("refresh1", "Refresh data", 
                                                       style = "color: white; background-color: blue;" 
                                          ),
                                          actionButton('callTextDownloadQAQC','Save notes'),
                                          tags$br(), tags$br(),
                                          textInput('notesQAQC', "Notes", value=NULL,
                                                    placeholder = 'Write notes to store in text output file. Text can be inserted into report later.'),
                                          
                                           radioButtons("checks", "Select data quality check function to run", 
                                                        choices = c('Variable class', 'Summary table', 'Outliers', 
                                                                    'NAs', 'NaNs', 'Unique observations', 'Empty variables', 
                                                                    'Latitude and Longitude'='Lat_Lon units', 'Spatial data')),
                                          
                                          conditionalPanel(
                                            condition = "input.checks == 'Variable class'",
                                            actionButton('rchclass', 'Change variable classes', style = "color: white; background-color: #0073e6;")
                                          ),
                                          uiOutput('outlier_column'),
                                          uiOutput('outlier_subset_method'),
                                          uiOutput('outlier_subset'),
                                          uiOutput('outlier_dist'),
                                          conditionalPanel("input.checks == 'NAs'",
                                            actionButton('NA_Filter_all', 'Remove all NAs', style = "color: white; background-color: #0073e6;"),
                                            actionButton('NA_Filter_mean', 'Replace NAs with mean value', style = "color: white; background-color: #0073e6;")
                                          ),
                                          conditionalPanel("input.checks == 'NaNs'",
                                            actionButton('NAN_Filter_all', 'Remove all NaNs', style = "color: white; background-color: #0073e6;"),
                                            actionButton('NAN_Filter_mean', 'Replace NaNs with mean value', style = "color: white; background-color: #0073e6;")
                                          ),
                                          conditionalPanel("input.checks=='Outliers'",
                                                           actionButton('Outlier_Filter', 'Remove outliers', style = "color: white; background-color: #0073e6;"),
                                                           uiOutput("hover_info1")
                                          ),
                                          conditionalPanel("input.checks=='Unique observations'",
                                            actionButton('Unique_Filter', 'Remove non-unique rows', style = "color: white; background-color: #0073e6;")
                                          ),
                                          conditionalPanel("input.checks=='Empty variables'",
                                            actionButton('Empty_Filter', 'Remove empty variables', style = "color: white; background-color: #0073e6;")
                                          ),
                                           uiOutput('LatLonDir'),
                                          
                                          conditionalPanel("input.checks=='Lat_Lon units'",
                                            selectInput('LatLon_Filter_Lat', 'Change sign for latitude direction', 
                                                        choices=c('None', 'All values'='all', 'Positve to negative'='neg', 
                                                        'Negative to positive'='pos'), selected='None'),
                                            selectInput('LatLon_Filter_Lon', 'Change sign for longitude direction', 
                                                        choices=c('None', 'All values'='all', 'Positve to negative'='neg', 
                                                                  'Negative to positive'='pos'), selected='None'),
                                            actionButton('LatLon_Filter', 'Convert lat/long to decimal degrees', 
                                                         value=FALSE, style = "color: white; background-color: #0073e6;")
                                          ),
                                          
                                          conditionalPanel("input.checks=='Spatial data'",
                                                  
                                                   conditionalPanel("input.spat_qaqc_tab == 'checks'",         
                                                           uiOutput("spatQAQC_checkUI")),
                                                   
                                                   conditionalPanel("input.spat_qaqc_tab == 'corrections'",
                                                           uiOutput("spatQAQC_correctUI"))
                                                           ),
                                          
                                          tags$br(),
                                          ##Inline scripting 
                                          textInput("exprQA", label = "Enter an R expression",
                                                    value = "values$dataset"),
                                          actionButton("runQA", "Run", class = "btn-success"),
                                          div(style = "margin-top: 2em;",
                                               uiOutput('resultQA')
                                          )
                             ),#END SIDEBAR LAYOUT             
                             mainPanel(width=9,
                                       tags$br(), tags$br(),
                                       htmlOutput("Case"),
                                       conditionalPanel("input.checks == 'Variable class'",
                                                        DT::dataTableOutput('changetable') ),
                                       conditionalPanel("input.checks=='Summary table'",
                                                        DT::DTOutput("output_table_summary")),
                                       tags$br(),tags$br(),
                                       conditionalPanel("input.checks=='Outliers' && input.column_check == ''",
                                                        plotOutput('outlierbox')),  
                                       conditionalPanel("input.checks=='Outliers'",
                                                    
                                       shinycssloaders::withSpinner(DT::DTOutput("output_table_outlier")),
                                       splitLayout(cellWidths = c('33%','33%','33%'),
                                                   shinycssloaders::withSpinner(plotOutput('plot1',
                                                              hover = hoverOpts("plot1_hover", delay = 100, delayType = "debounce"),
                                                              dblclick = "plot1_dblclick", 
                                                              brush = brushOpts(id = "plot1_brush",resetOnNew = TRUE )
                                                   )),
                                                   shinycssloaders::withSpinner(plotOutput('plot2', dblclick = "plot2_dblclick", 
                                                              brush = brushOpts(id = "plot2_brush",resetOnNew = TRUE))),
                                                   shinycssloaders::withSpinner(plotOutput('plot3',
                                                              hover = hoverOpts("plot3_hover", delay = 100, delayType = "debounce"),
                                                              dblclick = "plot3_dblclick", 
                                                              brush = brushOpts(id = "plot3_brush",resetOnNew = TRUE)
                                                   )))
                                       ),
                                       conditionalPanel("input.checks=='NAs'",
                                                        DT::DTOutput('missingtable')),
                                       DT::DTOutput('output_table_latlon'),
                                       
                                       conditionalPanel("input.checks=='Spatial data'",
                                                        
                                         tabsetPanel(id = "spat_qaqc_tab", selected = "checks",
                                                     
                                            tabPanel(title = "Spatial Checks", value = "checks",
                                                     shinycssloaders::withSpinner(uiOutput("spatQAQC_checkOut"))      
                                            ),
                                            
                                            tabPanel(title = "Spatial Corrections", value = "corrections",
                                                     uiOutput("spat_correct_msg"),
                                                     radioButtons("select_spat_tab", "Show:",
                                                                  choices = c("all", "points outside zone" = "out_zone")),
                                                     tags$div(shinycssloaders::withSpinner(DT::DTOutput("spat_correct_tab")), 
                                                             style = "font-size: 75%; width: 100%")
                                                     )))
                                       ))),
                  #---
      # Data exploration tabset panel ----
                  #---
                  tabPanel("Data Exploration", value = "explore",
                           sidebarLayout(
                             sidebarPanel(width=2,
                                          tags$br(),tags$br(),
                                          conditionalPanel("input.plot_table=='Plots'",
                                            tabPlotUI("explore")
                                          ),
                                          
                                          conditionalPanel("input.plot_table=='Table'",
                                            actionButton('subsetData', 'Remove variable from dataset')
                                          ),
                                          actionButton('callTextDownloadExplore','Save notes'),
                                          actionButton('saveDataExplore','Save data to FishSET database'),
                                          tags$button(
                                            id = 'closeExplore',
                                            type = "button",
                                            style="color: #fff; background-color: #FF6347; border-color: #800000;",
                                            class = "btn action-button",
                                            onclick = "setTimeout(function(){window.close();},500);",  # close browser
                                            "Close app"
                                          ),
                                          actionButton("refresh", "Refresh data", 
                                                       style = "color: white; background-color: blue;" 
                                          ),
                                          tags$br(), tags$br(),
                                          textInput('notesExplore', "Notes", value=NULL, 
                                          placeholder = 'Write notes to store in text output file. 
                                                    Text can be inserted into report later.'),
                                          
                                          uiOutput("SelectDatasetExploreUI"), 
                                          
                                          conditionalPanel("input.SelectDatasetExplore=='main' || input.SelectDatasetExplore=='grid'",
                                             selectInput('plot_table', 'View data table or plots', 
                                                         choices=c('Table','Plots'), selected='Table')
                                          ),
                                          
                                          conditionalPanel("input.SelectDatasetExplore=='main' && input.plot_table=='Plots'",
                                            selectInput('plot_type', 'Select Plot Type', 
                                                        choices=c('Temporal','Spatial','x-y plot'))
                                          ),
                                          
                                          conditionalPanel("input.SelectDatasetExplore=='main' && input.plot_table=='Plots' && input.plot_type=='Spatial'",
                                                           uiOutput("mtgt_output"),
                                                           uiOutput('mtgt_output_secondary'),
                                                           uiOutput('mtgt_out2'),
                                                           uiOutput("location_info_spatial")),
                                          
                                          conditionalPanel("input.SelectDatasetExplore=='main' && input.plot_table=='Table'",
                                            verbatimTextOutput('editText')
                                          ),
                                          
                                          conditionalPanel("input.SelectDatasetExplore == 'grid' && input.plot_table == 'Plots'",
                                             uiOutput("plot_grid_args")
                                             ),
                                        
                                          ##Inline scripting 
                                          textInput("expr", label = "Enter an R expression",
                                                   value = "values$dataset"),
                                          actionButton("runI", "Run", class = "btn-success"),
                                          div( style = "margin-top: 2em;",
                                               uiOutput('resultI')
                                          )
                             ),
                             mainPanel(width=10,
                                       tags$div(shinycssloaders::withSpinner(DT::DTOutput("output_table_exploration")), 
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
                                         shinycssloaders::withSpinner(plotOutput('plot_time')),
                                       ),
                                       
                                       conditionalPanel(
                                         "input.SelectDatasetExplore=='main' && input.plot_table=='Plots' && input.plot_type=='Spatial'",
                                         
                                         column(shinycssloaders::withSpinner(plotOutput('plot_spatial',
                                                           click = "plot_spatial_click",
                                                           dblclick = "plot_spatial_dblclick",
                                                           brush = brushOpts(id = "plot_spatial_brush", resetOnNew = FALSE))), width=7),
                                         column(shinycssloaders::withSpinner(plotOutput('map_kernel')), width=7),
                                         column(shinycssloaders::withSpinner(DT::DTOutput('output_table_gt_mt')), width=6)
                                         
                                       ),
                                       
                                       conditionalPanel(
                                         "input.SelectDatasetExplore=='main' && input.plot_table=='Plots' && input.plot_type=='x-y plot'",
                                         uiOutput('xy_selectUI'),
                                         shinycssloaders::withSpinner(plotOutput('plot_xy'))
                                       ),
                                       
                                       # gridded data plot 
                                       conditionalPanel(
                                          "input.SelectDatasetExplore == 'grid' && input.plot_table == 'Plots'",
                                          shinycssloaders::withSpinner(plotOutput("grid_plot"))
                                       )
                             ))),
                   
      # Fleet functions ----
                  
                  tabPanel("Fleet Assignment and Summary", value = "fleet",

                           sidebarLayout(

                             sidebarPanel(

                                uiOutput("fleetSaveOutputUI"),
                                saveDataTableUI("fleet"),
                                refreshUI("fleet"),
                                closeAppUI("fleet"),
                                noteUI("fleet"),
                                conditionalPanel("input.fleet_tab == 'fleet_summary'",
                                                uiOutput("run_fleet_fun")),

                                conditionalPanel("input.fleet_tab == 'fleet_assign'",

                                                 selectInput("assign_fun", label = "Select task",
                                                              choices = c("Define fleets", "Fleet assignment"),
                                                              selected = "Define fleets"),

                                                 conditionalPanel("input.assign_fun == 'Define fleets'",

                                                                  fleet_tableUI("f_table")),

                                                 conditionalPanel("input.assign_fun == 'Fleet assignment'",

                                                                  fleet_assignUI("f_assign"))
                                ),

                               conditionalPanel("input.fleet_tab == 'fleet_summary'",
                                                selectInput("fleet_fun", "Select function",
                                                            choices = c("vessel count" = "vessel_count", "species catch" = "species_catch",
                                                                        "rolling catch" = "roll_catch", "weekly catch" = "weekly_catch",
                                                                        "weekly effort" = "weekly_effort", "bycatch", "trip duration" = "trip_dur_out",
                                                                        "density plot" = "density_plot"), 
                                                            multiple = FALSE,
                                                            selected = "vessel_count"),


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

                                                                density_plotUI("den"))

                                 ),

                              # RexpressionUI("fleet")
                              textInput("exprFleet", label = "Enter an R expression",
                                        value = "values$dataset"),
                              actionButton("runFleet", "Run", class = "btn-success"),
                              div(style = "margin-top: 2em;",
                                  uiOutput('resultFleet')
                              )
                               ), # sidebarPanel

                             mainPanel(

                               tabsetPanel(id = "fleet_tab", selected = "fleet_assign",

                                           tabPanel(title = "Fleet Assignment", value = "fleet_assign",

                                                    conditionalPanel("input.assign_fun == 'Define fleets'",

                                                      fleet_exprUI("f_table"),

                                                      fleet_tableOut("f_table")),

                                                    conditionalPanel("input.assign_fun == 'Fleet assignment'",

                                                      fleet_assignOut("f_assign"))
                                            ),

                                             tabPanel(title = "Fleet Summary", value = "fleet_summary",

                                                    conditionalPanel("input.fleet_fun == 'vessel_count'",

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
                           )
                  ),
                  
                  
                  #---
      # Basic analyses tabset panel ----
                  #---
                  tabPanel("Simple Analyses", value = "analysis",
                           sidebarLayout(
                             sidebarPanel(
                               tabPlotUI("anal"),
                               modSaveUI("anal"),
                               actionButton('callTextDownloadAnal','Save notes'),
                               tags$button(
                                 id = 'closeAnalysis',
                                 type = "button",
                                 style="color: #fff; background-color: #FF6347; border-color: #800000;",
                                 class = "btn action-button",
                                 onclick = "setTimeout(function(){window.close();},500);",  # close browser
                                 "Close app"
                               ),
                               actionButton("refresh2", "Refresh data", 
                                            style = "color: white; background-color: blue;" 
                               ),
                               tags$br(),tags$br(),
                               textInput('notesAnal', "Notes", value=NULL, 
                                         placeholder = 'Write notes to store in text output file. Text can be inserted into report later.'),
                               tags$br(),tags$br(),
                               
                               conditionalPanel("input.corr_reg == 'Correlation'", 
                                                
                                                actionButton("run_corr", "Run function", 
                                                             style = "color: white; background-color: blue;")
                                                ),
                               
                               conditionalPanel("input.corr_reg == 'Regression'", 
                                                
                                                actionButton("run_reg", "Run function", 
                                                             style = "color: white; background-color: blue;")
                               ),
                               
                               selectInput('corr_reg','Show correlations or simple linear regression', 
                                           choices=c('Correlation','Regression'), selected='Correlation'),
                               ##Inline scripting 
                               textInput("exprA", label = "Enter an R expression",
                                         value = "values$dataset"),
                               actionButton("runA", "Run", class = "btn-success"),
                               div(style = "margin-top: 2em;",
                                   uiOutput('resultA')
                               )
                             ),
                             mainPanel(
                               tags$br(),
                               conditionalPanel("input.corr_reg=='Correlation'",
                                   uiOutput('corr_out'),
                                   verbatimTextOutput('output_text_corr'),
                                   div(DT::DTOutput('output_table_corr'), style = "font-size: 75%; width: 100%"),
                                   tags$br(), tags$br(),
                                   shinycssloaders::withSpinner(plotOutput('output_plot_corr', width='100%', height = "600px"))
                                ),
                               conditionalPanel("input.corr_reg=='Regression'",
                                   uiOutput('reg_resp_out'),
                                   uiOutput('reg_exp_out'),
                                   verbatimTextOutput('output_text_reg'),
                                   shinycssloaders::withSpinner(plotOutput('output_plot_reg'))
                                 ))
                           )),
                  
                  #---
      # Create new variables tabset panel ----
                  #---
                  tabPanel('Compute New Variables', value='new',
                           sidebarLayout(
                             sidebarPanel(
                                downloadButton("exportData", "Download data"),
                                
                                selectInput("export_type", "Export data as:",
                                            choices = c("csv", "txt", "rdata", "xlsx", 
                                                        "json", "stata", "sas", "spss", "matlab")),
                                
                                actionButton("save_final_modal", "Save final table to FishSET database",
                                             style = "color: #fff; background-color: #6EC479; border-color:#000000;"),
                                
                                tags$hr(style = "border-top: 3px solid #bbb;"),
                                
                                # downloadLink('downloadplotNew', label=''),
                                # actionButton('downloadplotNew', label='Save plot to folder'),
                                actionButton('callTextDownloadNew','Save notes'),
                                actionButton('saveData','Save data to FishSET database'),
                               tags$br(),
                               tags$button(
                                 id = 'closeNew',
                                 type = "button",
                                 style="color: #fff; background-color: #FF6347; border-color: #800000;",
                                 class = "btn action-button",
                                 onclick = "setTimeout(function(){window.close();},500);",  # close browser
                                 "Close app"
                               ),
                               actionButton("refreshNew", "Refresh data", 
                                            style = "color: white; background-color: blue;"),
                               actionButton('runNew',"Run function",
                                            style="color: #fff; background-color: #6da363; border-color: #800000;"),
                               tags$br(), tags$br(),                              
                               textInput('notesNew', "Notes", value=NULL, 
                                         placeholder = 'Write notes to store in text output file. Text can be inserted into report later.'),
                               
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
                                                            choices = c('Create distinct haul or trip ID'='ID_var',
                                                                        'Create binary fishery season identifier'='binary_seasonID',
                                                                        'Create location, gear, species-specific fishery season identifier'='create_seasonal_ID'),
                                                            multiple = FALSE, selected='ID_var')),
                               conditionalPanel("input.VarCreateTop=='Arithmetic functions'",
                                                selectInput('numfunc','Functions', 
                                                            choices = c('Numeric functions'='create_var_num',
                                                                        'Catch or revenue per unit effort'='cpue'),
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
                               conditionalPanel("input.trip!='haul_to_trip'||input.trip!='trip_centroid'", 
                                                
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
                               
                               ##Inline scripting 
                               textInput("exprN", label = "Enter an R expression",
                                         value = "values$dataset"),
                               actionButton("runN", "Run", class = "btn-success"),
                               div(style = "margin-top: 2em;",
                                   uiOutput('resultN')
                               )
                             ),
                             mainPanel(
                               DT::DTOutput("output_table_create")
                             )
                           )),
                  
                  
      # Map Viewer ----
                  
                  tabPanel("Map Viewer",
                           
                           map_viewerUI("map")
                           
                           ),
                  
                  #---
      # Alternative Choice tabset panel ----
                  #---
                  tabPanel('Define Alternative Fishing Choices', value = "altc",
                           sidebarLayout(
                             sidebarPanel(
                               tags$button(
                                 id = 'closeAlt',
                                 type = "button",
                                 style="color: #fff; background-color: #FF6347; border-color: #800000;",
                                 class = "btn action-button",
                                 onclick = "setTimeout(function(){window.close();},500);",  # close browser
                                 "Close app"
                               ),
                               
                               actionButton("refreshZ", "Refresh data", 
                                            style = "color: white; background-color: blue;" 
                               ),
                               
                               actionButton('saveALT','Save choices', style = "color: white; background-color: green;"), 
                                              
                               actionButton('callTextDownloadAlt','Save notes'),
                               textInput('notesAltc', "Notes", value = NULL, 
                                         placeholder = 'Write notes to store in text output file. Text can be inserted into report later.'),
                               # Inline scripting 
                               textInput("exprZ", label = "Enter an R expression",
                                         value = "values$dataset"),
                               actionButton("runZ", "Run", class = "btn-success"),
                               div(style = "margin-top: 2em;",
                                   uiOutput('resultZ')
                               )
                             ),
                             
                             mainPanel(
                               
                              uiOutput('altc_ui'),
                              
                              div(style="display: inline-block;vertical-align:top; width: 500px;",
                                  
                                  plotOutput('altc_zone_plot')),
                              
                               div(style="display: inline-block;vertical-align:top; width: 160px;",
                                   
                                   textOutput('zoneIDText')),
                             )
                           )),
                  #---
      # Expected catch tabset panel ----
                  #---
                  tabPanel("Expected Catch/Revenue", value = "expectedCatch",
                           sidebarLayout(
                             sidebarPanel(
                               tags$button(
                                 id = 'closeEC',
                                 type = "button",
                                 style="color: #fff; background-color: #FF6347; border-color: #800000;",
                                 class = "btn action-button",
                                 onclick = "setTimeout(function(){window.close();},500);",  # close browser
                                 "Close app"
                               ),
                               actionButton("refreshEC", "Refresh data", 
                                            style = "color: white; background-color: blue;" 
                               ),
                               tags$br(),
                               actionButton("submitE", "Run expected catch/revenue function", style="color: #fff; background-color: #6da363; border-color: #800000;"), 
                               tags$br(),tags$br(), 
                               actionButton('callTextDownloadEC','Save notes'),
                               textInput('notesEC', "Notes", value=NULL, 
                                         placeholder = 'Write notes to store in text output file. Text can be inserted into report later.'),

                               uiOutput('selectcp'),
                               #h5('Compute expectations for the entire fleet or by defined groups'),
                               
                               add_prompter(div(
                                            div(style="display:inline-block; width: 145px;", h4('Temporal options')), 
                                            div(style="display:inline-block; width: 10px;", icon('info-circle', verify_fa = FALSE))),
                                          position = "right", type='info', size='large', 
                                          message = 'Use the entire temporal record of catch or take the timeline of catch into account. 
                                  When timeline is considered, catch for a given day is the average for the defined number of days (window), 
                                  shifted to the past by the defined number of days (lag). For example, a window of 3 days and lag of 1 day means we take the 
                                  average catch over three days starting one day prior to the given date.'),
                               div(style = "margin-left:19px;font-size: 12px", 
                                   selectInput('temporal', 'Method to sort time:', c('Entire record of catch (no time)', 'Daily timeline'='daily', 'Sequential order'='sequential'))),
                               uiOutput('expcatch'),
                               conditionalPanel("input.temporal!='Entire record of catch (no time)'",
                                                style = "margin-left:19px;font-size: 12px",
                                                  numericInput('temp_year', 'No. of years to go back if expected catch based on from previous year(s) catch ', value=0, min=0, max='')),
                               #if(input$temporal!='Entire record of catch (no time)') {h5('Moving window averaging parameters')},
                               conditionalPanel("input.temporal!='Entire record of catch (no time)'",
                                                style = "margin-left:19px;font-size: 12px", 
                                                  numericInput('temp_window', 'Window size (days) to average over', value = 7, min=0)),
                               conditionalPanel("input.temporal!='Entire record of catch (no time)'", 
                                                style = "margin-left:19px;font-size: 12px", 
                                                  numericInput('temp_lag', 'Time lag (in days) ', value = 0, min=0, max='')),
                               conditionalPanel("input.temporal!='Entire record of catch (no time)'", 
                                                style = "margin-left:19px;font-size: 12px", 
                                                  selectInput('calc_method','Expectation calculation:', 
                                                            choices = c("Standard average"="standardAverage", 
                                                                        "Simple lag regression of means"="simpleLag"#, 
                                                                        #"Weights of regressed groups"="weights"
                                                            ))), 
                               conditionalPanel("input.temporal!='Entire record of catch (no time)'", 
                                                selectInput('lag_method', 'Method to average across time steps', 
                                                            choices= c("Entire time period"="simple", "Grouped time periods"="grouped"))),
                               conditionalPanel("input.temporal!='Entire record of catch (no time)'", 
                                      h4('Averaging options')),
                               conditionalPanel("input.temporal!='Entire record of catch (no time)'", 
                                  div(style = "margin-left:19px; font-size: 12px", 
                                      selectInput('empty_catch', 'Replace empty catch with:', 
                                                  choices = c("NA: NA's removed when averaging"='NA', '0', 'Mean of all catch' ="allCatch", 
                                                              'Mean of grouped catch' = "groupedCatch"))) 
                               ),#h6("Note: Na's removed when averaging"), 
                               h4('Expected Catch/Dummy options'), 
                               div(style = "margin-left:19px; font-size: 12px",
                                   selectInput('empty_expectation', 'Replace empty expected catch with:', 
                                               choices = c("NA: NA's removed when averaging"='NA', 1e-04, 0))),  
                               #h6("Note: Na's removed when averaging"),
                               div(style = "margin-left:19px; font-size: 14px",
                                   checkboxInput('dummy_exp', 'Output dummy variable for originally missing values?', value=FALSE)),
                               checkboxInput('replace_output', 'Replace previously saved expected catch output with new output', value=FALSE),
                               ##Inline scripting 
                               textInput("exprEC", label = "Enter an R expression",
                                         value = "values$dataset"),
                               actionButton("runEC", "Run", class = "btn-success"),
                               div(style = "margin-top: 2em;",
                                   uiOutput('resultEC')
                               )
                               ),
                             mainPanel(
                               tags$br(),tags$br(),
                               tags$p('Compute expected catch for each observation and zone.', 
                                      tags$br(), tags$br(),
                                      'Function returns the expected catch or expected revenue data frame based on selected parameters.',
                                      tags$br(), tags$br(),
                                      icon('exclamation-circle', verify_fa = FALSE), tags$b('Three default matrices are also all computed:'),  icon('exclamation-circle', verify_fa = FALSE),
                                      tags$br(),
                                      tags$b('Short-term expected catch:'), 'Expected catch/revenue based on catch of the previous two days.',
                                      tags$br(),
                                      tags$b('Medium-term expected catch:'), 'Expected catch/revenue based on catch for the previous seven days.', 
                                      tags$br(),
                                      tags$b('Long-term expected catch:'), 'Expected catch/revenue based on catch in the previous year.', 
                                      tags$br(), tags$br(),
                                      'Output saved in FishSET database. Previously saved expected catch/revenue output will be written over if the', 
                                      tags$i('Replace previously saved'), 'box is checked. Leaving the box unchecked will add new output to existing output.'),
                               tags$br(), tags$br(),
                               conditionalPanel("input.temp_var!='none'",
                                                tagList(
                                     h4('Sparsity of observations by time period and zone.'),
                                     h5('Higher values indicate greater sparsity.'),
                                    DT::DTOutput('spars_table'),
                                    shinycssloaders::withSpinner(plotOutput('spars_plot'))
                                                ))
                             )
                             )),
                  #---
      # Model design and Run tabset panel ----
                  #---
                   tabPanel("Models", value = "models",
                            tabsetPanel(
                                tabPanel("Run model(s)",
                           sidebarLayout(
                             sidebarPanel(
                               tags$button(
                                 id = 'closeModel',
                                 type = "button",
                                 style="color: #fff; background-color: #FF6347; border-color: #800000;",
                                 class = "btn action-button",
                                 onclick = "setTimeout(function(){window.close();},500);",  # close browser
                                 "Close app"
                               ),
                               tags$br(),
                               
                               # Models can't be run if final dataset not detected
                               uiOutput("disableMsg"),
                               
                                 actionButton("addModel", "Save model and add new model", 
                                              style="color: #fff; background-color: #337ab7; border-color: #800000;"),
                               
                               tags$br(),
                               conditionalPanel("input.addModel!='0'",
                                  shinyjs::disabled(
                                   actionButton("submit_modal", "Run model(s)", style="color: #fff; background-color: #6da363; border-color: #800000;")
                                  )
                               ),
                               tags$br(),tags$br(),
                               tags$p(tags$strong("More information"), tags$br(),
                                      "Model parameter table is editable. Double click a cell to edit."),
                               actionButton('callTextDownloadModels','Save notes'),
                               textInput('notesModel', "Notes", value=NULL, 
                                         placeholder = 'Write notes to store in text output file. Text can be inserted into report later.'),
                               ##Inline scripting 
                               textInput("exprM", label = "Enter an R expression",
                                         value = "values$dataset"),
                               actionButton("runM", "Run", class = "btn-success"),
                               div(style = "margin-top: 2em;", uiOutput('resultM'))
                             ),
                             mainPanel(
                               div(id = "form",
                                   #h4('Alternative choice matrix parameters'),
                                   #selectInput("alternatives", label = "Create alternative choice matrix from",
                                   #            choices = list("Loaded data" = 'loadedData', "Grid data" = "griddedData"),
                                  #             selected = 'loadedData'),
                                   #uiOutput('latlonB'),
                                   #uiOutput('portmd'),
                                   h4('Likelihood function'),
                                   selectInput("model", label = "",
                                               choices = list("Conditional logit" = 'logit_c', "Average catch" = "logit_avgcat", "Logit Dahl correction" = "logit_correction",
                                                              'EPM normal'='epm_normal', 'EPM lognormal'='epm_lognormal', 'EPM Weibull'='epm_weibull'),
                                               selected = 'logit_c'),
                                   selectInput('optmeth', 'Optimization method', 
                                               choices=c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN"), selected='BFGS'),
                                   h4('Select variables to include in model'),
                                   div(style="display: inline-block;vertical-align:top; width: 250px;", uiOutput('indvariables')),
                                   div(style="display: inline-block;vertical-align:top; width: 250px;", uiOutput('gridvariables')),
                                   uiOutput('catch_out'),
                                   uiOutput('logit_correction_extra'),
                                   uiOutput('logit_c_extra'),
                                   h3('Model parameters'),
                                   
                                   fluidRow(
                                     h4("Optimization options"),
                                     splitLayout(cellWidths = c("22%", "22%", "22%", "22%"),
                                                 numericInput("mIter", "max iterations", value = 100000),
                                                 numericInput("relTolX", "tolerance of x", value = 0.00000001),
                                                 numericInput("reportfreq", "report frequency", value = 1),
                                                 numericInput("detailreport", "detailed report", value = 1, max=6)
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
                                        Conditional logit and average catch logit: alternative-specific parameters,  travel-distance parameters
                                        Logit correction:  marginal utility from catch, catch-function parameters, polynomial starting parameters, travel-distance parameters, catch sigma
                                        EPM likelihood functions:  catch-function parameters, travel-distance parameters, catch sigma(s), scale parameter 
                                        See Likelihood section of Help Manual for more details.")),
                                     uiOutput('paramsourcechoose'),
                                     conditionalPanel(condition="input.initchoice=='prev'",
                                            uiOutput("paramtable")),
                                     uiOutput("Inits")
                                   
                                   ),
                                   
                                   DT::DTOutput('mod_param_table')
                               )
                             ))),
                           tabPanel("Compare models",
                                    sidebarLayout(
                                      sidebarPanel(
                                        tags$br(),tags$br(),
                                        actionButton("reload_btn", "Reload model output"),
                                        actionButton("delete_btn", "Delete row"),
                                        h3(''),
                                        actionButton("submit_ms", "Save table", style="color: #fff; background-color: #337ab7; border-color: #2e6da4;"),
                                        tags$br(),tags$br(),
                                        tags$button(
                                          id = 'closeCM',
                                          type = "button",
                                          style="color: #fff; background-color: #FF6347; border-color: #800000;",
                                          class = "btn action-button",
                                          onclick = "setTimeout(function(){window.close();},500);",  # close browser
                                          "Close window"
                                        ), width=2),
                                      mainPanel(
                                        h3('Model Output'),
                                        h4("Measures of fit"),
                                        DT::DTOutput("mytable"),
                                        tags$script(HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
                                                         Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());})")),
                                        tags$br(),
                                        h4('Model output (convergence, SE, Hessian)'),
                                        DT::DTOutput('modeltab'),
                                        tags$br(),
                                        h4('Error messages'),
                                        DT::DTOutput('errortab'),
                                        width=10
                                        )
                                      )  )
                            )),
                  #--- 
      # Bookmark tabset panel ----
                  #---
                  tabPanel('Bookmark Choices', value = "book",
                           
                     tabsetPanel(id = "boomark_tab", 
                     
                     tabPanel("Bookmark", value = "bookmark_page",
                           
                           tags$br(),
                           tags$button(
                             id = 'closeB',
                             type = "button",
                             style="color: #fff; background-color: #FF6347; border-color: #800000;",
                             class = "btn action-button",
                             onclick = "setTimeout(function(){window.close();},500);",  # close browser
                             "Close app"
                           ),
                           tags$br(),tags$br(),
                           h4('Bookmark selected choices'),
                           bookmarkButton(),
                           tags$br(),
                           textInput('notesBook', "Notes", value=NULL, placeholder = 'Paste bookmarked URL here.'),
                           actionButton('callTextDownloadBook','Save notes'),
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
                                                   'subtab to rerun logged function calls.')))
                     ),
                     
                     tabPanel("Rerun logged function calls", value = "log_rerun",
                              
                              sidebarLayout(
                                 
                                 sidebarPanel(
                                    
                                    tags$button(
                                       id = "closeRerun",
                                       type = "button",
                                       style="color: #fff; background-color: #FF6347; border-color: #800000;",
                                       class = "btn action-button",
                                       onclick = "setTimeout(function(){window.close();},500);",  # close browser
                                       "Close app"
                                    ),
                                    
                                    actionButton("run_log", "Rerun log",
                                                 style="color: #fff; background-color: #6da363; border-color: #800000;"),
                                    
                                    uiOutput('new_dat_cb_choices'),
                                    
                                    
                                    p("Click on the table rows to run specific function calls.")
                                 ),
                                 
                                 mainPanel(
                                    
                                    shinycssloaders::withSpinner(DT::DTOutput("log_table")),
                                 )
                              )
                           )
                        )
                     )
                  )
          
         
      )
    }
    