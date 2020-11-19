source("fleetUI.R", local = TRUE)
source("fleet_helpers.R", local = TRUE)
source("map_viewer_app.R", local = TRUE)


   ## USER INTERFACE    
    ui = function(request){
      fluidPage(
      shinyjs::useShinyjs(),
      #---- 
      #Formatting
      #----
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
      #----

      #----
      tabsetPanel(id = "tabs",
                  #Landing page
                  #----
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
                                                        tags$p('The FishSET toolbox is provided as a set of R functions that can be run in an R console or here in this R Shiny application.'),
                                                        tags$p('The FishSET Shiny application is divided into tabs to guide users through the steps of creation, from uploading and exploring data to developing 
                                                               and evaluating models. Tabs can be navigated in any order. 
                                                                All data is automatically saved to a SQL database called the FishSET database when first loaded. The database is housed in the FishSET R package folder. 
                                                                Modified versions of the data can be saved to the FishSET database in the', tags$em('Data Quality Evaluation'), 'and', 
                                                               tags$em('Compute New Variables'), 'tabs.   
                                                                Plots and table outputs are saved in an output folder within the FishSET R package. 
                                                              Function calls, including chosen parameters, are saved to the', tags$em('Logs'), 'folder in the FishSET package folder. 
                                                              The', tags$em('Quickstart Guide'), 'subtab provides further assistance on using the FishSET Shiny application.'),
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
                                                                                       'Zonal Definition tab'='ZonalTab',
                                                                                       'Expected Catch/Revenue tab'='ExpectedTab',
                                                                                       'Models tab'='ModelTab',
                                                                                       'Bookmark Choices tab'='BookmarkTab'
                                                                                       ), selected='AcrossTabs', inline = TRUE), style="font-size:115%; font-weight:bold;"
                                      ),
                                      uiOutput('AcrossTabsText'), 
                                      uiOutput('UploadTabsText'),
                                      uiOutput('ExploreTabsText'),
                                      uiOutput('DQTabsText'),
                                      uiOutput('AnalTabsText'),
                                      uiOutput('NewVarsTabsText'),
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
                           )),
                  #---
                  #----
                  #Upload data tabset panel   
                  #-----
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
                               "Load data"
                             ),
                             tags$br(), tags$br(),
                             fluidRow(
                               textInput('projectname', 'Name of project'),
                               column(4, radioButtons('loadmainsource', "Source main data from:", choices=c( 'Upload new file','FishSET database'), selected='Upload new file', inline=TRUE)),
                               uiOutput('main_upload')),
                           
                             fluidRow( 
                               column(width = 8, offset = 2,
                                      uiOutput('ui.action2'))),
                             fluidRow(
                               column(4, radioButtons('loadportsource', "Source port data from:", choices=c( 'Upload new file','FishSET database'), selected='Upload new file', inline=TRUE)),
                               uiOutput('port_upload')
                             ),
                             fluidRow(
                               column(width=8, offset=2,
                                      uiOutput('ui.actionP2'))
                             ),
                             fluidRow(
                               column(4, radioButtons('loadspatialsource', "Source spatial data from:", choices=c( 'Upload new file'), selected='Upload new file', inline=TRUE)),
                                                      #,'FishSET database')
                               uiOutput('spatial_upload')
                             ),
                             fluidRow(
                                 column(4, radioButtons('loadgridsource', "Source gridded data from:", choices=c( 'Upload new file','FishSET database'), selected='Upload new file', inline=TRUE)),
                                 uiOutput('grid_upload')
                               ),
                             fluidRow(
                               column(4, radioButtons('loadauxsource', "Source auxiliary data from:", choices=c( 'Upload new file','FishSET database'), selected='Upload new file', inline=TRUE)),
                               uiOutput('aux_upload')
                             ), 
                             uiOutput('mergeUI'),
                             #uiOutput("SaveButtonsUpload"),
                             #  downloadLink("downloadTextUp", label=''),
                             actionButton('callTextDownloadUp','Save notes'),
                             textInput('notesUp', "Notes", value=NULL, placeholder = 'Write notes to store in text output file. Text can be inserted into report later.')
                           )),
                  #-----
                  #Data quality evaluation tabset panel   
                  #----
                  tabPanel("Data Quality Evaluation", value = "qaqc",
                           sidebarLayout(
                             sidebarPanel(width=3,
                                          tags$br(),tags$br(),
                                          #uiOutput('SaveButtons'),
                                          #shinySaveButton(id = 'downloadplot', label ='Save plot to folder', title = "", filename = paste0(project,'_', input$checks, '_plot'), filetype = "png"),
                                          actionButton('downloadplot', label ='Save plot to folder'),
                                          downloadLink('downloadplotHIDE', label=''),
                                          actionButton('downloaddata', label ='Save table to folder as csv'),
                                          actionButton('callTextDownload','Save notes'),
                                          actionButton('saveDataQ','Save data to FishSET database'),
                                          tags$br(),
                                          tags$button(
                                            id = 'close1',
                                            type = "button",
                                            style="color: #fff; background-color: #FF6347; border-color: #800000;",
                                            class = "btn action-button",
                                            onclick = "setTimeout(function(){window.close();},500);",  # close browser
                                            "Close app"
                                          ),
                                          
                                          actionButton("refresh1", "Refresh data", 
                                                       icon = icon("fa fa-refresh"),
                                                       style = "color: white; background-color: blue;" 
                                          ),
                                          tags$br(), tags$br(),
                                          textInput('notesQAQC', "Notes", value=NULL,
                                                    placeholder = 'Write notes to store in text output file. Text can be inserted into report later.'),
                                          #conditionalPanel(condition = "input.checks == 'Summary table'",
                                          #  selectInput("SelectDatasetDQ", "Select a dataset", choices = c("main", "port", "auxiliary", "grid"))
                                          #),
                                          #h4('Select data check functions to run'),
                                          #Checkbox input widget  
                                           radioButtons("checks", "Select data quality check functions to run", choices = c('Variable class', 'Summary table', 'Outliers', 'NAs', 'NaNs', 
                                                                                  'Unique observations', 'Empty variables', 'Lat_Lon units')),
                                          uiOutput("checks_dataset"),
                                          conditionalPanel(
                                            condition = "input.checks == 'Variable class'",
                                            actionButton('rchclass', 'Change variable classes', style = "color: white; background-color: #0073e6;")
                                          ),
                                          uiOutput('outlier_column'),
                                          uiOutput('outlier_subset'),
                                          uiOutput('outlier_dist'),
                                          conditionalPanel(
                                            condition = "input.checks == 'NAs'",
                                            actionButton('NA_Filter_all', 'Remove all NAs', style = "color: white; background-color: #0073e6;")
                                          ),
                                          conditionalPanel(
                                            condition = "input.checks == 'NAs'",
                                            actionButton('NA_Filter_mean', 'Replace NAs with mean value', style = "color: white; background-color: #0073e6;")
                                          ),
                                          conditionalPanel(
                                            condition = "input.checks == 'NaNs'",
                                            actionButton('NAN_Filter_all', 'Remove all NaNs')
                                          ),
                                          conditionalPanel(
                                            condition = "input.checks == 'NaNs'",
                                            actionButton('NAN_Filter_mean', 'Replace NaNs with mean value')
                                          ),
                                          conditionalPanel(condition="input.checks=='Outliers'",
                                                           actionButton('Outlier_Filter', 'Remove outliers', style = "color: white; background-color: #0073e6;")),
                                          conditionalPanel(
                                            condition ='input.checks=="Outliers"',
                                            uiOutput("hover_info1")),
                                          conditionalPanel(
                                            condition ='input.checks=="Unique observations"',
                                            actionButton('Unique_Filter', 'Remove non-unique rows', style = "color: white; background-color: #0073e6;")
                                          ),
                                          conditionalPanel(
                                            condition ='input.checks=="Empty variables"',
                                            actionButton('Empty_Filter', 'Remove empty variables', style = "color: white; background-color: #0073e6;")
                                          ),
                                           uiOutput('LatLonDir'),
                                          
                                          conditionalPanel(
                                            condition ='input.checks=="Lat_Lon units"',
                                            checkboxInput('LatLon_Filter_Lat', 'Change sign for latitude direction', value=FALSE)
                                          ),
                                          conditionalPanel(
                                            condition ='input.checks=="Lat_Lon units"',
                                            checkboxInput('LatLon_Filter_Lon', 'Change sign for longitude direction', value=FALSE)
                                          ),
                                          conditionalPanel(
                                            condition ='input.checks=="Lat_Lon units"',
                                            actionButton('LatLon_Filter', 'Convert lat/long to decimal degrees', 
                                                         value=FALSE, style = "color: white; background-color: #0073e6;" )
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
                                       conditionalPanel(condition = "input.checks == 'Variable class'",
                                                        DT::dataTableOutput('changetable') ),
                                       conditionalPanel(condition="input.checks=='Summary table'",
                                                        DT::DTOutput("output_table_summary")),
                                       shinycssloaders::withSpinner(DT::DTOutput("output_table_outlier")),
                                       tags$br(),tags$br(),
                                       conditionalPanel(condition="input.checks=='Outliers'",
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
                                       conditionalPanel(condition="input.checks=='NAs'",
                                                        DT::DTOutput('missingtable')),
                                       DT::DTOutput('output_table_latlon')
                                
                                       ))),
                  #---- 
                  #Data exploration tabset panel 
                  #----
                  tabPanel("Data Exploration", value = "explore",
                           sidebarLayout(
                             sidebarPanel(width=2,
                                          tags$br(),tags$br(),
                                          conditionalPanel(
                                            condition='input.plot_table=="Plots"',
                                            #uiOutput('SaveButtonsExplore')),
                                            downloadLink('downloadplotEXPLOREHIDE', label=''),
                                            actionButton('downloadplotExplore', label ='Save plot to folder'),#, title = "", filename = paste0(project, input$plot_type , '_plot'), filetype = "png")
                                            downloadLink('downloadTableEXPLOREHIDE', label=''),
                                            conditionalPanel(condition = "input.plot_type=='Spatial'",
                                                             actionButton('downloadTableExplore', label ='Save table to folder as csv'))),
                                            #  downloadLink("downloadTextExplore", label=''),
                                          conditionalPanel(
                                            condition='input.plot_table=="Table"',
                                            actionButton('subsetData', 'Remove variable from data set')
                                          ),
                                          actionButton('callTextDownloadExplore','Save notes'),
                                          actionButton('saveData','Save data to FishSET database'),
                                          tags$button(
                                            id = 'close',
                                            type = "button",
                                            style="color: #fff; background-color: #FF6347; border-color: #800000;",
                                            class = "btn action-button",
                                            onclick = "setTimeout(function(){window.close();},500);",  # close browser
                                            "Close app"
                                          ),
                                          actionButton("refresh", "Refresh data", 
                                                       icon = icon("fa fa-refresh"),
                                                       style = "color: white; background-color: blue;" 
                                          ),
                                          tags$br(), tags$br(),
                                          textInput('notesExplore', "Notes", value=NULL, placeholder = 'Write notes to store in text output file. 
                                                    Text can be inserted into report later.'),
                                          selectInput("SelectDatasetExplore", "Select a dataset", 
                                                      choices = c("main", "port", "auxiliary", "grid")),
                                          conditionalPanel("input.SelectDatasetExplore=='main' | input.SelectDatasetExplore=='grid'",
                                          selectInput('plot_table', 'View data table or plots', choices=c('Table','Plots'), selected='Table')
                                          ),
                                          conditionalPanel(
                                            condition="input.SelectDatasetExplore=='main' & input.plot_table=='Plots'",
                                            selectInput('plot_type', 'Select Plot Type', choices=c('Temporal','Spatial','x-y plot'))
                                          ),
                                          
                                          #selectInput('varofint', 'Variable of interest', choices=c('A','B','C'))
                                          conditionalPanel(condition="input.SelectDatasetExplore=='main' & input.plot_table=='Plots'& input.plot_type=='Spatial'",
                                                           uiOutput("mtgt_output")),
                                          uiOutput('mtgt_out2'),
                                          conditionalPanel(
                                            condition='input.SelectDatasetExplore=="main" & input.plot_table=="Table"',
                                            verbatimTextOutput('editText')
                                          ),
                                          
                                          conditionalPanel(
                                            condition='input.SelectDatasetExplore=="main" & input.plot_table=="Plots" & input.plot_type=="Spatial"',
                                            uiOutput("location_info_spatial")
                                          ),
                                          
                                          conditionalPanel(
                                             condition = "input.SelectDatasetExplore == 'grid' & input.plot_table == 'Plots'",
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
                                       tags$div(shinycssloaders::withSpinner(DT::DTOutput("output_table_exploration")), style = "font-size: 75%; width: 100%"),
                                       conditionalPanel(
                                         condition="input.SelectDatasetExplore=='main' & input.plot_table=='Plots' && input.plot_type=='Temporal'", 
                                         uiOutput('column_select')),
                                       conditionalPanel(
                                         condition='input.SelectDatasetExplore=="main" & input.plot_table=="Plots" && input.plot_type=="x-y plot"',
                                         uiOutput('xy_select1')),
                                       conditionalPanel(
                                         condition='input.SelectDatasetExplore=="main" & input.plot_table=="Plots" && input.plot_type=="x-y plot"',
                                         uiOutput('xy_select2')),
                                       conditionalPanel(
                                         condition='input.SelectDatasetExplore=="main" & input.plot_table=="Plots" & input.plot_type=="Temporal"',
                                         tagList(
                                           tags$br(),tags$br(),
                                           fluidRow(column(12, align='right',
                                                           div(style="display: inline-block;vertical-align:top;  width: 33%;",
                                                               selectInput("p2fun", label="y-axis function:",c("No. observations",'No. unique observations','% of total observations'), selected='No. of observations')),
                                                           div(style="display: inline-block; vertical-align:top; width: 33%;",
                                                               selectInput("p3fun", "y-axis function:",c('mean','median','min','max','sum'), selected='mean'))
                                           )))
                                       ),
                                       conditionalPanel(
                                         condition='input.SelectDatasetExplore=="main" & input.plot_table=="Plots" & input.plot_type=="Temporal"',
                                         shinycssloaders::withSpinner(plotOutput('plot_time'))
                                       ),
                                       conditionalPanel(
                                         condition='input.SelectDatasetExplore=="main" & input.plot_table=="Plots" & input.plot_type=="Spatial"',
                                         column(shinycssloaders::withSpinner(plotOutput('plot_spatial',
                                                           click = "plot_spatial_click",
                                                           dblclick = "plot_spatial_dblclick", 
                                                           brush = brushOpts(id = "plot_spatial_brush",resetOnNew = FALSE ))), width=7)),
                                       conditionalPanel(
                                         condition='input.SelectDatasetExplore=="main" & input.plot_table=="Plots" & input.plot_type=="Spatial"',
                                         column(shinycssloaders::withSpinner(plotOutput('map_kernel')), width=7)),
                                       conditionalPanel(
                                         condition='input.SelectDatasetExplore=="main" & input.plot_table=="Plots" & input.plot_type=="Spatial"',
                                         column(DT::DTOutput('output_table_gt_mt'), width=6)),
                                       conditionalPanel(
                                         condition='input.SelectDatasetExplore=="main" & input.plot_table=="Plots" && input.plot_type=="x-y plot"',   
                                         shinycssloaders::withSpinner(plotOutput('plot_xy'))),
                                       
                                       # gridded data plot 
                                       conditionalPanel(
                                          condition = "input.SelectDatasetExplore == 'grid' & input.plot_table == 'Plots'",
                                          shinycssloaders::withSpinner(plotOutput("grid_plot"))
                                       )
                             ))),
                   
                  #Fleet functions ==== 
                  
                  tabPanel("Fleet Analysis and Assignment", value = "fleet",
                           
                           sidebarLayout(
                             
                             sidebarPanel(
                               
                               conditionalPanel("input.fleet_tab == 'density_plot'",
                                                
                                                density_plotUI("den")
                               ),
                               
                               conditionalPanel("input.fleet_tab == 'vessel_count'",
                                                
                                                vessel_countUI("ves")
                               ),
                               
                               conditionalPanel("input.fleet_tab == 'species_catch'",
                                                
                                                species_catchUI("spec")
                               ),
                               
                               conditionalPanel("input.fleet_tab == 'roll_catch'",
                                                
                                                roll_catchUI("roll")
                               ),
                               
                               conditionalPanel("input.fleet_tab == 'weekly_catch'",
                                                
                                                weekly_catchUI("wc")
                               ),
                               
                               conditionalPanel("input.fleet_tab == 'weekly_effort'",
                                                
                                                weekly_effortUI("we")
                               ),
                               
                               conditionalPanel("input.fleet_tab == 'bycatch'",
                                                
                                                bycatchUI("by")
                               ),
                               
                               conditionalPanel("input.fleet_tab == 'trip_length'",
                                                
                                                trip_lengthUI("trip")
                               ),
                               
                               conditionalPanel("input.fleet_tab == 'fleet_table'",
                                                
                                                fleet_tableUI("f_table")
                               ),
                               
                               conditionalPanel("input.fleet_tab == 'fleet_assign'",
                                                
                                                fleet_assignUI("f_assign")
                               )
                               
                             ),
                             
                             mainPanel(
                               
                               tabsetPanel(id = "fleet_tab", 
                                           
                                           tabPanel("Density Plot", value = "density_plot",
                                                    
                                                    density_plotOut("den")
                                           ),
                                           
                                           tabPanel("Vessel Count", value = "vessel_count",
                                                    
                                                    fleetOut("ves")
                                           ),
                                           
                                           tabPanel("Species Catch", value = "species_catch",
                                                    
                                                    fleetOut("spec")
                                           ),
                                           
                                           tabPanel("Rolling Catch", value = "roll_catch",
                                                    
                                                    fleetOut("roll")
                                           ),
                                           
                                           tabPanel("Weekly Catch", value = "weekly_catch",
                                                    
                                                    fleetOut("wc")
                                           ),
                                           
                                           tabPanel("Weekly Effort", value = "weekly_effort",
                                                    
                                                    fleetOut("we")
                                           ),
                                           
                                           tabPanel("Bycatch", value = "bycatch",
                                                    
                                                    fleetOut("by")
                                           ),
                                           
                                           tabPanel("Trip Length", value = "trip_length",
                                                    
                                                    fleetOut("trip")
                                           ),
                                           
                                           tabPanel("Fleet Table", value = "fleet_table",
                                                    
                                                    fleet_tableOut("f_table")
                                           ),
                                           
                                           tabPanel("Fleet Assign", value = "fleet_assign",
                                                    
                                                    fleet_assignOut("f_assign")
                                           )
           
                               )
                             )
                           )
                  ),
                  
                  
                  #---- 
                  #Basic analyses tabset panel 
                  #----
                  tabPanel("Simple Analyses", value = "analysis",
                           sidebarLayout(
                             sidebarPanel(
                               #uiOutput('SaveButtonsAnal'),
                                downloadLink('downloadplotAnalHIDE', label =''),
                                downloadLink('downloaddataAnalHIDE', label =''),
                                actionButton('downloadplotAnal', label ='Save plot to folder'),#, title = "", filename = paste0(project,'_', input$corr_reg, '_plot'), filetype = "png"),
                                actionButton('downloaddataAnal', label ='Save table to folder as csv'),
                                #  downloadLink("downloadTextAnal", label=''),
                                actionButton('callTextDownloadAnal','Save notes'),
                               tags$button(
                                 id = 'close2',
                                 type = "button",
                                 style="color: #fff; background-color: #FF6347; border-color: #800000;",
                                 class = "btn action-button",
                                 onclick = "setTimeout(function(){window.close();},500);",  # close browser
                                 "Close app"
                               ),
                               actionButton("refresh2", "Refresh data", 
                                            icon = icon("fa fa-refresh"),
                                            style = "color: white; background-color: blue;" 
                               ),
                               tags$br(),tags$br(),
                               textInput('notesAnal', "Notes", value=NULL, 
                                         placeholder = 'Write notes to store in text output file. Text can be inserted into report later.'),
                               tags$br(),tags$br(),
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
                               conditionalPanel(
                                 condition="input.corr_reg=='Correlation'",
                                 tagList(
                                   uiOutput('corr_out'),
                                   verbatimTextOutput('output_text_corr'),
                                   div(DT::DTOutput('output_table_corr'), style = "font-size: 75%; width: 100%"),
                                   tags$br(), tags$br(),
                                   shinycssloaders::withSpinner(plotOutput('output_plot_corr', width='100%', height = "600px"))
                                 )),
                               conditionalPanel(
                                 condition="input.corr_reg=='Regression'",
                                 tagList(
                                   uiOutput('reg_resp_out'),
                                   uiOutput('reg_exp_out'),
                                   verbatimTextOutput('output_text_reg'),
                                   shinycssloaders::withSpinner(plotOutput('output_plot_reg'))
                                 ))  )
                           )),
                  
                  #----
                  #Create new variables tabset panel
                  #----
                  tabPanel('Compute New Variables', value='new',
                           sidebarLayout(
                             sidebarPanel(
                               #uiOutput('SaveButtonsNew'),
                                downloadLink('downloadplotNew', label=''),
                                actionButton('downloadplotNew', label ='Save plot to folder'),#, title = "", filename = paste0(project, input$plot_type , '_plot'), filetype = "png")
                                #  downloadLink("downloadTextNew", label=''),
                                actionButton('callTextDownloadNew','Save notes'),
                               actionButton('saveDataNew','Save data to FishSET database'),
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
                                            icon = icon("fa fa-refresh"),
                                            style = "color: white; background-color: blue;"),
                               actionButton('runNew',"Run function",
                                            style="color: #fff; background-color: #6da363; border-color: #800000;"),
                               tags$br(),tags$br(),                              
                               textInput('notesNew', "Notes", value=NULL, 
                                         placeholder = 'Write notes to store in text output file. Text can be inserted into report later.'),
                               selectInput('VarCreateTop', "Create variables based on", multiple=FALSE,  
                                           choices=c('Data transformations', 'Nominal ID', 'Arithmetic and temporal functions', 'Dummy variables', 'Spatial functions', 'Trip-level functions')),
                               
                               ## Function options              
                               conditionalPanel(condition="input.VarCreateTop=='Data transformations'",
                                                selectInput('trans','Functions',multiple = FALSE, 
                                                            choices = c("Change time unit"='temp_mod','Coded variable based on quantiles'='set_quants'))),
                               
                               conditionalPanel(condition="input.VarCreateTop=='Nominal ID'",
                                                selectInput('ID','Functions', 
                                                            choices = c('Create distinct haul or trip ID'='ID_var','Create fishery season identifier'='create_seasonal_ID'),
                                                            multiple = FALSE)),
                               conditionalPanel(condition="input.VarCreateTop=='Arithmetic and temporal functions'",
                                                selectInput('numfunc','Functions',multiple = FALSE, 
                                                            choices = c('Numeric functions'='create_var_num',
                                                                        'Duration of time between two temporal variables'='create_duration', 
                                                                        'Catch per unit effort'='cpue'))),
                               conditionalPanel(condition="input.VarCreateTop=='Dummy variables'",
                                                selectInput('dummyfunc','Functions', multiple=FALSE,
                                                            choices=c('From variable', 'From policy dates', 'From area closures'))),
                               conditionalPanel(condition="input.VarCreateTop=='Spatial functions'",
                                                selectInput('dist','Functions',multiple = FALSE, 
                                                            choices = c('Distance between two points'='create_dist_between',
                                                                        'Midpoint location (lon/lat) for each haul'='create_mid_haul',
                                                                        'Zone when choice of where to go next was made'='create_startingloc'))),
                               conditionalPanel(condition="input.VarCreateTop=='Trip-level functions'",
                                                selectInput('trip','Functions',multiple = FALSE, 
                                                            choices = c('Collapse haul to trip'='haul_to_trip','Calculate trip distance'='trip_distance',
                                                                        'Calculate trip centroid'='trip_centroid'))),
                               conditionalPanel(condition="input.trip!='haul_to_trip'|input.trip!='trip_centroid'", 
                                                textInput('varname','Name of new variable', value='', placeholder = '')),
                               
                               #More sub choices Data Transformations     
                               uiOutput('trans_time_out'),
                               conditionalPanel(condition="input.VarCreateTop=='Data transformations'&input.trans=='temp_mod'",
                                                style = "margin-left:19px;", selectInput('define_format','Temporal units to return data in',choices=c('year', "month","day", 'hour', 'minute'))),
                               uiOutput('trans_quant_name'),
                               conditionalPanel(condition="input.VarCreateTop=='Data transformations'&input.trans=='set_quants'",
                                                style = "margin-left:19px;", selectInput('quant_cat','Quantile categories',
                                                                                         choices=c('0%, 20%, 40%, 60%, 80%, 100%'='0.2', '0%, 25%, 50%, 75%, 100%'='0.25', 
                                                                                                   '0%, 10%, 50%, 90%, 100%'='0.4'))),
                               #More sub choices Nominal IDS  
                               uiOutput('unique_col_id'),
                               conditionalPanel(condition="input.VarCreateTop=='Nominal ID'&input.ID=='create_seasonal_ID'",
                                                style = "margin-left:19px;", fileInput("seasonal.dat", "Choose data file containing data on fishery seasons",
                                                                                       multiple = FALSE)),
                               uiOutput('sp_col.select'),
                               conditionalPanel(condition="input.VarCreateTop=='Nominal ID'&input.ID=='create_seasonal_ID'",
                                                style = "margin-left:19px;", checkboxInput('sp_collocation', "Optional: Do fishery season dates depend on fishery location?", value=FALSE)),
                               conditionalPanel(condition="input.VarCreateTop=='Nominal ID'&input.ID=='create_seasonal_ID'",
                                                style = "margin-left:19px;", checkboxInput('sp_colgeartype', "Optional: Do fishery season dates depend on fishery location?", value=FALSE)),
                               conditionalPanel(condition="input.VarCreateTop=='Nominal ID'&input.ID=='create_seasonal_ID'",
                                                style = "margin-left:19px;", textInput('target', "Optional: Name of target species", value=NULL)),
                               
                               #More sub choices Arithmetic functions  
                               uiOutput('var_x_select'),
                               uiOutput('var_y_select'),
                               conditionalPanel(condition="input.VarCreateTop=='Arithmetic and temporal functions'&input.numfunc=='create_var_num'",
                                                style = "margin-left:19px;", selectInput('create_method', 'Arithmetic expression', choices=c('addition', 'subtraction', 'multiplication', 'division'))),
                               uiOutput('input_dur_start'),
                               uiOutput('input_dur_end'),
                               conditionalPanel(condition="input.VarCreateTop=='Arithmetic and temporal functions'&input.numfunc=='create_duration'",
                                                style = "margin-left:19px;", selectInput('dur_units', 'Unit of time for calculating duration', 
                                                                                         choices = c("week", "day", "hour", "minute"))),
                               uiOutput('input_xWeight'),
                               uiOutput('input_xTime'),
                               uiOutput('dur_add'),
                               
                               #dummy_var        Vector of TRUE or FALSE the length  (rows) of the data set. 
                               #dummy_matrix     Matrix with same dimensions at the data set filled with TRUE or FALSE.
                               #More sub choices for dummy functions
                               uiOutput('dummy_select'),
                               uiOutput('dummy_sub'),
                               
                               #More sub choices Spatial functions  
                               uiOutput('dist_between_input'),
                               uiOutput('dist_betwn_opts'),
                               conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_dist_between'",
                                                style = "margin-left:19px;", selectInput('units', 'Distance unit',choices = c('miles','meters','km'))),
                               uiOutput('start_mid_input'),
                               uiOutput('end_mid_input'),
                               uiOutput('input_startingloc'),
                               uiOutput('input_startingloc_extra'),
                               conditionalPanel(condition="input.VarCreateTop=='Trip-level functions'&input.trip=='haul_to_trip'",
                                                style = "margin-left:19px;", selectInput('fun_numeric','Numeric function to transform data', 
                                                                                         choices = c('min','mean','max','median','sum'), selected = 'mean')),
                               conditionalPanel(condition="input.VarCreateTop=='Trip-level functions'&input.trip=='haul_to_trip'",
                                                style = "margin-left:19px;", selectInput('fun_time','Numeric function to transform temporal data',
                                                                                         choices = c('min','mean','max','median'), selected = 'mean')),
                               uiOutput('input_IDVAR'),
                               uiOutput('input_trip_dist_vars'),
                               uiOutput('input_tri_cent'),
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
                  
                  
                  # Map Viewer ====
                  
                  tabPanel("Map Viewer",
                           
                           map_viewerUI("map")
                           
                           ),
                  
                  #----   
                  #Zonal definition tabset panel
                  #-----
                  tabPanel('Zonal Definition', value = "zone",
                           sidebarLayout(
                             sidebarPanel(
                               tags$button(
                                 id = 'closeZ',
                                 type = "button",
                                 style="color: #fff; background-color: #FF6347; border-color: #800000;",
                                 class = "btn action-button",
                                 onclick = "setTimeout(function(){window.close();},500);",  # close browser
                                 "Close app"
                               ),
                               actionButton("refreshZ", "Refresh data", 
                                            icon = icon("fa fa-refresh"),
                                            style = "color: white; background-color: blue;" 
                               ),
                               #runcodeUI (code='', type='ace'),
                               # actionButton("eval", "Evaluate"),
                               radioButtons('choiceTab', '', choices=c( #basic parameters to populate elsewhere like catch, price
                                                                       'Calculate zonal centroid and assign observations to zones'='zone', #calculate zonal centroid
                                                                       'Select variables that define alternative fishing choices'='distm',
                                                                       'Select catch and price variables'='primary')),#, #calculate distance matrix
                               #checkboxInput('ExpedCatch', 'Define variables to calculate expected catch', value=FALSE)
                               conditionalPanel(condition="input.choiceTab=='zone'",  
                                                actionButton('runCentroid','Assign observations to zones', style = "color: white; background-color: green;")),
                               conditionalPanel(condition="input.choiceTab=='distm'",
                                                actionButton('saveALT','Save choices', style = "color: white; background-color: green;")),
                               actionButton('callTextDownloadZone','Save notes'),
                               textInput('notesZone', "Notes", value=NULL, 
                                         placeholder = 'Write notes to store in text output file. Text can be inserted into report later.'),
                               ##Inline scripting 
                               textInput("exprZ", label = "Enter an R expression",
                                         value = "values$dataset"),
                               actionButton("runZ", "Run", class = "btn-success"),
                               div(style = "margin-top: 2em;",
                                   uiOutput('resultZ')
                               )
                             ),
                             mainPanel(
                               #BASELINE
                               verbatimTextOutput("output"),
                              
                               #--------#
                               #CENTROID
                               uiOutput('conditionalInput2'),
                               uiOutput('cond2'),
                               #runs assignment column and find_centroid functions
                               #--------#
                               #DISTANCE MATRIX
                               uiOutput('conditionalInput3a'),
                               uiOutput('conditionalInput3'),
                               div(style="display: inline-block;vertical-align:top; width: 500px;",
                                   conditionalPanel(condition="input.choiceTab=='distm'", plotOutput('zoneIDNumbers_plot'))),
                               div(style="display: inline-block;vertical-align:top; width: 160px;",
                                   conditionalPanel(condition="input.choiceTab=='distm'", textOutput('zoneIDText'))),
                               #--------# 
                               uiOutput('conditionalInput1')
                               #EXPECTED CATCH
                             )
                           )),
                  #-----
                  #Expected catch tabset panel
                  #----
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
                                            icon = icon("fa fa-refresh"),
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
                               
                               
                               h4('Temporal options'),
                               h5('Use the entire temporal record of catch or take the timeline of catch into account. 
                                  When timeline is considered, catch for a given day is the average for the defined number of days (window), 
                                  shifted to the past by the defined number of days (lag). For example, a window of 3 days and lag of 1 day means we take the 
                                  average catch of the three days priors to the given date.'),
                               div(style = "margin-left:19px;font-size: 12px", 
                                   selectInput('temporal', 'Method to sort time:', c('Entire record of catch (no time)', 'Daily timeline'='daily', 'Sequential order'='sequential'))),
                               uiOutput('expcatch'),
                               conditionalPanel(condition="input.temporal!='Entire record of catch (no time)'",
                                                style = "margin-left:19px;font-size: 12px",
                                                numericInput('temp_year', 'No. of years to go back if expected catch based on from previous year(s) catch ', value=0, min=0, max='')),
                               #if(input$temporal!='Entire record of catch (no time)') {h5('Moving window averaging parameters')},
                               conditionalPanel(condition="input.temporal!='Entire record of catch (no time)'",
                                                style = "margin-left:19px;font-size: 12px", 
                                                numericInput('temp_window', 'Window size (days) to average over', value = 7, min=0)),
                               conditionalPanel(condition="input.temporal!='Entire record of catch (no time)'", 
                                                style = "margin-left:19px;font-size: 12px", 
                                                numericInput('temp_lag', 'Time lag (in days) ', value = 0, min=0, max='')),
                               conditionalPanel(condition="input.temporal!='Entire record of catch (no time)'", 
                                                style = "margin-left:19px;font-size: 12px", 
                                                selectInput('calc_method','Expectation calculation:', 
                                                            choices = c("Standard average"="standardAverage", "Simple lag regression of means"="simpleLag"#, 
                                                                        #"Weights of regressed groups"="weights"
                                                            ))), 
                               conditionalPanel(condition="input.temporal!='Entire record of catch (no time)'", 
                                                selectInput('lag_method', 'Method to average across time steps', 
                                                            choices= c("Entire time period"="simple", "Grouped time periods"="grouped"))),
                               conditionalPanel(condition="input.temporal!='Entire record of catch (no time)'", 
                                  h4('Averaging options')),
                               conditionalPanel(condition="input.temporal!='Entire record of catch (no time)'", 
                                  div(style = "margin-left:19px; font-size: 12px", 
                                      selectInput('empty_catch', 'Replace empty catch with:', 
                                                  choices = c("NA: NA's removed when averaging"='NA', '0', 'Mean of all catch' ="allCatch", 'Mean of grouped catch' = "groupedCatch"))) 
                               ),#h6("Note: Na's removed when averaging"), 
                               h4('Expected Catch/Dummy options'), 
                               div(style = "margin-left:19px; font-size: 12px",
                                   selectInput('empty_expectation', 'Replace empty expected catch with:', choices = c("NA: NA's removed when averaging"='NA', 1e-04, 0))),  
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
                               tags$p('Compute expected catch for each observation and zone. 
                                      Function returns the expected catch or expected revenue data frame based on selected parameters along with three null functions: 
                                      expected catch/revenue based on catch of the previous two day (short-term expected catch),
                                      expected catch/revenue based on catch for the previous seven days (medium-term expected catch), and 
                                      expected catch/revenue based on catch in the previous year (long-term expected catch).
                                      Output saved in FishSET database. Previously saved expected catch/revenue output will be written over if the', 
                                      tags$i('Replace previously saved'), 'box is checked. Leaving the box unchecked will add new output to existing output.'),
                               tags$br(), tags$br(),
                               conditionalPanel(condition="input.temp_var!='none'",
                                                tagList(
                                     h4('Sparsity of observations by time period and zone.'),
                                     h5('Higher values indicate greater sparsity.'),
                                    DT::DTOutput('spars_table'),
                                    shinycssloaders::withSpinner(plotOutput('spars_plot'))
                                                ))
                             )
                             )),
                  #----
                  #Model design and Run tabset panel
                  #----
                   tabPanel("Models", value = "models",
                            tabsetPanel(
                                tabPanel("Run model(s)",
                           sidebarLayout(
                             sidebarPanel(
                               tags$button(
                                 id = 'close',
                                 type = "button",
                                 style="color: #fff; background-color: #FF6347; border-color: #800000;",
                                 class = "btn action-button",
                                 onclick = "setTimeout(function(){window.close();},500);",  # close browser
                                 "Close app"
                               ),
                               tags$br(),
                               actionButton("addModel", "Save model and add new model", style="color: #fff; background-color: #337ab7; border-color: #800000;"),
                               tags$br(),
                               conditionalPanel(condition='input.addModel>0',
                                   actionButton("submit", "Run model(s)", style="color: #fff; background-color: #6da363; border-color: #800000;")
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
                                   h3('Model parameters'),
                                   
                                   fluidRow(
                                     h4("Optimization options"),
                                     splitLayout(cellWidths = c("22%", "22%", "22%", "22%"),
                                                 numericInput("mIter", "max iterations", value = 100000),
                                                 numericInput("relTolX", "tolerance of x", value = 0.00000001),
                                                 numericInput("reportfreq", "report frequency", value = 1),
                                                 numericInput("detailreport", "detailed report", value = 1)
                                     )
                                   ),
                                   fluidRow(
                                     h4('Initial parameters'),
                                     uiOutput("Inits")
                                     #uiOutput("ui1")
                                   ),
                                   
                                   DT::DTOutput('mod_param_table')
                               )
                             ))),
                           tabPanel("Compare models",
                                    sidebarLayout(
                                      sidebarPanel(
                                        tags$br(),tags$br(),
                                        actionButton("delete_btn", "Delete row"),
                                        h3(''),
                                        actionButton("submit_ms", "Save table", style="color: #fff; background-color: #337ab7; border-color: #2e6da4;"),
                                        tags$br(),tags$br(),
                                        tags$button(
                                          id = 'close',
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
                  #---- 
                  #Bookmark tabset panel
                  #-----
                  tabPanel('Bookmark Choices', value = "book",
                           
                     tabsetPanel(id = "boomark_tab", 
                     
                     tabPanel("bookmark", value = "bookmark_page",
                           
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
                           tags$ul(tags$ul(tags$li('Click bookmark button and copy and save url address'))),
                           tags$ul(tags$ul(tags$li('Save url address in chosen location or in notes section of application'))),
                           tags$ul('Reopen saved state:'),
                           tags$ul(tags$ul(tags$li('Reopen the app'))),
                           tags$ul(tags$ul(tags$li('Open a tab in a web browser'))),
                           tags$ul(tags$ul(tags$li('Paste saved url in web browser. The saved application can then be used in the web browser.')))
                     ),
                     
                     tabPanel("log rerun", value = "log_rerun",
                              
                              sidebarLayout(
                                 
                                 sidebarPanel(
                                    
                                    tags$button(
                                       id = "rerun_close",
                                       type = "button",
                                       style="color: #fff; background-color: #FF6347; border-color: #800000;",
                                       class = "btn action-button",
                                       onclick = "setTimeout(function(){window.close();},500);",  # close browser
                                       "Close app"
                                    ),
                                    
                                    actionButton("run_log", "Rerun log",
                                                 style="color: #fff; background-color: #6da363; border-color: #800000;"),
                                    
                                    selectInput("log", "Select a log file", choices = list_logs()),
                                    
                                    checkboxInput("new_dat_cb", "Run log with different data table"),
                                    
                                    conditionalPanel("input.new_dat_cb",
                                                     
                                                     selectizeInput("new_dat", "Choose main table", 
                                                                    choices = list_MainDataTables(), multiple = TRUE,
                                                                    options = list(maxItems = 1)), # sets dat to NULL by default
                                                     
                                                     selectizeInput("new_port", "Choose port table", 
                                                                    choices = list_PortTables(), multiple = TRUE,
                                                                    options = list(maxItems = 1)),
                                                     
                                                     selectizeInput("new_aux", "Choose aux table", 
                                                                    choices = tables_database(), multiple = TRUE,
                                                                    options = list(maxItems = 1)),
                                                     
                                                     selectizeInput("new_grid", "Choose gridded table", 
                                                                    choices = tables_database(), multiple = TRUE,
                                                                    options = list(maxItems = 1)),
                                                     
                                                     selectizeInput("new_spat", "Choose spatial table", 
                                                                    choices = tables_database(), multiple = TRUE,
                                                                    options = list(maxItems = 1))
                                    ),
                                    
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
    