#2004	Central GOA Rockfish Cooperative
#2008 Amendment 80 Alaska
#1993	Pacific halibut and Sablefish IFQ Program	Alaska
#1999	American Fisheries Act (AFA) Pollock Program Alaska

    ## USER INTERFACE    
    ui = fluidPage(
 
      #---- 
      #Formatting
      #----
      tags$head(tags$style(HTML("
                                .multicol { 
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
                
                
                #  tags$style(HTML(
                #    ".my_div .select_int .select-input label{
                #                                            margin-left:17px;}")),
                
                tags$script(HTML('Shiny.addCustomMessageHandler("jsCode",
                                 function(message) {
                                 eval(message.value);
                                 });'))),

      #----
      tabsetPanel(id = "tabs",
                  #---
                  #Landing page
                  tabPanel("Information",
                           tabsetPanel(
                             tabPanel("Background",
                                      fluidRow(
                                        column(width = 6,
                                               tags$div(tags$p(tags$br(), 'Background on FishSET'),
                                                        tags$p('This page should be brief but visually appealing'),
                                                        tags$p('Page should quickly explain the tabs on the page'),
                                                        tags$p('Contact with questions'))
                                        )
                                      )
                             ),
                             tabPanel('Quickstart guide',
                                      tags$div(
                                        tags$br(),
                                        tags$p('Briefly describe how to use the app with some images'))
                             ),
                             tabPanel("AK catch and management data",
                                      fluidRow(
                                        column(width = 6,
                                               tags$div(tags$p(tags$br(), 'This page will have some information on AK catch and management data with some links and tables.')) 
                                        ) 
                                      )
                             ),
                             tabPanel("More information and links",
                                      fluidRow(
                                        column(width = 6,
                                               tags$div(tags$p(tags$br(),
                                                               'We can add more text here or not'),
                                                        tags$p('Relevant papers', tags$br(),
                                                               #tags$iframe(style="height:600px; width:100%", src=paste0(loc, 'inst/www/FleetProfiles2012.pdf'))
                                                               tags$a('Fleet Profile', href='FleetProfiles2012.pdf', target="_blank")
                                                        ),
                                                        tags$p('Some links', tags$br(),
                                                               tags$a('National Marine Fisheries Science Center (NMFS)', href='https://www.fisheries.noaa.gov/', target="_blank"), tags$br(),
                                                               tags$a('NOAA Office of Science and Technology Economics Program', href='https://www.st.nmfs.noaa.gov/economics/', target="_blank")
                                                        )
                                               )
                                        ) 
                                      )
                             ) 
                           )),
                  #---
                  #----
                  #UPLOAD DATA
                  #-----
                  tabPanel("Upload Data", value = "upload",
                           tags$style(type='text/css', "#uploadMain { width:100%; margin-top: 24px;margin-left:-20px;padding-left:2px; padding-right:5px}"),
                           
                           mainPanel(
                             tags$br(), tags$br(),
                             #div(style="display:inline-block;vertical-align:bottom;",
                             fluidRow(
                               column(6,
                                      fileInput("maindat", "Choose primary data file",
                                                multiple = FALSE, placeholder = 'Required data')),
                               column(3,
                                      uiOutput('ui.action')
                               )
                             ),
                             fluidRow( 
                               column(width = 8, offset = 2,
                                      uiOutput('ui.action2'))),
                             
                             fluidRow(
                               column(6,
                                      fileInput("portdat", "Choose port data file",
                                                multiple = FALSE, placeholder = 'Required data')),
                               column(3, uiOutput('ui.actionP'))
                             ),
                             fluidRow(
                               column(width=8, offset=2,
                                      uiOutput('ui.actionP2'))
                             ),
                             fluidRow( 
                               column(6,
                                      fileInput("griddat", "Choose data file that varies over two dimensions (gridded)",
                                                multiple = FALSE, placeholder = 'Optional data')),
                               column(3, uiOutput('ui.actionG'))
                             ),
                             fluidRow(
                               column(6, 
                                      fileInput("auxdat", "Choose auxiliary data file that links to primary data",
                                                multiple = FALSE, placeholder = 'Optional data')),
                               column(3, uiOutput('ui.actionA'))
                             ),
                             uiOutput("SaveButtonsUpload"),
                             textInput('notesUp', "Notes", value=NULL, placeholder = 'Write notes to store in text output file. Text can be inserted into report later.')
                           )),
                  #-----
                  #---- 
                  #BEGIN DATA EXPLORATION TABSET PANEL 
                  #----
                  tabPanel("Data Exploration", value = "explore",
                           sidebarLayout(
                             sidebarPanel(width=2,
                                          tags$br(),tags$br(),
                                          conditionalPanel(
                                            condition='input.plot_table=="Plots"',
                                            uiOutput('SaveButtonsExplore')),
                                          conditionalPanel(
                                            condition='input.plot_table=="Table"',
                                            actionButton('subsetData', 'Remove variable from data set')
                                          ),
                                          actionButton('saveData','Save data to fishset_db database'),
                                          
                                          tags$br(),tags$br(),
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
                                          selectInput('plot_table', 'View data table or plots', choices=c('Table','Plots'), selected='Table'),
                                          conditionalPanel(
                                            condition="input.plot_table=='Plots'",
                                            selectInput('plot_type', 'Select Plot Type', choices=c('Temporal','Spatial','x-y plot'))
                                          ),
                                          
                                          #selectInput('varofint', 'Variable of interest', choices=c('A','B','C'))
                                          conditionalPanel(condition="input.plot_table=='Plots'& input.plot_type=='Spatial'",
                                                           uiOutput("mtgt_output")),
                                          uiOutput('mtgt_out2'),
                                          conditionalPanel(
                                            condition='input.plot_table=="Table"',
                                            verbatimTextOutput('editText')
                                          ),
                                          
                                          conditionalPanel(
                                            condition='input.plot_table=="Plots" & input.plot_type=="Spatial"',
                                            uiOutput("location_info_spatial")
                                          ),
                                          textInput('notesExplore', "Notes", value=NULL, placeholder = 'Write notes to store in text output file. Text can be inserted into report later.')
                                          
                                          
                             ),
                             mainPanel(width=10,
                                       tags$div(DT::DTOutput("output_table_exploration"), style = "font-size: 75%; width: 100%"),
                                       conditionalPanel(
                                         condition='input.plot_table=="Plots" && input.plot_type=="Temporal"', 
                                         uiOutput('column_select')),
                                       conditionalPanel(
                                         condition='input.plot_table=="Plots" && input.plot_type=="x-y plot"',
                                         uiOutput('xy_select1')),
                                       conditionalPanel(
                                         condition='input.plot_table=="Plots" && input.plot_type=="x-y plot"',
                                         uiOutput('xy_select2')),
                                       conditionalPanel(
                                         condition='input.plot_table=="Plots" & input.plot_type=="Temporal"',
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
                                         condition='input.plot_table=="Plots" & input.plot_type=="Temporal"',
                                         plotOutput('plot_time')
                                       ),
                                       conditionalPanel(
                                         condition='input.plot_table=="Plots" & input.plot_type=="Spatial"',
                                         column(plotOutput('plot_spatial',
                                                           click = "plot_spatial_click",
                                                           dblclick = "plot_spatial_dblclick", 
                                                           brush = brushOpts(id = "plot_spatial_brush",resetOnNew = FALSE )), width=7)),
                                       conditionalPanel(
                                         condition='input.plot_table=="Plots" & input.plot_type=="Spatial"',
                                         column(plotOutput('map_kernel'), width=7)),
                                       conditionalPanel(
                                         condition='input.plot_table=="Plots" & input.plot_type=="Spatial"',
                                         column(DT::DTOutput('output_table_gt_mt'), width=6)),
                                       conditionalPanel(
                                         condition='input.plot_table=="Plots" && input.plot_type=="x-y plot"',   
                                         plotOutput('plot_xy'))
                             ))),
                  
                  
                  #----
                  #BEGIN DATA QUALITY EVALUATION TABSET PANEL  
                  #----
                  tabPanel("Data Quality Evaluation", value = "qaqc",
                           sidebarLayout(
                             sidebarPanel(width=3,
                                          tags$br(),tags$br(),
                                          uiOutput('SaveButtons'),
                                          actionButton('saveDataQ','Save data to fishset_db database'),
                                          tags$br(),tags$br(),
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
                                          h4('Select data validation check functions to run'),
                                          #Checkbox input widget  
                                          radioButtons("checks", "", choices = c('Summary table', 'Outliers', 'NAs', 'NaNs', 'Unique observations', 
                                                                                 'Empty variables', 'Lat_Lon units')),
                                          uiOutput('outlier_column'),
                                          uiOutput('outlier_subset'),
                                          uiOutput('outlier_dist'),
                                          conditionalPanel(
                                            condition = "input.checks == 'NAs'",
                                            radioButtons('NA_Filter', 'Filter NAs by', choices=c('none','Remove all','Replace with mean'), selected='none')
                                          ),
                                          conditionalPanel(
                                            condition = "input.checks == 'NaNs'",
                                            radioButtons('NAN_Filter', 'Filter NaNs by', choices=c('none','Remove all','Replace with mean'), selected='none')
                                          ),
                                          conditionalPanel(condition="input.checks=='Outliers'",
                                                           checkboxInput('Outlier_Filter', 'Remove outliers', value=FALSE)),
                                          conditionalPanel(
                                            condition ='input.checks=="Outliers"',
                                            uiOutput("hover_info1")),
                                          conditionalPanel(
                                            condition ='input.checks=="Unique observations"',
                                            checkboxInput('Unique_Filter', 'Remove non-unique rows', value=FALSE)
                                          ),
                                          conditionalPanel(
                                            condition ='input.checks=="Empty variables"',
                                            checkboxInput('Empty_Filter', 'Remove empty variables', value=FALSE)
                                          ),
                                          conditionalPanel(
                                            condition ='input.checks=="Lat_Lon units"',
                                            checkboxInput('LatLon_Filter', 'Convert lat/long to decimal degrees', value=FALSE)
                                          ),
                                          textInput('notesQAQC', "Notes", value=NULL, placeholder = 'Write notes to store in text output file. Text can be inserted into report later.')
                             ),#END SIDEBAR LAYOUT             
                             mainPanel(width=9,
                                       tags$br(), tags$br(),
                                       verbatimTextOutput("Case"),
                                       DT::DTOutput("output_table_summary"),
                                       DT::DTOutput("output_table_outlier"),
                                       tags$br(),tags$br(),
                                       splitLayout(cellWidths = c('33%','33%','33%'),
                                                   plotOutput('plot1',
                                                              hover = hoverOpts("plot1_hover", delay = 100, delayType = "debounce"),
                                                              dblclick = "plot1_dblclick", 
                                                              brush = brushOpts(id = "plot1_brush",resetOnNew = TRUE )
                                                   ),
                                                   plotOutput('plot2', dblclick = "plot2_dblclick", 
                                                              brush = brushOpts(id = "plot2_brush",resetOnNew = TRUE)),
                                                   plotOutput('plot3',
                                                              hover = hoverOpts("plot3_hover", delay = 100, delayType = "debounce"),
                                                              dblclick = "plot3_dblclick", 
                                                              brush = brushOpts(id = "plot3_brush",resetOnNew = TRUE)
                                                   )
                                       )))),
                  #---- 
                  #BEGIN BASIC ANALYSIS TABSET PANEL 
                  #----
                  tabPanel("Simple Analyses", value = "analysis",
                           sidebarLayout(
                             sidebarPanel(
                               uiOutput('SaveButtonsAnal'),
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
                               selectInput('corr_reg','Show correlations or simple linear regression', choices=c('Correlation','Regression'), selected='Correlation'),
                               textInput('notesAnal', "Notes", value=NULL, placeholder = 'Write notes to store in text output file. Text can be inserted into report later.')
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
                                   plotOutput('output_plot_corr', width='100%', height = "600px")
                                 )),
                               conditionalPanel(
                                 condition="input.corr_reg=='Regression'",
                                 tagList(
                                   uiOutput('reg_resp_out'),
                                   uiOutput('reg_exp_out'),
                                   verbatimTextOutput('output_text_reg'),
                                   plotOutput('output_plot_reg')
                                 ))  )
                           )),
                  
                  #----
                  #Create New variables
                  #----
                  tabPanel('Compute new variables', value='new',
                           sidebarLayout(
                             sidebarPanel(
                               uiOutput('SaveButtonsNew'),
                               actionButton('saveDataNew','Save data to fishset_db database'),
                               tags$br(),tags$br(),
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
                                            icon=icon('fa fa-run'),
                                            syle="color: white; background-color: green;"),
                               textInput('notesNew', "Notes", value=NULL, placeholder = 'Write notes to store in text output file. Text can be inserted into report later.'),
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
                                                            choices = c('Numeric functions'='create_var_num','Catch per unit effort'='cpue'))),
                               conditionalPanel(condition="input.VarCreateTop=='Dummy variables'",
                                                selectInput('dummyfunc','Functions', multiple=FALSE,
                                                            choices=c('From variable', 'From policy dates', 'From area closures'))),
                               conditionalPanel(condition="input.VarCreateTop=='Spatial functions'",
                                                selectInput('dist','Functions',multiple = FALSE, 
                                                            choices = c('Distance between two points'='create_dist_between',
                                                                        'Midpoint location (lon/lat) for each haul'='create_mid_haul',
                                                                        'Duration of time between two temporal variables'='create_duration',
                                                                        'Zone when choice of where to go next was made'='create_startingloc'))),
                               conditionalPanel(condition="input.VarCreateTop=='Trip-level functions'",
                                                selectInput('trip','Functions',multiple = FALSE, 
                                                            choices = c('Collapse haul to trip'='haul_to_trip','Calculate trip distance'='trip_distance',
                                                                        'Calculate trip centroid'='trip_centroid'))),
                               conditionalPanel(condition="input.VarCreateTop!='Trip-level functions'", 
                                                textInput('varname','Name of new variable', value='', placeholder = '')),
                               #dummy_var        Vector of TRUE or FALSE the length  (rows) of the data set. 
                               #dummy_matrix     Matrix with same dimensions at the data set filled with TRUE or FALSE.
                               
                               #More sub choices Data Transformations     
                               uiOutput('trans_time_out'),
                               conditionalPanel(condition="input.VarCreateTop=='Data transformations'&input.trans=='temp_mod'",
                                                style = "margin-left:19px;", selectInput('define.format','Temporal units to return data in',choices=c('year', "month","day", 'hour', 'minute'))),
                               uiOutput('trans_quant_name'),
                               conditionalPanel(condition="input.VarCreateTop=='Data transformations'&input.trans=='set_quants'",
                                                style = "margin-left:19px;", selectInput('quant.cat','Quantile categories',choices=c('0.2', '0.25', '0.4'))),
                               #More sub choices Nominal IDS  
                               uiOutput('unique_col_id'),
                               conditionalPanel(condition="input.VarCreateTop=='Nominal ID'&input.ID=='create_seasonal_ID'",
                                                style = "margin-left:19px;", fileInput("seasonal.dat", "Choose data file containing data on fishery seasons",
                                                                                       multiple = FALSE)),
                               uiOutput('sp.col.select'),
                               conditionalPanel(condition="input.VarCreateTop=='Nominal ID'&input.ID=='create_seasonal_ID'",
                                                style = "margin-left:19px;", checkboxInput('use.location', "Optional: Do fishery season dates depend on fishery location?", value=FALSE)),
                               conditionalPanel(condition="input.VarCreateTop=='Nominal ID'&input.ID=='create_seasonal_ID'",
                                                style = "margin-left:19px;", checkboxInput('use.geartype', "Optional: Do fishery season dates depend on fishery location?", value=FALSE)),
                               conditionalPanel(condition="input.VarCreateTop=='Nominal ID'&input.ID=='create_seasonal_ID'",
                                                style = "margin-left:19px;", textInput('target', "Optional: Name of target species", value=NULL)),
                               
                               #More sub choices Arithmetic functions  
                               uiOutput('var_x_select'),
                               uiOutput('var_y_select'),
                               conditionalPanel(condition="input.VarCreateTop=='Arithmetic and temporal functions'&input.numfunc=='create_var_num'",
                                                style = "margin-left:19px;", selectInput('create.method', 'Arithmetic expression', choices=c('addition', 'subtraction', 'multiplication', 'division'))),
                               uiOutput('input_xWeight'),
                               uiOutput('input_xTime'),
                               uiOutput('dur_add'),
                               
                               #More sub choices for dummy functions
                               uiOutput('dummy_select'),
                               uiOutput('dummy_sub'),
                               uiOutput('dum_num_sub'),
                               
                               #More sub choices Spatial functions  
                               uiOutput('dist_between_input'),
                               uiOutput('dist_betwn_opts'),
                               conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_dist_between'",
                                                style = "margin-left:19px;", selectInput('units', 'Distance unit',choices = c('miles','meters','km','midpoint'))),
                               uiOutput('start_mid_input'),
                               uiOutput('end_mid_input'),
                               uiOutput('input_dur_start'),
                               uiOutput('input_dur_end'),
                               conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_duration'",
                                                style = "margin-left:19px;", selectInput('dur.units', 'Unit of time for calculating duration', 
                                                                                         choices = c("week", "day", "hour", "minute"))),
                               uiOutput('input_startingloc'),
                               uiOutput('input_startingloc_extra'),
                               conditionalPanel(condition="input.VarCreateTop=='Trip-level functions'&input.trip=='haul_to_trip'",
                                                style = "margin-left:19px;", selectInput('fun.numeric','Numeric function to transform data', 
                                                                                         choices = c('min','mean','max','median','sum'), selected = 'mean')),
                               conditionalPanel(condition="input.VarCreateTop=='Trip-level functions'&input.trip=='haul_to_trip'",
                                                style = "margin-left:19px;", selectInput('fun.time','Numeric function to transform temporal data',
                                                                                         choices = c('min','mean','max','median'), selected = 'mean')),
                               uiOutput('input_IDVAR'),
                               uiOutput('input_trip_dist_vars'),
                               uiOutput('input_tri_cent')
                             ),
                             mainPanel(
                               DT::DTOutput("output_table_create")
                             )
                           ))
                  #----           
      ) )
    