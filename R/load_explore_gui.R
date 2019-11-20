#' Interactive tool to run FishSET functions 
#' Runs functions associated with loading data, exploring data, checking for data quality issues, generating new variables, and basic data analyis function.

#' load_explore
#'
#' @param dat Main data frame containing data on hauls or trips. Table in fishset_db database should contain the string `MainDataTable`.
#' @param project Name of project. Parameter is used to generate meaningful table names in fishset_db database.
#' @import shiny
#' @import ggplot2
#' @importFrom DT DTOutput renderDT
#' @importFrom ggpubr ggarrange
#' @importFrom ggcorrplot ggcorrplot
#' @importFrom DBI  dbDisconnect dbConnect dbListTables dbWriteTable 
# @importFrom gridExtra grid.table
#' @importFrom stringi stri_count_regex
#' @export load_explore
#' @details Opens an interactive page that allows users to select which functions to run by clicking check boxes. 
# @examples
# \dontrun{
# load_explore('pcodMainDataTable', 'pcod')
# }


load_explore <- function(dat, project){
  requireNamespace(shiny)
  requireNamespace(ggplot2)
#----
  #Helper functions
#----
  if(!exists('loc')){
    loc = getwd()
  } else {
    loc = loc
  }
 
  if(is.character(dat)==TRUE){
    suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), paste0(loc,"/fishset_db.sqlite")))
    dataset <- table_view(dat)
    DBI::dbDisconnect(fishset_db)
  } else {
    dataset <- dat  
  }
  
  if(is.character(dat)==TRUE){
    dat <- dat
  } else {
    dat <- deparse(substitute(dat))
  }
  # default global search value
  if (!exists("default_search")) {default_search <- ""}
  
  # default column search values
  if (!exists("default_search_columns")) {default_search_columns <- NULL}
  
  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  }
  
#----  
  
shinyApp(
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
                                 ")),
    tags$style(HTML(
      ".checkbox { 
                      margin-left: 0px;
                      margin-right: 10px;
                    }
                    .checkbox+.checkbox {
                      margin-left: 0px;
                      margin-right: 10px;
                    }
                    ")),

          tags$style(HTML("
                  .sidebar { height: 90vh; overflow-y: auto; }
                  .dataTables_wrapper { overflow-x: scroll; }
                  " )
  ),
  
  
#  tags$style(HTML(
#    ".my_div .select_int .select-input label{
#                                            margin-left:17px;}")),
  
  tags$script(HTML('Shiny.addCustomMessageHandler("jsCode",
                           function(message) {
                           eval(message.value);
                           });'))),

#----
tabsetPanel(id = "tabs",
#----
#Landing page
#----
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
        ) ),
    
### SERVER SIDE    
    server = function(input, output, session) {

##Pull data functions 
    values <- reactiveValues(dataset=dataset)
      # refresh data
    observeEvent(c(input$refresh,input$refresh1,input$refresh2,input$refreshNew), {
      suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), paste0(loc,"/fishset_db.sqlite")))
      values$dataset <- table_view(dat)
      DBI::dbDisconnect(fishset_db)
    }, ignoreInit = F) 
#    observeEvent(input$refresh1, {
#      suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), paste0(loc,"/fishset_db.sqlite")))
#      values$dataset <- table_view(dat)
#      DBI::dbDisconnect(fishset_db)
#    }, ignoreInit = F) 
 #   observeEvent(input$refresh2, {
 #     suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), paste0(loc,"/fishset_db.sqlite")))
 #     values$dataset <- table_view(dat)
#      DBI::dbDisconnect(fishset_db)
#    }, ignoreInit = F) 
    
##----     

#DATA UPLOAD FUNCTIONS
###----
    observeEvent(input$maindat, {
      type <- sub('.*\\.', '', input$maindat$name)
      if(type == 'shp') { type <- 'shape'} else if(type == 'RData') { type <- 'R'} else { type <- type}
      df_data <- FishSET::read_dat(input$maindat$datapath, type)
    }) 
    output$ui.action <- renderUI({
      if (is.null(input$maindat)) return()
      actionButton("uploadMain", label = "Save to database", 
                   style = "color: white; background-color: blue;", size = "extra-small")
    })
    output$ui.actionP <- renderUI({
      if (is.null(input$portdat)) return()
      actionButton("uploadPort", label = "Save to database", 
                   style = "color: white; background-color: blue;", size = "extra-small")
    })
    output$ui.actionG <- renderUI({
      if (is.null(input$griddat)) return()
      actionButton("uploadGrid", label = "Save to database", 
                   style = "color: white; background-color: blue;", size = "extra-small")
    })
    output$ui.actionA <- renderUI({
      if (is.null(input$auxdat)) return()
      actionButton("uploadAux", label = "Save to database", 
                   style = "color: white; background-color: blue;", size = "extra-small")
    })
    
    output$ui.action2 <- renderUI({
      if (is.null(input$maindat)) return()
      tagList(
      textInput('compare', label=div(style = "font-size:14px;  font-weight: 400;", 'If comparing data to previous year, enter saved table name'), 
                value='', placeholder = 'Saved table name in fishset_db database'),
      checkboxInput('over_write','If file exsits, over write?', value=FALSE)
      )
    })
    
    output$ui.actionP2 <- renderUI({
      if (is.null(input$portdat)) return()
      tagList(
        selectInput('port_name', "Enter column name containing port names", 
                    choices=names(FishSET::read_dat(input$portdat$datapath, if(sub('.*\\.', '', input$portdat$name) == 'shp') { 
            'shape'} else if(sub('.*\\.', '', input$portdat$name) == 'RData') { 
              'R'} else { sub('.*\\.', '', input$portdat$name)})), selected="")
         
       # ))#label=div(style = "font-size:14px;  font-weight: 400;", 'Enter column name containing port names'), 
                 # value='', placeholder = 'Column name')
      )
    })
    
    observeEvent(input$uploadMain, {
      type <- sub('.*\\.', '', input$maindat$name)
      if(type == 'shp') { type <- 'shape'} else if(type == 'RData') { type <- 'R'} else { type <- type}
      df_data <- FishSET::read_dat(input$maindat$datapath, type)
      df_y <- input$compare
      df_compare <- ifelse(nchar(input$compare)>0, TRUE, FALSE)
      load_maindata(df_data, over_write=input$over_write, project=project, compare=df_compare, y=df_y)
    })
    
    observeEvent(input$uploadPort, {
      type <- sub('.*\\.', '', input$portdat$name)
      if(type == 'shp') { type <- 'shape'} else if(type == 'RData') { type <- 'R'} else { type <- type}
      df_data <- FishSET::read_dat(input$portdat$datapath, type)
      load_port(df_data, input$port_name, over_write=TRUE, project=project, compare=FALSE, y=NULL)
    }) 
    
    observeEvent(input$uploadGrid, {
      type <- sub('.*\\.', '', input$griddat$name)
      if(type == 'shp') { type <- 'shape'} else if(type == 'RData') { type <- 'R'} else { type <- type}
      df_data <- FishSET::read_dat(input$griddat$datapath, type)
      load_grid(paste0(project, 'MainDataTable'), df_data, over_write=TRUE, project=project)
    }) 
    
    observeEvent(input$uploadAux, {
      type <- sub('.*\\.', '', input$auxdat$name)
      if(type == 'shp') { type <- 'shape'} else if(type == 'RData') { type <- 'R'} else { type <- type}
      df_data <- FishSET::read_dat(input$auxdat$datapath, type)
      load_aux(paste0(project, 'MainDataTable'), df_data, over_write=TRUE, project=project)
    }) 
    
 
###----
    
#DATA EXPLORATION FUNCTIONS
###----
#1. TABLE
    output$output_table_exploration <- DT::renderDT(
      if (input$plot_table=='Table') { 
        c1 <- values$dataset
        colnames(c1)=gsub("_","-", colnames(c1))
        return(c1)
      } else {
        NULL
      }, server = FALSE, editable=TRUE, filter='top', selection=list(target ='column'),
      extensions = c("Buttons"), rownames=FALSE,
      options = list(autoWidth=TRUE, scrolly=T, responsive=TRUE, pageLength = 15,
                     searchCols = default_search_columns, buttons = c('csv'))
    )
    observeEvent(c(input$saveData,input$saveDataQ),{
      # when it updates, save the search strings so they're not lost
      isolate({
        # update global search and column search strings
        default_search_columns <- c("", input$output_table_exploration_search_columns)
        default_sub <- which(default_search_columns!='')
       if(length(default_sub)==0){
          NULL
          } else {
       if (table_exists(paste0(project, "FilterTable")) == F) {
          FilterTable <- data.frame(dataframe = NA, vector = NA, FilterFunction = NA)
        } else {
          FilterTable <- table_view(paste0(project, "FilterTable"))
        }
        for(i in 1:length(default_sub)){
           if( grepl("\\..\\.", default_search_columns[default_sub[i]])==TRUE){
              FilterTable <- rbind(FilterTable, c(dat, (colnames(values$dataset[default_sub])[i]), 
                                                  paste(colnames(values$dataset[default_sub])[i], '>', as.numeric(sapply(strsplit(default_search_columns[default_sub[i]], "\\..\\."), head, 1)), '&', 
                                                  colnames(values$dataset[default_sub])[i], '<', as.numeric(sapply(strsplit(default_search_columns[default_sub[i]], "\\..\\."), tail, 1)))))
            } else {
              FilterTable <- rbind(FilterTable, c(dat, (colnames(values$dataset[default_sub])[i]), 
                                                 paste0("grepl('", default_search_columns[default_sub[i]],"', ", colnames(values$dataset[default_sub])[i],")")))
            }
        }

        fishset_db <- suppressWarnings(DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"))
        DBI::dbWriteTable(fishset_db, paste0(project, 'FilterTable'),  FilterTable, overwrite=TRUE)
        DBI::dbDisconnect(fishset_db)
          }      
      })
    })
    
    observeEvent(input$saveDataNew,{
      fishset_db <- suppressWarnings(DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"))
      DBI::dbWriteTable(fishset_db, paste0(project, 'FilterTable'),  FilterTable, overwrite=TRUE)
      DBI::dbDisconnect(fishset_db)
    })
      
      
    observeEvent(input$subsetData,{
      values$dataset <- values$dataset[,-(input$output_table_exploration_columns_selected+1)]
    })
    
    output$editText <- renderText('Edit cells: double click.\n\nFilter: Boxes at top.\nFilter functions saved to FilterTable in fishet_db database when "save data" button is pushed.\n\nRemove variables: Click on column cell then click "Remove Variable" button.\nVariables can be added back using the add_vars function.\n\nClick the "Save Data" button to save changes.')

    #Subset by columns
    
#2. Temporal PLOTS
    output$xy_select1 <- renderUI({
       selectInput('x_y_select1', 'Select x-axis variable', choices= names(which(lapply(values$dataset, is.numeric)==TRUE)), 
                   selected= names(which(lapply(values$dataset, is.numeric)==TRUE))[1], multiple=FALSE, selectize=TRUE)
    })
    output$xy_select2 <- renderUI({
      selectInput('x_y_select2', 'Select y-axis variable', choices= names(which(lapply(values$dataset, is.numeric)==TRUE)), 
                  selected= names(which(lapply(values$dataset, is.numeric)==TRUE))[2], multiple=FALSE, selectize=TRUE)
    })
    
    output$column_select <- renderUI({
      tags$div(align = 'left', class = 'multicol', 
                                radioButtons("col_select", "Select 1 variable", choices = names(values$dataset), 
                                                   selected = names(lapply(values$dataset, is.numeric)[1]), 
                                                   inline=FALSE))
    })

   t2 = reactive({
    if(length(unique(lubridate::year(date_parser(values$dataset[,grep('date', colnames(values$dataset), ignore.case = TRUE)[1]]))))>1){
        'Year'
    } else { 
        'Month'}
  })
  
   df2l=reactive({
     if(input$p2fun=='No. observations'){
        if(length(unique(lubridate::year(date_parser(values$dataset[,grep('date', colnames(values$dataset), ignore.case = TRUE)[1]]))))>1){
          aggregate(values$dataset[[input$col_select]]~
                lubridate::year(date_parser(values$dataset[,grep('date', colnames(values$dataset), ignore.case = TRUE)[1]])),FUN=length)
        } else {
          aggregate(values$dataset[[input$col_select]]~
                 lubridate::month(date_parser(values$dataset[,grep('date', colnames(values$dataset), ignore.case = TRUE)[1]])),FUN=length)
        }
     } else if(input$p2fun=='No. unique observations'){
        if(length(unique(lubridate::year(date_parser(values$dataset[,grep('date', colnames(values$dataset), ignore.case = TRUE)[1]]))))>1){
          aggregate(values$dataset[[input$col_select]]~
                     lubridate::year(date_parser(values$dataset[,grep('date', colnames(values$dataset), ignore.case = TRUE)[1]])),FUN=function(x) length(unique(x)))
        } else {
         aggregate(values$dataset[[input$col_select]]~
                     lubridate::month(date_parser(values$dataset[,grep('date', colnames(values$dataset), ignore.case = TRUE)[1]])),FUN=function(x) length(unique(x)))
         }
     } else {
        if(length(unique(lubridate::year(date_parser(values$dataset[,grep('date', colnames(values$dataset), ignore.case = TRUE)[1]]))))>1){
           aggregate(values$dataset[[input$col_select]]~
                     lubridate::year(date_parser(values$dataset[,grep('date', colnames(values$dataset), ignore.case = TRUE)[1]])),FUN=function(x) round(length(x)/nrow(values$dataset)*100,2))
        } else {
          aggregate(values$dataset[[input$col_select]]~
                     lubridate::month(date_parser(values$dataset[,grep('date', colnames(values$dataset), ignore.case = TRUE)[1]])),FUN=function(x) round(length(x)/nrow(values$dataset)*100,2))
        }
      }
    })
 
   df2m=reactive({
     if(length(unique(lubridate::year(date_parser(values$dataset[,grep('date', colnames(values$dataset), ignore.case = TRUE)[1]]))))>1){
     aggregate(values$dataset[[input$col_select]]~
                 lubridate::year(date_parser(values$dataset[,grep('date', colnames(values$dataset), ignore.case = TRUE)[1]])),FUN=input$p3fun, na.rm=T)
   } else {
     aggregate(values$dataset[[input$col_select]]~
                 lubridate::month(date_parser(values$dataset[,grep('date', colnames(values$dataset), ignore.case = TRUE)[1]])),FUN=input$p3fun,na.rm=T)
   }
   })
   

   plotInput_time <-  reactive({
      if(grepl('date', input$col_select[1], ignore.case=T)==TRUE){
        p1 <- ggplot(values$dataset, 
               aes_string(x=as.Date(values$dataset[,grep('date',  colnames(values$dataset), ignore.case = TRUE)[1]], origin='01-01-1970'),
                          y=as.Date(values$dataset[[input$col_select]], origin='01-01-1970'))) + geom_point()+
          labs(subtitle=paste(input$col_select, 'by Date'), x="Date", y=input$col_select) +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text=element_text(size=11),
                axis.title=element_text(size=11)) 
      } else {
        p1 <- ggplot(values$dataset, 
               aes_string(x=as.Date(values$dataset[,grep('date', colnames(values$dataset), ignore.case = TRUE)[1]], origin='01-01-1970'),
                          y=input$col_select)) + geom_point()+
          labs(subtitle=paste(input$col_select, 'by Date'), x="Date", y=input$col_select) +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text=element_text(size=11),
                axis.title=element_text(size=11))
      }
      p2 <- ggplot(df2l(), aes_string(x=df2l()[,1], y=df2l()[,2]))+ geom_bar(stat='identity')+
        labs(subtitle=paste(input$p2fun, 'by', tolower(t2())), x=t2(),y='')+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text=element_text(size=11),
              axis.title=element_text(size=11))
     if(!is.numeric(values$dataset[[input$col_select]])) {
         p3 <- NULL
      } else {
      p3 <- ggplot(df2m(), aes_string(x=df2m()[,1], y=df2m()[,2]))+ geom_bar(stat='identity')+
        labs(subtitle=paste(simpleCap(input$p3fun), 'of value by', tolower(t2())), x=t2(), y='')+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text=element_text(size=11),
              axis.title=element_text(size=11))
    } 
      if (is.null(values$dataset)) {
    return(NULL)
     } else {
   # if(input$plot_table=='Plots'&input$plot_type=='Temporal'){
   #  return(NULL)
   # } else {
      return(suppressWarnings(ggpubr::ggarrange(p1,p2,p3, ncol=3, nrow=1)))
    }
  #}
})

   output$plot_time <- renderPlot({
  print(plotInput_time())
})


#3. SPATIAL DISTRIBUTION
     ranges_spatial <- reactiveValues(x = NULL, y=NULL)
     observeEvent(input$plot_type,{
       ranges_spatial$x <- c(ifelse((min(values$dataset[, which(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)', ignore.case=TRUE)==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)', ignore.case=TRUE)))[1]], na.rm=TRUE)-abs(min(values$dataset[, which(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)', ignore.case=TRUE)==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)', ignore.case=TRUE)))[1]], na.rm=TRUE)/10) < -180), 
                                    -180, min(values$dataset[, which(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)', ignore.case=TRUE)==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)', ignore.case=TRUE)))[1]], na.rm=TRUE)-abs(min(values$dataset[, which(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)', ignore.case=TRUE)==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)', ignore.case=TRUE)))[1]], na.rm=TRUE)/10)), 
                             ifelse((max(values$dataset[, which(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)', ignore.case=TRUE)==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)', ignore.case=TRUE)))[1]], na.rm=TRUE)+abs(max(values$dataset[, which(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)', ignore.case=TRUE)==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)', ignore.case=TRUE)))[1]], na.rm=TRUE)/10) > 180), 
                                    180, max(values$dataset[, which(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)', ignore.case=TRUE)==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)', ignore.case=TRUE)))[1]], na.rm=TRUE)+abs(max(values$dataset[, which(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)', ignore.case=TRUE)==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)', ignore.case=TRUE)))[1]], na.rm=TRUE)/10))) 
       ranges_spatial$y <-c(ifelse((min(values$dataset[, which(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)', ignore.case=TRUE)==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)', ignore.case=TRUE)))[1]], na.rm=TRUE)-abs(min(values$dataset[, which(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)', ignore.case=TRUE)==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)', ignore.case=TRUE)))[1]], na.rm=TRUE)/10) < -90), 
                                   -90, min(values$dataset[, which(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)', ignore.case=TRUE)==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)', ignore.case=TRUE)))[1]], na.rm=TRUE)-abs(min(values$dataset[, which(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)', ignore.case=TRUE)==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)', ignore.case=TRUE)))[1]], na.rm=TRUE)/10)), 
                            ifelse((max(values$dataset[, which(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)', ignore.case=TRUE)==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)', ignore.case=TRUE)))[1]], na.rm=TRUE)+abs(max(values$dataset[, which(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)', ignore.case=TRUE)==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)', ignore.case=TRUE)))[1]], na.rm=TRUE)/10) > 90),
                                   90, max(values$dataset[, which(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)', ignore.case=TRUE)==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)', ignore.case=TRUE)))[1]], na.rm=TRUE)+abs(max(values$dataset[, which(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)', ignore.case=TRUE)==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)', ignore.case=TRUE)))[1]], na.rm=TRUE)/10)))
     })
     output$plot_spatial <- renderPlot({#plotInput_spatial <-  reactive({
       if (is.null(values$dataset)) {
         return(NULL)
       } else {
           longitude <- which(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)', ignore.case=TRUE)==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)', ignore.case=TRUE)))[1]
           latitude <- which(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)', ignore.case=TRUE)==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)', ignore.case=TRUE)))[1]
           cf <- coord_fixed()
           cf$default <- TRUE
            ggplot(data = map_data("world"), mapping = aes(x = long, y = lat, group=group)) + 
             geom_polygon(color = "black", fill = "gray") + 
             geom_point(data = values$dataset, aes(x = values$dataset[,longitude], y = values$dataset[,latitude], group=rep(1, nrow(values$dataset))), color = "red", size = 1) +
             cf + coord_fixed(xlim = ranges_spatial$x, ylim = ranges_spatial$y, ratio=1.3, expand = TRUE)+
             labs(x='Longitude', y='Latitude', subtitle='Observed locations')+
             theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   panel.background = element_blank(),  axis.text=element_text(size=12),
                   axis.title=element_text(size=12),panel.border = element_rect(colour = "black", fill=NA, size=1) )
         } 
     })
     plotInput_kernel <- reactive ({
           if (is.null(values$dataset)) {
            return(NULL)
          } else {
            if(input$plot_table=='Plots'&input$plot_type=='Spatial'){
                  map_kernel('gradient', values$dataset[,c(which(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)', ignore.case=TRUE)==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)', ignore.case=TRUE)))[1], 
                                                              which(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)', ignore.case=TRUE)==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)', ignore.case=TRUE)))[1])])
         } else {
           return(NULL)
         }
       }
         })
     output$map_kernel <- renderPlot({
       plotInput_kernel()
     })
     
     #Location info       
     output$location_info_spatial <- renderUI({
      hover <- input$plot_spatial_click
      if (is.null(hover)) return(NULL)
        # calculate point position INSIDE the image as percent of total dimensions
       # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
       top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
       
       # calculate distance from left and bottom side of the picture in pixels
       left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
       top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
       
       # create style property fot tooltip
       # background color is set so tooltip is a bit transparent
       # z-index is set so we are sure are tooltip will be on top
       style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                       "left:", (hover$range$right +(hover$range$right)/10), "px; top:", (hover$range$bottom+hover$range$bottom/10), "px;")
       
       # actual tooltip created as wellPanel
       wellPanel(
         style = style,
         p(HTML(paste0("<b> longitude: </b>", hover$x, "<br><b>  latitude: </b>", hover$y))
       ))
     })
     
     
     # When a double-click happens, check if there's a brush on the plot.
     # If so, zoom to the brush bounds; if not, reset the zoom.
     observeEvent(input$plot_spatial_dblclick, {
       brush <- input$plot_spatial_brush
      if (!is.null(brush)) {
         
         ranges_spatial$x <- c(brush$xmin, brush$xmax)
         ranges_spatial$y <- c(brush$ymin, brush$ymax)
         
       } else {
        longitude <- which(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)', ignore.case=TRUE)==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)', ignore.case=TRUE)))[1]
        latitude <- which(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)', ignore.case=TRUE)==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)', ignore.case=TRUE)))[1]
        ranges_spatial$x <- c(ifelse((min(values$dataset[, longitude], na.rm=TRUE)-abs(min(values$dataset[, longitude], na.rm=TRUE)/10) < -180), 
                                      -180, min(values$dataset[, longitude], na.rm=TRUE)-abs(min(values$dataset[, longitude], na.rm=TRUE)/10)), 
                               ifelse((max(values$dataset[, longitude], na.rm=TRUE)+abs(max(values$dataset[, longitude], na.rm=TRUE)/10) > 180), 
                                     180, max(values$dataset[, longitude], na.rm=TRUE)+abs(max(values$dataset[, longitude], na.rm=TRUE)/10)))
         ranges_spatial$y <- c(ifelse((min(values$dataset[, latitude], na.rm=TRUE)-abs(min(values$dataset[, latitude], na.rm=TRUE)/10) < -90), 
                                      -90, min(values$dataset[, latitude], na.rm=TRUE)-abs(min(values$dataset[, latitude], na.rm=TRUE)/10)), 
                               ifelse((max(values$dataset[, latitude], na.rm=TRUE)+abs(max(values$dataset[, latitude], na.rm=TRUE)/10) > 90), 
                                     90, max(values$dataset[, latitude], na.rm=TRUE)+abs(max(values$dataset[, latitude], na.rm=TRUE)/10)))
       }
     })
     
     output$mtgt_output <- renderUI({
              tagList( 
                conditionalPanel(condition="input.plot_table=='Plots'&input.plot_type=='Spatial'",
                                 style = "margin-left:19px;", selectizeInput('varofint', 'Variable to test for spatial autocorrelation',
                                                                          choices=colnames(values$dataset[,sapply(values$dataset,is.numeric)]))),
                conditionalPanel(condition="input.plot_table=='Plots'&input.plot_type=='Spatial'",
                                style = "margin-left:19px;",  
                                selectizeInput('gtmt_lonlat', 'Select lat then lon from data set to assign observations to zone', 
                                      choices=c(NULL, names(values$dataset)[grep('lon|lat', names(values$dataset), ignore.case=TRUE)]),
                                      multiple = TRUE, options = list(maxItems = 2))), 
                conditionalPanel(condition="input.plot_table=='Plots'&input.plot_type=='Spatial'",
                                         style = "margin-left:19px;", 
                                         fileInput("gtmtfileGrid", "Choose file defining area/zone polygons", multiple = FALSE)) 
       )
     })    
     gtmtGridFileData <- reactive({
       if(is.null(input$gtmtfileGrid)){return()} 
       type <- sub('.*\\.', '', input$gtmtfileGrid$name)
       if(type == 'shp') { type <- 'shape'} else if(type == 'RData') { type <- 'R'} else { type <- type}
       g <- read_dat(input$gtmtfileGrid$datapath, type)
       return(g)
     })
     output$mtgt_out2 <- renderUI({
        tagList(
          conditionalPanel(condition="input.plot_table=='Plots'&input.plot_type=='Spatial'",
                        style = "margin-left:19px;", selectInput('mtgtcat',  "Variable defining zones or areas", 
                                                                 choices= c('', names(as.data.frame(gtmtGridFileData()))), selected='')),
           conditionalPanel(condition="input.plot_table=='Plots'&input.plot_type=='Spatial'",
                        style = "margin-left:19px;", selectizeInput('mtgtlonlat', 'Select vector containing latitude then longitude from spatial data set', 
                                                     choices= c(NULL, names(as.data.frame(gtmtGridFileData()))), multiple=TRUE, options = list(maxItems = 2)))
        )
      })
    gtmt_table <- reactive({
      if(input$mtgtcat==''){
        return( NULL)
      } else {
        gt <- getis_ord_stats(values$dataset, input$varofint, gtmtGridFileData(), lon.dat=input$gtmt_lonlat[2], lat.dat=input$gtmt_lonlat[1], cat=input$mtgtcat, lon.grid=input$mtgtlonlat[2], lat.grid=input$mtgtlonlat[1])$getistable
        mt <- moran_stats(values$dataset, input$varofint, gtmtGridFileData(), lon.dat=input$gtmt_lonlat[2], lat.dat=input$gtmt_lonlat[1], cat=input$mtgtcat, lon.grid=input$mtgtlonlat[2], lat.grid=input$mtgtlonlat[1])$morantable
        print(gt)
               return(as.data.frame(merge(gt, mt)))
      }
      }) 
  
  output$output_table_gt_mt <- DT::renderDT(  
    gtmt_table()
  )

     
     #4. X VS. Y
     plotInput_xy <- reactive({
             if (is.null(values$dataset)) {
         return(NULL)
       }  else {
           ggplot(values$dataset, aes_string(x=values$dataset[[input$x_y_select1]],y=values$dataset[[input$x_y_select2]])) + geom_point()+
             labs(subtitle=paste(input$x_y_select1, 'by', input$x_y_select2), x=input$x_y_select1, y=input$x_y_select2) +
             theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text=element_text(size=11),
                   axis.title=element_text(size=11))
         } 
     })
     output$plot_xy <- renderPlot({
       print(plotInput_xy())
     })
     
     
      ##
###----    
    
#DATA ANALYSIS FUNCTIONS
###----
     output$corr_out <- renderUI({
       selectInput('corr_select', 'Select variables to include in correlation test', choices= names(which(lapply(values$dataset, is.numeric)==TRUE)), 
                   selected= names(which(lapply(values$dataset, is.numeric)==TRUE)), multiple=TRUE, selectize=TRUE, width='90%')
     })
     
     
     tableInputCorr <- reactive({
       if(length(input$corr_select)>2){
               c1 <- round(cor(values$dataset[,input$corr_select], use="complete.obs"), 2)
               colnames(c1)=gsub("_","-", colnames(c1))
               return(c1)
         } else {
                NULL
          } 
     })
      output$output_table_corr <- DT::renderDT(
        tableInputCorr(),  server=FALSE, extensions = list('Scroller'), 
        options=list(autoWidth = TRUE, scrollX=TRUE, deferRender = T,
                     scrollY = 'auto', scroller = TRUE, scrollX = T, pageLength = 25)
     )
     output$output_text_corr <- renderPrint(
       if(length(input$corr_select)==2){
         cor.test(values$dataset[[input$corr_select[1]]], values$dataset[[input$corr_select[2]]])
       }# else if(length(input$corr_select)>2){
       #  return(NULL)
      # }
     )
     
     plotInputcorr <- reactive({
       if(length(input$corr_select)==2){
         ggplot(values$dataset, aes_string(x=values$dataset[[input$corr_select[1]]], y=values$dataset[[input$corr_select[2]]])) + geom_point()+
           geom_smooth(method=lm)+labs(subtitle=paste(input$corr_select[1], 'by', input$corr_select[2]),x=input$corr_select[1],y=input$corr_select[2])+
           theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                 panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text=element_text(size=11),
                 axis.title=element_text(size=11))
       } else if(length(input$corr_select)>2){
         ggcorrplot::ggcorrplot(round(cor(values$dataset[,input$corr_select], use="complete.obs"), 2), 
                                type='lower',outline.color = 'white', hc.order=TRUE,show.diag=TRUE,
                                title = paste("Correlation matrix plot for", project, "data"),
                                ggtheme=ggplot2::theme_minimal())
       } 
     })
     output$output_plot_corr <- renderPlot({
       plotInputcorr()
     })
     
     output$reg_resp_out <- renderUI({
       selectInput('reg_resp_select', 'Select response variable', choices= names(values$dataset), 
                   selected= names(which(lapply(values$dataset, is.numeric)==TRUE))[1], multiple=FALSE, selectize=TRUE)
     })

     output$reg_exp_out <- renderUI({
       selectInput('reg_exp_select', 'Select explanatory variable(s)', choices= names(values$dataset), 
                   selected= "", multiple=FALSE, selectize=TRUE)
     })
     
     ## Add regression component
     #Run model
     output$output_text_reg <- renderPrint(
           summary(lm(values$dataset[[input$reg_resp_select]]~values$dataset[,input$reg_exp_select]))
     )
    
     plotInputreg <- reactive({
       if(length(input$reg_exp_select)!=1){
         return(NULL)
       } else {
         ggpubr::annotate_figure(ggpubr::ggarrange(ggplot(values$dataset, aes_string(x=input$reg_exp_select, y=input$reg_resp_select)) + geom_point()+
           geom_smooth(method=lm)+
             labs(subtitle=paste(input$reg_resp_select, 'against', input$reg_exp_select), x=input$reg_exp_select, y=input$reg_resp_select)+
           theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                 panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text=element_text(size=11),
                 axis.title=element_text(size=11)),
          ggplot(lm(values$dataset[[input$reg_resp_select]]~values$dataset[[input$reg_exp_select]])) + 
                geom_point(aes(x=.fitted, y=.resid)) + 
                labs(subtitle = 'Residuals against fitted values', x='Fitted',y='Residuals')+
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                    panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text=element_text(size=11),
                      axis.title=element_text(size=11)),
          ncol=2, nrow=1), top=ggpubr::text_grob('Simple linear regression plots', size=14))
       }
     })
  
      output$output_plot_reg <- renderPlot({
        print(plotInputreg())
      })    
###----

#DATA CREATION/MODIFICATION FUNCTIONS
###----
      #Transformations 
      output$trans_time_out <- renderUI({
                 conditionalPanel(condition="input.VarCreateTop=='Data transformations'&input.trans=='temp_mod'",
                                  style = "margin-left:19px;", selectInput('TimeVar','Select variable',
                                   choices=c(colnames(values$dataset)[grep('date|hour|time|year', colnames(values$dataset), ignore.case=TRUE)]), selectize=TRUE))
      })
      output$trans_quant_name <-  renderUI({
                  conditionalPanel(condition="input.VarCreateTop=='Data transformations'&input.trans=='set_quants'",
                                   style = "margin-left:19px;", selectInput('trans_var_name','Select variable', 
                                                               choices=names(values$dataset[,unlist(lapply(values$dataset, is.numeric))]), multiple=FALSE, selectize=TRUE))
      })
      output$unique_col_id <- renderUI({
                  conditionalPanel(condition="input.VarCreateTop=='Nominal ID'&input.ID=='ID_var'",
                                   style = "margin-left:19px;", selectInput('unique_identifier','Variables that identify unique observations',
                                                                    choices=colnames(values$dataset), multiple=TRUE, selectize=TRUE))
      })
      seasonalData <- reactive({
        if(is.null(input$seasonal.dat)){return()} 
        type <- sub('.*\\.', '', input$seasonal.dat$name)
        if(type == 'shp') { type <- 'shape'} else if(type == 'RData') { type <- 'R'} else { type <- type}
        g <- read_dat(input$seasonal.dat$datapath, type)
        return(g)
      })
      output$sp.col.select<- renderUI({
        conditionalPanel(condition="input.VarCreateTop=='Nominal ID'&input.ID=='create_seasonal_ID'",
                         style = "margin-left:19px;",  selectInput('sp.col', "Column containing species names in table containing seasonal data", 
                                                             choices=names(seasonalData())
                                                             , multiple = FALSE, selectize=TRUE))
        })
      output$var_x_select <- renderUI({
            conditionalPanel(condition="input.VarCreateTop=='Arithmetic and temporal functions'&input.numfunc=='create_var_num'",
                             style = "margin-left:19px;", selectInput('var_x', 'First variable. Will be the numerator if dividing.', 
                                      choices=names(values$dataset[,unlist(lapply(values$dataset, is.numeric))]), selectize=TRUE))
        })
      output$var_y_select <- renderUI({
            conditionalPanel(condition="input.VarCreateTop=='Arithmetic and temporal functions'&input.numfunc=='create_var_num'",
                             style = "margin-left:19px;", selectInput('var_y', 'Second variable. Will be the denomenator if dividing.',  
                                      choices=names(values$dataset[,unlist(lapply(values$dataset, is.numeric))]), selectize=TRUE))
        })
      output$input_xWeight <- renderUI({
                    conditionalPanel(condition="input.VarCreateTop=='Arithmetic and temporal functions'&input.numfunc=='cpue'",
                                     style = "margin-left:19px;", selectInput('xWeight','Weight variable', 
                                   choices=names(values$dataset[,grep("mt|lb|ton|pound|weight|metric|kilo|mass", names(values$dataset), ignore.case = TRUE)]), selectize=TRUE))
      })
      output$input_xTime <- renderUI({
                  conditionalPanel(condition="input.VarCreateTop=='Arithmetic and temporal functions'&input.numfunc=='cpue'",
                                   style = "margin-left:19px;", selectInput('xTime','Time variable. Must be weeks, days, hours, or minutes', 
                                   choices=c('Calculate duration', names(values$dataset[,unlist(lapply(values$dataset, is.numeric))])), selectize=TRUE))
      })
      output$dur_add <- renderUI({
             tagList(
               conditionalPanel(condition="input.VarCreateTop=='Arithmetic and temporal functions'&input.numfunc=='cpue'&input.xTime=='Calculate duration'",
                                style = "margin-left:19px;", selectInput('dur.start2', 'Variable indicating start of time period', 
                                     choices = names(values$dataset[,grep("date|min|hour|week|month|TRIP_START|TRIP_END", names(values$dataset), ignore.case = TRUE)]), selectize=TRUE)),
               conditionalPanel(condition="input.VarCreateTop=='Arithmetic and temporal functions'&input.numfunc=='cpue'&input.xTime=='Calculate duration'",         
                                style = "margin-left:19px;", selectInput('dur.end2', 'Variable indicating end of time period', 
                                                 choices = names(values$dataset[,grep("date|min|hour|week|month|TRIP_START|TRIP_END", names(values$dataset), ignore.case = TRUE)]), selectize=TRUE)),
               conditionalPanel(condition="input.VarCreateTop=='Arithmetic and temporal functions'&input.numfunc=='cpue'&input.xTime=='Calculate duration'",          
                                style = "margin-left:19px;", selectInput('dur.units2', 'Unit of time for calculating duration', choices = c("week", "day", "hour", "minute")))
              )
      })
      output$dist_between_input <- renderUI({
        tagList(
          conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_dist_between'",
                           style = "margin-left:19px;", selectInput('start', 'Starting location',choices = c('Zonal centroid', 'Port', 'Lat/lon coordinates'))),
          conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_dist_between'",
                           style = "margin-left:19px;", selectInput('end', 'Ending location', choices = c('Zonal centroid', 'Port', 'Lat/lon coordinates'))),
      #Port
              conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_dist_between'&input.start=='Port'||
                           input.VarCreateTop=='Spatial functions'&input.dist=='create_dist_between'&input.end=='Port'" ,
                               style = "margin-left:19px;", selectInput("filePort", "Choose file from FishSET SQL database containing port data", 
                                       choices=tables_database()[grep('port', tables_database(), ignore.case=TRUE)], multiple = FALSE)),
      #Gridded dat
        conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_dist_between'&input.start=='Zonal centroid'||
                         input.VarCreateTop=='Spatial functions'&input.dist=='create_dist_between'&input.end=='Zonal centroid'",
                         style = "margin-left:19px;", fileInput("fileGrid", "Choose file defining area/zone polygons", multiple = FALSE)),   
        
      #port
          conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_dist_between'&input.start=='Port'",
                           style = "margin-left:19px;", selectInput('port.start', 'Variable containing port name at starting location', 
                                       choices=names(values$dataset[,grep('port', names(values$dataset), ignore.case=T)]), selectize=TRUE)),
          conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_dist_between'&input.end=='Port'" ,
                           style = "margin-left:19px;", selectInput('port.end', 'Variable containing port name at ending location', 
                                                                    choices=names(values$dataset[,grep('port', names(values$dataset), ignore.case=T)]), selectize=TRUE)),
                         # fileInput("filePort", "Choose file containing port data",    
      #Zonal
         #coords
          conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_dist_between'&input.start=='Lat/lon coordinates'" ,
                           style = "margin-left:19px;", selectizeInput('start.latlon', 'Select lat then lon for starting location', 
                                       choices=names(values$dataset[,grep('lat|lon', names(values$dataset), ignore.case=T)]), multiple=TRUE), options = list(maxItems = 2)),
          conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_dist_between'&input.end=='Lat/lon coordinates'" ,
                           style = "margin-left:19px;", selectizeInput('end.latlon', 'Select lat then lon for ending location', 
                                   choices=names(values$dataset[,grep('lat|lon', names(values$dataset), ignore.case=T)]), multiple=TRUE), options = list(maxItems = 2))
          

        )#)  
           })   
      output$dist_betwn_opts <- renderUI({
      tagList(
          conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_dist_between'&input.start=='Zonal centroid'||
                            input.VarCreateTop=='Spatial functions'&input.dist=='create_dist_between'&input.end=='Zonal centroid'" ,
                           style = "margin-left:19px;", selectInput('cat', 'Individual areas/zones from the spatial data set', choices=names(as.data.frame(griddata())))),
           if(any(class(griddata())=='sf')==FALSE){
                   conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_dist_between'&input.start=='Zonal centroid'||
                            input.VarCreateTop=='Spatial functions'&input.dist=='create_dist_between'&input.end=='Zonal centroid'" , 
                                    style = "margin-left:19px;", selectizeInput('long.grid', 'Select vector containing latitude then longitude from spatial data set',
                                                                             choices=names(values$dataset), multiple=TRUE), options = list(maxItems = 2))
          },
          conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_dist_between'&input.start=='Zonal centroid'||
                            input.VarCreateTop=='Spatial functions'&input.dist=='create_dist_between'&input.end=='Zonal centroid'" ,
                           style = "margin-left:19px;",  selectizeInput('lon.dat', 'Select lat then lon from data set to assign observations to zone', 
                                   choices=names(values$dataset[,grep('lat|lon', names(values$dataset), ignore.case=T)]), multiple=TRUE), options = list(maxItems = 2))
      )
    })
      griddata <- reactive({
        if(is.null(input$fileGrid)){return()} 
        type <- sub('.*\\.', '', input$fileGrid$name)
        if(type == 'shp') { type <- 'shape'} else if(type == 'RData') { type <- 'R'} else { type <- type}
        g <- read_dat(input$fileGrid$datapath, type)
        return(g)
      })
      output$start_mid_input <- renderUI({
        conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&&input.dist=='create_mid_haul'",
                         style = "margin-left:19px;", selectizeInput('mid.start','Select Lat then Lat that define starting locations',multiple = TRUE,
                                     choices = names(values$dataset[,grep('lat|lon', names(values$dataset), ignore.case=TRUE)]), options = list(maxItems = 2)))
      })
      output$end_mid_input <- renderUI({
        conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_mid_haul'",
                         style = "margin-left:19px;",  selectizeInput('mid.end','Select Lon then Lat that define ending locations',multiple = TRUE,
                                     choices = names(values$dataset[,grep('lat|lon', names(values$dataset), ignore.case=TRUE)]), options = list(maxItems = 2)))
      })
      output$input_dur_start <- renderUI({
        conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_duration'",
                         style = "margin-left:19px;", selectizeInput('dur.start', 'Variable indicating start of time period', 
                                               choices = names(values$dataset[,grep("date|min|hour|week|month|TRIP_START|TRIP_END", names(values$dataset), ignore.case = TRUE)])))
        })
      output$input_dur_end <- renderUI({
        conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_duration'",
                         style = "margin-left:19px;", selectInput('dur.end', 'Variable indicating end of time period', 
                                                choices = names(values$dataset[,grep("date|min|hour|week|month|TRIP_START|TRIP_END", names(values$dataset), ignore.case = TRUE)]), selectize=TRUE))
        })
      GridFileData <- reactive({
        if(is.null(input$grid.dat)){return()} 
        type <- sub('.*\\.', '', input$grid.dat$name)
        if(type == 'shp') { type <- 'shape'} else if(type == 'RData') { type <- 'R'} else { type <- type}
        g <- read_dat(input$grid.dat$datapath, type)
        return(g)
      })
      output$input_startingloc <- renderUI({
       tagList(
          conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_startingloc'",
                           style = "margin-left:19px;", selectInput('trip_id_SL', 'Variable in primary data set to identify unique trips', choices=c('',names(values$dataset)), selectize=TRUE)),
          conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_startingloc'",
                           style = "margin-left:19px;", selectInput('haul_order_SL', 'Variable in primary data set defining haul order within a trip. Can be time, coded variable, etc.',
                                   choices=c('', names(values$dataset)), selectize=TRUE)),
          conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_startingloc'",
                           style = "margin-left:19px;", selectInput('starting_port_SL',  "Variable in primary data set identifying port at start of trip", 
                                   choices=names(values$dataset[,grep('port',names(values$dataset), ignore.case = TRUE)]), selectize=TRUE)),
          conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_startingloc'",
                           style = "margin-left:19px;", selectInput('lon.dat_SL', "Longitude variable in primary data set", 
                                   choices= names(values$dataset[,grep("lon", names(values$dataset), ignore.case = TRUE)]), selectize=TRUE)), 
          conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_startingloc'",
                           style = "margin-left:19px;", selectInput('lat.dat_SL', "Latitude variable in primary data set", 
                                   choices= names(values$dataset[,grep("lat", names(values$dataset), ignore.case = TRUE)]), selectize=TRUE)),
          conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_startingloc'",
                           style = "margin-left:19px;",  selectInput("port.dat", "Choose file from FishSET SQL database containing port data", 
                                   choices=tables_database()[grep('port', tables_database(), ignore.case=TRUE)], multiple = FALSE)),
          conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_startingloc'",
                           style = "margin-left:19px;", fileInput("grid.dat", "Choose data file containing map shapefile (shape, json, and csv formats are supported)",
                                 multiple = FALSE, placeholder = ''))
          )})
      output$input_startingloc_extra <- renderUI({
           tagList(
         if(any(class(GridFileData())=='sf')==FALSE){
            conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_startingloc'",
                             style = "margin-left:19px;", selectInput('lat.grid_SL', 'Select vector containing latitude from spatial data set', choices= names(as.data.frame(GridFileData())), multiple=TRUE))
          },
          if(any(class(GridFileData())=='sf')==FALSE){
            conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_startingloc'",
                             style = "margin-left:19px;", selectInput('lon.grid_SL', 'Select vector containing longitude from spatial data set', 
                                                                      choices= names(as.data.frame(GridFileData())), multiple=TRUE, selectize=TRUE))
          },
          conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_startingloc'",
                           style = "margin-left:19px;", selectInput('cat_SL', "Variable defining zones or areas", choices= names(as.data.frame(GridFileData())), selectize=TRUE))
       )
     })
      output$input_IDVAR <- renderUI({
        conditionalPanel(condition="input.VarCreateTop=='Trip-level functions'&input.trip=='haul_to_trip'",
                         style = "margin-left:19px;", selectInput("Haul_Trip_IDVar", "Variable(s) that define unique trips", choices=names(values$dataset), multiple=TRUE, selectize=TRUE))
      })
      output$input_trip_dist_vars <- renderUI({
        tagList(
          conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_dist_between'&input.start=='Port'||
                           input.VarCreateTop=='Spatial functions'&input.dist=='create_dist_between'&input.end=='Port'" ,
                           style = "margin-left:19px;", selectInput("port.dat.dist", "Choose file from FishSET SQL database containing port data", 
                                       choices=tables_database()[grep('port', tables_database(), ignore.case=TRUE)], multiple = FALSE)),
          #
          conditionalPanel(condition="input.VarCreateTop=='Trip-level functions'&input.trip=='trip_distance'",
                           style = "margin-left:19px;", selectInput('trip_ID','Variable in data file to identify unique trips', multiple = FALSE, 
                                   choices = names(values$dataset), selectize=TRUE)),
          conditionalPanel(condition="input.VarCreateTop=='Trip-level functions'&input.trip=='trip_distance'",
                           style = "margin-left:19px;", selectInput('starting_port','Variable in data file to identify port at start of trip',multiple = FALSE, 
                                   choices = names(values$dataset)[grep('port', names(values$dataset), ignore.case=TRUE)], selectize=TRUE)),
          conditionalPanel(condition="input.VarCreateTop=='Trip-level functions'&input.trip=='trip_distance'",
                           style = "margin-left:19px;", selectizeInput('starting_haul','Select variables containing lat then long at START of haul',multiple = TRUE, 
                                   choices = names(values$dataset)[grep('lat|long', names(values$dataset), ignore.case=TRUE)], options = list(maxItems = 2))),
          conditionalPanel(condition="input.VarCreateTop=='Trip-level functions'&input.trip=='trip_distance'",
                           style = "margin-left:19px;", selectizeInput('ending_haul','Select variables containing lat then long at END of haul',multiple = TRUE, 
                                   choices = names(values$dataset)[grep('lat|long', names(values$dataset), ignore.case=TRUE)], options = list(maxItems = 2))),
          conditionalPanel(condition="input.VarCreateTop=='Trip-level functions'&input.trip=='trip_distance'",
                           style = "margin-left:19px;", selectizeInput('ending_port','Variable in data file to identify port at end of trip',multiple = FALSE, 
                                   choices = names(values$dataset)[grep('port', names(values$dataset), ignore.case=TRUE)])),
          conditionalPanel(condition="input.VarCreateTop=='Trip-level functions'&input.trip=='trip_distance'",
                           style = "margin-left:19px;", selectizeInput('haul_order','Variable in data file containing information on the order that hauls occur within a trip.',
                                   multiple = FALSE, choices = names(values$dataset)))
        )
      })
      output$input_tri_cent <-  renderUI({
        tagList(
        conditionalPanel(condition="input.VarCreateTop=='Trip-level functions'&input.trip=='trip_centroid'",
                         style = "margin-left:19px;", selectInput('trip_cent_lon','Vector containing longitudinal data', 
                                     choices =names(values$dataset)[grep('lon|lat', names(values$dataset), ignore.case=TRUE)], multiple = FALSE, selectize=TRUE)),
        conditionalPanel(condition="input.VarCreateTop=='Trip-level functions'&input.trip=='trip_centroid'",
                         style = "margin-left:19px;", selectInput('trip_cent_lat', 'Vector containing latitudinal data', 
                                     choices =names(values$dataset)[grep('lon|lat', names(values$dataset), ignore.case=TRUE)], multiple = FALSE, selectize=TRUE)),
        conditionalPanel(condition="input.VarCreateTop=='Trip-level functions'&input.trip=='trip_centroid'",
                         style = "margin-left:19px;", selectInput('trip_cent_weight','Variable for weighted average', multiple = FALSE, 
                                     choices=c('', names(values$dataset)), selected='', selectize=TRUE)),
        conditionalPanel(condition="input.VarCreateTop=='Trip-level functions'&input.trip=='trip_centroid'",
                         style = "margin-left:19px;", selectInput('trip_cent_id','Column(s) that identify the individual trip', multiple = TRUE, 
                                     choices = names(values$dataset), selectize=TRUE))
      )
      })
      output$dummy_select <- renderUI({
        tagList(
          conditionalPanel(condition="input.VarCreateTop=='Dummy variables'&input.dummyfunc=='From variable'",
                           style = "margin-left:19px;",selectInput('dummyvarfunc','Select variable', multiple=FALSE, choices=c(NULL, names(values$dataset)), selectize=TRUE)),
          conditionalPanel(condition="input.VarCreateTop=='Dummy variables'&input.dummyfunc=='From policy dates'",
                           style = "margin-left:19px;",selectInput('dummypolyfunc','Select variable', multiple=FALSE, choices=c())),
          conditionalPanel(condition="input.VarCreateTop=='Dummy variables'&input.dummyfunc=='From area closures'",
                           style = "margin-left:19px;", selectInput('dummclosfunc','Select Variable', multiple=FALSE, choices=c()))
        )
      })                           
      dum_temp <- reactive({
        if(is.null(input$dummyvarfunc)){return()} 
        if(grepl('dat|year', input$dummyvarfunc, ignore.case=TRUE)) { 
              out <- 'date'
        } else if(is.numeric(values$dataset[[input$dummyvarfunc]])) { 
                out <- 'num'
        } else { 
                  out <- 'other'
                  }
      })
     output$dummy_sub <- renderUI({
       if(input$VarCreateTop=='Dummy variables'&input$dummyfunc=='From variable'&!is.null(dum_temp())){
        if(dum_temp()=='date'){
              tagList(
                      conditionalPanel(condition="input.VarCreateTop=='Dummy variables'&input.dummyfunc=='From variable'",
                                       style = "margin-left:19px;", selectInput("dumsubselect", 'Set dummy variable based on', 
                                                                                choices=c('selected year(s) vs. all other years','before vs. after'))),
                      conditionalPanel(condition="input.VarCreateTop=='Dummy variables'&input.dummyfunc=='From variable'",
                                       style = "margin-left:19px;",  selectInput("select.val", 'Select year(s)', 
                                                                                 choices=c(NULL, unique(lubridate::year(values$dataset[[input$dummyvarfunc]]))), multiple=TRUE))
                             )
               } else if(dum_temp()=='num'){
                       conditionalPanel(condition="input.VarCreateTop=='Dummy variables'&input.dummyfunc=='From variable'",
                                        style = "margin-left:19px;", selectInput("dumsubselect2", 'Set dummy variable based on',
                                                                                 choices=c('multi','single','selected value vs all other value','less than vs more than'), selected='multi'))
              } else if(dum_temp()=='other') {
                     conditionalPanel(condition="input.VarCreateTop=='Dummy variables'&input.dummyfunc=='From variable'",
                                      style = "margin-left:19px;", selectInput("select.val", 'Select categories to set to zero', choices=c(NULL, unique(values$dataset[[input$dummyvarfunc]])), multiple=TRUE))
             }
                                       
       }
     })                  
       dum_num <- reactive({
         if(input$dumsubselect2=='multi') { out2 <- 'multi'} else { out2 <- 'single'}
         print(out2)
       })                                 
     dum_num_sub <- renderUI({
       if(input$VarCreateTop=='Dummy variables'&input$dummyfunc=='From variable'&dum_temp()=='num'&dum_num()=='multi'){
              sliderInput("select.val", 'Select single or range of values to set to zero', min=min(values$dataset[input$dummyvarfunc],na.rm=TRUE),
                                                           max=max(values$dataset[input$dummyvarfunc],na.rm=TRUE),
                                                           value=c(mean(values$dataset[input$dummyvarfunc], na.rm=TRUE)-mean(values$dataset[input$dummyvarfunc], na.rm=TRUE)/10, 
                                                                   mean(values$dataset[input$dummyvarfunc], na.rm=TRUE)+mean(values$dataset[input$dummyvarfunc], na.rm=TRUE)/10))
        } else if(input$VarCreateTop=='Dummy variables'&input$dummyfunc=='From variable'&dum_temp()=='num'&dum_num()=='single') {
              sliderInput("select.val", 'Select single or range of values to set to zero', min=min(values$dataset[input$dummyvarfunc],na.rm=TRUE),
                                                     max=max(values$dataset[input$dummyvarfunc],na.rm=TRUE),
                                                     value=(mean(values$dataset[input$dummyvarfunc], na.rm=TRUE)-mean(values$dataset[input$dummyvarfunc], na.rm=TRUE)))
      } 
    })
     observeEvent(input$runNew, {
        if(input$VarCreateTop=='Data transformations'&input$trans=='temp_mod') {
          values$dataset[[input$varname]] <- temporal_mod(values$dataset, input$TimeVar, input$define.format) #!
        } else if(input$VarCreateTop=='Data transformations'&input$trans=='set_quants'){
          values$dataset[[input$varname]] <- set_quants(values$dataset, x=input$trans_var_name, quant.cat = input$quant.cat, name=input$varname) #!
        } else if(input$VarCreateTop=='Nominal ID'&input$ID=='ID_var'){
          values$dataset <- ID_var(values$dataset, newID=input$varname, input$unique_identifier) 
        } else if(input$VarCreateTop=='Nominal ID'&input$ID=='create_seasonal_ID'){
          values$dataset <- create_seasonal_ID(values$dataset, seasonal.dat=seasonalData(), use.location=input$use.location, 
                                               use.geartype=input$use.geartype, sp.col=input$sp.col, target=input$target)
        } else if(input$VarCreateTop=='Arithmetic and temporal functions'&input$numfunc=='create_var_num'){
          values$dataset[[input$varname]] <- create_var_num(values$dataset, input$var_x, input$var_y, method=input$create.method, name=input$varname) 
        } else if(input$VarCreateTop=='Arithmetic and temporal functions'&input$numfunc=='cpue') {
              if(input$xTime!='Calculate duration'){
          values$dataset[[input$varname]] <- cpue(values$dataset, input$xWeight, input$xTime, name=input$varname)  
              } else {
           values$dataset[['dur']] <- create_duration(values$dataset, input$dur.start2, input$dur.end2, input$dur.units2, name=NULL)
           values$dataset[[input$varname]] <- cpue(values$dataset, input$xWeight, 'dur', name=input$varname)  
              }
        } else if(input$VarCreateTop=='Spatial functions' & input$dist=='create_dist_between'){
          
          #'Zonal centroid', 'Port', 'Lat/lon coordinates'
          if(input$start=='Lat/lon coordinates'){
            start <-input$start.latlon
          } else if(input$start=='Port'){
            start <- input$port.start
          } else {
            start <- 'centroid'
          }
          if(input$end=='Lat/lon coordinates'){
            end <-input$end.latlon
          } else if(input$end=='Port'){
            end <- input$port.end
          } else {
            end <- 'centroid'
          }
            values$dataset[[input$varname]] <- create_dist_between_for_gui(values$dataset, start=start, end=end, input$units,  portTable=input$filePort, 
                                                                        gridfile=griddata(),lon.dat=input$lon.dat[2], lat.dat=input$lon.dat[1], 
                                                                         input$cat, lon.grid=input$long.grid[2], lat.grid=input$long.grid[1]) 
        } else if(input$VarCreateTop=='Spatial functions' & input$dist=='create_mid_haul'){
           values$dataset <- create_mid_haul(values$dataset, input$mid.start, input$mid.end, input$varname)
        } else if(input$VarCreateTop=='Spatial functions'&input$dist=='create_duration'){
           values$dataset[[input$varname]] <- create_duration(values$dataset, input$dur.start, input$dur.end, input$dur.units, name=NULL)
        } else if(input$VarCreateTop=='Spatial functions'&input$dist=='create_startingloc'){
           values$dataset[['startingloc']] <- create_startingloc(values$dataset,  gridfile=GridFileData(),  portTable=input$port.dat, 
                                                                trip_id=input$trip_id_SL, haul_order=input$haul_order_SL, starting_port=input$starting_port_SL, 
                         input$lon.dat_SL, input$lat.dat_SL, input$cat_SL, input$lon.grid_SL, input$lat.grid_SL)
        } else if(input$VarCreateTop=='Trip-level functions'&input$trip=='haul_to_trip'){
           values$dataset <- haul_to_trip(values$dataset, project=project, input$fun.numeric, input$fun.time, input$Haul_Trip_IDVar)
        } else if(input$VarCreateTop=='Trip-level functions'&input$trip=='trip_distance'){
           values$dataset$TripDistance <- create_trip_distance(values$dataset, input$port.dat.dist, input$trip_ID, input$starting_port, 
                                                              c(input$starting_haul[2], input$starting_haul[1]), 
                                                              c(input$ending_haul[2],input$ending_haul[1]), input$ending_port, input$haul_order)
        } else if(input$VarCreateTop=='Trip-level functions'&input$trip=='trip_centroid'){
           values$dataset <- create_trip_centroid(values$dataset, lon=input$trip_cent_lon, lat=input$trip_cent_lat, weight.var=input$trip_cent_weight, input$trip_cent_id)
        }
      })

      output$output_table_create <- DT::renderDT(
        head(values$dataset)
      )
###----
         
#DATA QUALITY FUNCTIONS
###-----      
#Basic functions   
##----
          RC <- isolate({sub(",([^,]*)$", ", and\\1",paste(names(which(apply(values$dataset, 2, function(x) any(is.na(x)))==TRUE)), collapse = ", "))})
          RN <- isolate({sub(",([^,]*)$", ", and\\1", paste(apply(values$dataset[,names(which(apply(values$dataset, 2, function(x) any(is.na(x)))==TRUE))], 2, 
                                                                  function(x) length(which(is.na(x)==TRUE))), collapse=", "))})
          RA <- isolate({length(unique(unlist(apply(values$dataset[,names(which(apply(values$dataset, 2, 
                                                                         function(x) any(is.na(x)))==TRUE))], 2, function(x) which(is.na(x)==TRUE)))))})
          RM <- isolate({sub(",([^,]*)$", ", and\\1", paste(apply(values$dataset[,names(which(apply(values$dataset, 2, 
                                                                          function(x) any(is.na(x)))==TRUE))],2, mean, na.rm=TRUE), collapse = ", "))})
  na <- function(x) { 
     if(any(apply(x, 2, function(x) any(is.na(x))))==TRUE) {
         if(input$NA_Filter=='none'){
          paste("The", RC,
              "variables contain", RN, "missing values, respectively.\nConsider using na_filter to replace or remove the", RA, "rows with missing values.") 
     }} else {
      if(input$NA_Filter=='none'){
         paste("No columns in the data set contain missing values.")
        } else {
          if(input$NA_Filter=='Remove all'){
         paste("The", RC, "variables contained", RN, "missing values.\n", RA, "rows containing missing values have been removed from the data set.")
        } else if(input$NA_Filter=='Replace with mean'){
         paste("The", RC, "variables contained", RN, "missing values.\nMissing values have been replaced with the mean values of", RM, "respectively.")
         }
        } 
      }}
      
  RCN <- isolate({sub(",([^,]*)$", ", and\\1",paste(names(which(apply(values$dataset, 2, function(x) any(is.nan(x)))==TRUE)), collapse = ", "))})
  RNN <- isolate({sub(",([^,]*)$", ", and\\1", paste(apply(values$dataset[,names(which(apply(values$dataset, 2, function(x) any(is.nan(x)))==TRUE))], 2, 
                                                          function(x) length(which(is.nan(x)==TRUE))), collapse=", "))})
  RAN <- isolate({length(unique(unlist(apply(values$dataset[,names(which(apply(values$dataset, 2, 
                                                                              function(x) any(is.nan(x)))==TRUE))], 2, function(x) which(is.nan(x)==TRUE)))))})
  RMN <- isolate({sub(",([^,]*)$", ", and\\1", paste(apply(values$dataset[,names(which(apply(values$dataset, 2, 
                                                                               function(x) any(is.nan(x)))==TRUE))],2, mean, na.rm=TRUE), collapse = ", "))})               
  nan <- function(x) { 
    if(any(apply(x, 2, function(x) any(is.nan(x))))==TRUE) {
      if(input$NAN_Filter=='none'){
        paste("The", RCN,
            "variables contain", RNN, "non-numbers, respectively.\nConsider using nan_filter to replace or remove the", RAN, "rows with non-numbers.") 
      }} else {
        if(input$NAN_Filter=='none'){
          "No columns in the data set contain non-numbers."
        } else {
          if(input$NAN_Filter=='Remove all'){
            paste("The", RC, "variables contained", RNN, "non-numbers.\n", RA, "rows containing non-numbers have been removed from the data set.")
          } else if(input$NAN_Filter=='Replace with mean'){
            paste("The", RCN, "variables contained", RNN, "non-numbers.\nNon-numbers have been replaced with the mean values of", RMN, "respectively.")
          }
        } 
      }}
  
      
      #Unique observations
      obs <- function(x) { if (dim(values$dataset)[1] == dim(unique(values$dataset))[1]) {
        "Each row is a unique choice occurrence. No further action required."
      } else {
        if(input$Unique_Filter=='FALSE'){
        "Each row in data set is not a unique choice occurrence at haul or trip level. \nConsider removing non-unique rows."
        } else {
          "Duplicate choice occurrence at haul or trip level existed in the data set and have been removed."
        }
      }
      }
      
      #Empty variables
      empty <- function(x) { if (any(apply(values$dataset, 2, function(x) all(is.na(x))) == TRUE)) {
        if(input$Empty_Filter=='FALSE'){
          paste(names(which(apply(values$dataset, 2, function(x) all(is.na(x))) == TRUE)), "is empty. 
            \nConsider removing the column from the data set.")
        } else {
          paste(names(which(apply(values$dataset, 2, function(x) all(is.na(x))) == TRUE)), "is empty and has been removed from the data set.")
        }
      } else {
        "No empty variables exist in the data set. No further action required."
      }
      }
      
      #Lat/Lon units
      lat_lon <- function(x) { 
      if(any(apply(values$dataset[,which(grepl('lat|lon', names(values$dataset), ignore.case=TRUE)==TRUE)], 2, function(x) !is.numeric(x))==TRUE)==TRUE){
        if(input$LatLon_Filter==FALSE){
          'At least one latitude or longitude variable is not in decimal degrees. \nClick the button to the left to convert to decimal degrees.'
        } else {
          'At least one latitude or longitude variable is not in decimal degrees. \nLatitude and longitude variables converted to decimal degrees.'
        }
        } else {
          'Latitude and longitude variables in decimal degrees. \nNo further action required.'
        }
      } 
      
##Output to main panel
      output$Case<-renderPrint({
        if(input$checks=='Summary table') {
          "Summary table of NUMERIC variables in data set."
        } else  if (input$checks=='Outliers'){
                      if(input$dat.remove=='none'){
                        paste('Table to assess outliers.', input$column_check, "shown. \nZoom in by highlighting desired area and double clicking. \nDouble click again to reset plot.")
              } else {
                paste('Table to assess outliers.', input$column_check, 'shown. \nZoom in by highlighting desired area and double clicking. \nDouble click again to reset plot. 
              \nExcluding points that fall outside the',  if(input$dat.remove=='5_95_quant'){
               '5th and 95th quantiles'
              } else if(input$dat.remove=='25_75_quant') {
                '25th and 75th quantiles'
              } else if(input$dat.remove=='mean_2SD'){
                'mean +/- 2SD'
              } else if(input$dat.remove=='mean_3SD'){
                'mean +/- 3SD'
              } else if(input$dat.remove=='median_2SD') {
                'median +/- 2SD'
              } else if(input$dat.remove=='median_3SD'){
                'median +/- 3SD'
              }, "results in removing", nrow(values$dataset)-tableInputOutlier()[which(rownames(tableInputOutlier())==input$dat.remove),1] ,"points from the data set.")
              }
        } else  if (input$checks=='NAs'){
          na(values$dataset)
        } else if(input$checks=='NaNs') {
          nan(values$dataset)
        } else if(input$checks=='Unique observations'){
          obs(values$dataset)
        } else if(input$checks=='Empty variables'){
          empty(values$dataset)
        } else if(input$checks=='Lat_Lon units'){
          lat_lon(values$dataset)
        } else {
          'Make a selection in the left hand column'
        } 
      })
      
     
      
      ##Output to saved file
   case_to_print <- reactive({
     if(input$tabs=='qaqc'){
        if(input$checks=='Summary table') {
          "Summary table of numeric variables viewed.\n"
        } else  if (input$checks=='Outliers'){
          if(input$dat.remove=='none'){
            paste0('Table and plots to assess outliers viewed for ', input$column_check, ".\n")
          } else {
            paste('Table and plot to assess outliers viewed for', input$column_check, 'with',
              nrow(values$dataset)-tableInputOutlier()[which(rownames(tableInputOutlier())==input$dat.remove),1], 
              'points that fall outside the',  if(input$dat.remove=='5_95_quant'){
                '5th and 95th quantiles'
              } else if(input$dat.remove=='25_75_quant') {
                '25th and 75th quantiles'
              } else if(input$dat.remove=='mean_2SD'){
                'mean +/- 2SD'
              } else if(input$dat.remove=='mean_3SD'){
                'mean +/- 3SD'
              } else if(input$dat.remove=='median_2SD') {
                'median +/- 2SD'
              } else if(input$dat.remove=='median_3SD'){
                'median +/- 3SD'
              }, "removed.\n")
          }
        } else if (input$checks=='NAs'){
          if(any(apply(values$dataset, 2, function(x) any(is.na(x))))==TRUE) {
            if(input$NA_Filter=='none'){
              paste("Occurrence of missing values checked. The", RC,
                    "variables contain", RN, "missing values, respectively.", RA, "rows have missing values. Missing values were not removed or replaced.\n") 
            }} else {
              if(input$NA_Filter=='none'){
                paste("Occurrence of missing values checked. No columns in the data set contain missing values.\n")
              } else {
                if(input$NA_Filter=='Remove all'){
                  paste("Occurrence of missing values checked. The", RC, "variables contained", RN, "missing values.", RA, "rows containing missing values were removed from the data set.\n")
                } else if(input$NA_Filter=='Replace with mean'){
                  paste("Occurrence of missing values checked. The", RC, "variables contained", RN, "missing values. Missing values were replaced with the mean values of", RM, "respectively.\n")
                }
              } }
        } else if(input$checks=='NaNs') {
          if(any(apply(values$dataset, 2, function(x) any(is.nan(x))))==TRUE) {
            if(input$NAN_Filter=='none'){
              paste("Occurruence of non-numbers checked. The", RCN,
                    "variables contain", RNN, "non-numbers, respectively.", RAN, "rows have non-numbers. No action was taken to remove or replace non-numbers.\n") 
            }} else {
              if(input$NAN_Filter=='none'){
                "Occurruence of non-numbers checked. No columns in the data set contain non-numbers.\n"
              } else {
                if(input$NAN_Filter=='Remove all'){
                  paste("Occurruence of non-numbers checked. The", RC, "variables contained", RNN, "non-numbers.", 
                        RA, "rows containing non-numbers were removed from the data set.\n")
                } else if(input$NAN_Filter=='Replace with mean'){
                  paste("Occurruence of non-numbers checked. The", RCN, "variables contained", RNN, "non-numbers. Non-numbers were replaced with the mean values of", RMN, "respectively.\n")
                }
              } }
        } else if(input$checks=='Unique observations'){
          if (dim(values$dataset)[1] == dim(unique(values$dataset))[1]) {
            "Each row is a unique choice occurrence.\n"
          } else {
            if(input$Unique_Filter=='FALSE'){
              "Each row in data set is not a unique choice occurrence at haul or trip level. No action taken.\n"
            } else {
              "Duplicate choice occurrence at haul or trip level existed in the data set and have been removed.\n"
            }
          }
        } else if(input$checks=='Empty variables'){
          if (any(apply(values$dataset, 2, function(x) all(is.na(x))) == TRUE)) {
            if(input$Empty_Filter=='FALSE'){
              paste('Occurrence of empty variables was checked and the', names(which(apply(values$dataset, 2, function(x) all(is.na(x))) == TRUE)), 
                    "variable is empty. The varible was not removed from the data set.\n")
            } else {
              paste('Occurrence of empty variables was checked and the', names(which(apply(values$dataset, 2, function(x) all(is.na(x))) == TRUE)), 
                    "was empty and was removed from the data set.\n")
            }
          } else {
            "Occurrence of empty variables was checked and not found in the data set.\n"
          }
        } else if(input$checks=='Lat_Lon units'){
          if(any(apply(values$dataset[,which(grepl('lat|lon', names(values$dataset), ignore.case=TRUE)==TRUE)], 2, function(x) !is.numeric(x))==TRUE)==TRUE){
            if(input$LatLon_Filter==FALSE){
              'Latitude and longitude units were checked and are not in decimal degrees.\n'
            } else {
              'Latitude or longitude units not in decimal degrees were converted to decimal degrees.\n'
            }
          } else {
            'Latitude and longitude units were checked and are in decimal degrees.\n'
          }
        }
     } else if(input$tabs=='explore'){
         if(input$plot_table=='Plots'& input$plot_type=='Temporal'){
          paste0("Viewed plots of ", input$col_select, ' against time for raw points, the ', input$p2fun, ", and the ",  input$p3fun, ' value.\n')
        } else if(input$plot_table=='Plots'& input$plot_type=='Spatial'){
          paste0("Viewed spatial distribution of occurrence points and spatial density of occurrence points.\n
          Getis-ord and Moran's I statistics provided for ", input$varofint, ". Default settings for spdep functions are used.")
        } else if(input$plot_table=='Plots'& input$plot_type=='x-y plot'){
            paste0("Viewed plotted relationship between ", input$x_y_select1,  'and ', input$x_y_select2, '.\n')
        } 
       } else if(input$tabs=='analysis'){
        if(input$corr_reg=='Correlation'){
            paste0("Viewed correlation matrix for ",  isolate({sub(",([^,]*)$", ", and\\1",paste(input$corr_select, collapse = ", "))}), '.\n')
        } else if(input$corr_reg=='Regression'){
            paste0('Veiwed plot and linear regression test output for ',input$reg_exp_select, ' on ', input$reg_resp_select,'.\n') 
        } 
       }
      })
   
  notes <- reactive({ 
   if(input$tabs=='qaqc'){
      if(!is.null(input$notesQAQC)){
          paste0(input$notesQAQC, "\n")
      }
   } else if(input$tabs=='anal') {
       if(!is.null(input$notesAnal)){
            paste0(input$notesAnal, "\n")
       }
   } else if(input$tabs=='explore'){
       if(!is.null(input$notesExplore)){
          paste0(input$notesExplore, "\n")
       }
   } else if(input$tabs=='upload'){
     if(!is.null(input$notesUp)){
       paste0(input$notesUp, "\n")
     }
   } else if(input$tabs=='new'){
     if(!is.null(input$notesNew)){
       paste0(input$notesNew, '\n')
     }
   }
  })
##----
     
##Table output
##----
      tableInputSummary <- reactive({
         if (input$checks=='Summary table') { 
          temp <- values$dataset
          stable <- summary_stats(temp) 
          nums <- unlist(lapply(temp, is.numeric))
          stable  <- apply(stable[nums], 2, function(x) gsub(".*:","", x))
          rownames(stable)=c('Min', 'Median','Mean', 'Max','NAs','Unique Obs.', "No. 0's")
          stable <- as.data.frame(as.matrix(stable))
          stable <- as.data.frame((t(stable)))
        } else {
          NULL
        }
      })
      
        output$output_table_summary <- DT::renderDT(
          tableInputSummary(), server = FALSE, rownames=TRUE,
            options = list(autoWidth=FALSE, scrollX=T, responsive=FALSE, pageLength = 25)
                  )
    
       tableInputOutlier <- reactive({
         if (input$checks=='Outliers'){
            table <- outlier_table(values$dataset, input$column_check)
            rownames(table)=table[,2]
            table <- table[,3:10]
            #table <<- table
          } else {
            NULL
          }
       })
        
         output$output_table_outlier <- DT::renderDT(
           if (input$checks=='Outliers'){
             table <- outlier_table(values$dataset, input$column_check)
             rownames(table)=table[,2]
             table <- table[,3:10]
             #table <<- table
           } else {
             NULL
           }, server = FALSE, selection='single', rownames=TRUE,
          options = list(autoWidth=FALSE, scrollX=T,  responsive=TRUE, pageLength = 7)
        )
        
        ranges1 <- reactiveValues(x = NULL, y = NULL)   
        ranges2 <- reactiveValues(x = NULL, y = NULL)   
        ranges3 <- reactiveValues(x = NULL, y = NULL)
        #Plot output
        output$plot1 <- renderPlot(
          if (is.null(values$dataset)) {
            return(NULL)
          } else {
          if(input$checks=='Outliers'){
            temp <- values$dataset
            temp$val <- 1:nrow(temp)
             dat_sub <- suppressWarnings(outlier_plot_int(temp, input$column_check, input$dat.remove, input$x.dist, plot_type=1))
             suppressWarnings(ggplot() + geom_point(data=dat_sub, aes_string(x='val', y=input$column_check, color = 'Points', na.rm=TRUE)) +
               scale_color_manual(breaks=c('Kept','Removed'),values=c('blue','red'))+
                 coord_cartesian(xlim = ranges1$x, ylim = ranges1$y, expand = FALSE)+
                labs(x='Data row')+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text=element_text(size=12),
                                        axis.title=element_text(size=12)))  #+ 
               #
          } else {
            NULL
          }}
        )
 
        output$plot2 <- renderPlot(
          if (is.null(values$dataset)) {
            return(NULL)
          } else {
            if(input$checks=='Outliers'){
              temp <- values$dataset
              temp$val <- 1:nrow(temp)
              dat_sub <- outlier_plot_int(temp, input$column_check, input$dat.remove, input$x.dist, plot_type=1)
              arg.return <- outlier_plot_int(temp, input$column_check, input$dat.remove, input$x.dist, plot_type=2)
              ggplot(dat_sub[dat_sub$Points=='Kept',], aes_string(input$column_check)) + 
                geom_histogram(aes(y = ..density..), na.rm=TRUE, bins=round(nrow(temp)/2)) + arg.return +
                coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)+
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                      panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text=element_text(size=12),
                      axis.title=element_text(size=12))
                         } else {
              NULL
            }}
        )
        
        output$plot3 <- renderPlot(
          if (is.null(values$dataset)) {
            return(NULL)
          } else {
            if(input$checks=='Outliers'){
              temp <- values$dataset
              temp$val <- 1:nrow(temp)
              temp <- outlier_plot_int(temp, input$column_check, input$dat.remove, input$x.dist, plot_type=3)
              ggplot(temp, aes(x=fit_quants, y=data_quants)) + geom_point(shape=1) + geom_abline() +
                labs(x='Theoretical Quantiles', y='Sample Quantiles', title=paste('Q-Q plot of', input$x.dist, 'fit against data'))+
                coord_cartesian(xlim = ranges3$x, ylim = ranges3$y, expand = FALSE)+
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                      panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text=element_text(size=12),
                      axis.title=element_text(size=12))
            } else {
              NULL
            }}
        )
     
        
        #Hover info       
        output$hover_info1 <- renderUI({
          temp <- values$dataset
          temp$val <- 1:nrow(temp)
          hover <- input$plot1_hover
          point <- nearPoints(temp, input$plot1_hover,  threshold = 5, maxpoints = 1, addDist = FALSE)
          if (nrow(point) == 0) return(NULL)
          
          # calculate point position INSIDE the image as percent of total dimensions
          # from left (horizontal) and from top (vertical)
          left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
          top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
          
          # calculate distance from left and bottom side of the picture in pixels
          left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
          top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
          
          # create style property fot tooltip
          # background color is set so tooltip is a bit transparent
          # z-index is set so we are sure are tooltip will be on top
          style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                       "left:", (hover$range$right +(hover$range$right)/4), "px; top:", (hover$range$bottom+hover$range$bottom/4), "px;")
          
          # actual tooltip created as wellPanel
          wellPanel(
            style = style,
            paste0("Value: ", point[[input$column_check]]))
        })
      
        
        # When a double-click happens, check if there's a brush on the plot.
        # If so, zoom to the brush bounds; if not, reset the zoom.
        observeEvent(input$plot1_dblclick, {
          brush <- input$plot1_brush
          if (!is.null(brush)) {
            ranges1$x <- c(brush$xmin, brush$xmax)
            ranges1$y <- c(brush$ymin, brush$ymax)
            
          } else {
            ranges1$x <- NULL
            ranges1$y <- NULL
          }
        })
        
        observeEvent(input$plot2_dblclick, {
          brush <- input$plot2_brush
          if (!is.null(brush)) {
            ranges2$x <- c(brush$xmin, brush$xmax)
            ranges2$y <- NULL
            
          } else {
            ranges2$x <- NULL
            ranges2$y <- NULL
          }
        })
        
        observeEvent(input$plot3_dblclick, {
          brush <- input$plot3_brush
          if (!is.null(brush)) {
            ranges3$x <- c(brush$xmin, brush$xmax)
            ranges3$y <- c(brush$ymin, brush$ymax)
            
          } else {
            ranges3$x <- NULL
            ranges3$y <- NULL
          }
        })
##----        
        
##Outlier options 
##----        
      output$outlier_column <- renderUI({
        conditionalPanel(
          condition="input.checks=='Outliers'",
            selectInput('column_check', 'Choose variable',
                        choices= names(values$dataset[1,unlist(lapply(values$dataset, is.numeric))]), selectize=TRUE))
                        })
        output$outlier_subset <- renderUI({
          conditionalPanel(
            condition="input.checks=='Outliers'",
            selectInput('dat.remove', 'Method to subset the data', 
                        choices=c('none', '5_95_quant', '25_75_quant','mean_2SD','mean_3SD','median_2SD','median_3SD'),
                        selected=c('none', '5_95_quant', '25_75_quant','mean_2SD','mean_3SD','median_2SD','median_3SD')[input$output_table_outlier_rows_selected]))
                        })
        output$outlier_dist <- renderUI({
          conditionalPanel(
            condition="input.checks=='Outliers'",
            selectInput('x.dist', 'Distribution', 
                        choices=c('normal', 'lognormal', 'exponential', 'weibull', 'poisson', 'negative binomial'), selected='normal'))
      })
##----

##Filtering options
##----
      #output_table())
     
      
      observeEvent(input$NA_Filter,{
        if(input$NA_Filter=='Remove all'){
          if(any(apply(values$dataset, 2, function(x) any(is.na(x))))==TRUE){
          values$dataset <- na_filter(values$dataset, names(which(apply(values$dataset, 2, function(x) any(is.na(x)))==TRUE)), replace = FALSE, remove = TRUE, over_write=FALSE)  
        } else {
          cat('No missing values to remove')
        }
          }else if(input$NA_Filter=='Replace with mean') {
          if(any(apply(values$dataset, 2, function(x) any(is.na(x))))==TRUE){
          values$dataset <- na_filter(values$dataset,  names(which(apply(values$dataset, 2, function(x) any(is.na(x)))==TRUE)), replace = TRUE, remove = FALSE, over_write=FALSE)
          } else {
            cat('No missing values to remove')
        }}
      })
      
        observeEvent(input$NAN_Filter,{
          if(input$NAN_Filter=='Remove all'){
            if(any(apply(values$dataset, 2, function(x) any(is.nan(x))))==TRUE){
            values$dataset <- nan_filter(values$dataset, names(which(apply(values$dataset, 2, function(x) any(is.nan(x)))==TRUE)), replace = FALSE, remove = TRUE, over_write=FALSE)  
                  } else{
                    print('No non-numbers to remove.')
                  } 
            }else if(input$NAN_Filter=='Replace with mean'){
                    if(any(apply(values$dataset, 2, function(x) any(is.nan(x))))==TRUE){
           values$dataset <- nan_filter(values$dataset,  names(which(apply(values$dataset, 2, function(x) any(is.nan(x)))==TRUE)), replace = TRUE, remove = FALSE, over_write=FALSE)
                    } else {
                      print('No non-numbers to remove.')
                    }}
      })
        
        observeEvent(input$Outlier_Filter,{
          if(input$Outlier_Filter=='TRUE'){
          values$dataset <- FishSET::outlier_remove(values$dataset, input$column_check, dat.remove = input$dat.remove, remove = T, over_write=FALSE)
            }
        })
        
        observeEvent(input$Unique_Filter,{
          if(input$Unique_Filter=='TRUE'){
            values$dataset <- unique(values$dataset)
          }
        })
        
        observeEvent(input$Empty_Filter,{
          if(input$Empty_Filter=='TRUE'){
            values$dataset <- values$dataset[, names(values$dataset)!=names(which(apply(values$dataset, 2, function(x) all(is.na(x))) == TRUE))]
          }
        })
        
        observeEvent(input$LatLon_Filter, {
          if(input$LatLon_Filter=='TRUE'){
            if(any(apply(values$dataset[,which(grepl('lat|lon', names(values$dataset), ignore.case=TRUE)==TRUE)], 2, function(x) !is.numeric(x))==TRUE)==TRUE){
             values$dataset <- degree(values$dataset, colnames(values$dataset[,which(grepl('lat', names(values$dataset), ignore.case=TRUE))]) ,
                                               colnames(values$dataset[,which(grepl('lon', names(values$dataset), ignore.case=TRUE))]) ) 
            } else {
            cat("All latitude and longitude variables are already in decimal degrees. Function not applied.")
            }
          }
        })
##----        

####----
##Resetting inputs
        observeEvent(input$refresh1,{
          updateCheckboxInput(session, 'Outlier_Filter', value=FALSE)
          updateRadioButtons(session, 'NA_Filter', selected='none')
          updateRadioButtons(session, 'NAN_Filter', selected='none')
        })
###----                

####-----        
##Save output       
###----      
        observeEvent(input$saveData, {
          suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"))
          DBI::dbWriteTable(fishset_db, paste0(project, 'MainDataTable'), values$dataset, overwrite=TRUE)
          DBI::dbDisconnect(fishset_db)
        })
        
        observeEvent(input$saveDataQ, {
          suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"))
          DBI::dbWriteTable(fishset_db, paste0(project, 'MainDataTable'), values$dataset, overwrite=TRUE)
          DBI::dbDisconnect(fishset_db)
        })
        
      output$SaveButtons <- renderUI({
        tagList(
          #shinySaveButton(id = 'downloadplot', label ='Save plot to folder', title = "", filename = paste0(project,'_', input$checks, '_plot'), filetype = "png"),
          actionButton('downloadplot', label ='Save plot to folder'),
          downloadLink('downloadplotHIDE', label=''),
          actionButton('downloaddata', label ='Save table to folder as csv'),
          downloadLink("downloadText", label=''),
          actionButton('callTextDownload','Save notes')
        )
      })
      
 
      output$SaveButtonsUpload <- renderUI({
        tagList(
          downloadLink("downloadTextUp", label=''),
          actionButton('callTextDownloadUp','Save notes')
        )
      })
      
      ## Save buttons
     output$SaveButtonsExplore <- renderUI({
       tagList(
          downloadLink('downloadplotEXPLOREHIDE', label=''),
          actionButton('downloadplotExplore', label ='Save plot to folder'),#, title = "", filename = paste0(project, input$plot_type , '_plot'), filetype = "png")
          downloadLink('downloadTableEXPLOREHIDE', label=''),
          actionButton('downloadTableExplore', label ='Save table to folder as csv'),#, title = "", filename = paste0(project, input$plot_type , '_plot'), filetype = "png")
          downloadLink("downloadTextExplore", label=''),
          actionButton('callTextDownloadExplore','Save notes')
       )
     })
    
      output$SaveButtonsAnal <- renderUI({
        tagList(
          downloadLink('downloadplotAnalHIDE', label =''),
          downloadLink('downloaddataAnalHIDE', label =''),
          actionButton('downloadplotAnal', label ='Save plot to folder'),#, title = "", filename = paste0(project,'_', input$corr_reg, '_plot'), filetype = "png"),
          actionButton('downloaddataAnal', label ='Save table to folder as csv'),
          downloadLink("downloadTextAnal", label=''),
          actionButton('callTextDownloadAnal','Save notes.')
        )
      })
      
      output$SaveButtonsNew <- renderUI({
        tagList(
          downloadLink('downloadplotNew', label=''),
          actionButton('downloadplotNew', label ='Save plot to folder'),#, title = "", filename = paste0(project, input$plot_type , '_plot'), filetype = "png")
          downloadLink("downloadTextNew", label=''),
          actionButton('callTextDownloadNew','Save notes.')
        )
      })
        

 ###----        

##Downloads      
##----
 savedText <- reactiveValues(answers = logical(0))
  observeEvent(c(input$callTextDownload,
                 input$callTextDownloadAnal,
                 input$callTextDownloadExplore,
                 input$callTextDownloadUp,
                 input$callTextDownloadNew),{
      savedText$answers <- as.character(c(savedText$answers, case_to_print(), notes()))
  })
  
#  Stored Txt
  observeEvent(input$callTextDownloadUp, {
    output$downloadTextUp <- downloadHandler(
      filename = function() {
        paste0(loc, '/inst/output/StoredText.txt')
      },
      content = function(file) {
        writeLines(savedText$answers, file)
      },
      contentType = "text/csv"
    )
    jsinject <- "setTimeout(function(){window.open($('#downloadTextUp').attr('href'))}, 100);"
    session$sendCustomMessage(type = 'jsCode', list(value = jsinject))   
  })
  observeEvent(input$callTextDownloadExplore, {
   output$downloadTextExplore <- downloadHandler(
        filename = function() {
          paste0(loc, '/inst/output/StoredText.txt')
        },
        content = function(file) {
          writeLines(savedText$answers, file)
        },
        contentType = "text/csv"
      )
    jsinject <- "setTimeout(function(){window.open($('#downloadTextExplore').attr('href'))}, 100);"
    session$sendCustomMessage(type = 'jsCode', list(value = jsinject))   
  })
  observeEvent(input$callTextDownloadAnal,{
               output$downloadTextAnal<- downloadHandler(
                   filename = function() {
                     paste0(loc, '/inst/output/StoredText.txt')
                   },
                   content = function(file) {
                     writeLines(savedText$answers, file)
                   },
                   contentType = "text/csv"
                 )
                 jsinject <- "setTimeout(function(){window.open($('#downloadTextAnal').attr('href'))}, 100);"
                 session$sendCustomMessage(type = 'jsCode', list(value = jsinject))   
               })
  observeEvent(input$callTextDownload,{
                 output$downloadText <- downloadHandler(
                   filename = function() {
                     paste0(loc, '/inst/output/StoredText.txt')
                   },
                   content = function(file) {
                     writeLines(savedText$answers, file)
                   },
                   contentType = "text/csv"
                 )
                 jsinject <- "setTimeout(function(){window.open($('#downloadText').attr('href'))}, 100);"
                 session$sendCustomMessage(type = 'jsCode', list(value = jsinject))   
               })
  observeEvent(input$callTextDownloadNew, {
    output$downloadTextNew <- downloadHandler(
      filename = function() {
        paste0(loc, '/inst/output/StoredText.txt')
      },
      content = function(file) {
        writeLines(savedText$answers, file)
      },
      contentType = "text/csv"
    )
    jsinject <- "setTimeout(function(){window.open($('#downloadTextNew').attr('href'))}, 100);"
    session$sendCustomMessage(type = 'jsCode', list(value = jsinject))   
  })
  
  
     observeEvent(input$downloadplot, {
        output$downloadplotHIDE <<- downloadHandler(
        filename = function() {
          paste0(loc, '/inst/output/',project,'Outlier.png')
        },
        content = function(file) {
             ggplot2::ggsave(file, plot=outlier_plot(values$dataset, input$column_check, input$dat.remove, input$x.dist))
        })
      jsinject <- "setTimeout(function(){window.open($('#downloadplotHIDE').attr('href'))}, 100);"
      session$sendCustomMessage(type = 'jsCode', list(value = jsinject))   
      })
  
     observeEvent(input$downloadplotAnal, {
       output$downloadplotAnalHIDE <<- downloadHandler(
         filename = function() {
           if(input$corr_reg=='Correlation'){
           paste0(loc, '/inst/output/', project,'CorrelationPlot.png')
           } else {
             paste0(loc, '/inst/output/',project,'RegressionPlot.png') 
           }
         },
         content = function(file) {
          if(input$corr_reg=='Correlation'){
            ggplot2::ggsave(file, plot=plotInputcorr(), device=function(..., width, height) grDevices::png(..., width = 12, height = 4, res = 300, units = "in"))
          } else if(input$corr_reg=='Regression'){
            ggplot2::ggsave(file, plot=plotInputreg(), device=function(..., width, height) grDevices::png(..., width = 12, height = 4, res = 300, units = "in"))
          }
         })
         jsinject <- "setTimeout(function(){window.open($('#downloadplotAnalHIDE').attr('href'))}, 100);"
         session$sendCustomMessage(type = 'jsCode', list(value = jsinject)) 
      })
      
      
         observeEvent(input$downloadplotExplore, {
           output$downloadplotEXPLOREHIDE <<- downloadHandler(
               filename = function() {
                 if(input$plot_type=='Temporal'){
                   paste0(loc, '/inst/output/', project,'TemporalPlot.png')
                 } else if(input$plot_type=='Spatial') {
                   paste0(loc, '/inst/output/',project,'SpatialPlot.png') 
                 } else {
                   paste0(loc, '/inst/output/',project,'x-yPlot.png') 
                 }
               },
             content = function(file) {
         if(input$plot_type=='Temporal'){
           ggplot2::ggsave(file, plot=plotInput_time(), device=function(..., width, height) grDevices::png(..., width = 12, height = 4, res = 300, units = "in")) 
        } else if(input$plot_type=='Spatial'){
          longitude <- which(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)', ignore.case=TRUE)==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)', ignore.case=TRUE)))[1]
          latitude <- which(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)', ignore.case=TRUE)==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)', ignore.case=TRUE)))[1]
          cf <- coord_fixed()
          cf$default <- TRUE
          p1 <- ggplot(data = map_data("world"), mapping = aes(x = long, y = lat, group=group)) + 
            geom_polygon(color = "black", fill = "gray") + 
            geom_point(data = values$dataset, aes(x = values$dataset[,longitude], y = values$dataset[,latitude], group=rep(1, nrow(values$dataset))), color = "red", size = 1) +
            cf + coord_fixed(xlim = ranges_spatial$x, ylim = ranges_spatial$y, ratio=1.3, expand = TRUE)+
            labs(x='Longitude', y='Latitude', subtitle='Observed locations')+
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                  panel.background = element_blank(),  axis.text=element_text(size=12),
                  axis.title=element_text(size=12),panel.border = element_rect(colour = "black", fill=NA, size=1) )
          
          ggplot2::ggsave(file, plot=suppressWarnings(ggpubr::ggarrange(p1, plotInput_kernel(),ncol =2, nrow = 1)), 
                          device=function(..., width, height) grDevices::png(..., width = 12, height = 4, res = 300, units = "in"))
        } else if(input$plot_type=='x-y plot'){
          ggplot2::ggsave(file, plot=plotInput_xy(), device=function(..., width, height) grDevices::png(..., width = 12, height = 4, res = 300, units = "in"))
        }
          })
        jsinject <- "setTimeout(function(){window.open($('#downloadplotEXPLOREHIDE').attr('href'))}, 100);"
        session$sendCustomMessage(type = 'jsCode', list(value = jsinject)) 
      })
      
         observeEvent(input$downloadTableExplore, {
           write.csv(gtmt_table(), paste0(loc,'/inst/output/',project,'GetisOrdMoransI.csv'))
         })
         
        observeEvent(input$downloaddata, {
          if(input$checks=='Summary table'){
              write.csv(tableInputSummary(), paste0(loc,'/inst/output/',project,'summary_table.csv'))
          } else if(input$checks=='Outliers'){
             write.csv(tableInputOutlier(), paste0(loc, '/inst/output/',project,'outlier_table.csv'))
          }
        })
        
        observeEvent(input$downloaddataAnal, {
          if(length(input$corr_select)>2){
            output$downloaddataAnalHIDE <<- downloadHandler(
              write.csv(tableInputCorr(), paste0(loc, '/inst/output/',project,'correlation_table.csv'))
            )
            } else {
         output$downloaddataAnalHIDE <<- downloadHandler(
                filename = function() {
                paste0(loc, '/inst/output/',project,'correlation_analysis.png')
                },
            content = function(file) {
              png(file)
              print(cor.test(values$dataset[[input$corr_select[1]]], values$dataset[[input$corr_select[2]]]))
              dev.off()
            }
            )   }
          jsinject <- "setTimeout(function(){window.open($('#downloaddataAnalHIDE').attr('href'))}, 100);"
          session$sendCustomMessage(type = 'jsCode', list(value = jsinject))   
         
        })
        
##----
# stop shiny
      observe({
        if (input$close > 0) stopApp()
      })
        observe({
          if (input$close1 > 0) stopApp()
        })
        observe({
          if (input$close2 > 0) stopApp()
        })
        observe({
          if (input$closeNew > 0) stopApp()
        })
        
    }
       )
}
