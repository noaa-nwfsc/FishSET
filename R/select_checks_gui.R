# Validation checks selection tool
# Select which data validation checks to apply.

###----->> WORKING ON DATA REFRESH <-----####

# select_checks
#' View and select which validation checks to run
#'
#' @param dat Main data frame containing data on hauls or trips. Table in fishset_db database should contain the string `MainDataTable`.
#' @param project Name of project. Parameter is used to generate meaningful table names in fishset_db database.
#' @importFrom DBI  dbDisconnect dbConnect dbListTables dbWriteTable 
#' @import shiny
#' @import shinyFiles
#' @importFrom ggpubr ggarrange
#' @importFrom gridExtra grid.table
#' @importFrom stringi stri_count_regex
#' @export select_checks
#' @details Opens an interactive table that allows uses to select which data validation checks to run by clicking check boxes. 
#' @examples 
#' \dontrun{
#' select_checks('pcodMainDataTable', 'pcod')
#' }

select_checks <- function(dat, project){#
  library(shiny)
  library(DT)
  library(shinyFiles)
  library(ggplot2)
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
    dataset <- FishSET:::table_view(dat)
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
  if (!exists("default_search")) default_search <- ""
  
  # default column search values
  if (!exists("default_search_columns")) default_search_columns <- NULL
  
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
  )),

#----
      tabsetPanel(
#----
#UPLOAD DATA
#-----
tabPanel("Upload Data",
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
                        fileInput("griddat", "Choose data file that varies over two dimensions (gridded)'",
                                    multiple = FALSE, placeholder = 'Optional data')),
                column(3, uiOutput('ui.actionG'))
           ),
           fluidRow(
                column(6, 
                        fileInput("auxdat", "Choose auxiliary data file that links to primary data",
                                  multiple = FALSE, placeholder = 'Optional data')),
                column(3, uiOutput('ui.actionA'))
           )
         )),
#-----
        
#---- 
#BEGIN DATA EXPLORATION TABSET PANEL 
#----   
          tabPanel("Data Exploration", 
                   sidebarLayout(
                     sidebarPanel(width=2,
                                  tags$br(),tags$br(),
                       uiOutput('SaveButtonsExplore'),
                       actionButton('saveData','Save data to fishset_db database'),
                       conditionalPanel(
                         condition='input.plot_table=="Table"',
                         actionButton('subsetData', 'Subset data to selected columns')
                       ),
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
                       
                       conditionalPanel(
                         condition='input.plot_table=="Table"',
                         verbatimTextOutput('editText')
                      ),
                      conditionalPanel(
                        condition='input.plot_table=="Plots" & input.plot_type=="Spatial"',
                        uiOutput("location_info_spatial")
                        )
                   ),
                   mainPanel(width=10,
                     tags$div(dataTableOutput("output_table_exploration"), style = "font-size: 75%; width: 100%"),
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
                        plotOutput('plot_spatial',
                              click = "plot_spatial_click",
                              dblclick = "plot_spatial_dblclick", 
                              brush = brushOpts(id = "plot_spatial_brush",resetOnNew = FALSE ))),
                     conditionalPanel(
                       condition='input.plot_table=="Plots" & input.plot_type=="Spatial"',
                       plotOutput('map_kernel')),
                     conditionalPanel(
                       condition='input.plot_table=="Plots" && input.plot_type=="x-y plot"',   
                        plotOutput('plot_xy'))
                   ))),
 
        
#----
#BEGIN DATA QUALITY EVALUATION TABSET PANEL  
#----
          tabPanel("Data Quality Evaluation", 
            sidebarLayout(
              sidebarPanel(width=3,
              tags$br(),tags$br(),
              uiOutput('SaveButtons'),
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
            h4('Select data validation check functions to run'),
            #Checkbox input widget  
            radioButtons("checks", "", choices = c('Summary table', 'Outliers', 'NAs', 'NaNs', 'Unique observations', 
                                                 'Empty variables', 'Lat_Lon units')),
            uiOutput('outlier_column'),
            uiOutput('outlier_subset'),
            uiOutput('outlier_dist'),
            uiOutput('removeNA'),
            uiOutput('removeNAN'),
            uiOutput('removeOutliers'),
            conditionalPanel(
              condition='input.checks=="Outliers"',
              uiOutput("hover_info1"))
            ),#END SIDEBAR LAYOUT             
          mainPanel(width=9,
                    tags$br(), tags$br(),
            verbatimTextOutput("Case"),
            dataTableOutput("output_table_summary"),
            dataTableOutput("output_table_outlier"),
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
             tabPanel("Simple Analyses",
                      sidebarLayout(
                        sidebarPanel(
                           uiOutput('SaveButtonsAnal'),
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
                          tags$br(),tags$br(),
                          selectInput('corr_reg','Show correlations or simple linear regression', choices=c('Correlation','Regression'), selected='Correlation')
                      ),
                        mainPanel(
                          tags$br(),
                          conditionalPanel(
                            condition="input.corr_reg=='Correlation'",
                            tagList(
                              uiOutput('corr_out'),
                              verbatimTextOutput('output_text_corr'),
                              div(DT::dataTableOutput('output_table_corr'), style = "font-size: 75%; width: 100%"),
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
                            ))
                        )
                      ))
      
#----           
        ) ),
    
### SERVER SIDE    
    server = function(input, output, session) {
   
##Pull data functions 
##----
    if(is.character(dat)==TRUE){
          suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), paste0(loc,"/fishset_db.sqlite")))
          dataset <- table_view(dat)
          DBI::dbDisconnect(fishset_db)
        } else {
          dataset <- dat  
        }
    values <- reactiveValues(dataset=dataset)
      # refresh data
    observeEvent(input$refresh, {
      suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), paste0(loc,"/fishset_db.sqlite")))
      values$dataset <- FishSET:::table_view(dat)
      DBI::dbDisconnect(fishset_db)
    }, ignoreInit = F) 
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
        textInput('port_name', label=div(style = "font-size:14px;  font-weight: 400;", 'Enter column name containing port names'), 
                  value='', placeholder = 'Column name')
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
    observe({input$saveData
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
    
    observeEvent(input$subsetData,{
      values$dataset <- values$dataset[,input$output_table_exploration_columns_selected+1]
    })
    
    output$editText <- renderText('To edit table, double click on a cell. \nUse boxes at top of table to subset data by filtering.\nFilter functions will be automatically saved to the FilterTable table in the fishet_db database when the save data button is pushed. \nTo select variables to retain for analyses and modeling, single click on column cells (not column names). \nThen click the "Subset Data" button. \nOnly selected variables will be available in the working data set.\nClick the "Save Data" button to save changes.')

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
    if(length(unique(lubridate::year(FishSET:::date_parser(values$dataset[,grep('date', colnames(values$dataset), ignore.case = TRUE)[1]]))))>1){
        'Year'
    } else { 
        'Month'}
  })
  
   df2l=reactive({
     if(input$p2fun=='No. observations'){
        if(length(unique(lubridate::year(FishSET:::date_parser(values$dataset[,grep('date', colnames(values$dataset), ignore.case = TRUE)[1]]))))>1){
          aggregate(values$dataset[[input$col_select]]~
                lubridate::year(FishSET:::date_parser(values$dataset[,grep('date', colnames(values$dataset), ignore.case = TRUE)[1]])),FUN=length)
        } else {
          aggregate(values$dataset[[input$col_select]]~
                 lubridate::month(FishSET:::date_parser(values$dataset[,grep('date', colnames(values$dataset), ignore.case = TRUE)[1]])),FUN=length)
        }
     } else if(input$p2fun=='No. unique observations'){
        if(length(unique(lubridate::year(FishSET:::date_parser(values$dataset[,grep('date', colnames(values$dataset), ignore.case = TRUE)[1]]))))>1){
          aggregate(values$dataset[[input$col_select]]~
                     lubridate::year(FishSET:::date_parser(values$dataset[,grep('date', colnames(values$dataset), ignore.case = TRUE)[1]])),FUN=function(x) length(unique(x)))
        } else {
         aggregate(values$dataset[[input$col_select]]~
                     lubridate::month(FishSET:::date_parser(values$dataset[,grep('date', colnames(values$dataset), ignore.case = TRUE)[1]])),FUN=function(x) length(unique(x)))
         }
     } else {
        if(length(unique(lubridate::year(FishSET:::date_parser(values$dataset[,grep('date', colnames(values$dataset), ignore.case = TRUE)[1]]))))>1){
           aggregate(values$dataset[[input$col_select]]~
                     lubridate::year(FishSET:::date_parser(values$dataset[,grep('date', colnames(values$dataset), ignore.case = TRUE)[1]])),FUN=function(x) round(length(x)/nrow(values$dataset)*100,2))
        } else {
          aggregate(values$dataset[[input$col_select]]~
                     lubridate::month(FishSET:::date_parser(values$dataset[,grep('date', colnames(values$dataset), ignore.case = TRUE)[1]])),FUN=function(x) round(length(x)/nrow(values$dataset)*100,2))
        }
      }
    })
 
df2m=reactive({
     if(length(unique(lubridate::year(FishSET:::date_parser(values$dataset[,grep('date', colnames(values$dataset), ignore.case = TRUE)[1]]))))>1){
     aggregate(values$dataset[[input$col_select]]~
                 lubridate::year(FishSET:::date_parser(values$dataset[,grep('date', colnames(values$dataset), ignore.case = TRUE)[1]])),FUN=input$p3fun, na.rm=T)
   } else {
     aggregate(values$dataset[[input$col_select]]~
                 lubridate::month(FishSET:::date_parser(values$dataset[,grep('date', colnames(values$dataset), ignore.case = TRUE)[1]])),FUN=input$p3fun,na.rm=T)
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
                  FishSET:::map_kernel('gradient', values$dataset[,c(which(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)', ignore.case=TRUE)==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)', ignore.case=TRUE)))[1], 
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
         annotate_figure(ggpubr::ggarrange(ggplot(values$dataset, aes_string(x=input$reg_exp_select, y=input$reg_resp_select)) + geom_point()+
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
          ncol=2, nrow=1), top=text_grob('Simple linear regression plots', size=14))
       }
     })
  
      output$output_plot_reg <- renderPlot({
        print(plotInputreg())
      })    
###----
     
         
#DATA QUALITY FUNCTIONS
###-----      
#Basic functions   
##----
      na <- function(x) { if (any(apply(values$dataset, 2, function(x) any(is.na(x))))==TRUE) {
        cat("The", names(which(apply(values$dataset, 2, function(x) any(is.na(x)))==TRUE)), "columns contain NAs.\nConsider using na_filter to replace or remove NAs")
      } else {
        cat("No columns in the dataframe contain NAs")
      }}
      
      nan <- function(x) { if (any(apply(values$dataset, 2, function(x) any(is.nan(x))))==TRUE) {
        cat("The", names(which(apply(values$dataset, 2, function(x) any(is.nan(x)))==TRUE)), "columns contain NaNs.\nConsider using nan_filter to replace or remove NaNs")
      } else {
        cat("No columns in the dataframe contain NaNs")
      }}
      
      #Unique observations
      obs <- function(x) { if (dim(values$dataset)[1] == dim(unique(values$dataset))[1]) {
        cat("Each row is a unique choice occurrence. No further action required.")
      } else {
        cat("Each row in dataset is not a unique choice occurrence at haul or trip level.")
      }
      }
      
      #Empty variables
      empty <- function(x) { if (any(apply(values$dataset, 2, function(x) all(is.na(x))) == TRUE)) {
        cat(names(which(apply(values$dataset, 2, function(x) all(is.na(x))) == TRUE)), "is empty. 
            \nConsider removing the column from the data set.")
      } else {
        cat("No empty variables exist in the data set. No further action required.")
      }
      }
      
      #Lat/Lon units
      lat_lon <- function(x) { if(any(is.numeric(colnames(values$dataset[,which(grepl('lat', names(values$dataset), ignore.case=TRUE)==TRUE)])))|
           any(is.numeric(colnames(values$dataset[,which(grepl('lon', names(values$dataset), ignore.case=TRUE)==TRUE)])))==TRUE){
          cat('At least one lat/lon variable is not in degrees. \nUse the function degree() to convert to degrees.')
        } else {
          cat('Latitude and longitude variables in degrees. \nNo further action required.')
        }
      } 
      
##Output to main panel       
      output$Case<-renderPrint({
         if (input$checks=='Summary table') {
          cat("Summary table of NUMERIC variables in data set. \nFilter rows to be saved in output by highlighting desired rows.")
        } else  if (input$checks=='Outliers'){
          cat('Table to assess outliers.', input$column_check, "shown. \nZoom in by highlighting desired area and double clicking. \nDouble click again to reset plot.")
        } else  if (input$checks=='NAs'){
            cat(na(values$dataset))
        } else if(input$checks=='NaNs') {
          cat(nan(values$dataset))
        } else if(input$checks=='Unique observations'){
          cat(obs(values$dataset))
        } else if(input$checks=='Empty variables'){
          cat(empty(values$dataset))
        } else if(input$checks=='Lat_Lon units'){
          cat(lat_lon(values$dataset))
        } else {
          cat('Make a selection in the left hand column')
        } 
      })
##----
     
##Table output
##----
      tableInputSummary <- reactive({
         if (input$checks=='Summary table') { 
          temp <- values$dataset
          stable <- FishSET:::summary_stats(temp) 
          nums <- unlist(lapply(temp, is.numeric))
          stable  <- apply(stable[nums], 2, function(x) gsub(".*:","", x))
          rownames(stable)=c('Min', 'Median','Mean', 'Max','NAs','Unique Obs.', "No. 0's")
          stable <- as.data.frame(as.matrix(stable))
          stable <- as.data.frame((t(stable)))
        } else {
          NULL
        }
      })
      
        output$output_table_summary <- DT::renderDataTable(
          tableInputSummary(), server = FALSE, selection=list(target ='row+column'),rownames=TRUE,
            options = list(autoWidth=FALSE, scrollX=T, responsive=TRUE, pageLength = 25)
                  )
    
       tableInputOutlier <- reactive({
         if (input$checks=='Outliers'){
            table <- FishSET:::outlier_table(values$dataset, input$column_check)
            rownames(table)=table[,2]
            table <- table[,3:10]
            #table <<- table
          } else {
            NULL
          }
       })
        
         output$output_table_outlier <- DT::renderDataTable(
         tableInputOutlier(), server = FALSE, selection='single', rownames=TRUE,
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
            values$dataset$val <- 1:nrow(values$dataset)
             dat_sub <- suppressWarnings(FishSET:::outlier_plot_int(values$dataset, input$column_check, input$dat.remove, input$x.dist, plot_type=1))
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
              values$dataset$val <- 1:nrow(values$dataset)
              dat_sub <- FishSET:::outlier_plot_int(values$dataset, input$column_check, input$dat.remove, input$x.dist, plot_type=1)
              arg.return <- FishSET:::outlier_plot_int(values$dataset, input$column_check, input$dat.remove, input$x.dist, plot_type=2)
              ggplot(dat_sub[dat_sub$Points=='Kept',], aes_string(input$column_check)) + 
                geom_histogram(aes(y = ..density..), na.rm=TRUE, bins=round(nrow(values$dataset)/2)) + arg.return +
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
              values$dataset$val <- 1:nrow(values$dataset)
              temp <- FishSET:::outlier_plot_int(values$dataset, input$column_check, input$dat.remove, input$x.dist, plot_type=3)
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
          hover <- input$plot1_hover
          point <- nearPoints(values$dataset, input$plot1_hover,  threshold = 5, maxpoints = 1, addDist = FALSE)
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
            p(HTML(paste0("<b> value: </b>", point[[input$column_check]], "<br/>")))
          )
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
                        choices= names(values$dataset[1,unlist(lapply(values$dataset, is.numeric))])))
                        })
        output$outlier_subset <- renderUI({
          conditionalPanel(
            condition="input.checks=='Outliers'",
            selectInput('dat.remove', 'Method to subset the data', 
                        choices=c('none', '5_95_quant', '25_75_quant','mean_2SD','median_2SD','mean_3SD','median_3SD'),
                        selected=c('none', '5_95_quant', '25_75_quant','mean_2SD','median_2SD','mean_3SD','median_3SD')[input$output_table_outlier_rows_selected]))
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
      output$removeNA <- renderUI ({
        conditionalPanel(
          condition = "input.checks == 'NAs'",#
           radioButtons('NA_Filter', 'Filter NAs by', choices=c('Remove all', 'Replace with mean'), selected='')
        )
        })
        output$removeNAN <- renderUI ({
        conditionalPanel(
          condition = "input.checks == 'NaNs'",#
          radioButtons('NAN_Filter', 'Filter NaNs by', choices=c('Remove all', 'Replace with mean'), selected='')
        )
        })
        output$removeOutliers <- renderUI ({
        conditionalPanel(condition="input.checks=='Outliers'",
                         checkboxInput('Outlier_Filter', 'Remove outliers', value=FALSE))
      }) 
        #output_table())
     
      
      observeEvent(input$NA_Filter,{
        if(input$NA_Filter=='Remove all'){
          if(any(apply(values$dataset, 2, function(x) any(is.na(x))))==TRUE){
          values$dataset <- FishSET:::na_filter(values$dataset, names(which(apply(values$dataset, 2, function(x) any(is.na(x)))==TRUE)), replace = FALSE, remove = TRUE, over_write=FALSE)  
        } }else {
          if(any(apply(values$dataset, 2, function(x) any(is.na(x))))==TRUE){
          values$dataset <- FishSET:::na_filter(values$dataset,  names(which(apply(values$dataset, 2, function(x) any(is.na(x)))==TRUE)), replace = TRUE, remove = FALSE, over_write=FALSE)
        }}
      })
      
        observeEvent(input$NAN_Filter,{
          if(input$NA_Filter=='Remove all'){
            if(any(apply(values$dataset, 2, function(x) any(is.nan(x))))==TRUE){
            values$dataset <- FishSET:::nan_filter(values$dataset, names(which(apply(values$dataset, 2, function(x) any(is.na(x)))==TRUE)), replace = FALSE, remove = TRUE, over_write=FALSE)  
                  } }else {
                    if(any(apply(values$dataset, 2, function(x) any(is.nan(x))))==TRUE){
           values$dataset <- FishSET:::nan_filter(values$dataset,  names(which(apply(values$dataset, 2, function(x) any(is.nan(x)))==TRUE)), replace = TRUE, remove = FALSE, over_write=FALSE)
                    }}
      })
        
        observeEvent(input$Outlier_Filter,{
          if(input$Outlier_Filter=='TRUE'){
          values$dataset <- FishSET::outlier_remove(values$dataset, input$column_check, dat.remove = input$dat.remove, remove = T, over_write=FALSE)
            }
        })
##----        

####-----        
##Save output       
###----      
        observeEvent(input$saveData, {
          suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"))
          DBI::dbWriteTable(fishset_db, paste0(project, 'MainDataTable'), values$dataset, overwrite=TRUE)
          DBI::dbDisconnect(fishset_db)
        })
        
        
      output$SaveButtons <- renderUI({
        tagList(
          shinySaveButton(id = 'downloadplot', label ='Save plot to folder', title = "", filename = paste0(project,'_', input$checks, '_plot'), filetype = "png"),
          actionButton('downloaddata', label ='Save table to folder as csv')#, filename = paste0(project,'_', input$checks, '_table'),  filetype = "png")
        )
      })
      
      ## Save buttons
     output$SaveButtonsExplore <- renderUI({
         shinySaveButton(id = 'downloadplotExplore', label ='Save plot to folder', title = "", filename = paste0(project, input$plot_type , '_plot'), filetype = "png")
     })
    
      output$SaveButtonsAnal <- renderUI({
        tagList(
          shinySaveButton(id = 'downloadplotAnal', label ='Save plot to folder', title = "", filename = paste0(project,'_', input$corr_reg, '_plot'), filetype = "png"),
          shinySaveButton(id = 'downloaddataAnal', label ='Save table to folder as csv')
        )
      })
      
###----        

##Downloads      
##----
      observeEvent(input$downloadplot, {
        volumes <- c("UserFolder"=paste0(loc, "/inst/output/"))
        shinyFileSave(input, "downloadplot", roots=volumes, session=session)
        fileinfo <- parseSavePath(volumes, input$downloadplot)
        if (nrow(fileinfo) > 0) {
          device <- function(..., width, height) grDevices::png(..., width = 10, height = 8, res = 300, units = "in")
          ggplot2::ggsave(FishSET:::outlier_plot(values$dataset, input$column_check, input$dat.remove, input$x.dist), 
                          filename = as.character(fileinfo$datapath), device=device)
        }
      })
  
      observeEvent(input$downloadplotAnal, {
        volumes <- c("UserFolder"=paste0(loc, "/inst/output/"))
        shinyFileSave(input, "downloadplotAnal", roots=volumes, session=session)
        fileinfo <- parseSavePath(volumes, input$downloadplotAnal)
        if (nrow(fileinfo) > 0) {
          if(input$corr_reg=='Correlation'){
              fig <- plotInputcorr()
          } else if(input$corr_reg=='Regression'){
             fig <- plotInputreg()
          }
          device <- function(..., width, height) grDevices::png(..., width = 9, height = 6, res = 300, units = "in")
          ggplot2::ggsave(fig, filename = as.character(fileinfo$datapath), device=device)
        }
      })
      
      
      observeEvent(input$downloadplotExplore, {
        volumes <- c("UserFolder"=paste0(loc, "/inst/output/"))
        shinyFileSave(input, "downloadplotExplore", roots=volumes, session=session)
        fileinfo <- parseSavePath(volumes, input$downloadplotExplore)
        if (nrow(fileinfo) > 0) {
         if(input$plot_type=='Temporal'){
          fig <- plotInput_time() 
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
          
             fig <- suppressWarnings(ggpubr::ggarrange(p1, plotInput_kernel(),ncol =2, nrow = 1))
        } else if(input$plot_type=='x-y plot'){
            fig <-plotInput_xy()
        }
          device <- function(..., width, height) grDevices::png(..., width = 12, height = 4, res = 300, units = "in")
          ggplot2::ggsave(fig, filename = as.character(fileinfo$datapath), device=device)
        }
      })
      
        observeEvent(input$downloaddata, {
          if(input$checks=='Summary table'){
              write.csv(tableInputSummary(), paste0(loc,'/inst/output/summary_table.csv'))
          } else if(input$checks=='Outliers'){
             write.csv(tableInputOutlier(), paste0(loc, '/inst/output/outlier_table.csv'))
          }
        })
        
        observeEvent(input$downloaddataAnal,{
          write.csv(tableInputCorr(), paste0(loc, '/inst/output/correlation_table.csv'))
        })
##----
            # stop shiny
      observe({
        if (input$close > 0) stopApp()
      })
      
    }
      )
}
