# model_design_gui
#' View and select models to run
#'
#' @param dat Name of table in database containing model measures of fit
#' @param project If FALSE, appends model selection to out.mod table. If true, overwrites new table with model selection columns. Set to TRUE if models are deleted or selected models are changed.
#' @import shiny 
#' @importFrom DBI dbExistsTable dbDisconnect dbConnect dbRemoveTable dbExecute dbGetQuery
#' @importFrom DT DTOutput renderDT JS
# @import shinyjs 
#' @importFrom shinyjs useShinyjs reset
#' @export
#' @details Opens an interactive session to specify data and parameters for models. Once all parameters have been chosen, select the 'save model and add new model' button
#' to save the selections. A new model can then be specified.
#' Click the 'run model' button to run the models.
#' @examples
#' \dontrun{
#' model_design('pollockMainDataTable', 'pollock')
#' }

model_design <- function(dat, project){

#library(shinyjs)
  #requireNamespace(shiny)
 #resettable

#dat <- pcodMainDataTable


  #Call in datasets
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset

  
  model_table <- data.frame('mod_name'='', 'likelihood'='', 'alternatives'='', 'optimOpts'='', 'inits'='', 
                            'vars1'='','vars2'='', 'catch'='', 'lon'='', 'lat'='', 'project'='', 'price'='', 'startloc'='', 'polyn'='')#,
  shinyApp(
    ui = fluidPage(
      shinyjs::useShinyjs(),
#----
#Expected Catch
#----
        tabsetPanel(
          tabPanel("Expected Catch/Revenue",
                   sidebarLayout(
                     sidebarPanel(
                      actionButton("submitE", "Run expected catch/revenue function", style="color: #fff; background-color: #6da363; border-color: #800000;"), 
                      tags$br(),tags$br(),
                      tags$p('Compute expected catch for each observation and zone. 
                             Function returns the expected catch or expected revenude data frame based on selected parameters along with three null functions: 
                             expected catch/revenue based on catch of the previous two day (short-term expected catch) .
                             expected catch/revemnue based on catch for the previous seven days (medium-term expected catch), and 
                             expected catch/revenue based on catch in the previous year (long-term expected catch).
                             Output saved in fishset_db sqLite database. Previously saved expected catch/revenue output will be written over if the', 
                             tags$i('Replace previously saved'), 'box is unchecked. Checking this box will add new output to existing output.')
                     ),
                mainPanel(
                      h4("Create an expectation of catch or revenue for alternative choices"),
                       fileInput("fileGridExC", "Choose data file containing spatial data defining zones (shape, json, and csv formats are supported)",
                            multiple = FALSE, placeholder = ''),
                      selectizeInput('catche','Catch variable for averaging',
                                   choices=colnames(dataset[,grep('haul|mt|lb|metric|pounds|catch', colnames(dataset), ignore.case=TRUE)])),
                      selectizeInput('price', 'If expected revenue is to be calculated, select variable containing price or value data', 
                                     choices=c('', colnames(dataset[,grep('value|dollar', colnames(dataset), ignore.case=TRUE)]))),
                       #h5('Compute expectations for the entire fleet or by defined groups'),
                       selectizeInput('group','Choose variable that defines groups',
                                    choices=c('Fleet (no group)', names(dataset[, !sapply(dataset, is.numeric)]))),
                       
                       h4('Temporal options'),
                       h5('Use the entire temporal record of catch or take the timeline of catch into account. 
                          When timeline in considered catch can be calculated as the moving average where 
                          catch for a given day is the average for the defined number of days (window), 
                          shifted to the past by the defined number of days (lag). For example, a window of 3 days and lag of 1 day means we take the 
                          average catch of the three days priors to the given date.'),
                       div(style = "margin-left:19px;font-size: 12px", 
                           selectInput('temporal', 'Method to sort time:', c('Entire record of catch (no time)', 'Daily timeline', 'Sequential order'))),
                       conditionalPanel(condition="input.temporal!='Entire record of catch (no time)'",
                                        style = "margin-left:19px;font-size: 12px", selectInput('temp.var', 'Temporal variable for averaging', 
                                                              choices=c(names(dataset)[grep('date|year|hour|day', colnames(dataset), ignore.case = TRUE)]))),
                       conditionalPanel(condition="input.temporal!='Entire record of catch (no time)'",
                                        style = "margin-left:19px;font-size: 12px",
                                        numericInput('temp.year', 'No. of years to go back if expected catch based on from previous year(s) catch ', value=0, min=0, max='')),
                       #if(input$temporal!='Entire record of catch (no time)') {h5('Moving window averaging parameters')},
                       conditionalPanel(condition="input.temporal!='Entire record of catch (no time)'",
                                        style = "margin-left:19px;font-size: 12px", numericInput('temp.window', 'Window size (days) to average over', value = 7, min=0)),
                       conditionalPanel(condition="input.temporal!='Entire record of catch (no time)'", 
                                        style = "margin-left:19px;font-size: 12px", numericInput('temp.lag', 'Time lag (in days) ', value = 0, min=0, max='')),
                       conditionalPanel(condition="input.temporal!='Entire record of catch (no time)'", 
                                        style = "margin-left:19px;font-size: 12px", selectInput('calc.method','Expectation calculation:', 
                                                    choices = c("Standard average"="standardAverage", "Simple lag regression of means"="simpleLag"#, 
                                                                #"Weights of regressed groups"="weights"
                                                                 ))), 
                       conditionalPanel(condition="input.temporal!='Entire record of catch (no time)'", 
                                        selectInput('lag.method', 'Method to average across time steps', choices= c("Entire time period"="simple", "Grouped time periods"="grouped"))),
                        
                       h4('Averaging options'),
                       div(style = "margin-left:19px; font-size: 12px", selectInput('empty.catch', 'Replace empty catch with:', 
                                   choices = c("NA: NA's removed when averaging"='NA', '0', 'Mean of all catch' ="allCatch", 'Mean of grouped catch' = "groupedCatch"))), 
                      #h6("Note: Na's removed when averaging"), 
                      h4('Expected Catch/Dummy options'), 
                        div(style = "margin-left:19px; font-size: 12px",selectInput('empty.expectation', 'Replace empty expected catch with:', 
                                                                                    choices = c("NA: NA's removed when averaging"='NA', 1e-04, 0))),  
                      #h6("Note: Na's removed when averaging"),
                         div(style = "margin-left:19px; font-size: 14px",
                             checkboxInput('dummy.exp', 'Output dummy variable for originally missing values?', value=FALSE)),
                             checkboxInput('replace.output', 'Replace previously saved expected catch output with new output', value=FALSE)
                  )

        )),
#----
#Model Parameters
#----
          tabPanel("Select model parameters",
          sidebarLayout(
          sidebarPanel(
            tags$br(),tags$br(),
            actionButton("addModel", "Save model and add new model", style="color: #fff; background-color: #337ab7; border-color: #800000;"),
            tags$br(),tags$br(),
            actionButton("resetModel", "Clear choices"),
            tags$br(),tags$br(),
            actionButton("submit", "Run model(s)", style="color: #fff; background-color: #6da363; border-color: #800000;"),
            tags$br(),tags$br(),
            tags$button(
              id = 'close',
              type = "button",
              style="color: #fff; background-color: #FF6347; border-color: #800000;",
              class = "btn action-button",
              onclick = "setTimeout(function(){window.close();},500);",  # close browser
              "Close window"
          ),
            tags$br(),tags$br(),
            tags$p(tags$strong("More information"), tags$br(),
                             "Model parameter table is editable. Double click a cell to edit.")
        ),
        mainPanel(
          div(id = "form",
              h4('Alternative choice matrix parameters'),
              selectInput("alternatives", label = "Create alternative choice matrix from",
                          choices = list("Loaded data" = 'loadedData', "Grid data" = "griddedData"),
                          selected = 'loadedData'),
              conditionalPanel(
                condition="input.alternatives=='loadedData'",
                div(style="display: inline-block;vertical-align:top; width: 200px;",
                    selectInput('lat', 'Occurrence latitude', choices=c('',colnames(dataset[,grep('lat', colnames(dataset), ignore.case=TRUE)])), selected='')),
                div(style="display: inline-block;vertical-align:top; width: 200px;",
                    selectInput('lon', 'Occurrence longitude', choices=c('',colnames(dataset[,grep('lon', colnames(dataset), ignore.case=TRUE)])), selected=''))
              ),
              h4('Likelihood function'),
              selectInput("model", label = "",
                          choices = list("Conditional logit" = 'logit_c', "Average catch" = "logit_avgcat", "Logit Dahl correction" = "logit_correction",
                                         'EPM normal'='epm_normal', 'EPM lognormal'='epm_lognormal', 'EPM Weibull'='epm_weibull'),
                          selected = 'logit_c'),
              h4('Select variables to include in model'),
              div(style="display: inline-block;vertical-align:top; width: 250px;", uiOutput('indvariables')),
              div(style="display: inline-block;vertical-align:top; width: 250px;", uiOutput('gridvariables')),
              uiOutput('catch_out'),
              conditionalPanel(
                condition="input.model=='epm_normal' || input.model=='epm_lognormal' || input.model=='epm_weibull'",
                checkboxInput('lockk', 'Location-specific catch parameter', value=FALSE),
                selectInput('price', 'Price variable', choices=c('none', colnames(dataset[,grep('dollar|val|euro|price|cost|earn', colnames(dataset), ignore.case=TRUE)])), 
                                                                 selected='none', multiple=FALSE)
              ),
              conditionalPanel(
                condition="input.model=='logit_correction'",
                numericInput('polyn', 'Correction polynomial degree', value=2),
                selectInput('startloc', 'Initial location during choice occassion', choices=c('none', colnames(dataset)), selected='none', multiple=FALSE) 
              ),
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
              
              DT::DTOutput('table')
          )
          
          
          
        )))
#----
)),

## BEGIN SERVER FILE ##
server = function(input, output, session) {
      
      
#----
#Expected Catch      
#----
  griddataExC <- reactive({
    if(is.null(input$fileGridExC)){return()} 
    type <- sub('.*\\.', '', input$fileGridExC$name)
    if(type == 'shp') { type <- 'shape'} else if(type == 'RData') { type <- 'R'} else { type <- type}
    g <- read_dat(input$fileGrid$datapath, type)
    return(g)
  })

      observeEvent(input$submitE, {
      showNotification('call create expectation function', type='message', duration=10)
        create_expectations(dataset, project, griddataExC(), input$catche, price=input$price, defineGroup=input$group, temp.var=input$temp.var, 
                                     temporal = input$temporal, calc.method = input$calc.method, lag.method = input$lag.method,
                                     empty.catch = input$empty.catch, empty.expectation = input$empty.expectation,  
                                     temp.window = input$temp.window, temp.lag = input$temp.lag, temp.year=input$temp.year, dummy.exp = input$dummy.exp)
    }) 
      

#----
#Model Parameters
#----
      # helper function for making checkbox
      #names <- c('one','two', 'three')
      inline = function (x) {
        tags$div(style="display:inline-block;", x)
      }
      
      output$catch_out <- renderUI({
        selectInput('catch','Variable containing catch data',
                    choices=colnames(dataset[,grep('haul|mt|lb|metric|pounds|catch', colnames(dataset), ignore.case=TRUE)]), selected=input$catche)
      })
      # Data needed
      ## Alternative choices
      if (!exists("Alt")) {
        if (!exists('AltMatrixName')) {
          fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
          Alt <- unserialize(DBI::dbGetQuery(fishset_db, paste0("SELECT AlternativeMatrix FROM ", project, "altmatrix LIMIT 1"))$AlternativeMatrix[[1]])
          DBI::dbDisconnect(fishset_db)
          if (!exists("Alt")) {
            stop("Alternative Choice Matrix does not exist. Please run the createAlternativeChoice() function.")
          }
        }
      }
      choice <- Alt[["choice"]]
      alt <- dim(table(choice))
      
      drop <- grep('date|port|processor|gear|target|lon|lat|permit|ifq', colnames(dataset), ignore.case=TRUE)
      
      intlab <- renderText({
        if(input$model=='logit_c') { label='travel-distance variables'} else { label='travel-distance variables'}
      })
      output$indvariables <- renderUI ({
        intvariables <- c('none', colnames(dataset[,-drop]))
        selectInput('indeVarsForModel', label = intlab(), multiple=TRUE,
                    choices = intvariables, selected = '')#)
      })
      
      gridlab <- renderText({
        if(input$model=='logit_c') { 
          label='alternative-specific variables'} else if(input$model=='logit_avgcat') { 
            label='alternative-specific variables'} else { label='catch-function variables'}
      })
      
      output$gridvariables <- renderUI ({
        gridvariables <- c('none')
        selectInput('gridVariablesInclude', label = gridlab(), multiple=TRUE,
                    choices = gridvariables, selected = '')
      })
      
      numInits <- reactive({
        polyn <- input$polyn
        gridNum <- as.integer(as.factor(length(input$gridVariablesInclude)))
        intNum <- as.integer(length(input$indeVarsForModel))
        if(input$model == 'logit_c'){
          numInits <- gridNum+intNum
        } else if(input$model == 'logit_avgcat') {
          numInits <- gridNum*(alt-1)+intNum
        } else if(input$model == 'logit_correction'){
          numInits <- gridNum*4 + intNum + ((((polyn+1)*2)+2)*4) +1+1
        } else {
          if(input$lockk=='TRUE'){
            numInits <- gridNum*alt+intNum+alt+1
          } else {
            numInits <- gridNum*alt+intNum+1+1
          }
        }
      })
      
      
      output$Inits <- renderUI({
        i = 1:numInits()
        numwidth <- rep(1/numInits()*100, numInits())
        numwidth <- paste("'",as.character(numwidth),"%'",collapse=", ",sep="")
        UI <- paste0("splitLayout(",
                     "cellWidths = c(",numwidth,")",",",
                     paste0("textInput(",
                            "'int", i, "', ",
                            paste0("''"), ",",
                            value=1,
                            #"width='",1/numInits*100,"%'",#50px'",
                            ")",
                            collapse = ", "),
                     ")")
        eval(parse(text = UI))
      })
      
      #mod.table <- data.frame('mod_name'='mod1', 'likelihood'=input$model, 'alternatives'=input$alternatives)#, 'independent vars'=input$indeVarsForModel,
      # 'gridvariables'=input$gridVariablesInclude)
      # Save model and add new model shiny
      #observe({
      #  if (input$addModel > 0) print('Save model, reset parameters')
      #  output$table <- renderDataTable(dat[1:3,1:3])
      #make_model_design()
      #})
      counter <- reactiveValues(countervalue = 0) # Defining & initializing the reactiveValues object
      model_table <- reactiveVal(model_table)
      
      
      #access variable int outside of observer
      int_name <- reactive({
        paste(lapply(1:numInits(), function(i) {
          inputName <- paste("int", i, sep = "")
          input[[inputName]]
        }))
      })
      
      
      observeEvent(input$addModel, {
        showNotification("Selected model parameters saved.", type='message', duration=10)
        if(is.null(input$gridVariablesInclude)|is.null(input$indeVarsForModel)) {
          showNotification('Model not saved as at least one variable not defined.')
        }
        counter$countervalue <- counter$countervalue + 1
        
        if(is.null(input$gridVariablesInclude)|is.null(input$indeVarsForModel)) {
          t <- rbind(data.frame('mod_name'='', 'likelihood'='', 'alternatives'='','optimOpts'='','inits'='',
                                'vars1'= '','vars2'='', 'catch'='','lon'='', 'lat'='', 'project'=project, 'price'='',
                                'startloc'='', 'polyn'='')
                     , model_table())
        } else {
          t = rbind(data.frame('mod_name'=paste0('mod',counter$countervalue), 'likelihood'=input$model, 'alternatives'=input$alternatives,
                               'optimOpts'=paste(input$mIter,input$relTolX, input$reportfreq, input$detailreport),
                               'inits'=paste(int_name(), collapse=','),#noquote(paste0('input$int',1:numInits())),
                               'vars1'= paste(input$indeVarsForModel, collapse=', '),'vars2'=input$gridVariablesInclude, 'catch'=input$catch,
                               'lon'=input$lon, 'lat'=input$lat, 'project'=project, 'price'=input$price,
                               'startloc'=input$startloc, 'polyn'=input$polyn)
                    , model_table())
        }
        model_table(t)
        
        
        ###Now save table to sql database. Will overwrite each time we add a model
        fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
        #First, remove any old instances of the table
        if(DBI::dbExistsTable(fishset_db, paste0(project,'modelDesignTable', format(Sys.Date(), format="%Y%m%d")))==TRUE){
          DBI::dbRemoveTable(DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"), paste0(project, 'modelDesignTable', format(Sys.Date(), format="%Y%m%d")))
        }
        
        if(DBI::dbExistsTable(fishset_db, paste0(project, 'modelDesignTable', format(Sys.Date(), format="%Y%m%d")))==FALSE){
          DBI::dbExecute(fishset_db, paste0("CREATE TABLE ", paste0(project,'modelDesignTable', format(Sys.Date(), format="%Y%m%d")),
                                            "(mod_name TEXT, likelihood TEXT, alternatives TEXT, optimOpts TEXT, inits TEXT, vars1 TEXT, vars2 TEXT,  
                                            catch TEXT, lon TEXT, lat TEXT, project TEXT, price TEXT, startloc TEXT, polyn TEXT)"))
        }
        # Construct the update query by looping over the data fields
        query <- sprintf(
          "INSERT INTO %s (%s) VALUES %s",
          paste0(project,'modelDesignTable', format(Sys.Date(), format="%Y%m%d")),
          paste(names(data.frame(as.data.frame(isolate(model_table())))), collapse = ", "),
          paste0("('", matrix(apply(as.data.frame(isolate(model_table())), 1, paste, collapse="','"), ncol=1), collapse=',', "')")
        )
        # Submit the update query and disconnect
        DBI::dbGetQuery(fishset_db, query)
        DBI::dbDisconnect(fishset_db)
        
        
      })
      
      observeEvent(input$resetModel, {
      shinyjs::reset("form")
      })
      
      output$table <- DT::renderDT(
        model_table(), editable = T
      )
      # Save model and add new model shiny
      observe({
        if (input$submit > 0) {
          print('call model design function, call discrete_subroutine file')
          times <- nrow(model_table())-1
          
          for(i in 1:times){
            make_model_design(dataset, catchID=model_table()$catch[i], alternativeMatrix = model_table()$alternatives[i], lon.dat= model_table()$lon[i], 
                              lat.dat= model_table()$lat[i], project=model_table()$project[i], likelihood=model_table()$likelihood[i], vars1=model_table()$vars1[i],
                              vars2=model_table()$vars2[i], priceCol=model_table()$price[i], startloc=model_table()$startloc[i], polyn=model_table()$polyn[i])
          }
        }
      })
#----      
      # stop shiny
      observe({
        if (input$close > 0) stopApp()
      })
      
    }
  )
 }

