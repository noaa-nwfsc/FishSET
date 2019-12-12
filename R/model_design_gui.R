### NOTES TO SELF - ZONEVAR NEEDS DEFINING
### REORDER PROCESS. NEED DATA TO BE UPDATED WITH ZONE ASSIGNMENT ON ITS OWN
### 

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
   # options(shiny.fullstacktrace = TRUE),
    ui = fluidPage(
      shinyjs::useShinyjs(),
        tabsetPanel(

#-----
#Choices tab
#-----
        tabPanel('NEED NAME',
                 sidebarLayout(
                   sidebarPanel(
                    radioButtons('choiceTab', 'Select XXXX', choices=c('Select catch and price variables'='primary', #basic parameters to populate elsewhere like catch, price
                     'Calculate zonal centroid and assign observations to zones'='zone', #calculate zonal centroid
                     'Select variables that define alternative fishing choices'='distm')),#, #calculate distance matrix
                     #checkboxInput('ExpedCatch', 'Define variables to calculate expected catch', value=FALSE)
                      actionButton('runCentroid','Assign observations to centroids'),
                      actionButton('saveALT','Save choices')
                   ),
                   mainPanel(
                     #We'll in in the choices here
                     #BASELINE
                     uiOutput('conditionalInput1'),
                     #--------#
                     #CENTROID
                    uiOutput('conditionalInput2'),
                    uiOutput('cond2'),
                        #runs assignment column and find_centroid functions
                            # find_centroid(dat, gridfile, lon.dat, lat.dat, cat, lon.grid, lat.grid, weight.var) 
                            #  assignment_column(dat, gridfile, lon.dat, lat.dat, cat, closest.pt, lon.grid, lat.grid, hull.polygon, epsg)
                     #--------#
                     #DISTANCE MATRIX
                    uiOutput('conditionalInput3'),
                    div(style="display: inline-block;vertical-align:top; width: 500px;",
                        conditionalPanel(condition="input.choiceTab=='distm'",plotOutput('zoneIDNumbers_plot'))),
                    div(style="display: inline-block;vertical-align:top; width: 160px;",
                        conditionalPanel(condition="input.choiceTab=='distm'",textOutput('zoneIDText')))
                     #--------#
                     #EXPECTED CATCH
                   )
                 )),
          
#-----
#Alternative choice
#-----
#         tabPanel('Define alternative choices',
#           sidebarLayout(
#             sidebarPanel(
#                 tags$p('The purpose of this function is written here. How it will be used/called.')     
#             ),
#             mainPanel(
           #    h4('Define how alternative fishing choices calculated.'),
           #      selectInput('case_ac', 'Variable which creates alternative choice:', choices=c("Centroid of zonal assignment")),#"Port", "Other"
           #      div(style="display: inline-block;vertical-align:top; width: 160px;",
           #             selectInput('alt_var_ac', 'between occurrence:', choices=c('Centroid of zonal assignment'='centroid', 
           #                                                                        names(dataset)[grep('lat|long|port', names(values$dataset), ignore.case=TRUE)]))), #Identifies how to find lat/lon for starting point (must have a lat/lon associated with it) 
           #      div(style="display: inline-block;vertical-align:top; width: 160px;",
           #               selectizeInput('occasion_ac', tags$div('and:', tags$h6('select centroid or lat/lon of location')), 
           #                              choices=c('Centroid of zonal assignment'='centroid', names(values$dataset)[grep('lat|long', names(values$dataset), ignore.case=TRUE)]), 
           #                           selected='centroid', options = list(maxItems = 2))),
           #      numericInput('min_haul_ac', 'Exclude trips with fewer hauls than', min=1, max=1000, value=1),
           #    checkboxInput('morec', 'Show more choices', value=FALSE),
               #Additional choices
          #     uiOutput('conditionalInput')
                 #create_alternative_choice(dat=values$dataset, gridfile=griddataExC(), case=input$case_ac, min.haul=input$min_haul_ac, alt_var=input$alt_var_ac, 
               #occasion=input$occassion_ac, lon.dat=input$lon_dat_ac, lat.dat=input$lat_dat_ac, lon.grid=input$long_grid_altc[2], lat.grid=input$long_grid_altc[1], 
               #cat=input$cat_altc, hull.polygon=input$hull_polygon_ac, closest.pt=input$closest_pt_ac, project=project, griddedDat=, weight.var=input$weight_var_ac) 
             
#             )
#        )),            
#----
#Expected Catch
#----
          tabPanel("Expected Catch/Revenue",
                   sidebarLayout(
                     sidebarPanel(
                      actionButton("submitE", "Run expected catch/revenue function", style="color: #fff; background-color: #6da363; border-color: #800000;"), 
                      tags$br(),tags$br(),
                      uiOutput('selectcp'),
                       #h5('Compute expectations for the entire fleet or by defined groups'),
                       
                       
                       h4('Temporal options'),
                       h5('Use the entire temporal record of catch or take the timeline of catch into account. 
                          When timeline in considered catch can be calculated as the moving average where 
                          catch for a given day is the average for the defined number of days (window), 
                          shifted to the past by the defined number of days (lag). For example, a window of 3 days and lag of 1 day means we take the 
                          average catch of the three days priors to the given date.'),
                       div(style = "margin-left:19px;font-size: 12px", 
                           selectInput('temporal', 'Method to sort time:', c('Entire record of catch (no time)', 'Daily timeline', 'Sequential order'))),
                    uiOutput('expcatch'),
                       conditionalPanel(condition="input.temporal!='Entire record of catch (no time)'",
                                        style = "margin-left:19px;font-size: 12px",
                                        numericInput('temp.year', 'No. of years to go back if expected catch based on from previous year(s) catch ', value=0, min=0, max='')),
                       #if(input$temporal!='Entire record of catch (no time)') {h5('Moving window averaging parameters')},
                       conditionalPanel(condition="input.temporal!='Entire record of catch (no time)'",
                                        style = "margin-left:19px;font-size: 12px", 
                                        numericInput('temp.window', 'Window size (days) to average over', value = 7, min=0)),
                       conditionalPanel(condition="input.temporal!='Entire record of catch (no time)'", 
                                        style = "margin-left:19px;font-size: 12px", 
                                        numericInput('temp.lag', 'Time lag (in days) ', value = 0, min=0, max='')),
                       conditionalPanel(condition="input.temporal!='Entire record of catch (no time)'", 
                                        style = "margin-left:19px;font-size: 12px", 
                                        selectInput('calc.method','Expectation calculation:', 
                                                    choices = c("Standard average"="standardAverage", "Simple lag regression of means"="simpleLag"#, 
                                                                #"Weights of regressed groups"="weights"
                                                                 ))), 
                      conditionalPanel(condition="input.temporal!='Entire record of catch (no time)'", 
                                        selectInput('lag.method', 'Method to average across time steps', 
                                                    choices= c("Entire time period"="simple", "Grouped time periods"="grouped"))),
                        
                       h4('Averaging options'),
                       div(style = "margin-left:19px; font-size: 12px", 
                           selectInput('empty.catch', 'Replace empty catch with:', 
                                   choices = c("NA: NA's removed when averaging"='NA', '0', 'Mean of all catch' ="allCatch", 'Mean of grouped catch' = "groupedCatch"))), 
                      #h6("Note: Na's removed when averaging"), 
                      h4('Expected Catch/Dummy options'), 
                        div(style = "margin-left:19px; font-size: 12px",
                            selectInput('empty.expectation', 'Replace empty expected catch with:', choices = c("NA: NA's removed when averaging"='NA', 1e-04, 0))),  
                      #h6("Note: Na's removed when averaging"),
                        div(style = "margin-left:19px; font-size: 14px",
                             checkboxInput('dummy.exp', 'Output dummy variable for originally missing values?', value=FALSE)),
                        checkboxInput('replace.output', 'Replace previously saved expected catch output with new output', value=FALSE)
                  ),
                mainPanel(
                      tags$br(),tags$br(),
                      tags$p('Compute expected catch for each observation and zone. 
                             Function returns the expected catch or expected revenude data frame based on selected parameters along with three null functions: 
                             expected catch/revenue based on catch of the previous two day (short-term expected catch),
                             expected catch/revemnue based on catch for the previous seven days (medium-term expected catch), and 
                             expected catch/revenue based on catch in the previous year (long-term expected catch).
                             Output saved in fishset_db sqLite database. Previously saved expected catch/revenue output will be written over if the', 
                             tags$i('Replace previously saved'), 'box is unchecked. Checking this box will add new output to existing output.'),
                      tags$br(), tags$br(),
                      DTOutput('spars_table'),
                      plotOutput('spars_plot')
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
              uiOutput('latlonB'),
              h4('Likelihood function'),
              selectInput("model", label = "",
                          choices = list("Conditional logit" = 'logit_c', "Average catch" = "logit_avgcat", "Logit Dahl correction" = "logit_correction",
                                         'EPM normal'='epm_normal', 'EPM lognormal'='epm_lognormal', 'EPM Weibull'='epm_weibull'),
                          selected = 'logit_c'),
              h4('Select variables to include in model'),
              div(style="display: inline-block;vertical-align:top; width: 250px;", uiOutput('indvariables')),
              div(style="display: inline-block;vertical-align:top; width: 250px;", uiOutput('gridvariables')),
              uiOutput('catch_out'),
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
 
#---
 # DATA - this will have to be changed when we move GUIs together 
#---
  values <- reactiveValues(dataset=dataset)
  # refresh data
  observeEvent(input$runCentroid, {
  
    values$dataset <- assignment_column(values$dataset, gridfile=griddataExC(), lon.dat=input$lon_dat_ac, lat.dat=input$lat_dat_ac, cat=input$cat_altc, closest.pt=input$closest_pt_ac, 
                                          lon.grid=input$long_grid_altc, lat.grid=input$lat_grid_altc, hull.polygon=input$hull_polygon_ac)
    showNotification('Zone assignment completed', type='message', duration=10)
  }, ignoreInit = F) 
  
  
  
#----
#Make choices
#----
  
  output$conditionalInput1 <- renderUI({
    conditionalPanel(condition="input.choiceTab=='primary'",
      tagList(
      selectizeInput('catchBase','Variable containing catch data',
                     choices=colnames(values$dataset[,grep('haul|mt|lb|metric|pounds|catch', colnames(values$dataset), ignore.case=TRUE)])),
      selectizeInput('priceBase', 'Variable containing price or value data', 
                     choices=c('none selected'='none', colnames(values$dataset[,grep('value|dollar', colnames(values$dataset), ignore.case=TRUE)]))),
      div(style="display: inline-block;vertical-align:top; width: 200px;",
          selectInput('latBase', 'Occurrence latitude', choices=c('',colnames(values$dataset[,grep('lat', colnames(values$dataset), ignore.case=TRUE)])), selected='')),
      div(style="display: inline-block;vertical-align:top; width: 200px;",
          selectInput('lonBase', 'Occurrence longitude', choices=c('',colnames(values$dataset[,grep('lon', colnames(values$dataset), ignore.case=TRUE)])), selected=''))
      ))
  })
  output$conditionalInput2 <- renderUI({
    conditionalPanel(condition="input.choiceTab=='zone'",
      tagList(
      fileInput("fileGridExC", "Choose data file containing spatial data defining zones (shape, json, and csv formats are supported)",
                multiple = FALSE, placeholder = ''),
      h5(tags$b('Select latitude than longitude from main dataset for assigning observations to zones')),
      div(style="display: inline-block;vertical-align:top; width: 200px;",
          selectizeInput('lat_dat_ac', '',
                     choices=c(input$latBase, names(values$dataset)[grep('lat', names(values$dataset), ignore.case=TRUE)]), 
                     selected=c(input$latBase))),
      div(style="display: inline-block;vertical-align:top; width: 200px;",
          selectizeInput('lon_dat_ac', '', choices=c(input$lonBase, names(values$dataset)[grep('lon', names(values$dataset), ignore.case=TRUE)]), 
                     selected=c(input$lonBase))),
      selectInput('cat_altc', 'Individual areas/zones from the spatial data set', choices=names(as.data.frame(griddataExC()))),
      selectInput('weight_var_ac', 'If desired, variable for use in calculcating weighted centroids', choices=c('none'="", colnames(values$dataset))), #variable weighted centroids
      checkboxInput('hull_polygon_ac', 'Use convex hull method to create polygon?', value=FALSE),
      checkboxInput('closest_pt_ac', 'Use closest polygon to point?', value=FALSE) 
      ) )
  })   
  
  output$cond2 <- renderUI({
    conditionalPanel(condition="input.choiceTab=='zone'",
    if(any(class(griddataExC())=='sf')==FALSE){
      tagList(
        h5(tags$b('Select vector containing latitude then longitude from spatial data set')),
        div(style="display: inline-block;vertical-align:top; width: 200px;",
            selectizeInput('lat_grid_altc', '', choices=names(as.data.frame(griddataExC())), multiple=TRUE)),
        div(style="display: inline-block;vertical-align:top; width: 200px;",
            selectizeInput('long_grid_altc',  '',choices=names(as.data.frame(griddataExC())), multiple=TRUE))
      )
      }
    ) 
  })
  output$conditionalInput3 <- renderUI({
    conditionalPanel(condition="input.choiceTab=='distm'",
      tagList(
      h5(tags$b('Define how alternative fishing choices calculated.')),
      selectInput('case_ac', 'Variable which creates alternative choice:', choices=c("Centroid of zonal assignment")),#"Port", "Other"
      div(style="display: inline-block;vertical-align:top; width: 160px;",
          selectInput('alt_var_ac', 'between occurrence:', choices=c('Centroid of zonal assignment'='centroid', 
                                                                     names(values$dataset)[grep('lat|lon|port', names(values$dataset), ignore.case=TRUE)]))), #Identifies how to find lat/lon for starting point (must have a lat/lon associated with it) 
      div(style="display: inline-block;vertical-align:top; width: 170px;",
          selectizeInput('occasion_ac', tags$div('and:', tags$h6('select centroid or lat/lon of location')), 
                         choices=c('Centroid of zonal assignment'='centroid', names(values$dataset)[grep('lat|lon', names(values$dataset), ignore.case=TRUE)]), 
                         selected='centroid', options = list(maxItems = 2))),
      numericInput('min_haul_ac', 'Include zones with more hauls than', min=1, max=1000, value=1),
      checkboxInput('morec', 'Show more choices', value=FALSE),
      #Additional choices
      uiOutput('conditionalInput')
      )
    )
  })

  #input$min_haul_ac
  zoneIDNumbers_dat <- reactive({
    if(!any(colnames(values$dataset)=='ZoneID')){
      return()
      #warning('This step cannot be completed. Observations not assigned to zones.')
    } else {
      temp <- data.frame(table(values$dataset$ZoneID))
      ggplot(values$dataset[which(values$dataset$ZoneID %in% temp[which(temp$Freq > input$min_haul_ac),1]), ], aes(x=ZoneID)) + geom_histogram() + 
        theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    }
  })
  
  output$zoneIDText <- renderText({
    if(!any(colnames(values$dataset)=='ZoneID')){
      return()
    } else {
      temp <- data.frame(table(values$dataset$ZoneID))
      print(input$min_haul_ac)
      print(head(values$dataset))
      paste('number of records:',dim(values$dataset[which(values$dataset$ZoneID %in% temp[which(temp$Freq > input$min_haul_ac),1]), ])[1], 
            '\nnumber of zones:', nrow(temp[which(temp$Freq > input$min_haul_ac),]))
    }
  })
    
  output$zoneIDNumbers_plot <- renderPlot(zoneIDNumbers_dat())
  
  griddataExC <- reactive({
    if(is.null(input$fileGridExC)){return()} 
    type <- sub('.*\\.', '', input$fileGridExC$name)
    if(type == 'shp') { type <- 'shape'} else if(type == 'RData') { type <- 'R'} else { type <- type}
    g <- read_dat(input$fileGridExC$datapath, type)
    return(g)
  })
##  output$altchoiceopts <- renderUI({
#    tagList(
#      selectizeInput('lon_dat_ac', 'Select latitude than longitude from main values$dataset for assigning observations to zones',
#                     choices=c(input$latBase, input$lonBase, names(values$dataset)[grep('lat|long', names(values$dataset), ignore.case=TRUE)]), 
#                     selected=c(input$latBase, input$lonBase), options = list(maxItems = 2)),
#        selectInput('cat_altc', 'Individual areas/zones from the spatial data set', choices=names(as.data.frame(griddataExC()))),
#    if(any(class(griddataExC())=='sf')==FALSE){
#        selectizeInput('long_grid_altc', 'Select vector containing latitude then longitude from spatial data set',
#                                                                   choices=names(as.data.frame(griddataExC())), multiple=TRUE, options = list(maxItems = 2))
#    } 
#    )
#  })
   
#----
  
#-----
#Alternative Choice
#----
 
#  output$conditionalInput <- renderUI({
#    if(input$morec){
#      fileInput('griddedDat', 'Choose gridded data set', multiple=FALSE, placeholder='')
#    }
#  })
#----
#Expected Catch      
#----
  output$selectcp <- renderUI({
    tagList(
      selectizeInput('catche','Catch variable for averaging',
                     choices=c(input$catchBase, colnames(values$dataset[,grep('haul|mt|lb|metric|pounds|catch', colnames(values$dataset), ignore.case=TRUE)])),
                     selected=input$catchBase),
      selectizeInput('price', 'If expected revenue is to be calculated, variable containing price or value data', 
                     choices=c(input$priceBase, colnames(values$dataset[,grep('value|dollar', colnames(values$dataset), ignore.case=TRUE)]))),
      selectizeInput('group','Choose variable that defines groups',
                     choices=c('Fleet (no group)', names(values$dataset[, !sapply(values$dataset, is.numeric)])))
    )
  })
  output$expcatch <-  renderUI({
    conditionalPanel(condition="input.temporal!='Entire record of catch (no time)'",
                     style = "margin-left:19px;font-size: 12px", 
                     selectInput('temp.var', 'Temporal variable for averaging', 
                                 choices=c(names(values$dataset)[grep('date|year|hour|day', colnames(values$dataset), ignore.case = TRUE)])))
  })
  sparstable_dat <- reactive({
    if(!any(colnames(values$dataset)=='ZoneID')){
      return()
    } else if(is.null(input$catche)){
      return()
    } else if(is.null(input$temp.var)){
      return()
    } else{
      sparsetable(values$dataset, timevar=input$temp.var, zonevar='ZoneID', var=input$catche)
    }
  })
    
  output$spars_table <- DT::renderDT(sparstable_dat())
  output$spars_plot <- renderPlot({
    if(!any(colnames(values$dataset)=='ZoneID')){
      return()
    } else if(is.null(input$catche)){
      return()
    } else if(is.null(input$temp.var)){
      return()
    } else {
    print(sparsplot(sparsetable(values$dataset, timevar=input$temp.var, zonevar='ZoneID', var=input$catche)))
    }
  })

  observeEvent(input$submitE, {
      showNotification('call create expectation function', type='message', duration=10)
        create_expectations(values$dataset, project, griddataExC(), input$catche, price=input$price, defineGroup=input$group, temp.var=input$temp.var, 
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
       # browser()
        tagList(
        selectInput('catch','Variable containing catch data',
                    choices=c(input$catchBase, colnames(values$dataset[,grep('haul|mt|lb|metric|pounds|catch', colnames(values$dataset), ignore.case=TRUE)])), 
                    selected=input$catchBase),
        conditionalPanel(
          condition="input.model=='epm_normal' || input.model=='epm_lognormal' || input.model=='epm_weibull'",
          checkboxInput('lockk', 'Location-specific catch parameter', value=FALSE)),
        conditionalPanel(
          selectInput('price', 'Price variable', choices=c(input$priceBase,'none', colnames(values$dataset[,grep('dollar|val|euro|price|cost|earn', colnames(values$dataset), ignore.case=TRUE)])), 
                      selected='none', multiple=FALSE)
        ),
        conditionalPanel(
          condition="input.model=='logit_correction'",
          numericInput('polyn', 'Correction polynomial degree', value=2)),
        conditionalPanel(
          selectInput('startloc', 'Initial location during choice occassion', choices=c('none', colnames(values$dataset)), selected='none', multiple=FALSE) 
        )
        )
      })
      output$latlonB <- renderUI({
        conditionalPanel(
          condition="input.alternatives=='loadedData'",
          div(style="display: inline-block;vertical-align:top; width: 200px;",
              selectInput('lat', 'Occurrence latitude', choices=c(input$latBase, colnames(values$dataset[,grep('lat', colnames(values$dataset), ignore.case=TRUE)])), selected='')),
          div(style="display: inline-block;vertical-align:top; width: 200px;",
              selectInput('lon', 'Occurrence longitude', choices=c(input$lonBase, colnames(values$dataset[,grep('lon', colnames(values$dataset), ignore.case=TRUE)])), selected=''))
        )
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
      
      drop <- reactive({grep('date|port|processor|gear|target|lon|lat|permit|ifq', colnames(values$dataset), ignore.case=TRUE)})
      
      intlab <- renderText({
        if(input$model=='logit_c') { label='travel-distance variables'} else { label='travel-distance variables'}
      })
      output$indvariables <- renderUI ({
        intvariables <- c('none', colnames(values$dataset[,-drop()]))
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
            make_model_design(values$dataset, catchID=model_table()$catch[i], alternativeMatrix = model_table()$alternatives[i], lon.dat= model_table()$lon[i], 
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

