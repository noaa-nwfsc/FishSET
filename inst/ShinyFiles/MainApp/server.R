
    ### SERVER SIDE    
    server = function(input, output, session) {
      options(shiny.maxRequestSize = 8000*1024^2)
      
      #Disable buttons
      toggle_inputs <- function(input_list,enable_inputs=T){
        # Toggle elements
        for(x in names(input_list))
          if(enable_inputs){
            shinyjs::enable(x)} else {
              shinyjs::disable(x) }
      }
      
      #inline scripting 
      #----
      r <- reactiveValues(done = 0, ok = TRUE, output = "")
      observeEvent(input$runI, {
        shinyjs::hide("error")
        r$ok <- FALSE
        tryCatch(
          {
            r$output <- isolate(
              paste(utils::capture.output(eval(parse(text = input$expr))), collapse = '\n')
            )
            r$ok <- TRUE
          },
          error = function(err) {r$output <- err$message}
        )
        r$done <- r$done + 1
      })
      
      output$resultI <- renderUI({
        if(r$done > 0 ) { 
          content <- paste(paste(">", isolate(input$expr)), r$output, sep = '\n')
          if(r$ok) {
            pre(content)
          } else {
            pre( style = "color: red; font-weight: bold;", content)
          }
        }
      })
      
      observeEvent(input$runQA, {
        shinyjs::hide("error")
        r$ok <- FALSE
        tryCatch(
          {
            r$output <- isolate(
              paste(capture.output(eval(parse(text = input$exprQA))), collapse = '\n')
            )
            r$ok <- TRUE
          },
          error = function(err) {r$output <- err$message}
        )
        r$done <- r$done + 1
      })
      output$resultQA <- renderUI({
        if(r$done > 0 ) { 
          content <- paste(paste(">", isolate(input$exprQA)), r$output, sep = '\n')
          if(r$ok) {
            pre(content)
          } else {
            pre( style = "color: red; font-weight: bold;", content)
          }
        }
      })
      
      observeEvent(input$runA, {
        shinyjs::hide("error")
        r$ok <- FALSE
        tryCatch(
          {
            r$output <- isolate(
              paste(capture.output(eval(parse(text = input$exprA))), collapse = '\n')
            )
            r$ok <- TRUE
          },
          error = function(err) {r$output <- err$message}
        )
        r$done <- r$done + 1
      })
      output$resultA <- renderUI({
        if(r$done > 0 ) { 
          content <- paste(paste(">", isolate(input$exprA)), r$output, sep = '\n')
          if(r$ok) {
            pre(content)
          } else {
            pre( style = "color: red; font-weight: bold;", content)
          }
        }
      })
      
      observeEvent(input$runN, {
        shinyjs::hide("error")
        r$ok <- FALSE
        tryCatch(
          {
            r$output <- isolate(
              paste(capture.output(eval(parse(text = input$exprN))), collapse = '\n')
            )
            r$ok <- TRUE
          },
          error = function(err) {r$output <- err$message}
        )
        r$done <- r$done + 1
      })
      output$resultN <- renderUI({
        if(r$done > 0 ) { 
          content <- paste(paste(">", isolate(input$exprN)), r$output, sep = '\n')
          if(r$ok) {
            pre(content)
          } else {
            pre( style = "color: red; font-weight: bold;", content)
          }
        }
      })
      
      observeEvent(input$runZ, {
        shinyjs::hide("error")
        r$ok <- FALSE
        tryCatch(
          {
            r$output <- isolate(
              paste(capture.output(eval(parse(text = input$exprZ))), collapse = '\n')
            )
            r$ok <- TRUE
          },
          error = function(err) {r$output <- err$message}
        )
        r$done <- r$done + 1
      })
      output$resultZ <- renderUI({
        if(r$done > 0 ) { 
          content <- paste(paste(">", isolate(input$exprZ)), r$output, sep = '\n')
          if(r$ok) {
            pre(content)
          } else {
            pre( style = "color: red; font-weight: bold;", content)
          }
        }
      })
      
      observeEvent(input$runEC, {
        shinyjs::hide("error")
        r$ok <- FALSE
        tryCatch(
          {
            r$output <- isolate(
              paste(capture.output(eval(parse(text = input$exprEC))), collapse = '\n')
            )
            r$ok <- TRUE
          },
          error = function(err) {r$output <- err$message}
        )
        r$done <- r$done + 1
      })
      output$resultEC <- renderUI({
        if(r$done > 0 ) { 
          content <- paste(paste(">", isolate(input$exprEC)), r$output, sep = '\n')
          if(r$ok) {
            pre(content)
          } else {
            pre( style = "color: red; font-weight: bold;", content)
          }
        }
      })
      
      observeEvent(input$runM, {
        shinyjs::hide("error")
        r$ok <- FALSE
        tryCatch(
          {
            r$output <- isolate(
              paste(capture.output(eval(parse(text = input$exprM))), collapse = '\n')
            )
            r$ok <- TRUE
          },
          error = function(err) {r$output <- err$message}
        )
        r$done <- r$done + 1
      })
      output$resultM <- renderUI({
        if(r$done > 0 ) { 
          content <- paste(paste(">", isolate(input$exprM)), r$output, sep = '\n')
          if(r$ok) {
            pre(content)
          } else {
            pre( style = "color: red; font-weight: bold;", content)
          }
        }
      })
      #----
      
      ##Pull data functions
      ##----
      values <- reactiveValues(
        dataset = data.frame('var1'=0, 'var2'=0)
        )
      
      #Add in reactive values once data  call is is not empty
      observeEvent(input$loadDat, {
        if(input$projectname==''){
          showNotification("Please enter a project name.", type='message', duration=10)
        }
        req(input$projectname)
        if(input$loadmainsource=='FishSET database'){
          if(table_exists(paste0(input$projectname, 'MainDataTable'))==FALSE){
            showNotification('Table not found in FishSET database. Check project spelling.', type='message', duration=15)
          } else {
        values$dataset <- table_view(paste0(input$projectname, 'MainDataTable'))
          }
        } else if(input$loadmainsource=='Upload new file' & !is.null(input$maindat)){
          type <- sub('.*\\.', '', input$maindat$name)
          if(type == 'shp') { type <- 'shape'} else if(type == 'RData') { type <- 'R'} else { type <- type}
          values$dataset <- read_dat(input$maindat$datapath, type)
       }   else {
          values$dataset <- values$dataset
       }
        if(names(values$dataset)[1]!='var1'){
          showNotification("Data loaded.", type='message', duration=10)
        }
      }, ignoreInit = TRUE, ignoreNULL = TRUE) 
      
      
      observeEvent(input$uploadMain, {
        if(input$loadmainsource=='Upload new file'){
          type <- sub('.*\\.', '', input$maindat$name)
          if(type == 'shp') { type <- 'shape'} else if(type == 'RData') { type <- 'R'} else { type <- type}
          values$dataset <- read_dat(input$maindat$datapath, type)
        } else {
          values$dataset <- values$dataset
        }
        if(names(values$dataset)[1]!='var1'){
          showNotification("Data loaded.", type='message', duration=10)
        }
      }, ignoreInit = TRUE, ignoreNULL = TRUE) 
      
      # refresh data
      observeEvent(c(input$refresh,input$refresh1,input$refresh2,input$refreshNew), {
        req(input$projectname)
        temp <- tables_database()[grep(paste0(input$projectname, 'MainDataTable\\d+'), tables_database())][which(
                      unlist(stringr::str_extract_all(tables_database()[grep(paste0(input$projectname, 'MainDataTable\\d+'), 
                      tables_database())], "\\d+"))==max((unlist(stringr::str_extract_all(tables_database()[grep(paste0(input$projectname, 
                      'MainDataTable\\d+'), tables_database())], "\\d+")))))]
        values$dataset <- table_view(temp)
        showNotification("Data refreshed", type='message', duration=10)
      }, ignoreInit = TRUE, ignoreNULL=TRUE) 
     
      #PORT
      ptdat <- reactiveValues(
        dataset = data.frame('var1'=0, 'var2'=0)
      )
      
      observeEvent(input$loadDat, {
        req(input$portdattext)
        if(input$loadportsource=='FishSET database'){
          ptdat$dataset <- table_view(paste0(input$projectname, input$portdattext))
        } else if(input$loadportsource=='Upload new file' & !is.null(input$portdat)){
          type <- sub('.*\\.', '', input$portdat$name)
          if(type == 'shp') { type <- 'shape'} else if(type == 'RData') { type <- 'R'} else { type <- type}
          ptdat$dataset <- read_dat(input$portdat$datapath, type)
          }else {
          ptdat$dataset <- ptdat$dataset
          }
        if(names(pdat$dataset)[1]!='var1'){
          showNotification("Port data loaded.", type='message', duration=10)
        }
      }, ignoreInit = TRUE, ignoreNULL = TRUE) 
      
      observeEvent(input$uploadPort, {
        if(input$loadportsource!='FishSET database'){
        type <- sub('.*\\.', '', input$portdat$name)
        if(type == 'shp') { type <- 'shape'} else if(type == 'RData') { type <- 'R'} else { type <- type}
        ptdat$dataset <- read_dat(input$portdat$datapath, type)
        } else {
          ptdat$dataset <- ptdat$dataset
        }
        if(names(ptdat$dataset)[1]!='var1'){
          showNotification("Port data loaded.", type='message', duration=10)
        }
      }, ignoreInit = TRUE, ignoreNULL = TRUE) 
         
      #SPATIAL
      spatdat <- reactiveValues(
        dataset = data.frame('var1'=0, 'var2'=0)
      )
      
      observeEvent(input$loadDat, {
        #req(input$spatialdattext)
        if(input$loadspatialsource=='FishSET database'){
          spatdat$dataset <- table_view(input$spatialdattext)
        } else if(input$loadspatialsource=='Upload new file' & !is.null(input$spatialdat)){
          type <- sub('.*\\.', '', input$spatialdat$name)
          if(type == 'shp') { type <- 'shape'} else if(type == 'RData') { type <- 'R'} else { type <- type}
          spatdat$dataset <- read_dat(input$spatialdat$datapath, type)
          } else {
          spatdat$dataset <- spatdat$dataset
          }
        if(names(spatdat$dataset)[1]!='var1'){
          showNotification("Map file loaded.", type='message', duration=10)
        }
      }, ignoreInit = TRUE, ignoreNULL = TRUE) 
      
       observeEvent(input$uploadspatial, {
         if(input$loadspatialsource!='FishSET database'){
        type <- sub('.*\\.', '', input$spatialdat$name)
        if(type == 'shp') { type <- 'shape'} else if(type == 'RData') { type <- 'R'} else { type <- type}
        spatdat$dataset <- read_dat(input$spatialdat$datapath, type)
       } else {
         spatdat$dataset <- spatdat$dataset
       }
       if(names(spatdat$dataset)[1]!='var1'){
           showNotification("Map file loaded.", type='message', duration=10)
         }
    }, ignoreInit = TRUE, ignoreNULL = TRUE) 
      
      #GRIDDED      
      grddat <- reactiveValues(
        dataset = data.frame('var1'=0, 'var2'=0)
      )
      observeEvent(input$loadDat, {
        req(input$griddattext)
        if(input$loadgridsource=='FishSET database'){
          grddat$dataset <- table_view(paste0(input$projectname, input$griddattext))
        } else if(input$loadgridsource=='Upload new file' & !is.null(input$griddat)) {
          type <- sub('.*\\.', '', input$griddat$name)
          if(type == 'shp') { type <- 'shape'} else if(type == 'RData') { type <- 'R'} else { type <- type}
          grddat$dataset <- read_dat(input$griddat$datapath, type)
        } else {
          grddat$dataset <- grddat$dataset
        }
        if(names(grddat$dataset)[1]!='var1'){
          showNotification("Gridded data loaded.", type='message', duration=10)
        }
      }, ignoreInit = TRUE, ignoreNULL = TRUE) 
      
      observeEvent(input$uploadGrid, {
        if(input$loadgridsource!='FishSET database'){
          type <- sub('.*\\.', '', input$griddat$name)
          if(type == 'shp') { type <- 'shape'} else if(type == 'RData') { type <- 'R'} else { type <- type}
          grddat$dataset <- read_dat(input$griddat$datapath, type)
         } else {
           grddat$dataset <- grddat$dataset
         }
        if(names(grddat$dataset)[1]!='var1'){
          showNotification("Gridded data loaded.", type='message', duration=10)
        }
    }, ignoreInit = TRUE, ignoreNULL = TRUE) 
         
     
  #Auxiliary  
      aux <- reactiveValues(
        dataset = data.frame('var1'=0, 'var2'=0)
      )
      observeEvent(input$loadDat, {
        req(input$auxdattext)
        if(input$loadauxsource=='FishSET database'){
          aux$dataset <- table_view(paste0(input$projectname, input$auxdattext))
        } else if(input$loadauxsource=='Upload new file' & !is.null(input$auxdat)){
          type <- sub('.*\\.', '', input$auxdat$name)
          if(type == 'shp') { type <- 'shape'} else if(type == 'RData') { type <- 'R'} else { type <- type}
          aux$dataset <-read_dat(input$auxdat$datapath, type)
          } else {
          aux$dataset <- aux$dataset
          }
        if(names(aux$dataset)[1]!='var1'){
          showNotification("Auxiliary data loaded.", type='message', duration=10)
        }
      }, ignoreInit = TRUE, ignoreNULL = TRUE) 
       observeEvent(input$uploadAux, {
         if(input$loadauxsource!='FishSET database'){
            type <- sub('.*\\.', '', input$auxdat$name)
            if(type == 'shp') { type <- 'shape'} else if(type == 'RData') { type <- 'R'} else { type <- type}
            aux$dataset <-read_dat(input$auxdat$datapath, type)
         } else {
           aux$dataset <- aux$dataset
         }
         if(names(aux$dataset)[1]!='var1'){
           showNotification("Auxiliary data loaded.", type='message', duration=10)
         }
       }, ignoreInit = TRUE, ignoreNULL = TRUE) 
  
      ##----     
      
      #Landing Page
      ###----
      output$AcrossTabsText <- renderUI({
        if(input$QuickStartChoices=='AcrossTabs'){
          tags$div(
                 tags$br(), tags$br(),
                 tags$p('All tabs have buttons to close the app and refresh the data. 
                      Refreshing the data pulls the raw, unprocessed data from the FishSET database.'),
                 tags$p('Buttons to save plots and tables to the output folder are provided where appropriate.'), 
                 tags$p('Each tab has a space to write notes and a button to save notes to the', tags$div(title='folder located in FishSET R package directory', 'output folder.')),
                 tags$p('Each tab has an', tags$em('Enter an R expression'), 'area where R code can be run. 
                        Within the FishSET Shiny application, the primary data frame is called', tags$em('values$dataset.')), 
                 tags$p('Some examples. To view the mean of the fifth column type',  tags$em('mean(values$dataset[,5])'), 
                        'and click the', tags$em('Run'), 'button.',
                       tags$br(),
                      'To view summary details of a column called Vessel_Length type', tags$em('summary(values$dataset$Vessel_Length)'), 
                        'and push the', tags$em('Run'), 'button.'), 
                 tags$br(), tags$br(),
                 tags$div(style="display: inline-block; align:left; ", img(src="QuickStart1.png",  height="75%", width="75%"))
                )
        }
      })
     
      output$UploadTabsText <- renderUI({
        if(input$QuickStartChoices=='UploadTab'){ 
          tags$div(
          tags$br(), tags$br(),
            tags$p('To get started, a primary data set must be loaded and a project name must be specified.'),
            tags$p('Upload data sources (primary, port, map, gridded, auxiliary) from the FishSET database or from source.'), 
	          tags$p('To upload from a local file location select', tags$em('Upload new file'), 'and then browse to file location (arrow 1).', 
              tags$br(),
              'Fill in the project name (arrow 2).'),
	          tags$p('To upload from the FishSET database, select', tags$em('FishSET database'), ' (arrow 1) type in project name (arrow 2), and click the',
              tags$em('Load data'), 'button (arrow 3).'),
            tags$div(style="display: inline-block; align:center", img(src="upload.png",  height="75%", width="75%"))
          )
        }
      })
      
      output$ExploreTabsText <- renderUI({
        if(input$QuickStartChoices=='ExplorTab'){ 
          tags$div(
            tags$br(), tags$br(),
            tags$p('View and explore the primary data.',
                tags$br(), 'Use the', tags$em('View data or plots'), 'dropdown box to view data as a table or plot.'),
            tags$p(HTML(paste(tags$h4('Table:'), tags$h5('Edit, remove variables, filter'))),
            tags$br(),
            'Edit cells by double-clicking a cell'),
         tags$br(),
         tags$div(style="display: inline-block; align:center", img(src="Correct.png", height="75%", width="75%")),
         tags$br(),tags$br(),
         tags$p('Remove variables by clicking on the column, then clicking the', tags$em('Remove variable'), 'button. The edited data frame will not be saved unless the', 
            tags$em('Save data'), 'button is clicked. Press the', tags$em('Refresh data'), 'button to restore the original, unprocessed data frame.'),
         tags$br(), 
         tags$div(style="display: inline-block; align:center", img(src="DeleteVar.png", height="75%", width="75%")),
         tags$br(),tags$br(),
         tags$p('Data can be filtered using the', tags$em('boxes'), 'below the variable name. Selected filters are saved when the', tags$em('Save data'), 'button is pushed.
            Modified data is stored as "project, MainDataTable, and date". Raw, unmodified data can be reloaded by pressing the ', tags$em('Refresh data'), 'button.'),
         tags$br(), 
         tags$div(style="display: inline-block; align:center", img(src="Filter.png", height="75%", width="75%")),
         tags$br(),tags$br(),
         tags$p(tags$h4('Plots:'),
          'Temporal, spatial, or x-y plots are available.'), 
         tags$p('The', tags$em('Observed location'), 'spatial plot can be zoomed in on by double clicking a highlighted area.',
         tags$br(), 
         'Click an individual point to identify the latitude and longitude of that point.'),
         tags$br(), 
         tags$div(style="display: inline-block; align:center", img(src="MapZoom.png", height="75%", width="75%")),
         tags$br(),tags$br(),
          tags$p("Make further selections on the left to display global Moran's I (a measure of spatial autocorrelation) and
          GetisOrd (a measure of spatial clustering) statistics."
          )
          )
        }
      })

      output$DQTabsText <- renderUI({
        if(input$QuickStartChoices=='DQTab'){ 
          p(tags$br(), tags$br(),
            'View and correct common data quality issues such as outliers and missing values (arrow 1).',
            tags$br(),tags$br(),
            'Options to remove or correct for data quality issues are displayed before the', tags$em('R expression'), 'box (arrow 2).',
            tags$br(), tags$br(),
            tags$div(style="display: inline-block; align:center", img(src="dq.png", height="75%", width="75%")),
            tags$br(), tags$br(),
            'To save modified data, click the', tags$em('Save data to fishset_db database'), 'button.', 
            tags$br(),
            'The modified data will be saved to the FishSET database with a title based on the project, MainDataTable, and date.',
            'Click on the', tags$em('Refresh data'), 'button to restore the data frame to the original, unmodified state.',
            tags$br(),tags$br(),
          'The outlier plot is interactive. Click on individual points to get values and zoom in to a highlighted area by double clicking.'
          )
        }
      })
      
      output$AnalTabsText <- renderUI({
        if(input$QuickStartChoices=='AnalTab'){ 
          p(tags$br(), tags$br(), 
            'View correlations and simple linear regressions.', 
            tags$br(),tags$br(),
            tags$div(style="display: inline-block; align:center", img(src="Corr.png", height="75%", width="75%")),
            tags$br(), tags$br(),
            'For correlation plots/tables, variables can be removed from the', tags$em('select'), 'box by clicking on the variable and 
            then hitting the backspace or delete button on your keyboard.',
            tags$br(),
            'Variables can be added by clicking an empty space in the', tags$em('select'), 'box.'
            )
        }
      })
      
      output$NewVarsTabsText <- renderUI({
        if(input$QuickStartChoices=='NewVarsTab'){ 
          p(tags$br(), tags$br(),
            'Modify or create new variables such as CPUE or trip mid-point.',
            tags$br(),tags$br(),
            'After a function is created, by clicking the', tags$em('Run function'), 'button (arrow 1), the new variable will be 
            added as the last column of the displayed data table (arrow 2).', 
            tags$br(),tags$br(),
            tags$div(style="display: inline-block; align:center", img(src="NewVar.png", height="75%", width="75%")),
            tags$br(), tags$br(),
            'The created variable will be available for other analyses while the app is open but will not be available for future use unless the', tags$em('save data'), 'button is pushed.',
            tags$br(),tags$br(),
            'Variable creation functions are grouped by type: Arithmetic and temporal, data transformations, dummy variables, nominal ID, spatial, and trip-level.',
            tags$br(), tags$br(),
            'Default variable names will be provided if the', tags$em('Nme of new variable'), 'box is left empty (arrow 3).'
            )
        }
      })
      
      output$ZonalTabsText <- renderUI({
        if(input$QuickStartChoices=='ZonalTab'){ 
          p(tags$br(), tags$br(),
            'Assign observations to zones and define alternative fishing choices.',
            tags$br(), tags$br(),
	         'There are three steps on this tab.',
	         tags$br(), tags$br(),
	         tags$div(style="display: inline-block; align:center", img(src="zonal.png", height="75%", width="75%")),
	         tags$br(), tags$br(),
	         tags$ol(	         
	           tags$li('(Required) Defining zones, calculate the zone or fishing centroids, and assign observations to zones.
               FishSET defaults to geographic centroids. To use fishing centroids, select a variable in the weighted centroid box.'),
	           tags$li('Define the alternative fishing choices. These choices are used to develop the matrix of distances between alternative choices.'),
             tags$li('(Optional) Select catch and price variables, which can be done in this or other tabs.')
	         )
          )
        }
      })
      
      output$ExpectedTabsText <- renderUI({
        if(input$QuickStartChoices=='ExpectedTab'){ 
          p(tags$br(), tags$br(),
            'Define choices to calculate expectations of catch or revenue for alternative choices.',
            tags$br(),tags$br(),
            'Returns the expected catch or expected revenue data frame based on selected parameters along with three null outputs:', 
            tags$br(),
            'expected catch/revenue based on catch of the previous two days (short-term expected catch)', 
            tags$br(),
            'expected catch/revenue based on catch for the previous seven days (medium-term expected catch)', 
            tags$br(),
            'and expected catch/revenue based on catch in the previous year (long-term expected catch).'
            )
        }
      })
      
      output$ModelTabsText <- renderUI({
        if(input$QuickStartChoices=='ModelTab'){ 
          p(tags$br(),tags$br(),
            'Define the likelihood function and model parameters before running models and comparing output.',
            tags$br(),tags$br(),
          'Click', tags$em('Save model and Sdd new model'), 'to save model choices and define another model.',
          tags$br(), tags$br(),
          'Defined models are shown in a table at the bottom of the screen.', 
          tags$br(), tags$br(),
          'Once all models have been defined, select', tags$em('Run models'), 'to run all models.',
          tags$br(), tags$br(),
          'View and compare models in the', tags$em('Compare models'), 'subtab.',
          tags$div(style="display: inline-block; align:center", img(src="CompareModels.png", height="75%", width="75%"))
          )
        }
      })
      
      output$BookmarkTabsText <- renderUI({
        if(input$QuickStartChoices=='BookmarkTab'){ 
          p(tags$br(), tags$br(),
            'Save and reload the FishSET R Shiny application.',
            tags$br(), tags$br(),
            'To bookmark the app, click the bookmark button. Push', tags$em('Dismiss'), 'in the popup message.',
            tags$div(style="display: inline-block; align:center", img(src="Dismiss.png", height="75%", width="75%")),
            tags$br(), tags$br(),tags$br(),
            'To reload the saved state, click', tags$em('Browse'), 'and then migrate to the input.rds file and select', tags$em('Open.'),
            tags$br(), tags$br(),
            tags$div(style="display: inline-block; align:center", img(src="Bookmark2.png", height="75%", width="75%"))
            )
        }
      })
      ###----
      
      #DATA UPLOAD FUNCTIONS
      ###----
      
      output$main_upload <- renderUI({     
        tagList( 
          conditionalPanel(condition="input.loadmainsource=='Upload new file'", 
                           tagList(
                             fluidRow(
                                 column(3, fileInput("maindat", "Choose primary data file",
                                            multiple = FALSE, placeholder = 'Required data')),
                                column(1, uiOutput('ui.action'))
                           ))
          ),     
          conditionalPanel(condition="input.loadmainsource=='FishSET database'", 
                             fluidRow(
                               column(3, textInput("maindatabasedat", "Name of data frame in FishSET database",
                                                   value='', placeholder = 'Optional. Use if loading modified data frame'))
                               
                             )
          ))
      })
      output$ui.action <- renderUI({
        if(is.null(input$maindat)) return()
        actionButton("uploadMain", label = "Save to database", 
                     style = "color: white; background-color: blue;", size = "extra-small")
      })
 #     observeEvent(input$maindat, {
 #       type <- sub('.*\\.', '', input$maindat$name)
 #       if(type == 'shp') { type <- 'shape'} else if(type == 'RData') { type <- 'R'} else { type <- type}
 #       df_data <- FishSET::read_dat(input$maindat$datapath, type)
 #     }) 
      
      output$port_upload <- renderUI({     
        tagList( 
          conditionalPanel(condition="input.loadportsource=='Upload new file'", 
                           tagList(
                             fluidRow(
                               column(3, fileInput("portdat", "Choose port data file",
                                                   multiple = FALSE, placeholder = 'Required data')),
                               column(1, uiOutput('ui.actionP'))
                             ))
          ),
          conditionalPanel(condition="input.loadportsource!='Upload new file'", 
                           tagList(
                             fluidRow(
                               column(3, textInput("portdattext", "Port data file name in database", placeholder = 'Optional data'))
                             ))
          ))
      })
      output$ui.actionP <- renderUI({
        if(is.null(input$portdat)) return()
        actionButton("uploadPort", label = "Save to database", 
                     style = "color: white; background-color: blue;", size = "extra-small")
      })
 
      output$spatial_upload <- renderUI({     
        tagList( 
          conditionalPanel(condition="input.loadspatialsource=='Upload new file'", 
                           tagList(
                             fluidRow(
                               column(3, fileInput("spatialdat", "Choose spatial data file",
                                                   multiple = FALSE, placeholder = 'Suggested data')),
                               column(1, uiOutput('ui.actionS'))
                             ))
          ),
          conditionalPanel(condition="input.loadspatialsource!='Upload new file'", 
                           tagList(
                             fluidRow(
                               column(3, textInput("spatialdattext", "Spatial data file name in database", placeholder = 'Suggested data'))
                             ))
          ))
      })
      output$ui.actionS <- renderUI({
        if(is.null(input$spatialdat)) return()
        actionButton("uploadspatial", label = "Save to database", 
                     style = "color: white; background-color: blue;", size = "extra-small")
      })
      
      output$grid_upload <- renderUI({     
        tagList( 
          conditionalPanel(condition="input.loadgridsource=='Upload new file'", 
                           tagList(
                             fluidRow(
                               column(3, fileInput("griddat", "Choose data file that varies over two dimensions (gridded)",
                                                   multiple = FALSE, placeholder = 'Optional data')),
                               column(1, uiOutput('ui.actionG'))
                             ))
          ),
          conditionalPanel(condition="input.loadgridsource!='Upload new file'", 
                           tagList(
                             fluidRow(
                               column(3, textInput("griddattext", "Gridded data file name in database", placeholder = 'Optional data'))
                             ))
          ))
      })
      output$ui.actionG <- renderUI({
        if(is.null(input$griddat)) return()
        actionButton("uploadGrid", label = "Save to database", 
                     style = "color: white; background-color: blue;", size = "extra-small")
      })
      
      output$ui.actionA <- renderUI({
        if(is.null(input$auxdat)) return()
        actionButton("uploadAux", label = "Save to database", 
                     style = "color: white; background-color: blue;", size = "extra-small")
      })
      output$aux_upload <- renderUI({     
        tagList( 
          conditionalPanel(condition="input.loadauxsource=='Upload new file'", 
                           tagList(
                             fluidRow(
                               column(3, fileInput("auxdat", "Choose auxiliary data file that links to primary data",
                                                   multiple = FALSE, placeholder = 'Optional data')),
                               column(1, uiOutput('ui.actionA'))
                             ))
          ),
          conditionalPanel(condition="input.loadauxsource!='Upload new file'", 
                           tagList(
                             fluidRow(
                               column(3, textInput("auxdattext", "Auxiliary data file name in database", placeholder = 'Optional data'))
                             ))
          )
          )
      })
    
      output$ui.action2 <- renderUI({
        if(is.null(input$maindat)) return()
        tagList(
          textInput('compare', label=div(style = "font-size:14px;  font-weight: 400;", 'If comparing data to previous year, enter saved table name'), 
                    value='', placeholder = 'Saved table name in fishset_db database'),
          checkboxInput('over_write','If file exsits, over write?', value=FALSE)
        )
      })
      output$ui.actionP2 <- renderUI({
        if(is.null(input$portdat)) return()
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
        load_maindata(df_data, over_write=input$over_write, project=input$projectname, compare=df_compare, y=df_y)
      })
      observeEvent(input$uploadPort, {
        type <- sub('.*\\.', '', input$portdat$name)
        if(type == 'shp') { type <- 'shape'} else if(type == 'RData') { type <- 'R'} else { type <- type}
        df_data <- FishSET::read_dat(input$portdat$datapath, type)
        load_port(df_data, input$port_name, over_write=TRUE, project=input$projectname, compare=FALSE, y=NULL)
      }) 
      observeEvent(input$uploadspatial, {
        type <- sub('.*\\.', '', input$spatialdat$name)
        if(type == 'shp') { type <- 'shape'} else if(type == 'RData') { type <- 'R'} else { type <- type}
        df_data <- FishSET::read_dat(input$spatialdat$datapath, type)
        fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase())
        DBI::dbWriteTable(fishset_db, input$spatialdat$name,  df_data, overwrite=TRUE) 
        DBI::dbDisconnect(fishset_db)
      }) 
      observeEvent(input$uploadGrid, {
        type <- sub('.*\\.', '', input$griddat$name)
        if(type == 'shp') { type <- 'shape'} else if(type == 'RData') { type <- 'R'} else { type <- type}
        df_data <- FishSET::read_dat(input$griddat$datapath, type)
        load_grid(paste0(input$projectname, 'MainDataTable'), df_data, over_write=TRUE, project=input$projectname)
      }) 
      observeEvent(input$uploadAux, {
        type <- sub('.*\\.', '', input$auxdat$name)
        if(type == 'shp') { type <- 'shape'} else if(type == 'RData') { type <- 'R'} else { type <- type}
        df_data <- FishSET::read_dat(input$auxdat$datapath, type)
        load_aux(paste0(input$projectname, 'MainDataTable'), df_data, over_write=TRUE, project=input$projectname)
      }) 
      
      ###----
      
      #DATA QUALITY FUNCTIONS
      ###-----      

      output$LatLonDir <- renderUI ({
        tagList(
        conditionalPanel(condition="input.checks=='Lat_Lon units'",
                         selectizeInput('LatDirection','Latitudinal variable', choices=c('None', colnames(values$dataset[,grep('lat', names(values$dataset), ignore.case=TRUE)])))),
        conditionalPanel(condition="input.checks=='Lat_Lon units'",
                         selectizeInput('LonDirection','Longitudinal variable', choices=c('None', colnames(values$dataset[,grep('lon', names(values$dataset), ignore.case=TRUE)]))))
        )
      })
      
      output$output_table_latlon <- DT::renderDT(
        if(colnames(values$dataset)[1] == 'var1') {
          return(NULL)
        } else if(input$checks=='Lat_Lon units'){
          table <- head(values$dataset[,grep('lat|lon', names(values$dataset), ignore.case=TRUE)])
        } else {
          NULL
        }, server = TRUE, selection = list(target = 'column'), rownames=FALSE,
        options = list(autoWidth=FALSE, scrollX=T,  responsive=TRUE, pageLength = 7)
      )
      
      ##Output to main panel
      output$Case<-renderPrint({
        if(input$checks=='Summary table') {
          "Summary table of NUMERIC variables in data set."
        } else if(input$checks=='Outliers'){
          if(input$dat.remove=='none'){
            HTML('Table to assess outliers.', input$column_check, "shown. <br>Zoom in to plot by highlighting desired area and double clicking. <br>Double click again to reset plot.")
          } else {
            HTML('Table to assess outliers.', input$column_check, 'shown. <br>Zoom in to plot by highlighting desired area and double clicking. <br>Double click again to reset plot. 
                  <br>Excluding points that fall outside the',  if(input$dat.remove=='5_95_quant'){
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
        } else if(input$checks=='NAs'){
          #na(values$dataset)
          na_filter(values$dataset, names(which(apply(values$dataset, 2, function(x) anyNA(x))==TRUE)), replace = FALSE, remove = FALSE, rep.value=NA, over_write=FALSE)
        } else if(input$checks=='NaNs') {
          nan_filter(values$dataset, names(which(apply(values$dataset, 2, function(x) any(is.nan(x)))==TRUE)), replace = FALSE, remove = FALSE, rep.value=NA,  over_write=FALSE)
        } else if(input$checks=='Unique observations'){
          unique_filter(values$dataset, remove=FALSE)
        } else if(input$checks=='Empty variables'){
          empty_vars_filter(values$dataset, remove=FALSE)
        } else if(input$checks=='Lat_Lon units'){
          degree(values$dataset,lat=NULL, lon=NULL, latsign=FALSE, lonsign=FALSE, replace=FALSE)
        } else {
          'Make a selection in the left hand column'
        } 
      })
      
      ##Output to saved file
      case_to_print <- reactive({
        if(input$tabs=='qaqc'){
          if(input$checks=='Summary table') {
            "Summary table of numeric variables viewed.\n"
          } else  if(input$checks=='Outliers'){
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
          } else if(input$checks=='NAs'){
            if(any(apply(values$dataset, 2, function(x) anyNA(x)))==TRUE) {
              if(input$NA_Filter_all==0&NA_Filter_allNA_Filter_mean==0){
                paste("Occurrence of missing values checked. The", sub(",([^,]*)$", ", and\\1",paste(names(which(apply(values$dataset, 2, function(x) any(is.na(x)))==TRUE)), collapse = ", ")),
                      "variables contain",  sub(",([^,]*)$", ", and\\1", paste(apply(values$dataset[,names(which(apply(values$dataset, 2, function(x) anyNA(x))==TRUE))], 2, 
                                                                                     function(x) length(which(is.na(x)==TRUE))), collapse=", ")), 
                      "missing values, respectively.", length(unique(unlist(apply(values$dataset[,names(which(apply(values$dataset, 2, 
                                                                                                                    function(x) anyNA(x))==TRUE))], 2, function(x) which(is.na(x)==TRUE))))), "rows have missing values. Missing values were not removed or replaced.\n") 
              }} else {
                if(input$NA_Filter_all==0&NA_Filter_allNA_Filter_mean==0){
                  paste("Occurrence of missing values checked. No columns in the data set contain missing values.\n")
                } else {
                  if(input$NA_Filter_all>0){
                    paste("Occurrence of missing values checked. The", sub(",([^,]*)$", ", and\\1",paste(names(which(apply(values$dataset, 2, function(x) anyNA(x))==TRUE)), collapse = ", ")), "variables contained", sub(",([^,]*)$", ", and\\1", paste(apply(values$dataset[,names(which(apply(values$dataset, 2, function(x) anyNA(x))==TRUE))], 2, 
                                                                                                                                           function(x) length(which(is.na(x)==TRUE))), collapse=", ")), "missing values.", length(unique(unlist(apply(values$dataset[,names(which(apply(values$dataset, 2, 
                                                                                                                                                                                      function(x) anyNA(x))==TRUE))], 2, function(x) which(is.na(x)==TRUE))))), "rows containing missing values were removed from the data set.\n")
                  } else if(input$NA_Filter_mean>0){
                    paste("Occurrence of missing values checked. The", sub(",([^,]*)$", ", and\\1",paste(names(which(apply(values$dataset, 2, function(x) anyNA(x))==TRUE)), collapse = ", ")), "variables contained", sub(",([^,]*)$", ", and\\1", paste(apply(values$dataset[,names(which(apply(values$dataset, 2, function(x) anyNA(x))==TRUE))], 2, 
                                                                                                                                           function(x) length(which(is.na(x)==TRUE))), collapse=", ")), "missing values. Missing values were replaced with the mean values of", RM, "respectively.\n")
                  }
                } }
          } else if(input$checks=='NaNs') {
            if(any(apply(values$dataset, 2, function(x) any(is.nan(x))))==TRUE) {
              if(input$NAN_Filter_all==0&input$NAN_Filter_mean==0){
                paste("Occurruence of non-numbers checked. The", sub(",([^,]*)$", ", and\\1",paste(names(which(apply(values$dataset, 2, function(x) any(is.nan(x)))==TRUE)), collapse = ", ")),
                      "variables contain", 
                      sub(",([^,]*)$", ", and\\1", paste(apply(values$dataset[,names(which(apply(values$dataset, 2, function(x) any(is.nan(x)))==TRUE))], 2, 
                                                               function(x) length(which(is.nan(x)==TRUE))), collapse=", ")), "non-numbers, respectively.", 
                      length(unique(unlist(apply(values$dataset[,names(which(apply(values$dataset, 2, 
                                                                                   function(x) any(is.nan(x)))==TRUE))], 2, function(x) which(is.nan(x)==TRUE))))), "rows have non-numbers. No action was taken to remove or replace non-numbers.\n") 
              }} else {
                if(input$NAN_Filter_all==0&input$NAN_Filter_mean==0){
                  "Occurruence of non-numbers checked. No columns in the data set contain non-numbers.\n"
                } else {
                  if(input$NAN_Filter_all>0){
                    paste("Occurruence of non-numbers checked. The", sub(",([^,]*)$", ", and\\1",paste(names(which(apply(values$dataset, 2, function(x) any(is.nan(x)))==TRUE)), collapse = ", ")), "variables contained", 
                          sub(",([^,]*)$", ", and\\1", paste(apply(values$dataset[,names(which(apply(values$dataset, 2, function(x) any(is.nan(x)))==TRUE))], 2, 
                                                                   function(x) length(which(is.nan(x)==TRUE))), collapse=", ")), "non-numbers.", 
                          length(unique(unlist(apply(values$dataset[,names(which(apply(values$dataset, 2, 
                                                                                       function(x) any(is.nan(x)))==TRUE))], 2, function(x) which(is.nan(x)==TRUE))))), "rows containing non-numbers were removed from the data set.\n")
                  } else if(input$NAN_Filter_mean>0){
                    paste("Occurruence of non-numbers checked. The", sub(",([^,]*)$", ", and\\1",paste(names(which(apply(values$dataset, 2, function(x) any(is.nan(x)))==TRUE)), collapse = ", ")), "variables contained", 
                          sub(",([^,]*)$", ", and\\1", paste(apply(values$dataset[,names(which(apply(values$dataset, 2, function(x) any(is.nan(x)))==TRUE))], 2, 
                                                                   function(x) length(which(is.nan(x)==TRUE))), collapse=", ")), "non-numbers.\n")
                  }
                } }
          } else if(input$checks=='Unique observations'){
            if(dim(values$dataset)[1] == dim(unique(values$dataset))[1]) {
              "Each row is a unique choice occurrence.\n"
            } else {
              if(input$Unique_Filter==0){
                "Each row in data set is not a unique choice occurrence at haul or trip level. No action taken.\n"
              } else {
                "Duplicate choice occurrence at haul or trip level existed in the data set and have been removed.\n"
              }
            }
          } else if(input$checks=='Empty variables'){
            if(any(apply(values$dataset, 2, function(x) all(is.na(x))) == TRUE)) {
              if(input$Empty_Filter==0){
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
            if(any(apply(values$dataset[,grep('lat|lon', names(values$dataset), ignore.case=TRUE)], 2, function(x) !is.numeric(x))==TRUE)==TRUE){
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
            paste0('Viewed plot and linear regression test output for ',input$reg_exp_select, ' on ', input$reg_resp_select,'.\n') 
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
        } else if(input$tabs=='book'){
          if(!is.null(input$notesBook)){
            paste0(input$notesBook, '\n')
          }
        }
      })
      
      ##Table output
      tableInputSummary <- reactive({
        if(colnames(values$dataset)[1] == 'var1') {
          return(NULL)
        } else if(input$checks=='Summary table'|input$checks=='NAs') { 
          temp <- values$dataset
          stable <- summary_stats(temp, input$projectname) 
          nums <- unlist(lapply(temp, is.numeric))
          stable  <- apply(stable[nums], 2, function(x) gsub(".*:","", x))
          rownames(stable)=c('Min', 'Median','Mean', 'Max',"Missing",'Unique Obs.', "No. 0's")
          stable <- as.data.frame(as.matrix(stable))
          stable <- as.data.frame((t(stable)))
        } else {
          NULL
        }
      })
      
      output$missingtable <- DT::renderDT(
        if(length(which(tableInputSummary()$Missing!=" 0" & !is.na(tableInputSummary()$Missing)))>0){
        tableInputSummary()[which(tableInputSummary()$Missing!=" 0" & !is.na(tableInputSummary()$Missing)),]
         } else {
          return(NULL)
        }  , server = TRUE, rownames=TRUE,
        options = list(autoWidth=FALSE, scrollX=T, responsive=FALSE, pageLength = 25)
       
      )
      
      output$output_table_summary <- DT::renderDT(
        tableInputSummary(), server = TRUE, rownames=TRUE,
        options = list(autoWidth=FALSE, scrollX=T, responsive=FALSE, pageLength = 25)
      )
      
      tableInputOutlier <- reactive({
        if(colnames(values$dataset)[1] == 'var1') {
          return(NULL)
        } else if(input$checks=='Outliers'){
          table <- outlier_table(values$dataset, input$projectname, input$column_check)
          rownames(table)=table[,2]
          table <- table[,3:10]
          #table <<- table
        } else {
          NULL
        }
      })
      
      output$output_table_outlier <- DT::renderDT(
        if(colnames(values$dataset)[1] == 'var1') {
          return(NULL)
        } else if(input$checks=='Outliers'){
          table <- outlier_table(values$dataset, input$projectname, input$column_check)
          rownames(table)=table[,2]
          table <- table[,3:10]
          #table <<- table
        } else {
          NULL
        }, server = TRUE, selection='single', rownames=TRUE,
        options = list(autoWidth=FALSE, scrollX=T,  responsive=TRUE, pageLength = 7)
      )
      
      ranges1 <- reactiveValues(x = NULL, y = NULL)   
      ranges2 <- reactiveValues(x = NULL, y = NULL)   
      ranges3 <- reactiveValues(x = NULL, y = NULL)
      #Plot output
      output$plot1 <- renderPlot(
        if(is.null(values$dataset)) {
          return(NULL)
        } else if(colnames(values$dataset)[1] == 'var1') {
          return(NULL)
        } else {
          if(input$checks=='Outliers'){
            temp <- values$dataset
            temp$val <- 1:nrow(temp)
            dat_sub <- suppressWarnings(outlier_plot_int(temp, input$column_check, input$dat.remove, input$x_dist, plot_type=1))
            suppressWarnings(ggplot2::ggplot() + ggplot2::geom_point(data=dat_sub, ggplot2::aes_string(x='val', y=input$column_check, color = 'Points', na.rm=TRUE)) +
                               ggplot2::scale_color_manual(breaks=c('Kept','Removed'),values=c('blue','red'))+
                               ggplot2::coord_cartesian(xlim = ranges1$x, ylim = ranges1$y, expand = FALSE)+
                               ggplot2::labs(x='Data row')+ ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), 
                                                         panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"), axis.text=ggplot2::element_text(size=12),
                                                         axis.title=ggplot2::element_text(size=12)))  #+ 
            #
          } else {
            NULL
          }}
      )
      
      output$plot2 <- renderPlot(
        if(is.null(values$dataset)) {
          return(NULL)
        } else if(colnames(values$dataset)[1] == 'var1') {
          return(NULL)
        } else {
          if(input$checks=='Outliers'){
            temp <- values$dataset
            temp$val <- 1:nrow(temp)
            dat_sub <- outlier_plot_int(temp, input$column_check, input$dat.remove, input$x_dist, plot_type=1)
            arg.return <- outlier_plot_int(temp, input$column_check, input$dat.remove, input$x_dist, plot_type=2)
            ggplot2::ggplot(dat_sub[dat_sub$Points=='Kept',], ggplot2::aes_string(input$column_check)) + 
              ggplot2::geom_histogram(ggplot2::aes(y = ..density..), na.rm=TRUE, bins=round(nrow(temp)/2)) + arg.return +
              ggplot2::coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)+
              ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), 
                    panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"), axis.text=ggplot2::element_text(size=12),
                    axis.title=ggplot2::element_text(size=12))
          } else {
            NULL
          }}
      )
      
      output$plot3 <- renderPlot(
        if(is.null(values$dataset)) {
          return(NULL)
        } else if(colnames(values$dataset)[1] == 'var1') {
          return(NULL)
        } else {
          if(input$checks=='Outliers'){
            temp <- values$dataset
            temp$val <- 1:nrow(temp)
            temp <- outlier_plot_int(temp, input$column_check, input$dat.remove, input$x_dist, plot_type=3)
            ggplot2::ggplot(temp, ggplot2::aes(x=fit_quants, y=data_quants)) + ggplot2::geom_point(shape=1) + ggplot2::geom_abline() +
              ggplot2::labs(x='Theoretical Quantiles', y='Sample Quantiles', title=paste('Q-Q plot of', input$x_dist, 'fit against data'))+
              ggplot2::coord_cartesian(xlim = ranges3$x, ylim = ranges3$y, expand = FALSE)+
              ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), 
                    panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"), axis.text=ggplot2::element_text(size=12),
                    axis.title=ggplot2::element_text(size=12))
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
        if(nrow(point) == 0) return(NULL)
        
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
        if(!is.null(brush)) {
          ranges1$x <- c(brush$xmin, brush$xmax)
          ranges1$y <- c(brush$ymin, brush$ymax)
          
        } else {
          ranges1$x <- NULL
          ranges1$y <- NULL
        }
      })
      
      observeEvent(input$plot2_dblclick, {
        brush <- input$plot2_brush
        if(!is.null(brush)) {
          ranges2$x <- c(brush$xmin, brush$xmax)
          ranges2$y <- NULL
          
        } else {
          ranges2$x <- NULL
          ranges2$y <- NULL
        }
      })
      
      observeEvent(input$plot3_dblclick, {
        brush <- input$plot3_brush
        if(!is.null(brush)) {
          ranges3$x <- c(brush$xmin, brush$xmax)
          ranges3$y <- c(brush$ymin, brush$ymax)
          
        } else {
          ranges3$x <- NULL
          ranges3$y <- NULL
        }
      })
        
      
      ##Outlier options 
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
          selectInput('x_dist', 'Distribution', 
                      choices=c('normal', 'lognormal', 'exponential', 'weibull', 'poisson', 'negative binomial'), selected='normal'))
      })
      
      ##Filtering options
      #output_table())
      
      
      observeEvent(input$NA_Filter_all,{
          if(any(apply(values$dataset, 2, function(x) anyNA(x)))==TRUE){
            values$dataset <- na_filter(values$dataset, names(which(apply(values$dataset, 2, function(x) anyNA(x))==TRUE)), replace = FALSE, remove = TRUE, rep.value=NA, over_write=FALSE)  
          } else {
            values$dataset <- values$dataset
            cat('No missing values to remove')
          }
        })
      
      observeEvent(input$NA_Filter_mean,{
          if(any(apply(values$dataset, 2, function(x) anyNA(x)))==TRUE){
            values$dataset <- na_filter(values$dataset,  names(which(apply(values$dataset, 2, function(x) anyNA(x))==TRUE)), replace = TRUE, remove = FALSE, rep.value=NA, over_write=FALSE)
          } else {
            values$dataset <- values$dataset
            cat('No missing values to remove')
          }
      })
      
      observeEvent(input$NAN_Filter_all,{
          if(any(apply(values$dataset, 2, function(x) any(is.nan(x))))==TRUE){
            values$dataset <- nan_filter(values$dataset, names(which(apply(values$dataset, 2, function(x) any(is.nan(x)))==TRUE)), replace = FALSE, remove = TRUE, rep.value=NA, over_write=FALSE)  
          } else{
            values$dataset <- values$dataset
            print('No non-numbers to remove.')
          }
      })
      
      observeEvent(input$NAN_Filter_mean,{
          if(any(apply(values$dataset, 2, function(x) any(is.nan(x))))==TRUE){
            values$dataset <- nan_filter(values$dataset,  names(which(apply(values$dataset, 2, function(x) any(is.nan(x)))==TRUE)), replace = TRUE, remove = FALSE, rep.value=NA, over_write=FALSE)
          } else {
            values$dataset <- values$dataset
            print('No non-numbers to remove.')
          }
      })
      
      observeEvent(input$Outlier_Filter,{
          values$dataset <- FishSET::outlier_remove(values$dataset, input$column_check, dat.remove = input$dat.remove, remove = T, over_write=FALSE)
      })
      
      observeEvent(input$Unique_Filter,{
        values$dataset <- unique_filter(values$dataset, remove=TRUE)
      })
      
      observeEvent(input$Empty_Filter,{
        values$dataset <- empty_vars_filter(values$dataset, remove=TRUE)            
        })
      
      observeEvent(input$LatLon_Filter, {
            values$dataset <- degree(values$dataset, 
                                     if(input$LatDirection=='None') { NULL } else {input$LatDirection},
                                     if(input$LonDirection=='None') { NULL } else { input$LonDirection},
                                     latsign=input$LatLon_Filter_Lat, lonsign=input$LatLon_Filter_Lon, replace=TRUE
                                      ) 
      })
      

      
      
      ##----        

      #DATA EXPLORATION FUNCTIONS
      ###----
      #1. TABLE
      output$output_table_exploration <- DT::renderDT(
        if(colnames(values$dataset)[1] == 'var1') {
          return(NULL)
        } else {
        if(input$plot_table=='Table') { 
          c1 <- values$dataset
          colnames(c1)=gsub("_","-", colnames(c1))
          return(c1)
        } else {
          NULL
        }}, server = TRUE, editable=TRUE, filter='top', selection=list(target ='column'),
        extensions = c("Buttons"), rownames=FALSE,
        options = list(autoWidth=TRUE, scrolly=T, responsive=TRUE, pageLength = 15,
                       searchCols = default_search_columns, buttons = c('csv'))
      )
      
      observeEvent(input$saveData,{
        # when it updates, save the search strings so they're not lost
          # update global search and column search strings
          default_search_columns <- c("", input$output_table_exploration_search_columns)
          default_sub <- which(default_search_columns!='')
          if(length(default_sub)==0){
            NULL
          } else {
            if(table_exists(paste0(input$projectname, "FilterTable")) == F) {
              FilterTable <- data.frame(dataframe = NA, vector = NA, FilterFunction = NA)
            } else {
              FilterTable <- table_view(paste0(input$projectname, "FilterTable"))
            }
            for(i in 1:length(default_sub)){
              if( grepl("\\..\\.", default_search_columns[default_sub[i]])==TRUE){
                FilterTable <- rbind(FilterTable, c(paste0(input$projectname, 'MainDataTable'), (colnames(values$dataset[default_sub])[i]), 
                                                    paste(colnames(values$dataset[default_sub])[i], '>', as.numeric(sapply(strsplit(default_search_columns[default_sub[i]], "\\..\\."), head, 1)), '&', 
                                                          colnames(values$dataset[default_sub])[i], '<', as.numeric(sapply(strsplit(default_search_columns[default_sub[i]], "\\..\\."), tail, 1)))))
              } else {
                FilterTable <- rbind(FilterTable, c(paste0(input$projectname, 'MainDataTable'), (colnames(values$dataset[default_sub])[i]), 
                                                    paste0("grepl('", default_search_columns[default_sub[i]],"', ", colnames(values$dataset[default_sub])[i],")")))
              }
            }
            
            showNotification('Filter table saved to FishSET database', type='message', duration=10)
            
            filter_data_function <- list()
            filter_data_function$functionID <- 'filter_table'
            filter_data_function$args <- c(dat, project, x, exp, project)
            filter_data_function$kwargs <- list()
            filter_data_function$output <- c('')
            filter_data_function$msg <- filterTable
            log_call(filter_data_function)
            
            fishset_db <- suppressWarnings(DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
            DBI::dbWriteTable(fishset_db, paste0(input$projectname, 'FilterTable'),  FilterTable, overwrite=TRUE)
            DBI::dbDisconnect(fishset_db)
          }  
      })
      
      observeEvent(input$saveDataNew,{
        fishset_db <- suppressWarnings(DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
        DBI::dbWriteTable(fishset_db, paste0(input$projectname, 'FilterTable'),  FilterTable, overwrite=TRUE)
        DBI::dbDisconnect(fishset_db)
      })
      
      observeEvent(input$subsetData,{
        values$dataset <- values$dataset[,-(input$output_table_exploration_columns_selected+1)]
      })
      
      output$editText <- renderText('Edit cells: double click. \nEdited table will not \nbe loaded into working \nenvironment until saved.
                                    \nFilter: Boxes at top.
                                    \nFilter functions saved to \nFishSET database as \nFilterTable when "save \ndata" button is pushed.
                                    \nRemove variables: Click on \ncolumn cell(s), then click \n"Remove Variable" button.\nVariables can be added back \nusing the add_vars function.
                                    \nClick the "Save Data" button \nto save changes.')
      
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
        if(is.null(values$dataset)) {
          return(NULL)
        } else if(colnames(values$dataset)[1] == 'var1') {
          return(NULL)
        } else {
        if(grepl('date', input$col_select[1], ignore.case=T)==TRUE){
          p1 <- ggplot2::ggplot(values$dataset, 
                       ggplot2::aes_string(x=as.Date(values$dataset[,grep('date',  colnames(values$dataset), ignore.case = TRUE)[1]], origin='01-01-1970'),
                                  y=as.Date(values$dataset[[input$col_select]], origin='01-01-1970'))) + ggplot2::geom_point()+
            ggplot2::labs(subtitle=paste(input$col_select, 'by Date'), x="Date", y=input$col_select) +
            ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), 
                  panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"), axis.text=ggplot2::element_text(size=11),
                  axis.title=ggplot2::element_text(size=11)) 
        } else {
          p1 <- ggplot2::ggplot(values$dataset, 
                       ggplot2::aes_string(x=as.Date(values$dataset[,grep('date', colnames(values$dataset), ignore.case = TRUE)[1]], origin='01-01-1970'),
                                  y=input$col_select)) + ggplot2::geom_point()+
            ggplot2::labs(subtitle=paste(input$col_select, 'by Date'), x="Date", y=input$col_select) +
            ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), 
                  panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"), axis.text=ggplot2::element_text(size=11),
                  axis.title=ggplot2::element_text(size=11))
        }
        p2 <- ggplot2::ggplot(df2l(), ggplot2::aes_string(x=df2l()[,1], y=df2l()[,2]))+ ggplot2::geom_bar(stat='identity')+
          ggplot2::labs(subtitle=paste(input$p2fun, 'by', tolower(t2())), x=t2(),y='')+
          ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), 
                panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"), axis.text=ggplot2::element_text(size=11),
                axis.title=ggplot2::element_text(size=11))
        if(!is.numeric(values$dataset[[input$col_select]])) {
          p3 <- NULL
        } else {
          p3 <- ggplot2::ggplot(df2m(), ggplot2::aes_string(x=df2m()[,1], y=df2m()[,2]))+ ggplot2::geom_bar(stat='identity')+
            ggplot2::labs(subtitle=paste(simpleCap(input$p3fun), 'of value by', tolower(t2())), x=t2(), y='')+
            ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), 
                  panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"), axis.text=ggplot2::element_text(size=11),
                  axis.title=ggplot2::element_text(size=11))
        } 
       
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
        if(is.null(values$dataset)) {
          return(NULL)
        } else if(colnames(values$dataset)[1] == 'var1') {
          return(NULL)
        } else {
          longitude <- which(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)', ignore.case=TRUE)==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)', ignore.case=TRUE)))[1]
          latitude <- which(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)', ignore.case=TRUE)==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)', ignore.case=TRUE)))[1]
          cf <- ggplot2::coord_fixed()
          cf$default <- TRUE
          ggplot2::ggplot(data = ggplot2::map_data("world"), mapping = ggplot2::aes(x = long, y = lat, group=group)) + 
            ggplot2::geom_polygon(color = "black", fill = "gray") + 
            ggplot2::geom_point(data = values$dataset, ggplot2::aes(x = values$dataset[,longitude], y = values$dataset[,latitude], group=rep(1, nrow(values$dataset))), color = "red", size = 1) +
            cf + ggplot2::coord_fixed(xlim = ranges_spatial$x, ylim = ranges_spatial$y, ratio=1.3, expand = TRUE)+
            ggplot2::labs(x='Longitude', y='Latitude', subtitle='Observed locations')+
            ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), 
                  panel.background = ggplot2::element_blank(),  axis.text=ggplot2::element_text(size=12),
                  axis.title=ggplot2::element_text(size=12),panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=1) )
        } 
      })
      plotInput_kernel <- reactive ({
        if(is.null(values$dataset)) {
          return(NULL)
        } else if(colnames(values$dataset)[1] == 'var1') {
          return(NULL)
        } else {
          if(input$plot_table=='Plots'&input$plot_type=='Spatial'){
            return(map_kernel(values$dataset, project=input$projectname, type='gradient', 
                       latlon=c(which(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)', 
                                                                                ignore.case=TRUE)==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)', ignore.case=TRUE)))[1], 
                                               which(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)', 
                                                                                 ignore.case=TRUE)==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)', ignore.case=TRUE)))[1])))
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
        if(is.null(hover)) return(NULL)
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
        if(!is.null(brush)) {
          
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
          h4('Further options to display measures of spatial autocorrelation'),
          if(names(spatdat$dataset)[1]=='var1'){
            tags$div(h5('Map file not loaded. Please load on Upload Data tab', style="color:red"))
          },
          conditionalPanel(condition="input.plot_table=='Plots'&input.plot_type=='Spatial'",
                           style = "margin-left:19px;", selectizeInput('varofint', 'Variable to test for spatial autocorrelation',
                                                                    choices=colnames(values$dataset[,sapply(values$dataset,is.numeric)]))),
          conditionalPanel(condition="input.plot_table=='Plots'&input.plot_type=='Spatial'",
                           style = "margin-left:19px;",  selectizeInput('gtmt_lonlat', 'Select lat then lon from data frame to assign observations to zone', 
                                                                     choices=c(NULL, names(values$dataset)[grep('lon|lat', names(values$dataset), ignore.case=TRUE)]), 
                                                                     multiple = TRUE, options = list(maxItems = 2)))#, 
        #  conditionalPanel(condition="input.plot_table=='Plots'&input.plot_type=='Spatial'",
        #                   style = "margin-left:19px;", fileInput("gtmtfileGrid", "Choose file defining area/zone polygons", multiple = FALSE)) 
        )
      })    
      output$mtgt_out2 <- renderUI({
        tagList(
          conditionalPanel(condition="input.plot_table=='Plots'&input.plot_type=='Spatial'",
                           style = "margin-left:19px;", selectInput('mtgtcat',  "Variable defining zones or areas", 
                                                                    choices= c('', names(as.data.frame(spatdat$dataset))), selected='')),
          conditionalPanel(condition="input.plot_table=='Plots'&input.plot_type=='Spatial'",
                           style = "margin-left:19px;", selectizeInput('mtgtlonlat', 'Select vector containing latitude then longitude from spatial data frame', 
                                                                    choices= c(NULL, names(as.data.frame(spatdat$dataset))), multiple=TRUE, options = list(maxItems = 2)))
        )
      })
      gtmt_table <- reactive({
        if(input$mtgtcat==''){
          return( NULL)
        } else {
          gt <- getis_ord_stats(values$dataset, input$projectname, input$varofint, spatdat$dataset, lon.dat=input$gtmt_lonlat[2], lat.dat=input$gtmt_lonlat[1], cat=input$mtgtcat, lon.grid=input$mtgtlonlat[2], lat.grid=input$mtgtlonlat[1])$getistable
          mt <- moran_stats(values$dataset, input$projectname, input$varofint, spatdat$dataset, lon.dat=input$gtmt_lonlat[2], lat.dat=input$gtmt_lonlat[1], cat=input$mtgtcat, lon.grid=input$mtgtlonlat[2], lat.grid=input$mtgtlonlat[1])$morantable
          print(gt)
          return(as.data.frame(merge(gt, mt)))
        }
      }) 
      
      output$output_table_gt_mt <- DT::renderDT(  
        gtmt_table(), server=TRUE
      )
      
      #4. X VS. Y
      plotInput_xy <- reactive({
        if(is.null(values$dataset)) {
          return(NULL)
        } else if(colnames(values$dataset)[1] == 'var1') {
          return(NULL)
        } else {
          ggplot2::ggplot(values$dataset, ggplot2::aes_string(x=values$dataset[[input$x_y_select1]], y=values$dataset[[input$x_y_select2]])) + 
            ggplot2::geom_point()+
            ggplot2::labs(subtitle=paste(input$x_y_select1, 'by', input$x_y_select2), x=input$x_y_select1, y=input$x_y_select2) +
            ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), 
                  panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"), axis.text = ggplot2::element_text(size=11),
                  axis.title=ggplot2::element_text(size=11))
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
        tableInputCorr(),  server=TRUE, extensions = list('Scroller'), 
        options=list(autoWidth = TRUE, scrollX=TRUE, deferRender = T,
                     scrollY = 'auto', scroller = TRUE, scrollX = T, pageLength = 25)
      )
      output$output_text_corr <- renderPrint(
        if(colnames(values$dataset)[1] == 'var1') {
          return(NULL)
        } else if(length(input$corr_select)==2){
          cor.test(values$dataset[[input$corr_select[1]]], values$dataset[[input$corr_select[2]]])
        } else {
          return(NULL)
         }
      )
      
      plotInputcorr <- reactive({
        if(is.null(values$dataset)) {
          return(NULL)
        } else if(colnames(values$dataset)[1] == 'var1') {
          return(NULL)
          } else {
        if(length(input$corr_select)==2){
          ggplot2::ggplot(values$dataset, ggplot2::aes_string(x=values$dataset[[input$corr_select[1]]], y=values$dataset[[input$corr_select[2]]])) + 
            ggplot2::geom_point()+
            ggplot2::geom_smooth(method=lm)+ggplot2::labs(subtitle=paste(input$corr_select[1], 'by', input$corr_select[2]), x=input$corr_select[1], y=input$corr_select[2])+
            ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), 
                  panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"), axis.text=ggplot2::element_text(size=11),
                  axis.title=ggplot2::element_text(size=11))
        } else if(length(input$corr_select)>2){
          ggcorrplot::ggcorrplot(round(cor(values$dataset[,input$corr_select], use="complete.obs"), 2), 
                                 type='lower',outline.color = 'white', hc.order=TRUE,show.diag=TRUE,
                                 title = paste("Correlation matrix plot for", input$projectname, "data"),
                                 ggtheme=ggplot2::theme_minimal())
        } 
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
      p2 <- reactive({summary(lm(values$dataset[[input$reg_resp_select]]~values$dataset[,input$reg_exp_select]))})
      output$output_text_reg <- renderPrint(
        p2()
      )
      
      plotInputreg <- reactive({
        if(colnames(values$dataset)[1] == 'var1') {
          return(NULL)
        } else if(length(input$reg_exp_select)!=1){
          return(NULL)
        } else {
          ggpubr::annotate_figure(ggpubr::ggarrange(ggplot2::ggplot(values$dataset, ggplot2::aes_string(x=input$reg_exp_select, y=input$reg_resp_select)) + 
                                                      ggplot2::geom_point()+ ggplot2::geom_smooth(method=lm)+
                                                      ggplot2::labs(subtitle=paste(input$reg_resp_select, 'against', input$reg_exp_select), x=input$reg_exp_select, y=input$reg_resp_select)+
                                                      ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), 
                                                            panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"), axis.text=ggplot2::element_text(size=11),
                                                            axis.title = ggplot2::element_text(size=11)),
                                                    ggplot2::ggplot(lm(values$dataset[[input$reg_resp_select]]~values$dataset[[input$reg_exp_select]])) + 
                                                      ggplot2::geom_point(ggplot2::aes(x=.fitted, y=.resid)) + 
                                                      ggplot2::labs(subtitle = 'Residuals against fitted values', x='Fitted',y='Residuals')+
                                                      ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), 
                                                            panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"), 
                                                            axis.text = ggplot2::element_text(size=11),
                                                            axis.title=ggplot2::element_text(size=11)),
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
      output$sp_col.select<- renderUI({
        conditionalPanel(condition="input.VarCreateTop=='Nominal ID'&input.ID=='create_seasonal_ID'",
                         style = "margin-left:19px;",  selectInput('sp_col', "Column containing species names in table containing seasonal data", 
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
                         style = "margin-left:19px;", selectInput('xTime','Duration. Select Calculate Duration to define and calculate duration.', 
                                                                  choices=c('Calculate duration', names(values$dataset[,unlist(lapply(values$dataset, is.numeric))])), selectize=TRUE))
      })
      output$dur_add <- renderUI({
        tagList(
          conditionalPanel(condition="input.VarCreateTop=='Arithmetic and temporal functions'&input.numfunc=='cpue'&input.xTime=='Calculate duration'",
                           style = "margin-left:19px;", selectInput('dur_start2', 'Variable indicating start of time period', 
                                                                    choices = names(values$dataset[,grep("date|min|hour|week|month|TRIP_START|TRIP_END", names(values$dataset), ignore.case = TRUE)]), selectize=TRUE)),
          conditionalPanel(condition="input.VarCreateTop=='Arithmetic and temporal functions'&input.numfunc=='cpue'&input.xTime=='Calculate duration'",         
                           style = "margin-left:19px;", selectInput('dur_end2', 'Variable indicating end of time period', 
                                                                    choices = names(values$dataset[,grep("date|min|hour|week|month|TRIP_START|TRIP_END", names(values$dataset), ignore.case = TRUE)]), selectize=TRUE)),
          conditionalPanel(condition="input.VarCreateTop=='Arithmetic and temporal functions'&input.numfunc=='cpue'&input.xTime=='Calculate duration'",          
                           style = "margin-left:19px;", selectInput('dur_units2', 'Unit of time for calculating duration', choices = c("week", "day", "hour", "minute")))
        )
      })
      output$dist_between_input <- renderUI({
        tagList(
         if(names(spatdat$dataset)[1]=='var1'){
           conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_dist_between'",
                              tags$div(h4('Map file not loaded. Please load on Upload Data tab', style="color:red")))
          },
         conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_dist_between'",
                           style = "margin-left:19px;", selectInput('start', 'Starting location',choices = c('Zonal centroid', 'Port', 'Lat/lon coordinates'))),
          conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_dist_between'",
                           style = "margin-left:19px;", selectInput('end', 'Ending location', choices = c('Zonal centroid', 'Port', 'Lat/lon coordinates'))),
          #Port
          conditionalPanel(condition="(input.VarCreateTop=='Spatial functions'&input.dist=='create_dist_between'&input.start=='Port')|
                           (input.VarCreateTop=='Spatial functions'&input.dist=='create_dist_between'&input.end=='Port')" ,
                           style = "margin-left:19px;", selectInput("filePort", "Choose file from FishSET SQL database containing port data", 
                                                                    choices=tables_database()[grep('port', tables_database(), ignore.case=TRUE)], multiple = FALSE)),
#port
          conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_dist_between'&input.start=='Port'",
                           style = "margin-left:19px;", selectInput('port_start', 'Variable containing port name at starting location', 
                                                                    choices=names(values$dataset[,grep('port', names(values$dataset), ignore.case=T)]), selectize=TRUE)),
          conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_dist_between'&input.end=='Port'" ,
                           style = "margin-left:19px;", selectInput('port_end', 'Variable containing port name at ending location', 
                                                                    choices=names(values$dataset[,grep('port', names(values$dataset), ignore.case=T)]), selectize=TRUE)),
          # fileInput("filePort", "Choose file containing port data",    
          #Zonal
          #coords
          conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_dist_between'&input.start=='Lat/lon coordinates'" ,
                           style = "margin-left:19px;", selectizeInput('start_latlon', 'Select lat then lon for starting location', 
                                                                    choices=names(values$dataset[,grep('lat|lon', names(values$dataset), ignore.case=T)]), multiple=TRUE), 
                           options = list(maxItems = 2)),
          conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_dist_between'&input.end=='Lat/lon coordinates'" ,
                           style = "margin-left:19px;", selectizeInput('end_latlon', 'Select lat then lon for ending location', 
                                                                    choices=names(values$dataset[,grep('lat|lon', names(values$dataset), ignore.case=T)]), 
                                                                    multiple=TRUE), options = list(maxItems = 2))
          
          
        )#)  
      })   
      output$dist_betwn_opts <- renderUI({
        tagList(
           conditionalPanel(condition="(input.VarCreateTop=='Spatial functions'&input.dist=='create_dist_between'&input.start=='Zonal centroid')|
                           (input.VarCreateTop=='Spatial functions'&input.dist=='create_dist_between'&input.end=='Zonal centroid')" ,
                           style = "margin-left:19px;",  selectizeInput('lon_dat', 'Select lat then lon from data set to assign observations to zone', 
                                                                     choices=names(values$dataset[,grep('lat|lon', names(values$dataset), ignore.case=T)]),
                                                                     multiple=TRUE, options = list(maxItems = 2))),
            conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_dist_between'&input.start=='Zonal centroid'||
                           input.VarCreateTop=='Spatial functions'&input.dist=='create_dist_between'&input.end=='Zonal centroid'" ,
                           style = "margin-left:19px;", selectInput('cat', 'Individual areas/zones from the spatial data drame', choices=names(as.data.frame(spatdat$dataset)))),
          if(any(class(spatdat$dataset)=='sf')==FALSE){
            conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_dist_between'&input.start=='Zonal centroid'||
                             input.VarCreateTop=='Spatial functions'&input.dist=='create_dist_between'&input.end=='Zonal centroid'" , 
                             style = "margin-left:19px;", selectizeInput('long_grid', 'Select vector containing latitude then longitude from spatial data set',
                                                                      choices=names(as.data.frame(spatdat$dataset)), multiple=TRUE, options = list(maxItems = 2)))
          }
         
      )
      })

      output$start_mid_input <- renderUI({
        conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&&input.dist=='create_mid_haul'",
                         style = "margin-left:19px;", selectizeInput('mid_start','Select Lat then Lon that define starting locations',multiple = TRUE,
                                                                  choices = names(values$dataset[,grep('lat|lon', names(values$dataset), ignore.case=TRUE)]), 
                                                                  options = list(maxItems = 2)))
      })
      output$end_mid_input <- renderUI({
        conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_mid_haul'",
                         style = "margin-left:19px;",  selectizeInput('mid_end','Select Lat then Lon that define ending locations',multiple = TRUE,
                                                                   choices = names(values$dataset[,grep('lat|lon', names(values$dataset), ignore.case=TRUE)]),
                                                                   options = list(maxItems = 2)))
      })
      output$input_dur_start <- renderUI({
        conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_duration'",
                         style = "margin-left:19px;", selectInput('dur_start', 'Variable indicating start of time period', 
                                                                  choices = names(values$dataset[,grep("date|min|hour|week|month|TRIP_START|TRIP_END", names(values$dataset), ignore.case = TRUE)]), selectize=TRUE))
      })
      output$input_dur_end <- renderUI({
        conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_duration'",
                         style = "margin-left:19px;", selectInput('dur_end', 'Variable indicating end of time period', 
                                                                  choices = names(values$dataset[,grep("date|min|hour|week|month|TRIP_START|TRIP_END", names(values$dataset), ignore.case = TRUE)]), selectize=TRUE))
      })
      output$input_startingloc <- renderUI({
        tagList(
          if(names(spatdat$dataset)[1]=='var1'){
            conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_startingloc'",
                             tags$div(h4('Map file not loaded. Please load on Upload Data tab', style="color:red")))
          },
          conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_startingloc'",
                           style = "margin-left:19px;", selectInput('trip_id_SL', 'Variable in primary data set to identify unique trips', choices=c('',names(values$dataset)), selectize=TRUE)),
          conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_startingloc'",
                           style = "margin-left:19px;", selectInput('haul_order_SL', 'Variable in primary data set defining haul order within a trip. Can be time, coded variable, etc.',
                                                                    choices=c('', names(values$dataset)), selectize=TRUE)),
          conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_startingloc'",
                           style = "margin-left:19px;", selectInput('starting_port_SL',  "Variable in primary data set identifying port at start of trip", 
                                                                    choices=names(values$dataset[,grep('port',names(values$dataset), ignore.case = TRUE)]), selectize=TRUE)),
          conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_startingloc'",
                           style = "margin-left:19px;", selectInput('lon_dat_SL', "Longitude variable in primary data set", 
                                                                    choices= names(values$dataset[,grep("lon", names(values$dataset), ignore.case = TRUE)]), selectize=TRUE)), 
          conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_startingloc'",
                           style = "margin-left:19px;", selectInput('lat_dat_SL', "Latitude variable in primary data set", 
                                                                    choices= names(values$dataset[,grep("lat", names(values$dataset), ignore.case = TRUE)]), selectize=TRUE)),
          conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_startingloc'",
                           style = "margin-left:19px;",  selectInput("port.dat", "Choose file from FishSET SQL database containing port data", 
                                                                     choices=tables_database()[grep('port', tables_database(), ignore.case=TRUE)], multiple = FALSE))#,
          #conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_startingloc'",
          #                 style = "margin-left:19px;", fileInput("grid.dat", "Choose data file containing map shapefile (shape, json, and csv formats are supported)",
          #                                                        multiple = FALSE, placeholder = ''))
        )})
      output$input_startingloc_extra <- renderUI({
        tagList(
          if(any(class(spatdat$dataset)=='sf')==FALSE){
            conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_startingloc'",
                             style = "margin-left:19px;", selectInput('lat_grid_SL', 'Select vector containing latitude from spatial data set', choices= names(as.data.frame(spatdat$dataset)), multiple=TRUE))
          },
          if(any(class(spatdat$dataset)=='sf')==FALSE){
            conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_startingloc'",
                             style = "margin-left:19px;", selectInput('lon_grid_SL', 'Select vector containing longitude from spatial data set', 
                                                                      choices= names(as.data.frame(spatdat$dataset)), multiple=TRUE, selectize=TRUE))
          },
          conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_startingloc'",
                           style = "margin-left:19px;", selectInput('cat_SL', "Variable defining zones or areas", choices= names(as.data.frame(spatdat$dataset)), selectize=TRUE))
        )
      })
      output$input_IDVAR <- renderUI({
        conditionalPanel(condition="input.VarCreateTop=='Trip-level functions'&input.trip=='haul_to_trip'",
                         style = "margin-left:19px;", selectInput("Haul_Trip_IDVar", "Variable(s) that define unique trips", choices=names(values$dataset), multiple=TRUE, selectize=TRUE))
      })
      output$input_trip_dist_vars <- renderUI({
        tagList(
          conditionalPanel(condition="input.VarCreateTop=='Trip-level functions'&input.dist=='trip_distance'" ,
                           style = "margin-left:19px;", selectInput("port_dat_dist", "Choose file from FishSET SQL database containing port data", 
                                                                    choices=tables_database()[grep('port', tables_database(), ignore.case=TRUE)], multiple = FALSE)),
          #
          conditionalPanel(condition="input.VarCreateTop=='Trip-level functions'&input.trip=='trip_distance'",
                           style = "margin-left:19px;", selectInput('trip_ID','Variable in data file to identify unique trips', multiple = FALSE, 
                                                                    choices = names(values$dataset), selectize=TRUE)),
          conditionalPanel(condition="input.VarCreateTop=='Trip-level functions'&input.trip=='trip_distance'",
                           style = "margin-left:19px;", selectInput('starting_port','Variable in data file to identify port at START of trip',multiple = FALSE, 
                                                                    choices = names(values$dataset)[grep('port', names(values$dataset), ignore.case=TRUE)], selectize=TRUE)),
          conditionalPanel(condition="input.VarCreateTop=='Trip-level functions'&input.trip=='trip_distance'",
                           style = "margin-left:19px;", selectizeInput('starting_haul','Select variables containing LAT the LON at START of haul',multiple = TRUE, 
                                                                    choices = names(values$dataset)[grep('lat|long', names(values$dataset), ignore.case=TRUE)], 
                                                                    options = list(maxItems = 2))),
          conditionalPanel(condition="input.VarCreateTop=='Trip-level functions'&input.trip=='trip_distance'",
                           style = "margin-left:19px;", selectizeInput('ending_haul','Select variables containing LAT then LON at END of haul',multiple = TRUE, 
                                                                    choices = names(values$dataset)[grep('lat|long', names(values$dataset), ignore.case=TRUE)],
                                                                    options = list(maxItems = 2))),
          conditionalPanel(condition="input.VarCreateTop=='Trip-level functions'&input.trip=='trip_distance'",
                           style = "margin-left:19px;", selectizeInput('ending_port','Variable in data file to identify port at END of trip',multiple = FALSE, 
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
                           style = "margin-left:19px;",selectInput('dummypolyfunc','Select policy', multiple=FALSE, 
                                                                   choices=c('User defined', 'Central GOA Rockfish Cooperative (2004)'='Rockfish', 'Amendment 80 Alaska (2008)'='Amen80' ,
                                                                   "Pacific halibut and Sablefish IFQ Program	Alaska (1993)"="halsab", 
                                                                   "American Fisheries Act Pollock Program Alaska (1999)"="AFA"))),
          conditionalPanel(condition="input.VarCreateTop=='Dummy variables'&input.dummyfunc=='From policy dates'&input.dummypolyfunc=='User defined'",
                           style = "margin-left:19px;", textInput('polyear','Policy year', placeholder='Write policy year here')),
          conditionalPanel(condition="input.VarCreateTop=='Dummy variables'&input.dummyfunc=='From policy dates'",
                           style = "margin-left:19px;", selectInput('dummypolydate','Select date variable', multiple=FALSE, 
                                                                   choices=colnames(values$dataset)[grep('year|dat', colnames(values$dataset), ignore.case=TRUE)])),
          conditionalPanel(condition="input.VarCreateTop=='Dummy variables'&input.dummyfunc=='From area closures'",
                           style = "margin-left:19px;", selectInput('dummclosfunc','Select Variable', multiple=FALSE, choices=c()))
        )
      }) 
      dum_pol_year <- reactive({
        if(input$dummypolyfunc=='User defined') {
          return(polyear)
        } else if(input$dummypolyfunc=='Rockfish') {
            return('2004')
        }else if(input$dummypolyfunc=='Amen80') {
          return('2008')
        }else if(input$dummypolyfunc=='halsab') {
          return('1993')
        }else if(input$dummypolyfunc=='AFA') {
          return('1999')
        }
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
                                                                        choices=c('selected year(s) vs. all other years'='x_y','before vs. after'='more_less'))),
              conditionalPanel(condition="input.VarCreateTop=='Dummy variables'&input.dummyfunc=='From variable'",
                               style = "margin-left:19px;",  selectInput("select.val", 'Select year(s)', 
                                                                         choices=c(NULL, unique(lubridate::year(values$dataset[[input$dummyvarfunc]]))), multiple=TRUE))
            )
          } else if(dum_temp()=='num'){
            tagList(
            conditionalPanel(condition="input.VarCreateTop=='Dummy variables'&input.dummyfunc=='From variable'",
                             style = "margin-left:19px;", selectInput("dumsubselect", 'Set dummy variable based on',
                                                                      choices=c('selected value(s) vs all other values'='x_y','less than vs more than'='more_less'), selected='more_less')),
            conditionalPanel(condition="input.VarCreateTop=='Dummy variables'&input.dummyfunc=='From variable'",
                             sliderInput("select.val", 'Select single or range of values to set to zero', min=min(values$dataset[input$dummyvarfunc],na.rm=TRUE),
                                         max=max(values$dataset[input$dummyvarfunc],na.rm=TRUE),
                                         value=c(mean(values$dataset[input$dummyvarfunc], na.rm=TRUE)-mean(values$dataset[input$dummyvarfunc], na.rm=TRUE)/10, 
                                                 mean(values$dataset[input$dummyvarfunc], na.rm=TRUE)+mean(values$dataset[input$dummyvarfunc], na.rm=TRUE)/10)))
            )
          } else if(dum_temp()=='other') {
            conditionalPanel(condition="input.VarCreateTop=='Dummy variables'&input.dummyfunc=='From variable'",
                             style = "margin-left:19px;", selectInput("select.val", 'Select categories to set to zero', choices=c(NULL, unique(values$dataset[[input$dummyvarfunc]])), multiple=TRUE))
          }
          
        } 
      })                  
      observeEvent(input$runNew, {
        if(input$VarCreateTop=='Dummy variables'&input$dummyfunc=='From policy dates') {
          ShowNotification(
            capture.output(
              values$dataset[[input$varname]] <- dummy_num(values$dataset, var=input$dummypolydate, value=dum_pol_year(), opts='more_less')),
            type='message', duration=10)
        } else if(input$VarCreateTop=='Dummy variables'&input$dummyfunc=='From variable') {
          ShowNotification(
            capture.output(
              values$dataset[[input$varname]] <- dummy_num(values$dataset, var=input$dummyvarfunc, value=input$select.val, opts=input$dumsubselect)),
            type='message', duration=10)
        } else if(input$VarCreateTop=='Data transformations'&input$trans=='temp_mod') {
          ShowNotification(
            capture.output(
              values$dataset[[input$varname]] <- temporal_mod(values$dataset, input$TimeVar, input$define_format)),
            type='message', duration=10) #!
        } else if(input$VarCreateTop=='Data transformations'&input$trans=='set_quants'){
          ShowNotification(
            capture.output(
              values$dataset[[input$varname]] <- set_quants(values$dataset, x=input$trans_var_name, quant.cat = input$quant_cat, name=input$varname)),
            type='message', duration=10) #!
        } else if(input$VarCreateTop=='Nominal ID'&input$ID=='ID_var'){
          ShowNotification(
            capture.output(
              values$dataset <- ID_var(values$dataset, newID=input$varname, input$unique_identifier)),
            type='message', duration=10) 
        } else if(input$VarCreateTop=='Nominal ID'&input$ID=='create_seasonal_ID'){
          ShowNotification(
            capture.output(
              values$dataset <- create_seasonal_ID(values$dataset, seasonal.dat=seasonalData(), use.location=input$use_location, 
                                               use.geartype=input$use_geartype, sp.col=input$sp_col, target=input$target)),
            type='message', duration=10)
        } else if(input$VarCreateTop=='Arithmetic and temporal functions'&input$numfunc=='create_var_num'){
          ShowNotification(
            capture.output(
              values$dataset[[input$varname]] <- create_var_num(values$dataset, input$var_x, input$var_y, method=input$create_method, name=input$varname)),
            type='message', duration=10) 
        } else if(input$VarCreateTop=='Arithmetic and temporal functions'&input$numfunc=='cpue') {
          if(input$xTime!='Calculate duration'){
            ShowNotification(
              capture.output(
                values$dataset[[input$varname]] <- cpue(values$dataset, input$xWeight, input$xTime, name=input$varname)),
              type='message', duration=10)   
          } else {
            values$dataset[['dur']] <- create_duration(values$dataset, input$dur_start2, input$dur_end2, input$dur_units2, name=NULL)
            showNotification(
              capture.output(
                values$dataset[[input$varname]] <- cpue(values$dataset, input$xWeight, 'dur', name=input$varname)),
                type='message', duration=10)  
          }
        } else if(input$VarCreateTop=='Spatial functions' & input$dist=='create_dist_between'){
          #'Zonal centroid', 'Port', 'Lat/lon coordinates'
          if(input$start=='Lat/lon coordinates'){
            startdist <-input$start_latlon
          } else if(input$start=='Port'){
            startdist <- input$port_start
          } else {
            startdist <- 'centroid'
          }
          if(input$end=='Lat/lon coordinates'){
            enddist <-input$end_latlon
          } else if(input$end=='Port'){
            enddist <- input$port_end
          } else {
            enddist <- 'centroid'
          }
          showNotification(
            capture.output(
           values$dataset[[input$varname]] <- create_dist_between_for_gui(values$dataset, start=startdist, end=enddist, input$units,  portTable=input$filePort, 
                                                                         gridfile=spatdat$dataset,lon.dat=input$lon_dat[2], lat.dat=input$lon_dat[1], 
                                                                         input$cat, lon.grid=input$long_grid[2], lat.grid=input$long_grid[1])),
           type='message', duration=10)
           } else if(input$VarCreateTop=='Spatial functions' & input$dist=='create_mid_haul'){
             showNotification(
               capture.output(
                 values$dataset <- create_mid_haul(values$dataset, c(input$mid_start[2],input$mid_start[1]), c(input$mid_end[2],input$mid_end[1]), input$varname)),
               type='message', duration=10)
        } else if(input$VarCreateTop=='Spatial functions'&input$dist=='create_duration'){
          showNotification(
            capture.output(
              values$dataset[[input$varname]] <- create_duration(values$dataset, input$dur_start, input$dur_end, input$dur_units, name=NULL)),
            type='message', duration=10)
        } else if(input$VarCreateTop=='Spatial functions'&input$dist=='create_startingloc'){
          showNotification(
            capture.output(
              values$dataset[['startingloc']] <- create_startingloc(values$dataset,  gridfile=spatdat$dataset,  portTable=input$port.dat, 
                                                                trip_id=input$trip_id_SL, haul_order=input$haul_order_SL, starting_port=input$starting_port_SL, 
                                                                input$lon_dat_SL, input$lat_dat_SL, input$cat_SL, input$lon_grid_SL, input$lat_grid_SL)),
            type='message', duration=10)
        } else if(input$VarCreateTop=='Trip-level functions'&input$trip=='haul_to_trip'){
          showNotification(
            capture.output(
              values$dataset <- haul_to_trip(values$dataset, project=input$projectname, input$fun_numeric, input$fun_time, input$Haul_Trip_IDVar)),
            type='message', duration=10)
        } else if(input$VarCreateTop=='Trip-level functions'&input$trip=='trip_distance'){
          showNotification(
            capture.output(
              values$dataset$TripDistance <- create_trip_distance(values$dataset, input$port_dat_dist, input$trip_ID, input$starting_port, 
                                                              c(input$starting_haul[2], input$starting_haul[1]), 
                                                              c(input$ending_haul[2],input$ending_haul[1]), input$ending_port, input$haul_order)),
            type='message', duration=10)
        } else if(input$VarCreateTop=='Trip-level functions'&input$trip=='trip_centroid'){
          showNotification(
            capture.output(
              values$dataset <- create_trip_centroid(values$dataset, lon=input$trip_cent_lon, lat=input$trip_cent_lat, weight.var=input$trip_cent_weight, 
                                                 input$trip_cent_id)),
            type='message', duration=10)
        }
      })
      
      output$output_table_create <- DT::renderDT(
        if(colnames(values$dataset)[1] == 'var1') {
          return(NULL)
        } else {
        head(values$dataset)
        }
      )
      ###----
      
      #----
      #Zonal definition
      #----
      output$conditionalInput1 <- renderUI({
        conditionalPanel(condition="input.choiceTab=='primary'",
                         tagList(
                           selectizeInput('catchBase','Variable containing catch data',
                                          choices=colnames(values$dataset[,grep('haul|mt|lb|metric|pounds|catch', colnames(values$dataset), ignore.case=TRUE)])),
                           selectizeInput('priceBase', 'Variable containing price or value data', 
                                          choices=c('none selected'='none', colnames(values$dataset[,grep('value|dollar', colnames(values$dataset), ignore.case=TRUE)])), selected='none'),
                           div(style="display: inline-block;vertical-align:top; width: 200px;",
                               selectInput('latBase', 'Occurrence latitude', choices=c('',colnames(values$dataset[,grep('lat', colnames(values$dataset), ignore.case=TRUE)])), selected='')),
                           div(style="display: inline-block;vertical-align:top; width: 200px;",
                               selectInput('lonBase', 'Occurrence longitude', choices=c('',colnames(values$dataset[,grep('lon', colnames(values$dataset), ignore.case=TRUE)])), selected=''))
                         ))
      })
      output$conditionalInput2 <- renderUI({
        conditionalPanel(condition="input.choiceTab=='zone'",
                         tagList(
                           #fileInput("fileGridExC", "Choose data file containing spatial data defining zones (shape, json, and csv formats are supported)",
                           #          multiple = FALSE, placeholder = ''),
                           if(names(spatdat$dataset)[1]=='var1'){
                             tags$div(h4('Map file not loaded. Please load on Upload Data tab', style="color:red"))
                           },
                           h5(tags$b('Select latitude than longitude from main dataset for assigning observations to zones')),
                           div(style="display: inline-block;vertical-align:top; width: 200px;",
                               selectizeInput('lat_dat_ac', '',
                                              choices=c(input$latBase, names(values$dataset)[grep('lat', names(values$dataset), ignore.case=TRUE)]), 
                                              selected=c(input$latBase))),
                           div(style="display: inline-block;vertical-align:top; width: 200px;",
                               selectizeInput('lon_dat_ac', '', choices=c(input$lonBase, names(values$dataset)[grep('lon', names(values$dataset), ignore.case=TRUE)]), 
                                              selected=c(input$lonBase))),
                           selectInput('cat_altc', 'Individual areas/zones from the spatial data set', choices=names(as.data.frame(spatdat$dataset))),
                           selectInput('weight_var_ac', 'If desired, variable for use in calculating weighted centroids', choices=c('none'="", colnames(values$dataset))), #variable weighted centroids
                           checkboxInput('hull_polygon_ac', 'Use convex hull method to create polygon?', value=FALSE),
                           checkboxInput('closest_pt_ac', 'Use closest polygon to point?', value=FALSE) 
                         ) )
      })  
      
       observeEvent(input$runCentroid, {
         showNotification(
           capture.output(
              values$dataset <-  assignment_column(dat=values$dataset, gridfile=spatdat$dataset, lon.dat=input$lon_dat_ac, lat.dat=input$lat_dat_ac, 
                                             cat=input$cat_altc, closest.pt = input$closest_pt_ac, lon.grid=NULL,
                                              lat.grid=NULL, hull.polygon = input$hull_polygon_ac, epsg=NULL)),
           type='message', duration=10)
        
      })
       
      output$cond2 <- renderUI({
        conditionalPanel(condition="input.choiceTab=='zone'",
                         if(any(class(spatdat$dataset)=='sf')==FALSE){
                           tagList(
                             h5(tags$b('Select vector containing latitude then longitude from spatial data set')),
                             div(style="display: inline-block;vertical-align:top; width: 200px;",
                                 selectizeInput('lat_grid_altc', '', choices=names(as.data.frame(spatdat$dataset)), multiple=TRUE)),
                             div(style="display: inline-block;vertical-align:top; width: 200px;",
                                 selectizeInput('long_grid_altc',  '',choices=names(as.data.frame(spatdat$dataset)), multiple=TRUE))
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
                               selectizeInput('occasion_ac','and centroid or lat/lon of location', 
                                              choices=c('Centroid of zonal assignment'='centroid', names(values$dataset)[grep('lat|lon', names(values$dataset), ignore.case=TRUE)]), 
                                              selected='centroid', options = list(maxItems = 2))),
                           selectizeInput('dist_ac','Distance units', choices=c('miles','kilometers','meters'), selected='miles'),
                           numericInput('min_haul_ac', 'Include zones with more hauls than', min=1, max=1000, value=1)#,
                           #checkboxInput('morec', 'Show more choices', value=FALSE),
                           #Additional choices
                           #uiOutput('conditionalInput')
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
          ggplot2::ggplot(values$dataset[which(values$dataset$ZoneID %in% temp[which(temp$Freq > input$min_haul_ac),1]), ], ggplot2::aes(x=ZoneID)) + ggplot2::geom_histogram() + 
            ggplot2::theme_bw() + ggplot2::theme(panel.border = ggplot2::element_blank(), panel.grid.major = ggplot2::element_blank(),
                               panel.grid.minor = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"))
        }
      })
      
      output$zoneIDText <- renderText({
        if(!any(colnames(values$dataset)=='ZoneID')){
          return()
        } else {
          temp <- data.frame(table(values$dataset$ZoneID))
          paste('number of records:',dim(values$dataset[which(values$dataset$ZoneID %in% temp[which(temp$Freq > input$min_haul_ac),1]), ])[1], 
                '\nnumber of zones:', nrow(temp[which(temp$Freq > input$min_haul_ac),]))
        }
      })
      
      output$zoneIDNumbers_plot <- renderPlot(zoneIDNumbers_dat())
      
      
      #-----
      
      #----
      #Expected Catch      
      #----
      output$selectcp <- renderUI({
        tagList(
          selectInput('catche','Catch variable for averaging',
                      choices=c(input$catchBase, colnames(values$dataset[,grep('haul|mt|lb|metric|pounds|catch', colnames(values$dataset), ignore.case=TRUE)])),
                      selected=input$catchBase),
          selectizeInput('price', 'If expected revenue is to be calculated, variable containing price or value data', 
                         choices=c(input$priceBase, "none", colnames(values$dataset[,grep('value|dollar', colnames(values$dataset), ignore.case=TRUE)]))),
          selectizeInput('group','Choose variable that defines groups',
                         choices=c('Fleet (no group)', names(values$dataset[, !sapply(values$dataset, is.numeric)])))
        )
      })
      output$expcatch <-  renderUI({
        conditionalPanel(condition="input.temporal!='Entire record of catch (no time)'",
                         style = "margin-left:19px;font-size: 12px", 
                         selectInput('temp_var', 'Temporal variable for averaging', 
                                     choices=c('none', names(values$dataset)[grep('date|year|hour|day', colnames(values$dataset), ignore.case = TRUE)]),
                                     selected='none'))
      })
      sparstable_dat <- reactive({
        if(!any(colnames(values$dataset)=='ZoneID')){
          return()
        } else if(is_empty(input$catche)){
          return()
        } else if(input$temp_var=='none'){
          return()
        } else{
          sparsetable(values$dataset, input$projectname, timevar=input$temp_var, zonevar='ZoneID', var=input$catche)
        }
      })
      
      output$spars_table <- DT::renderDT(sparstable_dat(), server=TRUE)
      output$spars_plot <- renderPlot({
        if(!any(colnames(values$dataset)=='ZoneID')){
          return()
        } else if(is_empty(input$catche)){
          return()
        } else if(input$temp_var=='none'){
          return()
        } else {
          print(sparsplot(sparstable_dat(), input$projectname))
        }
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
        tagList(
          selectInput('catch','Variable containing catch data',
                      choices=c(input$catchBase, colnames(values$dataset[,grep('haul|mt|lb|metric|pounds|catch', colnames(values$dataset), ignore.case=TRUE)])), 
                      selected=input$catchBase),
          conditionalPanel(
            condition="input.model=='epm_normal' || input.model=='epm_lognormal' || input.model=='epm_weibull'",
            checkboxInput('lockk', 'Location-specific catch parameter', value=FALSE)),
          conditionalPanel(condition="input.model=='epm_normal' || input.model=='epm_lognormal' || input.model=='epm_weibull'",
            selectInput('price', 'Price variable', choices=c(input$priceBase,'none', colnames(values$dataset[,grep('dollar|val|euro|price|cost|earn', colnames(values$dataset), ignore.case=TRUE)])), 
                        selected='none', multiple=FALSE)),
        #logit correction
          conditionalPanel(condition="input.model=='logit_correction'",
            numericInput('polyn', 'Correction polynomial degree', value=2)),
          conditionalPanel(condition="input.model=='logit_correction'",
                           radioButtons('startlocdefined', 'Starting location variable', choices=c('Exists in data set'='exists', 'Create variable'='create'))
        ))
        })
        output$logit_correction_extra <- renderUI({
          tagList(
          conditionalPanel(condition="input.model=='logit_correction' && input.startlocdefined=='exists'",
            selectInput('startloc_mod', 'Initial location during choice occassion', choices=names(values$dataset), 
                        selected='startingloc', 
                        multiple=FALSE)),
            conditionalPanel(condition="input.model=='logit_correction' && input.startlocdefined=='create'",
                              if(names(spatdat$dataset)[1]=='var1'){
                                                 tags$div(h4('Map file not loaded. Please load on Upload Data tab', style="color:red"))
                              }),
            conditionalPanel(condition="input.model=='logit_correction' && input.startlocdefined=='create'",
                             selectInput('trip_id_SL_mod', 'Variable in primary data set to identify unique trips', choices=c('',names(values$dataset)), selectize=TRUE)),
            conditionalPanel(condition="input.model=='logit_correction' && input.startlocdefined=='create'",
                             selectInput('haul_order_SL_mod', 'Variable in primary data set defining haul order within a trip. Can be time, coded variable, etc.',
                                            choices=c('', names(values$dataset)), selectize=TRUE)),
            conditionalPanel(condition="input.model=='logit_correction' && input.startlocdefined=='create'",
                             selectInput('starting_port_SL_mod',  "Variable in primary data set identifying port at start of trip", 
                                            choices=names(values$dataset[,grep('port',names(values$dataset), ignore.case = TRUE)]), selectize=TRUE)),
            conditionalPanel(condition="input.model=='logit_correction' && input.startlocdefined=='create'",
                             selectInput('lon_dat_SL_mod', "Longitude variable in primary data set", 
                                            choices= names(values$dataset[,grep("lon", names(values$dataset), ignore.case = TRUE)]), selectize=TRUE)), 
            conditionalPanel(condition="input.model=='logit_correction' && input.startlocdefined=='create'",
                             selectInput('lat_dat_SL_mod', "Latitude variable in primary data set", 
                                            choices= names(values$dataset[,grep("lat", names(values$dataset), ignore.case = TRUE)]), selectize=TRUE)),
            conditionalPanel(condition="input.model=='logit_correction' && input.startlocdefined=='create'",
                           if(any(class(spatdat$dataset)=='sf')==FALSE){
                                                selectInput('lat_grid_SL_mod', 'Select vector containing latitude from spatial data set',
                                                            choices= names(as.data.frame(spatdat$dataset)), multiple=TRUE)
                              }),
            conditionalPanel(condition="input.model=='logit_correction' && input.startlocdefined=='create'",
                             if(any(class(spatdat$dataset)=='sf')==FALSE){
                                selectInput('lon_grid_SL_mod', 'Select vector containing longitude from spatial data set', 
                                                             choices= names(as.data.frame(spatdat$dataset)), multiple=TRUE, selectize=TRUE)
                              }),
            conditionalPanel(condition="input.model=='logit_correction' && input.startlocdefined=='create'",
                             selectInput('cat_SL_mod', "Variable defining zones or areas", choices= names(as.data.frame(spatdat$dataset)), selectize=TRUE)
                         
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
      Alt_vars <- reactive({
        if(!exists("Alt")) {
        if(!exists('AltMatrixName')) {
          if(DBI::dbExistsTable( DBI::dbConnect(RSQLite::SQLite(), locdatabase()), paste0(input$projectname, 'altmatrix'))){
          return(unserialize(DBI::dbGetQuery( DBI::dbConnect(RSQLite::SQLite(), locdatabase()), paste0("SELECT AlternativeMatrix FROM ", 
                                                                                              input$projectname, "altmatrix LIMIT 1"))$AlternativeMatrix[[1]]))
          } else {
            warning("Alternative Choice Matrix does not exist. Please run the createAlternativeChoice() function.")
            return(data.frame('choice'=NA, 'X2'=NA, 'X3'=NA))
        }
          DBI::dbDisconnect( DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
        }} else {
        return(Alt)
        }
      })
          
      choice <- reactive({Alt_vars()$choice})
      alt <- reactive({dim(table(choice()))})
      
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
      
      output$portmd <- renderUI ({
      selectInput("port.datMD", "Choose file from FishSET SQL database containing port data", 
                                                                 choices=tables_database()[grep('port', tables_database(), ignore.case=TRUE)], multiple = FALSE)#,
      })
      
      numInits <- reactive({
        polyn <- input$polyn
        gridNum <- as.integer(as.factor(length(input$gridVariablesInclude)))
        intNum <- as.integer(length(input$indeVarsForModel))
        if(input$model == 'logit_c'){
          numInits <- gridNum+intNum
        } else if(input$model == 'logit_avgcat') {
          numInits <- gridNum*(alt()-1)+intNum
        } else if(input$model == 'logit_correction'){
          numInits <- gridNum*4 + intNum + ((((polyn+1)*2)+2)*4) +1+1
        } else {
          if(input$lockk=='TRUE'){
            numInits <- gridNum*alt()+intNum+alt+1
          } else {
            numInits <- gridNum*alt()+intNum+1+1
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
      #  if(input$addModel > 0) print('Save model, reset parameters')
      #  output$table <- renderDataTable(dat[1:3,1:3])
      #make_model_design()
      #})
      counter <- reactiveValues(countervalue = 0) # Defining & initializing the reactiveValues object
      rv <- reactiveValues(
        data = model_table,
        deletedRows = NULL,
        deletedRowIndices = list()
      )
      
      
      #model_table <- reactiveVal(model_table)
                     
      #access variable int outside of observer
      int_name <- reactive({
        paste(lapply(1:numInits(), function(i) {
          inputName <- paste("int", i, sep = "")
          input[[inputName]]
        }))
      })
      
      observeEvent(input$addModel, {
        if(is.null(input$gridVariablesInclude)|is.null(input$indeVarsForModel)) {
          showNotification('Model not saved as at least one variable not defined.')
        } else {
          showNotification("Selected model parameters saved.", type='message', duration=10)
        }
       

        if(input$model=='logit_correction' & input$startlocdefined =='create'){
          values$dataset$startingloc <- create_startingloc(dat=values$dataset, gridfile=spatdat$dataset, portTable=input$port.datMD, 
                                            trip_id=input$trip_id_SL_mod, haul_order=input$haul_order_SL_mod, starting_port=input$starting_port_SL_mod, 
                                            lon.dat=input$lon_dat_SL_mod, lat.dat=input$lat_dat_SL_mod, cat=input$cat_SL_mod, 
                                            lon.grid==input$lat_grid_SL, lat.grid==input$lat_grid_SL_mod)
        } 
        counter$countervalue <- counter$countervalue + 1
        
        if(is.null(input$gridVariablesInclude)|is.null(input$indeVarsForModel)) {
          rv$data <- rbind(data.frame('mod_name'='', 
                                'likelihood'='',
                                'alternatives'='',
                                'optimOpts'='',
                                'inits'='',
                                'vars1'= '',
                                'vars2'='', 
                                'catch'='',
                                'lon'='', 
                                'lat'='', 
                                'project'=input$projectname, 
                                'price'='',
                                'startloc'='',
                                'polyn'='')
                     , rv$data)#model_table())
        } else {
          rv$data = rbind(data.frame('mod_name'=paste0('mod',counter$countervalue), 
                               'likelihood'=input$model, 
                               'alternatives'=input$alternatives,
                               'optimOpts'=paste(input$mIter,input$relTolX, input$reportfreq, input$detailreport),
                               'inits'=paste(int_name(), collapse=','),#noquote(paste0('input$int',1:numInits())),
                               'vars1'= paste(input$indeVarsForModel, collapse=', '),
                               'vars2'=input$gridVariablesInclude, 
                               'catch'=input$catch,
                               'lon'=input$lon, 
                               'lat'=input$lat,
                               'project'=input$projectname, 
                               'price'=input$price,
                               'startloc'=if(input$startlocdefined=='exists'){input$startloc_mod} else {'startingloc'}, 
                               'polyn'=input$polyn)
                    , rv$data)#model_table())
          print(rv$data)
        }
      #  rv$data(t)#model_table(t)
        
        
        ###Now save table to sql database. Will overwrite each time we add a model
        fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase())
        #First, remove any old instances of the table
        if(DBI::dbExistsTable(fishset_db, paste0(input$projectname,'modelDesignTable', format(Sys.Date(), format="%Y%m%d")))==TRUE){
          DBI::dbRemoveTable(DBI::dbConnect(RSQLite::SQLite(), locdatabase()), paste0(input$projectname, 'modelDesignTable', format(Sys.Date(), format="%Y%m%d")))
        }
        
        if(DBI::dbExistsTable(fishset_db, paste0(input$projectname, 'modelDesignTable', format(Sys.Date(), format="%Y%m%d")))==FALSE){
          DBI::dbExecute(fishset_db, paste0("CREATE TABLE ", paste0(input$projectname,'modelDesignTable', format(Sys.Date(), format="%Y%m%d")),
                                            "(mod_name TEXT, likelihood TEXT, alternatives TEXT, optimOpts TEXT, inits TEXT, vars1 TEXT, vars2 TEXT,  
                                            catch TEXT, lon TEXT, lat TEXT, project TEXT, price TEXT, startloc TEXT, polyn TEXT)"))
        }
        # Construct the update query by looping over the data fields
        query <- sprintf(
          "INSERT INTO %s (%s) VALUES %s",
          paste0(input$projectname,'modelDesignTable', format(Sys.Date(), format="%Y%m%d")),
          paste(names(data.frame(as.data.frame(isolate(rv$data #model_table()
                                                       )))), collapse = ", "),
          paste0("('", matrix(apply(as.data.frame(isolate(rv$data #model_table()
                                                          )), 1, paste, collapse="','"), ncol=1), collapse=',', "')")
        )
        # Submit the update query and disconnect
        DBI::dbGetQuery(fishset_db, query)
        DBI::dbDisconnect(fishset_db)
        
        
      })
      
#      observeEvent(input$resetModel, {
#        shinyjs::reset("form")
#      })
      
      
      observeEvent(input$deletePressed, {
        rowNum <- parseDeleteEvent(input$deletePressed)
        dataRow <- rv$data[rowNum,]
        
        # Put the deleted row into a data frame so we can undo
        # Last item deleted is in position 1
        rv$deletedRows <- rbind(dataRow, rv$deletedRows)
        rv$deletedRowIndices <- append(rv$deletedRowIndices, rowNum, after = 0)
        
        # Delete the row from the data frame
        rv$data <- rv$data[-rowNum,]
      })
      
      
      output$mod_param_table <- DT::renderDataTable(
        # Add the delete button column
        deleteButtonColumn(rv$data, 'delete_button')
      )
    
#         output$mod_param_table <- DT::renderDT(
#        model_table(), editable = T, server=TRUE
#      )
 
  
      # Run models shiny
      observe({
        if(input$submit > 0) {
          input_list <- reactiveValuesToList(input)
          toggle_inputs(input_list,F)
          #print('call model design function, call discrete_subroutine file')
          times <- nrow(rv$data)-1
          i <- 1
          showNotification(paste('1 of', times, 'model design files created.'), type='message', duration=10)
          make_model_design(values$dataset, project=rv$data$project[i], catchID=rv$data$catch[i], alternativeMatrix = rv$data$alternatives[i], 
                            replace=TRUE, lonlat= c(as.vector(rv$data$lon[i]), as.vector(rv$data$lat[i])), PortTable = input$port.datMD, likelihood=rv$data$likelihood[i], vars1=rv$data$vars1[i],
                            vars2=rv$data$vars2[i], priceCol=rv$data$price[i], startloc=rv$data$startloc[i], polyn=rv$data$polyn[i])
          
          if(times>1){
          for(i in 2:times){
            make_model_design(values$dataset, project=rv$data$project[i], catchID=rv$data$catch[i], alternativeMatrix = rv$data$alternatives[i], 
                              replace=FALSE, lonlat=c(as.vector(rv$data$lon[i]), as.vector(rv$data$lat[i])), PortTable = input$port.datMD, likelihood=rv$data$likelihood[i], vars1=rv$data$vars1[i],
                              vars2=rv$data$vars2[i], priceCol=rv$data$price[i], startloc=rv$data$startloc[i], polyn=rv$data$polyn[i])
            showNotification(paste(i, 'of', times, 'model design files created.'), type='message', duration=10)
          }
          }
         # showNotification(
         #   capture.output(
                showNotification('Model is running. Models can take 30 minutes.
                                  All buttons are inactive while model function is running.
                                  Check R console for progress.', type='message', duration=30)
                discretefish_subroutine(input$projectname, initparams=rv$data$inits, optimOpt=rv$data$optimOpt,  
                                  methodname='BFGS', mod.name=rv$data$mod_name, select.model=FALSE, name='discretefish_subroutine')              
        #    ), type='message', duration=10)
                showMessage('Model run is complete. Check the `Compare Models` subtab to view output', type='message', duration=10)
          toggle_inputs(input_list,T)
        }
      })
      
      
    ## Explore models sections
      #out_mod <- reactive({
      fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase())
      #     return(DBI::dbGetQuery(DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"), paste0("SELECT * FROM", paste0(project, "modelfit"))))
      # })
      
      shinyInput = function(FUN, len, id, ...) { 
        inputs = character(len) 
        for (i in seq_len(len)) { 
          inputs[i] = as.character(FUN(paste0(id, i), label = NULL, ...)) 
        } 
        inputs 
      } 
      
      temp <- isolate(paste0(input$projectname, "modelfit"))
      this_table <- reactive(
        if(DBI::dbExistsTable(fishset_db, paste0(input$projectname, 'modelfit'))){
          data.frame(t(DBI::dbGetQuery(DBI::dbConnect(RSQLite::SQLite(), locdatabase()), 
                                       paste0("SELECT * FROM ", paste0(input$projectname, "modelfit")))))
        } else {
          data.frame('X1'=NA, 'X2'=NA, 'X3'=NA, 'X4'=NA)
        }
        )#,Select=shinyInput(checkboxInput,nrow(t(out.mod)),"cbox_")))
      
      observeEvent(input$delete_btn, {
        t = this_table()
        if(!is.null(input$mytable_rows_selected)) {
          t <- t[-as.numeric(input$mytable_rows_selected),]
        }
        this_table(t)
        session$sendCustomMessage('unbind-DT', 'mytable')
      })
      
      # datatable with checkbox
      output$mytable <- DT::renderDT({
        data.frame(this_table(),Select=shinyInput(checkboxInput,nrow(this_table()),"cbox_"))
      }, colnames=c('Model','AIC','AICc','BIC','PseudoR2','Selected'),  filter='top', server = TRUE, escape = FALSE, options = list( 
        dom = 't', paging=FALSE,
        preDrawCallback = DT::JS('function() { 
                                 Shiny.unbindAll(this.api().table().node()); }'), 
        drawCallback = DT::JS('function() { 
                              Shiny.bindAll(this.api().table().node()); } ') 
        ) )
      
      
      # helper function for reading checkbox
      shinyValue = function(id, len) { 
        unlist(lapply(seq_len(len), function(i) { 
          value = input[[paste0(id, i)]] 
          if(is.null(value)) NA else value 
        })) 
      } 
      
      shinyDate = function(id, len) { 
        unlist(lapply(seq_len(len), function(i) { 
          value=ifelse(input[[paste0(id, i)]]!=TRUE, '' , as.character(Sys.Date())) 
        })) 
      }
      
      checkedsave <- reactive(cbind(
        model = rownames(isolate(this_table())),#colnames(out.mod), 
        AIC=isolate(this_table()[,1]),
        AICc=isolate(this_table()[,2]),
        BIC=isolate(this_table()[,3]),
        PseudoR2=isolate(this_table()[,4]),#t(out.mod), 
        Selected = shinyValue("cbox_", nrow(this_table())),#t(out.mod))), 
        Date = shinyDate("cbox_", nrow(this_table())) 
      ))#t(out.mod)))))
      
      
      # When the Submit button is clicked, save the form data
      observeEvent(input$submit_ms, {
        # Connect to the database
        fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase())
        if(overwrite_table==T){
          if(DBI::dbExistsTable(fishset_db, 'modelChosen')==TRUE){
            DBI::dbRemoveTable(DBI::dbConnect(RSQLite::SQLite(), locdatabase()), 'modelChosen')
          }
        }
        
        if(DBI::dbExistsTable(fishset_db, 'modelChosen')==FALSE){
          DBI::dbExecute(fishset_db, "CREATE TABLE modelChosen(model TEXT, AIC TEXT, AICc TEXT, BIC TEXT, PseudoR2 TEXT, Selected TEXT, Date TEXT)")
        }
        # Construct the update query by looping over the data fields
        query <- sprintf(
          "INSERT INTO %s (%s) VALUES %s",
          "modelChosen", 
          paste(names(data.frame(as.data.frame(isolate(checkedsave())))), collapse = ", "),
          paste0("('", matrix(apply(as.data.frame(isolate(checkedsave())), 1, paste, collapse="','"), ncol=1), collapse=',', "')")
        )
        # Submit the update query and disconnect
        DBI::dbGetQuery(fishset_db, query)
        
        showNotification("Table saved to database")
      })
      DBI::dbDisconnect(fishset_db)
      
    
      #Add in two more tables for model evaulations
      suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
      mod_sum_out <- reactive({
        if(DBI::dbExistsTable(fishset_db, paste0(input$projectname, 'modelOut', format(Sys.Date(), format="%Y%m%d")))){#pollockmodelOut20190610#))
          model_out_view(paste0(input$projectname, 'modelOut', format(Sys.Date(), format="%Y%m%d")))#pollockmodelOut20190610))#
      } else {
         data.frame('var1'=0, 'var2'=0)
      }
      })
      
      output$modeltab <- DT::renderDT({
        modeltab <- data.frame(Model_name=rep(NA, length(mod_sum_out())), covergence=rep(NA, length(mod_sum_out())), Stand_Errors=rep(NA, length(mod_sum_out())), Hessian=rep(NA, length(mod_sum_out())))
        #if(dim(mod_sum_out())[2]>2){
        #  modeltab[i,1] <- mod_sum_out()[[i]]$name
        if(is.data.frame(mod_sum_out())){
          modeltab <- modeltab
        } else {
        for(i in 1:length(mod_sum_out())){
          modeltab[i,1] <- mod_sum_out()[[i]]$name
          modeltab[i,2] <- mod_sum_out()[[i]]$optoutput$convergence
          modeltab[i,3] <- toString(round(mod_sum_out()[[i]]$seoutmat2,3))
          modeltab[i,4] <- toString(round(mod_sum_out()[[i]]$H1,5))
        }}
        #return(modeltab)
      })
      
      
      output$errortab <- DT::renderDT({

          error_out <- data.frame(Model_name=rep(NA, length(mod_sum_out())), Model_error=rep(NA, length(mod_sum_out())), Optimization_error=rep(NA, length(mod_sum_out())))
          #if(dim(mod_sum_out())[2]>2){ 
          if(colnames(mod_sum_out())[1]=='var1'){
            error_out <- error_out
          } else {
          for(i in 1: length(mod_sum_out())){
              error_out[i,1] <- mod_sum_out()[[i]]$name
              error_out[i,2] <- ifelse(is.null(mod_sum_out()[[i]]$errorExplain), 'No error reported', toString(mod_sum_out()[[i]]$errorExplain))
              error_out[i,3] <- ifelse(is.null(mod_sum_out()[[i]]$optoutput$optim_message), 'No message reported', toString(mod_sum_out()[[i]]$optoutput$optim_message))
            }}
          #return(error_out)
      })
      
      #----      
      
      #----
      # Run functions
      #-----
      observeEvent(input$saveALT, {
         showNotification('Alternative choice matrix updated', type='message', duration=10)
        showNotification(
          capture.output(
              create_alternative_choice(dat=values$dataset, gridfile=spatdat$dataset, case=input$case_ac, min.haul=input$min_haul_ac,
                                  alt_var=input$alt_var_ac, occasion=input$occasion_ac, dist.unit=input$dist_ac, lon.dat=input$lon_dat_ac,
                                  lat.dat=input$lat_dat_ac, lon.grid=input$long_grid_altc, lat.grid=input$lat_grid_altc, 
                                  cat=input$cat_altc, hull.polygon=input$hull_polygon_ac, 
                                  closest.pt=input$closest_pt_ac, project=input$projectname, griddedDat=NULL, weight.var=input$weight_var_ac)),
          type='message', duration=10)

      }, ignoreInit = F) 
      
      
      
      observeEvent(input$submitE, {
        showNotification('Create expectated catch function called', type='message', duration=10)
        showNotification(
          capture.output(
                create_expectations(values$dataset, input$projectname, input$catche, price=input$price, defineGroup=if(grepl('no group',input$group)){NULL} else {input$group}, temp.var=input$temp_var, 
                            temporal = input$temporal, calc.method = input$calc_method, lag.method = input$lag_method,
                            empty.catch = input$empty_catch, empty.expectation = input$empty_expectation,  
                            temp.window = input$temp_window, temp.lag = input$temp_lag, year.lag=input$temp_year, dummy.exp = input$dummy_exp, replace.output = TRUE)),
          type='message', duration=10)
      }) 
      
      
      ####----
      ##Resetting inputs
      observeEvent(input$refresh1,{
        updateCheckboxInput(session, 'Outlier_Filter', value=FALSE)
      })
      ###----                
      
      ####-----        
      ##Save output       
      ###----      
      observeEvent(input$saveData, {
        suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
        DBI::dbWriteTable(fishset_db, paste0(input$projectname, 'MainDataTable'), values$dataset, overwrite=TRUE)
        DBI::dbDisconnect(fishset_db)
      })
      
      observeEvent(input$saveDataQ, {
        suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
        DBI::dbWriteTable(fishset_db, paste0(input$projectname, 'MainDataTable'), values$dataset, overwrite=TRUE)
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
          conditionalPanel(condition = "input.plot_type=='Spatial'",
          actionButton('downloadTableExplore', label ='Save table to folder as csv')),
          downloadLink("downloadTextExplore", label='')
        )
      })
      
      output$SaveButtonsAnal <- renderUI({
        tagList(
          downloadLink('downloadplotAnalHIDE', label =''),
          downloadLink('downloaddataAnalHIDE', label =''),
          actionButton('downloadplotAnal', label ='Save plot to folder'),#, title = "", filename = paste0(project,'_', input$corr_reg, '_plot'), filetype = "png"),
          actionButton('downloaddataAnal', label ='Save table to folder as csv'),
          downloadLink("downloadTextAnal", label=''),
          actionButton('callTextDownloadAnal','Save notes')
        )
      })
      
      output$SaveButtonsNew <- renderUI({
        tagList(
          downloadLink('downloadplotNew', label=''),
          actionButton('downloadplotNew', label ='Save plot to folder'),#, title = "", filename = paste0(project, input$plot_type , '_plot'), filetype = "png")
          downloadLink("downloadTextNew", label=''),
          actionButton('callTextDownloadNew','Save notes')
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
                     input$callTextDownloadNew,
                     input$callTextDownloadBook),{
                       savedText$answers <- as.character(c(savedText$answers, case_to_print(), notes()))
                     })
      
      #  Stored Txt
      observeEvent(input$callTextDownloadUp, {
        output$downloadTextUp <- downloadHandler(
          filename = function() {
            paste0(locoutput(), 'StoredText.txt')
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
            paste0(locoutput(), 'StoredText.txt')
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
            paste0(locoutput(), 'StoredText.txt')
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
            paste0(locoutput(), 'StoredText.txt')
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
            paste0(locoutput(), 'StoredText.txt')
          },
          content = function(file) {
            writeLines(savedText$answers, file)
          },
          contentType = "text/csv"
        )
        jsinject <- "setTimeout(function(){window.open($('#downloadTextNew').attr('href'))}, 100);"
        session$sendCustomMessage(type = 'jsCode', list(value = jsinject))   
      })
      observeEvent(input$callTextDownloadBook, {
        output$downloadTextBook <- downloadHandler(
          filename = function() {
            paste0(locoutput(), 'StoredText.txt')
          },
          content = function(file) {
            writeLines(savedText$answers, file)
          },
          contentType = "text/csv"
        )
        jsinject <- "setTimeout(function(){window.open($('#downloadTextBook').attr('href'))}, 100);"
        session$sendCustomMessage(type = 'jsCode', list(value = jsinject))   
      })
      
      observeEvent(input$downloadplot, {
        output$downloadplotHIDE <<- downloadHandler(
          filename = function() {
            paste0(locoutput(), input$projectname, "_", 'Outlier.png')
          },
          content = function(file) {
            ggplot2::ggsave(file, plot=outlier_plot(values$dataset, input$projectname, input$column_check, input$dat.remove, input$x_dist))
          })
        jsinject <- "setTimeout(function(){window.open($('#downloadplotHIDE').attr('href'))}, 100);"
        session$sendCustomMessage(type = 'jsCode', list(value = jsinject))   
      })
      
      observeEvent(input$downloadplotAnal, {
        output$downloadplotAnalHIDE <<- downloadHandler(
          filename = function() {
            if(input$corr_reg=='Correlation'){
              paste0(locoutput(), input$projectname, "_", 'CorrelationPlot.png')
            } else {
              paste0(locoutput(), input$projectname,"_", 'RegressionPlot.png')
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
              
              paste0(locoutput(), input$projectname, "_", 'TemporalPlot.png')
            } else if(input$plot_type=='Spatial') {
              paste0(locoutput(), input$projectname,"_", 'SpatialPlot.png') 
            } else {
              paste0(locoutput(), input$projectname,"_", 'x-yPlot.png') 
            }
          },
          content = function(file) {
            if(input$plot_type=='Temporal'){
              ggplot2::ggsave(file, plot=plotInput_time(), device=function(..., width, height) grDevices::png(..., width = 12, height = 4, res = 300, units = "in")) 
            } else if(input$plot_type=='Spatial'){
              longitude <- which(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)', ignore.case=TRUE)==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)', ignore.case=TRUE)))[1]
              latitude <- which(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)', ignore.case=TRUE)==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)', ignore.case=TRUE)))[1]
              cf <- ggplot2::coord_fixed()
              cf$default <- TRUE
              p1 <- ggplot2::ggplot(data = ggplot2::map_data("world"), mapping = ggplot2::aes(x = long, y = lat, group=group)) + 
                ggplot2::geom_polygon(color = "black", fill = "gray") + 
                ggplot2::geom_point(data = values$dataset, ggplot2::aes(x = values$dataset[,longitude], y = values$dataset[,latitude], group=rep(1, nrow(values$dataset))), color = "red", size = 1) +
                cf + ggplot2::coord_fixed(xlim = ranges_spatial$x, ylim = ranges_spatial$y, ratio=1.3, expand = TRUE)+
                ggplot2::labs(x='Longitude', y='Latitude', subtitle='Observed locations')+
                ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), 
                      panel.background = ggplot2::element_blank(),  axis.text=ggplot2::element_text(size=12),
                      axis.title=ggplot2::element_text(size=12),panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=1) )
              
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
        write.csv(gtmt_table(), paste0(locoutput(), input$projectname, '_', 'GetisOrdMoransI.csv'))
      })
      
      observeEvent(input$downloaddata, {
        if(input$checks=='Summary table'){
          write.csv(tableInputSummary(), paste0(locoutput(), input$projectname, '_', 'summary_table.csv'))
        } else if(input$checks=='Outliers'){
          write.csv(tableInputOutlier(), paste0(locoutput(), input$projectname, '_', 'outlier_table.csv'))
        }
      })
      
      observeEvent(input$downloaddataAnal, {
        if(input$corr_reg=='Correlation'){
        if(length(input$corr_select)>2){
            write.csv(tableInputCorr(), paste0(locoutput(), input$projectname,'_', 'correlation_table.csv'))
        } else {
            sink(paste0(locoutput(),input$projectname, "_", 'correlation_analysis_output.csv'))
            print(cor.test(values$dataset[[input$corr_select[1]]], values$dataset[[input$corr_select[2]]]))
            sink()
       }} else {
            sink(paste0(locoutput(), input$projectname,'_', 'regression_model_output.csv'))
            print(p2())
            sink()
        }
      })
      
      ##----
      # stop shiny
      observe({
        if(input$close > 0) stopApp()
      })
      observe({
        if(input$close1 > 0) stopApp()
      })
      observe({
        if(input$close2 > 0) stopApp()
      })
      observe({
        if(input$closeNew > 0) stopApp()
      })
      
      
      ###----
      # Update From Bookmarked state
      ###----    
      bookmarkedstate <- reactive({
        req(input$uploadbookmark)
        readRDS(input$uploadbookmark$datapath)
      })
      
      observe({
        req(input$uploadbookmark)
          req(bookmarkedstate()$loadDat==1)
          if(bookmarkedstate()$loadmainsource=="FishSET database"){
          updateTextInput(session, 'projectname', value = bookmarkedstate()$projectname)
          #values$dataset <- table_view(paste0(input$projectname, 'MainDataTable'))
          }
      })
      
      observe({
        req(input$uploadbookmark)
        req(input$projectname)
        req(bookmarkedstate()$loadDat==1)
        if(bookmarkedstate()$loadmainsource=="FishSET database"){
          values$dataset <- table_view(paste0(input$projectname, 'MainDataTable'))
        }
      })
      
      observeEvent(input$uploadbookmark, {
        req(input$projectname)
        if(colnames(values$dataset)[1]!='var1'){
          #-----
        updateSelectInput(session, "alt_var_ac", selected = bookmarkedstate()$alt_var_ac) 
        updateSelectInput(session, "alternatives", selected = bookmarkedstate()$alternatives) 
        updateSelectInput(session, "calc_method", selected = bookmarkedstate()$calc_method) 
        updateSelectInput(session, "case_ac", selected = bookmarkedstate()$case_ac) 
        updateSelectInput(session, "cat", selected = bookmarkedstate()$cat) 
        updateSelectInput(session, "cat_altc", selected = bookmarkedstate()$cat_altc) 
        updateSelectInput(session, "cat_SL", selected = bookmarkedstate()$cat_SL) 
        updateSelectInput(session, "catch", selected = bookmarkedstate()$catch) 
        updateSelectInput(session, "catchBase", selected = bookmarkedstate()$catchBase) 
        updateSelectInput(session, "catche", selected = bookmarkedstate()$catche) 
        updateRadioButtons(session, 'checks', selected = bookmarkedstate()$checks)
        updateRadioButtons(session, 'choiceTab', selected=bookmarkedstate()$choiceTab)
        updateCheckboxInput(session, 'sp_colgeartype', value=bookmarkedstate()$sp_colgeartype)
        updateCheckboxInput(session, 'sp_collocation', value=bookmarkedstate()$sp_collocation)
        updateCheckboxInput(session, "closest_pt_ac", value = bookmarkedstate()$closest_pt_ac) 
        updateSelectInput(session, "column_check", selected = bookmarkedstate()$column_check) 
        updateSelectInput(session, "corr_reg", selected = bookmarkedstate()$corr_reg) 
        updateSelectInput(session, "corr_select", selected = bookmarkedstate()$corr_select) 
        updateSelectInput(session, "create_method", selected = bookmarkedstate()$create_method)
        updateSelectInput(session, "define_format", selected = bookmarkedstate()$define_format) 
        updateNumericInput(session, "detailreport", value = bookmarkedstate()$detailreport) 
        updateSelectInput(session, "dist", selected = bookmarkedstate()$dist) 
        updateSelectInput(session, "dist_ac", selected = bookmarkedstate()$dist_ac) 
        updateSelectInput(session, "dummclosfunc", selected = bookmarkedstate()$dummclosfunc) 
        updateCheckboxInput(session, "dummy_exp", value = bookmarkedstate()$dummy_exp) 
        updateSelectInput(session, "dummyfunc", selected = bookmarkedstate()$dummyfunc) 
        updateSelectInput(session, "dummypolydate", selected = bookmarkedstate()$dummypolydate) 
        updateSelectInput(session, "dummypolyfunc", selected = bookmarkedstate()$dummypolyfunc) 
        updateSelectInput(session, "dummyvarfunc", selected = bookmarkedstate()$dummyvarfunc) 
        updateSelectInput(session, "dur_end", selected = bookmarkedstate()$dur_end) 
        updateSelectInput(session, "dur_end2", selected = bookmarkedstate()$dur_end2) 
        updateSelectInput(session, "dur_start", selected = bookmarkedstate()$dur_start)
        updateSelectInput(session, "dur_start2", selected = bookmarkedstate()$dur_start2) 
        updateSelectInput(session, "dur_units", selected = bookmarkedstate()$dur_units) 
        updateSelectInput(session, "empty_catch", selected = bookmarkedstate()$empty_catch) 
        updateSelectInput(session, "empty_expectation", selected = bookmarkedstate()$empty_expectation) 
        updateSelectInput(session, "end", selected = bookmarkedstate()$end) 
        updateSelectInput(session, "end_latlon", selected = bookmarkedstate()$end_latlon) 
        updateSelectInput(session, "ending_haul", selected = bookmarkedstate()$ending_haul) 
        updateSelectInput(session, "ending_port", selected = bookmarkedstate()$ending_port) 
        updateSelectInput(session, "fun_numeric", selected = bookmarkedstate()$fun_numeric) 
        updateSelectInput(session, "fun_time", selected = bookmarkedstate()$fun_time) 
        updateSelectInput(session, "gridVariablesInclude", selected = bookmarkedstate()$gridVariablesInclude) 
        updateSelectInput(session, "group", selected = bookmarkedstate()$group) 
        updateSelectInput(session, "haul_order", selected = bookmarkedstate()$haul_order) 
        updateSelectInput(session, "Haul_Trip_IDVar", selected = bookmarkedstate()$Haul_Trip_IDVar) 
        updateSelectInput(session, "haul_order_SL", selected = bookmarkedstate()$haul_order_SL) 
        updateCheckboxInput(session, "hull_polygon_ac", value = bookmarkedstate()$hull_polygon_ac)
        updateSelectInput(session, "ID", selected = bookmarkedstate()$ID) 
        updateSelectInput(session, "indeVarsForModel", selected = bookmarkedstate()$indeVarsForModel) 
        updateSelectInput(session, "lag_method", selected = bookmarkedstate()$lag_method) 
        updateSelectInput(session, "lat", selected = bookmarkedstate()$lat) 
        updateSelectInput(session, "lat_dat_ac", selected = bookmarkedstate()$lat_dat_ac) 
        updateSelectInput(session, "lat_dat_SL", selected = bookmarkedstate()$lat_dat_SL) 
        updateSelectInput(session, "lat_grid_SL", selected = bookmarkedstate()$lat_grid_SL) 
        updateSelectInput(session, "lat_grid_altc", selected = bookmarkedstate()$lat_grid_altc) 
        updateSelectInput(session, "latBase", selected = bookmarkedstate()$latBase) 
        updateCheckboxInput(session, "LatLon_Filter", value = bookmarkedstate()$LatLon_Filter) 
        updateCheckboxInput(session, "lockk", value = bookmarkedstate()$lockk) 
        updateSelectInput(session, "lon", selected = bookmarkedstate()$lon) 
        updateSelectInput(session, "lon_dat", selected = bookmarkedstate()$lon_dat) 
        updateSelectInput(session, "lon_dat_ac", selected = bookmarkedstate()$lon_dat_ac) 
        updateSelectInput(session, "lon_dat_SL", selected = bookmarkedstate()$lon_dat_SL) 
        updateSelectInput(session, "long_grid", selected = bookmarkedstate()$long_grid) 
        updateSelectInput(session, "lon_grid_SL", selected = bookmarkedstate()$lon_grid_SL) 
        updateSelectInput(session, "lonBase", selected = bookmarkedstate()$lonBase) 
        updateSelectInput(session, "long_grid_altc", selected = bookmarkedstate()$long_grid_altc)
        updateSelectInput(session, "mid_end", selected = bookmarkedstate()$mid_end) 
        updateSelectInput(session, "mid_start", selected = bookmarkedstate()$mid_start) 
        updateNumericInput(session, "min_haul_ac", value = bookmarkedstate()$min_haul_ac) 
        updateNumericInput(session, "mIter", value = bookmarkedstate()$mIter) 
        updateSelectInput(session, "mtgtcat", selected = bookmarkedstate()$mtgtcat) 
        updateSelectInput(session, "mtgtlonlat", selected = bookmarkedstate()$mtgtlonlat) 
        updateSelectInput(session, "NA_Filter", selected = bookmarkedstate()$NA_Filter) 
        updateSelectInput(session, "NAN_Filter", selected = bookmarkedstate()$NAN_Filter) 
        updateSelectInput(session, "numfunc", selected = bookmarkedstate()$numfunc) 
        updateSelectInput(session, "occasion_ac", selected = bookmarkedstate()$occasion_ac) 
        updateSelectInput(session, "plot_table", selected = bookmarkedstate()$plot_table) 
        updateSelectInput(session, "plot_type", selected = bookmarkedstate()$plot_type) 
        updateTextInput(session, 'polyear', vaue=bookmarkedstate()$polyear)
        updateNumericInput(session, "polyn", value = bookmarkedstate()$polyn) 
        updateSelectInput(session, "port_dat_dist", selected = bookmarkedstate()$port_dat_dist) 
        updateSelectInput(session, "port_end", selected = bookmarkedstate()$port_end) 
        updateSelectInput(session, "port_start", selected = bookmarkedstate()$port_start) 
        updateSelectInput(session, "price", selected = bookmarkedstate()$price)
        updateSelectInput(session, "priceBase", selected = bookmarkedstate()$priceBase) 
        updateTextInput(session, 'projectname', value = bookmarkedstate()$projectname)
        updateSelectInput(session, "p2fun", selected = bookmarkedstate()$p2fun) 
        updateSelectInput(session, "p3fun", selected = bookmarkedstate()$p3fun) 
        updateNumericInput(session, "quant_cat", value = bookmarkedstate()$quant_cat) 
        updateNumericInput(session, "relTolX", value = bookmarkedstate()$relTolX) 
        updateNumericInput(session, "reportfreq", value = bookmarkedstate()$reportfreq) 
        updateSelectInput(session, "sp_col", selected = bookmarkedstate()$sp_col) 
        updateSelectInput(session, "start", selected = bookmarkedstate()$start) 
        updateSelectInput(session, "start_latlon", selected = bookmarkedstate()$start_latlon) 
        updateSelectInput(session, "starting_haul", selected = bookmarkedstate()$starting_haul) 
        updateSelectInput(session, "starting_port", selected = bookmarkedstate()$starting_port) 
        updateSelectInput(session, "starting_port_SL", selected = bookmarkedstate()$starting_port_SL) 
        updateSelectInput(session, "startloc", selected = bookmarkedstate()$startloc) 
        updateTextInput(session, "target", value = bookmarkedstate()$target) 
        updateNumericInput(session, "temp_lag", value = bookmarkedstate()$temp_lag) 
        updateNumericInput(session, "temp_window", value = bookmarkedstate()$temp_window) 
        updateSelectInput(session, "temporal", selected = bookmarkedstate()$temporal) 
        updateNumericInput(session, "temp_year", value = bookmarkedstate()$temp_year) 
        updateSelectInput(session, "temp_var", selected = bookmarkedstate()$temp_var) 
        updateSelectInput(session, "TimeVar", selected = bookmarkedstate()$TimeVar) 
        updateSelectInput(session, "trans", selected = bookmarkedstate()$trans)
        updateSelectInput(session, "trans_var_name", selected = bookmarkedstate()$trans_var_name) 
        updateSelectInput(session, "trip", selected = bookmarkedstate()$trip) 
        updateSelectInput(session, "trip_cent_id", selected = bookmarkedstate()$trip_cent_id) 
        updateSelectInput(session, "trip_cent_lat", selected = bookmarkedstate()$trip_cent_lat) 
        updateSelectInput(session, "trip_cent_lon", selected = bookmarkedstate()$trip_cent_lon) 
        updateSelectInput(session, "trip_cent_weight", selected = bookmarkedstate()$trip_cent_weight) 
        updateSelectInput(session, "trip_ID", selected = bookmarkedstate()$trip_ID) 
        updateSelectInput(session, "trip_id_SL", selected = bookmarkedstate()$trip_id_SL) 
        updateCheckboxInput(session, "use_geartype", value = bookmarkedstate()$use_geartype ) 
        updateSelectInput(session, "units", selected = bookmarkedstate()$units) 
        updateCheckboxInput(session, "Unique_Filter", value = bookmarkedstate()$Unique_Filter) 
        updateSelectInput(session, "unique_identifier", selected = bookmarkedstate()$unique_identifier) 
        updateCheckboxInput(session, "use_location", value = bookmarkedstate()$use_location) 
        updateSelectInput(session, "var_x", selected = bookmarkedstate()$var_x) 
        updateSelectInput(session, "var_y", selected = bookmarkedstate()$var_y) 
        updateSelectInput(session, "VarCreateTop", selected = bookmarkedstate()$VarCreateTop) 
        updateSelectInput(session, "weight_var_ac", selected = bookmarkedstate()$weight_var_ac) 
        updateSelectInput(session, "xTime", selected = bookmarkedstate()$xTime) 
        updateSelectInput(session, 'xWeight', selected = bookmarkedstate()$xWeight)
        updateSelectInput(session, "x_dist", selected = bookmarkedstate()$x_dist) 
        }
#----
        })
      ###----
      
    }
            
