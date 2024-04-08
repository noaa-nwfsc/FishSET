source("fleetServ.R", local = TRUE)
source("fleetUI.R", local = TRUE)
source("fleet_helpers.R", local = TRUE)
source("map_viewer_app.R", local = TRUE)
source("zone_outsample_UI.R", local = TRUE)
source("zone_outsample_server.R", local = TRUE)
source("zone_closure_UI.R", local = TRUE)
source("zone_closure_Server.R", local = TRUE)

# default global search value
if(!exists("default_search")) {default_search <- ""}

# default column search values
if (!exists("default_search_columns")) {default_search_columns <- NULL}

# check for FishSET folder 
fs_exist <- exists("folderpath", where = ".GlobalEnv")

### SERVER SIDE    
server = function(input, output, session) {
  options(shiny.maxRequestSize = 8000*1024^2)
  
  #Disable buttons
  toggle_inputs <- function(input_list, enable_inputs = TRUE){
    # Toggle elements
    for(x in names(input_list))
      if(enable_inputs){
        shinyjs::enable(x)} else {
          shinyjs::disable(x) }
  }
  
  
  #---
  #Inline scripting ----
  #---
  r <- reactiveValues(done = 0, ok = TRUE, output = "")
  
  observeEvent(input$runUp, {
    shinyjs::hide("error")
    r$ok <- FALSE
    tryCatch(
      {
        r$output <- isolate(
          paste(utils::capture.output(eval(parse(text = input$exprUp))), collapse = '\n')
        )
        r$ok <- TRUE
      },
      error = function(err) {r$output <- err$message}
    )
    r$done <- r$done + 1
  })
  output$resultUp <- renderUI({
    if(r$done > 0 ) { 
      content <- paste(paste(">", isolate(input$expr)), r$output, sep = '\n')
      if(r$ok) {
        pre(content)
      } else {
        pre( style = "color: red; font-weight: bold;", content)
      }
    }
  })
  
  
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
  #---
  
  ##Pull data functions ----
  ##---
  values <- reactiveValues(
    dataset = data.frame('var1'=0, 'var2'=0)
  )
  # project name 
  project <- reactiveValues()
  
  # refresh data   
  observeEvent(c(input$refresh, input$refresh1, input$refresh2, input$refreshNew), {
    if(!is.null(project$name)){
      tmp_tabs <- tables_database(project$name)[grep(paste0(project$name, 'MainDataTable\\d+'), tables_database(project$name))]
      tab_dates1 <- unlist(stringi::stri_extract_all_regex(tmp_tabs, "\\d{6,}")) # all dates following MainDataTable
      tab_dates2 <- max(tab_dates1) # max date
      tmp_tabs <- tmp_tabs[which(tab_dates1 == tab_dates2)] # get the latest table

      ref_err <- FALSE
      tryCatch(
        values$dataset <- table_view(tmp_tabs, project$name),
        error = function(e) {ref_err <<- TRUE}
      )
      
      if(ref_err){
        showNotification("Error refreshing data", type='error', duration=10)
      } else {
        showNotification("Data refreshed", type='message', duration=10)  
      }
    }
    
  }, ignoreInit = TRUE, ignoreNULL=TRUE) 
  
  #Track Times tab selected
  # vars<-reactiveValues()
  # vars = reactiveValues(counter = 0)
  # observe({
  #   input$tabs
  #   if(input$tabs == 'upload'){
  #     isolate({
  #       vars$counter <- vars$counter + 1
  #     })
  #   }
  # })
  
  
  # ---
  # INFORMATION ----
  # ---
  output$AcrossTabsText <- renderUI({
    if(input$QuickStartChoices=='AcrossTabs'){
      tags$div(
        tags$br(), tags$br(),
        tags$p('All tabs have the following elements:',
               tags$ul(
                 tags$li('Buttons that enable you to close the app and refresh the data.', 
                         tags$ul('Refreshing the data pulls the original SQL data table loaded into the FishSET database. 
                            Instead of refreshing to the original state, you can refresh the 
                            data to an intermediate state by reloading a data table on the', tags$code('Upload Data'), 'tab. 
                            Select the desired data table in the', tags$code('Choose a table'), 'box.')
                 ),
                 tags$li('Buttons that allow you to save plots and tables to the', tags$em('output folder'), 'in the FishSET folder.'), 
                 tags$li("A", tags$code('notes'), "section and a button to save notes to the", tags$em('output folder'), "in the FishSET folder.")
                 ,
                 tags$li('An', tags$code('R expression'), 'area where you can enter and run R code. 
                        Within the FishSET Shiny application, the primary data frame is called', tags$em('values$dataset.')), 
                 tags$ul(tags$ul('Some examples:'), 
                         tags$ul(tags$code('mean(values$dataset[,5])'), 'displays the mean of the fifth column.'),
                         tags$ul(tags$code('summary(values$dataset$Vessel_Length)'), 'displays summary details of a column called Vessel_Length.')
                 )
               )),
        tags$br(), tags$br(),
        tags$div(style="display: inline-block; align:left; ", img(src="QuickStart1.png",  height="75%", width="75%"))
      )
    }
  })
  
  output$UploadTabsText <- renderUI({
    if(input$QuickStartChoices=='UploadTab'){ 
      tags$div(
        tags$br(), tags$br(),
        tags$p(tags$strong('Purpose:'), tags$br(), 'The', tags$em('Upload Data'), 
               'tab is used to load data (primary, port, map, gridded, auxiliary) from the FishSET database or 
                        from a local file location, and to manage project tables, confidentiality settings,',
               'and viewing and rerunning logged function calls.'), 
        tags$p("To get started, first write or select a project name in the", tags$code('Name of project'), "text box.",
               tags$ul("The project name is a user-created unique identifier for all data tables  and outputs (plots, model results, etc.) 
                 associated with the analysis. Example project names are 'pollock2019' and 'AKGOA'.")
        ),
        tags$p('Next, load data.',
               tags$ul('To load from a local file location, select the', tags$code('Upload new file'), 'radio button and then browse to file location.'), 
               tags$ul('To load from the FishSET database, select the', tags$code('FishSET database'), 'radio button. Select the 
	                  desired data table.')
        ), 
        tags$p('Finally, press the', tags$code('Load data'), 'button.'),
        # tags$div(style="display: inline-block; align:center", img(src="upload.png",  height="75%", width="75%"))
        tags$br(), tags$br(),
        tags$p('Once data is loaded, confidentiality rules can be defined. Two confidentiality rules can be applied to summarized data,
		    	       the rule of n', tags$em('(n)'),'and the majority allocation rule', tags$em('(k).'),
               'The', tags$em('(rule of n)'),'specifies the minimum number of unique observational units (such as vessels)
			        required for summarized data to be displayed. The', tags$em('majority allocation rule'), 
			        'checks that no single observation units accounts for more than', tags$em('k%'), 'of the summarized valued.'),
        tags$br(), tags$br(),
        tags$p(tags$strong("Data:"),
               tags$ul('primary (required)'), 
               tags$ul('port (optional)'),
               tags$ul('spatial (required)'), 
               tags$ul('auxiliary (optional)'), 
               tags$ul('gridded (optional)'), 
               tags$br(),
               "The", tags$strong('primary data'), "contains the core data used in models. The data file must include vectors containing information on 
             ports (e.g., id, name), date (e.g., haul, trip start), catch amount (e.g., metric tons, kg), and fishing location (e.g., latitude/longitude, zone/area). 
			       Additional information such as price, species caught, and vessel characteristics may be included in the primary data file or added later. 
			       Each row of the primary data file should be a unique observation and each column a unique vector. Single or double apostrophes, commas other than as 
             CSV separators, and periods other than as decimal points, should not be included in the file. Use underscores rather than spaces in column names and use NA or leave 
             cells empty to represent no or missing data.",
			       tags$br(),tags$br(),
			       "The", tags$strong('port data'), "file contains the location (lat/lon) of ports in the primary data file and a vector containing port name or ID that links to the 
			       primary data file. Values in the port name vector must exactly match values in the primary data port vector. 
			       Check spelling, capitalization, and spaces if port data is not successfully merged into the primary data frame.
			       Location variables (latitude and longitude) must be in decimal degrees with cardinal direction indicated by sign.",
			       tags$br(), tags$br(),
			       "The", tags$strong('spatial or map data'), "file is an essential file that contains the latitude and longitude points defining fishery zone polygons. 
              The preferred file format is geojson but other formats are accepted. In FishSET, multiple zones with the same ID are combined and treated as a single zone,
              even if spatially separated. FishSET imports your spatial data file as is, even if the zone outlines cover land. 
              The land will not be subtracted out by FishSET.",
			       tags$br(),tags$br(),
			       tags$strong("Auxiliary data"), "files are optional and can contain anything you want to merge with the primary data file within FishSET 
			       (e.g., prices by date, vessel characteristics). Each column should be a unique vector and each row a unique observation. 
            Auxiliary data does not need to be at the haul or trip level but must contain a vector in common with 
			       the primary data file. Auxiliary data files are useful when a set of data is common across multiple primary data files, 
			       or as an efficient way to include data that is not haul- or trip-level specific.",
			       tags$br(),tags$br(),
			       "The", tags$strong("gridded data"), "is an optional file that contains a variable that varies by fishery zone and, optionally, by a second 
              dimension (e.g., date/time). Both dimensions in the gridded data file need to be vectors in the primary data file. 
              The grid locations (zones) must define the columns and the optional second dimension defines the rows. 
              The row values must match the values of the vector in the primary data file that it will be linked to. 
              Examples of gridded data include sea surface temperature, ice cover, and wind speed."
        ),
        tags$br(), tags$br(),
        tags$p(tags$strong("Manage Tables"), tags$br(),
               'The blue “Manage Tables” button at the top of the upload page allows users to view and delete FishSET Database',
               'tables for all projects. When clicked, a popup appears displaying a table containing the table name, project,',
               'and type. Select one or more tables to delete by clicking on the table row and selecting “Next”. A new popup will',
               'appear confirming which tables should be deleted.'),
        tags$br(), tags$br(),
        tags$p(tags$strong("Metadata"), tags$br(),
               'The blue "Metadata" button at the top of the upload page allows users to load, create, save, and edit metadata', 
               'for tables saved to the FishSET database. To manually create metadata, select a project and table from the FishSET database',
               'and select “Load data” on the “Create” tab. A metadata template will be loaded into the main panel based on the table,',
               'including fields for each column. To import raw metadata files, select a file using the “Browse” button. File reader options',
               'will populate at the bottom of the sidebar based on the file type. Select “Load raw meta” to populate the raw metadata',
               'section of the main panel. Select “create meta” to save the metadata to the project file.',
               'To view, edit, or delete metadata, go to the “Edit” tab and select a table. Changes can be made in the main panel.'
        ),
        tags$br(), tags$br(),
        tags$p(tags$strong("Confidentiality"), tags$br(),
               'To automatically check for confidentiality in plots and tables, check “Check for confidentiality”, select a vessel',
               'ID variable, a confidentiality rule, and a threshold value. Confidentiality is not checked by default.'
        ),
        tags$br(), tags$br(),
        tags$p(tags$strong("Reset log"), tags$br(),
               'Whenever a new project is started a log file that stores FishSET function calls is created. A project’s log file can',
               'be reset by going to the “Reset log” menu. This creates a new log file. The previous log is not deleted, with one',
               'exception: resetting a log two or more times in a single day will overwrite that day’s log file.'
        )
      )
    }
  })
  
  output$DQTabsText <- renderUI({
    if(input$QuickStartChoices=='DQTab'){ 
      p(tags$br(), tags$br(),
        tags$strong('Purpose:'), tags$br(), 
        'The', tags$em('Data Quality and Evaluation'), 'tab is used to identify and correct common data quality issues such as erroneous outliers and missing values.',
        tags$br(),tags$br(),
        'Users can view data classes for variables, check for missing values, empty variables, duplicate data, erroneous outliers, and that lat/lon variables 
				    are in the correct format.',tags$br(), 
        'Output from most data evaluation checks appear as a statement of check results at the top of the main panel. 
            For the', tags$em('Variable class'), 'and', tags$em('Outliers'), 'options, interactive tables and plots are displayed.', 
        tags$br(), tags$br(),
        'Options to address data quality issues are specific to each data evaluation check option and are displayed 
				above the', tags$code('R expression'), 'box (arrow 2). Calls to check for and correct data quality issues will be recorded in the log file. 
				Evaluation statements outputs are automatically saved to the log file and notes file.',
				tags$br(), tags$br(),
				'For the variable class option, select the correct variable data class if the wrong class is shown. For instance, a variable containing 
          all numbers should generally be classed as numeric and not character.', 
				tags$br(), 'Click the', tags$code('Change variable class'), 'button when selections are done. 
			    The data will not be changed but the variable data class will.', 
				tags$br(), tags$br(),
				'For the outlier option, a data table and three plots are displayed. The plots show the distribution of the data in three 
				   different ways.', tags$br(), 'You can click on the data table to view the impact of removing a defined subset of the data. Click on individual points 
         in the first plot to view the point value or zoom in to a highlighted area by double clicking. Further details on the outlier 
			    checks are in the Help Manual.',
				tags$div(style="display: inline-block; align:center", img(src="dq.png", height="75%", width="75%")),
				tags$br(), tags$br(),
				'Once all changes have been made, save the revised data to the FishSET database by clicking the', tags$code('Save data to FishSET database'), 'button. 
            The revised data will be saved to the FishSET database with a name based on the project, MainDataTable, and date. Even if the 
				revised data is not saved to the FishSET database, the Shiny application will use the revised data.',
				'To undo changes, click on the', tags$code('Refresh data'), 'button. This action will restore the data to its original, 
				unaltered state. Any previous actions will also be lost.',
				tags$br(), tags$br(),
				tags$p(tags$strong("Spatial checks"), tags$br(),
				       'To perform a spatial data check, upload a spatial file containing regulatory zones then select a latitude, longitude,',
				       'and date column from the main data table. An EPSG code and a grouping variable are optional. Spatial checks determine',
				       'whether points occur on land, outside zone, on zone boundaries, and/or at sea and within zones.',
				       tags$br(), tags$br(),
				       'The spatial corrections tab allows users to change lat/lon signs, remove points by distance from nearest zone, and',
				       'edit individual lat/lon entries (this can be done by double-clicking on a lat/lon cell in the spatial corrections table).'
				)
      )
      
    }
  })
  
  output$FleetText <- renderUI({
    if(input$QuickStartChoices=='FleetTab'){ 
      p(tags$br(), tags$br(),
        tags$strong('Purpose:'), tags$br(), 
        'The', tags$em(' Fleet Assignment and Summary'), 'tab is used to define fleets and explore data at a fleet level.',
        tags$br(), tags$br(),
        'The tab is divided into two subtabs, the', tags$em('Fleet assignment'), 'subtab where fleet definitions are created  
            and vessels assigned to fleets and the', tags$em('Fleet summary'), 'subtab where users can summarize and return fleet 
            information as tables and plots. Data can be treated as a single fleet or as multiple fleets. The', 
        tags$em('Fleet assignment'), 'subtab can be skipped if the entire data table reflects a single fleet or if a fleet 
            identifier variable already exists in the data frame.',
        tags$br(),
        'Working with', tags$em('Fleet assignment'), 'functions is described first.',
        tags$br(),tags$br(),
        tags$strong('Fleet Assignment Subtab'), tags$br(),
        'In this subtab, users first define fleets and then assign vessels to fleets. A fleet definition table must be 
            created or loaded before running the fleet assignment function. Use the', tags$code('Select task'), 
        'menu on the sidebar (arrow 1) to switch between defining and assigning fleets.',
        tags$br(),
        tags$ul(
          tags$strong('Fleet Definition Table'), tags$br(),
          'A fleet definition table consists of a fleet definition column and a fleet name column. Each row of the table 
                is a different fleet. The fleet definition column contains a logical condition that defines a fleet. 
                New fleet definition tables can be created with the', tags$code('Expression Builder'), 'or by entering logical conditions 
                directly into the', tags$em('Fleet Definition Table.'), 'Existing fleet definition tables that are not saved to the FishSET 
                database can be loaded using', tags$code('Import table from local file'), 'browser button at the bottom of the left panel. 
                Click the blue', tags$code('Import table'), 'button to load the table into the main panel of the app.',
          tags$br(),tags$br(),
          tags$ul(tags$strong('Expression builder'), tags$br(),
                  'The', tags$code('Expression builder'), '(located in the top-half of the main panel) is a tool that allows users to 
                create fleet definitions quickly and accurately. It is used to fill in the condition column of the ',
                tags$code('Fleet Definition Table.'), 'The expression builder consists of a Variable, Operator, and Value menu. 
                To begin constructing an expression, first select a', tags$code('variable.'), 'Next, select an', tags$code('operation'), 
                'to apply; these are written out as phrases such as “equal to” and “less than or equal to”. Lastly, 
                select a', tags$code('value.'), 'Value lists the first 15 unique values present in the variable. If you do not 
                see the value you want to include, type the value into the list and select “add”.', 
                tags$br(),
                'If the expression is done, click the green', tags$code('Insert expression'), 'button (arrow 3). 
                Else, you can add additional expressions to make a condition.  To chain multiple expressions together, 
                click the blue', tags$code('Add to expression'), 'button beneath the drop-down menus (arrow 2). This will 
                add a new line to the expression builder with an additional menu on the left side. The new menu contains 
                two logical operators:', tags$em('AND'), 'and', tags$em('OR'), 'which determine whether the proceeding 
                expression will be required (“AND”) or optional (“OR”).',
                tags$br(),
                'As the condition is being created, a text box beneath the builder menu will appear, showing the condition 
                in its current state. This is the condition statement that will be added to the fleet definition table.',
                tags$br(),
                'When finished, select', tags$code('Insert expression'), '(arrow 3) and the condition will be added to the', 
                tags$em('fleet definition table.'), 'A fleet name for the condition must manually entered into the table. 
                Double-clicking the appropriate cell in the fleet column and type in a name. To save your changes you', 
                tags$strong('MUST'), 'enter', tags$code('Ctrl+Enter.'), 'Pressing', tags$code('Esc'), 'or', 
                tags$code('Enter'), 'will exit without saving.',
                tags$br(),
                'To add additional fleet conditions, select', tags$code('Reset expression'), 'to start the steps over.',
                tags$br(), tags$br(),
                tags$strong('Defining Conditions'), tags$br(),
                'Instead of using the', tags$code('Expression Builder,'), 'conditions can be written directly into the', 
                tags$code('Fleet Definition Table.'), 'The condition must be a valid logical expression that outputs a 
                TRUE or FALSE value and contains a variable name, an operator, and a value (numeric or string). 
                For example,', tags$code('GEAR_TYPE == 1.'), 'To combine multiple expressions into one fleet definition, 
                add a logical operator (e.g. “&” or  “|”) between the expressions:', 
                tags$code('GEAR_TYPE == 2 & VESSEL_TYPE == ‘CP’.'), tags$br(),
                'Use quotes (single or double) when referencing a categorical variable in an expression. For example,', 
                tags$code('season == “A”'), 'is acceptable but', tags$code('season == A'), 'will produce errors. 
                A reference table of operators is provided below the fleet definition table.', 
                tags$br(), tags$br(),
                tags$strong('Editing the Fleet Definition Table'), tags$br(),
                'Conditions in the', tags$em('fleet definition table'), 'can be edited by inserting a new condition 
                expression into the desired cell. After the expression has been built, click on the select cell button 
                above the fleet table, and then select a cell within a condition column, finally click', tags$code('Insert expression.'),
                tags$br(),
                'Rows and columns can be added to the table by clicking the', tags$code('Add row'), 'and', tags$code('Add column'), 
                'buttons.', tags$br(),
                'To remove a row, click the', tags$code('Select rows'), 'button, click on one or more rows, and then click',
                tags$code('Delete row.'), 'To delete a column, click', tags$code('select columns,'), 'select a column, 
                then click', tags$code('Delete column.'), 
                tags$br(),
                'Columns can be renamed by selecting a column, entering a valid column name into the', 
                tags$code('New column name'), 'text box--either “condition” or “fleet”--then clicking', 
                tags$code('Change column name.'), 'Note: A fleet table may only have two columns: “condition” and “fleet”.', 
                tags$br(),
                'Any unused rows in the fleet table will be removed before being saved--these include cells containing no values 
                and empty strings.', tags$br(),
                'After completing a fleet table, save it to the FishSET database by clicking', 
                tags$code('Save table to FishSET database'), '(arrow 4). Once saved, fleet tables can be reused for future projects.'
          ),#end line indent
          tags$br(),
          tags$strong('Fleet Assign'), tags$br(),
          'To assign vessels to fleets, select', tags$code('Fleet assignment'), 'in the', tags$code('Select Tasks'), 
          'drawdown box in the left hand panel.', 
          tags$br(),
          'Select a fleet table saved in the FishSET database using the', tags$code('Available fleet tables'), 
          'menu (arrow 1). Click the blue', tags$code('Load table'), 'button to load the fleet table into the app.',
          tags$br(),
          'To select a subset of fleet conditions to apply, click on the desired rows of the fleet definition table in the 
            main panel. All highlighted rows will be used. All fleet definitions will be applied if no rows are selected.',
          tags$br(),
          'To allow vessels to have multiple fleet assignments, check', tags$code('Allow overlapping fleet assignments.'),
          'Otherwise, output will not be generated if overlap between observations is detected.',
          tags$br(),
          'Next, select from the', tags$code('Fleet variable type'), 'dropdown box whether fleet assignment should be a 
            single column containing fleet names', tags$em('(String)'), 'or', tags$em('n'), 'binary columns, one for each 
            fleet name', tags$em('(Dummy variable)'), '(arrow 2) .', tags$br(),
          'Click the green', tags$code('Assign fleets'), 'button (arrow 3) to add the fleet assignment variables(s) to 
            the data table.'),
        tags$br(),tags$br(),
        tags$strong('Summary Functions'), tags$br(),
        'The', tags$em('Fleet Summary'), 'subtab contains eight functions that displays summarized fleet data in plots 
            and tables. Use the', tags$code('Select function'), 'menu (arrow 1) to navigate between them.',
        tags$br(),tags$br(),
        tags$ul(tags$em('Vessel Count:'), 'Number of unique vessels by time period and/or grouping variable', tags$br(),
                tags$em('Species Catch:'), 'Summarized catch by time period and/or grouping variable', tags$br(),
                tags$em('Rolling Catch:'), 'A moving window function summarizing catch over time', tags$br(),
                tags$em('Weekly Catch:'), 'Summarizes catch by week', tags$br(),
                tags$em('Bycatch:'), 'Compares species bycatch using CPUE and share of total catch', tags$br(),
                tags$em('Weekly Effort:'), 'Displays average daily CPUE by week', tags$br(),
                tags$em('Trip duration:'), 'Displays the distribution of trip duration', tags$br(),
                tags$em('Density Plot:'), 'Displays the distribution of a variable using a kernel density estimate, 
                    cumulative distribution function, or empirical cumulative distribution function'),
        tags$br(),
        'Populate options and then press the green', tags$code('Run Function'), 'button to run the function and 
            display the chosen output.', tags$br(), 
        'Press the green', tags$code('Save plot to folder'), 'and', 
        tags$code('Save table to folder buttons'), 'to save output to Output folder.',
        tags$br(),tags$br(),
        'There are four optional features that apply to each of these functions (arrow 2).  
            These options are to subset the data, group the data, split plots, and other plot options.',
        tags$br(),tags$br(),
        tags$ul(tags$strong('Subset'),
                'Subset data by a variable value and/or by time (arrow 3).', tags$br(),
                'To subset by a variable value, go to', tags$code('Subset'), 'check', tags$code('Subset by variable,'),
                'then choose a variable to subset by.', tags$br(), 
                'A box will appear below the variable where you can select or type 
                    the value(s) to include in the table/plot.', tags$br(),
                'To subset by date, go to', tags$code('Subset'), 'check', tags$code('Subset by date'),
                'and enter a date variable and a subset type.', tags$br(), 
                'There are two ways to subset by time: using a date range or by period selection.',
                tags$br(),
                tags$em('Date ranges'), 'will only display data within a start and end date. To use this feature, 
                    enter a start and end date either by browsing the pop-up calendar or by typing into the calendar textbox 
                    using YYYY-MM-DD format.', tags$br(),
                tags$em('Period subsetting'), 'allows users to quickly subset their data by specified periods using a slider bar. 
                    Period options include year, month, week, year-month, and year-week.', 
                tags$br(),tags$br(),
                tags$strong('Group'), tags$br(),
                'Aggregate data by one or more categorical variables (arrow 4).', tags$br(),
                'Go to', tags$code('Group,'), 'and click on', tags$code('Group by'), 'to add grouping variables.',
                tags$br(), 'The', tags$code('Group by'), 'menu will include all non-continuous variables 
                    in the data table, plus three variables that can be created by the function: year, month, and week 
                    (date variable required).', tags$br(),
                'For plots, each group variable is assigned to a color or line type. Which aesthetic a grouping variable 
                    is assigned to depends on the function, plot type, and the order in which the grouping variables are added. 
                    Generally, the first grouping variable is represented by color and the second group by line type. 
                    Bar plots will not display a second grouping variable, but the variable will be included in the table output.', 
                tags$br(),
                'Grouping variables can also be combined by checking the', tags$code('Combine group variables'), 
                'box. There is no limit to the number of grouping variables that can be combined, but no more than 
                    three is recommended.',
                tags$br(),tags$br(),
                tags$strong('Split'), tags$br(),
                'Divide a plot by a categorical variable (arrow 5).', tags$br(),
                'Each unique value in the split variable is given its own plot within a grid. 
                    Splitting variables are assigned to either rows or columns in the grid.', tags$br(), 'If a single 
                    splitting variable is selected, the plot is divided by row. If two splitting variables are 
                    selected (the maximum allowed), the first is assigned to rows and second to columns.', 
                tags$br(), 'Splitting by year, month, and week is available if a date variable is provided.', 
                tags$br(), 
                'For catch-related functions, splitting by species is available if multiple catch 
                    columns have been entered.',  
                tags$br(),tags$br(),
                tags$strong('Plot options'),
                'Several parameters are available for adjusting plots, including plot type, scale transformation, 
                    scale positioning, and aesthetic positioning. Options vary by function. Experiment with these to 
                    improve the plot appearance.'
        ) 
      ) #end paragraph
    }
  })
  
  output$ExploreTabsText <- renderUI({
    if(input$QuickStartChoices=='ExplorTab'){ 
      p(tags$br(), tags$br(),
        tags$strong('Purpose:'), tags$br(), 
        'The', tags$em('Data Exploration'), 'tab is used to view and explore the loaded data.',
        tags$br(),tags$br(),
        'Data can be viewed in table or plot format. Plots show the temporal and spatial distribution of the data 
                   and the relationship between two variables.',
        'Use the', tags$code('View data or plots'), 'dropdown box to select between viewing data as a table or as plots.',
        tags$br(),tags$br(),
        "We describe usage of the table option first and then of plots.",
        tags$br(), tags$br(),
        #tags$p(#HTML(paste(tags$h4('Table:'), 
        #                 tags$h5(
        tags$strong('Table:'), tags$br(),
        tags$ul('The table is used to edit individual cells, filter the data, 
                                      and remove columns from the data frame that are redundant or 
                                      will not be used in analyses or modeling. In addition, the 
                                      other data types (plot, auxiliary, gridded), can also be viewed 
                                      and edited using the', tags$code('Select a data file type'), 'dropdown box.',
                tags$br(),
                tags$strong('Edit cells'), 'by double-clicking a cell.',
                tags$br(),
                tags$div(style="display: inline-block; align:center", img(src="Correct.png", height="75%", width="75%")),
                tags$br(),tags$br(),
                tags$strong('Remove columns'), 'by clicking on a column, then clicking the', tags$code('Remove variable'), 'button.', 
                tags$br(), 'Save the edited data table to the FishSET database by clicking the', tags$code('Save data'), 
                'button.', tags$br(), 'Press the', tags$code('Refresh data'), 'button to load the original, unaltered data table.',
                tags$br(), 
                tags$div(style="display: inline-block; align:center", img(src="DeleteVar.png", height="75%", width="75%")),
                tags$br(),tags$br(),
                tags$strong('Filter data'), 'using the', tags$em('boxes'), 'between the columns name and the first row of data. Filters are saved when the', 
                tags$code('Save data to FishSET database'), 'button is pushed. The altered data is also 
            saved as the project, "MainDataTable", and the date.',
            tags$br(), 
            tags$div(style="display: inline-block; align:center", img(src="Filter.png", height="75%", width="75%"))
        ), #End indent
        tags$p(tags$br(),tags$br(),
               tags$h4('Plots:'),
               'Temporal, spatial, and x-y plots are available.'), 
        'The temporal plots show the relationship of the selected variable by date.',
        tags$br(),tags$br(),
        'The spatial plots show the distribution of hauls in the map region and hot spots of activity.',
        tags$br(),
        'You can zoom in on the', tags$em('Observed location'), 
        'spatial plot by double-clicking a highlighted area or view the latitude and longitude of a point
			       by single clicking on the point.', tags$br(), 'To reset the map, double-click outside the highlighted area.', 
        tags$br(), 'This map can be saved.', tags$br(), 'A more detailed spatial map can be viewed on the', tags$code('Map Viewer'),
        'tab.', 
        tags$br(),
        tags$div(style="display: inline-block; align:center", img(src="MapZoom.png", height="75%", width="75%")),
        tags$br(),tags$br(),
        "A measure of spatial autocorrelation (global Moran's I) and of spatial clustering (GetisOrd statistic) will display below the 
            plots once selections below plot type are made. The table can be saved.",
        tags$br(), tags$br(),
        'The final plot type, x-y plots, shows the relationship between two selected variables.', tags$br(),
        'To assess the degree of correlation between variables or the fit of the relationship, visit the', 
        tags$code('Simple Analyses'), 'tab.',
        tags$br(), tags$br(),
        tags$p(tags$strong("Gridded data"), tags$br(),
               'To visualize gridded data, select “Gridded” from the “Select a data file type” menu. Select “Plots”',
               'and choose a longitude, latitude, and value column. Plots can be split by a categorical variable.',
               'Aggregate the data by selecting a column to aggregate by and a function. The option “lat/lon” aggregates by grid cell. ')
      )
    }
  })
  
  output$AnalTabsText <- renderUI({
    if(input$QuickStartChoices=='AnalTab'){ 
      p(tags$br(), tags$br(), 
        tags$strong('Purpose:'), tags$br(),
        'The', tags$em('Simple Analyses'), 'tab is used to view correlation and simple linear regression among selected variables.', 
        tags$br(),tags$br(),
        tags$div(style="display: inline-block; align:center", img(src="Corr.png", height="75%", width="75%")),
        tags$br(), tags$br(),
        'Variables can be removed from the correlation plots and tables by clicking on a variable in the,', tags$code('Select variables'), 'box and 
				then hitting the backspace or delete button on your keyboard.',
				tags$br(),
				'Variables can be added by clicking an empty space in the', tags$code('Select variables'), 'box.',
				tags$br(), tags$br(),
				'Switch to the regression output using the', tags$code('Show correlation or simple linear regression'), 'button. 
				    The response variable is limited to numeric variables. The explanatory variable may be any variable.',
				tags$br(), tags$br(),
				'Plot and table outputs for both correlation and regression analyses can be downloaded.'
      )
    }
  })
  
  output$NewVarsTabsText <- renderUI({
    if(input$QuickStartChoices=='NewVarsTab'){ 
      p(tags$br(), tags$br(),
        tags$strong('Purpose:'), tags$br(),
        'The', tags$em('Compute New Variables'), 'tab is used to parse variables and derive new variables such as CPUE or trip mid-point.',
        tags$br(),tags$br(),
        'Functions are grouped into seven categories:',
        tags$li('Arithmetic'), 
        tags$li('Data transformations'), 
        tags$li('Dummy variables'), 
        tags$li('Nominal ID'), 
        tags$li('Temporal'),
        tags$li('Spatial'), 
        tags$li('Trip-level'),
        tags$br(),tags$br(),
        'We describe first how to run functions, view the generated variable, and save the altered data. We then describe the 
				    functions in the six variable creation categories.',
        tags$br(), tags$br(),
        'To run a function, click the', tags$code('Run function'), 'button (arrow 1). This will run the selected function and
            generate the output.',
        tags$br(),
        'The new variable can then be viewed in the displayed data table (arrow 2). It will be the last 
				    variable in the data table.', tags$br(), 'Default variable names will be provided if the', 
        tags$code('Name of new variable'), 'box is left empty (arrow 3).', 
        tags$br(),
        'Save the revised data by pressing the', tags$code('Save data'), 'button. If the data is not saved the created variables will 
            not be available once the app is closed.',
        tags$br(),tags$br(),
        tags$div(style="display: inline-block; align:center", img(src="NewVar.png", height="75%", width="75%")),
        tags$br(), tags$br(),
        tags$strong('Function category descriptions'), tags$br(),
        tags$em('Arithmetic'), 
        'functions focus on deriving variables based on numeric calculations (i.e., plus, minus) 
				          between two variables.',
        tags$br(), tags$br(),
        tags$em('Data transformations'), 
        'functions focus on transforming data into coded variables. These functions can be used to 
				        mask confidential data.',
        tags$br(), tags$br(),
        tags$em('Dummy variables'),
        'functions focus on creating binary variables.',tags$br(), 
        'These are useful for contrasting between two states, such as caught at least 50 metric tons or not, 
				        before versus after a policy was enacted, or fishery zone was open versus closed.',
        tags$br(), tags$br(),
        tags$em('Nominal ID'), 
        'functions focus on creating identifiers - haul, trip, or fishery season.',
        tags$br(), tags$br(),
        tags$em('Temporal'), 
        'functions focus on converting a date variable into the needed unit of time and calculated duration of time 
				        between two temporal variables.',
        tags$br(), tags$br(),
        tags$em('Spatial'), 
        'functions focus on creating variables that vary over space, such as haul midpoint and distance between points.
				        A spatial data file is required.', tags$br(), 'The function to assign observations to fishery or regulatory zones 
				        is in this function category.',
        tags$br(), tags$br(),
        tags$em('Trip-level'), 
        'functions focus on trip-level variables including trip distance and trip centroid. A function to collapse 
				        data from haul level to trip level is also included.',
        
        tags$br()
      )
    }
  })
  
  output$MapTabsText <- renderUI({
    if(input$QuickStartChoices=='MapTab'){ 
      p(tags$br(), tags$br(),
        tags$strong('Purpose:'), tags$br(), 
        'The', tags$em("Map Viewer"),'tab is used to view the spatial distribution of hauls.',
        tags$br(), tags$br(),
        'The Map Viewer requires a spatial data file containing zone polygons. Go to the', 
        tags$strong('Upload Data'), 'tab to load the file if necessary. The map viewer also requires  
            the zone identifier, which links the primary data frame to the spatial data file. Go to the', 
        tags$strong('Spatial'), 'functions on the', tags$strong('Compute New Variables'), 'tab to 
              assign observations to zones if a zonal identifier does 
            not exist in the primary data table.',
        tags$br(), tags$br(),
        'There are a number of required and suggested choices. Once all choices have been made, 
            press the green', tags$code('Run'), 'button. An interactive map will appear at the top of the page.',
        tags$br(),
        'To display information on individual points in the top right-hand corner of the map, hover over 
              a point. Other informational plots are shown in the left-hand side of the map.',
        tags$br(),
        'This map cannot be saved.',
        tags$br(), tags$br(),
        tags$strong('Choices'),
        tags$ul(
          tags$em('Area variable (data):'), 'Variable in primary data containing zone identifier such as ZoneID',
          tags$br(),
          tags$em('Area variable (map):'), 'Name of the property in the spatial data file that identifies the zones. 
                    Links to Area variable (data).',
          tags$br(),
          tags$em('Numeric variables:'), 'Numeric variables to include in plotting. This variable is required. 
                    Points on the map are color coded based on the numeric variable value. Multiple variables can be chosen 
                    but only one variable will be plotted at a time.',
          tags$br(),
          tags$em('Temporal variables:'), 'Temporal variables to plot the numeric variable against. 
                      Scatter plot is provided in the left-hand side of the map. Multiple temporal variables can be included but 
                      only one will be plotted at a time.',
          tags$br(),
          tags$em('ID variables (recommended):'), 'Categorical variables for grouping in plots. Multiple ID variables 
                    can be included but only one will be plotted at a time.', 
          tags$br(),
          tags$em('Location point or path:'), 'Should the map show hauls as individual points or paths?', 
          tags$br(),
          tags$ul('If points are to be plotted',
                  tags$ul('Longitude point'),
                  tags$ul('Latitude point')
          ),
          tags$ul('If path is to be plotted',
                  tags$ul('Starting longitude'),
                  tags$ul('Starting latitude'),
                  tags$ul('Ending longitude'),
                  tags$ul('Ending latitude')
          )
          
        )
      )
    }
  })
  
  output$ZonalTabsText <- renderUI({
    if(input$QuickStartChoices=='ZonalTab'){ 
      p(tags$br(), tags$br(),
        tags$strong('Purpose:'), tags$br(), 
        'The', tags$em("Define Alternative Fishing Choices"),'tab is used to define alternative fishing choices.',
        tags$br(), tags$br(),
        #$div(style="display: inline-block; align:center", img(src="zonal.png", height="75%", width="75%")),
        #	           tags$li('(Required) Identify fishery zones or management areas, calculate zone or fishing centroids, and assign each observations 
        #                      in the main data table to zones. FishSET defaults to geographic centroids. #
        #					             To use fishing centroids, select a weighting variable in the weighted centroid box. Points that fall outside of 
        #	                    any zones can be assigned to the closest zone by checking the', tags$code('Use closest polygon to point'), 'box. 
        #	                    If spatial data creating polygons are sparse or irregular, the', tags$code('Use convex hull method'),'is recommended. 
        #                    '),
        'The choices on this page are used to develop the matrix of distances 
	                   between observed and alternative fishing choices (where they could have fished but did not). 
                     The distance matrix can be generated from the data or from gridded data. We describe generating the distance matrix 
                     from the data first.',
        tags$br(), tags$br(),
        'If the distance matrix comes from the primary haul-level data, the function requires defining how the 
                     starting location (in longitude and latitude) should be found. Choices are the centroid of the zone where the haul occurred,
                     port, or other lon/lat variable such as haul starting location.', tags$br(), 'Next, define how to find 
                     the location of the alternative fishing locations. Choices are the centroid of 
                     each of the alternative zones or a lon/lat location in the primary data table, such as haul ending 
                     location.',tags$br(), 'The distance matrix is then calculated between defined starting and alternative choice locations. 
                     Distance can be returned in miles, kilometers, or meters.', 
        tags$br(),tags$br(),
        'Alternatively, the distance matrix can be generated from a gridded data table, such as sea surface temperature. 
	                   Columns in the gridded data file must be individual zones.',
        tags$br(),
        'The number of observed hauls can vary considerably between zones. The histogram at the bottom of the page 
	                    is provided to assess variance in the amount of data between hauls. Observations from zones with insufficient zones can be 
	                   removed from analyses by setting the minimum haul number in the', tags$code('Include zones with more hauls than'), 'box.',
        tags$br(),tags$br(),
        'Select catch and price variables. This can be done here or in the', tags$em('Expected Catch/Revenue'), 
        'or', tags$em('Models'), 'tabs.'
      )
    }
  })
  
  output$ExpectedTabsText <- renderUI({
    if(input$QuickStartChoices=='ExpectedTab'){ 
      p(tags$br(), tags$br(),
        tags$strong('Purpose:'), tags$br(), 
        'The', tags$em('Expected Catch/Revenue'), 'tab is used to calculate the expected catch or revenue matrix for alternative
            fishing zones (zones where fishing could have happened but did not). A', tags$code('catch variable'), 
        'is required. Include a', tags$code('price or value'), 'variable if you want  expected revenue. Price is 
			      multiplied against catch to produce revenue. If a revenue variable exists in the data table, it can be used for 
			      the', tags$code('Catch Variable.'), 'This matrix is required to run the conditional logit model.',
        tags$br(), tags$br(),
        
        tags$br(), tags$br(),
        'The function returns four expected catch or expected revenue matrices based on selected parameters:', 
        tags$br(),
        tags$ul('selected temporal parameters'),
        tags$ul('expected catch/revenue based the previous two days (short-term) catch,'), 
        tags$ul('expected catch/revenue based the previous seven days (medium-term) catch,'), 
        tags$ul('and expected catch/revenue based the previous years (long-term) catch.'),
        
        tags$br(),tags$br(),	
        'There are a number of choices. The first choice is whether to calculate expected catch/revenue over the entire data table (“fleet”) 
        or within groups using the', tags$code('Choose variable that defines group'), 'dropdown box.', 
        tags$br(),
        'Next are', tags$em('Temporal options.'), 'First, identify the', tags$code('Method to sort time.'), 'Options are', 
        tags$em('Entire record of time, Sequential,'), 'or', tags$em('Daily.'), 'No additional temporal options are required if', 
        tags$code('Method to sort time'), 'is the', tags$code('Entire record of time'), '(temporal patterns of catch are not taken 
        into account). If using', tags$em('Sequential'), 'or', tags$em('Daily'), 'select the', 
        tags$code('Temporal variable for averaging'), 'and populate choices defining the window size (in days) and time lags 
        (in years and days) for averaging.', 
        tags$br(),tags$br(),
        'Using the specified moving window parameters, a matrix of average catch is created with', tags$em('zone*group'), 'as the rows and date 
        the columns. This is the standard average catch (calc.method = “standardAverage”). Alternatively, you can use the simple lag 
          regression of the mean, which returns the predicted value for each', tags$em('zone*group'),
        'and date given regression coefficients', tags$em('p'), 'for each', tags$em('zone*group.'),
        'The', tags$code('method to average across time steps,'), 'can be to regress over the', 
        tags$code('entire time period'), 'or', tags$em('grouped time periods.'), tags$br(), tags$br(),
        'The expected catch matrix is pulled from the matrix of calculated catches for each date and zone. The matrix is of dimensions',
        tags$em('(number of rows of the primary data)*(number of alternatives).'), 'Expected catch is filled out by mapping the 
        calculated catch for each zone given the observed date (if specified) and group (if specified) in the primary data table.',
        tags$br(), tags$br(),
        'Note that empty catch values are considered to be times of no fishing activity. These values are not included. Values of 0 
        in the catch variable are considered times when fishing activity occurred but with no catch, should these exist. 
        These zero values are included in calculations. Sparsity in data should be considered when deciding how to take into account 
        that catch may vary over time. A sparsity table will appear in the main panel.  If data is sparse, a broader window size 
        or using the entire temporal record may be necessary. Empty catch values and empty expected catch values can be filled but 
        only on a limited basis as doing so can lead to biased or misleading results. If there are a lot of empty values, consider 
        changing the temporal arguments to reduce data sparsity.'
      )
    }
  })
  
  output$ModelTabsText <- renderUI({
    if(input$QuickStartChoices=='ModelTab'){ 
      p(tags$br(),tags$br(),
        tags$strong('Purpose:'), tags$br(), 
        'The', tags$em('Models'), 'tab is used to define model parameters (including the likelihood function), 
            and then run model and compare output.',
        tags$br(),tags$br(),
        'First, we describe how to define model parameters in the', tags$code('Run models'), 'subtab and then we describe 
            the output in the', tags$code('Compare models'), 'subtab.',
        tags$br(), tags$br(),
        tags$strong('Run Models'),
        
        'Click', tags$code('Save model and Add new model'), 'to save selected model choices and define another model 
			      so you can compare multiple models.',
        tags$br(), tags$br(),
        'Defined models are shown in a table at the bottom of the screen.', 
        tags$br(), tags$br(),
        'Once all models have been defined, select', tags$code('Run models'), 'to run all models.',
        tags$br(), tags$br(),
        'View and compare models in the', tags$code('Compare models'), 'subtab.',
        tags$div(style="display: inline-block; align:center", img(src="CompareModels.png", height="75%", width="75%"))
      )
    }
  })
  
  output$BookmarkTabsText <- renderUI({
    if(input$QuickStartChoices=='BookmarkTab'){ 
      p(tags$br(), tags$br(),
        tags$strong('Purpose:'), tags$br(),
        'The', tags$em('Bookmark Choices'), 'tab is used to save choices made in the FishSET R Shiny', 
        'application and enable current application state to be reloaded at a later date.',
        tags$br(), tags$br(),
        'Reloading a bookmarked state will restore the last selections in the application. The data will need to be reloaded 
            and no functions will be applied to the data. It is best to save the data before bookmarking the current application state. 
				    After the application is reloaded, load the saved data.',  
        tags$br(), tags$br(),
        'To bookmark the application, click the', tags$code('bookmark'), 'button. Click', tags$em('Dismiss'), 'in the popup message.',
        tags$div(style="display: inline-block; align:center", img(src="Dismiss.png", height="75%", width="75%")),
        tags$br(), tags$br(),tags$br(),
        'To reload a perviously saved application state, you must have the FishSET R application open. Navigate to the', tags$code('Bookmark Choices'), 
        'tab. Click the', tags$code('Browse'), 'button and then migrate to the', tags$em('input.rds'), 'file.',
        tags$br(), tags$br(),
        tags$div(style="display: inline-block; align:center", img(src="Bookmark2.png", height="75%", width="75%")),
        tags$br(), tags$br(),
        tags$p(tags$strong("Log Rerun"), tags$br(),
               'The Log Rerun tab allows users to selectively run FishSET function calls saved to a project log file.',
               'Selecting a log file will display a table of functions and arguments (it is possible for a project to',
               'have two or more log files, see “Reset Log” in the Upload tab). Click on the table rows containing',
               'function calls to rerun. Functions can be rerun using the original data or by selecting a new dataset',
               'from the sidebar menu after checking “Run log with different data table”. Click “Rerun log” to execute',
               'the selected function calls.') 
      )
    }
  })
  
  
  # ---
  # DATA UPLOAD ----
  # ---
  # Allow users to change FishSET folders easily.
  folderpath <- reactiveVal({
    if (fs_exist) get("folderpath", envir = as.environment(1L))

  })

  # Show path of current FS folder
  output$fish_folder_path <- renderUI({
    if (!is_value_empty(folderpath())) {
      p(tags$strong('Directory is currently set to ', folderpath()), style = "font-size: 18px;")

    } else {
      p('No FishSET Folder found. Please select "Update FishSET Folder".')

    }
  })

  # update FS folder path
  observeEvent(input$change_fs_folder, {

    fs_path <- update_folderpath()
    folderpath(fs_path)
  })

  output$projects <- renderUI({

    req(input$load_main_src)

    if (input$load_main_src == 'Upload new file') {

      textInput('projectname', 'Name of project', placeholder = 'Required to load data')

    } else if (input$load_main_src == 'FishSET database') {


      if(length(suppressWarnings(projects())) > 0) {

        selectInput("project_select", "Select project", choices = projects())

      } else {

        p("No projects found in FishSET Database. Create a project by uploading a new file")
      }
    }
  })

  
  output$load_manage_proj_ui <- renderUI({
      actionButton('load_manage_proj', 'Manage Projects',
                   style = "color: white; background-color: blue;")
  })
  
  # Manage Projects
  
  show_proj_modal <- function() {
    
    showModal(
      modalDialog(title = "Manage Projects",
                  
                  uiOutput('load_proj_modal_ui'),
                  
                  footer = tagList(
                    modalButton("Close"),
                    actionButton("delete_proj", "Delete Project", 
                                 style = "color: white; background-color: red;")
                  ),
                  easyClose = FALSE, size = "l"))
  }
  
  proj_r <- reactiveValues(projects = NULL)
  
  observeEvent(input$load_manage_proj, {
    
    proj_r$projects <- projects()
    
    show_proj_modal()
    
    output$load_proj_modal_ui <- renderUI({
      
      checkboxGroupInput('load_proj_cb', 'Select which projects to delete',
                         choices = proj_r$projects)
    })
    
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(input$delete_proj, {
    
    if (is_value_empty(input$load_proj_cb)) {
      
      showNotification('No projects selected')
      
    } else {
      
      q_test <- quietly_test(erase_project, show_msg = TRUE)
      lapply(input$load_proj_cb, q_test)
      proj_r$projects <- projects()
      removeModal()
    }
    
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  
  # assign project name
  observeEvent(c(input$load_main_src, input$project_select, input$loadDat), {
    
    req(input$load_main_src)
    
    if (input$load_main_src == 'Upload new file') {
      
      project$name <- input$projectname
      
    } else if (input$load_main_src == 'FishSET database') {
      
      project$name <- input$project_select
    }
  }, priority = 1)
  
  
  output$main_upload <- renderUI({    
    
    req(input$load_main_src)
    
    if (input$load_main_src=='Upload new file') {
      
      tagList(
        fluidRow(
          column(5, fileInput("maindat", "Choose primary data file",
                              multiple = FALSE, placeholder = 'Required data')),
          column(width=8, offset=4, 
                 textInput('mainadd', label=div(style = "font-size:14px;  font-weight: 400;", 
                                                'Write additional arguments for reading in data'), 
                           placeholder = "header=FALSE, sep=','"))
          
        ))
      
    } else if (input$load_main_src=='FishSET database') {
      
      if (length(suppressWarnings(projects())) > 0) {
        
        if (isTruthy(project$name)) {
          
          tagList(
            fluidRow(
              column(5,
                     selectInput("main_db_table", "Choose a main table",
                                 choices = main_tables(project$name, show_all = FALSE))
              ))
          )
        }
      }
    }
  })
  
  output$ui.action2 <- renderUI({
    if(is.null(input$maindat)) return()
    tagList(
      textInput('compare', label=div(style = "font-size:14px;  font-weight: 400;", 
                                     'If comparing data to previous year, enter saved table name'), 
                value='', placeholder = 'Saved table name in FishSET database'),
      checkboxInput('over_write','If file exsits, over write?', value=FALSE)
    )
  })
  
  # load counter
  load_r <- reactiveValues(main = 0, port = 0, spat = 0, grid = 0, aux = 0)
  # track project, file, and DB table name
  track_load <- reactiveValues(project = NULL, 
                               main = list(file = NULL, DB = NULL),
                               port = list(file = NULL, DB = NULL), 
                               spat = list(file = NULL, DB = NULL), 
                               grid = list(file = NULL, DB = NULL), 
                               aux = list(file = NULL, DB = NULL))
  
  spat_file <- function(file_type) {
    
    if (file_type == "Upload single file") input$spatialdat
    else if (file_type == "Upload shape files") input$spatialdatshape
  }
  
  # load only if project, file, or DB table change
  load_helper <- function(dat) {
    
    dat_src <- switch(dat, "main" = input$load_main_src, "port" = input$load_port_src,
                      "spat" = input$load_spat_src, "grid" = input$load_grid_src,
                      "aux" = input$load_aux_src)
    dat_file <- switch(dat, "main" = input$maindat, "port" = input$portdat, 
                       "spat" = spat_file(input$filefolder), "grid" = input$griddat,
                       "aux" = input$auxdat)
    db_tab <- switch(dat, "main" = input$main_db_table, "port" = input$port_db_table,
                     "spat" = input$spat_db_table, "grid" = input$grid_db_table,
                     "aux" = input$aux_db_table)
    
    if (dat_src == 'Upload new file') {
      
      if (!is.null(dat_file)) {
        
        if (!is.null(track_load[[dat]]$file)) {
          
          if (track_load[[dat]]$file$datapath == dat_file$datapath) {
            
            if (!is_value_empty(track_load$project)) {
              
              if (project$name == track_load$project) FALSE
              else TRUE
              
            } else TRUE 
            
          } else TRUE
          
        } else TRUE 
        
      } else FALSE
      
    } else if (dat_src == 'FishSET database') {
      
      if (!is.null(db_tab)) {
        
        if (!is.null(track_load[[dat]]$DB)) {
          
          if (track_load[[dat]]$DB == db_tab) FALSE
          else TRUE
          
        } else TRUE
        
      } else FALSE
    }
  }
  
  ## Main ----  
  #Add in reactive values once data  call is is not empty
  observeEvent(input$loadDat, {
    
    if (!isTruthy(project$name)) {
      
      if (input$load_main_src == 'FishSET database') {
        
        showNotification("No project found. Please upload a new file.", 
                         type = 'message', duration = 10)
        
      } else if (input$load_main_src=='Upload new file') {
        
        showNotification("Please enter a project name.", type='message', duration=10)
      }
    }
    
    req(project$name)
    req(load_helper("main"))
    
    if (input$load_main_src=='FishSET database') {
      
      if (table_exists(paste0(project$name, 'MainDataTable'), project$name)==FALSE) {
        
        showNotification('Primary data table not found in FishSET database. Check project spelling.',
                         type='message', duration=15)
      } else {
        
        values$dataset <- table_view(input$main_db_table, project$name)
        
        edit_proj_settings(project$name, 
                           tab_name = input$main_db_table, 
                           tab_type = "main")
        
        track_load$project <- project$name
        track_load$main$DB <- input$main_db_table
        load_r$main <- load_r$main + 1
      }
      
    } else if (input$load_main_src=='Upload new file' & !is.null(input$maindat)) {
      
      if (!is_empty(input$mainadd)) {
        
        values$dataset <- do.call(read_dat, c(list(input$maindat$datapath),
                                              eval(parse(text=paste0("list(",input$mainadd, ")"))) ))
      } else {
        
        values$dataset <- read_dat(input$maindat$datapath) 
      }
      
      df_y <- input$compare
      df_compare <- ifelse(nchar(input$compare)>0, TRUE, FALSE)
      q_test <- quietly_test(load_maindata)
      
      qc_pass <- 
        q_test(values$dataset, over_write = input$over_write, project = project$name, 
               compare = df_compare, y = df_y)
      
      if (qc_pass) {
        
        showNotification("Primary data saved to database.", type = "message", 
                         duration = 10)
        track_load$project <- project$name
        track_load$main$file <- input$maindat
        load_r$main <- load_r$main + 1
        
        edit_proj_settings(project$name, 
                           tab_name = paste0(project$name, "MainDataTable"),
                           tab_type = "main")
        
      } else {
        # reset values$dataset
        values$dataset <- data.frame('var1'=0, 'var2'=0)
      }
      
    } else if (input$load_main_src=='Upload new file' & is.null(input$maindat)) {
      
      showNotification("Select a main file to upload.", type='message', 
                       duration=10)
    }
    
    if (names(values$dataset)[1]!='var1') {
      showNotification("Primary data loaded.", type='message', duration=10)
    }
    
  }, ignoreInit = TRUE, ignoreNULL = TRUE) 
  
  
  ## Port ----
  output$port_upload <- renderUI({     
    
    if(input$load_port_src=='Upload new file'){ 
      tagList(
        fluidRow(
          column(5, fileInput("portdat", "Choose port data file",
                              multiple = FALSE, placeholder = 'Required data'))
        ))
    } else if (input$load_port_src == 'FishSET database') {
      
      if (isTruthy(project$name)) {
        
        tagList(
          fluidRow(
            column(5, selectInput("port_db_table", "Choose a port table",
                                  choices = list_tables(project = project$name, type = "port")))
          ))
      } 
    }
  })
  
  output$ui.actionP2 <- renderUI({
    
    if(is.null(input$portdat)) return()
    tagList(
      column(width=10, offset=3, 
             selectInput('port_name', "Enter column name containing port names", 
                         choices=names(FishSET::read_dat(input$portdat$datapath, if(sub('.*\\.', '', input$portdat$name) == 'shp') { 
                           'shape'} else if(sub('.*\\.', '', input$portdat$name) == 'RData') { 
                             'R'} else { sub('.*\\.', '', input$portdat$name)})), selected="")),
      column(width=10, offset=3,textInput('portadd', div(style = "font-size:14px;  font-weight: 400;", 
                                                         'Additional arguments for reading in data'), placeholder="header=T, sep=';', skip=2"))
      
      # ))#label=div(style = "font-size:14px;  font-weight: 400;", 'Enter column name containing port names'), 
      # value='', placeholder = 'Column name')
    )
  })
  
  ptdat <- reactiveValues(
    dataset = data.frame('var1'=0, 'var2'=0)
  )
  
  ptdat_temp <- reactiveValues(
    dataset = data.frame('var1' = 0, 'var2' = 0)
  )
  
  show <- reactiveValues(port_combine = FALSE, port_merge = FALSE, save = FALSE)
  
  observeEvent(input$loadDat, { 
    
    if (!isTruthy(project$name)) {
      
      showNotification("Please enter a project name.", type = 'message', duration = 10)
    }
    
    req(project$name)
    req(load_helper("port"))
    
    if (input$load_port_src == 'FishSET database') {
      
      if (isTruthy(input$port_db_table)) {
        
        ptdat$dataset <- table_view(input$port_db_table, project$name)
        
        edit_proj_settings(project$name, 
                           tab_name = input$port_db_table, 
                           tab_type = "port")
        
        track_load$port$DB <- input$port_db_table
        load_r$port <- load_r$port + 1
      }
      
    } else if (input$load_port_src == 'Upload new file' & !is.null(input$portdat)) {
      # skip new file upload if user already merged multiple tables
      if (is.null(input$port_combine_save)) {
        
        if (!is_empty(input$portadd)) {
          
          ptdat$dataset <- do.call(read_dat, c(list(input$portdat$datapath),
                                               eval(parse(text=paste0("list(",input$portadd, ")"))) ))
        } else {
          
          ptdat$dataset <- read_dat(input$portdat$datapath)
        }
        
        q_test <- quietly_test(load_port)
        
        qc_pass <- 
          q_test(ptdat$dataset, port_name = input$port_name, over_write = TRUE, 
                 project = project$name, compare = FALSE, y = NULL)
        
        if (qc_pass) {
          
          showNotification("Port data saved to database.", type = "message", 
                           duration = 10)
          track_load$port$file <- input$portdat
          load_r$port <- load_r$port + 1
          
          edit_proj_settings(project$name, 
                             tab_name = paste0(project$name, "PortTable"),
                             tab_type = "port")
          
        } else {
          
          ptdat$dataset <- data.frame('var1' = 0, 'var2' = 0)
        }
      }
    }
    
    if (names(ptdat$dataset)[1] != 'var1') {
      
      showNotification("Port data loaded.", type = 'message', duration = 10)
      show$port_combine <- TRUE
    }
    
  }, ignoreInit = TRUE, ignoreNULL = TRUE) 
  
  # conditional panel for importing additional port tables
  output$portAddTable <- renderUI({
    
    if (show$port_combine) {
      tagList(
        
        checkboxInput("port_combine_cb", "Import and combine additional port table",
                      value = FALSE),
        
        conditionalPanel("input.port_combine_cb",
                         
                         fileInput("port_combine_fi", "Import additional port table"),
                         fluidRow(
                           actionButton("port_combine_load", "Load table"),
                           
                           shinyjs::disabled(
                             actionButton("port_combine_save", "Save combined port table to FishSET Database",
                                          style = "color: white; background-color: blue;")
                           )
                         ))
      )
    }
  })
  
  
  # conditional panel for merging port tables
  output$PortAddtableMerge <- renderUI({
    
    if (show$port_merge) {
      # merge UI module see fleetUI.R)
      mergeUI("port_combine", dat_type = "port")
    }
  })
  # merge server module (see fleetServ.R)
  mergeServer("port_combine", main = ptdat, other = ptdat_temp, 
              reactive(project$name), merge_type = "full", 
              dat_type = "port", show)
  
  # load additional port table
  observeEvent(input$port_combine_load, {
    
    if (!is.null(input$port_combine_fi)) {
      
      ptdat_temp$dataset <- read_dat(input$port_combine_fi$datapath)
      
      show$port_merge <- TRUE 
      show$save <- FALSE
      showNotification("Additional port table loaded", type = "message")
    }
    
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
  # enable save combined port table button
  observe({
    shinyjs::toggleState("port_combine_save", condition = {show$save == TRUE})
  })
  
  
  # save combined port table to FishSET DB
  observeEvent(input$port_combine_save, {
    
    req(project$name)
    
    q_test <- quietly_test(load_port)
    q_test(ptdat$dataset, port_name = "Port_Name", over_write = TRUE, 
           project = project$name, compare = FALSE, y = NULL)
    
    showNotification("Combined port table saved to database.", type = "message", 
                     duration = 10)
    # so column names match with DB version
    ptdat$dataset <- table_view(paste0(project$name, 'PortTable'), project$name)  
    
    edit_proj_settings(project$name, 
                       tab_name = paste0(project$name, 'PortTable'), 
                       tab_type = "port")
    
    show$save <- FALSE
    show$port_merge <- FALSE
    
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
  ## Spatial ----
  output$spatial_upload <- renderUI({     
    
    if (input$load_spat_src == 'Upload new file') {
      
      if (input$filefolder == 'Upload single file') {
        fluidRow(
          column(5, fileInput("spatialdat", "Choose spatial data file",
                              multiple = FALSE, placeholder = 'Suggested data')),
          column(7, offset=4, textInput('spatadd', div(style = "font-size:14px;  font-weight: 400;",
                                                       'Additional arguments for reading in data'),
                                        placeholder="header=T, sep=',', skip=2"))
        )
        
      } else if (input$filefolder == 'Upload shape files') {
        fluidRow(
          column(5, fileInput("spatialdatshape", "Choose spatial data file",
                              accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj", ".cpg"),
                              multiple = TRUE, placeholder = 'Suggested data'))
        )
      }
      
    } else if (input$load_spat_src == 'FishSET database') {
      
      if (isTruthy(project$name)) {
        
        tagList(
          fluidRow(
            column(5, selectInput("spat_db_table", "Choose a spatial table",
                                  choices = list_tables(project$name, "spat")))
          ))   
      }
    }
    
  })
  
  output$spatial_upload2 <- renderUI({
    if (!is.null(input$spatialdat) | !is.null(input$spatialdatshape)) {
        tagList(
          fluidRow(
            column(5, textInput("spatName", "Spatial table name")  )
          )
        )
    }
  })
  
  spatdat <- reactiveValues(
    dataset = data.frame('var1'=0, 'var2'=0)
  )
  
  
  observeEvent(input$loadDat, {
    
    if (!isTruthy(project$name)) {
      
      showNotification("Please enter a project name.", type = 'message', duration = 10)
    }
    
    req(project$name)
    req(load_helper("spat"))
    
    # reset spatial qaqc
    spat_qaqc_r <- reactiveValues(flag = FALSE, c_tab = NULL, remove = FALSE)
    
    if (input$load_spat_src=='FishSET database') {
      
      if (isTruthy(input$spat_db_table)) {
        
        spatdat$dataset <- table_view(input$spat_db_table, project$name)
        track_load$spat$DB <- input$spat_db_table
        load_r$spat <- load_r$spat + 1
        
        edit_proj_settings(project$name, tab_name = input$spat_db_table,
                           tab_type = "spat")
        
        showNotification("Spatial table loaded", type = "message")
      }
      
    } else {
      
      if (!isTruthy(input$spatName)) {
        
        showNotification("Please enter a name for spatial table.", 
                         type = 'message', duration = 10)
      }
      
      req(input$spatName)
      
      if (!is.null(input$spatialdat) | !is.null(input$spatialdatshape)) {
        
        if (input$filefolder == "Upload single file") {
          
          if (!is_empty(input$spatadd)) {
            
            if (sub('.*\\.', '', input$spatialdat$datapath)!='shp') {
              
              spatdat$dataset <- do.call(read_dat, 
                                         c(list(input$spatialdat$datapath, is.map=TRUE), 
                                           eval(parse(text=paste0("list(",input$spatadd, ")")))))
            } else {
              
              showNotification("Spatial file not loaded. Select `Upload shape files` to load shape files.",
                               type='message', duration=10)
            }
            
            track_load$spat$file <- input$spatialdatshape[1,4] 
            
          } else {
            
            if (sub('.*\\.', '', input$spatialdat$datapath)!='shp') {
              
              spatdat$dataset <- read_dat(input$spatialdat$datapath, is.map=TRUE)
              
            } else {
              
              showNotification("Spatial file not loaded. Select `Upload shape files` to load shape files.",
                               type='message', duration=10)
            }
          }
          
          track_load$spat$file <- input$spatialdat
          
        } else if (input$filefolder == "Upload shape files") {
          
          if (length(input$spatialdatshape$name) > 1) {
            
            shpdf <- input$spatialdatshape
            
            if (is.null(shpdf)) {
              
              return()
            }
            # wd should be the folder containing shiny app
            previous_wd <- getwd()
            upload_directory <- dirname(shpdf$datapath[1]) # temp folder containing shapefiles
            setwd(upload_directory)
            
            # rename files so st_read can load properly
            for (i in 1:nrow(shpdf)) {
              
              file.rename(shpdf$datapath[i], shpdf$name[i])
            }
            
            setwd(previous_wd)
            
            spatdat$dataset <- sf::st_read(upload_directory, as_tibble = TRUE)
            spatdat$dataset <- sf::st_transform(spatdat$dataset, crs = 4326) # WGS 84
            track_load$spat$file <- input$spatialdatshape[1,4] 
            
          } else {
            
            showNotification("Shapefiles require, at a minimum, .shp, .shx, and .dbf files.'", 
                             type='message', duration=10)
          }
        }
        
        q_test <- quietly_test(load_spatial)
        pass <- q_test(spatdat$dataset, name = input$spatName, overwrite = TRUE, 
                       project = project$name)
        
        
        if (is.null(pass)) pass <- FALSE
        
        if (pass) {
          
          showNotification("Spatial table loaded and saved to project data folder.", type = "message")
          load_r$spat <- load_r$spat + 1
          edit_proj_settings(project$name, 
                             tab_name = paste0(project$name, input$spatName, "SpatTable"),
                             tab_type = "spat")
          
        } else {
          
          showNotification("Spatial table was not saved to project data folder.", type = "warning")
        }
      }
      
      if (names(spatdat$dataset)[1]!='var1') {
        
        showNotification("Spatial data loaded.", type='message', duration=10)
      }
    } 
    
  }, ignoreInit = TRUE, ignoreNULL = TRUE) 
  
  
  ## Grid ----     
  output$grid_upload <- renderUI({     
    
    if (input$load_grid_src=='Upload new file') {
      
      tagList(
        fluidRow(
          column(5, fileInput("griddat", "Choose data file that varies over two dimensions (gridded)",
                              multiple = FALSE, placeholder = 'Optional data')),
          column(7, offset=4, textInput('gridadd', div(style = "font-size:14px;  font-weight: 400;", 
                                                       'Additional arguments for reading in data'), 
                                        placeholder="header=T, sep=';', skip=2")),
          column(5, 
                 
                 if (!is.null(input$griddat)) {
                   
                   textInput("GridName", "Grid table name")
                 }
          )
        ))
      
    } else if (input$load_grid_src == 'FishSET database') {
      
      if (isTruthy(project$name)) {
        
        tagList(
          fluidRow(
            column(5, selectInput("grid_db_table", "Choose a gridded table",
                                  choices = list_tables(project = project$name, type = "grid")))
          ))
      }
    }
    
  })
  
  
  grddat <- reactiveValues()
  
  observeEvent(input$loadDat, {
    
    req(project$name)
    req(load_helper("grid"))
    
    if (input$load_grid_src == 'FishSET database') {
      
      if (isTruthy(input$grid_db_table)) {
        
        grid_name <-input$grid_db_table
        grddat[[grid_name]] <- table_view(grid_name, project$name)
        track_load$grid$DB <- input$grid_db_table
        load_r$grid <- load_r$grid + 1
        
        edit_proj_settings(project$name, 
                           tab_name = input$grid_db_table,
                           tab_type = "grid")
      }
      
    } else if (input$load_grid_src == 'Upload new file' & !is.null(input$griddat)) {
      
      if (!isTruthy(input$GridName)) {
        
        showNotification("Please enter a name for Gridded table.",
                         type = "warning", duration = 10)
      }
      
      req(input$GridName)
      
      grid_name <- paste0(project$name, input$GridName)
      
      if (!is_empty(input$gridadd)) {
        
        grddat[[grid_name]] <- do.call(read_dat, c(list(input$griddat$datapath), 
                                                   eval(parse(text=paste0("list(",input$gridadd, ")")))))
      } else {
        
        grddat[[grid_name]] <- read_dat(input$griddat$datapath)   
      }
      
      q_test <- quietly_test(load_grid)
      
      qc_pass <- 
        q_test(paste0(project$name, 'MainDataTable'), grid = grddat[[grid_name]], 
               name = input$GridName, over_write = TRUE, project = project$name)
      
      if (qc_pass) {
        
        showNotification('Gridded data saved to database.', type = 'message', duration = 10)
        track_load$grid$file <- input$griddat
        load_r$grid <- load_r$grid + 1
        
        edit_proj_settings(project$name, 
                           tab_name = paste0(project$name, input$GridName, "GridTable"),
                           tab_type = "grid")
        
      } else {
        
        grddat[[grid_name]] <- NULL
        # showNotification('Gridded data was not saved to database.', type = 'warning', duration = 10)
      }
    }
    
    if (length(names(grddat)) > 0) {
      
      showNotification("Gridded data loaded.", type = 'message', duration = 10)
    }
    
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
  output$gridded_uploaded <- renderUI({
    
    if (length(names(grddat)) > 0) {
      
      tagList(
        p(strong("Gridded data tables uploaded:")),
        renderText(paste(names(grddat), collapse = ", "))
      )
    }
  })
  
  ## Auxiliary ----     
  
  output$aux_upload <- renderUI({     
    
    if(input$load_aux_src=='Upload new file') { 
      tagList(
        fluidRow(
          column(5, fileInput("auxdat", "Choose auxiliary data file that links to primary data",
                              multiple = FALSE, placeholder = 'Optional data')),
          column(7, offset=4, textInput('auxadd', div(style = "font-size:14px;  font-weight: 400;", 
                                                      'Additional arguments for reading in data'), 
                                        placeholder="c(header=T, sep=';', skip=2)")),
          column(5, uiOutput('auxNameUI'))
        ))
      
    } else if (input$load_aux_src == 'FishSET database') { 
      
      if (isTruthy(project$name)) {
        
        tagList(
          fluidRow(
            column(5, selectInput("aux_db_table", "Choose a auxiliary table",
                                  choices = list_tables(project = project$name, type = "aux")))))
      }
    }
  })
  
  output$auxNameUI <- renderUI({
    
    if (!is.null(input$auxdat)) {
      
      textInput("AuxName", "Auxiliary table name:" )
    }
  })
  
  aux <- reactiveValues(
    dataset = data.frame('var1'=0, 'var2'=0)
  )
  
  observeEvent(input$loadDat, {
    
    req(project$name)
    req(load_helper("aux"))
    
    if (input$load_aux_src=='FishSET database') {
      
      if (isTruthy(input$aux_db_table)) {
        
        aux$dataset <- table_view(input$aux_db_table, project$name)
        
        edit_proj_settings(project$name, 
                           tab_name = input$aux_db_table, 
                           tab_type = "aux")
        
        track_load$aux$DB <- input$aux_db_table
        load_r$aux <- load_r$aux + 1
      }
      
    } else if (input$load_aux_src=='Upload new file' & !is.null(input$auxdat)) {
      
      if (isTruthy(input$AuxName)) {
        
        if (!is_empty(input$auxadd)) {
          
          aux$dataset <- do.call(read_dat, c(list(input$auxdat$datapath), 
                                             eval(parse(text=paste0("list(",input$auxadd, ")")))))
        } else {
          
          aux$dataset <- read_dat(input$auxdat$datapath)
        }
        
        q_test <- quietly_test(load_aux)
        
        qc_pass <- 
          q_test(paste0(project$name, 'MainDataTable'), aux=aux$dataset, name = input$AuxName, 
                 over_write = TRUE, project = project$name)
        
        if (qc_pass) {
          
          showNotification('Auxiliary data saved to FishSET database.', type = 'message', duration = 10)
          track_load$aux$file <- input$auxdat
          load_r$aux <- load_r$aux + 1
          
          edit_proj_settings(project$name, 
                             tab_name = paste0(project$name, input$AuxName, "AuxTable"),
                             tab_type = "main")
          
        } else {
          
          aux$dataset <- data.frame('var1' = 0, 'var2' = 0)
        }
        
      } else {
        
        showNotification("Please enter name for Auxiliary table.", 
                         type = "warning", duration = 10)
      }
    } 
    
    if (names(aux$dataset)[1]!='var1') {
      
      showNotification("Auxiliary data loaded.", type='message', duration=10)
    }
    
  }, ignoreInit = TRUE, ignoreNULL = TRUE) 
  
  ###---
  ## Merge ----
  ###---      
  # Merge aux with main ---
  mergeServer("aux", values, aux, reactive(project$name), 
              merge_type = "left", dat_type = "aux")
  
  
  ## delete DB tables ----
  dbTab <- reactiveValues(tabs = NULL)
  
  show_delete_modal <- function() {
    
    showModal(
      modalDialog(title = "Manage Database Tables",
                  
                  h5("Click on rows to select tables."),
                  DT::DTOutput("dbTables"),
                  
                  footer = tagList(
                    modalButton("Close"),
                    actionButton("delete_tab_mod", "Next", 
                                 style = "color: white; background-color: blue;")
                  ),
                  easyClose = FALSE, size = "l"))
  }
  
  observeEvent(input$delete_tabs_bttn, {
    
    if (length(suppressWarnings(projects())) == 0) {
      
      showNotification("No project tables found.", type = "message")
      
    } else {
      
      dbTab$tabs <- fishset_tables()
      
      show_delete_modal()
      
      output$dbTables <- DT::renderDT(dbTab$tabs,
                                      filter = "top", style = 'bootstrap', 
                                      class = 'table-bordered table-condensed table-hover table-striped')
    }
  })
  
  observeEvent(input$delete_tab_mod, {
    
    showModal(
      modalDialog(title = "Delete these tables?",
                  
                  tagList(
                    uiOutput("delete_warn_msg"),
                    
                    shinycssloaders::withSpinner(
                      DT::DTOutput("dbTables_confirm"),
                      type = 6
                    )),
                  
                  footer = tagList(
                    actionButton("cancel_delete_tab", "Cancel"),
                    actionButton("confirm_delete_tab", "Delete", 
                                 style = "color: #fff; background-color: red; border-color:#000000;")
                  ),
                  easyClose = FALSE, size = "m"))
    
    warn_ind <- which(grepl("final|raw", dbTab$tabs$type))
    warn_colors <- ifelse(input$dbTables_rows_selected %in% warn_ind, "#FFC20A", "white") # "#E66100" (orange)
    
    output$dbTables_confirm <- DT::renderDT({
      
      if (length(input$dbTables_rows_selected) > 0) {
        
        DT::formatStyle(
          DT::datatable(dbTab$tabs[input$dbTables_rows_selected, ]),
          "table", target = "row", 
          backgroundColor = DT::styleEqual(dbTab$tabs$table[input$dbTables_rows_selected], warn_colors))
        
      } else {
        
        data.frame(table = "No tables selected")
      } 
    })
    
    output$delete_warn_msg <- renderUI({
      
      if (any(warn_ind %in% input$dbTables_rows_selected)) {
        
        div(style = "background-color: #FFC20A;", 
            h4("Warning: final and/or raw tables selected."))
      }
    })
  })
  
  observeEvent(input$confirm_delete_tab, {
    
    tabs_to_delete <- dbTab$tabs$table[input$dbTables_rows_selected]
    tab_proj <- as.character(dbTab$tabs$project[input$dbTables_rows_selected])
    
    lapply(seq_along(tabs_to_delete), function(i) {
      
      table_remove(tabs_to_delete[i], project = tab_proj[i])
    })
    
    dbTab$tabs <- fishset_tables()
    showNotification("Table(s) deleted.")
    show_delete_modal()
  })
  
  observeEvent(input$cancel_delete_tab, show_delete_modal())
  
  ## confidentiality check ----
  
  confid_vals <- reactiveValues(check = FALSE, v_id = NULL, rule = "n", value = 3)
  
  observeEvent(project$name, {
    
    req(isTruthy(project$name))
    
    # Should only run if data is loaded (load_helper)?
    
    conf_rv$current_len <- length(get_confid_cache(project$name))
    conf_rv$last_len <- conf_rv$current_len
    
    conf <- get_confid_check(project$name)
    confid_vals$check <- conf$check
    confid_vals$v_id <- conf$v_id
    confid_vals$rule <- conf$rule
    confid_vals$value <- conf$value
    
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  
  observeEvent(input$confid_modal, {
    
    showModal(
      modalDialog(title = "Check Confidentiality",
                  checkboxInput("confid_check", "Check for confidentiality",
                                value = confid_vals$check),
                  
                  conditionalPanel("input.confid_check", 
                                   selectInput("confid_vid", "Select vessel identifier variable",
                                               choices = names(values$dataset),
                                               selected = confid_vals$v_id),
                                   selectInput("confid_rule", "Select rule", choices = c("n", "k"),
                                               selected = confid_vals$rule),
                                   conditionalPanel("input.confid_rule=='n'",
                                                    numericInput("confid_value_n", "Threshold", value = confid_vals$value,
                                                                 min = 0, max = 100)),
                                   conditionalPanel("input.confid_rule=='k'",
                                                    numericInput("confid_value_k", "Threshold", value = confid_vals$value,
                                                                 min = 0, max = 100, step = 5))
                  ),
                  
                  footer = tagList(
                    modalButton("Close"),
                    actionButton("save_confid", "Save", 
                                 style = "color: #fff; background-color: #6EC479; border-color:#000000;")
                  ),
                  easyClose = TRUE
      )
    )
  }, ignoreInit = TRUE)
  
  observeEvent(input$save_confid, {
    
    c_val <- ifelse(input$confid_rule == "n", input$confid_value_n, input$confid_value_k)
    
    pass_check <-
      set_confid_check(project$name, 
                       check = input$confid_check, 
                       v_id = input$confid_vid,
                       rule = input$confid_rule, 
                       value = c_val)
    
    if (pass_check) {
      
      showNotification("Confidentiality settings saved.", type = "message")
      confid_vals$check <- input$confid_check
      confid_vals$v_id <- input$confid_vid
      confid_vals$rule <- input$confid_rule
      confid_vals$value <- c_val
      
    } else {
      
      showNotification("Confidentiality settings not saved. Invalid threshold value.", type = "warning")
    }
    
    removeModal()
  }, ignoreInit=FALSE)
  
  ## reset log ----
  
  log_overwrite <- reactiveVal(NULL)
  
  observeEvent(input$reset_modal, {
    
    showModal(
      modalDialog(title = "Reset Log",
                  
                  uiOutput("overwriteUI"),
                  DT::DTOutput("logreset_table"),
                  
                  footer = tagList(
                    modalButton("Close"),
                    actionButton("reset_log", "Reset log", 
                                 style = "color: #fff; background-color: #6EC479; border-color:#000000;")),
                  easyClose = TRUE))
    
    last_log <- current_log(project$name)
    today_log <- paste0(project$name, "_", Sys.Date(), ".json")
    log_overwrite(last_log == today_log)
    
    output$overwriteUI <- renderUI({
      
      if (log_overwrite()) {
        
        checkboxInput("log_overwrite", paste("Overwrite", last_log), value = FALSE)
      }
    })
    
    log_tab <- project_logs(project$name, modified = TRUE)
    
    output$logreset_table <- DT::renderDT(log_tab)
  })
  
  observeEvent(input$reset_log, {
    
    if (log_overwrite()) overwrite <- input$log_overwrite
    else overwrite <- FALSE
    
    q_test <- quietly_test(log_reset)
    log_reset_pass <- q_test(project$name, over_write = overwrite)
    
    if (log_reset_pass) {
      
      showNotification(paste0("Log has been reset for project \"", project$name, "\""),
                       type = "message")
      removeModal()
    }
  })
  
  
  # metadata ----
  
  create_meta <- reactiveValues(raw = NULL, raw_html = NULL, par = NULL)
  edit_meta <- reactiveValues(raw = NULL, raw_html = NULL, meta = NULL)
  
  create_cols <- reactiveValues(nms = NULL, nms_fix = NULL, ui = NULL)
  edit_cols <- reactiveValues(nms = NULL, nms_fix = NULL, ui = NULL)
  
  meta_modal <- function() {
    
    showModal(
      modalDialog(title = "Create and Edit Metadata",
                  
                  bslib::page_fluid(
                    
                    actionButton("meta_close", "Close",
                                 style = "color: #fff; background-color: #FF6347; border-color: #800000;"),
                   # tags$br(), tags$br(),
                    bslib::navset_tab(id = "tab", 
                                bslib::nav_panel(  "Create", value = "create_tab", 
                                         bslib::page_sidebar(
                                           sidebar = bslib::sidebar( width = 550,
                                                        h4(strong("Create metadata")),
                                                        
                                                        p("Metadata can be created by loading a data table and",
                                                          "typing into the text boxes in the main panel.",
                                                          "Import a \"raw\" metadata file (e.g. pre-exsting metadata",
                                                          "located in a .xml or .csv file) by selecting \"Download",
                                                          "metadata file\" and clicking \"Load raw meta\".",
                                                          "See \"parse_meta\" in the Help Manual for instructions",
                                                          "on extracting metadata from a data file."),
                                                        
                                                        FishSET:::metaProjUI("meta_create"),
                                                        FishSET:::metaCreateSaveUI("meta_create"),
                                                        
                                                        tags$hr(style = "border-top: 3px solid #bbb;"),
                                                        
                                                        FishSET:::metaRawUI("meta_create")
                                           ),
                                                     FishSET:::metaOut("meta_create"),
                                                     FishSET:::metaRawOut("meta_create"))
                                ),
                                
                               bslib::nav_panel(  "Edit", value = "edit_tab",      
                                         bslib::page_sidebar(
                                           sidebar = bslib::sidebar( width = 550,
                                                        h4(strong("View, edit, and delete metadata")),
                                                        
                                                        p("To edit existing metadata, select a project and table", 
                                                          "and click \"Load meta\". Click \"Save meta\" after", 
                                                          "changes are added. To delete metadata, select a table",
                                                          "and click \"Delete meta\". Select \"Delete\" in the popup", 
                                                          " to confirm. "),
                                                        
                                                        FishSET:::metaProjUI("meta_edit"),
                                                        FishSET:::metaEditSaveUI("meta_edit"),
                                                        FishSET:::metaDeleteUI("meta_edit")
                                           ),
                                                     FishSET:::metaOut("meta_edit"),
                                                     FishSET:::metaRawOut("meta_edit"))
                                ) 
                    ) 
                  ), 
                  
                  footer = modalButton("Close"),
                  size = "xl",
                  easyClose = FALSE)
    )
  }
  
  # meta servers
  FishSET:::metaCreateServ("meta_create", create_cols, create_meta)
  FishSET:::metaEditServ("meta_edit", edit_cols, edit_meta)
  
  observeEvent(input$meta_modal, {
    
    meta_modal()
  })
  
  observeEvent(input[["meta_edit-confirm_meta_delete"]], {
    # re-call metadata pop up after delete
    meta_modal()
  })
  
  observeEvent(input$meta_close, removeModal())
  
  
  # Plot settings ----
  
  in_to_px <- function(x) x * 96
  
  r_plot_set_h <- reactive(in_to_px(input$plot_set_h))
  r_plot_set_w <- reactive(in_to_px(input$plot_set_w))
  
  observeEvent(input$plot_set, {
    if(isTruthy(project$name)){
      p_set <- get_proj_settings(project$name)$plot_size
    }
    showModal(
      modalDialog(title = "Plot settings",
                  
                  sliderInput("plot_set_w", "Width (in)", min = 1, max = 20, 
                              value = p_set[1], step = .1),
                  sliderInput("plot_set_h", "Height (in)", min = 1, max = 20, 
                              value = p_set[2], step = .1),
                  plotOutput("plotSetFig", width = "auto", height = "auto"),
                  
                  footer = tagList(
                    modalButton("Close"),
                    actionButton("plot_set_save", "Save", 
                                 style = "color: #fff; background-color: #6EC479; border-color:#000000;")),
                  size = "xl",
                  easyClose = TRUE))
    
    output$plotSetFig <- renderPlot({
      
      hist(rnorm(100))
      
    }, width = r_plot_set_w, height = r_plot_set_h)
  })
  
  
  observeEvent(input$plot_set_save, {
    if(isTruthy(project$name)){
      edit_proj_settings(project$name, 
                         plot_size = c(input$plot_set_w, input$plot_set_h))
      showNotification("Plot size saved.", type = "message")
    }
  })
  
  
  # ---
  # DATA QUALITY ----
  # ---  
  # change variable class ----
  output$change_var_inputs <- renderUI({
    tagList(
      selectInput("change_class_var", "Select variable to change class", choices = names(values$dataset)),
      selectInput("change_class", "Select new class type",
                  choices = c('numeric', 'character', 'factor', 'date')))
  })
  
  change_class_tab <- reactive({
    
    if(colnames(values$dataset)[1] == 'var1') {
      
      return(NULL)
      
    } else if (input$checks == 'Variable class') {
      
      first_class <- function(x) class(x)[1]
      
      int = t(t(vapply(values$dataset, first_class, character(1))))
      
      df = matrix(as.character(1:2), nrow = nrow(int), ncol = 2, byrow = TRUE,
                  dimnames = list(rownames(int), c('class', 'first value')))
      
 
      df[,1] = int
      df[,2] = t(values$dataset[1,])

      return(df)
    } 
  })
  
  output$changetable <- DT::renderDataTable( 
    
    if (colnames(values$dataset)[1] == 'var1') {
      
      return(NULL)
      
    } else if (input$checks=='Variable class') {
      
      change_class_tab()
    }
  )
  
  
  observeEvent(input$rchclass, {
    
    q_test <- quietly_test(change_class, show_msg = TRUE)

    values$dataset <- q_test(dat = values$dataset, project = project$name,
                             x = input$change_class_var, new_class = input$change_class, save = FALSE)

  })
  
  ##Table output
  tableInputSummary <- reactive({
    if(colnames(values$dataset)[1] == 'var1') {
      return(NULL) 
    } else if(input$checks == 'Summary table') {
      int <- values$dataset
      stable <- summary_stats(dat=int, project = project$name) 
      nums <- unlist(lapply(int, is.numeric))
      stable  <- apply(stable[nums], 2, function(x) gsub(".*:","", x))
      rownames(stable)=c('Min', 'Median','Mean', 'Max',"Missing",'Unique Obs.', "No. 0's")
      stable <- as.data.frame(as.matrix(stable))
      stable <- as.data.frame((t(stable)))
      
      qaqc_out_proj$sum_tab <- project$name
      
      return(stable)
    } else {
      NULL
    }
  })
  
  
  output$output_table_summary <- DT::renderDataTable(
    if(colnames(values$dataset)[1] == 'var1') {
      return(NULL)
    } else if(input$checks=='Summary table'){
      tableInputSummary()
    } else {
      NULL
    },
    server = FALSE, rownames=TRUE,
    options = list(autoWidth=FALSE, scrollX=TRUE, responsive=FALSE, pageLength = 25)
  )
  
  
  
  ##Outlier options 
  output$outlier_column <- renderUI({
    conditionalPanel(
      condition="input.checks=='Outliers'",
      selectInput('column_check', 'Choose variable',
                  choices= c('', numeric_cols(values$dataset)), selected='', selectize=TRUE))
  })
  output$outlier_subset_method <- renderUI({
    conditionalPanel(condition="input.checks=='Outliers'",
                     selectInput('outremovemethod', 'Select user or predefined rules', 
                                 choices=c('User-defined rule', 'Pre-defined rules'), 
                                 selected='Pre-defined rules'))
  })
  output$outlier_subset <- renderUI({
    tagList(
      conditionalPanel("input.checks=='Outliers' && input.outremovemethod=='Pre-defined rules'",
                       selectInput('dat.remove', 'Pre-defined method to subset the data', 
                                   choices=c('none', '5_95_quant', '25_75_quant','mean_2SD',
                                             'mean_3SD','median_2SD','median_3SD'),
                                   selected=c('none', '5_95_quant', '25_75_quant','mean_2SD',
                                              'mean_3SD','median_2SD','median_3SD')[input$output_table_outlier_rows_selected])),
      conditionalPanel("input.checks=='Outliers' && input.outremovemethod=='User-defined rule'",
                       numericInput('datremovenum', 'Number of standard deviations from the mean', value=NULL, min=4, max=25, step=1)#[input$output_table_outlier_rows_selected]
      ))
  })
  
  output$outlier_dist <- renderUI({
    conditionalPanel(
      condition="input.checks=='Outliers'",
      selectInput('x_dist', 'Distribution', 
                  choices=c('normal', 'lognormal', 'exponential', 'weibull', 
                            'poisson', 'negative binomial'), selected='normal'))
  })
  
  #Lat/Lon
  output$LatLonDir <- renderUI({
    tagList(
      conditionalPanel(condition="input.checks=='Lat_Lon units'",
                       selectizeInput('LatDirection','Latitudinal variable', 
                                      choices=c('None', find_lat(values$dataset)),
                                      options = list(create = TRUE, placeholder='Select or type variable name')),
                       
                       selectizeInput('LonDirection','Longitudinal variable', 
                                      choices=c('None', find_lon(values$dataset)),
                                      options = list(create = TRUE, placeholder='Select or type variable name')))
    )
  })
  
  output$output_table_latlon <- DT::renderDT(
    if(colnames(values$dataset)[1] == 'var1') {
      return(NULL)
    } else if(input$checks=='Lat_Lon units'){
      table <- head(values$dataset[, find_lonlat(values$dataset)])
    } else {
      NULL
    }, server = TRUE, selection = list(target = 'column'), rownames=FALSE,
    options = list(autoWidth=FALSE, scrollX=TRUE,  responsive=TRUE, pageLength = 7)
  )
  
  ##Check UI
  
  ##Output to main panel
  output$Case <- renderPrint({
    
    if(input$checks == 'Variable class'){
      
      h4("Check and change variable data classes")
      
    }else if(input$checks=='Summary table') {
      
      h4("Summary table of NUMERIC variables in data table.")
      
    } else if (input$checks=='Outliers'){
      
      if (input$dat.remove=='none'){
        
        h4("Table to assess outliers.", strong(input$column_check), " shown.")
        
      } else {
        
        rm_txt <- switch(input$dat.remove, '5_95_quant' = '5th and 95th quantiles',  '25_75_quant' = '25th and 75th quantiles', 
                         'mean_2SD' = 'mean +/- 2SD', 'mean_3SD' = 'mean +/- 3SD', 
                         'median_2SD' = 'median +/- 2SD', 'median_3SD' = 'median +/- 3SD')
        out_tab <- tableInputOutlier()[[1]]
        rm_num <- nrow(values$dataset) - out_tab[rownames(out_tab) == input$dat.remove, 1]
        
        h4("Table to assess outliers.", strong(input$column_check), " shown.", 
           h4("Excluding points that fall outside the", strong(rm_txt), "results in removing", strong(rm_num), "points from the data table."))
      }
    } else if(input$checks=='NAs'){
      #na(values$dataset)
      na_filter(values$dataset, project = project$name, x=qaqc_helper(values$dataset, "NA", "names"), 
                       replace = FALSE, remove = FALSE, rep.value=NA, over_write=FALSE)
    } else if(input$checks=='NaNs'){
      nan_filter(values$dataset, project = project$name, x=qaqc_helper(values$dataset, "NaN", "names"), 
                 replace = FALSE, remove = FALSE, rep.value=NA,  over_write=FALSE)
    } else if(input$checks=='Unique observations'){
      unique_filter(values$dataset, project = project$name, remove=FALSE)
    } else if(input$checks=='Empty variables'){
      empty_vars_filter(values$dataset, project = project$name, remove=FALSE)
      
    } else if(input$checks=='Lat_Lon units'){
      degree(values$dataset, project = project$name, lat=NULL, lon=NULL, latsign=NULL, lonsign=NULL, replace=FALSE)
    } else {
      
      h4("Spatial data checks and corrections")

    } 
  })
  
  case_to_print <- reactiveValues(dataQuality = logical(0),
                                  explore = logical(0),
                                  analysis = logical(0))
  
  observeEvent(c(input$checks, input$column_check, input$dat.remove, input$x_dist,
                 input$plot_table, input$plot_type, input$col_select, input$x_y_select1, input$x_y_select2,
                 input$corr_reg, input$corr_select, input$reg_resp_select, input$reg_exp_select,
                 input$runSpatQAQC), {
                   
                   if(input$tabs=='qaqc'){
                     
                     if(input$checks=='Summary table') {
                       
                       case_to_print$dataQuality <- c(case_to_print$dataQuality, "Summary table of numeric variables viewed.\n")
                       
                     } else if(input$checks=='Outliers') {
                       
                       if(input$dat.remove=='none'){
                         
                         case_to_print$dataQuality <- c(case_to_print$dataQuality, paste0('Table and plots to assess outliers viewed for ', input$column_check, ".\n"))
                         
                       } else {
                         
                         rm_txt <- switch(input$dat.remove, '5_95_quant' = '5th and 95th quantiles', '25_75_quant' = '25th and 75th quantiles', 
                                          'mean_2SD' = 'mean +/- 2SD', 'mean_3SD' = 'mean +/- 3SD', 
                                          'median_2SD' = 'median +/- 2SD', 'median_3SD' = 'median +/- 3SD')
                         out_tab <- tableInputOutlier()[[1]]
                         rm_num <- nrow(values$dataset) - out_tab[rownames(out_tab) == input$dat.remove, 1]
                         
                         out_txt <- paste('Table and plot to assess outliers viewed for', input$column_check, 
                                          'with', rm_num,  'points that fall outside the', rm_txt, "removed.\n")
                         
                         case_to_print$dataQuality <- c(case_to_print$dataQuality, out_txt)
                       }
                     } else if(input$checks=='NAs'){
                       
                       na_names <- qaqc_helper(values$dataset, "NA", "names")
                       na_quantity <- qaqc_helper(values$dataset[na_names], 
                                                  function(x) sum(is.na(x)), "value")
                       
                       if (any(qaqc_helper(values$dataset, "NA"))) {
                         
                         if(input$NA_Filter_all==0&input$NA_Filter_mean==0){ 
                           g <- paste("Occurrence of missing values checked. The",
                                      sub(",([^,]*)$", ", and\\1", paste(na_names, collapse = ", ")),
                                      "variables contain",  sub(",([^,]*)$", ", and\\1", paste(na_quantity, collapse=", ")),
                                      "missing values, respectively.", 
                                      length(unique(unlist(lapply(values$dataset[na_names], function(x) which(is.na(x)==TRUE))))), 
                                      "rows have missing values. Missing values were not removed or replaced.\n")
                         }
                         
                       } else {
                         
                         if(input$NA_Filter_all==0&input$NA_Filter_mean==0){
                           # case_to_print$dataQuality <- c(case_to_print$dataQuality, 
                           g <- paste("Occurrence of missing values checked. No columns in the data table contain missing values.\n")#)
                         } else {
                           if(input$NA_Filter_all>0){
                             #case_to_print$dataQuality <- c(case_to_print$dataQuality, 
                             g<- paste("Occurrence of missing values checked. The", 
                                       sub(",([^,]*)$", ", and\\1", paste(na_names, collapse = ", ")), 
                                       "variables contained", sub(",([^,]*)$", ", and\\1", paste(na_quantity, collapse=", ")), 
                                       "missing values.", length(unique(unlist(lapply(values$dataset[na_names], function(x) which(is.na(x)==TRUE))))), 
                                       "rows containing missing values were removed from the data table.\n")#)
                           } else if(input$NA_Filter_mean>0){
                             #case_to_print$dataQuality <- c(case_to_print$dataQuality, 
                             g <- paste("Occurrence of missing values checked. The",
                                        sub(",([^,]*)$", ", and\\1", paste(qaqc_helper(values$dataset, "NA", "names"), collapse = ", ")),
                                        "variables contained", sub(",([^,]*)$", ", and\\1", paste(apply(values$dataset[,names(which(apply(values$dataset, 2, function(x) anyNA(x))==TRUE))], 2, 
                                                                                                        function(x) length(which(is.na(x)==TRUE))), collapse=", ")), "missing values. Missing values were replaced with the 
                               mean values of", names(which(apply(values$dataset, 2, function(x) anyNA(x))==TRUE)), "respectively.\n")#)
                           }
                         } 
                       }
                     } else if(input$checks=='NaNs'){
                       nan_names <- qaqc_helper(values$dataset, "NaN", "names")
                       nan_quantity <- qaqc_helper(values$dataset[nan_names], 
                                                   function(x) sum(is.nan(x)), "value")
                       if (any(qaqc_helper(values$dataset, "NaN"))) {
                         
                         if(input$NAN_Filter_all==0 & input$NAN_Filter_mean==0){
                           case_to_print$dataQuality <- c(case_to_print$dataQuality, 
                                                          g<-   paste("Occurrence of non-numbers checked. The", sub(",([^,]*)$", ", and\\1", paste(nan_names, collapse = ", ")),
                                                                      "variables contain", 
                                                                      sub(",([^,]*)$", ", and\\1", paste(nan_quantity, collapse=", ")), "non-numbers, respectively.", 
                                                                      length(unique(unlist(lapply(values$dataset[nan_names], function(x) which(is.nan(x)==TRUE))))), 
                                                                      "rows have non-numbers. No action was taken to remove or replace non-numbers.\n")) 
                         }
                       } else {
                         if(input$NAN_Filter_all==0&input$NAN_Filter_mean==0){
                           case_to_print$dataQuality <- c(case_to_print$dataQuality, 
                                                          g <-  paste("Occurrence of non-numbers checked. No columns in the data table contain non-numbers.\n"))
                         } else {
                           if(input$NAN_Filter_all>0){
                             case_to_print$dataQuality <- c(case_to_print$dataQuality, 
                                                            g<-  paste("Occurrence of non-numbers checked. The", sub(",([^,]*)$", ", and\\1", paste(nan_names, collapse = ", ")), 
                                                                       "variables contained", sub(",([^,]*)$", ", and\\1", paste(nan_quantity, collapse=", ")), "non-numbers.", 
                                                                       length(unique(unlist(lapply(values$dataset[nan_names], function(x) which(is.nan(x)==TRUE))))), 
                                                                       "rows containing non-numbers were removed from the data frame.\n"))
                             
                           } else if(input$NAN_Filter_mean>0){
                             case_to_print$dataQuality <- c(case_to_print$dataQuality,
                                                            g <-  paste("Occurrence of non-numbers checked. The", sub(",([^,]*)$", ", and\\1", paste(nan_names, collapse = ", ")), 
                                                                        "variables contained", sub(",([^,]*)$", ", and\\1", paste(nan_quantity, collapse=", ")), "non-numbers.\n"))
                           }
                         } 
                       }
                     } else if(input$checks=='Unique observations'){
                       if(dim(values$dataset)[1] == dim(unique(values$dataset))[1]) {
                         case_to_print$dataQuality <- c(case_to_print$dataQuality, "Each row is a unique choice occurrence.\n")
                       } else {
                         if(input$Unique_Filter==0){
                           case_to_print$dataQuality <- c(case_to_print$dataQuality, "Each row in the data frame is not a unique choice occurrence at haul or trip level. No action taken.\n")
                         } else {
                           case_to_print$dataQuality <- c(case_to_print$dataQuality, "Duplicate choice occurrence at haul or trip level existed in the data frame and have been removed.\n")
                         }
                       }
                     } else if(input$checks=='Empty variables'){
                       
                       if(any(qaqc_helper(values$dataset, function(x) all(is.na(x))))) {
                         
                         empty_names <- qaqc_helper(values$dataset, function(x) all(is.na(x)), "names")
                         
                         if(input$Empty_Filter==0){
                           case_to_print$dataQuality <- c(case_to_print$dataQuality, 
                                                          paste('Occurrence of empty columns was checked and the', empty_names, 
                                                                "column is empty. The column was not removed from the data frame\n"))
                         } else {
                           case_to_print$dataQuality <- c(case_to_print$dataQuality, 
                                                          paste('Occurrence of empty columns was checked and the', empty_names, 
                                                                "was empty and was removed from the data frame.\n"))
                         }
                       } else {
                         case_to_print$dataQuality <- c(case_to_print$dataQuality, "Occurrence of empty columns was checked and not found in the data frame.\n")
                       }
                     } else if(input$checks=='Lat_Lon units'){
                       
                       if(any(qaqc_helper(values$dataset[find_lonlat(values$dataset)], function(x) !is.numeric(x)))){
                         if(input$LatLon_Filter==FALSE){
                           case_to_print$dataQuality <- c(case_to_print$dataQuality, 
                                                          'Latitude and longitude units were checked and are not in decimal degrees.\n')
                         } else {
                           case_to_print$dataQuality <- c(case_to_print$dataQuality, 
                                                          'Latitude or longitude units not in decimal degrees were converted to decimal degrees.\n')
                         }
                       } else {
                         case_to_print$dataQuality <- c(case_to_print$dataQuality, 
                                                        'Latitude and longitude units were checked and are in decimal degrees.\n')
                       }
                     } 
                   } else if(input$tabs=='explore'){
                     if(input$plot_table=='Plots'& input$plot_type=='Temporal'){
                       case_to_print$explore <- c(case_to_print$explore, paste0("Viewed plots of ", input$col_select, 
                                                                                ' against time for raw points, the ', 
                                                                                input$p2fun, ", and the ",  input$p3fun, 
                                                                                ' value.\n'))
                     } else if(input$plot_table=='Plots'& input$plot_type=='Spatial-autocorrelation'){
                       case_to_print$explore <- c(case_to_print$explore, paste0("Viewed spatial distribution of occurrence 
                                                                      points and spatial density of occurrence points.\n
                                                                      Getis-ord and Moran's I statistics provided for ", 
                                                                      input$varofint, ". Default settings for
                                                                     spdep functions are used."))
                     } else if(input$plot_table=='Plots'& input$plot_type=='x-y plot'){
                       case_to_print$explore <- c(case_to_print$explore, paste0("Viewed plotted relationship between ", 
                                                                                input$x_y_select1,  ' and ', input$x_y_select2, '.\n'))
                     } 
                   } else if(input$tabs=='analysis'){
                     if(input$corr_reg=='Correlation'){
                       case_to_print$analysis <- c(case_to_print$analysis, paste0("Viewed correlation matrix for ",  
                                                                                  isolate({sub(",([^,]*)$", ", and\\1",
                                                                                               paste(input$corr_select, collapse = ", "))}), '.\n'))
                     } else if(input$corr_reg=='Regression'){
                       case_to_print$analysis <- c(case_to_print$analysis, paste0('Viewed plot and linear regression test 
                                                                       output for ',input$reg_exp_select, ' on ', 
                                                                       input$reg_resp_select,'.\n')) 
                     } 
                   }
                 }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # Notes ----
  
  notes <- reactiveValues(upload = "Upload data: ",
                          dataQuality = "Data quality evaluation: ",
                          explore = "Data exploration: ",
                          fleet = list(density = "Fleet functions: ", vessel = character(0),
                                       spec = character(0), roll = character(0),
                                       wc = character(0), we = character(0),
                                       by = character(0), trip = character(0),
                                       f_table = character(0), f_assign = character(0)),
                          analysis = "Simple analysis: ",
                          new = "Create new variable: ",
                          altchoice = "Alternative choice: ",
                          ec = "Expected catch/revenue: ",
                          models = "Models: ",
                          book = "Bookmark URL: ")
  
  observeEvent(c(input$callTextDownloadAnal,
                 input$callTextDownloadQAQC,
                 input$callTextDownloadExplore,
                 input$callTextDownloadUp,
                 input$callTextDownloadNew,
                 input[["fleet-callTextDownload"]],
                 input$callTextDownloadAlt,
                 input$callTextDownloadEC,
                 input$callTextDownloadModels,
                 input$callTextDownloadBook), {
                   
                   if (input$tabs == 'upload') {
                     if (!is.null(input$notesUp)) {
                       notes$upload <- c(notes$upload, paste0(input$notesUp, "\n"))
                     }
                   } else if (input$tabs == 'qaqc') {
                     if (!is.null(input$notesQAQC)) {
                       notes$dataQuality <- c(notes$dataQuality, paste0(input$notesQAQC, "\n"))
                     }
                   } else if (input$tabs == 'explore') {
                     if (!is.null(input$notesExplore)) {
                       notes$explore <- c(notes$explore, paste0(input$notesExplore, "\n"))
                     }
                   } else if (input$tabs == 'fleet') {
                     if (!is.null(input[["fleet-notes"]])) {
                       notes$fleet <- c(notes$fleet, paste0(input[["fleet-notes"]], "\n"))
                     }
                   } else if (input$tabs == 'analysis') {
                     if (!is.null(input$notesAnal)) {
                       notes$analysis <- c(notes$analysis, paste0(input$notesAnal, "\n"))
                     }
                   } else if (input$tabs == 'new') {
                     if (!is.null(input$notesNew)) {
                       notes$new <- c(notes$new, paste0(input$notesNew, '\n'))
                     }
                   } else if (input$tabs == 'altc') {
                     if (!is.null(input$notesAltc)) {
                       notes$altchoice <- c(notes$altchoice, paste0(input$notesAltc, "\n"))
                     } 
                   } else if (input$tabs == 'expectedCatch') {
                     if (!is.null(input$notesEC)) {
                       notes$ec <- c(notes$ec, paste0(input$notesEC, "\n"))
                     } 
                   } else if (input$tabs == 'models') {
                     if (!is.null(input$notesModel)) {
                       notes$models <- c(notes$models, paste0(input$notesModel, "\n"))
                     }
                   } else if (input$tabs == 'book') {
                     if (!is.null(input$notesBook)) {
                       notes$book <- c(notes$book, paste0(input$notesBook, '\n'))
                     }
                   }
                 })
  
  
  #---
  #Continue   QAQC   
  
  # output project tracker
  qaqc_out_proj <- reactiveValues(tab_sum = NULL, out_tab = NULL, 
                                  out_plot = NULL, spat = NULL)
  
  
  outlierBoxplot <- reactive({
    if(input$column_check == ''){
      if (colnames(values$dataset)[1] != 'var1') {
        q_test <- quietly_test(outlier_boxplot)
        out <- q_test(values$dataset, project=project$name)
        out
      }
    } else {
      return()
    }
  })
  
  output$outlierbox <- renderPlot(outlierBoxplot())
  
  tableInputOutlier <- reactive({
    
    req(input$column_check %in% names(values$dataset))
    
    if (colnames(values$dataset)[1] != 'var1') {
      
      q_test <- quietly_test(outlier_table)
      tab <- q_test(values$dataset, project=project$name, x=input$column_check, sd_val=input$datremovenum)
      rownames(tab)=tab[,2]
      tab <- tab[,3:10]
      out <- list(tab)
      names(out) <- input$column_check # track column for saving
      
      qaqc_out_proj$out_tab <- project$name
      
      out
    }
    
  })
  
  
  output$missingtable <- DT::renderDT({
    
    missing_sum <- tableInputSummary()[["Missing"]]
    miss_ind <- which(missing_sum != " 0" & !is.na(missing_sum))
    
    if (length(miss_ind) > 0) tableInputSummary()[miss_ind, ]
    
  }, server = TRUE, rownames=TRUE,
  options = list(autoWidth=FALSE, scrollX=TRUE, responsive=FALSE, pageLength = 25)
  )
  
  output$output_table_outlier <- DT::renderDT(
    
    tableInputOutlier()[[1]], server = TRUE, selection='single', rownames=TRUE,
    options = list(autoWidth=FALSE, scrollX=TRUE, responsive=TRUE, pageLength = 7)
  )
  
  ranges1 <- reactiveValues(x = NULL, y = NULL)   
  ranges2 <- reactiveValues(x = NULL, y = NULL)   
  ranges3 <- reactiveValues(x = NULL, y = NULL)
  
  outlierPlot1 <- reactive({
    
    req(input$column_check %in% names(values$dataset))
    
    if (colnames(values$dataset)[1] != 'var1') {
      
      temp <- values$dataset
      temp$val <- 1:nrow(temp)
      col_check <- rlang::sym(input$column_check)
      q_test <- quietly_test(outlier_plot_int)
      dat_sub <- q_test(dat=temp, x=input$column_check, dat_remove=input$dat.remove,
                        x_dist = input$x_dist, sd_val = input$datremovenum, plot_type = 1)
      
      qaqc_out_proj$out_plot <- project$name
      suppressWarnings(
        ggplot2::ggplot() +
          ggplot2::geom_point(data=dat_sub, ggplot2::aes(x=val, y=!!col_check, color = Points),
                              na.rm=TRUE) +
          ggplot2::scale_color_manual(breaks=c('Kept','Removed'),
                                      values=c('blue','red')) +
          ggplot2::coord_cartesian(xlim = ranges1$x, ylim = ranges1$y, expand = FALSE) +
          ggplot2::labs(x='Data row') +
          fishset_theme() +
          ggplot2::theme(axis.text=ggplot2::element_text(size=12),
                         axis.title=ggplot2::element_text(size=12))
      )
    }
  })
  
  outlierPlot2 <- reactive({
    
    req(input$column_check %in% names(values$dataset))
    
    if (colnames(values$dataset)[1] != 'var1') {
      
      temp <- values$dataset
      temp$val <- 1:nrow(temp)
      col_check <- rlang::sym(input$column_check)
      dat_sub <- outlier_plot_int(temp, x=input$column_check, dat_remove=input$dat.remove,
                                  x_dist=input$x_dist, sd_val = input$datremovenum, plot_type=1)
      arg.return <- outlier_plot_int(temp, x=input$column_check, dat_remove=input$dat.remove,
                                     x_dist=input$x_dist, sd_val = input$datremovenum, plot_type=2)
      
      ggplot2::ggplot(dat_sub[dat_sub$Points=='Kept',], ggplot2::aes(!!col_check)) +
        ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                                na.rm=TRUE, bins = 30, fill = "gray", color = "black") +
        arg.return +
        ggplot2::coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE) +
        fishset_theme() +
        ggplot2::theme(axis.text=ggplot2::element_text(size=12),
                       axis.title=ggplot2::element_text(size=12))
    }
  })
  
  outlierPlot3 <- reactive({
    
    req(input$column_check %in% names(values$dataset))
    
    if (colnames(values$dataset)[1] != 'var1') {
      
      temp <- values$dataset
      temp$val <- 1:nrow(temp)
      temp <- outlier_plot_int(temp, x=input$column_check, dat_remove=input$dat.remove,
                               x_dist=input$x_dist, sd_val = input$datremovenum, plot_type = 3)
      
      ggplot2::ggplot(temp, ggplot2::aes(x=fit_quants, y=data_quants)) +
        ggplot2::geom_point(shape=1) + ggplot2::geom_abline() +
        ggplot2::labs(x='Theoretical Quantiles', y='Sample Quantiles',
                      title=paste('Q-Q plot of', input$x_dist, 'fit against data')) +
        ggplot2::coord_cartesian(xlim = ranges3$x, ylim = ranges3$y, expand = FALSE) +
        fishset_theme() +
        ggplot2::theme(axis.text=ggplot2::element_text(size=12),
                       axis.title=ggplot2::element_text(size=12))
    }
  })
  
  outlier_fig_title <- reactive({
    
    req(input$column_check %in% names(values$dataset))
    
    h4("Plots for ", strong(input$column_check), " with ", strong(input$x_dist),
           " distribution and data removed based on '", strong(input$dat.remove),
           "'. \nBlue: included points   Red: removed points",
       h5("Zoom in on plot by highlighting desired area and double clicking. Double click again to reset the plot."))
  })
  
  # combine outlier plots into one
  outlierPlotAll <- reactive({
    fig <- gridExtra::grid.arrange(outlierPlot1(), outlierPlot2(), outlierPlot3(),
                                   ncol = 2, nrow = 2)
    fig
  })
  
  # Outlier plot output
  output$plot1 <- renderPlot(outlierPlot1())
  
  output$plot2 <- renderPlot(outlierPlot2())
  
  output$plot3 <- renderPlot(outlierPlot3())
  
  output$outlier_fig_title <- renderPrint(
    outlier_fig_title()
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
    style <- paste0("position:absolute; 
                        z-index:100; 
                        background-color: rgba(245, 245, 245, 0.85);  
                        width: 40;",
                    "left:", (hover$range$right +(hover$range$right)/4), "px;",
                    "top:", (hover$range$bottom+hover$range$bottom/4), "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0(
        "<b> Value: </b>", point[[input$column_check]], "<br/>",
        "<b> SD from mean: </b>", find_dev(point[[input$column_check]], values$dataset[[input$column_check]]), "<br/>"
      ))))
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
  
  
  ##Filtering options
  #output_table())
  
  
  observeEvent(input$NA_Filter_all, {
    
    na_names <- qaqc_helper(values$dataset, "NA", output = "names")
    
    if (length(na_names) > 0) {
      
      q_test <- quietly_test(na_filter)
      
      output <- q_test(values$dataset, project = project$name, x = na_names,  
                       replace = FALSE, remove = TRUE, over_write = FALSE)  
      
      if (!is_value_empty(output)) {
        
        values$dataset <- output
      }
      
    } else {
      
      showNotification('No missing values to remove', type = "message")
    }
  })
  
  observeEvent(input$NA_Filter_mean, {
    
    na_names <- qaqc_helper(values$dataset, "NA", output = "names")
    
    if (length(na_names) > 0) {
      
      q_test <- quietly_test(na_filter)
      
      output <- q_test(values$dataset, project = project$name, x = na_names, 
                       replace = TRUE, remove = TRUE, rep.value = "mean", over_write = FALSE)  
      
      if (!is_value_empty(output)) {
        
        values$dataset <- output
      }
      
    } else {
      
      showNotification('No missing values to remove', type = "message")
    }
  })
  
  observeEvent(input$NAN_Filter_all, {
    
    nan_names <- qaqc_helper(values$dataset, "NaN", output = "names")
    
    if (length(nan_names) > 0) {
      
      q_test <- quietly_test(nan_filter)
      output <- q_test(values$dataset, project = project$name, x = nan_names, 
                       replace = FALSE, remove = TRUE, over_write = FALSE) 
      
      if (!is_value_empty(output)) {
        
        values$dataset <- output
      }
      
    } else {
      
      showNotification('No non-numbers to remove.', type = "message")
    }
  })
  
  observeEvent(input$NAN_Filter_mean, {
    
    nan_names <- qaqc_helper(values$dataset, "NaN", output = "names")
    
    if (length(nan_names) > 0) {
      
      q_test <- quietly_test(nan_filter)
      output <- q_test(values$dataset, project = project$name, x = nan_names, 
                       replace = TRUE, remove = FALSE, rep.value = "mean", over_write = FALSE) 
      
      if (!is_value_empty(output)) {
        
        values$dataset <- output
      }
      
    } else {
      
      showNotification('No non-numbers to remove.', type = "message")
    }
  })
  
  observeEvent(input$Outlier_Filter, {
    
    q_test <- quietly_test(outlier_remove)
    output <- q_test(values$dataset, project = project$name, x = input$column_check, 
                     dat.remove = input$dat.remove, sd_val = input$datremovenum, over_write = FALSE)
    
    if (!is_value_empty(output)) {
      
      values$dataset <- output
    }
  })
  
  observeEvent(input$Unique_Filter, {
    
    q_test <- quietly_test(unique_filter)
    output <- q_test(values$dataset, project = project$name, remove = TRUE)
    
    if (!is_value_empty(output)) {
      
      values$dataset <- output
    }
  })
  
  observeEvent(input$Empty_Filter, {
    
    q_test <- quietly_test(empty_vars_filter)
    output <- q_test(values$dataset,  project = project$name, remove = TRUE) 
    
    if (!is_value_empty(output)) {
      
      values$dataset <- output
    }
  })
  
  observeEvent(input$LatLon_Filter, {
    
    lat <- if (input$LatDirection == 'None') NULL else input$LatDirection
    lon <- if (input$LonDirection == 'None') NULL else input$LonDirection
    latsign <- if (input$LatLon_Filter_Lat=='None') NULL else input$LatLon_Filter_Lat
    lonsign <- if (input$LatLon_Filter_Lon=='None') NULL else input$LatLon_Filter_Lon
    
    q_test <- quietly_test(degree)
    output <- q_test(values$dataset, project$name, lat = lat, lon = lon, latsign = latsign, 
                     lonsign = lonsign, replace = TRUE)
    
    if (!is_value_empty(output)) {
      
      values$dataset <- output
    }   
  })
  
  ## Spatial QAQC ----
  
  spat_ui <- reactiveValues(lon_cols = NULL, lat_cols = NULL,
                            date_cols = NULL, grp_cols = NULL,
                            ID = NULL)
  
  # TODO: update so that these are done in selectizeInput, add placeholder
  observeEvent(load_r$main, {
    
    spat_ui$lon_cols <- find_lon(values$dataset)
    spat_ui$lat_cols <- find_lat(values$dataset)
    spat_ui$date_cols <- colnames(values$dataset)
    spat_ui$grp_cols <- category_cols(values$dataset)
    spat_ui$ID <- colnames(values$dataset)
  })
  
  output$spatQAQC_checkUI <- renderUI({
    
    if (names(spatdat$dataset)[1] != "var1") {
      
      tagList(
        selectInput("spat_qaqc_ID", "Select zone ID from main data",
                    choices = spat_ui$ID, multiple = FALSE),
        selectizeInput("spat_qaqc_lon", "Select Longitude from main data",
                       choices = spat_ui$lon_cols, multiple = FALSE, 
                       options = list(create = TRUE)),
        selectizeInput("spat_qaqc_lat", "Select Latitude from main data",
                       choices = spat_ui$lat_cols, multiple = FALSE, 
                       options = list(create = TRUE)),
        selectizeInput("spat_qaqc_date", "Select date variable", 
                       choices = spat_ui$date_cols, multiple = FALSE, 
                       options = list(create = TRUE)),
        add_prompter(textInput("spat_qaqc_epsg", "(Optional) enter spatial reference EPSG code",
                               value = NULL),
                     message = "Option to manually set the spatial reference EPSG code for
                                spatial and primary datasets. If EPSG is specified in the spatial data and 
                                this box is left empty, then the EPSG of the spatial data will be 
                                automatically applied to primary data.",
                     type = "info", size = "medium", position = "top"),
        selectizeInput("spat_qaqc_grp", "(Optional) select grouping variable",
                       choices = spat_ui$grp_cols,
                       multiple = TRUE, options = list(maxItems = 1, create = TRUE)),
        actionButton("runSpatQAQC", "Run spatial check",
                     style = "color: white; background-color: #0073e6;")
      )
    }
  })
  
  spat_qaqc_r <- reactiveValues(flag = FALSE, c_tab = NULL)
  
  # run spatial checks 
  spat_qaqc <- eventReactive(input$runSpatQAQC, {
    
    q_test <- quietly_test(spatial_qaqc)
    
    out <- q_test(dat = values$dataset, project = project$name, spat = spatdat$dataset, 
                  lon.dat = input$spat_qaqc_lon, lat.dat = input$spat_qaqc_lat,
                  date = input$spat_qaqc_date, group = input$spat_qaqc_grp, epsg = input$spat_qaqc_epsg)
    
    if (!is_value_empty(out)) {
      
      flag_nms <- c("ON_LAND", "OUTSIDE_ZONE", "ON_ZONE_BOUNDARY")
      spat_qaqc_r$flag <- vapply(flag_nms, function(x) x %in% names(out$dataset), logical(1))
      
      values$dataset <- subset(out$dataset, select=-c(YEAR))
      out$dataset <- NULL
      
      qaqc_out_proj$spat <- project$name
      
      out
    }
    
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # spatial checks output
  output$spatQAQC_checkOut <- renderUI({
    
    if (names(spatdat$dataset)[1] != "var1") {
      
      render_out <-
        lapply(names(spat_qaqc()), function(x) {
          
          if (is.data.frame(spat_qaqc()[[x]])) {
            
            tab_header <- switch(x, "spatial_summary" = "Spatial summary table",
                                 "distance_freq" = "Distance (m) frequency table",
                                 "distance_summary" = "Distance (m) summary table")
            
            tagList(
              h4(strong(tab_header)),
              DT::renderDT(spat_qaqc()[[x]])
            )
            
          } else {
            
            plot_header <- switch(x, "outside_plot" = "Points outside zone",
                                  "land_plot" = "Points on land",
                                  "land_outside_plot" = "Points on land/outside zone",
                                  "boundary_plot" = "Points on zone boundary",
                                  "expected_plot" = "Points at sea and within zones",
                                  "distance_plot" = "Density of point distance (m) from nearest zone")
            
            tagList(
              h4(strong(plot_header)),
              n_plot_output(spat_qaqc()[[x]])
            )
          }
          
        })
      
      render_out
      
    } else {
      
      p("Spatial data not loaded. Import spatial data on the 'Upload' tab.")
    }
  })
  
  # Spatial Correction
  
  output$spatQAQC_correctUI <- renderUI({
    
    if (any(spat_qaqc_r$flag)) {
      
      tagList(
        h6("Spatial correction options:"),
        p("1. The table to the right can be used to edit spatial columns as needed. Note that the table can be filtered (e.g., show only rows where ON_LAND = true)."),
        p("2. Use the inputs below to change signs of latitude or longitude. Running spatial corrections will also convert lat, lon coordinates to decimal degrees."),
        
        selectInput('spat_filter_lat', 'Change sign for latitude direction', 
                    choices=c('None', 'All values'='all', 'Positve to negative'='neg', 
                              'Negative to positive'='pos'), selected='None'),
        
        selectInput('spat_filter_lon', 'Change sign for longitude direction', 
                    choices=c('None', 'All values'='all', 'Positve to negative'='neg', 
                              'Negative to positive'='pos'), selected='None'),
        
        add_prompter(
          actionButton("spat_filter_bttn", "Run spatial corrections",
                       style = "color: white; background-color: #0073e6;"),
          message = "Running spatial corrections will execute any sign changes specified and will 
                     convert lat and lon coordinates to decimal degress if not already in this format",
          type = "info", size = "medium", position = "top"),
        
        if ("NEAREST_ZONE_DIST_M" %in% names(values$dataset)) {
          
          tagList(
            actionButton("dist_remove_bttn", "Remove points",
                         style = "color: white; background-color: #0073e6;"),
            numericInput("dist_remove", "Distance (m) from nearest zone",
                         value = 100, min = 1),
            sliderInput("dist_slider", "",
                        min = 1,
                        max = ceiling(max(values$dataset$NEAREST_ZONE_DIST_M, na.rm = TRUE)),
                        value = 100))
        }
      )
    }
  })
  
  
  output$spat_correct_msg <- renderUI({
    
    if (names(spatdat$dataset)[1] == "var1") {
      
      p("Spatial data not loaded. Import spatial data on the 'Upload' tab.")
      
    } else if (length(spat_qaqc_r$flag) == 1) {
      
      p("Spatial checks have not been run.")
      
    } else if (length(spat_qaqc_r$flag) == 3 & all(spat_qaqc_r$flag) == FALSE) {
      
      p("No spatial issues were found.")
    }
  })
  
  # add flag cols to dataset
  observeEvent(any(spat_qaqc_r$flag), {
    
    case_to_print$dataQuality <- c(case_to_print$dataQuality,
                                   'Spatial data quality checked.')
    
    if (any(spat_qaqc_r$flag)) {
      
      case_to_print$dataQuality <- c(case_to_print$dataQuality,
                                     'Detected observations on land, outside zone, and/or on zone boundary.\n')
    } else {
      
      case_to_print$dataQuality <- c(case_to_print$dataQuality, 'No spatial issues detected.\n')
    }
    
    # disable editing for non-latlon columns
    latlon <- which(names(values$dataset) %in%
                      c(input$spat_qaqc_lon, input$spat_qaqc_lat))
    
    spat_qaqc_r$disable <- which(!(seq_along(values$dataset) %in% latlon))
    #
    
  })
  
  # Distance filter
  dist_filter <- reactive({
    
    if (any(spat_qaqc_r$flag)) {
      
      if ("NEAREST_ZONE_DIST_M" %in% names(values$dataset)) {
        
        values$dataset$NEAREST_ZONE_DIST_M >= input$dist_slider
        
      } else FALSE
    }
  })
  
  # correction table
  c_tab <- reactive({
    
    if (any(spat_qaqc_r$flag)) {
      
      if (input$select_spat_tab == "out_zone") {
          
        if (sum(dist_filter()) > 0) values$dataset[dist_filter(), c(input$spat_qaqc_ID, input$spat_qaqc_date, input$spat_qaqc_lat,
                                                                    input$spat_qaqc_lon, "ON_LAND", "ON_ZONE_BOUNDARY", "EXPECTED_LOC")]
      } else { # "all"
        
        new_cols <- c("ON_LAND", "ON_ZONE_BOUNDARY", "EXPECTED_LOC")
        new_cols <- new_cols[which(spat_qaqc_r$flag)]
        
        values$dataset[,c(input$spat_qaqc_ID, input$spat_qaqc_lat,
                          input$spat_qaqc_lon, new_cols)]
      }
    }
  })
  
  
  observe({
    spat_qaqc_r$c_tab <- c_tab()
  })
  
  # Correction table
  output$spat_correct_tab <- DT::renderDT({
    
    c_tab()
  },
  
  server = TRUE, 
  editable = list(target ='cell', disable = list(columns = c(1:2,5:7))), 
  filter = 'top', extensions = c("Buttons"),
  options = list(autoWidth = TRUE, scrolly = TRUE, responsive = TRUE, pageLength = 15,
                 searchCols = default_search_columns, buttons = c('csv'))
  )
  
  # edit table
  spat_qaqc_proxy <- DT::dataTableProxy("spat_correct_tab", session)
  
  observeEvent(input$spat_correct_tab_cell_edit, {
    
    spat_qaqc_r$c_tab <<- DT::editData(spat_qaqc_r$c_tab,
                                       info = input$spat_correct_tab_cell_edit,
                                       proxy = "spat_correct_tab",
                                       resetPaging = FALSE)
    
    DT::replaceData(spat_qaqc_proxy, spat_qaqc_r$c_tab, resetPaging = FALSE)
  })
  
  # update correction table UI
  observeEvent(input$dist_remove, {
    
    updateSliderInput(session, "dist_slider", value = input$dist_remove)
  })
  
  observeEvent(input$dist_slider, {
    
    updateSliderInput(session, "dist_remove", value = input$dist_slider)
  })
  
  observeEvent(sum(dist_filter()), {
    
    updateActionButton(session, "dist_remove_bttn",
                       label = paste("Remove", sum(dist_filter()), "points"))
  })
  
  # remove points based on distance
  observeEvent(input$dist_remove_bttn, {
    
    # add pop-up confirming removal 
    
    showModal(
      modalDialog(title = paste("Remove", sum(dist_filter()), "rows?"),
                  
                  actionButton("confirm_dist_remove", "Remove", 
                               style = "color: white; background-color: #0073e6;"),
                  actionButton("dist_remove_cancel", "Cancel", 
                               style = "color: #fff; background-color: #FF6347; border-color: #800000;"),
                  
                  footer = tagList(modalButton("Close")),
                  easyClose = FALSE, size = "s"))
  })
  
  
  observeEvent(input$dist_remove_cancel, removeModal())
  
  
  observeEvent(input$confirm_dist_remove, {
    
    nr <- sum(dist_filter())
    
    values$dataset <- values$dataset[!dist_filter(), ]
    
    removeModal()
    
    showNotification(paste(nr, "points removed"), type = "message")
    
    filter_table(values$dataset, project$name, x = "NEAREST_ZONE_DIST_M",
                 exp = paste0("NEAREST_ZONE_DIST_M < ", input$dist_remove))
    
    case_to_print$dataQuality <- c(case_to_print$dataQuality,
                                   paste("Points greater than", input$dist_remove,
                                         "from nearest zone were remove.", nr,
                                         "observations in total."))
    
  }, ignoreInit = TRUE)
  
  # update Lat Lon
  observeEvent(input$spat_correct_tab_cell_edit, {
    
    if (input$select_spat_tab == "out_zone") {
      
      values$dataset[dist_filter(),
                     c(input$spat_qaqc_lat,
                       input$spat_qaqc_lon)] <- spat_qaqc_r$c_tab[c(input$spat_qaqc_lat,
                                                                    input$spat_qaqc_lon)]
    } else {
      
      values$dataset[c(input$spat_qaqc_lat, 
                       input$spat_qaqc_lon)] <- spat_qaqc_r$c_tab[c(input$spat_qaqc_lat, 
                                                                    input$spat_qaqc_lon)]
    }
    
    showNotification("Latitude and longitude values updated to main table",
                     type = "message")
  })
  
  # change Lat/Lon signs
  observeEvent(input$spat_filter_bttn, {
    
    q_test <- quietly_test(degree)
    
    if (input$select_spat_tab == "out_zone") {
      
      values$dataset[dist_filter(), ] <-
        q_test(c_tab(), project = project$name, lat = input$spat_qaqc_lat, 
               lon = input$spat_qaqc_lon, latsign = input$spat_filter_lat,
               lonsign = input$spat_filter_lon, replace = TRUE)
    } else {
    
      values$dataset <-
        q_test(c_tab(), project = project$name, lat = input$spat_qaqc_lat, 
               lon = input$spat_qaqc_lon, latsign = input$spat_filter_lat,
               lonsign = input$spat_filter_lon, replace = TRUE)
    }
  })
  
  
  # ---        
  # DATA EXPLORATION ----
  # ---
  #1. TABLE
  # TODO add spatial data
  output$SelectDatasetExploreUI <- renderUI({
    
    tagList(
      conditionalPanel("input.plot_table=='Table'",
                       selectInput("SelectDatasetExplore", "Select a data file type", 
                                   choices = c("Primary"="main", "Port"="port", 
                                               "Auxiliary"="auxiliary", "Gridded" = "grid"),
                                   selected='main')),
      conditionalPanel("input.SelectDatasetExplore == 'grid'",
                       selectInput("grid_select", "select gridded data file", 
                                   choices = names(grddat)))
    )
  })
  
  explore_temp <- reactive({
    req(input$SelectDatasetExplore)
    if (input$SelectDatasetExplore == "grid") {
      
      grddat[[input$grid_select]]  
      
    } else {
      switch(input$SelectDatasetExplore,
             "main" = values$dataset,
             "port" = ptdat$dataset,
             "auxiliary" = aux$dataset)
    }
  })
  
  observeEvent(c(input$subsetData,
                 input$output_table_exploration_search_columns), {
                   # cell edited? 
                   if (input$SelectDatasetExplore == "main") {
                     values$dataset <- explore_temp()
                   } else if (input$SelectDatasetExplore == "port") {
                     ptdat$dataset <- explore_temp()
                   } else if (input$SelectDatasetExplore == "grid") {
                     grddat[[input$grid_select]] <- explore_temp()
                   } else if (input$SelectDatasetExplore == "aux") {
                     aux$dataset <- explore_temp()
                   }
                 }, ignoreInit = TRUE)
  
  output$output_table_exploration <- DT::renderDT(
    if(colnames(explore_temp())[1] == 'var1') {
      return(NULL)
    } else {
      if(input$plot_table=='Table') { 
        c1 <- explore_temp()
        colnames(c1)=gsub("_","-", colnames(c1))
        return(c1)
      } else {
        NULL
      }}, server = TRUE, editable=TRUE, filter='top', selection=list(target ='column'),
    extensions = c("Buttons"), rownames=FALSE,
    options = list(autoWidth=TRUE, scrolly=TRUE, responsive=TRUE, pageLength = 15,
                   searchCols = default_search_columns, buttons = c('csv'))
  )
  
  observeEvent(input$output_table_exploration_cell_edit, {
    info = 
      str(info)
    i = info$row
    j = info$col + 1
    v = info$value
    values$dataset[i, j] <<- DT:::coerceValue(v, values$dataset[i, j])
    
    #replaceData(proxy, x, resetPaging = FALSE, rownames = FALSE)
  })
  
  
  # TODO: check that this is working properly (very messy)
  observeEvent(input$saveData,{
    req(project$name)
    # when it updates, save the search strings so they're not lost
    # update global search and column search strings
    default_search_columns <- c(input$output_table_exploration_search_columns)
    default_sub <- which(default_search_columns!='')
    temp <- values$dataset
    table_type <- #switch(input$SelectDatasetExplore, 
      #"main" = 
      "MainDataTable"#,
    #"port" = "PortTable",
    #"grid" = input$GridName,
    #"auxiliary" = input$AuxName)
    
    if(length(default_sub)==0){
      NULL
    } else {
      if(table_exists(paste0(project$name, "FilterTable"), project$name) == FALSE) {
        FilterTable <- data.frame(dataframe = NA, vector = NA, FilterFunction = NA)
      } else {
        FilterTable <- table_view(paste0(project$name, "FilterTable"), project$name)
      }
      for(i in 1:length(default_sub)){
        if( grepl("\\..\\.", default_search_columns[default_sub[i]])==TRUE){
          FilterTable <- rbind(FilterTable, c(paste0(project$name, table_type), (colnames(explore_temp()[default_sub])[i]), 
                                              paste(colnames(explore_temp()[default_sub])[i], '>', as.numeric(sapply(strsplit(default_search_columns[default_sub[i]], "\\..\\."), head, 1)), '&', 
                                                    colnames(explore_temp()[default_sub])[i], '<', as.numeric(sapply(strsplit(default_search_columns[default_sub[i]], "\\..\\."), tail, 1)))))
          values$dataset <-  subset(explore_temp(), eval(parse(text=paste(colnames(explore_temp()[default_sub])[i], '>', 
                                                                          as.numeric(sapply(strsplit(input$output_table_exploration_search_columns[default_sub[i]], "\\..\\."), head, 1)), '&', 
                                                                          colnames(explore_temp()[default_sub])[i], '<', 
                                                                          as.numeric(sapply(strsplit(input$output_table_exploration_search_columns[default_sub[i]], "\\..\\."), tail, 1))))))
        } else {
          FilterTable <- rbind(FilterTable, c(paste0(project$name, table_type), (colnames(explore_temp()[default_sub])[i]), 
                                              paste0("grepl('", default_search_columns[default_sub[i]],"', ", colnames(explore_temp()[default_sub])[i],")")))
          values$dataset <-  subset(explore_temp(), eval(parse(text=  paste0("grepl('", default_search_columns[default_sub[i]],"', ", colnames(explore_temp()[default_sub])[i],")"))))
        }
      }
      showNotification('Filter table saved to FishSET database', type='message', duration=10)
      
      filter_data_function <- list()
      filter_data_function$functionID <- 'filter_table'
      filter_data_function$args <- c(paste0(project$name, table_type), project$name, FilterTable$vector[nrow(FilterTable)],  FilterTable$FilterFunction[nrow(FilterTable)])
      filter_data_function$kwargs <- list()
      filter_data_function$output <- c('')
      filter_data_function$msg <- FilterTable
      log_call(project$name, filter_data_function)
      
      fishset_db <- suppressWarnings(DBI::dbConnect(RSQLite::SQLite(), locdatabase(project$name)))
      DBI::dbWriteTable(fishset_db, paste0(project$name, 'FilterTable'),  FilterTable, overwrite=TRUE)
      DBI::dbDisconnect(fishset_db)
    }
    #values$dataset <- temp
  })
  
  
  observeEvent(input$subsetData,{
    req(!is.null(input$output_table_exploration_columns_selected))
    values$dataset <- values$dataset[,-(input$output_table_exploration_columns_selected+1)]
  })
  
  output$editText <- renderText('Edit cells: double click. \nEdited table will not \nbe loaded into working \nenvironment until saved.
                                    \nFilter: Boxes at top.
                                    \nFilter functions saved to \nFishSET database as \nFilterTable when "save \ndata" button is pushed.
                                    \nRemove variables: Click on \ncolumn cell(s), then click \n"Remove Variable" button.\nVariables can be added back \nusing the add_vars function.
                                    \nClick the "Save Data" button \nto save changes.')
  
  #Subset by columns
  
  #2. Temporal PLOTS
  output$xy_selectUI <- renderUI({
    tagList(
      selectInput('x_y_select1', 'Select x-axis variable', choices= numeric_cols(values$dataset), 
                  selected= numeric_cols(values$dataset)[1], multiple=FALSE, selectize=TRUE),
      
      selectInput('x_y_select2', 'Select y-axis variable', choices= numeric_cols(values$dataset), 
                  selected= numeric_cols(values$dataset)[2], multiple=FALSE, selectize=TRUE)
    )
  })
  
  output$column_select <- renderUI({
    #  tags$div(align = 'left', class = 'multicol', 
    tagList(
      selectInput("col_select", "Select column name", choices = names(values$dataset), 
                  selected = numeric_cols(values$dataset)[1], 
                  multiple=FALSE, selectize = TRUE), #)
      
      selectInput("date_select", "Select date column", choices = date_cols(values$dataset))
    )
  })
  
  # output project tracker
  explore_out_proj <- reactiveValues(temp = NULL, map = NULL, kernel = NULL, 
                                     xy = NULL, grid = NULL, gtmt = NULL)
  
  plotInputTemporal <-  eventReactive(input$run_temporal_plot, {
    
    req(input$col_select, input$p2fun, input$p3fun, input$date_select)
    
    if (colnames(values$dataset)[1] != 'var1') {
      
      len_fun <- switch(input$p2fun, "No. observations" = "length",
                        'No. unique observations' = "unique",
                        '% of total observations' = "percent")
      
      explore_out_proj$temp <- project$name
      
      q_test <- quietly_test(temp_plot)
      out <- q_test(values$dataset, project$name, input$col_select,
                    len.fun = len_fun, agg.fun = input$p3fun, 
                    date.var = input$date_select)
      
      out 
    }
  }, ignoreInit = TRUE)
  
  
  output$plot_time <- renderPlot(plotInputTemporal())
  
  #3. SPATIAL DISTRIBUTION
  ranges_spatial <- reactiveValues(x = NULL, y=NULL)
  
  
  lon_col <- reactive({
    
    if (colnames(values$dataset)[1] != 'var1') {
      find_lon(values$dataset)[1]
      #lon_count <- stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)')
      #which(lon_count==max(lon_count))[1]
    }
  })
  
  lat_col <- reactive({
    
    if (colnames(values$dataset)[1] != 'var1') {
      find_lat(values$dataset)[1]
      #lat_count <- stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)')
      #which(lat_count==max(lat_count))[1]
    }
  })
  
  
  observeEvent(input$plot_type, {
    
    if (input$plot_type == "Spatial-autocorrelation") {
      
      min_lon <- min(values$dataset[lon_col()], na.rm=TRUE)
      max_lon <- max(values$dataset[lon_col()], na.rm=TRUE)
      
      min_lat <- min(values$dataset[lat_col()], na.rm=TRUE)
      max_lat <- max(values$dataset[lat_col()], na.rm=TRUE)
      
      ranges_spatial$x <-
        c(ifelse((min_lon - abs(min_lon/10) < -180), -180, min_lon - abs(min_lon/10)),
          ifelse((max_lon + abs(max_lon/10) > 180), 180, max_lon + abs(max_lon/10)))
      
      ranges_spatial$y <-
        c(ifelse((min_lat - abs(min_lat/10) < -90), -90, min_lat - abs(min_lat/10)),
          ifelse((max_lat + abs(max_lat/10) > 90), 90, max_lat + abs(max_lat/10)))
    }
  }, ignoreInit = TRUE)
  
  
  plotInputSpatial <- reactive({
    
    req(ranges_spatial$x, ranges_spatial$y)
    
    if (colnames(values$dataset)[1] != 'var1') {
      
      explore_out_proj$spat <- project$name
      
      q_test <- quietly_test(map_plot)
      q_test(values$dataset, project$name, lat = lat_col(), lon = lon_col(),
             minmax = c(ranges_spatial$y, ranges_spatial$x))
    }
  })
  
  output$plot_spatial <- renderPlot(plotInputSpatial())
  
  plotZoneSummary <- eventReactive(input$run_zone_summ, {
    # Need to convert date cols to date variables
    d_cols <- date_cols(values$dataset)
    zone_summ_df <- values$dataset
    try(
      zone_summ_df[d_cols] <- lapply(d_cols, function(d) as.Date(zone_summ_df[[d]]))
    )

    # If user wants to filter dataset
    if(!is.null(input$zone_summ_value) && input$zone_summ_value != ""){
      # Check if value should be wrapped in quotes
      if(any(class(zone_summ_df[[input$zone_summ_var]]) %in% c("character", "factor", "Date", "POSIXct", "POSIXt"))){
        zone_summ_expr <- paste0(input$zone_summ_var, input$zone_summ_operator, '"', input$zone_summ_value, '"')
      } else {
        zone_summ_expr <- paste0(input$zone_summ_var, input$zone_summ_operator, input$zone_summ_value)
      }
      zone_summ_df <- eval(parse(text = paste0("subset(values$dataset, ", zone_summ_expr, ")")))
    }

    if(input$zone_summ_varPlot == 'observations'){
      zone_summ_count <- TRUE
      zone_summ_varIN <- NULL
      zone_summ_fun <- input$zone_summ_fun1
    } else {
      zone_summ_count <- FALSE
      zone_summ_varIN <- input$zone_summ_varPlot
      zone_summ_fun <- input$zone_summ_fun2
    }

    if(colnames(values$dataset)[1] != 'var1') {
      if(zone_summ_fun == "number of obs"){
        fun_option <- NULL
      } else {
        fun_option <- zone_summ_fun
      }

      q_test <- quietly_test(zone_summary)
      q_test(dat = zone_summ_df, project = project$name, spat = spatdat$dataset,
             zone.dat = input$zone_summ_dat, zone.spat = input$zone_summ_spat,
             output = "plot", count = zone_summ_count, breaks = NULL, n.breaks = 10, na.rm = TRUE,
             fun = fun_option, var = zone_summ_varIN)
    }
  }, ignoreInit = TRUE)

  output$plot_zone_summary <- plotly::renderPlotly(plotZoneSummary())
    
  plotInputKernel <- reactive({
    
    if(colnames(values$dataset)[1] != 'var1') {
      
      explore_out_proj$kernel <- project$name
      
      q_test <- quietly_test(map_kernel)
      q_test(values$dataset, project=project$name, type='gradient',
             latlon=c(lat_col(), lon_col()))
    }
  })
  
  output$map_kernel <- renderPlot(plotInputKernel())
  
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
      longitude <- which(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)')==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)')))[1]
      latitude <- which(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)')==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)')))[1]
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
      
      if (names(spatdat$dataset)[1]=='var1') {
        
        tags$div(h5('Spatial data file not loaded. Please load on Upload Data tab', style="color:red"))
      },
      
      tags$div(style = "margin-left:19px;", 
               selectizeInput('varofint', 'Variable to test for spatial autocorrelation',
                              choices = numeric_cols(values$dataset))),
      checkboxInput('datzone_gt', 'Primary data contains a zone/area assignment variable', value = FALSE),
    )
  })    
  
  output$mtgt_output_secondary <- renderUI({
    tagList(
      if (!is.null(input$datzone_gt) && input$datzone_gt) {
        
        selectInput('mtgtZone', 'Column containing zone identifier',
                    choices = colnames(values$dataset))
      } else {
        
        tags$div(style = "margin-left:19px;",
                 selectizeInput('gtmt_lonlat', 'Select lat then lon from data frame to assign observations to zone',
                                choices=find_lonlat(values$dataset),
                                multiple = TRUE, options = list(maxItems = 2,
                                                                create = TRUE,
                                                                placeholder='Select or type variable name')))
      }
    )
  })
  
  
  output$mtgt_out2 <- renderUI({
    tagList(
      tags$div(style = "margin-left:19px;", 
               selectInput('mtgtcat', "Variable defining zones or areas from spatial data frame", 
                           choices = c('none', names(spatdat$dataset)),
                           selected='none')),
      
      tags$div(style = "margin-left:19px;", 
               selectizeInput('mtgtlonlat', 'Select vector containing latitude then longitude from spatial data frame', 
                              choices= c(NULL, names(spatdat$dataset)), 
                              multiple=TRUE, options = list(maxItems = 2,
                                                            create = TRUE, 
                                                            placeholder='Select or type variable name')))
    )
  })
  
  
  output$zone_summary_out1 <- renderUI({
    tagList(
      h4('Select variables to display observations by zone'),

      if (names(spatdat$dataset)[1]=='var1') {

        tags$div(h5('Spatial data file not loaded. Please load on Upload Data tab', style="color:red"))
      },

      tags$div(style = "margin-left:19px;",
               selectInput('zone_summ_dat', 'Select column containing zone ID in main data table',
                           choices = colnames(values$dataset))),

      tags$div(style = "margin-left:19px;",
               selectInput('zone_summ_spat', 'Select column containing zone ID in spatial data table',
                           choices = colnames(spatdat$dataset))),

      tags$div(style = "margin-left:19px;",
               selectInput('zone_summ_varPlot', 'Select a variable to plot',
                           choices = c("observations", numeric_cols(values$dataset)), selected = "observations")),

      conditionalPanel(condition = "input.zone_summ_varPlot=='observations'",
                       tags$div(style = "margin-left:19px;",
                                selectizeInput('zone_summ_fun1', 'Select a function to summarize observations',
                                               choices = c("number of obs","percent")))),

      conditionalPanel(condition = "input.zone_summ_varPlot!='observations'",
                       tags$div(style = "margin-left:19px;",
                                selectizeInput('zone_summ_fun2', 'Select a function to summarize variable',
                                               choices = c("sum","percent","mean","median","min","max")))),

      tags$div(style = "margin-left:19px;",
               selectInput('zone_summ_var', 'Select a variable for filtering the dataset',
                           choices = c('none', colnames(values$dataset)), selected = 'none')),

      conditionalPanel("input.zone_summ_var!='none'",
                       tags$div(style = "margin-left:60px;",
                                selectizeInput('zone_summ_operator', 'Select an operator',
                                               choices = c("less than" = "<","greater than" = ">", "less than or equal to" = "<=",
                                                           "greater than or equal to" = ">=", "equal to" = "==", "not equal to" = "!="))),
                       uiOutput("zone_summ_valUI")
      )
    )
  })
  
  
  # used for value input in zone summary
  zs_unique_values <- reactive({
    
    if (is.null(input$zone_summ_var)) {
      
      NULL
      
    } else {
      
      out <- unique(values$dataset[[input$zone_summ_var]])
      
      if (length(out) > 50)  out <- out[1:50]
      
      out
    }
  })
  
  
  # select value input for zone summary
  output$zone_summ_valUI <- renderUI({
    
    if (is.null(input$zone_summ_operator)) {
      
      tags$div(style = "margin-left:60px;",
               selectizeInput("zone_summ_value", "Value", choices = "")) # placeholder widget
      
    } else {
      
      tags$div(style = "margin-left:60px;",
               selectizeInput("zone_summ_value", "Value",
                              choices = zs_unique_values(),
                              multiple = TRUE, options = list(maxOptions = 15, maxItems = 1,
                                                              placeholder = "Select or type value name",
                                                              create = TRUE)))
    }
  })
  
  
  gtmt_table <- eventReactive(input$mtgtcat, {
    
    if (names(spatdat$dataset)[1]!='var1' & names(spatdat$dataset)[1]!='var1') {
      
      if (input$mtgtcat != 'none') {
        
        q_test <- quietly_test(getis_ord_stats)
        gt <- q_test(values$dataset, project=project$name, varofint=input$varofint,
                     zoneid = input$mtgtZone,
                     spat=spatdat$dataset, lon.dat=input$gtmt_lonlat[2],
                     lat.dat=input$gtmt_lonlat[1], cat=input$mtgtcat,
                     lon.spat=input$mtgtlonlat[2], lat.spat=input$mtgtlonlat[1])$getistable
        
        q_test <- quietly_test(moran_stats)
        mt <- q_test(values$dataset, project=project$name, varofint=input$varofint,
                     zoneid=input$mtgtZone,
                     spat=spatdat$dataset, lon.dat=input$gtmt_lonlat[2],
                     lat.dat=input$gtmt_lonlat[1], cat=input$mtgtcat,
                     lon.spat=input$mtgtlonlat[2], lat.spat=input$mtgtlonlat[1])$morantable
        
        qaqc_out_proj$gtmt <- project$name
        
        if (!is.null(gt) & !is.null(mt)) {
          
          dplyr::left_join(gt, mt)
        }
      }
    }
  }, ignoreInit = TRUE) 
  
  output$output_table_gt_mt <- DT::renderDT(gtmt_table(), server=TRUE)
  
  #4. X VS. Y
  plotInputXY <- reactive({
    
    req(input$x_y_select1, input$x_y_select2)
    
    if (colnames(values$dataset)[1] != 'var1') {
      
      explore_out_proj$xy <- project$name
      
      q_test <- quietly_test(xy_plot)
      q_test(values$dataset, project$name, input$x_y_select1, input$x_y_select2, 
             regress = FALSE)
    } 
  })
  
  output$plot_xy <- renderPlot(plotInputXY())
  
  output$plot_grid_args <- renderUI({
    tagList(
      actionButton("run_grid", "Plot grid",
                   style = "color: #fff; background-color: #6da363; border-color: #800000;"),
      selectInput("grid_lon", "Longitude", 
                  choices = find_lon(grddat[[input$grid_select]])),
      selectInput("grid_lat", "Latitude", 
                  choices = find_lat(grddat[[input$grid_select]])),
      selectInput("grid_value", "Value", 
                  choices = numeric_cols(grddat[[input$grid_select]])),
      selectizeInput("grid_split", "Split plot by",
                     choices = colnames(grddat[[input$grid_select]]),
                     multiple = TRUE, options = list(maxItems = 2, create = TRUE, 
                                                     placeholder = 'Select or type variable name')),
      selectizeInput("grid_agg", "Group mean value by",
                     choices = c("lonlat", colnames(grddat[[input$grid_select]])),
                     multiple = TRUE, options = list(maxItems = 2, create = TRUE, 
                                                     placeholder = 'Select or type variable name')),
      selectInput("grid_agg_fun", "Aggregate function", 
                  choices = c("mean", "median", "min", "max"))
    )
  })
  
  grid_values <- reactiveValues(plot = NULL)
  
  observeEvent(input$run_grid, {
    
    if (isTruthy(project$name)) {
      
      edit_proj_settings(project$name, 
                         tab_name = input$grid_select, 
                         tab_type = "grid")
      
      explore_out_proj$grid <- project$name
      
      q_test <- quietly_test(view_grid_dat)
      
      grid_values$plot <-
        q_test(gridfile = grddat[[input$grid_select]], project = project$name,
               lon = input$grid_lon, lat = input$grid_lat, value = input$grid_value, 
               split_by = input$grid_split, group = input$grid_agg, 
               agg_fun = input$grid_agg_fun)
    }
  }, ignoreInit = TRUE)
  
  output$grid_plot <- renderPlot(grid_values$plot)
  
  
  # ---
  # FLEET FUNCTIONS ----
  # ---
  fleet_id <- reactive({
    switch(input$fleet_fun, "vessel_count" = "ves", "species_catch" = "spec",
           "roll_catch" = "roll", "weekly_catch" = "wc", "weekly_effort" = "we",
           "bycatch" = "by", "trip_dur_out" = "trip", "density_plot" = "den")
  })
  
  # Save buttons
  output$fleetSaveOutput1 <- renderUI({
    
    tabPlotUI(paste0(fleet_id(), "-save"), type = "tab_plot")
  })
  
  output$fleetSaveOutput2 <- renderUI({
    
    tabPlotUI(paste0(fleet_id(), "-save-summary"), type = "tab_plot")
  })
  
  saveDataTableServ("fleet", values, reactive(project$name))
  
  saveDataTableServ("fleet_summary", values, reactive(project$name))
  
  closeAppServ("fleet")
  
  closeAppServ("fleet_summary")
  
  refreshServ("fleet", values, reactive(project$name))
  
  refreshServ("fleet_summary", values, reactive(project$name))
  
  output$run_fleet_fun <- renderUI({
    runFunUI(fleet_id())
  })
  
  # Fleet modules
  density_serv("den", values, reactive(project$name))
  
  vessel_serv("ves",  values, reactive(project$name))
  
  species_serv("spec", values, reactive(project$name))
  
  roll_serv("roll", values, reactive(project$name))
  
  weekly_catch_serv("wc", values, reactive(project$name))
  
  weekly_effort_serv("we", values, reactive(project$name))
  
  bycatch_serv("by", values, reactive(project$name))
  
  trip_serv("trip", values, reactive(project$name))
  
  fleet_table_serv("f_table", values, reactive(project$name))
  
  fleet_assign_serv("f_assign", values, reactive(project$name))
  
  # R expr output
  # RexpressionServ("fleet", values)
  prevRunFleet <- reactiveValues(fleet = 0,
                                 fleet_summary = 0,
                                 fleetExprText = "values$dataset")
  
  observeEvent(c(input$runFleet, input$runFleetSummary), {
    shinyjs::hide("error")
    r$ok <- FALSE
    
    # Check which tab triggered the observeEvent
    if(prevRunFleet$fleet != input$runFleet){ # Fleet assignment tab triggered
      prevRunFleet$fleetExprText <- input$exprFleet
    } else { # Fleet summary tab triggered
      prevRunFleet$fleetExprText <- input$exprFleetSummary
    }
    
    # Update status of even triggers
    prevRunFleet$fleet <- input$runFleet
    prevRunFleet$fleetSummary <- input$runFleetSummary
    
    tryCatch(
      {
        r$output <- isolate(
          paste(utils::capture.output(eval(parse(text = prevRunFleet$fleetExprText))), collapse = '\n')
        )
        r$ok <- TRUE
      },
      error = function(err) {r$output <- err$message}
    )
    r$done <- r$done + 1
  })
  
  output$resultFleet <- renderUI({
    if(r$done > 0 ) { 
      content <- paste(paste(">", isolate(prevRunFleet$fleetExprText)), r$output, sep = '\n')
      if(r$ok) {
        pre(content)
      } else {
        pre(style = "color: red; font-weight: bold;", content)
      }
    }
  })
  
  output$resultFleetSummary <- renderUI({
    if(r$done > 0 ) { 
      content <- paste(paste(">", isolate(prevRunFleet$fleetExprText)), r$output, sep = '\n')
      if(r$ok) {
        pre(content)
      } else {
        pre(style = "color: red; font-weight: bold;", content)
      }
    }
  })
  
  
  # ---    
  # DATA ANALYSIS ----
  # ---
  # output project tracker
  anal_out_proj <- reactiveValues(corr = NULL, reg = NULL, corr_out = NULL, 
                                  reg_out = NULL)
  
  output$corr_out <- renderUI({
    selectInput('corr_select', 'Select variables to include in correlation test', 
                choices = numeric_cols(values$dataset),
                selected = numeric_cols(values$dataset),
                multiple=TRUE, selectize=TRUE, width='90%')
  })
  
  corr_out_2 <- reactive({
    
    if (colnames(values$dataset)[1] != 'var1') {
      
      if (length(input$corr_select) == 2) {
        
        anal_out_proj$corr_out <- project$name
        fm <- stats::reformulate(c(input$corr_select[1], input$corr_select[2]), 
                                 response = NULL)
        formula <- rlang::enexpr(fm)
        cor_call <- rlang::expr(cor.test(!!formula, data = values$dataset))
        
        eval(cor_call, rlang::current_env())
      }
    }
  })
  
  output$output_text_corr <- renderPrint(corr_out_2())
  
  inputCorr <- eventReactive(input$run_corr, {
    
    if (colnames(values$dataset)[1] != 'var1') {
      
      anal_out_proj$corr <- project$name
      q_test <- quietly_test(corr_out)
      q_test(values$dataset, project$name, input$corr_select)
    }
  }, ignoreInit = TRUE)
  
  output$output_table_corr <- DT::renderDT(
    inputCorr()$table, server=TRUE, extensions = list('Scroller'), 
    options=list(autoWidth = TRUE, scrollX=TRUE, deferRender = TRUE,
                 scrollY = 'auto', scroller = TRUE, scrollX = TRUE, pageLength = 25)
  )
  
  output$output_plot_corr <- renderPlot(inputCorr()$plot)
  
  output$reg_resp_out <- renderUI({
    
    selectInput('reg_resp_select', 'Select response variable', 
                choices = numeric_cols(values$dataset), 
                multiple = FALSE, selectize = TRUE)
  })
  
  output$reg_exp_out <- renderUI({
    
    selectInput('reg_exp_select', 'Select explanatory variable', 
                choices = names(values$dataset), multiple = FALSE, selectize = TRUE)
  })
  
  lm_out1 <- eventReactive(input$run_reg, {
    if (colnames(values$dataset)[1] != 'var1') {
      anal_out_proj$reg <- project$name
      q_test <- quietly_test(xy_plot)
      q_test(values$dataset, project$name, input$reg_exp_select,
                        input$reg_resp_select, regress = TRUE)
    }
  }, ignoreInit = TRUE)

  # TODO: Had to repeat the same eventReactive expression to display both the lm print and plot outputs, but there must be a way without repeating this expression
  lm_out2 <- eventReactive(input$run_reg, {
    if (colnames(values$dataset)[1] != 'var1') {
      anal_out_proj$reg <- project$name
      q_test <- quietly_test(xy_plot)
      q_test(values$dataset, project$name, input$reg_exp_select,
             input$reg_resp_select, regress = TRUE)
    }
  }, ignoreInit = TRUE)
  
  output$output_plot_reg <- renderPlot(lm_out1()$plot)
  
  output$output_text_reg <- renderPrint(lm_out2()$refout)
  
  
  ###---
  # DATA CREATION/MODIFICATION ----
  ###---
  # Nominal functions
  
  # Seasonal ID
  seasonalData <- reactive({
    if (is.null(input$seasonal.dat)) return(NULL)
    
    type <- sub('.*\\.', '', input$seasonal.dat$name)
    
    if (type == 'shp') type <- 'shape' 
    else if (type == 'RData') type <- 'R' 
    else type <- type
    
    g <- read_dat(input$seasonal.dat$datapath, type)
    return(g)
  })
  
  output$unique_col_id <- renderUI({
    tagList(
      selectInput('unique_identifier','Variables that identify unique observations',
                  choices=colnames(values$dataset), multiple=TRUE, selectize=TRUE),
      selectInput('ID_type', "Select ID column class type",
                  choices = c("string", "integer")))
  })
  
  
  output$sp_col.select <- renderUI({ 
    
    selectInput('sp_col', "Column containing species names in table containing seasonal data", 
                choices=names(seasonalData()), multiple = FALSE, selectize=TRUE)
  })
  
  # Arithmetic functions 
  output$var_xy_select <- renderUI({
    
    tagList(
      selectInput('var_x', 'First variable. Will be the numerator if dividing.', 
                  choices = numeric_cols(values$dataset), selectize = TRUE),
      selectInput('var_y', 'Second variable. Will be the denomenator if dividing.',  
                  choices = numeric_cols(values$dataset), selectize = TRUE))
  })
  
  #Scale catch variable
  output$scale_catch_select <- renderUI({
    
    tagList(
      selectInput('scale_var_x', 'Select the catch variable', 
                  choices = numeric_cols(values$dataset), selectize = TRUE),
      selectInput('scale_method', 'Arithmetic expression', 
                  choices=c('addition', 'subtraction', 'multiplication', 'division'), selected='division'),
      numericInput('scale_val', 'Insert number to scale catch variable by', 10))
  })
  
  # CPUE
  output$input_cpue <- renderUI({
    tagList(
      selectizeInput('xWeight', 'Weight variable', 
                     choices = numeric_cols(values$dataset),
                     options = list(maxItem = 1, create = TRUE, 
                                    placeholder = 'Select or type variable name'),
                     multiple = TRUE),
      
      selectizeInput('xTime', 'Duration', 
                     choices = numeric_cols(values$dataset), 
                     options = list(create = TRUE, 
                                    placeholder = 'Select or type variable name')),
      
      selectizeInput('rpueprice', "Price variable", choices = numeric_cols(values$dataset), 
                     options = list(maxItem = 1, create = TRUE, 
                                    placeholder = 'Select or type variable name'),
                     multiple = TRUE)
    )
  })
  
  # Temporal functions
  output$trans_time_out <- renderUI({
    
    selectizeInput('TimeVar','Select variable',
                   choices = find_datetime(values$dataset), 
                   options = list(create=TRUE, placeholder = 'Select or type input'))                            
  })
  
  output$input_dur <- renderUI({
    tagList(
      
      selectizeInput('dur_start', 'Variable indicating start of time period', 
                     choices = find_duration(values$dataset), 
                     options = list(create = TRUE, placeholder='Select or type variable name')),
      
      selectizeInput('dur_end', 'Variable indicating end of time period', 
                     choices = find_duration(values$dataset),
                     options = list(create = TRUE, placeholder='Select or type variable name')))
  })
  
  
  #Transformations 
  
  output$trans_quant_name <-  renderUI({
    
    selectInput('trans_var_name','Select variable', choices = numeric_cols(values$dataset),
                multiple=FALSE, selectize=TRUE)
  })
  
  output$grp_perc <- renderUI({
    conditionalPanel("input.VarCreateTop=='Data transformations'&&input.trans=='group_perc'",
                     style = "margin-left:19px;", 
                     selectInput('perc_id_grp', 'Select primary grouping variable(s)',
                                 choices = colnames(values$dataset), multiple = TRUE),
                     selectInput('perc_grp', 'Select secondary grouping variable(s)',
                                 choices = colnames(values$dataset), multiple = TRUE),
                     selectInput('perc_value', 'Select numeric variable',
                                 choices = numeric_cols(values$dataset)),
                     checkboxInput('perc_id_col', 'Create an ID variable'),
                     checkboxInput('perc_drop', 'Drop total columns'))
  })
  
  output$grp_diff <- renderUI({
    conditionalPanel("input.VarCreateTop=='Data transformations'&&input.trans=='group_diff'",
                     style = "margin-left:19px;", 
                     selectInput('diff_sort', 'Sort table by',
                                 choices = date_select(values$dataset)),
                     selectInput('diff_grp', 'Select secondary grouping variable(s)',
                                 choices = colnames(values$dataset), multiple = TRUE),
                     selectInput('diff_value', 'Select numeric variable',
                                 choices = numeric_cols(values$dataset)),
                     checkboxInput('diff_id_col', 'Create an ID variable'),
                     checkboxInput('diff_drop', 'Drop total columns'))
  })
  
  output$grp_cumsum <- renderUI({
    conditionalPanel("input.VarCreateTop=='Data transformations'&&input.trans=='group_cumsum'",
                     style = "margin-left:19px;", 
                     selectInput('cumsum_sort', 'Sort table by',
                                 choices = date_select(values$dataset)),
                     selectInput('cumsum_grp', 'Select secondary grouping variable(s)',
                                 choices = colnames(values$dataset), multiple = TRUE),
                     selectInput('cumsum_value', 'Select numeric variable',
                                 choices = numeric_cols(values$dataset)),
                     checkboxInput('cumsum_id_col', 'Create an ID variable'),
                     checkboxInput('cumsum_drop', 'Drop total columns'))
  })
  
  # Dummy variable
  output$dummy_select <- renderUI({
    tagList(
      conditionalPanel("input.dummyfunc=='From variable'",
                       selectizeInput('dummyvarfunc','Select variable', multiple = TRUE, 
                                      choices = names(values$dataset), options = list(maxItems = 1))),
      
      conditionalPanel("input.dummyfunc=='From policy dates'",
                       selectInput('dummypolyfunc','Select policy', multiple=FALSE, 
                                   choices=c('User defined', 'Central GOA Rockfish Cooperative (2004)'='Rockfish', 
                                             'Amendment 80 Alaska (2008)'='Amen80', 
                                             "Pacific halibut and Sablefish IFQ Program	Alaska (1993)"="halsab", 
                                             "American Fisheries Act Pollock Program Alaska (1999)"="AFA")),
                       
                       conditionalPanel("input.dummypolyfunc=='User defined'", 
                                        textInput('polyear','Policy year', placeholder='Write policy year here')),
                       
                       selectInput('dummypolydate','Select date variable', multiple=FALSE, 
                                   choices=date_cols(values$dataset))),
      
      conditionalPanel("input.dummyfunc=='From area closures'", 
                       selectInput('dummclosfunc','Select Variable', multiple=FALSE, choices=c())) # Not used 
    )
  }) 
  
  dum_pol_year <- reactive({
    if(input$dummypolyfunc=='User defined') {
      return(input$polyear)
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
    
    if (input$VarCreateTop=='Dummy variables') {
      
      if (input$dummyfunc == 'From variable') {
        
        if (!is.null(input$dummyvarfunc)) {
          
          if(grepl('dat|year', input$dummyvarfunc, ignore.case=TRUE)) { 
            out <- 'date'
          } else if (is.numeric(values$dataset[[input$dummyvarfunc]])) { 
            out <- 'num'
          } else { 
            out <- 'other'
          }   
        } 
      }
    }
  })
  
  dum_range <- reactive({
    
    list(mean = mean(values$dataset[[input$dummyvarfunc]], na.rm=TRUE),
         min = min(values$dataset[[input$dummyvarfunc]], na.rm=TRUE),
         max = max(values$dataset[[input$dummyvarfunc]], na.rm=TRUE))
  })
  
  dum_year <- reactive({
    
    out <- lubridate::as_date(values$dataset[[input$dummyvarfunc]])
    unique(lubridate::year(out))
  })
  
  output$dummy_sub <- renderUI({
    
    if (!is.null(dum_temp())) {
      
      if (dum_temp()=='date') {
        tagList(
          selectInput("dumsubselect", 'Set dummy variable based on', 
                      choices=c('selected year(s) vs. all other years'='x_y',
                                'before vs. after'='more_less')),
          selectInput("select.val", 'Select year(s)', 
                      choices = dum_year(), multiple = TRUE))
        
      } else if (dum_temp()=='num') {
        
        tagList(
          selectInput("dumsubselect", 'Set dummy variable based on',
                      choices=c('selected value(s) vs all other values'='x_y',
                                'less than vs more than'='more_less'), selected='more_less'),
          sliderInput("select.val", 'Select single or range of values to set to zero', 
                      min = dum_range()$min, max = dum_range()$max, 
                      value = c(dum_range()$mean - dum_range()$mean/10, 
                                dum_range()$mean + dum_range()$mean/10))
        )
        
      } else if (dum_temp()=='other') {
        
        selectizeInput("select.val", 'Select categories to set to zero', 
                       choices=unique(values$dataset[[input$dummyvarfunc]]), 
                       multiple=TRUE)
      }
    }
  }) 
  
  
  # Spatial functions
  # Zone 
  output$zone_assign_1 <- renderUI({
    
    tagList(
      if (names(spatdat$dataset)[1]=='var1') {
        
        tags$div(h4('Spatial data file not loaded. Please load on Upload Data tab', style="color:red"))
      },
      
      selectizeInput('lat_dat_zone', 'Latitude from data', choices = find_lat(values$dataset),
                     options = list(create = TRUE, placeholder = 'Select or type LATITUDE variable name')),
      selectizeInput('lon_dat_zone', 'Longitude from data', choices = find_lon(values$dataset),
                     options = list(create = TRUE, placeholder='Select or type LONGITUDE variable name')),
      
      selectInput('cat_zone', 'Individual areas/zones from the spatial data file', 
                  choices = names(as.data.frame(spatdat$dataset))),
      checkboxInput('hull_polygon_zone', 'Use convex hull method to create polygon?', value = FALSE),
      checkboxInput('closest_pt_zone', 'Use closest polygon to point?', value = FALSE) 
    ) 
  })  
  
  output$zone_assign_2 <- renderUI({
    
    if (!inherits(spatdat$dataset, "sf")) {
      tagList(
        selectizeInput('lat_grid_zone', 'Latitude from spatial data', 
                       choices=names(as.data.frame(spatdat$dataset)), multiple = TRUE,
                       options = list(create = TRUE, maxItem = 1, 
                                      placeholder = 'Select or type LONGITUDE variable name')),
        
        selectizeInput('lon_grid_zone',  'Longitude from spatial data', 
                       choices=names(as.data.frame(spatdat$dataset)), multiple = TRUE,
                       options = list(create = TRUE, maxItem = 1, 
                                      placeholder = 'Select or type LONGITUDE variable name'))
      )
    } 
  })
  
  # Zonal Centroid UI
  output$zone_cent_ui <- renderUI({
    tagList(
      
      if (names(spatdat$dataset)[1]=='var1') {
        
        return(
          tags$div(h4('Spatial data file not loaded. Please load on Upload Data tab', style="color:red"))
        )
        
      },
      
      selectInput('zone_cent_spatID', 'Select zone ID from spatial data',
                  choices = names(spatdat$dataset)),
      
      textInput('zone_cent_name', 'Name for new centroid table', 
                placeholder = 'Ex: NMFSAreas'),
      
      checkboxInput('zone_cent_join', 'Join zonal centroids to primary data',
                    value = FALSE),
      
      conditionalPanel(condition = 'input.zone_cent_join',
                       
                       selectInput('zone_cent_zoneID', 'Select zonal ID from primary data',
                                   choices = names(values$dataset))
                       
      ) 
      
    )
    
  })
  
  
  # Fishing centroid UI
  output$fish_weight_cent <- renderUI({
    
    tagList(
      textInput('cat_cent', 'Zone identifier in main data file or spatial data file', 
                value='ZoneID'),
      selectInput('weight_var_cent', 'Weighting variable', 
                  choices=c('none'="", colnames(values$dataset))),
      tags$b('Select latitude then longitude from primary data frame for assigning observations to zones'),
      div(style="display: inline-block;vertical-align:top; width: 200px;",
          selectizeInput('lat_dat_cent', '', choices = find_lat(values$dataset),
                         options = list(create = TRUE, placeholder='Select or type LATITUDE variable name'))),
      div(style="display: inline-block;vertical-align:top; width: 200px;",
          selectizeInput('lon_dat_cent', '', choices = find_lon(values$dataset),
                         options = list(create = TRUE, placeholder='Select or type LONGITUDE variable name')))
    )
  })
  
  output$fish_weight_cent_2 <- renderUI({
    
    if (!is.null(input$cat_cent)) {
      if (!(input$cat_cent %in% colnames(values$dataset))) {
        if (names(spatdat$dataset)[1]=='var1') {
          
          tags$div(h4('Spatial data file not loaded. Please load on Upload Data tab if required', 
                      style="color:red"))
        }
      }
    }
  })
  
  output$fish_weight_cent_3 <- renderUI({
    
    if (!('sf' %in% class(spatdat$dataset))) {
      tagList(
        h5(tags$b('Select vector containing latitude then longitude from spatial data file')),
        div(style="display: inline-block;vertical-align:top; width: 200px;",
            selectizeInput('lat_grid_cent', '', choices=names(as.data.frame(spatdat$dataset)), multiple=TRUE)),
        div(style="display: inline-block;vertical-align:top; width: 200px;",
            selectizeInput('lon_grid_cent',  '', choices=names(as.data.frame(spatdat$dataset)), multiple=TRUE)))
    }
  })
  
  # Distance between obs
  output$dist_between_input <- renderUI({
    
    tagList(
      if (names(spatdat$dataset)[1]=='var1'){
        tags$div(h4('Spatial data file not loaded. Please load on Upload Data tab', 
                    style="color:red"))    
      },
      
      selectInput('start', 'Starting location',
                  choices = c('Zonal centroid', 'Port', 'Lat/Lon coordinates')),
      
      selectInput('end', 'Ending location', 
                  choices = c('Zonal centroid', 'Port', 'Lat/Lon coordinates')),
      #Port
      conditionalPanel("input.start=='Port'||input.end=='Port'", style = "margin-left:19px;", 
                       selectInput("filePort", "Choose file from the FishSET database containing port data",
                                   choices = list_tables(project$name, "port"), multiple = FALSE)),
      
      conditionalPanel("input.start=='Port'", style = "margin-left:19px;", 
                       selectInput('port_start', 'Variable containing port name at starting location', 
                                   choices=find_port(values$dataset), selectize=TRUE)),
      
      conditionalPanel("input.end=='Port'",style = "margin-left:19px;", 
                       selectInput('port_end', 'Variable containing port name at ending location', 
                                   choices=find_port(values$dataset), selectize=TRUE)),
      
      #Zonal coords
      conditionalPanel("input.start=='Lat/Lon coordinates'", style = "margin-left:19px;", 
                       selectizeInput('start_latlon', 'Select lat then lon for starting location', 
                                      choices = find_lonlat(values$dataset), 
                                      multiple=TRUE, options = list(maxItems = 2, create = TRUE, 
                                                                    placeholder='Select or type variable name'))),
      
      conditionalPanel("input.end=='Lat/Lon coordinates'", style = "margin-left:19px;", 
                       selectizeInput('end_latlon', 'Select lat then lon for ending location', 
                                      choices=find_lonlat(values$dataset),
                                      multiple=TRUE, options = list(maxItems = 2, create = TRUE,
                                                                    placeholder='Select or type variable name')))
    ) 
  })   
  
  
  output$dist_betwn_opts <- renderUI({
    
    conditionalPanel("input.start=='Zonal centroid'||input.end=='Zonal centroid'",
                     style = "margin-left:19px;",  
                     
                     selectInput('zone_dist', 'Zone/area assignment variable (if exists in data)', choices=c('', 'ZoneID', colnames(values$dataset))),
                     
                     selectizeInput('lon_dat', 'Select lat then lon columns from dataframe to assign observations to zone', 
                                    choices = find_lonlat(values$dataset),
                                    multiple=TRUE, options = list(maxItems = 2, create = TRUE, 
                                                                  placeholder='Select or type variable name')),
                     
                     selectInput('cat', 'Individual areas/zones from the spatial data drame', 
                                 choices=names(as.data.frame(spatdat$dataset))),
                     
                     if (!inherits(spatdat$dataset, "sf")) {
                       selectizeInput('long_grid', 'Select vector containing latitude then longitude from spatial data file',
                                      choices=names(as.data.frame(spatdat$dataset)), multiple=TRUE, 
                                      options = list(maxItems = 2,create = TRUE, 
                                                     placeholder='Select or type variable name'))
                     }
    )
  })
  
  output$mid_haul_input <- renderUI({
    tagList(
      selectizeInput('mid_start','Select Lat then Lon that define starting locations',multiple = TRUE,
                     choices = find_lonlat(values$dataset), 
                     options = list(maxItems = 2, create = TRUE, 
                                    placeholder='Select or type variable name')),
      selectizeInput('mid_end','Select Lat then Lon that define ending locations',multiple = TRUE,
                     choices = find_lonlat(values$dataset),
                     options = list(maxItems = 2, create = TRUE, 
                                    placeholder='Select or type variable name')))
  })
  
  # Starting loc
  output$input_startingloc <- renderUI({
    tagList(
      
      if (names(spatdat$dataset)[1]=='var1') {
        tags$div(h4('Spatial data file not loaded. Please load on Upload Data tab', 
                    style="color:red"))
      },
      
      tags$div(style = "margin-left:19px;",
               
               selectInput('trip_id_SL', 'Variable that identifies unique trips', 
                           choices=c('', names(values$dataset)), selectize=TRUE),
               
               selectInput('haul_order_SL', 'Variable defining haul order within a trip. Can be time, coded variable, etc.',
                           choices=c('', names(values$dataset)), selectize=TRUE),
               selectInput('zone_SL', 'Zone/area assignment variable (if exists in data)', 
                           choices=c("", 'ZoneID', colnames(values$dataset)), selected=''),
               
               selectizeInput('starting_port_SL',  "Variable that identifies port at start of trip", 
                              choices = find_port(values$dataset), 
                              options = list(create = TRUE, placeholder='Select or type variable name')),
               
               selectizeInput('lon_dat_SL', "Longitude variable in primary data table", 
                              choices= find_lon(values$dataset), 
                              options = list(create = TRUE, placeholder='Select or type variable name')), 
               
               selectizeInput('lat_dat_SL', "Latitude variable in primary data table",
                              choices= find_lat(values$dataset),
                              options = list(create = TRUE, placeholder='Select or type variable name')),
               
               selectInput("port.dat", "Choose port table from the FishSET database",
                           choices = list_tables(project$name, "port"), 
                           multiple = FALSE),
               
               if (!('sf' %in% class(spatdat$dataset))) {
                 
                 tagList(
                   selectInput('lat_grid_SL', 'Select vector containing latitude from spatial data file', 
                               choices= names(as.data.frame(spatdat$dataset)), multiple=TRUE),
                   selectInput('lon_grid_SL', 'Select vector containing longitude from spatial data file', 
                               choices= names(as.data.frame(spatdat$dataset)), multiple=TRUE, selectize=TRUE))
               },
               
               selectInput('cat_SL', "Property from spatial data file that identifies zones or areas names", 
                           choices = names(as.data.frame(spatdat$dataset)))    
      )
    )
  })
  
  
  # Trip-level functions
  
  # haul to trip
  output$input_IDVAR <- renderUI({
    selectInput("Haul_Trip_IDVar", "Variable(s) that define unique trips",
                choices = names(values$dataset), multiple=TRUE)
  })
  
  # Trip distance
  output$input_trip_dist_vars <- renderUI({
    tagList(
      selectInput("port_dat_dist", "Choose port table from the FishSET database", 
                  choices = list_tables(project$name, "port"), multiple = FALSE),
      
      selectInput('trip_ID','Variable that identifies unique trips', 
                  choices=names(values$dataset), multiple = FALSE),
      
      selectInput('starting_port','Variable that identifies port at START of trip', 
                  multiple = FALSE, choices = find_port(values$dataset), selectize=TRUE),
      
      selectizeInput('starting_haul','Select LAT then LON variables at START of haul', 
                     multiple = TRUE, choices = find_lat(values$dataset), 
                     options = list(maxItems = 2,create = TRUE, 
                                    placeholder='Select or type variable name')),
      
      selectizeInput('ending_haul','Select LAT then LON variables at END of haul', 
                     multiple = TRUE, choices = find_lon(values$dataset),
                     options = list(maxItems = 2, create = TRUE, 
                                    placeholder='Select or type variable name')),
      
      selectizeInput('ending_port','Variables that identifies port at END of trip', 
                     multiple = FALSE, choices = find_port(values$dataset),
                     options = list(create = TRUE, placeholder='Select or type variable name')),
      
      selectizeInput('haul_order','Variables that identifies the order of hauls within a trip.',
                     choices=names(values$dataset), multiple = FALSE)
    )
  })
  
  # Trip centroid
  output$input_tri_cent <-  renderUI({
    tagList(
      selectizeInput('trip_cent_lon','Column name containing longitudinal data', 
                     choices = find_lon(values$dataset), multiple = FALSE,  
                     options = list(create = TRUE, placeholder='Select or type variable name')),
      
      selectizeInput('trip_cent_lat', 'Column name containing latitudinal data', 
                     choices =find_lat(values$dataset), multiple = FALSE,  
                     options = list(create = TRUE, placeholder='Select or type variable name')),
      
      selectInput('trip_cent_weight','Variable for weighted average', multiple = FALSE, 
                  choices=c('', names(values$dataset)), selected='', selectize=TRUE),
      
      selectizeInput('trip_cent_id','Variable(s) that identify the individual trip', 
                     choices = c('', names(values$dataset)), selected='', multiple = TRUE)
    )
  })
  
  
  
  # Run data creation function 
  observeEvent(input$runNew, {
    
    output_except <- FALSE # for create_centroid side-effect (otherwise error)
    
    if (input$VarCreateTop == 'Dummy variables' & input$dummyfunc == 'From policy dates') {
      
      q_test <- quietly_test(dummy_num)
      output <- q_test(values$dataset, project = project$name, var = input$dummypolydate, 
                       value = dum_pol_year(), opts = 'more_less', name = input$varname)
      notif <- "Policy dummy variable"
      
    } else if (input$VarCreateTop == 'Dummy variables' & input$dummyfunc == 'From variable') {
      
      q_test <- quietly_test(dummy_num)
      output <- q_test(values$dataset, project = project$name, var = input$dummyvarfunc, 
                       value = input$select.val, opts = input$dumsubselect, name = input$varname)
      notif <- "Dummy variable"
      
    } else if (input$VarCreateTop == 'Temporal functions' & input$tempfunc == 'temp_mod') {
      
      q_test <- quietly_test(temporal_mod)
      output <- q_test(values$dataset, project = project$name, x = input$TimeVar, 
                       define.format = input$define_format, timezone = input$timezone, 
                       name = input$varname)
      notif <- "Temporal modification"
      
    } else if (input$VarCreateTop == 'Data transformations' & input$trans == 'set_quants') {
      
      q_test <- quietly_test(set_quants)
      output <- q_test(values$dataset, project = project$name, x = input$trans_var_name, 
                       quant.cat = input$quant_cat, name = input$varname)
      notif <- "Quant variable"
      
    } else if (input$VarCreateTop == 'Nominal ID' & (input$ID == 'ID_var' | input$ID == 'ID_seq_var')) {
      
      if(input$ID == 'ID_var'){
        vars_in <- input$unique_identifier
      } else {
        vars_in <- NULL
      }
      
      q_test <- quietly_test(ID_var)
      output <- q_test(values$dataset, project = project$name, name = input$varname, 
                       vars = vars_in, type = input$ID_type)
      notif <- "Nominal ID"
      
    } else if (input$VarCreateTop == 'Nominal ID' & input$ID == 'binary_seasonID') {      
      
      q_test <- quietly_test(seasonalID)
      output <- q_test(values$dataset, project = project$name, seasonal.dat = seasonalData(), 
                       start = input$seasonstart, end = input$seasonend, overlap = input$overlap, 
                       name = input$varname)
      notif <- "Binary seasonal ID"
      
    } else if (input$VarCreateTop == 'Nominal ID' & input$ID == 'create_seasonal_ID') {
      
      q_test <- quietly_test(create_seasonal_ID)
      output <- q_test(values$dataset, project = project$name, seasonal.dat = seasonalData(), 
                       use.location = input$use_location, use.geartype = input$use_geartype, 
                       sp.col = input$sp_col, target = input$target)
      notif <- "Seasonal ID"
      
    } else if (input$VarCreateTop == 'Data transformations' & input$trans == 'group_perc') {
      
      q_test <- quietly_test(group_perc)
      output <- q_test(values$dataset, project = project$name, id_group = input$perc_id_grp,
                       group = input$perc_grp, value = input$perc_value, name = input$varname, 
                       create_group_ID = input$perc_id_col, drop_total_col = input$perc_drop)
      notif <- "Grouped percentage"
      
    } else if (input$VarCreateTop == 'Data transformations' & input$trans == 'group_diff') {
      
      q_test <- quietly_test(group_diff)
      output <- q_test(values$dataset, project = project$name, group = input$diff_grp,  
                       sort_by = input$diff_sort, value = input$diff_value, name = input$varname, 
                       create_group_ID = input$diff_id_col, drop_total_col = input$diff_drop)
      notif <- "Grouped difference"
      
    } else if (input$VarCreateTop == 'Data transformations' & input$trans == 'group_cumsum') {
      
      q_test <- quietly_test(group_cumsum)
      output <- q_test(values$dataset, project = project$name, group = input$cumsum_grp,  
                       sort_by = input$cumsum_sort, value = input$cumsum_value, 
                       name = input$varname, create_group_ID = input$cumsum_id_col, 
                       drop_total_col = input$cumsum_drop)
      notif <- "Grouped cumulative sum"
      
    } else if (input$VarCreateTop == 'Arithmetic functions' & (input$numfunc == 'create_var_num' | input$numfunc == 'scale_catch')) {
      
      if(input$numfunc == 'create_var_num'){
        x_in <- input$var_x
        y_in <- input$var_y
        method_in <- input$create_method
      } else {
        x_in <- input$scale_var_x
        y_in <- input$scale_val
        method_in <- input$scale_method
      }
      
      q_test <- quietly_test(create_var_num)
      output <- q_test(values$dataset, project = project$name, x = x_in, 
                       y = y_in, method = method_in, name = input$varname)
      notif <- "Arithemtic variable"
      
    } else if (input$VarCreateTop == 'Arithmetic functions' & input$numfunc == 'cpue') {
      
      q_test <- quietly_test(cpue)
      output <- q_test(values$dataset, project = project$name, xWeight = input$xWeight,
                       xTime = input$xTime, price = input$rpueprice, name = input$varname)
      notif <- "CPUE variable"
      
    } else if (input$VarCreateTop == 'Spatial functions' & input$dist == 'zone') {
      
      q_test <- quietly_test(assignment_column)
      output <-  q_test(dat = values$dataset, project$name, spat = spatdat$dataset, 
                        lon.dat = input$lon_dat_zone, lat.dat = input$lat_dat_zone, 
                        cat = input$cat_zone, name = input$varname, closest.pt = input$closest_pt_zone, 
                        lon.spat = input$lon_grid_zone, lat.spat = input$lat_grid_zone, 
                        hull.polygon = input$hull_polygon_zone, epsg = NULL)
      notif <- "Zone assignment column"
      
    } else if (input$VarCreateTop == 'Spatial functions' & input$dist == 'zone_cent') {
      
      q_test <- quietly_test(create_centroid, show_msg = TRUE)
      
      if (input$zone_cent_join) {
        # create centroid table, join centroids to primary data
        output <- q_test(dat = values$dataset, spat = spatdat$dataset, project = project$name,
                         spatID = input$zone_cent_spatID, zoneID = input$zone_cent_zoneID,
                         cent.name = input$zone_cent_name, output = 'dataset')
      } else {
        # save centroid table, don't join to primary data
        q_test(project = project$name, spat = spatdat$dataset, spatID = input$zone_cent_spatID, 
               cent.name = input$zone_cent_name, output = 'centroid table')
        output <- NULL; output_except <- TRUE;
      }
      
      notif <- 'Zonal centroid'
      
    } else if (input$VarCreateTop == 'Spatial functions' & input$dist == 'fish_cent') {
      # TODO: update to use create_centroid -- fishing centroid
      q_test <- quietly_test(find_fishing_centroid)
      output <- q_test(dat = values$dataset, project$name, spat = spatdat$dataset, 
                       lon.dat = input$lon_dat_cent, lat.dat = input$lat_dat_cent, 
                       cat = input$cat_cent, weight.var = input$weight_var_cent,
                       lon.spat = input$lon_grid_cent,lat.spat = input$lat_grid_cent)
      notif <- "Fishing centroid"
      
    } else if (input$VarCreateTop == 'Spatial functions' & input$dist == 'create_dist_between') {
      
      if (input$start == 'Lat/Lon coordinates') startdist <-input$start_latlon
      else if (input$start == 'Port')           startdist <- input$port_start
      else                                      startdist <- 'centroid'
      
      if (input$end=='Lat/Lon coordinates') enddist <-input$end_latlon
      else if (input$end=='Port') enddist <- input$port_end
      else enddist <- 'centroid'
      
      q_test <- quietly_test(create_dist_between_for_gui)
      output <-  q_test(values$dataset, project = project$name, start = startdist, 
                        end = enddist, units = input$units, name = input$varname, 
                        portTable = input$filePort, spat = spatdat$dataset,
                        zoneid = input$zone_dist, lon.dat = input$lon_dat[2], 
                        at.dat = input$lon_dat[1], cat = input$cat, lon.spat = input$long_grid[2], 
                        lat.spat = input$long_grid[1])
      notif <- "Point distance"
      
    } else if (input$VarCreateTop == 'Spatial functions' & input$dist == 'create_mid_haul') {
      
      q_test <- quietly_test(create_mid_haul)
      output <-  q_test(values$dataset, project = project$name, 
                        start = c(input$mid_start[2], input$mid_start[1]), 
                        end = c(input$mid_end[2], input$mid_end[1]), name = input$varname)
      notif <- "Haul midpoint"
      
    } else if (input$VarCreateTop == 'Temporal functions' & input$tempfunc == 'create_duration') {
      
      q_test <- quietly_test(create_duration)
      output <-  q_test(values$dataset, project = project$name, start = input$dur_start, 
                        end = input$dur_end, units = input$dur_units, name = input$varname)
      notif <- "Duration variable"
      
    } else if (input$VarCreateTop == 'Spatial functions' & input$dist == 'create_startingloc') {
      
      q_test <- quietly_test(create_startingloc)
      output <-  q_test(values$dataset, project = project$name, spat = spatdat$dataset, 
                        portTable = input$port.dat, trip_id = input$trip_id_SL,
                        haul_order = input$haul_order_SL, starting_port = input$starting_port_SL, 
                        lon.dat = input$lon_dat_SL, lat.dat = input$lat_dat_SL, 
                        cat = input$cat_SL, zoneid = input$zone_SL, name = input$varname, 
                        lon.spat = input$lon_grid_SL, lat.spat = input$lat_grid_SL)
      notif <- "Starting location"
      
    } else if (input$VarCreateTop == 'Trip-level functions' & input$trip == 'haul_to_trip') {
      
      q_test <- quietly_test(haul_to_trip)
      output <-  q_test(values$dataset, project=project$name, 
                        input$fun_numeric, input$fun_time, input$Haul_Trip_IDVar)
      notif <- "Haul-to-trip"
      
    } else if (input$VarCreateTop == 'Trip-level functions' & input$trip == 'trip_distance') {
      
      q_test <- quietly_test(create_trip_distance)
      output <-  q_test(values$dataset, project = project$name, 
                        PortTable = input$port_dat_dist, trip_id = input$trip_ID, 
                        staring_port = input$starting_port, 
                        starting_haul = c(input$starting_haul[2], input$starting_haul[1]), 
                        ending_haul = c(input$ending_haul[2],input$ending_haul[1]), 
                        ending_port = input$ending_port, 
                        haul_order = input$haul_order, name = input$varname)
      notif <- "Trip distance"
      
    } else if (input$VarCreateTop == 'Trip-level functions' & input$trip == 'trip_centroid') {
      
      q_test <- quietly_test(create_trip_centroid)
      output <-  q_test(values$dataset, project = project$name, 
                        lon = input$trip_cent_lon, lat=input$trip_cent_lat, 
                        tripID = input$trip_cent_id, weight.var = input$trip_cent_weight)
      notif <- "Trip centroid"
    }
    
    if (!is_value_empty(output)) {
      
      values$dataset <- output
      showNotification(paste(notif, "was successfully created."), type = "message")
      
    } else {
      
      if (!output_except) {
        
        showNotification(paste(notif, "could not be created."), type = "error")
      }
    }
  })
  
  output$output_table_create <- DT::renderDT(
    if(colnames(values$dataset)[1] == 'var1') {
      return(NULL)
    } else {
      head(values$dataset)
    }
  )
  
  
  # --- 
  # MAP VIEWER ----
  # --- 
  map_viewer_serv("map", values, spatdat, reactive(project$name))
  
  
  # ---
  # ALTERNATIVE CHOICE ----
  # ---
  
  output$altc_ui <- renderUI({
    tagList(
      
      h4(tags$b('Define how alternative fishing choices are calculated between occurrence and alternative location')),
      
      fluidRow(
        column(5,
               add_prompter(
                 
                 selectizeInput('altc_occasion', 'occurrence:', 
                                choices=c('Centroid of zonal assignment' = 'zone',
                                          'Fishing centroid' = 'fish', 'Port' = 'port', 
                                          'Lon-Lat Coordinates' = 'lon-lat')),
                 
                 message = 'A centroid table must be saved to the FishSET database to be used as trip/haul occurances',
                 type = 'info', size = 'large', position = 'bottom'),
               
               conditionalPanel("input.altc_occasion=='zone'||input.altc_alt_var=='zone'",
                                uiOutput('altc_zone_cent_ui')),
               
               selectizeInput('altc_zoneID', 'Column containing zone identifier', 
                              choices = colnames(values$dataset), options = list(maxItems = 1), 
                              multiple = TRUE)
        ),
        
        column(5, 
               add_prompter(
                 
                 selectizeInput('altc_alt_var', 'alternative location:', 
                                
                                choices=c('Centroid of zonal assignment' = 'zone',
                                          'Fishing centroid' = 'fish', 
                                          'Nearest Point' = 'near')),
                 
                 message = 'A centroid table must be saved to the FishSET database to be used as trip/haul occurances',
                 type = 'info', size = 'large', position = 'bottom'),
               
               selectizeInput('altc_dist','Distance units', choices = c('miles','kilometers','meters'), 
                              selected = 'miles'),
               
               numericInput('altc_min_haul', 'Include zones with more observations than', 
                            min = 1, max = 1000, value = 1)
        )
      ),     
      
      conditionalPanel("input.altc_occasion=='fish'||input.altc_alt_var=='fish'",
                       
                       uiOutput('altc_fish_cent_ui')),
      
      conditionalPanel("input.altc_alt_var=='near'", 
                       
                       uiOutput('altc_spat_ui')),
      
      uiOutput('altc_occ_var_ui')
    )
  })
  
  # tracking/updating zonal and fishing centroid tables
  altc <- reactiveValues(zone_cent = NULL, fish_cent = NULL)
  
  
  output$altc_occ_var_ui <- renderUI({
    
    if (input$altc_occasion == 'lon-lat') {
      
      tagList(
        h5(tags$em('Longitude must be specified before latitude.')),
        
        selectizeInput('altc_occ_var', 'Choose longitude and latitude occasion columns', 
                       choices = find_lonlat(values$dataset), 
                       options = list(maxItems = 2, create = TRUE, 
                                      placeholder = 'Select or type variable name'),
                       multiple = TRUE)
      )
      
    } else {
      
      selectInput('altc_occ_var', 'Choose occasion ID variable',
                  choices = colnames(values$dataset))
    }
    
  })
  
  output$altc_zone_cent_ui <- renderUI({
    
    if (is_value_empty(altc$zone_cent)) {
      
      return(h5('No zonal centroid tables exist. Create centroid tables on the Compute New Variables tab.', style="color:red"))
    }
    
    selectInput('altc_zone_cent', 'Select a zonal centroid table',
                choices = altc$zone_cent)
  })
  
  output$altc_fish_cent_ui <- renderUI({
    
    fish_cent_tabs <- 
      suppressWarnings(grep('FishCentroid$', list_tables(project$name, 'centroid'), value = TRUE))
    
    if (is_value_empty(altc$fish_cent)) {
      
      return(h5('No fishing centroid tables exist. Create centroid tables on the Compute New Variables tab.', style="color:red"))
    }
    
    selectInput('altc_fish_cent', 'Select a fishing centroid table',
                choices = altc$fish_cent)
  })
  
  # update zone/fish centroid tab list when alt choice tab is selected
  observeEvent(input$tabs == 'altc', { 
    
    altc$zone_cent <- suppressWarnings(grep('ZoneCentroid$', list_tables(project$name, 'centroid'), value = TRUE))
    altc$fish_cent <- suppressWarnings(grep('FishCentroid$', list_tables(project$name, 'centroid'), value = TRUE))
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  output$altc_spat_ui <- renderUI({
    
    if (names(spatdat$dataset)[1]=='var1') {
      
      return(h5('Spatial data file not loaded. Please load on Upload Data tab.', style="color:red"))
    }
    
    selectInput('altc_spatID', "Select zone ID column from spatial dataset",
                choices = colnames(spatdat$dataset))
  })
  
  output$zoneIDText <- renderText({
    
    if (!is_value_empty(input$altc_zoneID)) {
      
      temp <- data.frame(table(values$dataset[[input$altc_zoneID]]))
      paste('number of records:',
            dim(values$dataset[which(values$dataset[[input$altc_zoneID]] %in% 
                                       temp[which(temp$Freq > input$altc_min_haul),1]), ])[1], 
            '\nnumber of zones:', nrow(temp[which(temp$Freq > input$altc_min_haul),]))
    }
  })
  
  # zone freq table
  zone_freq <- reactive({
    
    req(input$altc_min_haul)
    req(input$altc_zoneID)
    
    freq_tab <- agg_helper(values$dataset, value = input$altc_zoneID, 
                           count = TRUE, fun = NULL)
    freq_tab$include <- freq_tab$n >= input$altc_min_haul
    freq_tab
  })
  
  # barplot of zone freq
  zoneIDNumbers_dat <- reactive({
    
    req(input$altc_zoneID)
    
    dat <- zone_freq()
    z_sym <- rlang::sym(input$altc_zoneID)
    
    ggplot2::ggplot(dat[dat$include, ]) +
      ggplot2::geom_col(ggplot2::aes(x=!!z_sym, y = n)) + 
      fishset_theme()
  })
  
  output$altc_zone_plot <- renderPlot(zoneIDNumbers_dat())
  
  # map showing which zones will be included
  output$zone_include_plot <- renderPlot({
    
    req(input$altc_spatID)
    req(input$altc_zoneID)
    
    join_by <- stats::setNames(input$altc_zoneID, input$altc_spatID)
    spat <- dplyr::left_join(spatdat$dataset[input$altc_spatID], zone_freq(), by = join_by)
    
    ggplot2::ggplot() +  
      ggplot2::geom_sf(data = spat, 
                       ggplot2::aes(fill = include), color = "black", alpha = .8) +
      ggplot2::scale_fill_manual(breaks = c(TRUE, FALSE), values=c('green', 'grey20')) + 
      fishset_theme()
  })
  
  # Save alternative choice 
  observeEvent(input$altc_save, {
    # switch to values that function accepts
    occ_type <- switch(input$altc_occasion, 'zone' = 'zonal centroid', 
                       'fish' = 'fishing centroid', 'port' = 'port', 'lon-lat' = 'lon-lat')
    
    alt_type <- switch(input$altc_alt_var, 'zone' = 'zonal centroid', 
                       'fish' = 'fishing centroid', 'near' = 'nearest point')
    
    q_test <- quietly_test(create_alternative_choice, show_msg = TRUE)
    
    q_test(dat=values$dataset, project=project$name, occasion=occ_type,
           occasion_var=input$altc_occ_var, alt_var=alt_type, 
           dist.unit=input$altc_dist, min.haul=input$altc_min_haul, 
           spat=spatdat$dataset, zoneID=input$altc_zoneID, spatID = input$altc_spatID,
           zone.cent.name=input$altc_zone_cent, fish.cent.name=input$altc_fish_cent)
    
  }, ignoreInit = FALSE) 
  
  
  # ---
  # EXPECTED CATCH ----     
  # ---
  exp_react <- reactiveValues(altc_exists = FALSE)
  
  # check if alt choice list exists when exp catch tab is selected
  observeEvent(input$tabs == 'expectedCatch', {
    
    exp_react$altc_exists <- length(suppressWarnings(list_tables(project$name, 'altc'))) > 0
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # check if alt choice list exists
  output$exp_altc_check <- renderUI({
    
    if (!exp_react$altc_exists) {
      
      tagList(
        
        tags$b(),
        h4(tags$b('Alternative choice matrix does not exist. Go to the ', 
                  'Define Alternative Fishing Choices tab.'), style='color: red;'))
    }
  })
  
  output$exp_ui <- renderUI({
    tagList(
      selectInput('exp_catch_var','Catch variable for averaging',
                  choices = numeric_cols(values$dataset)),
      
      selectizeInput('exp_price', 'If expected revenue is to be calculated, column name containing price or value data', 
                     choices = c("none", find_value(values$dataset)),
                     options = list(create = TRUE, placeholder='Select or type column name')),
      
      selectizeInput('exp_group','Choose column name containing data that defines groups',
                     choices=c('No group', category_cols(values$dataset))),
      # zoneID used for sparsity plots
      selectizeInput('exp_zoneID', 'Column containing zone identifier', choices = colnames(values$dataset),
                     options = list(maxItems = 1), multiple = TRUE)
    )
  })
  
  output$exp_temp_var_ui <-  renderUI({
    
    conditionalPanel(condition="input.exp_temporal!='Entire record of catch (no time)'",
                     style = "margin-left:19px;font-size: 12px", 
                     
                     selectizeInput('exp_temp_var', 'Column name containing temporal data for averaging', 
                                    choices=c('none', find_datetime(values$dataset)),
                                    selected='none', options = list(create = TRUE, placeholder='Select or type column name')))
  })
  
  # Sparsity table
  sparstable_dat <- reactive({
    
    req(input$exp_temp_var)
    req(input$exp_zoneID)
    
    if (!is_value_empty(input$exp_catch_var) & input$exp_temp_var!='none') {
      
      sparsetable(values$dataset, project=project$name, timevar=input$exp_temp_var, 
                  zonevar=input$exp_zoneID, var=input$exp_catch_var)
    }
  })
  
  output$spars_table <- DT::renderDT(sparstable_dat(), server=TRUE)
  
  # sparsity plot
  output$spars_plot <- renderPlot({
    
    req(input$exp_temp_var)
    req(input$exp_zoneID)
    
    if (!is_value_empty(input$exp_catch_var) & input$exp_temp_var!='none') {
      
      sparsplot(project = project$name, x = sparstable_dat())
    }
  })
  
  # Run expected catch
  observeEvent(input$exp_submit, {
    
    showNotification('Generating expected catch/revenue matrices can take several minutes. A message will appear when complete.',
                     type = 'message', duration = 20)
    
    q_test <- quietly_test(create_expectations)
    
    defineGroup <- if (input$exp_group == 'No group') NULL else input$exp_group
    defaults <- if (is_value_empty(input$exp_default)) FALSE else input$exp_default
    price <- if (input$exp_price == 'none') NULL else input$exp_price
    empty_catch <- switch(input$empty_expectation, 'NA' = NA, '0' = 0, 
                          allCatch = 'allCatch', groupCatch = 'groupCatch')
    empty_exp <- switch(input$empty_expectation, 'NA' = NA, '1e-04' = 1e-04, '0' = 0)
    
    q_test(values$dataset, project$name, input$exp_catch_var, price=price, 
           defineGroup=defineGroup, temp.var=input$exp_temp_var, temporal = input$exp_temporal, 
           calc.method = input$exp_calc_method, lag.method = input$exp_lag_method, 
           empty.catch = empty_catch,  empty.expectation = empty_exp, 
           temp.window = input$exp_temp_window, temp.lag = input$exp_temp_lag, 
           year.lag=input$exp_temp_year, dummy.exp = input$exp_dummy, 
           default.exp = defaults, replace.output = input$exp_replace_output, weight_avg = input$weight_avg)
    
    showNotification('Expected catch/revenue matrix complete',
                     type = 'message', duration = 10)
  }) 
  
  
  ## Merge exp ----
  exp_r <- reactiveValues(ec_names = NULL)
  
  observeEvent(c(input$exp_tab == 'exp_merge', input$exp_merge_reload), {
    
    req(project$name)
    
    exp_r$ec_names <- exp_catch_names(project$name)
    
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  output$exp_merge_ui <- renderUI({
    
    tagList(
      
      selectInput('exp_merge_date', 'Date column',
                  choices = date_cols(values$dataset)),
      
      selectInput('exp_merge_zoneID', 'Zone Identifier Column',
                  choices = colnames(values$dataset)),
      
      selectizeInput('exp_merge_select', 'Select one or more expected catch matrix to merge',
                     choices = exp_r$ec_names, multiple = TRUE),
      
      uiOutput('exp_merge_name_ui')
      
    )
  })
  
  output$exp_merge_name_ui <- renderUI({
    
    if (!is_value_empty(input$exp_merge_select)) {
      
      lapply(seq_along(input$exp_merge_select), function(i) {
        
        textInput(paste0('exp_merge_name_', i), 'Name of new column:',
                  value = input$exp_merge_select[i])
      })
    }
  })
  
  output$exp_merge_tab <- DT::renderDataTable({DT::datatable(values$dataset)})
  
  observeEvent(input$exp_merge_run, {
    
    req(project$name)
    req(input$exp_merge_select)
    
    new_names <- vapply(seq_along(input$exp_merge_select), 
                        function(i) input[[paste0('exp_merge_name_', i)]],
                        character(1))
    
    q_test <- quietly_test(merge_expected_catch, show_msg = TRUE)
    
    values$dataset <- 
      q_test(dat = values$dataset, 
             project = project$name, 
             zoneID = input$exp_merge_zoneID,
             date = input$exp_merge_date,
             exp.name = input$exp_merge_select,
             new.name = new_names,
             ec.table = NULL,
             log_fun = TRUE)
    
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  
  # ---
  # MODEL PARAMETERS ----
  # ---
  
  # helper function for making checkbox
  #names <- c('one','two', 'three')
  inline = function (x) {
    tags$div(style="display:inline-block;", x)
  }
  
  mod_rv <- reactiveValues(final = FALSE, exp = NULL, exp_select = NULL,
                           alt_made = FALSE, alt_num = NULL, alt_choice = NULL,
                           mod_design = FALSE, mod_names = NULL)
  
  # enable run model (modal) button if final table exists
  observeEvent(input$tabs == 'models', {
    
    req(isTruthy(project$name))
    
    # check if final table exists
    mod_rv$final <- table_exists(paste0(project$name, "MainDataTable_final"), project$name)
    # check if expected catch table exists
    exp_exists <- table_exists(paste0(project$name, "ExpectedCatch"), project$name)
    
    if (exp_exists) {
      # list the names of existing expected catch matrices
      e_list <- expected_catch_list(project$name)
      # remove units and scale entry
      e_list <- e_list[!grepl('^scale$|^units$', names(e_list))]
      # save names of matrices that aren't empty
      e_list <- names(e_list[!vapply(e_list, is.null, logical(1))])
      e_list <- e_list[!grepl("_settings", e_list)]
      mod_rv$exp <- e_list
    }
    
    # check for alt choice list (check for dated as well)
    mod_rv$alt_made <- table_exists(paste0(project$name, "AltMatrix"), project$name)
    
    # retrieve # of alts if exists
    if (mod_rv$alt_made) {
      
      alt_list <- alt_choice_list(project$name)
      mod_rv$alt_num <- length(alt_list$greaterNZ)
      mod_rv$alt_choice <- alt_list$alt_var
    }
    
    # check for existing model design files/tables
    mod_rv$mod_design <- table_exists(paste0(project$name, "ModelInputData"), project$name)
    
    
    shinyjs::toggleState("mod_check", condition = mod_rv$final) 
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  output$disableMsg <- renderUI({
    
    tagList(
      if (!mod_rv$final) {
        div(style = "background-color: yellow; border: 1px solid #999; margin: 5px; text-align: justify; padding: 5px;",
            p("Finalized dataset must be saved before modeling."))
      },
      
      if (!mod_rv$alt_made) {
        div(style = "background-color: yellow; border: 1px solid #999; margin: 5px; text-align: justify; padding: 5px;",
            p("Alternative choice list must be saved before modeling."))
      }
    )
  })
  
  # model checklist reactives
  cList <- reactiveValues(out = NULL, pass = NULL)
  
  # checklist modal
  observeEvent(input$mod_check, {
    
    showModal(
      modalDialog(title = "",
                  
                  uiOutput("checklistMsg"),
                  
                  footer = tagList(
                    modalButton("Close")
                  ),
                  easyClose = FALSE
      )
    )
    
    q_test <- quietly_test(checklist)
    cList$out <- q_test(project$name, rv$data)
    cList$pass <- all(vapply(cList$out, function(x) x$pass, logical(1)))
    # find expected catch matrices (if logit_c used)
    
    output$mod_add_run_bttn <- renderUI({
      if (cList$pass) {
        tagList(
          tags$br(),
          
          actionButton("mod_add", "Save model and add new model", 
                       style="color: #fff; background-color: #337ab7; border-color: #800000;"),
          
          tags$br(), tags$br(),
          
          uiOutput('mod_run_bttn')
        )
      }
    })
    
    output$mod_run_bttn <- renderUI({
      
      if (length(mod_rv$mod_names) > 0) {
        
        tagList(
          
          selectInput('mod_run_select', 'Model Run Options', 
                      choices = c('new', 'all', 'select')),
          
          add_prompter(tags$div(style = "margin-left:60px;", 
                                selectInput('mod_explore_starts', 
                                            label = list('Explore starting parameters', icon('info-circle', verify_fa = FALSE)),
                                            choices = c(TRUE,FALSE), selected = TRUE)),
                       position = 'top', type = 'info', size = 'medium',
                       message = 'Default = FALSE. Set to TRUE if unsure of the number of starting parameter values
                                      to include or unsure of reasonable starting values.'
          ),
          
          conditionalPanel("input.mod_explore_starts=='TRUE'",
                           add_prompter(tags$div(style = "margin-left:60px;", 
                                                 selectizeInput('mod_break_early', label = list('Return first valid parameters', icon('info-circle', verify_fa = FALSE)),
                                                                choices = c(TRUE, FALSE), selected = TRUE)),
                                        position = 'top', type = 'info', size = 'medium',
                                        message = "Set to TRUE to return the first set of starting parameter values that returns a valid loglikelihood value;
                                                       set to FALSE to consider the entire parameter space."
                           ),
                           add_prompter(tags$div(style = "margin-left:60px;", 
                                                 textInput('space_vec', label = list('(Optional) Enter number(s) of starting value permutations', icon('info-circle', verify_fa = FALSE)),
                                                           value = "", placeholder = "e.g. 10 or 10,15,... for multiple models")),
                                        position = 'top', type = 'info', size = 'medium',
                                        message = "Enter a vector of the same length as the number of models to run. space is the number(s)
                                                       of starting value permutations to test. The greater the dev value, the larger the space value should be.
                                                       Default value = 10"
                           ),
                           add_prompter(tags$div(style = "margin-left:60px;", 
                                                 textInput('dev_vec', label = list('(Optional) Enter number(s) for how far to deviate from the mean parameter values', icon('info-circle', verify_fa = FALSE)),
                                                           value = "", placeholder = "e.g. 5 or 3,5,... for multiple models")),
                                        position = 'top', type = 'info', size = 'medium',
                                        message = "Enter a vector of the same length as the number of models to run. dev refers to how far to deviate from the average parameter
                                                       values when exploring the parameter space. The less certain the average parameters are, the greater the dev value(S) should be.
                                                       Default value = 5"
                           )
          ),
          
          uiOutput('mod_run_custom_ui'),
          
          actionButton("mod_submit", "Run model(s)",
                       style = "color: #fff; background-color: #6EC479; border-color:#000000;")
        )
      }
    })
    
    output$mod_run_custom_ui <- renderUI({
      
      if (input$mod_run_select == 'select') {
        
        selectizeInput('mod_run_custom', 'Select models to run',
                       choices = mod_rv$mod_names, multiple = TRUE)
      }
    })
    
    #shinyjs::toggleState("mod_submit", condition = {cList$pass == TRUE})
    
    ec_required <- FALSE
    e_catch <- suppressWarnings(list_tables(project = project$name, type = "ec"))
    ec_exists <- length(e_catch) > 0
    
    if (any(rv$data$likelihood %in% "logit_c")) ec_required <- TRUE
    
    # message functions
    passed <- function(type) cList$out[[type]]$pass
    
    pass_icon <- function(type) {
      
      if (passed(type)) {
        if (type == "expect_catch" & ec_required==FALSE & ec_exists==FALSE) {
          
          icon("exclamation-triangle", style = "color: #FFC30B")
        } else icon("check")
        
      } else icon("times")
    }
    
    show_msg <- function(type) {
      if (!passed(type)) tags$ul(tags$li(cList$out[[type]]$msg))
    }
    
    qaqc_msg <- function() {
      
      if (passed("qaqc")) {
        
        out <- lapply(cList$out$qaqc$msg, function(x) tags$li(icon("check"), x))
        
        tags$ul(out, id = "cl-unorList")
      }
    }
    
    ec_msg <- function() {
      
      if (ec_required == FALSE & ec_exists == FALSE) {
        tags$ul(tags$li(cList$out$expect_catch$msg))
      }
    }
    
    # checklist message
    output$checklistMsg <- renderUI({
      tags$div(
        
        tags$h1("Model Checklist"),
        tags$ul(
          
          tags$li(pass_icon("qaqc"), tags$strong("Data quality checks")),
          show_msg("qaqc"),
          qaqc_msg(),
          tags$li(pass_icon("occur_pnts"), tags$strong("Valid occurrence points")),
          show_msg("occur_pnts"),
          tags$li(pass_icon("alt_choice"), tags$strong("Alternative choice matrix created")),
          show_msg("alt_choice"),
          tags$li(pass_icon("expect_catch"), tags$strong("Expected catch/revenue matrix created")),
          show_msg("expect_catch"),
          ec_msg()
        )
      )
    })
  })
  
  # generate a valid model name
  # TODO: check model table
  mod_name_r <- reactive({
    
    mod_nm_default <- paste0(input$model, '_mod1')
    
    if (mod_rv$mod_design) {
      
      mod_list <- model_design_list(project$name)
      mod_names <- vapply(mod_list, function(x) x$mod.name, character(1))
      
      if (mod_nm_default %in% mod_names) {
        
        # recursive naming function
        mod_nm_r <- function(n1, n2, v1) {
          
          if (n1 %in% n2) {
            
            n1 <- gsub('\\d+', '', n1)
            v1 <- v1 + 1
            n1 <- paste0(n1, v1)
            
            if (n1 %in% n2) mod_nm_r(n1, n2, v1)
            else n1
            
          } else n1
        }
        
        mod_nm_default <- mod_nm_r(mod_nm_default, mod_names, 1)
      }
    }
    
    mod_nm_default
  })
  
  output$mod_name_ui <- renderUI({
    
    textInput('mod_name', 'Type model name', value = mod_name_r())
  })
  
  
  output$mod_catch_out <- renderUI({
    tagList(
      selectInput('mod_catch','Column name containing catch data',
                  choices = numeric_cols(values$dataset)),
      
      conditionalPanel(
        condition = "['epm_normal', 'epm_lognormal', 'epm_weibull'].includes(input.model)",
        
        checkboxInput('mod_lockk', 'Location-specific catch parameter', value=FALSE),
        
        selectizeInput('mod_price', 'Price variable', choices= find_value(values$dataset), 
                       multiple = TRUE), options = list(maxItems = 1)),
      
      conditionalPanel(
        condition="input.model=='logit_correction'",
        
        numericInput('mod_polyn', 'Correction polynomial degree', value=2),
        
        radioButtons('startlocdefined', 'Starting location variable', 
                     choices=c('Exists in data frame'='exists', 'Create variable'='create')))
    )
  })
  
  output$mod_logit_correction <- renderUI({
    
    tagList(
      conditionalPanel(condition="input.model=='logit_correction' && input.startlocdefined=='exists'",
                       
                       selectizeInput('mod_startloc', 'Initial location during choice occassion', 
                                      choices=names(values$dataset), 
                                      multiple = TRUE), options = list(maxItems = 1)),
      
      conditionalPanel(condition="input.model=='logit_correction' && input.startlocdefined=='create'",
                       
                       selectInput('mod_trip_id_SL', 'Variable that identifies unique trips', 
                                   choices=names(values$dataset), selectize=TRUE),
                       
                       selectInput('mod_haul_order_SL', 'Variable that identifies haul order within a trip. Can be time, coded variable, etc.',
                                   choices=names(values$dataset), selectize=TRUE),
                       
                       selectizeInput('mod_starting_port_SL', "Variable that identifies port at start of trip", 
                                      choices = list_tables(project$name, type = 'port')),
                       
                       if (names(spatdat$dataset)[1]=='var1') {
                         
                         tags$div(h4('Spatial data file not loaded. Please load on Upload Data tab', style="color:red"))
                       },
                       
                       selectInput('mod_spatID_SL', "Property from spatial data file identifying zones or areas", 
                                   choices= names(spatdat$dataset), selectize=TRUE),
                       
                       selectInput('mod_zoneID_SL', "Column from primary data identifying zones or areas", 
                                   choices= names(spatdat$dataset), selectize=TRUE)
      )
    )
  })
  
  output$mod_select_exp_ui <- renderUI({
    # wrap select UI in a div container so it can be removed as a group
    div(
      class = 'mod-select-exp-container',
      selectInput('mod_select_exp_1', 'Select matrices',
                  choices = mod_rv$exp, multiple = TRUE)
    )
  })
  
  observeEvent(input$mod_add_exp, {
    
    insertUI(selector = '#mod_select_exp_1',
             where = 'afterEnd',
             ui = selectInput(paste0('mod_select_exp_', input$mod_add_exp + 1),
                              label = '', choices = mod_rv$exp, multiple = TRUE)
    )
  })
  
  observeEvent(input$mod_add_exp_reset, {
    # remove old exp selector container
    removeUI(selector = '.mod-select-exp-container')
    # insert new exp selector container
    insertUI('#mod_add_exp_reset', where = 'afterEnd',
             ui = div(class = 'mod-select-exp-container',
                      selectInput('mod_select_exp_1', 'Select matrices',
                                  choices = mod_rv$exp, multiple = TRUE)
             )
    )
  })
  
  # Data needed
  # TODO: check if this is a good idea, may remove necessary variables
  drop <- reactive({grep('date|port|processor|gear|target|lon|lat|permit|ifq', colnames(values$dataset), ignore.case=TRUE)})
  
  output$mod_ind_var_ui <- renderUI({
    
    intvariables <- c('none', colnames(values$dataset[,-drop()]))
    selectInput('mod_ind_vars', label = 'travel-distance variables', multiple=TRUE,
                choices = intvariables, selected = '')
  })
  
  gridlab <- reactive({
    
    if (input$model %in% c('logit_c', 'logit_zonal')) { 
      
      'alternative-specific variables'
      
    } else label='catch-function variables'
  })
  
  output$mod_grid_var_ui <- renderUI({
    
    add_prompter(selectizeInput('mod_grid_vars', 
                                label = list(gridlab(), icon('info-circle', verify_fa = FALSE)),
                                multiple=TRUE, choices = colnames(values$dataset)),
                 
                 position = "top", type='info', size='medium', 
                 message = "Generally, variables that vary by zonal alternatives 
                   or are interacted with zonal constants. See Likelihood functions 
                   sections of the FishSET Help Manual for details. Select 'none' 
                   if no variables are to be included.")
  })
  
  # Determine the # of parameters needed
  numInits <- reactive({
    # TODO account for expected catch matrices
    polyn <- input$mod_polyn
    gridNum <- length(input$mod_grid_vars)
    intNum <- length(input$mod_ind_vars)
    
    if (gridNum == 0 || is.null(gridNum)) gridNum <- 1
    if (intNum == 0 || is.null(intNum)) intNum <- 1
    
    if (input$model == 'logit_c') {
      
      gridNum+intNum
      
    } else if (input$model == 'logit_zonal') {
      
      gridNum*(mod_rv$alt_num-1)+intNum
      
    } else if (input$model == 'logit_correction') {
      
      gridNum*mod_rv$alt_num + ((((polyn+1)*2)+2)*mod_rv$alt_num) + intNum +1+1
      
    } else {
      
      if((input$alt_spec_epm1 && input$model == 'epm_weibull') ||
         (input$alt_spec_epm2 && (input$model == 'epm_lognormal' || input$model == 'epm_normal'))){
        
        gridNum*mod_rv$alt_num+intNum+mod_rv$alt_num+1
      
      # } else if (input$mod_lockk) {
      #   
      #   gridNum*mod_rv$alt_num+intNum+alt+1
        
      } else {
        
        gridNum*mod_rv$alt_num+intNum+1+1
      }
    }
  })

  
  ## Parameter table ----
  # Reactive value to hold initial parameter values and display in a editable table
  iparams <- reactiveValues(data = NULL)
  
  # Generate parameter names for each likelihood
  observeEvent(c(input$model, input$alt_spec_epm1, input$alt_spec_epm2, project$name), {
    # # Extra code just in case numInits() == 0 in the future
    # # Below code will pull output to autofill table if numInits() == 0, but I don't think this will ever be zero given numInits code above
    # x_temp <- read_dat(paste0(locoutput(project$name),
    #                           pull_shiny_output(project$name, type='table',
    #                                             fun=paste0("params_", input$modname))))
    
    if(!(is_empty(project$name))){
      # Get the number of beta and gamma variables to name and initialize parameter values
      gridNum <- length(input$mod_grid_vars)
      intNum <- length(input$mod_ind_vars)
      if (gridNum == 0 || is.null(gridNum)) gridNum <- 1
      if (intNum == 0 || is.null(intNum)) intNum <- 1

      # Generate parameter names based on likelihood function
      if(input$model == "logit_c"){
        par.names <- c(unlist(lapply(1:gridNum, function(x) {paste0('beta.',x)})),
                       unlist(lapply(1:intNum, function(x) {paste0('gamma.',x)})))

      } else {
        # Get zones included in the model to include in param names
        tmpaltc <- unserialize_table(paste0(project$name,"AltMatrix"), project$name)
        tmpzone <- sort(unlist(as.vector(tmpaltc$zoneRow), use.names = FALSE))
        
        if(input$model == "logit_zonal"){
          # beta for the first zone is normalized to zero in the zonal logit
          par.names <- c(unlist(lapply(1:gridNum, function(x) {paste0('beta', x, '.', tmpzone[-1])})),
                         unlist(lapply(1:intNum, function(x) {paste0('gamma', x)})))  
          
        } else if (input$model == "logit_correction") {
          #TODO: once logit_correction is updated add code for param names
          par.names <- seq(1:numInits())    
          
        } else {
          # EPMs
          par.names <- c(unlist(lapply(1:gridNum, function(x) {paste0('beta', x, '.', tmpzone)})),
                         unlist(lapply(1:intNum, function(x) {paste0('gamma', x)})))
          
          if(input$model == "epm_weibull"){
            if(!input$alt_spec_epm1){ # single shape parameter
              par.names <- c(par.names, "k", "sigma")
            } else { # alternative-specific shape parameters
              par.names <- c(par.names, unlist(lapply("k", function(x) {paste0(x, ".", tmpzone)})), "sigma")
            }
            
          } else if (input$model == "epm_lognormal" || input$model == "epm_normal"){
            if(!input$alt_spec_epm2){ # single standard deviation parameter
              par.names <- c(par.names, "stdev", "sigma")
            } else { # alternative-specific standard deviation parameters
              par.names <- c(par.names, unlist(lapply("stdev", function(x) {paste0(x, ".", tmpzone)})), "sigma")
            }
            
          } else {
            par.names <- seq(1:numInits())    
          }
        }
      }
  
      # Save to reactive value dataframe and output in editable table
      iparams$data <- data.frame(Parameter = par.names,
                                 Initial_value = rep(1, numInits()))
    
    }
  })
  
  # create a proxy object to allow editing to existing datatable
  proxy <- DT::dataTableProxy("param_tab")
  
  # observe edits to initial param table
  observeEvent(input$param_tab_cell_edit, {
    info <- input$param_tab_cell_edit
    tab_i = info$row
    tab_j = info$col
    tab_k = info$value

    isolate(
      if(tab_j %in% match("Initial_value", names(iparams$data))) {
        iparams$data[tab_i, tab_j] <<- DT::coerceValue(tab_k, iparams$data[tab_i, tab_j])
      }
    )

    DT::replaceData(proxy, iparams$data, resetPaging = FALSE)
  })
  
  # Generate editable table for initial parameter values
  output$param_tab <- DT::renderDataTable({
    DT::datatable(iparams$data, editable = TRUE)})
  
  # TODO: update/tidy
  output$mod_param_choose <- renderUI(
    radioButtons('mod_init_choice', "",
                 if(length(grep(paste0("_", 'params', "_"), grep(".*\\.csv$", project_files(project$name)), value = TRUE))!=0){
                   choices=c('Use output of previous model as parameter set' = 'prev','Choose parameter set' ='new')
                 } else { choices=c('Choose parameter set' ='new')}, selected='new')
  )
  
  # TODO: update/tidy
  output$mod_param_tab_ui <- renderUI({
    if(length(grep(paste0("_", 'params', "_"), grep(".*\\.csv$", project_files(project$name)), value = TRUE))!=0){
      param_table <- paste0(locoutput(project$name), pull_output(project$name, type='table', fun=paste0('params')))
      param_table <- sub(".*params_", "", param_table)
      param_table <- gsub('.csv', '', param_table)
      selectInput('modname','Select previous model', choices=param_table)
    }
  })
  
  
  counter <- reactiveValues(countervalue = 0) # Defining & initializing the reactiveValues object
  rv <- reactiveValues(
    data = data.frame('mod_name' = NULL, 'likelihood' = NULL, 'optimOpt' = NULL, 
                      'inits'= NULL, 'methodname' = NULL, 'vars1' = NULL,
                      'vars2' = NULL, 'catch' = NULL, 'project' = NULL, 
                      'price' = NULL, 'startloc' = NULL, 'polyn' = NULL, 
                      'exp' = NULL, 'spat' = NULL, 'spatID' = NULL),
    #model_table,
    deletedRows = NULL,
    deletedRowIndices = list()
  )
  
  
  #access variable int outside of observer
  int_name <- reactive({
    paste(lapply(1:numInits(), function(i) {
      inputName <- paste("int", i, sep = "")
      input[[inputName]]
    }))
  })
  
  spatID_choices <- reactive({
    if (!is_value_empty(input$mod_spat)) {
      
      spat <- table_view(input$mod_spat, project$name)
      colnames(spat)
    }
  })
  
  output$mod_spat_ui <- renderUI({
    
    if (mod_rv$alt_choice == 'nearest point') {
      
      tagList(
        h5(strong('Alternative Choice: Nearest Point')),
        
        selectizeInput('mod_spat', 'Select spatial table', 
                       choices = list_tables(project$name, 'spat'), 
                       multiple = TRUE, options = list(maxItems = 1)),
        
        uiOutput('mod_spatID_ui')
        
      )
    }
  })
  
  output$mod_spatID_ui <- renderUI({
    
    selectInput('mod_spatID', 'Select spatial ID column',
                choices = spatID_choices())
  })
  
  # Add model design file 
  observeEvent(input$mod_add, {
    
    req(project$name)
    
    # TODO: check if this is necessary, otherwise remove (grid and ind can be NULL)
    # if (is.null(input$mod_grid_vars)|is.null(input$mod_ind_vars)) {
    #   
    #   showNotification('Model not saved as at least one variable not defined.')
    #   
    # } else {
    #   
    #   showNotification("Selected model parameters saved.", type='message', duration=10)
    # }
    
    if (input$model=='logit_correction' & input$startlocdefined =='create') {
      # TODO: replace with previous_loc()
      # Also, consider moving to data creation tab
      values$dataset$startingloc <-
        create_startingloc(dat=values$dataset, spat=spatdat$dataset, port=input$mod_port_SL,
                           trip_id=input$mod_trip_id_SL, haul_order=input$mod_haul_order_SL,
                           starting_port=input$mod_starting_port_SL,
                           zoneID=input$mod_zoneID_SL, spatID = input$mod_spatID_SL)
    }
    
    counter$countervalue <- counter$countervalue + 1
    
    # combine each select input
    exp_select <- sort(grep('mod_select_exp_', names(input), value = TRUE))
    
    if (!is_value_empty(exp_select))
      mod_rv$exp_list <- lapply(exp_select, function(x) input[[x]])
    
    # replace empties w/ empty string
    str_rpl <- function(string) if (is_value_empty(string)) '' else string
    
    # reformat exp matrix list for mod table
    if (!is_value_empty(mod_rv$exp_list)) {
      
      exp_list <- vapply(mod_rv$exp_list, 
                         function(x) paste(x, collapse = ', '), 
                         character(1))
      
      exp_list <- paste(exp_list, collapse = ' + ')
      
    } else exp_list <- NULL
    
    # CRS 
    mod_crs <- input$mod_spat_crs
    if (mod_crs == 'NA') mod_crs <- NA
    
    rv$data =
      rbind(
        data.frame('mod_name' = input$mod_name,
                   'likelihood' = input$model,
                   'optimOpt' = paste(input$mod_iter, input$mod_relTolX,
                                      input$mod_report_freq, input$mod_detail_report),
                   # 'inits' = paste(int_name(), collapse=','),
                   'inits' = paste(iparams$data$Initial_value, collapse = ','),
                   'methodname' = input$mod_optmeth,
                   'vars1'= str_rpl(paste(input$mod_ind_vars, collapse=',')),
                   'vars2'= str_rpl(input$mod_grid_vars),
                   'catch'= input$mod_catch,
                   'project'= project$name,
                   'price'= str_rpl(input$mod_price),
                   'startloc'= str_rpl(input$mod_startloc),# 'startingloc',
                   'polyn'= input$mod_polyn,
                   'exp' = str_rpl(exp_list),
                   'spat' = str_rpl(input$mod_spat),
                   'spatID' = str_rpl(input$mod_spatID),
                   'crs' = str_rpl(mod_crs)),
        rv$data)
    
    rv$data <- subset(rv$data, mod_name!='')
    
    # TODO: include an option to load a previously saved model design table
    
    # Save table to sql database. Will overwrite each time we add a model
    fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project$name))
    
    modDT <- paste0(project$name, 'ModelDesignTable', format(Sys.Date(), format="%Y%m%d"))
    
    # TODO: reevaluate this approach
    # First, remove any old instances of the table
    if (table_exists(modDT, project$name)) table_remove(modDT, project$name)
    
    if (!table_exists(modDT, project$name)) {
      
      DBI::dbExecute(fishset_db, paste0('CREATE TABLE ', modDT,
                                        '(mod_name TEXT, likelihood TEXT, optimOpt TEXT, inits TEXT,
                                            methodname TEXT, vars1 TEXT, vars2 TEXT, catch TEXT,
                                            lon TEXT, lat TEXT, project TEXT, price TEXT, startloc
                                            TEXT, polyn TEXT, exp TEXT, spat TEXT, spatID TEXT,
                                            crs TEXT)'))
    }
    
    # Construct the update query by looping over the data fields
    query <- sprintf(
      "INSERT INTO %s (%s) VALUES %s",
      modDT,
      paste(names(data.frame(as.data.frame(isolate(rv$data
      )))), collapse = ", "),
      paste0("('", matrix(apply(as.data.frame(isolate(rv$data
      )), 1, paste, collapse="','"), ncol=1), collapse=',', "')")
    )
    # Submit the update query and disconnect
    DBI::dbExecute(fishset_db, query)
    DBI::dbDisconnect(fishset_db)
    
    # save to MDF
    q_test <- quietly_test(make_model_design, show_msg = TRUE)
    
    # force exp list to NULL for zonal logit because this will automatically fill with previous model designs
    if(input$model == "logit_zonal" || grepl("epm", input$model)){
      mod_rv$exp_list <- NULL
    }

    q_test(project = project$name, 
           catchID = input$mod_catch,
           likelihood = input$model, 
           # initparams = paste(int_name(), collapse = ","),
           initparams = iparams$data$Initial_value,
           optimOpt = c(input$mod_iter, input$mod_relTolX,
                        input$mod_report_freq, input$mod_detail_report),
           methodname = input$mod_optmeth, 
           mod.name = input$mod_name,
           vars1 = input$mod_ind_vars, vars2 = input$mod_grid_vars,
           priceCol = input$mod_price, expectcatchmodels = mod_rv$exp_list,
           startloc = input$mod_startloc, polyn = input$mod_polyn,
           spat = input$mod_spat, spatID = input$mod_spatID, crs = mod_crs)
    
    # reset exp select
    mod_rv$exp_list <- NULL
    
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  
  
  
  #      observeEvent(input$resetModel, {
  #        shinyjs::reset("form")
  #      })
  
  parseDeleteEvent <- function(idstr) {
    res <- as.integer(sub(".*_([0-9]+)", "\\1", idstr))
    if (! is.na(res)) res
  }
  
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
  
  deleteButtonColumn <- function(df, id, ...) {
    # function to create one action button as string
    f <- function(i) {
      as.character(
        actionButton(
          # The id prefix with index
          paste(id, i, sep="_"),
          label = NULL,
          icon = icon('trash'),
          onclick = 'Shiny.setInputValue(\"deletePressed\", this.id, {priority: "event"})'))
    }
    
    deleteCol <- unlist(lapply(seq_len(nrow(df)), f))
    
    # Return a data table
    DT::datatable(cbind(delete = deleteCol, df),
                  # Need to disable escaping for html as string to work
                  escape = FALSE,
                  options = list(
                    # Disable sorting for the delete column
                    columnDefs = list(
                      list(targets = 1, sortable = FALSE))
                  ))
  }
  
  output$mod_param_table <- DT::renderDataTable(
    # Add the delete button column
    deleteButtonColumn(as.data.frame(rv$data[-9]), 'delete_button')
  )
  
  
  
  # update model names list
  
  observeEvent(c(input$mod_reload, input$mod_add, input$mod_delete, 
                 input$tabs == 'models', cList$pass == TRUE, input$mod_check), {
                   
                   req(project$name)
                   
                   if (!mod_rv$mod_design){
                     # check for existing model design files/tables
                     mod_rv$mod_design <- table_exists(paste0(project$name, "ModelInputData"), project$name)
                   }
                   
                   if (mod_rv$mod_design) {
                     
                     mod_rv$mod_names <- model_names(project$name)[!grepl("_outsample",model_names(project$name)) &
                                                                     !grepl("train",model_names(project$name)) &
                                                                     !grepl("test",model_names(project$name))]
                   }
                   
                 }, ignoreNULL = TRUE, ignoreInit = TRUE, priority = -1) # run after other events
  
  
  ## Save/run models ----
  observeEvent(input$mod_submit, {
    
    # Prepare model args from user input
    if(!is_empty(input$space_vec)){
      space_val <- unlist(strsplit(gsub(" ", "", input$space_vec), ",")) # remove white space, separate commas
      tryCatch(
        space_val <- as.numeric(space_val),
        warning = function(w){
          showNotification("One or more 'space' values invalid and coerced to NULL - model is running with default space = 10", type = "warning", duration = 45)
        }
      )
    } else { # if it is empty then set = NULL
      space_val <- NULL
    }
    
    if(!is_empty(input$dev_vec)){
      dev_val <- unlist(strsplit(gsub(" ", "", input$dev_vec), ",")) # remove white space, separate commas
      tryCatch(
        dev_val <- as.numeric(dev_val),
        warning = function(w){
          showNotification("One or more 'dev' values invalid and coerced to NULL - model is running with default dev = 5", type = "warning", duration = 45)
        }
      )
    } else { # if it is empty then set = NULL
      dev_val <- NULL
    }
    
    
    removeModal()
    
    input_list <- reactiveValuesToList(input)
    
    toggle_inputs(input_list, FALSE)
    
    # Run model(s)
    # TODO: make these args available in the app (pop-up?)
    # add run arg
    
    if (input$mod_run_select == 'select') mod_run <- input$mod_run_custom
    else mod_run <- input$mod_run_select
    
    q_test <- quietly_test(discretefish_subroutine, show_msg = TRUE)
    
    withProgress(
      discretefish_subroutine(project = project$name, run = mod_run, select.model = FALSE, 
                              explorestarts = input$mod_explore_starts, breakearly = input$mod_break_early, space = space_val, 
                              dev = dev_val, use.scalers = FALSE, scaler.func = NULL),
      message = "Running model(s): ",
      detail = 'Models can take up to a few minutes to run. All buttons are inactive while models are running.
                    Check R console for progress.'
    )
    
    showNotification('Model run is complete. Check the `Compare Models` subtab to view output', 
                     type='message', duration=30)
    
    toggle_inputs(input_list, TRUE)
  })
  
  #Add in two more tables for model evaluations
  mod_sum_out <- reactive({
    
    input$mod_reload
    
    tab <- paste0(project$name, 'modelOut')
    
    if (table_exists(tab, project$name)) {
      
      model_out_view(project$name)
    }
  })
  
  
  mod_params_out <- reactive({
    
    input$mod_reload
    
    mod_tab <- data.frame(Model_name=rep(NA, length(mod_sum_out())),
                          Covergence=rep(NA, length(mod_sum_out())),
                          # Stand_Errors=rep(NA, length(mod_sum_out())),
                          Estimates=rep(NA, length(mod_sum_out())),
                          Hessian=rep(NA, length(mod_sum_out())))
    
    for (i in seq_along(mod_sum_out())) {
      
      mod_tab[i,1] <- mod_sum_out()[[i]]$name
      mod_tab[i,2] <- 1
      tryCatch({
        mod_tab[i,2] <- mod_sum_out()[[i]]$optoutput$convergence
      }, error = function(cond){
        # do nothing
      })
      model_out <- mod_sum_out()[[i]]$OutLogit
      mod_tab[i,3] <- to_html_table(model_out, rownames = TRUE, digits = 3)
      if(length(grep("Error", mod_sum_out()[[i]]$H1)) == 0){
        hess <- round(mod_sum_out()[[i]]$H1, 5)
        colnames(hess) <- row.names(model_out)
        mod_tab[i,4] <- to_html_table(hess, digits = 5)
      } else {
        hess <- 1
        mod_tab[i,4] <- 1
      }

    }
    
    return(mod_tab)
  })
  
  ## Model list ----
  
  mod_design_list_r <- reactive({
    
    req(project$name)
    req(mod_rv$mod_design)
    
    input$mod_reload
    
    mdl <- model_design_list(project$name)
    names(mdl) <- model_names(project$name)
    mdl
  })
  
  output$mod_list_ui <- renderPrint({
    
    str(mod_design_list_r())
  })
  
  ## Model Output ----
  # TODO: better method/msg for missing convergence msg
  mod_conv <- function(x) if (is_value_empty(x)) '' else x
  
  output$mod_model_tab <- DT::renderDataTable({
    if (!is_value_empty(mod_sum_out())) DT::datatable(mod_params_out(), escape = FALSE)
  })
  
  ## model manage ----
  
  mod_del_select <- reactive({
    
    req(input$mod_man_mod_type)
    
    mod_n <- vapply(mod_design_list_r(), function(x) x$mod.name, character(1))
    
    mdf_nn <- lapply(mod_design_list_r(), function(x) {
      
      if (!is.null(x$expectcatchmodels)) {
        # if exp matrices included, created full model name
        vapply(x$expectcatchmodels, function(y) {
          
          paste0(c(x$mod.name, y), collapse = '.')
        }, character(1))
        # otherwise, return unnested name
      } else x$mod.name
    })
    
    if (input$mod_man_mod_type == 'nested model') {
      
      unlist(mdf_nn, use.names = FALSE)
      
    } else mod_n
  })
  
  output$mod_man_ui <- renderUI({
    
    tagList(
      
      selectizeInput('mod_man_select', 'Select model(s) to delete',
                     choices = mod_del_select(), multiple = TRUE)
    )
  })
  
  observeEvent(input$mod_delete, {
    
    q_test <- quietly_test(delete_models, show_msg = TRUE) 
    
    q_test(project = project$name, model.names = input$mod_man_select, 
           delete.nested = TRUE)
    
    # refresh model list/output
    input$mod_reload
    
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  ## Model Error ----
  
  mod_err_out <- reactive({
    
    input$mod_reload
    
    error_out <- data.frame(Model_name=rep(NA, length(mod_sum_out())), 
                            Model_error=rep(NA, length(mod_sum_out())), 
                            Optimization_error=rep(NA, length(mod_sum_out())))
    
    for (i in seq_along(mod_sum_out())) {
      
      error_out[i,1] <- mod_sum_out()[[i]]$name
      error_out[i,2] <- ifelse(is.null(mod_sum_out()[[i]]$errorExplain), 
                               'No error reported', toString(mod_sum_out()[[i]]$errorExplain))
      error_out[i,3] <- ifelse(is.null(mod_sum_out()[[i]]$optoutput$optim_message), 
                               'No message reported', toString(mod_sum_out()[[i]]$optoutput$optim_message))
    }
    
    return(error_out)
  })
  
  output$mod_error_msg <- DT::renderDT({
    if (!is_value_empty(mod_sum_out())) mod_err_out()
  })
  
  ## Model Fit ----
  
  mod_compare <- reactiveValues(fit = NULL)
  
  observeEvent(c(input$mod_sub == 'model_compare', input$mod_reload), {
    
    fit_tab <- paste0(project$name, 'ModelFit')
    
    if (table_exists(fit_tab, project$name)) {
      
      mf_out <- as.data.frame(t(model_fit(project$name)))
      mod_compare$fit <- tibble::rownames_to_column(mf_out, 'model')
    }
    
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(input$mod_delete, {
    
    temp = mod_compare$fit
    
    if (!is.null(input$mod_fit_out_rows_selected)) {
      temp <- temp[-as.numeric(input$mod_fit_out_rows_selected),]
    }
    
    mod_compare$fit <- temp
    session$sendCustomMessage('unbind-DT', 'mod_fit_out')
  })
  
  shinyInput = function(FUN, len, id, ...) { 
    inputs = character(len) 
    for (i in seq_len(len)) { 
      inputs[i] = as.character(FUN(paste0(id, i), label = NULL, ...)) 
    } 
    inputs 
  } 
  
  # datatable with checkbox
  output$mod_fit_out <- DT::renderDT({
    
    data.frame(mod_compare$fit, 
               select=shinyInput(checkboxInput,
                                 nrow(mod_compare$fit),
                                 "cbox_"))
  }, 
  colnames=c('Model','AIC','AICc','BIC','PseudoR2','Selected'),  
  filter='top', server = TRUE, escape = FALSE,
  options = list(dom = 't', paging=FALSE,
                 preDrawCallback = DT::JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                 drawCallback = DT::JS('function() { Shiny.bindAll(this.api().table().node()); } '),
                 scrollX = TRUE
  ) 
  )
  
  # helper function for reading checkbox
  shinyValue = function(id, len) { 
    unlist(lapply(seq_len(len), function(i) { 
      value = input[[paste0(id, i)]] 
      if(is.null(value)) NA else value 
    })) 
  } 
  
  shinyDate = function(id, len) { 
    unlist(lapply(seq_len(len), function(i) { 
      value=ifelse(input[[paste0(id, i)]]!=TRUE, '', as.character(Sys.Date())) 
    })) 
  }
  
  checkedsave <- 
    reactive({
      
      cbind(model = rownames(isolate(mod_compare$fit)),
            AIC=isolate(mod_compare$fit[,1]),
            AICc=isolate(mod_compare$fit[,2]),
            BIC=isolate(mod_compare$fit[,3]),
            PseudoR2=isolate(mod_compare$fit[,4]), 
            Selected = shinyValue("cbox_", nrow(mod_compare$fit)),
            Date = shinyDate("cbox_", nrow(mod_compare$fit)) 
      )
    })
  
  # TODO: fix this
  # When the Submit button is clicked, save the form data
  observeEvent(input$mod_save_table, {
    req(project$name)
    # Connect to the database
    fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project$name))
    
    mod_chosen <- paste0(project$name, 'ModelChosen')
    
    # TODO: revisit this action
    if (table_exists(mod_chosen, project$name)) {
      
      table_remove(mod_chosen, project$name)
    }
    
    DBI::dbExecute(fishset_db, paste('CREATE TABLE', mod_chosen, 
                                     '(model TEXT, AIC TEXT, AICc TEXT, BIC TEXT, 
                                         PseudoR2 TEXT, Selected TEXT, Date TEXT)'))
    # Construct the update query by looping over the data fields
    query <- sprintf(
      "INSERT INTO %s (%s) VALUES %s",
      mod_chosen, 
      paste(names(data.frame(as.data.frame(isolate(checkedsave())))), collapse = ", "),
      paste0("('", matrix(apply(as.data.frame(isolate(checkedsave())), 1, paste, collapse="','"), ncol=1), collapse=',', "')")
    )
    # Submit the update query and disconnect
    DBI::dbExecute(fishset_db, query)
    
    showNotification("Table saved to database")
    DBI::dbDisconnect(fishset_db)
  })
  
  
  
  ## Resetting inputs
  observeEvent(input$refresh1,{
    
    updateCheckboxInput(session, 'Outlier_Filter', value=FALSE)
    
  })         
  
  
  ## Model performance and prediction ----
  output$cv_ui <- renderUI({
    tagList(
      selectInput('cv_model','Select model for cross-validation:',
                  choices = mod_rv$mod_names),
      
      selectizeInput('cv_zoneID', 'Column containing zone identifier:', choices = colnames(values$dataset),
                     options = list(maxItems = 1)),
      
      add_prompter(
        selectizeInput('cv_group', label = list('Group data into folds by:', icon('info-circle', verify_fa = FALSE)), 
                       choices = c('Observations', 'Years', colnames(values$dataset)), selected = 'Observations',
                       options = list(maxItems = 1), width = "100%"),
        message = 'Determine how data will be subsetted into groups. "Observations" will randomly split the data into
                       k number of groups. "Years" will split data annually. Other categorical variable will split data by categories.
                       the number of groups should be limited to 10 total regardless of method for subsetting data.',
        position = "top", type='info', size='medium', 
      ),
      
      conditionalPanel(condition = "input.cv_group == 'Observations'",
                       numericInput('cv_k', 'Select k number of folds (groups)', value = NULL, min=1)                 
      ),
      
      conditionalPanel(condition = "input.cv_group == 'Years'",
                       selectizeInput('cv_time_var', 'Select column containing dates:', 
                                      choices = colnames(values$dataset), selected = NULL,
                                      options = list(maxItems = 1, placeholder = 'Select option below',
                                                     onInitialize = I('function() {this.setValue("");}'))),
      )
    )
  })
  
  observeEvent(input$cv_k, {
    if(!is.na(input$cv_k)){
      if(input$cv_k > 10) showNotification("Values of k > 10 can results in long runtimes", type = 'warning', duration = 15)
    }
  })
  
  observeEvent(input$run_cv, {
    
    crossVal_k <- if(input$cv_group != "Observations") NULL else input$cv_k
    crossVal_timeVar <- if(input$cv_group != "Years") NULL else input$cv_time_var
    
    if((is.na(crossVal_k) || is.null(crossVal_k)) && input$cv_group == "Observations"){
      showNotification("k is required if grouping by observations", type = 'error', duration = 10)
      
    } else if (is_empty(crossVal_timeVar) && input$cv_group == "Years") {
      showNotification("A date variable is required if grouping data annually", type = 'error', duration = 10)
      
    } else {
      if(!is.na(crossVal_k) && !is.null(crossVal_k) && crossVal_k > 10) showNotification("Values of k > 10 can results in long runtimes", type = 'warning', duration = 15)
      
      showNotification('The cross validation function can take several minutes (up to 15 minutes for 10 groups) to run. A 
                         message will appear when complete. View progress in the R console window.',
                       type = 'message', duration = 30)
      
      cross_validation(project$name, input$cv_model, input$cv_zoneID, input$cv_group, crossVal_k,
                       crossVal_timeVar, use.scalers = FALSE, scaler.func = NULL)
      
      showNotification('Cross validation complete', type = 'message', duration = 30)
    }
  })
  
  cv_out <- reactive({
    
    input$reload_cv
    
    cv_out <- readRDS(paste0(locoutput(project$name), project$name, "CrossValidationOutput.rds"))
    
    cv_out[[3]] <- lapply(seq_along(cv_out[[3]]), function(x, n, i){ 
      x[[i]]$iteration <- n[[i]]
      return(x[[i]])
    }, x = cv_out[[3]], n = names(cv_out[[3]]))
    
    cv_out[[3]] <- dplyr::bind_rows(cv_out[[3]])
    
    return(cv_out)
  })      
  
  output$cv_perf_tab <- DT::renderDT({
    
    if(!is_value_empty(cv_out()[[1]])){
      tmp_tab <- cv_out()[[1]]
      names(tmp_tab) <- c("Test Group", "Percent Absolute Prediction Error")
    }
    
    return(tmp_tab)
    
  }, escape = FALSE)
  
  output$cv_modfit_tab <- DT::renderDT({
    
    if(!is_value_empty(cv_out()[[2]])){
      tmp_tab <- cv_out()[[2]]
    }
    
    return(tmp_tab)
    
  }, escape = FALSE)
  
  output$cv_modout_tab <- DT::renderDT({
    
    if(!is_value_empty(cv_out()[[3]])){
      tmp_tab <- cv_out()[[3]]
    }
    
    return(tmp_tab)
    
  }, escape = FALSE)
  
  
  output$load_outsample <- renderUI({
    add_prompter(
      fileInput('outsample_dat', 
                label = list('Choose out-of-sample data file:', icon('info-circle', verify_fa = FALSE)), 
                multiple = FALSE, placeholder = 'Required data'),
      message = 'If out-of-sample dataset loaded in a previous session of FishSET then skip this step.',
      position = "top", type='info', size='medium', 
    )
    
  })
  
  observeEvent(input$outsample_dat, {
    
    load_err <- FALSE
    tmp_outsample <- NULL
    
    tryCatch(
      tmp_outsample <- readRDS(input$outsample_dat$datapath),
      error = function(e) {load_err <<- TRUE} 
    )
    
    if(!load_err & !is_value_empty(tmp_outsample)){
      
      q_test <- quietly_test(load_outsample)
      
      qc_pass <- q_test(tmp_outsample, project = project$name, over_write = TRUE,
                        compare = FALSE, y = NULL)
      
      if (qc_pass) {
        
        showNotification("Out-of-sample data saved to database.", type = "message", 
                         duration = 10)
        
      } else {
        
        showNotification('Selected file not loaded. Check file type and load_outsample()
                           documentation.', type = 'error', duration = 15)
        
      }
      
    } else {
      
      showNotification('Selected file not loaded. Check file type and load_outsample()
                           documentation.', type = 'error', duration = 15)
      
    }
    
  })
  
  # Save model name as reactive for model_design_outsample() below
  main_modname <- reactiveValues(name = NULL)
  
  output$filter_outsample <- renderUI({    
    tagList(
      add_prompter(
        selectizeInput('mod_name_outsample', 
                       label = list('Select main model name', icon('info-circle', verify_fa = FALSE)), 
                       choices = mod_rv$mod_names, multiple = FALSE),
        message = 'The main model name is required for filtering out of sample data (step 2), creating the out-
        of-sample model designs (step 3), and running predictions (step 4).',
        position = "top", type='info', size='medium', 
      ),
      
      tags$div(style = "margin-top: -10px;",
               selectizeInput('filter_outsample_datzone', 'Select primary data column containing zone identifier:', 
                              choices = colnames(values$dataset), options = list(maxItems = 1))),
      
      selectizeInput('filter_outsample_spatzone', 'Select spatial data column containing zone identifier:', 
                     choices = colnames(spatdat$dataset), options = list(maxItems = 1)),
      
      checkboxInput("spat_outsample", "Are data out-of-sample spatially?"),
      
      add_prompter(
        actionButton('run_outsample_filter', "Filter out-of-sample data",
                     style = "background-color: blue; color: white;"),
        message = 'QAQC filters are automatically applied and remove rows containing NA and NAN values',
        position = "top", type='info', size='medium', 
      )
      
    )
  })
  
  observeEvent(input$mod_name_outsample, {
    main_modname$name <- input$mod_name_outsample
  })
  
  # Reactive values for selecting out-of-sample zones
  outsample_clicked_ids <- reactiveValues(ids = vector())
  outsample_table <- reactiveValues(data = NULL)
  filename <- reactiveValues(name = NULL)
  
  observeEvent(input$run_outsample_filter, {
    
    showNotification("Starting filtering process.", type = "message")
    
    load_err <- FALSE
    dat <- NULL
    spat <- spatdat$dataset
    
    tryCatch(
      dat <- table_view(paste0(project$name, "OutSampleDataTable"), project$name),
      error = function(e) {load_err <<- TRUE} 
    )
    
    if(!load_err & !is_value_empty(dat)){
      
      # QAQC on out-of-sample data
      dat <- na_filter(dat, project = project$name, x=qaqc_helper(dat, "NA", "names"),
                       replace = FALSE, remove = TRUE, rep.value=NA, over_write=FALSE)
      dat <- nan_filter(dat, project = project$name, x=qaqc_helper(dat, "NA", "names"),
                        replace = FALSE, remove = TRUE, rep.value=NA, over_write=FALSE)
      
      if(input$spat_outsample) showNotification("Loading map for selecting out-of-sample locations.", type = "message", duration = 20)    
      
      filter_out <- filter_outsample(dat = dat, project = project$name, mod.name = input$mod_name_outsample,
                                     spatial_outsample = input$spat_outsample, zone.dat = input$filter_outsample_datzone,
                                     spat = spat, zone.spat = input$filter_outsample_spatzone)
      
      # Not out-of-sample spatially
      if(length(filter_out) == 1){
        showNotification("Out-of-sample data filtering complete", type = "message")  
        
        # Out-of-sample spatially
      } else {
        spat <- filter_out[[1]]
        mod.spat <- filter_out[[2]]
        zone.dat <- input$filter_outsample_datzone
        
        showModal(
          modalDialog(title = "Select out-of-sample zones",
                      
                      zone_outsample_mapUI("map1"),
                      "Click on one or more polygons to select zones for out-of-sample predictions.",
                      "\n Click the 'Save out-of-sample zones' button to save choices.",
                      
                      fluidRow(
                        column(6, zone_outsample_tableUI("table1")),
                        column(4, zone_outsample_saveUI("save"), offset = 1),
                      ),
                      size = "l",
                      footer = modalButton("Close window")
          )
        )
        
        zone_outsample_mapServer("map1", outsample_clicked_ids, spat, mod.spat, zone.dat)
        zone_outsample_tableServer("table1", outsample_clicked_ids, outsample_table)
        zone_outsample_saveServer("save", outsample_table, filename, zone.dat, project$name, dat)
      }
      
      
    } else {
      
      showNotification("Out-of-sample table not found in data base.", type = "error", duration = 15)
      
    }
  })
  
  output$mod_design_outsample <- renderUI({    
    tagList(
      add_prompter(
        textInput('outsample_mod_name', 
                  label = list('Enter name for out-of-sample model design:', icon('info-circle', verify_fa = FALSE)), 
                  value = NULL, placeholder = 'include "_outsample" at the end of model name', width = "100%"),
        message = 'Name must be unique and not exist already in model design list. If left blank, 
                   then a default name will be chosen as "[main model name]_outsample".',
        position = "top", type='info', size='medium', 
      ),
      
      add_prompter(
        actionButton('make_outsample_design', "Create out-of-sample model design",
                     style = "background-color: blue; color: white;"),
        message = 'Select main model name in Step 2 above if not done already',
        position = "top", type='info', size='medium' 
      )
      
    )
  })
  
  # Reactive value for out-of-sample model names
  outsample_names <- reactiveValues(names = NULL)
  
  observeEvent(input$make_outsample_design, {
    
    # Check if the model name exists already
    mod_names <- model_names(project$name)
    
    if(is_empty(input$outsample_mod_name)){
      tmp_name <- paste0(input$mod_name_outsample, "_outsample")
    } else {
      tmp_name <- input$outsample_mod_name
    }
    
    if(tmp_name %in% mod_names){
      showNotification(paste0("'", tmp_name, "'", " already exists. Enter a new model name followed by '_outsample'."), type = "error", duration = 10)  
      
    } else if(!grepl("_outsample", tmp_name)) {
      showNotification("Error: must include '_outsample' at the end of the model name.", type = "error", duration = 10)  
      
    } else {
      showNotification("Generating out-of-sample model design. See R console for progress.", type = "message", duration = 10)
      
      model_design_outsample(project = project$name, mod.name = input$mod_name_outsample, 
                             outsample.mod.name = input$outsample_mod_name, CV = FALSE, CV_dat = NULL, 
                             use.scalers = FALSE, scaler.func = NULL)
      
      outsample_names$names <- model_names(project$name)[grep("_outsample", model_names(project$name))]
      
      showNotification("Out-of-sample model design added to FishSET database.", type = "message")
      
    }
  })
  
  observeEvent(input$mod_sub, {
    if(input$mod_sub == "outsample_predict"){
      outsample_names$names <- model_names(project$name)[grep("_outsample", model_names(project$name))]
    }
  })
  
  # Run out-of-sample prediction
  output$run_outsample_prediction <- renderUI({
    tagList(
      add_prompter(
        selectizeInput('outsample_predict_name', 
                       label = list('Select out-of-sample model design', icon('info-circle', verify_fa = FALSE)), 
                       choices = outsample_names$names, multiple = FALSE),
        message = "Select out-of-sample model design for running prediction. Only model designs with '_outsample' in their
        names will be available for running out-of-sample predictions.",
        position = "top", type='info', size='medium'
      ),
      
      tags$br(),
      
      actionButton('run_outsample', "Run prediction",
                   style = "color: #fff; background-color: #6da363; border-color: #800000;")
    )
  })
  
  # Reactive value for out-of-sample prediction results
  outsample_predouts <- reactiveValues(pred_probs = NULL,
                                       perc_abs_pred_error = NULL)
  
  observeEvent(input$run_outsample, {
    
    tmp_out <- predict_outsample(project = project$name, mod.name = input$mod_name_outsample,
                                 outsample.mod.name = input$outsample_predict_name,
                                 use.scalers = FALSE, scaler.func = NULL)
    
    outsample_predouts$pred_probs <- tmp_out[[1]]
    outsample_predouts$perc_abs_pred_error <- tmp_out[[2]]
    
    output$outsample_preds <- DT::renderDT({
      
      if(!is_value_empty(outsample_predouts$pred_probs)){
        tmp_tab <- outsample_predouts$pred_probs
        names(tmp_tab) <- c("Zone", "Probability")
      }
      
      return(tmp_tab)
      
    }, escape = FALSE)
    
    outsample_plot <- predict_map(project = project$name, spat = spatdat$dataset, zone.spat = input$filter_outsample_spatzone,
                                  outsample = TRUE, outsample_pred = outsample_predouts$pred_probs)
    
    if(length(outsample_plot) == 1){
      showNotification("Unable to create map of predicted fishing probabilities. Check settings in all steps and rerun prediction.",
                       type = "error", duration = 10)
      output$map_outsample <- renderPlot({
        plot.new()
        text(.3, 1, labels = "Error: unable to generate map - check settings in left side panel", col = 'red', cex = 1.5)
      })  
    } else {
      output$map_outsample <- renderPlot({
        plot(outsample_plot)
      })  
    }
  })
  
  output$outsample_pred_err <- renderUI({
    tagList(
      tags$div(style = "font-size: 18px; margin-top:15px;",
               "Percent absolute prediction error = ",  
               if(!is.null(outsample_predouts$perc_abs_pred_error)){
                 strong(round(outsample_predouts$perc_abs_pred_error, digits = 3))    
               }
      )
    )
  })
  
  
  
  ### ---  
  # Save output ----   
  ###---   
  observeEvent(input$saveDataExplore, {
    
    req(project$name)
    
    q_test <- quietly_test(table_save)
    
    if (input$SelectDatasetExplore == "main") {
      
      saved <- q_test(values$dataset, project = project$name, type = "main")
      
    } else if (input$SelectDatasetExplore == "port") {
      
      saved <- q_test(ptdat$dataset, project = project$name, type = "port")
      
    } else if (input$SelectDatasetExplore == "grid") {
      
      g_name <- gsub(project$name, "", input$grid_select)
      
      saved <- q_test(grddat$dataset, project = project$name, type = "grid", name = g_name)
      
    } else if (input$SelectDatasetExplore == "auxiliary") {
      
      saved <- q_test(aux$dataset, project = project$name, type = "aux", name = input$AuxName)
    }
    
    if (is.logical(saved) && saved) {
      
      showNotification('Data saved to FishSET database', type = 'message', duration = 10)
      
    } else {
      
      showNotification("Table was not saved.", type = "warning", duration = 10)
    }
  })
  
  observeEvent(input$saveData, {
    
    req(project$name)
    
    q_test <- quietly_test(table_save)
    saved <- q_test(values$dataset, project = project$name, type = "main")
    
    if (is.logical(saved) && saved) {
      
      showNotification('Data saved to FishSET database', type = 'message', duration = 10)
      
    } else {
      
      showNotification("Table was not saved.", type = "warning", duration = 10)
    }
  })
  
  observeEvent(input$saveDataNewVars, {
    
    req(project$name)
    
    q_test <- quietly_test(table_save)
    saved <- q_test(values$dataset, project = project$name, type = "main")
    
    if (is.logical(saved) && saved) {
      
      showNotification('Data saved to FishSET database', type = 'message', duration = 10)
      
    } else {
      
      showNotification("Table was not saved.", type = "warning", duration = 10)
    }
  })
  
  # Export data ----
  file_ext <- reactive({
    
    switch(input$export_type, "csv" = ".csv", "txt" = ".txt", "rdata" = ".RData", 
           "xlsx" = ".xlsx", "json" = ".json", "stata" = ".dta", "sas" = ".sas", "spss" = ".sav",  
           "matlab" = ".mat")
  })
  
  output$exportData <- downloadHandler(
    filename = function() {
      paste0(project$name, "MainDataTable", file_ext())
    },
    content = function(file) {
      write_dat(values$dataset, file = file, file_type = input$export_type, project$name)
    }
  )
  
  # Save final Dataset ----
  save_final <- reactiveValues()
  
  saveModal <- function(ns) {  
    
    modalDialog(title = "Save the final version of the data before modeling",
                # TODO: check uniqueID select, not sure it works as intended 
                selectInput("final_uniqueID", "Select column containing unique occurrence identifier",
                            choices = names(values$dataset)),
                
                selectInput("final_latlon", "Select lat/lon variables to be used in models",
                            choices = names(values$dataset), multiple = TRUE),
                
                shinycssloaders::withSpinner(
                  uiOutput("checkMsg"),
                  type = 6
                ),
                
                footer = tagList(
                  modalButton("Close"),
                  actionButton("save_final_table", "Save", 
                               style = "color: #fff; background-color: #6EC479; border-color:#000000;")
                )
    )
  }
  
  observeEvent(input$save_final_modal, {
    
    showModal(saveModal(session$ns))
  })
  
  
  observeEvent(input$save_final_table, {
    
    q_test <- quietly_test(check_model_data)
    save_final$out <- q_test(dat = values$dataset,  project = project$name,
                             uniqueID = input$final_uniqueID, latlon = input$final_latlon)
  })
  
  output$checkMsg <- renderUI({
    
    if (!is.null(save_final$out$save_out)) {
      
      if (save_final$out$save_out) {
        
        div(p("Final table saved"), style = "color: green;")
        
      } else {
        
        tagList(
          div(p("Final table was not saved:"), style = "color: red;"),
          renderText(paste(save_final$out$msg, collapse = "\n")),
          renderText("Correct data quality issues on the `Data Quality Evaluation` tab.")
        )
      }
      
    } else p("")
  })
  
  
  #Downloads ====  
  savedText <- reactiveValues(answers = logical(0))
  # TODO: choose location to display notes, add info icons to note text boxes
  observeEvent(c(input$callTextDownloadAnal,
                 input$callTextDownloadExplore,
                 input$callTextDownloadQAQC,
                 input$callTextDownloadUp,
                 input$callTextDownloadNew,
                 input[["fleet-callTextDownload"]],
                 input$callTextDownloadAlt,
                 input$callTextDownloadEC,
                 input$callTextDownloadModels,
                 input$callTextDownloadBook),{
                   
                   if (!isTruthy(project$name)) {
                     
                     showNotification("Enter a project name. Note not saved.", type = 'message', duration = 5)
                     
                   } else {
                     
                     savedText$answers <- reactiveValuesToList(notes)
                     nms <- c("dataQuality", "explore", "analysis")
                     for (n in nms) {
                       savedText$answers[[n]] <- c(savedText$answers[[n]], case_to_print[[n]])
                     }
                     
                     updateTextInput(session, 'notesUp', "Notes", value = "", 
                                     placeholder = 'Write notes to store in text output file. Text can be inserted into report later.')
                     updateTextInput(session, 'notesQAQC', "Notes", value="", 
                                     placeholder = 'Write notes to store in text output file. Text can be inserted into report later.')
                     updateTextInput(session, 'notesExplore', "Notes", value = "", 
                                     placeholder = 'Write notes to store in text output file. Text can be inserted into report later.')
                     updateTextInput(session, "fleet-notes", "Notes", value = "",
                                     placeholder = 'Write notes to store in text output file. Text can be inserted into report later.')
                     updateTextInput(session, 'notesAnal', "Notes", value="", 
                                     placeholder = 'Write notes to store in text output file. Text can be inserted into report later.')
                     updateTextInput(session, 'notesNew', "Notes", value = "",
                                     placeholder = 'Write notes to store in text output file. Text can be inserted into report later.')
                     updateTextInput(session, 'notesAltc', "Notes", value = "", 
                                     placeholder = 'Write notes to store in text output file. Text can be inserted into report later.')
                     updateTextInput(session, 'notesEC', "Notes", value = "",
                                     placeholder = 'Write notes to store in text output file. Text can be inserted into report later.')
                     updateTextInput(session, 'notesModel', "Notes", value = "", 
                                     placeholder = 'Write notes to store in text output file. Text can be inserted into report later.')
                     updateTextInput(session, 'notesBook', "Notes", value = "", placeholder = 'Paste bookmarked URL here.')
                     
                     showNotification("Note saved.", type = 'message', duration = 5)
                   }
                 }, ignoreInit = TRUE)
  
  
  ### QAQC save ----
  
  get_reactive <- function(r, current_proj, proj_out) {
    
    if (!is.null(proj_out)) {
      
      if (proj_out == current_proj) {
        
        if (is.reactive(r)) r()
        else r
        
      } else NULL
    } else NULL
  }
  
  
  qaqc_outs <- eventReactive(c(input[["qaqc_tab-save_table"]],
                               input[["qaqc_plot-save_plot"]]), {
                                 list(
                                   summary_stats = get_reactive(tableInputSummary, project$name, qaqc_out_proj$sum_tab),
                                   outlier_table = get_reactive(tableInputOutlier, project$name, qaqc_out_proj$out_tab),
                                   outlier_plot = get_reactive(outlierPlotAll, project$name, qaqc_out_proj$out_plot), 
                                   spatial_qaqc = get_reactive(spat_qaqc, project$name, qaqc_out_proj$spat)
                                 )
                               })
  
  tabPlotServ("qaqc", proj = reactive(project$name),
              out = qaqc_outs, type = "tab_plot")
  
  
  ### Explore save ----
  
  explore_outs <- eventReactive(c(input[["explore_tab-save_table"]],
                                  input[["explore_plot-save_plot"]]), {
                                    
                                    list(
                                      temp_plot = get_reactive(plotInputTemporal, project$name, explore_out_proj$temp),
                                      map_plot = get_reactive(plotInputSpatial, project$name, explore_out_proj$spat),
                                      kernel_plot = get_reactive(plotInputKernel, project$name, explore_out_proj$kernel),
                                      getis_moran = get_reactive(gtmt_table, project$name, explore_out_proj$gtmt),
                                      xy_plot = get_reactive(plotInputXY, project$name, explore_out_proj$xy),
                                      view_grid_dat = get_reactive(grid_values$plot, project$name, explore_out_proj$grid)
                                    )
                                  })
  
  tabPlotServ("explore", proj = reactive(project$name),
              out = explore_outs, type = "tab_plot")
  
  ### Analysis save ----
  
  anal_outs <- eventReactive(c(input[["anal_tab-save_table"]],
                               input[["anal_plot-save_plot"]]), {
                                 list(
                                   corr_plot = get_reactive(inputCorr()$plot, project$name, anal_out_proj$corr),
                                   corr_tab = get_reactive(inputCorr()$table, project$name, anal_out_proj$corr),
                                   reg_plot = get_reactive(inputReg()$plot, project$name, anal_out_proj$reg)
                                 )
                               })
  
  anal_mods <- eventReactive(input[["anal-save_mod"]], {
    list(
      corr_test = get_reactive(corr_out_2, project$name, anal_out_proj$corr_out),
      reg_out = get_reactive(inputReg()$refout, project$name, anal_out_proj$reg)
    )
  })
  
  tabPlotServ("anal", proj = reactive(project$name),
              out = anal_outs, type = "tab_plot")
  
  modSaveServ("anal", proj = reactive(project$name), out = anal_mods)
  
  
  # confid pop up ----
  conf_rv <- reactiveValues(current_len = NULL,
                            last_len = NULL)
  
  conf_event <- reactive({
    
    # fleet functions
    f_nm <- grep("-fun_run", names(input), value = TRUE)
    f_nm <- f_nm[!("f_assign-fun_run" %in% f_nm)]
    
    vapply(f_nm, function(x) input[[x]], numeric(1))
  })
  
  observeEvent(c(conf_event(), input$col_select, input$date_select), {
    
    req(isTruthy(project$name))
    
    c_rule <- get_confid_check(project$name)
    
    if (!is.null(c_rule) && c_rule$check) {
      
      conf_rv$current_len <- length(get_confid_cache(project$name))
      
      if (conf_rv$current_len > conf_rv$last_len) { # if cache list increased, show popup
        
        conf_tab <- get_confid_cache(project$name, show = "last")
        c_lab <- paste(c_rule$rule, "=", c_rule$value)
        
        shinyjs::delay(500, {  # delay so conf_tab can load
          showModal(
            modalDialog(title = paste0("Confidential data detected (rule: ", c_lab,")"),
                        
                        DT::DTOutput("conf_table"),
                        
                        footer = tagList(
                          modalButton("Close")),
                        easyClose = TRUE
            )
          )
        })
        
        output$conf_table <- DT::renderDT(conf_tab)
      }
      
      conf_rv$last_len <- conf_rv$current_len
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE, priority = -1)
  
  
  ##---
  
  #Stop shiny ----
  ##---
  observeEvent(c(input$closeDat, input$closeQAQC, input$closeExplore, 
                 input$closeAnalysis, input$closeNew, input$altc_close, 
                 input$closeEC, input$closeModel, input$closeCM, input$closeB, 
                 input$closeRerun), {
                   stopApp()
                 }, ignoreInit = TRUE)
  
  ###---
  
  #Update From Bookmarked state----
  ###---   
  bookmarkedstate <- reactive({
    req(input$uploadbookmark)
    readRDS(input$uploadbookmark$datapath)
  })
  
  observe({
    req(input$uploadbookmark)
    req(bookmarkedstate()$loadDat==1)
    if(bookmarkedstate()$load_main_src=="FishSET database"){
      updateTextInput(session, 'projectname', value = bookmarkedstate()$projectname)
      #values$dataset <- table_view(paste0(project$name, 'MainDataTable'))
    }
  })
  
  observe({
    req(input$uploadbookmark)
    req(project$name)
    req(bookmarkedstate()$loadDat==1)
    if(bookmarkedstate()$load_main_src=="FishSET database"){
      values$dataset <- table_view(paste0(project$name, 'MainDataTable'), project$name)
    }
  })
  
  observeEvent(input$uploadbookmark, {
    req(project$name)
    if(colnames(values$dataset)[1]!='var1'){
      #---
      updateSelectInput(session, "altc_alt_var", selected = bookmarkedstate()$altc_alt_var)
      updateSelectInput(session, "exp_calc_method", selected = bookmarkedstate()$exp_calc_method)
      updateSelectInput(session, "cat", selected = bookmarkedstate()$cat)
      updateSelectInput(session, "cat_altc", selected = bookmarkedstate()$cat_altc)
      updateSelectInput(session, "cat_SL", selected = bookmarkedstate()$cat_SL)
      updateSelectInput(session, "catch", selected = bookmarkedstate()$catch)
      updateSelectInput(session, "exp_catch_var", selected = bookmarkedstate()$exp_catch_var)
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
      updateNumericInput(session, "mod_detail_report", value = bookmarkedstate()$mod_detail_report)
      updateSelectInput(session, "dist", selected = bookmarkedstate()$dist)
      updateSelectInput(session, "altc_dist", selected = bookmarkedstate()$altc_dist)
      updateSelectInput(session, "dummclosfunc", selected = bookmarkedstate()$dummclosfunc)
      updateCheckboxInput(session, "exp_dummy", value = bookmarkedstate()$exp_dummy)
      updateSelectInput(session, "dummyfunc", selected = bookmarkedstate()$dummyfunc)
      updateSelectInput(session, "dummypolydate", selected = bookmarkedstate()$dummypolydate)
      updateSelectInput(session, "dummypolyfunc", selected = bookmarkedstate()$dummypolyfunc)
      updateSelectInput(session, "dummyvarfunc", selected = bookmarkedstate()$dummyvarfunc)
      updateSelectInput(session, "dur_end", selected = bookmarkedstate()$dur_end)
      updateSelectInput(session, "dur_end2", selected = bookmarkedstate()$dur_end2)
      updateSelectInput(session, "dur_start", selected = bookmarkedstate()$dur_start)
      updateSelectInput(session, "dur_start2", selected = bookmarkedstate()$dur_start2)
      updateSelectInput(session, "dur_units", selected = bookmarkedstate()$dur_units)
      updateSelectInput(session, "exp_empty_catch", selected = bookmarkedstate()$exp_empty_catch)
      updateSelectInput(session, "empty_expectation", selected = bookmarkedstate()$empty_expectation)
      updateSelectInput(session, "end", selected = bookmarkedstate()$end)
      updateSelectInput(session, "end_latlon", selected = bookmarkedstate()$end_latlon)
      updateSelectInput(session, "ending_haul", selected = bookmarkedstate()$ending_haul)
      updateSelectInput(session, "ending_port", selected = bookmarkedstate()$ending_port)
      updateSelectInput(session, "fun_numeric", selected = bookmarkedstate()$fun_numeric)
      updateSelectInput(session, "fun_time", selected = bookmarkedstate()$fun_time)
      updateSelectInput(session, "mod_grid_vars", selected = bookmarkedstate()$mod_grid_vars)
      updateSelectInput(session, "exp_group", selected = bookmarkedstate()$exp_group)
      updateSelectInput(session, "haul_order", selected = bookmarkedstate()$haul_order)
      updateSelectInput(session, "Haul_Trip_IDVar", selected = bookmarkedstate()$Haul_Trip_IDVar)
      updateSelectInput(session, "haul_order_SL", selected = bookmarkedstate()$haul_order_SL)
      updateCheckboxInput(session, "hull_polygon_ac", value = bookmarkedstate()$hull_polygon_ac)
      updateSelectInput(session, "ID", selected = bookmarkedstate()$ID)
      updateSelectInput(session, "mod_ind_vars", selected = bookmarkedstate()$mod_ind_vars)
      updateSelectInput(session, "exp_lag_method", selected = bookmarkedstate()$exp_lag_method)
      updateSelectInput(session, "lat", selected = bookmarkedstate()$lat)
      updateSelectInput(session, "lat_dat_ac", selected = bookmarkedstate()$lat_dat_ac)
      updateSelectInput(session, "lat_dat_SL", selected = bookmarkedstate()$lat_dat_SL)
      updateSelectInput(session, "lat_grid_SL", selected = bookmarkedstate()$lat_grid_SL)
      updateSelectInput(session, "lat_grid_altc", selected = bookmarkedstate()$lat_grid_altc)
      updateCheckboxInput(session, "LatLon_Filter", value = bookmarkedstate()$LatLon_Filter)
      updateCheckboxInput(session, "mod_lockk", value = bookmarkedstate()$mod_lockk)
      updateSelectInput(session, "lon", selected = bookmarkedstate()$lon)
      updateSelectInput(session, "lon_dat", selected = bookmarkedstate()$lon_dat)
      updateSelectInput(session, "lon_dat_ac", selected = bookmarkedstate()$lon_dat_ac)
      updateSelectInput(session, "lon_dat_SL", selected = bookmarkedstate()$lon_dat_SL)
      updateSelectInput(session, "long_grid", selected = bookmarkedstate()$long_grid)
      updateSelectInput(session, "lon_grid_SL", selected = bookmarkedstate()$lon_grid_SL)
      updateSelectInput(session, "long_grid_altc", selected = bookmarkedstate()$long_grid_altc)
      updateSelectInput(session, "mid_end", selected = bookmarkedstate()$mid_end)
      updateSelectInput(session, "mid_start", selected = bookmarkedstate()$mid_start)
      updateNumericInput(session, "altc_min_haul", value = bookmarkedstate()$altc_min_haul)
      updateNumericInput(session, "mod_iter", value = bookmarkedstate()$mod_iter)
      updateSelectInput(session, "mtgtcat", selected = bookmarkedstate()$mtgtcat)
      updateSelectInput(session, "mtgtlonlat", selected = bookmarkedstate()$mtgtlonlat)
      updateSelectInput(session, "NA_Filter", selected = bookmarkedstate()$NA_Filter)
      updateSelectInput(session, "NAN_Filter", selected = bookmarkedstate()$NAN_Filter)
      updateSelectInput(session, "numfunc", selected = bookmarkedstate()$numfunc)
      updateSelectInput(session, "altc_occasion", selected = bookmarkedstate()$altc_occasion)
      updateSelectInput(session, "plot_table", selected = bookmarkedstate()$plot_table)
      updateSelectInput(session, "plot_type", selected = bookmarkedstate()$plot_type)
      updateTextInput(session, 'polyear', value = bookmarkedstate()$polyear)
      updateNumericInput(session, "mod_polyn", value = bookmarkedstate()$mod_polyn)
      updateSelectInput(session, "port_dat_dist", selected = bookmarkedstate()$port_dat_dist)
      updateSelectInput(session, "port_end", selected = bookmarkedstate()$port_end)
      updateSelectInput(session, "port_start", selected = bookmarkedstate()$port_start)
      updateSelectInput(session, "exp_price", selected = bookmarkedstate()$exp_price)
      updateSelectInput(session, "mod_price", selected = bookmarkedstate()$mod_price)
      updateTextInput(session, 'projectname', value = bookmarkedstate()$projectname)
      updateSelectInput(session, "p2fun", selected = bookmarkedstate()$p2fun)
      updateSelectInput(session, "p3fun", selected = bookmarkedstate()$p3fun)
      updateNumericInput(session, "quant_cat", value = bookmarkedstate()$quant_cat)
      updateNumericInput(session, "mod_relTolX", value = bookmarkedstate()$mod_relTolX)
      updateNumericInput(session, "mod_report_freq", value = bookmarkedstate()$mod_report_freq)
      updateSelectInput(session, "sp_col", selected = bookmarkedstate()$sp_col)
      updateSelectInput(session, "start", selected = bookmarkedstate()$start)
      updateSelectInput(session, "start_latlon", selected = bookmarkedstate()$start_latlon)
      updateSelectInput(session, "starting_haul", selected = bookmarkedstate()$starting_haul)
      updateSelectInput(session, "starting_port", selected = bookmarkedstate()$starting_port)
      updateSelectInput(session, "starting_port_SL", selected = bookmarkedstate()$starting_port_SL)
      updateSelectInput(session, "startloc", selected = bookmarkedstate()$startloc)
      updateTextInput(session, "target", value = bookmarkedstate()$target)
      updateNumericInput(session, "exp_temp_lag", value = bookmarkedstate()$exp_temp_lag)
      updateNumericInput(session, "exp_temp_window", value = bookmarkedstate()$exp_temp_window)
      updateSelectInput(session, "exp_temporal", selected = bookmarkedstate()$exp_temporal)
      updateNumericInput(session, "exp_temp_year", value = bookmarkedstate()$exp_temp_year)
      updateSelectInput(session, "exp_temp_var", selected = bookmarkedstate()$exp_temp_var)
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
      updateSelectInput(session, "xTime", selected = bookmarkedstate()$xTime)
      updateSelectInput(session, 'xWeight', selected = bookmarkedstate()$xWeight)
      updateSelectInput(session, "x_dist", selected = bookmarkedstate()$x_dist)
    }
    
    #---
  })
  
  #Rerun log -----
  
  output$new_dat_cb_choices <- renderUI({
    
    conditionalPanel("input.new_dat_cb",
                     selectInput("log", "Select a log file", choices = list_logs(project=project$name)),
                     
                     checkboxInput("new_dat_cb", "Run log with different data table"),
                     
                     selectizeInput("new_dat", "Choose primary table",  
                                    choices = main_tables(project$name), multiple = TRUE,
                                    options = list(maxItems = 1)), # sets dat to NULL by default
                     
                     selectizeInput("new_port", "Choose port table", 
                                    choices = list_tables(project = project$name, type = "port"), multiple = TRUE,
                                    options = list(maxItems = 1)),
                     
                     selectizeInput("new_aux", "Choose aux table", 
                                    choices = tables_database(project$name), multiple = TRUE,
                                    options = list(maxItems = 1)),
                     
                     selectizeInput("new_grid", "Choose gridded table", 
                                    choices = tables_database(project$name), multiple = TRUE,
                                    options = list(maxItems = 1)),
                     
                     selectizeInput("new_spat", "Choose spatial table", 
                                    choices = tables_database(project$name), multiple = TRUE,
                                    options = list(maxItems = 1))
    )
  })
  
  
  fetch_log <- reactive(log_rerun(input$log, run = FALSE))
  
  log_table_r <- reactive({
    
    log_file <- as.character(fetch_log()) # prevents evaluation when coerced to data.frame
    
    log_file <- tibble::rownames_to_column(as.data.frame(log_file))
    
    log_file <- tibble::remove_rownames(log_file)
    
    names(log_file) <- c("call order", "function call")
    
    log_file
  })
  
  output$log_table <- DT::renderDT(log_table_r(),
                                   selection = list(mode ='multiple', 
                                                    target = 'row'),
                                   rownames = FALSE)
  
  observeEvent(input$run_log, {
    
    log_rerun(input$log, dat = input$new_dat, portTable = input$new_port,
              aux = input$new_aux, gridfile = input$new_grid, spat = input$new_spat,
              ind = input$log_table_rows_selected, run = TRUE)
    
    showNotification("Log has been successfully rerun.", type = "message", duration = 10)
  })
  
  ###---
  
  onStop(function() {
    
    if (isTruthy(isolate(project$name))) {
      
      if (sum(isolate(c(input$callTextDownloadAnal,
                        input$callTextDownloadQAQC,
                        input$callTextDownloadExplore,
                        input[["fleet-callTextDownload"]],
                        input$callTextDownloadUp,
                        input$callTextDownloadNew,
                        input$callTextDownloadAlt,
                        input$callTextDownloadEC,
                        input$callTextDownloadModels,
                        input$callTextDownloadBook))) > 0) {
        
        notes_out <- unlist(isolate(savedText$answers))
        
        filename <- paste0(locoutput(project = project$name), isolate(project$name), "_notes_", Sys.Date(), ".txt")
        
        if (file.exists(filename)) {
          
          note_pd <- paste0(isolate(project$name), "_notes_", Sys.Date())
          
          note_int <- sum(grepl(note_pd, current_out(project$name)))
          
          writeLines(notes_out,
                     con = paste0(locoutput(project = project$name), isolate(project$name),
                                  "_notes_", Sys.Date(), "(", (note_int + 1), ").txt"))
          
        } else {
          
          writeLines(notes_out, con = filename)
        }
      }
    }
    
    # map viewer -- add check to see if run w/ servr::daemon_list()
    servr::daemon_stop()
  }) 
  
  
  
  # policy -----
  
  ### create reactive data frame 
  V <- reactiveValues(data = NULL)
  clicked_ids <- reactiveValues(ids = vector())
  closures <- reactiveValues()
  rv <- reactiveValues(edit = NULL)
  
  zone_closure_sideServer("policy", project = project$name, spatdat = spatdat$dataset)

  zone_closure_mapServer("policy", project =project$name, spatdat = spatdat$dataset, clicked_ids, V, closures, rv)
  
  zone_closure_tblServer("policy", project =project$name, spatdat = spatdat$dataset, clicked_ids, V)
  

}

