source("fleetServ.R", local = TRUE)
source("fleetUI.R", local = TRUE)
source("fleet_helpers.R", local = TRUE)
source("map_viewer_app.R", local = TRUE)

# default global search value
if(!exists("default_search")) {default_search <- ""}

# default column search values
if (!exists("default_search_columns")) {default_search_columns <- NULL}

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
      
      #inline scripting ----
      #---
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
      #---
      
      ##Pull data functions ----
      ##---
      values <- reactiveValues(
        dataset = data.frame('var1'=0, 'var2'=0)
        )
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
     
        
      
      #Landing Page ----
      ###---
      output$AcrossTabsText <- renderUI({
        if(input$QuickStartChoices=='AcrossTabs'){
          tags$div(
                 tags$br(), tags$br(),
                 tags$p('All tabs have the following elements:',
                  tags$ul(
                    tags$li('Buttons that enable you to close the app and refresh the data.', 
                            tags$ul('Refreshing the data pulls the original data loaded into the FishSET database. Instead of refreshing to the original state, you can refresh the 
                            data to an intermediate state by entering the name of the data table in the', tags$code('Optional text'),
                            'input box then the', tags$code('FishSET database'), 'radio button is chosen on the', tags$code('Upload Data'), 'tab. Intermediate 
                            data will contain a date in the table name, such as', tags$em('ExampleMainDataTable01012020.'))
                          ),
                    tags$li('Buttons that allow you to save plots and tables to the output folder in the FishSET folder.'), 
                    tags$li("A", tags$code('notes'), "section and a button to save notes to the output folder in the FishSET folder.")
                          ,
                    tags$li('An', tags$code('R expression'), 'area where you can enter and run R code. 
                        Within the FishSET Shiny application, the primary data is called', tags$em('values$dataset.')), 
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
           tags$p(tags$strong('Purpose:'), tags$br(), 'The', tags$em('Upload Data'), 'tab is used to load data (primary, port, map, gridded, auxiliary) from the FishSET database or 
                        from a local file location.'), 
	         tags$p("To get started, first write a project name in the", tags$code('Name of project'), "text box.",
	                tags$ul("The project name is a user-created unique identifier for all data tables  and outputs (plots, model results, etc) 
                 associated with the analysis. Example project names are 'pollock2019' and 'AKGOA'.")
	                ),
          tags$p('Next, load data.',
            tags$ul('To load from a local file location, select the', tags$code('Upload new file'), 'radio button and then browse to file location.'), 
	          tags$ul('To load from the FishSET database, select the', tags$code('FishSET database'), 'radio button. Fill out the optional', 
				tags$code('Name of data table in FishSET database'), 'text box if using a data table other than the original, unmodified table first loaded into the FishSET database.')
				), 
	         tags$p('Finally, press the', tags$code('Load data'), 'button.'),
           # tags$div(style="display: inline-block; align:center", img(src="upload.png",  height="75%", width="75%"))
			tags$br(), tags$br(),
			tags$p(tags$strong("Data:"),
			       tags$ul('primary (required)'), 
			       tags$ul('port (required)'),
			       tags$ul('spatial (required)'), 
			       tags$ul('auxiliary (optional)'), 
			       tags$ul('gridded (optional)'), 
			       tags$br(),
			       "The", tags$strong('primary data'), " file is a flat data file containing the core data used in models. It must contain at least one vector containing information on 
             ports (id, name), date (haul, trip start), catch amount (metric tons, kg), and fishing location (latitude/longitude, zone/area). 
			       Additional information such as price, species caught, and vessel characteristics may be included in the primary data file or added later. 
			       Each row of the primary data file should be a unique observation and each column a unique variable. Single or double apostrophes, commas other than as 
             CSV separators, and periods other than as decimal points, should not be included in the file. Use underscores rather than spaces in column names and use NA or leave 
             cells empty to represent no or missing data.",
			       tags$br(),tags$br(),
			       "The", tags$strong('port data'), "file contains the location (lat/lon) of ports in the primary data file and a variable containing port name or ID that links to the 
			       primary data file. Values in the port name variable must exactly match values in the primary data port variable. 
			       Check spelling, capitalization, and spaces if port data is not successfully merged into the primary dataset.
			       Location variables (latitude and longitude) must be in decimal degrees with cardinal direction indicated by sign.",
			       tags$br(), tags$br(),
			       "The", tags$strong('spatial or map data'), "file is an essential file that contains the latitude and longitude points defining fishery zone polygons. 
              The preferred file format is geojson but other formats are accepted. In FishSET, multiple zones with the same ID are combined and treated as a single zone,
              even if spatially separated. FishSET imports your map file as is, even if the zone outlines cover land. 
              The land will not be subtracted out by FishSET.",
			       tags$br(),tags$br(),
			      tags$strong("Auxiliary data"), "files are optional and can contain anything you want to merge with the primary data file within FishSET 
			       (ex. prices by date, vessel characteristics). Each column should be a unique variable and each row a unique observation. 
            Auxiliary data does not need to be at the haul or trip level but does need to contain a variable in common with 
			       the primary data file. Auxiliary data files are useful when a set of data is common across multiple primary data files, 
			       or as an efficient way to include data that is not haul or trip level specific.",
			       tags$br(),tags$br(),
			       "The", tags$strong("gridded data"), "is an optional file that contains a variable that varies by the map grid and, optionally, by a second 
              dimension (e.g., date/time). Both dimensions in the gridded data file need to be variables in the primary data file. 
              The grid locations (zones) must define the columns and the optional second dimension defines the rows. The row variable must have the exact name as the variable in the primary data file that it will be linked to. 
              Examples of gridded data include sea surface temperature, ice cover, and wind speed."
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
				unaltered state. Any previous actions will also be lost.'
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
            identifier variable already exists in the dataframe.',
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
                above the fleet table, then select a cell within a condition column, finally click', tags$code('Insert expression.'),
                tags$br(),
                'Rows and columns can be added to the table by clicking the', tags$code('Add row'), 'and', tags$code('Add column'), 
                'buttons.', tags$br(),
                'To remove a row, click the', tags$code('Select rows'), 'button, click on one or more rows, then click',
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
            the dataset.'),
            tags$br(),tags$br(),
            tags$strong('Summary Functions'), tags$br(),
            'The', tags$em('Fleet Summary'), 'subtab contains eight functions that displays summarized fleet data in plots 
            and tables. Use the', tags$code('Select function'), 'menu (arrow 1) to navigate between them.',
            tags$br(),tags$br(),
            tags$ul(tags$em('Vessel Count:'), 'Number of unique vessels', tags$br(),
              tags$em('Species Catch:'), 'Summarizes catch', tags$br(),
              tags$em('Rolling Catch:'), 'A moving window function summarizing catch over time', tags$br(),
              tags$em('Weekly Catch:'), 'Summarizes catch by week', tags$br(),
              tags$em('Bycatch:'), 'Compares species bycatch using CPUE and share of total catch', tags$br(),
              tags$em('Weekly Effort:'), 'Displays average daily CPUE by week', tags$br(),
              tags$em('Trip Length:'), 'Displays the distribution of trip duration', tags$br(),
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
                    in the dataset, plus three variables that can be created by the function: year, month, and week 
                    (date variable required).', tags$br(),
                    'For plots, each group variable is assigned to a color or line type. Which aesthetic a grouping variable 
                    is assigned to depends on the function, plot type, and the order in which the grouping variables are added. 
                    Generally, the first grouping variable is represented by color and the second group by line type. 
                    Barplots will not display a second grouping variable, but the variable will be included in the table output.', 
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
                                      and remove variables from the dataset that are redundant or 
                                      will not be used in analyses or modeling. In addition, the 
                                      other data types (plot, auxilliary, gridded), can also be viewed 
                                      and edited using the', tags$code('Select a dataset'), 'dropdown box.',
				    tags$br(),
				    tags$strong('Edit cells'), 'by double-clicking a cell.',
				    tags$br(),
			      tags$div(style="display: inline-block; align:center", img(src="Correct.png", height="75%", width="75%")),
			    	tags$br(),tags$br(),
			      tags$strong('Remove variables'), 'by clicking on a column, then clicking the', tags$code('Remove variable'), 'button.', 
              tags$br(), 'Save the edited data table to the FishSET database by clicking the', tags$code('Save data'), 
				      'button.', tags$br(), 'Press the', tags$code('Refresh data'), 'button to load the original, unaltered data table.',
				    tags$br(), 
			      tags$div(style="display: inline-block; align:center", img(src="DeleteVar.png", height="75%", width="75%")),
				    tags$br(),tags$br(),
			      tags$strong('Filter data'), 'using the', tags$em('boxes'), 'between the variable name the first row of data. Filters are saved when the', 
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
				    tags$code('Simple Analyses'), 'tab.'
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
            'The', tags$em('Compute New Variables'), 'tab is used to modify or create new variables such as CPUE or trip mid-point.',
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
				    'We describe first how to run functons, view the created variable, and save the altered data. We then describe the 
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
            'functions focus on creating variables based on numeric calculations (i.e., plus, minus) 
				          between two variables and catch per unit effort (CPUE).',
				tags$br(), tags$br(),
				tags$em('Data transformations'), 
            'functions focus on transforming data into coded variables. These functions can be used to 
				        transform confidential data.',
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
				        A spatial data file is required. ',
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
            'The Map Viewer requires a spatial data set containing zone polygons. Go to the', 
              tags$strong('Upload Data'), 'tab to load the file if necessary. The map viewer also requires  
            the zone identifier, which links the primary dataset to the spatial dataset. Go to the', 
            tags$strong('Zonal Definition'), 'tab to assign observations to zones if a zonal identifier does 
            not exist in the primary dataset.',
            tags$br(), tags$br(),
            'There are a number of required and suggested choices. Once all choices have been made, 
            press the green', tags$code('Run'), 'button. An interactive map will appear at the top of the page. 
            To display information on individual points in the top right-hand corner of the map, hover over 
            a point. Other informational plots are shown in the left-hand side of the map. This map cannot be saved.',
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
              tags$em('Temporal variables (recommended):'), 'Temporal variables to plot the numeric variable against. 
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
           'The', tags$em("Zonal Definition"),'tab is used to assign observations (e.g., hauls) to zones and define alternative fishing choices.',
				tags$br(), tags$br(),
	        'There are three sets of functions that can be run on this tab.',
				tags$br(), tags$br(),
	        tags$div(style="display: inline-block; align:center", img(src="zonal.png", height="75%", width="75%")),
				tags$br(), tags$br(),
	        tags$ol(	         
	           tags$li('(Required) Identify fishery zones or management areas, calculate zone or fishing centroids, and assign each observations 
                      in the main data set to zones. FishSET defaults to geographic centroids. 
					             To use fishing centroids, select a weighting variable in the weighted centroid box. Points that fall outside of 
	                    any zones can be assigned to the closest zone by checking the', tags$code('Use closest polygon to point'), 'box. 
	                    If spatial data creating polygons are sparse or irregular, the', tags$code('Use convex hull method'),'is recommended. 
                    '),
	           tags$br(),
	           tags$li('(Required) Define the alternative fishing choices. These choices are used to develop the matrix of distances 
	                   between observed and alternative fishing choices (where they could have fished but did not). 
                     The distance matrix can be generated from the data or from gridded data. We describe generating the distance matrix 
                     from the data first.',
	                   tags$br(),
                     'If the distance matrix comes from the primary haul-level data, the function requires defining how the 
                     starting location (in longitude and latitude) should be found. Choices are the centroid of the zone where the haul occurred,
                     port, or other lon/lat variable such as haul starting location. Next, define how to find 
                     the location of the alternative fishing locations. Choices are the centroid of 
                     each of the alternative zones or a lon/lat location in the primary dataset, such as haul ending 
                     location. The distance matrix is then calculated between defined starting and alternative choice locations. 
                     Distance can be returned in miles, kilometers, or meters.', 
	                   tags$br(),
	                   'Alternatively, the the distance matrix can be generated from a gridded dataset, such as sea surface temperature. 
	                   Columns in the gridded data file must be individual zones.',
            tags$br(),
                     'The number of observed hauls can vary considerably between zones. The histogram at the bottom of the page 
	                    is provided to assess variance in the amount of data between hauls. Observations from zones with insufficient zones can be 
	                   removed from analyses by setting the minimum haul number in the', tags$code('Include zones with more hauls than'), 'box.'),
				    tags$br(),
             tags$li('(Optional) Select catch and price variables. This can be done here or in the', tags$em('Expected Catch/Revenue'), 
					'or', tags$em('Models'), 'tabs.')
	         )
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
			      multiplied against catch to produce revenue. If a revenue variable exists in the dataset, it can be used for 
			      the', tags$code('Catch Variable.'), 'This matrix is required to run the conditional logit model.',
			  tags$br(), tags$br(),
				
			  #  'The primary choices are whether to treat data as a fleet or to group the data', tags$strong('defineGroup'), 'and the time
			  #  frame of catch data for calculating expected catch. Catch is averaged along a daily or sequential timeline',
			  #  tags$strong('temporal'), 'using a rolling average.', tags$strong('temp.window'), 'and tags$strong('temp.lat') determine the window size and
			  #  temporal lag of the window for averaging. Use tags$strong('temp_obs_table') before using this function to assess
			  #  the availability of data for the desired temporal moving window size. Sparse data is not suited for shorter moving
			  #  window sizes. For very sparse data, consider setting,' tags$strong('temp.var'), 'to NULL and excluding temporal patterns in
			  #  catch.', tags$br(),
			  #  'Empty catch values are considered to be times of no fishing activity. Values of 0 in the catch variable
			  #  are considered times when fishing activity occurred but with no catch. These points are included in the averaging
			  #  and dummy creation as points in time when fishing occurred.' 
			  tags$br(), tags$br(),
            'The function returns four expected catch or expected revenue matrices based on selected parameters:', 
				tags$br(),
				    tags$ul('selected temporal parameters'),
            tags$ul('expected catch/revenue based the previous two days (short-term) catch,'), 
            tags$ul('expected catch/revenue based the previous seven days (medium-term) catch,'), 
            tags$ul('and expected catch/revenue based the previous years (long-term) catch.'),

        tags$br(),tags$br(),	
				'There are a number of choices. The first choice is whether to calculate expected catch/revenue over the entire dataset (“fleet”) 
        or within groups using the', tags$code('Choose viable that defines group'), 'dropdown box.', 
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
        calculated catch for each zone given the observed date (if specified) and group (if specified) in the primary dataset.',
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
            the output in the', tags$code('Compare models'), 'substab.',
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
            'Purpose:', tags$br(),
            'The', tags$em('Bookmark Choices'), 'tabs is to save choices made in the FishSET R Shiny application and enable current application state to be reloaded at a later date.',
				tags$br(), tags$br(),
				    'Reloading a bookmarked state will restore the last selections in the application. The data will not be automatically loaded 
            and no functions will be applied to the data. It is best to save the data before bookmarking the current application state. 
				    After the application is reloaded, load the saved data.',  
				    tags$br(), tags$br(),
            'To bookmark the application, click the', tags$code('bookmark'), 'button. Click', tags$em('Dismiss'), 'in the popup message.',
            tags$div(style="display: inline-block; align:center", img(src="Dismiss.png", height="75%", width="75%")),
				tags$br(), tags$br(),tags$br(),
            'To reload a perviously saved application state, you must have the FishSET R application open. Navigate to the', tags$code('Bookmark Choices'), 
        'tab. Click the', tags$code('Browse'), 'button and then migrate to the', tags$em('input.rds'), 'file.',
				tags$br(), tags$br(),
            tags$div(style="display: inline-block; align:center", img(src="Bookmark2.png", height="75%", width="75%"))
            )
        }
      })
      ###---
      
      #DATA UPLOAD FUNCTIONS ----
      ###---
      
      output$main_upload <- renderUI({     
        tagList( 
          conditionalPanel("input.loadmainsource=='Upload new file'", 
                           tagList(
                             fluidRow(
                                 column(5, fileInput("maindat", "Choose primary data file",
                                            multiple = FALSE, placeholder = 'Required data'))#,
                               # column(2, uiOutput('ui.action'))
                           ))
          ),     
          conditionalPanel("input.loadmainsource=='FishSET database'", 
                             fluidRow(
                               column(5, textInput("maindatabasedat", "Name of data table in FishSET database",
                                                   value='', placeholder = 'Optional. Use if loading modified data table'))
                               
                             )
          ))
      })
      #output$ui.action <- renderUI({
      #  if(is.null(input$maindat)) return()
      #  actionButton("uploadMain", label = "Save to database", 
      #               style = "color: white; background-color: blue;", size = "extra-small")
      #})
 
      output$ui.action2 <- renderUI({
        if(is.null(input$maindat)) return()
        tagList(
          textInput('compare', label=div(style = "font-size:14px;  font-weight: 400;", 
		  'If comparing data to previous year, enter saved table name'), 
                    value='', placeholder = 'Saved table name in FishSET database'),
          checkboxInput('over_write','If file exsits, over write?', value=FALSE)
        )
      })
     #Add in reactive values once data  call is is not empty
      observeEvent(input$loadDat, {
        if(input$projectname==''){
          showNotification("Please enter a project name.", type='message', duration=10)
        }
        req(input$projectname)
        if(input$loadmainsource=='FishSET database'){
          if(table_exists(paste0(input$projectname, 'MainDataTable'))==FALSE){
            showNotification('Primary data table not found in FishSET database. Check project spelling.', type='message', duration=15)
          } else {
        values$dataset <- table_view(paste0(input$projectname, 'MainDataTable'))
        dat_name <<- paste0(input$projectname, 'MainDataTable')
          }
        } else if(input$loadmainsource=='Upload new file' & !is.null(input$maindat)){
           values$dataset <- read_dat(input$maindat$datapath)
           df_y <- input$compare
          df_compare <- ifelse(nchar(input$compare)>0, TRUE, FALSE)
          q_test <- quietly_test(load_maindata)
          q_test(values$dataset, over_write=input$over_write, project=input$projectname, compare=df_compare, y=df_y)
          dat_name <<- paste0(input$projectname, 'MainDataTable')

#           if(input$uploadMain == 0){
#             showNotification('Data not saved to database. Press the Save to Database button.', type='warning', duration=20)
#           }
          # warning('')
       }   else {
          values$dataset <- values$dataset
       }
        if(names(values$dataset)[1]!='var1'){
          showNotification("Primary data loaded.", type='message', duration=10)
        }
      }, ignoreInit = TRUE, ignoreNULL = TRUE) 
      
#      observeEvent(input$uploadMain, {
#        if(input$loadmainsource=='Upload new file'){
#           values$dataset <- read_dat(input$maindat$datapath)
#        } else {
#          values$dataset <- values$dataset
#        }
#        if(names(values$dataset)[1]!='var1'){
#          showNotification("Data loaded.", type='message', duration=10)
#        }
#      }, ignoreInit = TRUE, ignoreNULL = TRUE) 
      
#      observeEvent(input$uploadMain, {
#        df_data <- read_dat(input$maindat$datapath)
#      })

     #PORT
      output$port_upload <- renderUI({     
        tagList( 
          conditionalPanel(condition="input.loadportsource=='Upload new file'", 
                           tagList(
                             fluidRow(
                               column(5, fileInput("portdat", "Choose port data file",
                                                   multiple = FALSE, placeholder = 'Required data'))#,
                               #column(1, uiOutput('ui.actionP'))
                             ))
          )#,
          #conditionalPanel(condition="input.loadportsource!='Upload new file'", 
          #                 tagList(
          #                   fluidRow(
          #                     column(5, textInput("portdattext", "Port data file name in database", placeholder = 'Optional. Provide if name '))
          #                   ))
          )#)
      })
      #output$ui.actionP <- renderUI({
      #  if(is.null(input$portdat)) return()
       # actionButton("uploadPort", label = "Save to database", 
      #               style = "color: white; background-color: blue;", size = "extra-small")
      #})
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

      ptdat <- reactiveValues(
        dataset = data.frame('var1'=0, 'var2'=0)
      )
      
      ptdat_temp <- reactiveValues(
        dataset = data.frame('var1' = 0, 'var2' = 0)
      )
      
      show <- reactiveValues(port_combine = FALSE, port_merge = FALSE, save = FALSE)
      
      observeEvent(input$loadDat, { 
        
        if (input$projectname == '') {
          
          showNotification("Please enter a project name.", type = 'message', duration = 10)
        }
        
        req(input$projectname)
        
        if (input$loadportsource == 'FishSET database') {
          
          if (table_exists(paste0(input$projectname, 'PortTable')) == FALSE) {
            
            showNotification('Table not found in FishSET database. Check project spelling.', 
                             type = 'message', duration = 15)
            
          } else {
            
            ptdat$dataset <- table_view(paste0(input$projectname, 'PortTable'))
            port_name <<- paste0(input$projectname, 'PortTable')
          }
          
        } else if (input$loadportsource == 'Upload new file' & !is.null(input$portdat)) {
          # skip new file upload if user already merged multiple tables
          if (is.null(input$port_combine_save)) {
            
            ptdat$dataset <- read_dat(input$portdat$datapath)
            q_test <- quietly_test(load_port)
            q_test(ptdat$dataset, port_name = input$port_name, over_write = TRUE, 
                   project = input$projectname, compare = FALSE, y = NULL)
            showNotification("Port data saved to database.", type = "message", 
                             duration = 10)
            port_name <<- paste0(input$projectname, 'PortTable')
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
                  reactive(input$projectname), merge_type = "full", 
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
        
        q_test <- quietly_test(load_port)
        q_test(ptdat$dataset, port_name = "Port_Name", over_write = TRUE, 
               project = input$projectname, compare = FALSE, y = NULL)
        
        showNotification("Combined port table saved to database.", type = "message", 
                         duration = 10)
        # so column names match with DB version
        ptdat$dataset <- table_view(paste0(input$projectname, 'PortTable'))  
      
        port_name <<- paste0(input$projectname, 'PortTable')
        show$save <- FALSE
        show$port_merge <- FALSE
        
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
      

  #Spatial
      output$spatial_upload <- renderUI({     
        tagList( 
          conditionalPanel(condition="input.loadspatialsource=='Upload new file'", 
                           tagList(
                             fluidRow(
                               column(5, fileInput("spatialdat", "Choose spatial data file",
                                                   multiple = FALSE, placeholder = 'Suggested data'))#,
                              # column(1, uiOutput('ui.actionS'))
                             ))
          ),
          conditionalPanel(condition="input.loadspatialsource!='Upload new file'", 
                           tagList(
                             fluidRow(
                               column(5, textInput("spatialdattext", "Spatial data table name in database", placeholder = 'Suggested data'))
                             ))
          ))
      })
#      output$ui.actionS <- renderUI({
#        if(is.null(input$spatialdat)) return()
#        actionButton("uploadspatial", label = "Save to database", 
#                     style = "color: white; background-color: blue;", size = "extra-small")
#      })

      spatdat <- reactiveValues(
        dataset = data.frame('var1'=0, 'var2'=0)
      )
      
      observeEvent(input$loadDat, {
        if(input$loadspatialsource=='FishSET database'){
          spatdat$dataset <- table_view(input$spatialdattext)
        } else if(input$loadspatialsource=='Upload new file' & !is.null(input$spatialdat)){
          spatdat$dataset <- read_dat(input$spatialdat$datapath, is.map=TRUE)
          #fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase())
          #DBI::dbWriteTable(fishset_db, input$spatialdat$name,  spatdat$dataset, overwrite=TRUE) 
          #DBI::dbDisconnect(fishset_db)
          #showNotification("Map saved to database")
          } else {
          spatdat$dataset <- spatdat$dataset
          }
        if(names(spatdat$dataset)[1]!='var1'){
          showNotification("Map file loaded but not currently able to save to database.", type='message', duration=10)
        }
      }, ignoreInit = TRUE, ignoreNULL = TRUE) 


 #GRIDDED      
      output$grid_upload <- renderUI({     
        tagList( 
          conditionalPanel(condition="input.loadgridsource=='Upload new file'", 
                           tagList(
                             fluidRow(
                               column(5, fileInput("griddat", "Choose data file that varies over two dimensions (gridded)",
                                                   multiple = FALSE, placeholder = 'Optional data')),
                               column(5, uiOutput('ui.actionG'))
                             ))
          ),
          conditionalPanel(condition="input.loadgridsource!='Upload new file'", 
                           tagList(
                             fluidRow(
                               column(5, textInput("griddattext", "Gridded data table name in database", placeholder = 'Name must be provided.'))
                             ))
          ))
      })
      output$ui.actionG <- renderUI({
        if(is.null(input$griddat)) return()
     #   tagList(
          textInput("GridName", "Grid table name" )#,
     #     actionButton("uploadGrid", label = "Save to database", 
     #                  style = "color: white; background-color: blue;", size = "extra-small")
     #   )
      })
      
      grddat <- reactiveValues()
      
      observeEvent(input$loadDat, {
        
        if (length(names(grddat)) == 0) {
          grid_name <<- list() 
        }
        
        if (input$loadgridsource == 'FishSET database') {
          
          req(input$griddattext)
          
          grid_name_app <- paste0(input$projectname, input$griddattext)
          grddat[[grid_name_app]] <- table_view(grid_name_app)
          grid_name[[grid_name_app]] <<- grid_name_app 
          
        } else if (input$loadgridsource == 'Upload new file' & !is.null(input$griddat)) {
          
          showNotification('Gridded data saved to database.', type = 'message', duration = 10)
          grid_name_app <- paste0(input$projectname, input$GridName)
          grddat[[grid_name_app]] <- read_dat(input$griddat$datapath)        
          
          q_test <- quietly_test(load_grid)
          
          q_test(paste0(input$projectname, 'MainDataTable'), grid = grddat[[grid_name_app]], 
                 x = input$GridName, over_write = TRUE, project = input$projectname)
          
          grid_name[[grid_name_app]] <<- grid_name_app
        }
        
        if (length(names(grddat)) > 0) {
          
          showNotification("Gridded data loaded.", type = 'message', duration = 10)
        }
        
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
      
      output$gridded_uploaded <- renderUI({
        tagList(
          p(strong("Gridded data tables uploaded:")),
          renderText(paste(names(grddat), collapse = ", "))
        )
      })
    
      #Auxiliary      
      output$ui.actionA <- renderUI({
        if(is.null(input$auxdat)) return()
        tagList(
        textInput("AuxName", "Auxiliary table name." ),
        #actionButton("uploadAux", label = "Save to database", 
        #             style = "color: white; background-color: blue;", size = "extra-small"),
        )
      })
      
      output$aux_upload <- renderUI({     
        tagList( 
          conditionalPanel(condition="input.loadauxsource=='Upload new file'", 
                           tagList(
                             fluidRow(
                               column(5, fileInput("auxdat", "Choose auxiliary data file that links to primary data",
                                                   multiple = FALSE, placeholder = 'Optional data')),
                               column(5, uiOutput('ui.actionA'))
                             ))
          ),
          conditionalPanel(condition = "input.loadauxsource !== 'Upload new file'", 
                           tagList(
                             fluidRow(
                               column(5, textInput("auxdattext", "Auxiliary data table name in database", placeholder = 'Optional data'))))
          )
          )
      })

      aux <- reactiveValues(
        dataset = data.frame('var1'=0, 'var2'=0)
      )
      
      observeEvent(input$loadDat, {
        #req(input$auxdattext)
        if(input$loadauxsource=='FishSET database'){
          aux$dataset <- table_view(paste0(input$projectname, input$auxdattext))
          aux_name <<- paste0(input$projectname, input$auxdattext)
        } else if(input$loadauxsource=='Upload new file' & !is.null(input$auxdat)){
            showNotification('Auxiliary data saved to FishSET database.', type = 'message', duration = 10)
           aux$dataset <-read_dat(input$auxdat$datapath)
           q_test <- quietly_test(load_aux)
            q_test(paste0(input$projectname, 'MainDataTable'), aux=aux$dataset, x = input$AuxName, over_write=TRUE,
                   project=input$projectname)
            aux_name <<- paste0(input$projectname, input$auxdattext)
        } else {
          aux$dataset <- aux$dataset
          }
        if(names(aux$dataset)[1]!='var1'){
          showNotification("Auxiliary data loaded.", type='message', duration=10)
        }
      }, ignoreInit = TRUE, ignoreNULL = TRUE) 
      
      ###---
      #MERGE ----
      ###---      
      # Merge aux with main ---
      mergeServer("aux", values, aux, reactive(input$projectname), 
                  merge_type = "left", dat_type = "aux")
      
      
      ###---
      
      #DATA QUALITY FUNCTIONS ----
      ###---     
#      output$checks_dataset <- renderUI({
#        if (input$SelectDatasetDQ == "main") {
#          radioButtons("checks", "", choices = c('Variable class', 'Summary table', 'Outliers', 'NAs', 'NaNs', 'Unique observations', #                                                 'Empty variables', 'Lat_Lon units'))
#        } else {
#          radioButtons("checks", "", choices = c('Summary table'))
#        }
#      })
      
      #change variable class
      m <- reactive({
        if(colnames(values$dataset)[1] == 'var1') {
          return(NULL)
        } else if(input$checks=='Variable class'){
          int = t(t(sapply(values$dataset, class)))
          df = matrix(
            as.character(1:7), nrow = nrow(int), ncol = 7, byrow = TRUE,
            dimnames = list(rownames(int), c('class', 'first value', 'no changes', 'numeric', 'character', 'factor', 'date'))
          )
          for(i in seq_len(nrow(df))) {
            for(j in seq_len(ncol(df))) {
              df[i, j] = sprintf(
                '<input type="radio" name="%s" value="%s" %s/>',
                rownames(int)[i], df[i, j], ifelse(j==3, 'checked="checked"', "")
              )
            }
          }
          df[,1] = int  
          df[,2] = t(values$dataset[1,])
          return(df)
        } else {
          NULL
        }
      })
        
      changecode <- reactive({
        if(input$checks=='Variable class'){
          g <- c('class', 'first value', 'no changes', 'numeric', 'character', 'factor', 'date')
          g <- g[as.numeric(as.vector(sapply(names(values$dataset), function(i) input[[i]])))]
        } else {
          NULL
        }
      })   
      
      output$changetable <- DT::renderDataTable(
        if(colnames(values$dataset)[1] == 'var1') {
          return(NULL)
        } else if(input$checks=='Variable class'){
          m()
          } else {
            NULL
          },
        escape = FALSE, selection = 'single', server = FALSE,
        options = list(dom = 't', paging = FALSE, ordering = FALSE),
        callback = DT::JS("table.rows().every(function(i, tab, row) {
          var $this = $(this.node());
          $this.attr('id', this.data()[0]);
          $this.addClass('shiny-input-radiogroup');
        });
        Shiny.unbindAll(table.table().node());
        Shiny.bindAll(table.table().node());")
      )
      
      changecode <- reactive({
        if(colnames(values$dataset)[1] == 'var1') {
          return(NULL)
        } else if(input$checks=='Variable class'){
          g <- c('class', 'first value', 'no changes', 'numeric', 'character', 'factor', 'date')
          g <- g[as.numeric(as.vector(sapply(names(values$dataset), function(i) input[[i]])))]
        } else {
          NULL
        } 
      })     
      
      observeEvent(input$rchclass, {
        xn <- which(changecode()!="no changes")
        x <- colnames(values$dataset)[xn]
        newclass <- changecode()[xn]
        values$dataset <- changeclass(values$dataset, project=input$projectname, x=x, newclass=newclass, savedat=FALSE)
        showNotification('Variable class changed.', type='message', duration=10)
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
      
      tableInputOutlier <- reactive({
        if(colnames(values$dataset)[1] == 'var1') {
          return(NULL)
        } else if (input$checks=='Outliers') {
          table <- outlier_table(values$dataset, project=input$projectname, x=input$column_check)
          rownames(table)=table[,2]
          table <- table[,3:10]
          #table <<- table
        } else {
          NULL
        }
      })
      
      #Lat/Lon
      output$LatLonDir <- renderUI ({
        tagList(
        conditionalPanel(condition="input.checks=='Lat_Lon units'",
                         selectizeInput('LatDirection','Latitudinal variable', 
                                        choices=c('None', colnames(values$dataset[,grep('lat', names(values$dataset), ignore.case=TRUE)])),
                                        options = list(create = TRUE, placeholder='Select or type variable name'))),
        conditionalPanel(condition="input.checks=='Lat_Lon units'",
                         selectizeInput('LonDirection','Longitudinal variable', 
                                        choices=c('None', colnames(values$dataset[,grep('lon', names(values$dataset), ignore.case=TRUE)])),
                                        options = list(create = TRUE, placeholder='Select or type variable name')))
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
            
      ##Check UI
      
      ##Output to main panel
      output$Case<-renderPrint({
        if(input$checks == 'Variable class'){
          "Check and change variable data classes."
        }else if(input$checks=='Summary table') {
          "Summary table of NUMERIC variables in data set."
        } else if(input$checks=='Outliers'){
          if(input$dat.remove=='none'){
            HTML('Table to assess outliers.', input$column_check, "shown. <br>Zoom in to plot by highlighting desired area and double clicking. <br>Double click again to reset plot. <br>")
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
          na_filter(values$dataset, x=names(which(apply(values$dataset, 2, function(x) anyNA(x))==TRUE)), 
                    replace = FALSE, remove = FALSE, rep.value=NA, over_write=FALSE)
        } else if(input$checks=='NaNs'){
          nan_filter(values$dataset, x=names(which(apply(values$dataset, 2, function(x) any(is.nan(x)))==TRUE)), 
                     replace = FALSE, remove = FALSE, rep.value=NA,  over_write=FALSE)
        } else if(input$checks=='Unique observations'){
          unique_filter(values$dataset, project = input$projectname, remove=FALSE)
        } else if(input$checks=='Empty variables'){
          empty_vars_filter(values$dataset, project = input$projectname, remove=FALSE)

        } else if(input$checks=='Lat_Lon units'){
          degree(values$dataset, lat=NULL, lon=NULL, latsign=NULL, lonsign=NULL, replace=FALSE)
        } else {
          'Make a selection in the left hand column'
        } 
      })
      
      case_to_print <- reactiveValues(dataQuality = logical(0),
                                      explore = logical(0),
                                      analysis = logical(0))
      
      observeEvent(c(input$checks, input$column_check, input$dat.remove, input$x_dist,
                     input$plot_table, input$plot_type, input$col_select, input$x_y_select1, input$x_y_select2,
                     input$corr_reg, input$corr_select, input$reg_resp_select, input$reg_exp_select), {
        
        if(input$tabs=='qaqc'){
          if(input$checks=='Summary table') {
           case_to_print$dataQuality <- c(case_to_print$dataQuality, "Summary table of numeric variables viewed.\n")
          } else if(input$checks=='Outliers') {
            if(input$dat.remove=='none'){
              case_to_print$dataQuality <- c(case_to_print$dataQuality, paste0('Table and plots to assess outliers viewed for ', input$column_check, ".\n"))
            } else {
              case_to_print$dataQuality <- c(case_to_print$dataQuality, paste('Table and plot to assess outliers viewed for', input$column_check, 'with',
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
                    }, "removed.\n"))
            }
          } else if(input$checks=='NAs'){
            if(any(apply(values$dataset, 2, function(x) anyNA(x)))==TRUE) {
              if(input$NA_Filter_all==0&input$NA_Filter_mean==0){
                #case_to_print$dataQuality <- c(case_to_print$dataQuality, 
                g <- paste("Occurrence of missing values checked. The", sub(",([^,]*)$", ", and\\1",paste(names(which(apply(values$dataset, 2, function(x) any(is.na(x)))==TRUE)), collapse = ", ")),
                      "variables contain",  sub(",([^,]*)$", ", and\\1", paste(apply(values$dataset[,names(which(apply(values$dataset, 2, function(x) anyNA(x))==TRUE))], 2, 
                                                                                     function(x) length(which(is.na(x)==TRUE))), collapse=", ")), 
                      "missing values, respectively.", length(unique(unlist(apply(values$dataset[,names(which(apply(values$dataset, 2, 
                                                                function(x) anyNA(x))==TRUE))], 2, function(x) which(is.na(x)==TRUE))))), "rows have missing values. Missing values were not removed or replaced.\n")#) 
              }} else {
                if(input$NA_Filter_all==0&input$NA_Filter_mean==0){
                 # case_to_print$dataQuality <- c(case_to_print$dataQuality, 
                  g <- paste("Occurrence of missing values checked. No columns in the data set contain missing values.\n")#)
                } else {
                  if(input$NA_Filter_all>0){
                    #case_to_print$dataQuality <- c(case_to_print$dataQuality, 
                   g<- paste("Occurrence of missing values checked. The", sub(",([^,]*)$", ", and\\1",paste(names(which(apply(values$dataset, 2, function(x) anyNA(x))==TRUE)), collapse = ", ")), "variables contained", sub(",([^,]*)$", ", and\\1", paste(apply(values$dataset[,names(which(apply(values$dataset, 2, function(x) anyNA(x))==TRUE))], 2, 
                                                                                                                                                                                                                                                                function(x) length(which(is.na(x)==TRUE))), collapse=", ")), "missing values.", length(unique(unlist(apply(values$dataset[,names(which(apply(values$dataset, 2, 
                                                                                                                                                                                                                                                                                                                                                                                                             function(x) anyNA(x))==TRUE))], 2, function(x) which(is.na(x)==TRUE))))), "rows containing missing values were removed from the data set.\n")#)
                  } else if(input$NA_Filter_mean>0){
                    #case_to_print$dataQuality <- c(case_to_print$dataQuality, 
                    g <- paste("Occurrence of missing values checked. The", sub(",([^,]*)$", ", and\\1",paste(names(which(apply(values$dataset, 2, function(x) anyNA(x))==TRUE)), collapse = ", ")), "variables contained", sub(",([^,]*)$", ", and\\1", paste(apply(values$dataset[,names(which(apply(values$dataset, 2, function(x) anyNA(x))==TRUE))], 2, 
                                                                                                                                                                                                                                                                function(x) length(which(is.na(x)==TRUE))), collapse=", ")), "missing values. Missing values were replaced with the mean values of", names(which(apply(values$dataset, 2, function(x) anyNA(x))==TRUE)), "respectively.\n")#)
                  }
                } }
 
            if(any(apply(values$dataset, 2, function(x) any(is.nan(x))))==TRUE) {
              if(input$NAN_Filter_all==0 & input$NAN_Filter_mean==0){
                case_to_print$dataQuality <- c(case_to_print$dataQuality, paste(g, "Occurruence of non-numbers checked. The", sub(",([^,]*)$", ", and\\1",paste(names(which(apply(values$dataset, 2, function(x) any(is.nan(x)))==TRUE)), collapse = ", ")),
                      "variables contain", 
                      sub(",([^,]*)$", ", and\\1", paste(apply(values$dataset[,names(which(apply(values$dataset, 2, function(x) any(is.nan(x)))==TRUE))], 2, 
                                                               function(x) length(which(is.nan(x)==TRUE))), collapse=", ")), "non-numbers, respectively.", 
                      length(unique(unlist(apply(values$dataset[,names(which(apply(values$dataset, 2, 
                                                                                   function(x) any(is.nan(x)))==TRUE))], 2, function(x) which(is.nan(x)==TRUE))))), "rows have non-numbers. No action was taken to remove or replace non-numbers.\n")) 
              }} else {
                if(input$NAN_Filter_all==0&input$NAN_Filter_mean==0){
                  case_to_print$dataQuality <- c(case_to_print$dataQuality, paste(g, "Occurruence of non-numbers checked. No columns in the data set contain non-numbers.\n"))
                } else {
                  if(input$NAN_Filter_all>0){
                    case_to_print$dataQuality <- c(case_to_print$dataQuality, paste(g, "Occurruence of non-numbers checked. The", sub(",([^,]*)$", ", and\\1",paste(names(which(apply(values$dataset, 2, function(x) any(is.nan(x)))==TRUE)), collapse = ", ")), "variables contained", 
                          sub(",([^,]*)$", ", and\\1", paste(apply(values$dataset[,names(which(apply(values$dataset, 2, function(x) any(is.nan(x)))==TRUE))], 2, 
                                                                   function(x) length(which(is.nan(x)==TRUE))), collapse=", ")), "non-numbers.", 
                          length(unique(unlist(apply(values$dataset[,names(which(apply(values$dataset, 2, 
                                                                                       function(x) any(is.nan(x)))==TRUE))], 2, function(x) which(is.nan(x)==TRUE))))), "rows containing non-numbers were removed from the data set.\n"))
                  } else if(input$NAN_Filter_mean>0){
                    case_to_print$dataQuality <- c(case_to_print$dataQuality, paste(g, "Occurruence of non-numbers checked. The", sub(",([^,]*)$", ", and\\1",paste(names(which(apply(values$dataset, 2, function(x) any(is.nan(x)))==TRUE)), collapse = ", ")), "variables contained", 
                          sub(",([^,]*)$", ", and\\1", paste(apply(values$dataset[,names(which(apply(values$dataset, 2, function(x) any(is.nan(x)))==TRUE))], 2, 
                                                                   function(x) length(which(is.nan(x)==TRUE))), collapse=", ")), "non-numbers.\n"))
                  }
                } }
          } else if(input$checks=='Unique observations'){
            if(dim(values$dataset)[1] == dim(unique(values$dataset))[1]) {
              case_to_print$dataQuality <- c(case_to_print$dataQuality, "Each row is a unique choice occurrence.\n")
            } else {
              if(input$Unique_Filter==0){
                case_to_print$dataQuality <- c(case_to_print$dataQuality, "Each row in data set is not a unique choice occurrence at haul or trip level. No action taken.\n")
              } else {
                case_to_print$dataQuality <- c(case_to_print$dataQuality, "Duplicate choice occurrence at haul or trip level existed in the data set and have been removed.\n")
              }
            }
          } else if(input$checks=='Empty variables'){
            if(any(apply(values$dataset, 2, function(x) all(is.na(x))) == TRUE)) {
              if(input$Empty_Filter==0){
                case_to_print$dataQuality <- c(case_to_print$dataQuality, paste('Occurrence of empty variables was checked and the', names(which(apply(values$dataset, 2, function(x) all(is.na(x))) == TRUE)), 
                      "variable is empty. The varible was not removed from the data set.\n"))
              } else {
                case_to_print$dataQuality <- c(case_to_print$dataQuality, paste('Occurrence of empty variables was checked and the', names(which(apply(values$dataset, 2, function(x) all(is.na(x))) == TRUE)), 
                      "was empty and was removed from the data set.\n"))
              }
            } else {
              case_to_print$dataQuality <- c(case_to_print$dataQuality, "Occurrence of empty variables was checked and not found in the data set.\n")
            }
          } else if(input$checks=='Lat_Lon units'){
            if(any(apply(values$dataset[,grep('lat|lon', names(values$dataset), ignore.case=TRUE)], 2, function(x) !is.numeric(x))==TRUE)==TRUE){
              if(input$LatLon_Filter==FALSE){
                case_to_print$dataQuality <- c(case_to_print$dataQuality, 'Latitude and longitude units were checked and are not in decimal degrees.\n')
              } else {
                case_to_print$dataQuality <- c(case_to_print$dataQuality, 'Latitude or longitude units not in decimal degrees were converted to decimal degrees.\n')
              }
            } else {
              case_to_print$dataQuality <- c(case_to_print$dataQuality, 'Latitude and longitude units were checked and are in decimal degrees.\n')
            }
          }
        } else if(input$tabs=='explore'){
          if(input$plot_table=='Plots'& input$plot_type=='Temporal'){
            case_to_print$explore <- c(case_to_print$explore, paste0("Viewed plots of ", input$col_select, ' against time for raw points, the ', input$p2fun, ", and the ",  input$p3fun, ' value.\n'))
          } else if(input$plot_table=='Plots'& input$plot_type=='Spatial'){
            case_to_print$explore <- c(case_to_print$explore, paste0("Viewed spatial distribution of occurrence points and spatial density of occurrence points.\n
                   Getis-ord and Moran's I statistics provided for ", input$varofint, ". Default settings for spdep functions are used."))
          } else if(input$plot_table=='Plots'& input$plot_type=='x-y plot'){
            case_to_print$explore <- c(case_to_print$explore, paste0("Viewed plotted relationship between ", input$x_y_select1,  ' and ', input$x_y_select2, '.\n'))
          } 
        } else if(input$tabs=='analysis'){
          if(input$corr_reg=='Correlation'){
            case_to_print$analysis <- c(case_to_print$analysis, paste0("Viewed correlation matrix for ",  isolate({sub(",([^,]*)$", ", and\\1",paste(input$corr_select, collapse = ", "))}), '.\n'))
          } else if(input$corr_reg=='Regression'){
            case_to_print$analysis <- c(case_to_print$analysis, paste0('Viewed plot and linear regression test output for ',input$reg_exp_select, ' on ', input$reg_resp_select,'.\n')) 
          } 
        }
      })
# Notes ===
      
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
                            zone = "Zone definition: ",
                            ec = "Expected catch/revenue: ",
                            models = "Models: ",
                            book = "Bookmark URL: ")
      
      observeEvent(c(input$callTextDownload,
                     input$callTextDownloadAnal,
                     input$callTextDownloadExplore,
                     input$callTextDownloadUp,
                     input$callTextDownloadNew,
                     input[["fleet-callTextDownload"]],
                     input$callTextDownloadZone,
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
        } else if (input$tabs == 'zone') {
          if (!is.null(input$notesZone)) {
            notes$zone <- c(notes$zone, paste0(input$notesZone, "\n"))
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
    #End NOTES
      
      ##Table output
      tableInputSummary <- reactive({
        
        temp <- #switch(input$SelectDatasetDQ, 
                       #"main" = 
          values$dataset#, 
                       #"port" = ptdat$dataset, 
                       #"grid" = grddat$dataset, 
                       #"auxiliary" = aux$dataset)
        
        if(colnames(temp)[1] == 'var1') {
          return(NULL)
        } else if(input$checks=='Summary table'|input$checks=='NAs') { 
          #temp <- values$dataset
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
      

      
      output$output_table_outlier <- DT::renderDT(
        if(colnames(values$dataset)[1] == 'var1') {
          return(NULL)
        } else if(input$checks=='Outliers'){
          table <- outlier_table(values$dataset, project=input$projectname, x=input$column_check)
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
        

      ##Filtering options
      #output_table())
      
      
      observeEvent(input$NA_Filter_all,{
          if(any(apply(values$dataset, 2, function(x) anyNA(x)))==TRUE){
            values$dataset <- na_filter(values$dataset, x=names(which(apply(values$dataset, 2, function(x) anyNA(x))==TRUE)), 
                                        replace = FALSE, remove = TRUE, rep.value=NA, over_write=FALSE)  
          } else {
            values$dataset <- values$dataset
            cat('No missing values to remove')
          }
        })
      
      observeEvent(input$NA_Filter_mean,{
          if(any(apply(values$dataset, 2, function(x) anyNA(x)))==TRUE){
            values$dataset <- na_filter(values$dataset,  x=names(which(apply(values$dataset, 2, function(x) anyNA(x))==TRUE)), 
                                        replace = TRUE, remove = FALSE, rep.value=NA, over_write=FALSE)
          } else {
            values$dataset <- values$dataset
            cat('No missing values to remove')
          }
      })
      
      observeEvent(input$NAN_Filter_all,{
          if(any(apply(values$dataset, 2, function(x) any(is.nan(x))))==TRUE){
            values$dataset <- nan_filter(values$dataset, x=names(which(apply(values$dataset, 2, function(x) any(is.nan(x)))==TRUE)), 
                                         replace = FALSE, remove = TRUE, rep.value=NA, over_write=FALSE)  
          } else{
            values$dataset <- values$dataset
            print('No non-numbers to remove.')
          }
      })
      
      observeEvent(input$NAN_Filter_mean,{
          if(any(apply(values$dataset, 2, function(x) any(is.nan(x))))==TRUE){
            values$dataset <- nan_filter(values$dataset, x=names(which(apply(values$dataset, 2, function(x) any(is.nan(x)))==TRUE)), 
                                         replace = TRUE, remove = FALSE, rep.value=NA, over_write=FALSE)
          } else {
            values$dataset <- values$dataset
            print('No non-numbers to remove.')
          }
      })
      
      observeEvent(input$Outlier_Filter,{
          values$dataset <- outlier_remove(values$dataset, x=input$column_check, dat.remove = input$dat.remove, over_write=FALSE)
      })
      
      observeEvent(input$Unique_Filter,{
        values$dataset <- unique_filter(values$dataset, project=input$projectname, remove=TRUE)
      })
      
      observeEvent(input$Empty_Filter,{
        values$dataset <- empty_vars_filter(values$dataset,  project=input$projectname, remove=TRUE)            
        })
      
      observeEvent(input$LatLon_Filter, {
            values$dataset <- degree(values$dataset, 
                                     if(input$LatDirection=='None') { lat=NULL } else { lat=input$LatDirection},
                                     if(input$LonDirection=='None') { lon=NULL } else { lon=input$LonDirection},
                                     if(input$input$LatLon_Filter_Lat=='None') { latsign=NULL } else { latsign=input$input$LatLon_Filter_Lat},
                                     if(input$input$LatLon_Filter_Lon=='None') { lonsign=NULL } else { lonsign=input$input$LatLon_Filter_Lon}, 
                                     replace=TRUE
                                      ) 
      })
      
      ##---        

      #DATA EXPLORATION FUNCTIONS ----
      ###---
      #1. TABLE
      
      output$SelectDatasetExploreUI <- renderUI({
        
        tagList(
          selectInput("SelectDatasetExplore", "Select a dataset", 
                      choices = c("main", "port", "auxiliary", "gridded" = "grid")),
          
          conditionalPanel("input.SelectDatasetExplore == 'grid'",
                           selectInput("grid_select", "select gridded table", 
                                       choices = names(grddat))
          )
        )
        
      })
  
      explore_temp <- reactive({
          
        if (input$SelectDatasetExplore != "grid") {
          
          switch(input$SelectDatasetExplore,
                 "main" = values$dataset,
                 "port" = ptdat$dataset,
                 "auxiliary" = aux$dataset)
        
        } else {
          
         grddat[[input$grid_select]] 
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
        options = list(autoWidth=TRUE, scrolly=T, responsive=TRUE, pageLength = 15,
                       searchCols = default_search_columns, buttons = c('csv'))
      )
      
      observeEvent(input$saveData,{
        # when it updates, save the search strings so they're not lost
          # update global search and column search strings
          #default_search_columns <- c("", input$output_table_exploration_search_columns)
        default_search_columns <- c(input$output_table_exploration_search_columns)
        default_sub <- which(default_search_columns!='')
        
        table_type <- #switch(input$SelectDatasetExplore, 
                             #"main" = 
                                "MainDataTable"#,
                             #"port" = "PortTable",
                             #"grid" = input$GridName,
                             #"auxiliary" = input$AuxName)
         
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
                FilterTable <- rbind(FilterTable, c(paste0(input$projectname, table_type), (colnames(explore_temp()[default_sub])[i]), 
                                                    paste(colnames(explore_temp()[default_sub])[i], '>', as.numeric(sapply(strsplit(default_search_columns[default_sub[i]], "\\..\\."), head, 1)), '&', 
                                                          colnames(explore_temp()[default_sub])[i], '<', as.numeric(sapply(strsplit(default_search_columns[default_sub[i]], "\\..\\."), tail, 1)))))
              } else {
                FilterTable <- rbind(FilterTable, c(paste0(input$projectname, table_type), (colnames(explore_temp()[default_sub])[i]), 
                                                    paste0("grepl('", default_search_columns[default_sub[i]],"', ", colnames(explore_temp()[default_sub])[i],")")))
              }
            }
            
            showNotification('Filter table saved to FishSET database', type='message', duration=10)
            
            filter_data_function <- list()
            filter_data_function$functionID <- 'filter_table'
            filter_data_function$args <- c(paste0(input$projectname, table_type), input$projectname, FilterTable$vector[nrow(FilterTable)],  FilterTable$FilterFunction[nrow(FilterTable)])
            filter_data_function$kwargs <- list()
            filter_data_function$output <- c('')
            filter_data_function$msg <- FilterTable
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
      #  tags$div(align = 'left', class = 'multicol', 
                 selectInput("col_select", "Select variable", choices = names(values$dataset), 
                              selected = numeric_cols(values$dataset)[1], 
                              multiple=FALSE, selectize = TRUE)#)
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
                        lubridate::year(date_parser(values$dataset[,grep('date', colnames(values$dataset), ignore.case = TRUE)[1]])), FUN=length)
          } else {
            aggregate(values$dataset[[input$col_select]]~
                        lubridate::month(date_parser(values$dataset[,grep('date', colnames(values$dataset), ignore.case = TRUE)[1]])), FUN=length)
          }
        } else if(input$p2fun=='No. unique observations'){
          if(length(unique(lubridate::year(date_parser(values$dataset[,grep('date', colnames(values$dataset), ignore.case = TRUE)[1]]))))>1){
            aggregate(values$dataset[[input$col_select]]~
                        lubridate::year(date_parser(values$dataset[,grep('date', colnames(values$dataset), ignore.case = TRUE)[1]])), FUN=function(x) length(unique(x)))
          } else {
            aggregate(values$dataset[[input$col_select]]~
                        lubridate::month(date_parser(values$dataset[,grep('date', colnames(values$dataset), ignore.case = TRUE)[1]])), FUN=function(x) length(unique(x)))
          }
        } else {
          if(length(unique(lubridate::year(date_parser(values$dataset[,grep('date', colnames(values$dataset), ignore.case = TRUE)[1]]))))>1){
            aggregate(values$dataset[[input$col_select]]~
                        lubridate::year(date_parser(values$dataset[,grep('date', colnames(values$dataset), ignore.case = TRUE)[1]])), FUN=function(x) round(length(x)/nrow(values$dataset)*100,2))
          } else {
            aggregate(values$dataset[[input$col_select]]~
                        lubridate::month(date_parser(values$dataset[,grep('date', colnames(values$dataset), ignore.case = TRUE)[1]])), FUN=function(x) round(length(x)/nrow(values$dataset)*100,2))
          }
        }
      })
      
      df2m = reactive({
        if(length(unique(lubridate::year(date_parser(values$dataset[,grep('date', colnames(values$dataset), ignore.case = TRUE)[1]]))))>1){
          aggregate(values$dataset[[input$col_select]]~
                      lubridate::year(date_parser(values$dataset[,grep('date', colnames(values$dataset), ignore.case = TRUE)[1]])), FUN=input$p3fun, na.rm=T)
        } else {
          aggregate(values$dataset[[input$col_select]]~
                      lubridate::month(date_parser(values$dataset[,grep('date', colnames(values$dataset), ignore.case = TRUE)[1]])), FUN=input$p3fun, na.rm=T)
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
        ranges_spatial$x <- c(ifelse((min(values$dataset[, which(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)')==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)')))[1]], na.rm=TRUE)-abs(min(values$dataset[, which(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)')==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)')))[1]], na.rm=TRUE)/10) < -180), 
                                     -180, min(values$dataset[, which(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)')==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)')))[1]], na.rm=TRUE)-abs(min(values$dataset[, which(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)')==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)')))[1]], na.rm=TRUE)/10)), 
                              ifelse((max(values$dataset[, which(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)')==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)')))[1]], na.rm=TRUE)+abs(max(values$dataset[, which(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)')==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)')))[1]], na.rm=TRUE)/10) > 180), 
                                     180, max(values$dataset[, which(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)')==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)')))[1]], na.rm=TRUE)+abs(max(values$dataset[, which(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)')==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LON|Lon|lon)')))[1]], na.rm=TRUE)/10))) 
        ranges_spatial$y <-c(ifelse((min(values$dataset[, which(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)')==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)')))[1]], na.rm=TRUE)-abs(min(values$dataset[, which(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)')==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)')))[1]], na.rm=TRUE)/10) < -90), 
                                    -90, min(values$dataset[, which(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)')==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)')))[1]], na.rm=TRUE)-abs(min(values$dataset[, which(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)')==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)')))[1]], na.rm=TRUE)/10)), 
                             ifelse((max(values$dataset[, which(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)')==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)')))[1]], na.rm=TRUE)+abs(max(values$dataset[, which(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)')==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)')))[1]], na.rm=TRUE)/10) > 90),
                                    90, max(values$dataset[, which(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)')==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)')))[1]], na.rm=TRUE)+abs(max(values$dataset[, which(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)')==max(stringi::stri_count_regex(colnames(values$dataset), '(?=LAT|Lat|lat)')))[1]], na.rm=TRUE)/10)))
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
          conditionalPanel("input.SelectDatasetExplore=='main' && input.plot_table=='Plots' && input.plot_type=='Spatial'",
                           style = "margin-left:19px;", selectizeInput('varofint', 'Variable to test for spatial autocorrelation',
                                                                    choices=colnames(values$dataset[,sapply(values$dataset,is.numeric)]))),
          conditionalPanel("input.SelectDatasetExplore=='main' && input.plot_table=='Plots' && input.plot_type=='Spatial'",
                           style = "margin-left:19px;",  selectizeInput('gtmt_lonlat', 'Select lat then lon from data frame to assign observations to zone', 
                                                                     choices=c(NULL, names(values$dataset)[grep('lon|lat', names(values$dataset), ignore.case=TRUE)]), 
                                                                     multiple = TRUE, options = list(maxItems = 2, create = TRUE, placeholder='Select or type variable name')))#, 
        #  conditionalPanel(condition="input.plot_table=='Plots'&input.plot_type=='Spatial'",
        #                   style = "margin-left:19px;", fileInput("gtmtfileGrid", "Choose file defining area/zone polygons", multiple = FALSE)) 
        )
      })    
      output$mtgt_out2 <- renderUI({
        tagList(
          conditionalPanel("input.SelectDatasetExplore=='main' && input.plot_table=='Plots' && input.plot_type=='Spatial'",
                           style = "margin-left:19px;", selectInput('mtgtcat',  "Variable defining zones or areas", 
                                                                    choices= c('', names(as.data.frame(spatdat$dataset))), selected='')),
          conditionalPanel("input.SelectDatasetExplore=='main' && input.plot_table=='Plots' && input.plot_type=='Spatial'",
                           style = "margin-left:19px;", selectizeInput('mtgtlonlat', 'Select vector containing latitude then longitude from spatial data frame', 
                                                                    choices= c(NULL, names(as.data.frame(spatdat$dataset))), multiple=TRUE,
                                                                    options = list(maxItems = 2,create = TRUE, placeholder='Select or type variable name')))
        )
      })
      gtmt_table <- reactive({
        if(input$mtgtcat==''){
          return( NULL)
        } else {
          gt <- getis_ord_stats(values$dataset, project=input$projectname, varofint=input$varofint, spat=spatdat$dataset, 
                                lon.dat=input$gtmt_lonlat[2], lat.dat=input$gtmt_lonlat[1], cat=input$mtgtcat, lon.grid=input$mtgtlonlat[2], lat.grid=input$mtgtlonlat[1])$getistable
          mt <- moran_stats(values$dataset, project=input$projectname, varofint=input$varofint, spat=spatdat$dataset, 
                            lon.dat=input$gtmt_lonlat[2], lat.dat=input$gtmt_lonlat[1], cat=input$mtgtcat, lon.grid=input$mtgtlonlat[2], lat.grid=input$mtgtlonlat[1])$morantable
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
      
      output$plot_grid_args <- renderUI({
        tagList(
          actionButton("run_grid", "Plot grid",
                       style = "color: #fff; background-color: #6da363; border-color: #800000;"),
          selectInput("grid_lon", "Longitude", 
                      choices = FishSET:::find_lon(grddat[[input$grid_select]])),
          selectInput("grid_lat", "Latitude", 
                      choices = FishSET:::find_lat(grddat[[input$grid_select]])),
          selectInput("grid_value", "Value", 
                      choices = numeric_cols(grddat[[input$grid_select]])),
          selectInput("grid_split", "Split plot by",
                      choices = c("none", colnames(grddat[[input$grid_select]]))),
          selectInput("grid_agg", "Group mean value by",
                      choices = c("latlon", colnames(grddat[[input$grid_select]])),
                      multiple = TRUE)
        )
      })
      
      grid_values <- reactiveValues(plot = FALSE,
                                    bbox = FALSE,
                                    gmap = FALSE)
      
      observeEvent(input$run_grid, {
        
        if (is.logical(grid_values$bbox)) {
          
          grid_values$bbox <- ggmap::make_bbox(grddat[[input$grid_select]][[input$grid_lon]], 
                                               grddat[[input$grid_select]][[input$grid_lat]])
          
          grid_values$gmap <- retrieve_map(grddat[[input$grid_select]], 
                                           input$grid_lon, 
                                           input$grid_lat) 
          
        } else {
          
          incoming_bbox <- ggmap::make_bbox(grddat[[input$grid_select]][[input$grid_lon]], 
                                            grddat[[input$grid_select]][[input$grid_lat]])
          
          same_bbox <- all(round(grid_values$bbox) == round(incoming_bbox))
          
          if (same_bbox == FALSE) { # retrieve new map if bbox changes, else keep existing map
            grid_values$gmap <- retrieve_map(grddat[[input$grid_select]],
                                             input$grid_lon, 
                                             input$grid_lat) 
            
            grid_values$bbox <- incoming_bbox # update bbox
          }
        }
        
        grid_values$plot <-
          view_grid_dat(gridfile = grddat[[input$grid_select]], project = input$projectname,
                      lon = input$grid_lon, lat = input$grid_lat,
                      value = input$grid_value, split_by = input$grid_split,
                      agg_by = input$grid_agg, gmap = grid_values$gmap)
      })
      
      output$grid_plot <- renderPlot(grid_values$plot)
      
      
      # Fleet Functions ========
      
      fleet_id <- reactive({
        switch(input$fleet_fun, "vessel_count" = "ves", "species_catch" = "spec",
               "roll_catch" = "roll", "weekly_catch" = "wc", "weekly_effort" = "we",
               "bycatch" = "by", "trip_length" = "trip", "density_plot" = "den")
      })
      # Save buttons
      output$fleetSaveOutputUI <- renderUI({
        saveOutputUI(paste0(fleet_id(), "-saveOut"))
        })

      saveDataTableServ("fleet", values, reactive(input$projectname))

      closeAppServ("fleet")

      refreshServ("fleet", values, reactive(input$projectname))
      
      output$run_fleet_fun <- renderUI(runFunUI(fleet_id()))
      
      # server modules
      density_serv("den", values, reactive(input$projectname))

      vessel_serv("ves",  values, reactive(input$projectname))

      species_serv("spec", values, reactive(input$projectname))

      roll_serv("roll", values, reactive(input$projectname))

      weekly_catch_serv("wc", values, reactive(input$projectname))

      weekly_effort_serv("we", values, reactive(input$projectname))

      bycatch_serv("by", values, reactive(input$projectname))

      trip_serv("trip", values, reactive(input$projectname))

      fleet_table_serv("f_table", values, reactive(input$projectname))

      fleet_assign_serv("f_assign", values, reactive(input$projectname))

      # R expr output
      #RexpressionServ("fleet", values)
      
      observeEvent(input$runFleet, {
        shinyjs::hide("error")
        r$ok <- FALSE
        tryCatch(
          {
            r$output <- isolate(
              paste(capture.output(eval(parse(text = input$exprFleet))), collapse = '\n')
            )
            r$ok <- TRUE
          },
          error = function(err) {r$output <- err$message}
        )
        r$done <- r$done + 1
      })
      output$resultFleet <- renderUI({
        if(r$done > 0 ) { 
          content <- paste(paste(">", isolate(input$exprFleet)), r$output, sep = '\n')
          if(r$ok) {
            pre(content)
          } else {
            pre( style = "color: red; font-weight: bold;", content)
          }
        }
      })
      
      

      
      
      ##
      ###---    
      
      #DATA ANALYSIS FUNCTIONS ----
      ###---
      output$corr_out <- renderUI({
        selectInput('corr_select', 'Select variables to include in correlation test', 
                    choices= names(which(lapply(values$dataset[,which(lapply(values$dataset, is.numeric)==TRUE)], var, na.rm=TRUE)>0)), 
                    selected= names(which(lapply(values$dataset[,which(lapply(values$dataset, is.numeric)==TRUE)], var, na.rm=TRUE)>0)), multiple=TRUE, selectize=TRUE, width='90%')
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
          return()
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
        varSelectInput('reg_resp_select', 'Select response variable', data = values$dataset[,unlist(lapply(values$dataset, is.numeric))], 
                       multiple = FALSE, selectize = TRUE)
      })
      
      output$reg_exp_out <- renderUI({
        varSelectInput('reg_exp_select', 'Select explanatory variable', data= values$dataset, 
                    selected= "", multiple=FALSE, selectize=TRUE)
      })
      
      ## Add regression component
      #Run model
      p2 <- reactive({
        summary(lm(values$dataset[[input$reg_resp_select]]~values$dataset[[input$reg_exp_select]]))
        })
      
      output$output_text_reg <- renderPrint(
        p2()
      )
      
      plotInputreg <- reactive({
        if(colnames(values$dataset)[1] == 'var1') {
          return(NULL)
        } else if(length(input$reg_exp_select)!=1){
          return(NULL)
        }  else {
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
      ###---
      
      #DATA CREATION/MODIFICATION FUNCTIONS----
      ###---
      #Transformations 
      output$trans_time_out <- renderUI({
        conditionalPanel("input.VarCreateTop=='Temporal functions'&&input.tempfunc=='temp_mod'",
                         style = "margin-left:19px;", selectizeInput('TimeVar','Select variable',
                                                                  choices=c(colnames(values$dataset)[grep('date|hour|time|year', colnames(values$dataset), ignore.case=TRUE)]), 
                                                                  options=list(create=TRUE, placeholder='Select or type input')))
      })
      output$trans_quant_name <-  renderUI({
        conditionalPanel("input.VarCreateTop=='Data transformations'&&input.trans=='set_quants'",
                         style = "margin-left:19px;", selectInput('trans_var_name','Select variable', 
                                                                  choices=names(values$dataset[,unlist(lapply(values$dataset, is.numeric))]),
                                                                  multiple=FALSE, selectize=TRUE))
      })
      output$unique_col_id <- renderUI({
        conditionalPanel("input.VarCreateTop=='Nominal ID'&&input.ID=='ID_var'",
                         style = "margin-left:19px;", selectInput('unique_identifier','Variables that identify unique observations',
                                                                  choices=colnames(values$dataset), multiple=TRUE, selectize=TRUE),
                         selectInput('ID_type', "Select ID column class type",
                                     choices = c("string", "integer")))
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
      
      seasonalData <- reactive({
        if(is.null(input$seasonal.dat)){return()} 
        type <- sub('.*\\.', '', input$seasonal.dat$name)
        if(type == 'shp') { type <- 'shape'} else if(type == 'RData') { type <- 'R'} else { type <- type}
        g <- read_dat(input$seasonal.dat$datapath, type)
        return(g)
      })
      output$sp_col.select<- renderUI({
        conditionalPanel("input.VarCreateTop=='Nominal ID'&&input.ID=='create_seasonal_ID'",
                         style = "margin-left:19px;",  selectInput('sp_col', "Column containing species names in table containing seasonal data", 
                                                                   choices=names(seasonalData())
                                                                   , multiple = FALSE, selectize=TRUE))
      })
      output$var_x_select <- renderUI({
        conditionalPanel("input.VarCreateTop=='Arithmetic functions'&&input.numfunc=='create_var_num'",
                         style = "margin-left:19px;", selectInput('var_x', 'First variable. Will be the numerator if dividing.', 
                                                                  choices=names(values$dataset[,unlist(lapply(values$dataset, is.numeric))]), selectize=TRUE))
      })
      output$var_y_select <- renderUI({
        conditionalPanel("input.VarCreateTop=='Arithmetic functions'&&input.numfunc=='create_var_num'",
                         style = "margin-left:19px;", selectInput('var_y', 'Second variable. Will be the denomenator if dividing.',  
                                                                  choices=names(values$dataset[,unlist(lapply(values$dataset, is.numeric))]), selectize=TRUE))
      })
      output$input_xWeight <- renderUI({
        conditionalPanel("input.VarCreateTop=='Arithmetic functions'&&input.numfunc=='cpue'",
                         style = "margin-left:19px;", selectizeInput('xWeight','Weight variable', 
                                                                  choices=names(values$dataset[,grep("mt|lb|ton|pound|weight|metric|kilo|mass", names(values$dataset), ignore.case = TRUE)]),
                                                                  options = list(create = TRUE, placeholder='Select or type variable name')))
      })
      output$input_xTime <- renderUI({
        conditionalPanel("input.VarCreateTop=='Arithmetic functions'&&input.numfunc=='cpue'",
                         style = "margin-left:19px;", selectInput('xTime','Duration. To calculate duration, select the Calculate Duration option.', 
                                                                  choices=c('Calculate duration', names(values$dataset[,unlist(lapply(values$dataset, is.numeric))])), selectize=TRUE))
      })
      output$dur_add <- renderUI({
        tagList(
          conditionalPanel("input.VarCreateTop=='Arithmetic functions'&&input.numfunc=='cpue'&&input.xTime=='Calculate duration'",
                           style = "margin-left:19px;", 
                           selectizeInput('dur_start2', 'Variable indicating start of time period', 
                                                                    choices = names(values$dataset[,grep("date|min|hour|week|month|TRIP_START|TRIP_END", names(values$dataset), ignore.case = TRUE)]),
                                                                    options = list(create = TRUE, placeholder='Select or type variable name'))),
          conditionalPanel("input.VarCreateTop=='Arithmetic functions'&&input.numfunc=='cpue'&&input.xTime=='Calculate duration'",         
                           style = "margin-left:19px;", 
                           selectizeInput('dur_end2', 'Variable indicating end of time period', 
                                                       choices = names(values$dataset[,grep("date|min|hour|week|month|TRIP_START|TRIP_END", names(values$dataset), ignore.case = TRUE)]),
                                                       options = list(create = TRUE, placeholder='Select or type variable name'))),
          conditionalPanel("input.VarCreateTop=='Arithmetic functions'&&input.numfunc=='cpue'&&input.xTime=='Calculate duration'",          
                           style = "margin-left:19px;", 
                           selectInput('dur_units2', 'Unit of time for calculating duration', 
                                       choices = c("week", "day", "hour", "minute")))
        )
      })
      output$input_dur_start <- renderUI({
        conditionalPanel("input.VarCreateTop=='Temporal functions'&&input.tempfunc=='create_duration'",
                         style = "margin-left:19px;", selectizeInput('dur_start', 'Variable indicating start of time period', 
                                                                  choices = names(values$dataset[,grep("date|min|hour|week|month|TRIP_START|TRIP_END", names(values$dataset), ignore.case = TRUE)]), 
                                                                  options = list(create = TRUE, placeholder='Select or type variable name')))
      })
      output$input_dur_end <- renderUI({
        conditionalPanel("input.VarCreateTop=='Temporal functions'&&input.tempfunc=='create_duration'",
                         style = "margin-left:19px;", selectizeInput('dur_end', 'Variable indicating end of time period', 
                                                                  choices = names(values$dataset[,grep("date|min|hour|week|month|TRIP_START|TRIP_END", names(values$dataset), ignore.case = TRUE)]),
                                                                  options = list(create = TRUE, placeholder='Select or type variable name')))
      })
      output$dist_between_input <- renderUI({
        tagList(
         if(names(spatdat$dataset)[1]=='var1'){
           conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_dist_between'",
                              tags$div(h4('Map file not loaded. Please load on Upload Data tab', style="color:red")))
          },
         conditionalPanel("input.VarCreateTop=='Spatial functions'&&input.dist=='create_dist_between'",
                           style = "margin-left:19px;", selectInput('start', 'Starting location',choices = c('Zonal centroid', 'Port', 'Lat/lon coordinates'))),
          conditionalPanel("input.VarCreateTop=='Spatial functions'&&input.dist=='create_dist_between'",
                           style = "margin-left:19px;", selectInput('end', 'Ending location', choices = c('Zonal centroid', 'Port', 'Lat/lon coordinates'))),
          #Port
          conditionalPanel("(input.VarCreateTop=='Spatial functions'&&input.dist=='create_dist_between'&&input.start=='Port')||
                           (input.VarCreateTop=='Spatial functions'&&input.dist=='create_dist_between'&&input.end=='Port')" ,
                           style = "margin-left:19px;", selectInput("filePort", "Choose file from the FishSET database containing port data", 
                                                                    choices=tables_database()[grep('port', tables_database(), ignore.case=TRUE)], multiple = FALSE)),
#port
          conditionalPanel("input.VarCreateTop=='Spatial functions'&&input.dist=='create_dist_between'&&input.start=='Port'",
                           style = "margin-left:19px;", selectInput('port_start', 'Variable containing port name at starting location', 
                                                                    choices=names(values$dataset[,grep('port', names(values$dataset), ignore.case=T)]), selectize=TRUE)),
          conditionalPanel("input.VarCreateTop=='Spatial functions'&&input.dist=='create_dist_between'&&input.end=='Port'" ,
                           style = "margin-left:19px;", selectInput('port_end', 'Variable containing port name at ending location', 
                                                                    choices=names(values$dataset[,grep('port', names(values$dataset), ignore.case=T)]), selectize=TRUE)),
          # fileInput("filePort", "Choose file containing port data",    
          #Zonal
          #coords
          conditionalPanel("input.VarCreateTop=='Spatial functions'&&input.dist=='create_dist_between'&&input.start=='Lat/lon coordinates'" ,
                           style = "margin-left:19px;", selectizeInput('start_latlon', 'Select lat then lon for starting location', 
                                                                    choices=names(values$dataset[,grep('lat|lon', names(values$dataset), ignore.case=T)]), 
                                                                    multiple=TRUE, options = list(maxItems = 2, create = TRUE, placeholder='Select or type variable name'))), 
          conditionalPanel("input.VarCreateTop=='Spatial functions'&&input.dist=='create_dist_between'&&input.end=='Lat/lon coordinates'" ,
                           style = "margin-left:19px;", selectizeInput('end_latlon', 'Select lat then lon for ending location', 
                                                                    choices=names(values$dataset[,grep('lat|lon', names(values$dataset), ignore.case=T)]), 
                                                                    multiple=TRUE, options = list(maxItems = 2, create = TRUE, placeholder='Select or type variable name')))
          
          
        )#)  
      })   
      output$dist_betwn_opts <- renderUI({
        tagList(
           conditionalPanel("(input.VarCreateTop=='Spatial functions'&&input.dist=='create_dist_between'&&input.start=='Zonal centroid')||
                           (input.VarCreateTop=='Spatial functions'&&input.dist=='create_dist_between'&&input.end=='Zonal centroid')" ,
                           style = "margin-left:19px;",  selectizeInput('lon_dat', 'Select lat then lon from data set to assign observations to zone', 
                                                                     choices=names(values$dataset[,grep('lat|lon', names(values$dataset), ignore.case=T)]),
                                                                     multiple=TRUE, options = list(maxItems = 2, create = TRUE, placeholder='Select or type variable name'))),
            conditionalPanel("input.VarCreateTop=='Spatial functions'&&input.dist=='create_dist_between'&&input.start=='Zonal centroid'||
                           (input.VarCreateTop=='Spatial functions'&&input.dist=='create_dist_between'&&input.end=='Zonal centroid')" ,
                           style = "margin-left:19px;", selectInput('cat', 'Individual areas/zones from the spatial data drame', choices=names(as.data.frame(spatdat$dataset)))),
          if(any(class(spatdat$dataset)=='sf')==FALSE){
            conditionalPanel("input.VarCreateTop=='Spatial functions'&&input.dist=='create_dist_between'&&input.start=='Zonal centroid'||
                             (input.VarCreateTop=='Spatial functions'&&input.dist=='create_dist_between'&&input.end=='Zonal centroid')" , 
                             style = "margin-left:19px;", selectizeInput('long_grid', 'Select vector containing latitude then longitude from spatial data set',
                                                                      choices=names(as.data.frame(spatdat$dataset)), multiple=TRUE, 
                                                                      options = list(maxItems = 2,create = TRUE, placeholder='Select or type variable name')))
          }
         
      )
      })
      output$start_mid_input <- renderUI({
        conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&&input.dist=='create_mid_haul'",
                         style = "margin-left:19px;", selectizeInput('mid_start','Select Lat then Lon that define starting locations',multiple = TRUE,
                                                                  choices = names(values$dataset[,grep('lat|lon', names(values$dataset), ignore.case=TRUE)]), 
                                                                  options = list(maxItems = 2, create = TRUE, placeholder='Select or type variable name')))
      })
      output$end_mid_input <- renderUI({
        conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_mid_haul'",
                         style = "margin-left:19px;",  selectizeInput('mid_end','Select Lat then Lon that define ending locations',multiple = TRUE,
                                                                   choices = names(values$dataset[,grep('lat|lon', names(values$dataset), ignore.case=TRUE)]),
                                                                   options = list(maxItems = 2, create = TRUE, placeholder='Select or type variable name')))
      })
      
      output$input_startingloc <- renderUI({
        tagList(
          if(names(spatdat$dataset)[1]=='var1'){
            conditionalPanel("input.VarCreateTop=='Spatial functions'&&input.dist=='create_startingloc'",
                             tags$div(h4('Map file not loaded. Please load on Upload Data tab', style="color:red")))
          },
          conditionalPanel("input.VarCreateTop=='Spatial functions'&&input.dist=='create_startingloc'",
                           style = "margin-left:19px;", selectInput('trip_id_SL', 'Variable in primary data set to identify unique trips', choices=c('',names(values$dataset)), selectize=TRUE)),
          conditionalPanel("input.VarCreateTop=='Spatial functions'&&input.dist=='create_startingloc'",
                           style = "margin-left:19px;", selectInput('haul_order_SL', 'Variable in primary data set defining haul order within a trip. Can be time, coded variable, etc.',
                                                                    choices=c('', names(values$dataset)), selectize=TRUE)),
          conditionalPanel("input.VarCreateTop=='Spatial functions'&&input.dist=='create_startingloc'",
                           style = "margin-left:19px;", selectizeInput('starting_port_SL',  "Variable in primary data set identifying port at start of trip", 
                                                                    choices=names(values$dataset[,grep('port',names(values$dataset), ignore.case = TRUE)]), 
                                                                    options = list(create = TRUE, placeholder='Select or type variable name'))),
          conditionalPanel("input.VarCreateTop=='Spatial functions'&&input.dist=='create_startingloc'",
                           style = "margin-left:19px;", selectizeInput('lon_dat_SL', "Longitude variable in primary data set", 
                                                                    choices= names(values$dataset[,grep("lon", names(values$dataset), ignore.case = TRUE)]), 
                                                                    options = list(create = TRUE, placeholder='Select or type variable name'))), 
          conditionalPanel("input.VarCreateTop=='Spatial functions'&&input.dist=='create_startingloc'",
                           style = "margin-left:19px;", selectizeInput('lat_dat_SL', "Latitude variable in primary data set", 
                                                                    choices= names(values$dataset[,grep("lat", names(values$dataset), ignore.case = TRUE)]),
                                                                    options = list(create = TRUE, placeholder='Select or type variable name'))),
          conditionalPanel("input.VarCreateTop=='Spatial functions'&&input.dist=='create_startingloc'",
                           style = "margin-left:19px;",  selectInput("port.dat", "Choose file from the FishSET database containing port data", 
                                                                     choices=tables_database()[grep('port', tables_database(), ignore.case=TRUE)], multiple = FALSE))#,
          #conditionalPanel(condition="input.VarCreateTop=='Spatial functions'&input.dist=='create_startingloc'",
          #                 style = "margin-left:19px;", fileInput("grid.dat", "Choose data file containing map shapefile (shape, json, and csv formats are supported)",
          #                                                        multiple = FALSE, placeholder = ''))
        )})
      output$input_startingloc_extra <- renderUI({
        tagList(
          if(any(class(spatdat$dataset)=='sf')==FALSE){
            conditionalPanel("input.VarCreateTop=='Spatial functions'&&input.dist=='create_startingloc'",
                             style = "margin-left:19px;", selectInput('lat_grid_SL', 'Select vector containing latitude from spatial data set', 
                                                                      choices= names(as.data.frame(spatdat$dataset)), multiple=TRUE))
          },
          if(any(class(spatdat$dataset)=='sf')==FALSE){
            conditionalPanel("input.VarCreateTop=='Spatial functions'&&input.dist=='create_startingloc'",
                             style = "margin-left:19px;", selectInput('lon_grid_SL', 'Select vector containing longitude from spatial data set', 
                                                                      choices= names(as.data.frame(spatdat$dataset)), multiple=TRUE, selectize=TRUE))
          },
          conditionalPanel("input.VarCreateTop=='Spatial functions'&&input.dist=='create_startingloc'",
                           style = "margin-left:19px;", selectInput('cat_SL', "Variable defining zones or areas", choices= names(as.data.frame(spatdat$dataset)), selectize=TRUE))
        )
      })
      
      output$input_IDVAR <- renderUI({
        conditionalPanel("input.VarCreateTop=='Trip-level functions'&input.trip=='haul_to_trip'",
                         style = "margin-left:19px;", varSelectInput("Haul_Trip_IDVar", "Variable(s) that define unique trips",
                                                                 data = values$dataset, multiple=TRUE))
      })
      output$input_trip_dist_vars <- renderUI({
        tagList(
          conditionalPanel("input.VarCreateTop=='Trip-level functions'&&input.dist=='trip_distance'" ,
                           style = "margin-left:19px;", selectInput("port_dat_dist", "Choose file from the FishSET database containing port data", 
                                                                    choices=tables_database()[grep('port', tables_database(), ignore.case=TRUE)], multiple = FALSE)),
          #
          conditionalPanel("input.VarCreateTop=='Trip-level functions'&&input.trip=='trip_distance'",
                           style = "margin-left:19px;", varSelectInput('trip_ID','Variable in data file to identify unique trips', data = values$dataset,
                                                                       multiple = FALSE)),
          conditionalPanel("input.VarCreateTop=='Trip-level functions'&&input.trip=='trip_distance'",
                           style = "margin-left:19px;", selectInput('starting_port','Variable in data file to identify port at START of trip',multiple = FALSE, 
                                                                    choices = names(values$dataset)[grep('port', names(values$dataset), ignore.case=TRUE)], selectize=TRUE)),
          conditionalPanel("input.VarCreateTop=='Trip-level functions'&&input.trip=='trip_distance'",
                           style = "margin-left:19px;", selectizeInput('starting_haul','Select variables containing LAT the LON at START of haul',multiple = TRUE, 
                                                                    choices = names(values$dataset)[grep('lat|long', names(values$dataset), ignore.case=TRUE)], 
                                                                    options = list(maxItems = 2,create = TRUE, placeholder='Select or type variable name'))),
          conditionalPanel("input.VarCreateTop=='Trip-level functions'&&input.trip=='trip_distance'",
                           style = "margin-left:19px;", selectizeInput('ending_haul','Select variables containing LAT then LON at END of haul',multiple = TRUE, 
                                                                    choices = names(values$dataset)[grep('lat|long', names(values$dataset), ignore.case=TRUE)],
                                                                    options = list(maxItems = 2, create = TRUE, placeholder='Select or type variable name'))),
          conditionalPanel("input.VarCreateTop=='Trip-level functions'&&input.trip=='trip_distance'",
                           style = "margin-left:19px;", selectizeInput('ending_port','Variable in data file to identify port at END of trip',multiple = FALSE, 
                                                                    choices = names(values$dataset)[grep('port', names(values$dataset), ignore.case=TRUE)],
                                                                    options = list(create = TRUE, placeholder='Select or type variable name'))),
          conditionalPanel("input.VarCreateTop=='Trip-level functions'&&input.trip=='trip_distance'",
                           style = "margin-left:19px;", varSelectizeInput('haul_order','Variable in data file containing information on the order that hauls occur within a trip.',
                                                                    data = values$dataset, multiple = FALSE))
        )
      })
      output$input_tri_cent <-  renderUI({
        tagList(
          conditionalPanel("input.VarCreateTop=='Trip-level functions'&&input.trip=='trip_centroid'",
                           style = "margin-left:19px;", selectizeInput('trip_cent_lon','Vector containing longitudinal data', 
                                                                    choices =names(values$dataset)[grep('lon|lat', names(values$dataset), ignore.case=TRUE)], 
                                                                    multiple = FALSE,  options = list(create = TRUE, placeholder='Select or type variable name'))),
          conditionalPanel("input.VarCreateTop=='Trip-level functions'&&input.trip=='trip_centroid'",
                           style = "margin-left:19px;", selectizeInput('trip_cent_lat', 'Vector containing latitudinal data', 
                                                                    choices =names(values$dataset)[grep('lon|lat', names(values$dataset), ignore.case=TRUE)], 
                                                                    multiple = FALSE,  options = list(create = TRUE, placeholder='Select or type variable name'))),
          conditionalPanel("input.VarCreateTop=='Trip-level functions'&&input.trip=='trip_centroid'",
                           style = "margin-left:19px;", selectInput('trip_cent_weight','Variable for weighted average', multiple = FALSE, 
                                                                    choices=c('', names(values$dataset)), selected='', selectize=TRUE)),
          conditionalPanel("input.VarCreateTop=='Trip-level functions'&&input.trip=='trip_centroid'",
                           style = "margin-left:19px;", varSelectInput('trip_cent_id','Column(s) that identify the individual trip', data = values$dataset,
                                                                       multiple = TRUE))
        )
      })
      output$dummy_select <- renderUI({
        tagList(
          conditionalPanel("input.VarCreateTop=='Dummy variables'&&input.dummyfunc=='From variable'",
                           style = "margin-left:19px;", 
                           selectInput('dummyvarfunc','Select variable', multiple=FALSE, choices=c(NULL, names(values$dataset)), selectize=TRUE)),
          conditionalPanel("input.VarCreateTop=='Dummy variables'&&input.dummyfunc=='From policy dates'",
                           style = "margin-left:19px;", selectInput('dummypolyfunc','Select policy', multiple=FALSE, 
                                                                   choices=c('User defined', 'Central GOA Rockfish Cooperative (2004)'='Rockfish', 'Amendment 80 Alaska (2008)'='Amen80' ,
                                                                   "Pacific halibut and Sablefish IFQ Program	Alaska (1993)"="halsab", 
                                                                   "American Fisheries Act Pollock Program Alaska (1999)"="AFA"))),
          conditionalPanel("input.VarCreateTop=='Dummy variables'&&input.dummyfunc=='From policy dates'&&input.dummypolyfunc=='User defined'",
                           style = "margin-left:19px;", textInput('polyear','Policy year', placeholder='Write policy year here')),
          conditionalPanel("input.VarCreateTop=='Dummy variables'&input.dummyfunc=='From policy dates'",
                           style = "margin-left:19px;", selectInput('dummypolydate','Select date variable', multiple=FALSE, 
                                                                   choices=colnames(values$dataset)[grep('year|dat', colnames(values$dataset), ignore.case=TRUE)])),
          conditionalPanel("input.VarCreateTop=='Dummy variables'&&input.dummyfunc=='From area closures'",
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
              conditionalPanel("input.VarCreateTop=='Dummy variables'&&input.dummyfunc=='From variable'",
                               style = "margin-left:19px;", selectInput("dumsubselect", 'Set dummy variable based on', 
                                                                        choices=c('selected year(s) vs. all other years'='x_y','before vs. after'='more_less'))),
              conditionalPanel("input.VarCreateTop=='Dummy variables'&&input.dummyfunc=='From variable'",
                               style = "margin-left:19px;",  selectInput("select.val", 'Select year(s)', 
                                                                         choices=c(NULL, unique(lubridate::year(values$dataset[[input$dummyvarfunc]]))), multiple=TRUE))
            )
          } else if(dum_temp()=='num'){
            tagList(
            conditionalPanel("input.VarCreateTop=='Dummy variables'&&input.dummyfunc=='From variable'",
                             style = "margin-left:19px;", selectInput("dumsubselect", 'Set dummy variable based on',
                                                                      choices=c('selected value(s) vs all other values'='x_y','less than vs more than'='more_less'), selected='more_less')),
            conditionalPanel("input.VarCreateTop=='Dummy variables'&&input.dummyfunc=='From variable'",
                             sliderInput("select.val", 'Select single or range of values to set to zero', min=min(values$dataset[input$dummyvarfunc],na.rm=TRUE),
                                         max=max(values$dataset[input$dummyvarfunc],na.rm=TRUE),
                                         value=c(mean(values$dataset[input$dummyvarfunc], na.rm=TRUE)-mean(values$dataset[input$dummyvarfunc], na.rm=TRUE)/10, 
                                                 mean(values$dataset[input$dummyvarfunc], na.rm=TRUE)+mean(values$dataset[input$dummyvarfunc], na.rm=TRUE)/10)))
            )
          } else if(dum_temp()=='other') {
            conditionalPanel("input.VarCreateTop=='Dummy variables'&&input.dummyfunc=='From variable'",
                             style = "margin-left:19px;", selectInput("select.val", 'Select categories to set to zero', 
                                                                      choices=c(NULL, unique(values$dataset[[input$dummyvarfunc]])), multiple=TRUE))
          }
          
        } 
      })                  
      observeEvent(input$runNew, {
        if(input$VarCreateTop=='Dummy variables'&input$dummyfunc=='From policy dates') {
              q_test <- quietly_test(dummy_num)
              values$dataset <- q_test(values$dataset, var=input$dummypolydate, value=dum_pol_year(), opts='more_less', name=input$varname)
        } else if(input$VarCreateTop=='Dummy variables'&input$dummyfunc=='From variable') {
              q_test <- quietly_test(dummy_num)
              values$dataset <- q_test(values$dataset, var=input$dummyvarfunc, value=input$select.val, opts=input$dumsubselect, name=input$varname)
        } else if(input$VarCreateTop=='Temporal functions'&input$tempfunc=='temp_mod') {
              q_test <- quietly_test(temporal_mod)
              values$dataset <- q_test(values$dataset, x=input$TimeVar, define.format=input$define_format, name=input$varname)
        } else if(input$VarCreateTop=='Data transformations'&input$trans=='set_quants'){
              q_test <- quietly_test(set_quants)
              values$dataset <- q_test(values$dataset, x=input$trans_var_name, quant.cat = input$quant_cat, name=input$varname)
        } else if(input$VarCreateTop=='Nominal ID'&input$ID=='ID_var'){
              q_test <- quietly_test(ID_var)
              values$dataset <- q_test(values$dataset, name=input$varname, vars=input$unique_identifier, type=input$ID_type)
        } else if(input$VarCreateTop=='Nominal ID'&input$ID=='create_seasonal_ID'){
              q_test <- quietly_test(create_seasonal_ID)
              values$dataset <- q_test(values$dataset, seasonal.dat=seasonalData(), use.location=input$use_location, 
                                               use.geartype=input$use_geartype, sp.col=input$sp_col, target=input$target)
        } else if(input$VarCreateTop=='Data transformations'&input$trans=='group_perc'){
          q_test <- quietly_test(group_perc)
          values$dataset <- q_test(values$dataset, project=input$projectname, id_group=input$perc_id_grp, group=input$perc_grp,
                                   value=input$perc_value, name=input$varname, create_group_ID=input$perc_id_col, drop_total_col=input$perc_drop)
        } else if(input$VarCreateTop=='Data transformations'&input$trans=='group_diff'){
          q_test <- quietly_test(group_diff)
          values$dataset <- q_test(values$dataset, project=input$projectname, group=input$diff_grp,  sort_by=input$diff_sort,
                                   value=input$diff_value, name=input$varname, create_group_ID=input$diff_id_col, drop_total_col=input$diff_drop)
        } else if(input$VarCreateTop=='Data transformations'&input$trans=='group_cumsum'){
          q_test <- quietly_test(group_cumsum)
          values$dataset <- q_test(values$dataset, project=input$projectname, group=input$cumsum_grp,  sort_by=input$cumsum_sort,
                                   value=input$cumsum_value, name=input$varname, create_group_ID=input$cumsum_id_col, drop_total_col=input$cumsum_drop)
        } else if(input$VarCreateTop=='Arithmetic functions'&input$numfunc=='create_var_num'){
              q_test <- quietly_test(create_var_num)
              values$dataset <- q_test(values$dataset, x=input$var_x, y=input$var_y, method=input$create_method, name=input$varname)
        } else if(input$VarCreateTop=='Arithmetic functions'&input$numfunc=='cpue') {
          if(input$xTime!='Calculate duration'){
              q_test <- quietly_test(cpue)
              values$dataset <- q_test(values$dataset, xWeight=input$xWeight, xTime=input$xTime, name=input$varname)
          } else {
            q_test <- quietly_test(cpue)
            values$dataset <- create_duration(values$dataset, start=input$dur_start2, end=input$dur_end2, units=input$dur_units2, name='dur')
            values$dataset <- q_test(values$dataset, xWeight=input$xWeight, xTime='dur', name=input$varname)
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
            q_test <- quietly_test(create_dist_between_for_gui)
            values$dataset <- q_test(values$dataset, start=startdist, end=enddist, units$input$units,  name=input$varname, portTable=input$filePort, 
                                                                         gridfile=spatdat$dataset,lon.dat=input$lon_dat[2], lat.dat=input$lon_dat[1], 
                                                                         input$cat, lon.grid=input$long_grid[2], lat.grid=input$long_grid[1])
           } else if(input$VarCreateTop=='Spatial functions' & input$dist=='create_mid_haul'){
              q_test <- quietly_test(create_mid_haul)
              values$dataset <- q_test(values$dataset, start=c(input$mid_start[2], input$mid_start[1]), 
                                       end=c(input$mid_end[2], input$mid_end[1]), name=input$varname)
        } else if(input$VarCreateTop=='Temporal functions' & input$tempfunc=='create_duration'){
              q_test <- quietly_test(create_duration)
              values$datase <- q_test(values$dataset, start=input$dur_start, end=input$dur_end, units=input$dur_units, name=input$varname)
        } else if(input$VarCreateTop=='Spatial functions'&input$dist=='create_startingloc'){
              q_test <- quietly_test(create_startingloc)
              values$dataset <- q_test(values$dataset,  gridfile=spatdat$dataset,  portTable=input$port.dat,  trip_id=input$trip_id_SL,
                                                                haul_order=input$haul_order_SL, starting_port=input$starting_port_SL, input$lon_dat_SL, 
                                                                input$lat_dat_SL, input$cat_SL, input$varname, input$lon_grid_SL, input$lat_grid_SL)
        } else if(input$VarCreateTop=='Trip-level functions'&input$trip=='haul_to_trip'){
              q_test <- quietly_test(haul_to_trip)
              values$dataset <- q_test(values$dataset, project=input$projectname, input$fun_numeric, input$fun_time, input$Haul_Trip_IDVar)
        } else if(input$VarCreateTop=='Trip-level functions'&input$trip=='trip_distance'){
              q_test <- quietly_test(create_trip_distance)
              values$dataset <- q_test(values$dataset, PortTable = input$port_dat_dist, trip_id = input$trip_ID, 
                                       staring_port = input$starting_port, starting_haul = c(input$starting_haul[2], input$starting_haul[1]), 
                                      ending_haul = c(input$ending_haul[2],input$ending_haul[1]), ending_port = input$ending_port, 
                                       haul_order = input$haul_order, name = input$varname)
        } else if(input$VarCreateTop=='Trip-level functions'&input$trip=='trip_centroid'){
              q_test <- quietly_test(create_trip_centroid)
              values$dataset <- q_test(values$dataset, lon=input$trip_cent_lon, lat=input$trip_cent_lat, weight.var=input$trip_cent_weight, 
                                                 input$trip_cent_id)
        }
      })
      
      output$output_table_create <- DT::renderDT(
        if(colnames(values$dataset)[1] == 'var1') {
          return(NULL)
        } else {
        head(values$dataset)
        }
      )
      
      
      #Map Viewer ====
      
      map_viewer_serv("map", values, spatdat)
      #onStop(function() servr::daemon_stop()) 
      
      
      #---
      
      #Zonal definition ----
      #---
      output$conditionalInput1 <- renderUI({
        conditionalPanel("input.choiceTab=='primary'",
                         tagList(
                           selectizeInput('catchBase','Variable containing catch data',
                                          choices=colnames(values$dataset[,grep('haul|mt|lb|metric|pounds|catch', colnames(values$dataset), ignore.case=TRUE)]),
                                          options = list(create = TRUE, placeholder='Select or type variable name')),
                           selectizeInput('priceBase', 'Variable containing price or value data', 
                                          choices=c('none selected'='none', colnames(values$dataset[,grep('value|dollar', colnames(values$dataset), ignore.case=TRUE)])),
                                          selected='none', options = list(create = TRUE, placeholder='Select or type variable name')),
                           div(style="display: inline-block;vertical-align:top; width: 200px;",
                               selectizeInput('latBase', 'Occurrence latitude', choices=c('',colnames(values$dataset[,grep('lat', colnames(values$dataset), ignore.case=TRUE)])), 
                                           selected='', options = list(create = TRUE, placeholder='Select or type LATITUDE variable name'))),
                           div(style="display: inline-block;vertical-align:top; width: 200px;",
                               selectizeInput('lonBase', 'Occurrence longitude', choices=c('',colnames(values$dataset[,grep('lon', colnames(values$dataset), ignore.case=TRUE)])),
                                              selected='', options = list(create = TRUE, placeholder='Select or type LONGITUDE variable name')))
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
                           h5(tags$b('Select latitude then longitude from main dataset for assigning observations to zones')),
                           div(style="display: inline-block;vertical-align:top; width: 200px;",
                               selectizeInput('lat_dat_ac', '',
                                              choices = c(names(values$dataset)[which(stringr::str_count(names(values$dataset), "(?i)lat")
                                                                          ==max(stringr::str_count(names(values$dataset), "(?i)lat")))][1]),
                                              #choices=c(names(values$dataset)[grep('lat', names(values$dataset), ignore.case=TRUE)]), 
                                              options = list(create = TRUE, placeholder='Select or type LATITUDE variable name'))),
                           div(style="display: inline-block;vertical-align:top; width: 200px;",
                               selectizeInput('lon_dat_ac', '', 
                                              choices=c(names(values$dataset)[which(stringr::str_count(names(values$dataset), "(?i)lon")
                                                                                    ==max(stringr::str_count(names(values$dataset), "(?i)lon")))][1]), 
                                              options = list(create = TRUE, placeholder='Select or type LONGITUDE variable name'))),
                           selectInput('cat_altc', 'Individual areas/zones from the spatial data set', choices=names(as.data.frame(spatdat$dataset))),
                           selectInput('weight_var_ac', 'If desired, variable for use in calculating weighted centroids', 
                                       choices=c('none'="", colnames(values$dataset))), #variable weighted centroids
                           checkboxInput('hull_polygon_ac', 'Use convex hull method to create polygon?', value=FALSE),
                           checkboxInput('closest_pt_ac', 'Use closest polygon to point?', value=FALSE) 
                         ) )
      })  
      
       observeEvent(input$runCentroid, {
              q_test <- quietly_test(assignment_column)
              values$dataset <-  q_test(dat=values$dataset, gridfile=spatdat$dataset, lon.dat=input$lon_dat_ac, lat.dat=input$lat_dat_ac, 
                                             cat=input$cat_altc, closest.pt = input$closest_pt_ac, lon.grid=NULL,
                                              lat.grid=NULL, hull.polygon = input$hull_polygon_ac, epsg=NULL)
              if('ZoneID' %in% names(values$dataset)){
              showNotification('Zone assignment completed', duration=5, type='message')
              } else {
                showNotification('Zone assignment could not be completed', type='message', duration=5)
              }
      })
       
      output$cond2 <- renderUI({
        conditionalPanel(condition="input.choiceTab=='zone'",
                         if(any(class(spatdat$dataset)=='sf')==FALSE){
                           tagList(
                             h5(tags$b('Select vector containing latitude then longitude from spatial data set')),
                             div(style="display: inline-block;vertical-align:top; width: 200px;",
                                 selectizeInput('lat_grid_altc', '', choices=names(as.data.frame(spatdat$dataset)), multiple=TRUE)),
                             div(style="display: inline-block;vertical-align:top; width: 200px;",
                                 selectizeInput('long_grid_altc',  '', choices=names(as.data.frame(spatdat$dataset)), multiple=TRUE))
                           )
                         }
        ) 
      })

      output$conditionalInput3a <- renderUI({
        conditionalPanel(condition="input.choiceTab=='distm'",
                         selectInput('distMsource', 'Should distance matrix come from', 
                                     choices=c('Primary haul- or trip-level data'='primary', 'Gridded dataset'='gridded'), 
                                     selected='primary')
        )
      })
        
      output$conditionalInput3 <- renderUI({
        tagList(
        conditionalPanel(condition="input.choiceTab=='distm' && input.distMsource == 'primary'",
                         tagList(
                          h5(tags$b('Define how alternative fishing choices are calculated between')),
                           div(style="display: inline-block;vertical-align:top; width: 160px;",
                               selectizeInput('occasion_ac', 'occurrence:', 
                                           choices=c('Centroid of zonal assignment'='centroid', 
                                                      names(values$dataset)[grep('lat|lon|port', names(values$dataset), ignore.case=TRUE)]),
                                           selected='', options = list(maxItems=2))), #Identifies how to find lat/lon for starting point (must have a lat/lon associated with it) 
                           div(style="display: inline-block;vertical-align:top; width: 170px;",
                               selectizeInput('alt_var_ac', 'and alternative location', 
                                              choices=c('Centroid of zonal assignment'='centroid', 
                                                       names(values$dataset)[grep('lat|lon', names(values$dataset), ignore.case=TRUE)]), 
                                              selected='', options = list(maxItems = 2))),
                          h5(tags$em('Longitude must be specified before latitude.')),
                           selectizeInput('dist_ac','Distance units', choices=c('miles','kilometers','meters'), selected='miles'),
                           numericInput('min_haul_ac', 'Include zones with more hauls than', min=1, max=1000, value=1)#,
                         )
                         ),
        conditionalPanel(condition="input.choiceTab=='distm' && input.distMsource == 'gridded'",
                         fileInput("gridded.dat", "Load gridded data file", multiple = FALSE, placeholder = '')
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
      
      
     
       #---
      
      #Expected Catch----     
      #---
      output$selectcp <- renderUI({
        tagList(
          selectInput('catche','Catch variable for averaging',
                      choices=c(input$catchBase, colnames(values$dataset[,grep('haul|mt|lb|metric|pounds|catch', colnames(values$dataset), ignore.case=TRUE)])),
                      selected=input$catchBase),
          selectizeInput('price', 'If expected revenue is to be calculated, variable containing price or value data', 
                         choices=c(input$priceBase, "none", colnames(values$dataset[,grep('value|dollar', colnames(values$dataset), ignore.case=TRUE)])),
                                   options = list(create = TRUE, placeholder='Select or type variable name')),
          selectizeInput('group','Choose variable that defines groups',
                         choices=c('Fleet (no group)', names(values$dataset[, !sapply(values$dataset, is.numeric)])))
        )
      })
      output$expcatch <-  renderUI({
        conditionalPanel(condition="input.temporal!='Entire record of catch (no time)'",
                         style = "margin-left:19px;font-size: 12px", 
                         selectizeInput('temp_var', 'Temporal variable for averaging', 
                                     choices=c('none', names(values$dataset)[grep('date|year|hour|day', colnames(values$dataset), ignore.case = TRUE)]),
                                     selected='none', options = list(create = TRUE, placeholder='Select or type variable name')))
      })
      sparstable_dat <- reactive({
        if(!any(colnames(values$dataset)=='ZoneID')){
          return()
        } else if(is_empty(input$catche)){
          return()
        } else if(input$temp_var=='none'){
          return()
        } else{
          sparsetable(values$dataset, project=input$projectname, timevar=input$temp_var, zonevar='ZoneID', var=input$catche)
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
      
      
      #---
      
      #Model Parameters----
      #---
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
                             selectizeInput('starting_port_SL_mod',  "Variable in primary data set identifying port at start of trip", 
                                            choices=names(values$dataset[,grep('port',names(values$dataset), ignore.case = TRUE)]), 
                                            options = list(create = TRUE, placeholder='Select or type variable name'))),
            conditionalPanel(condition="input.model=='logit_correction' && input.startlocdefined=='create'",
                             selectizeInput('lon_dat_SL_mod', "Longitude variable in primary data set", 
                                            choices= names(values$dataset[,grep("lon", names(values$dataset), ignore.case = TRUE)]), 
                                            options = list(create = TRUE, placeholder='Select or type variable name'))), 
            conditionalPanel(condition="input.model=='logit_correction' && input.startlocdefined=='create'",
                             selectizeInput('lat_dat_SL_mod', "Latitude variable in primary data set", 
                                            choices= names(values$dataset[,grep("lat", names(values$dataset), ignore.case = TRUE)]), 
                                            options = list(create = TRUE, placeholder='Select or type variable name'))),
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
#      output$latlonB <- renderUI({
#        conditionalPanel(
#          condition="input.alternatives=='loadedData'",
#          div(style="display: inline-block;vertical-align:top; width: 200px;",
#              selectizeInput('lat', 'Occurrence latitude', choices=c(input$latBase, colnames(values$dataset[,grep('lat', colnames(values$dataset), ignore.case=TRUE)])), 
#                          selected='', options = list(create = TRUE, placeholder='Select or type variable name'))),
#          div(style="display: inline-block;vertical-align:top; width: 200px;",
#              selectizeInput('lon', 'Occurrence longitude', choices=c(input$lonBase, colnames(values$dataset[,grep('lon', colnames(values$dataset), ignore.case=TRUE)])), 
#                          selected='', options = list(create = TRUE, placeholder='Select or type variable name')))
#        )
#      })
      # Data needed
      ## Alternative choices
      Alt_vars <- reactive({
        if(!exists("Alt")) {
        if(!exists('AltMatrixName')) {
          if(DBI::dbExistsTable( DBI::dbConnect(RSQLite::SQLite(), locdatabase()), paste0(input$projectname, 'altmatrix'))){
          return(unserialize(DBI::dbExecute( DBI::dbConnect(RSQLite::SQLite(), locdatabase()), paste0("SELECT AlternativeMatrix FROM ", 
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
      selectInput("port.datMD", "Choose file from the FishSET database containing port data", 
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
        numwidth <- paste("'", as.character(numwidth),"%'", collapse=", ", sep="")
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
        data = data.frame('mod_name'='', 'likelihood'='', 'optimOpt'='', 'inits'='', 
                          'methodname'='', 'vars1'='','vars2'='', 'catch'='',
                           'project'='', 'price'='', 'startloc'='', 'polyn'=''),
        #model_table,
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
                                            lon.grid=input$lat_grid_SL, lat.grid=input$lat_grid_SL_mod)
        } 
        counter$countervalue <- counter$countervalue + 1
        
        if(is.null(input$gridVariablesInclude)|is.null(input$indeVarsForModel)) {
          rv$data <- rbind(data.frame('mod_name'='', 
                                'likelihood'='',
                                'optimOpt'='',
                                'inits'='',
                                'methodname' = '', 
                                'vars1'= '',
                                'vars2'='', 
                                'catch'='',
                                'project'=input$projectname, 
                                'price'='',
                                'startloc'='',
                                'polyn'='')
                     , rv$data)#model_table())
        } else {
          rv$data = rbind(data.frame('mod_name'=paste0('mod', counter$countervalue), 
                               'likelihood'=input$model, 
                               'optimOpt'=paste(input$mIter,input$relTolX, input$reportfreq, input$detailreport),
                               'inits'=paste(int_name(), collapse=','),#noquote(paste0('input$int',1:numInits())),
                               'methodname' = input$optmeth, 
                               'vars1'= paste(input$indeVarsForModel, collapse=', '),
                               'vars2'=input$gridVariablesInclude, 
                               'catch'=input$catch,
                               'project'=input$projectname, 
                               'price'=input$price,
                               'startloc'=if(input$startlocdefined=='exists'){input$startloc_mod} else {'startingloc'}, 
                               'polyn'=input$polyn)
                    , rv$data)#model_table())
          #print(rv$data)
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
                                            "(mod_name TEXT, likelihood TEXT, optimOpt TEXT, inits TEXT, 
                                            optmeth TEXT, vars1 TEXT, vars2 TEXT,   catch TEXT, 
                                           lon TEXT, lat TEXT, project TEXT, price TEXT, startloc TEXT, polyn TEXT)"))
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
        DBI::dbExecute(fishset_db, query)
        DBI::dbDisconnect(fishset_db)
        
        
      })
      
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
    
#         output$mod_param_table <- DT::renderDT(
#        model_table(), editable = T, server=TRUE
#      )
 
  
      # Run models shiny
      observe({
        if(input$submit > 0) {
          input_list <- reactiveValuesToList(input)
          toggle_inputs(input_list,F)
          #print('call model design function, call discrete_subroutine file')
          q_test <- quietly_test(make_model_design)
          times <- nrow(rv$data)-1
          i <- 1
          showNotification(paste('1 of', times, 'model design files created.'), type='message', duration=10)
          q_test(values$dataset, project=rv$data$project[i], catchID=rv$data$catch[i],  
                            replace=TRUE, likelihood=rv$data$likelihood[i], optimOpt=rv$data$optimOpt[i], 
                            inits=rv$data$inits[i], methodname = rv$data$optmeth[i], mod.name = rv$data$mod_name[i], 
                            vars1=rv$data$vars1[i], vars2=rv$data$vars2[i], 
                            priceCol=rv$data$price[i], startloc=rv$data$startloc[i], polyn=rv$data$polyn[i])
          
          if(times>1){
          for(i in 2:times){
            q_test(values$dataset, project=rv$data$project[i], catchID=rv$data$catch[i], 
                              replace=FALSE, likelihood=rv$data$likelihood[i], optimOpt=rv$data$optimOpt[i], 
                              inits=rv$data$inits[i], methodname =rv$data$optmeth[i], mod.name = rv$data$mod_name[i], 
                              vars1=rv$data$vars1[i], vars2=rv$data$vars2[i], 
                              priceCol=rv$data$price[i], startloc=rv$data$startloc[i], polyn=rv$data$polyn[i])
            showNotification(paste(i, 'of', times, 'model design files created.'), type='message', duration=10)
          }
          }
         # showNotification(
         #   capture.output(
                showNotification('Model is running. Models can take 30 minutes.
                                  All buttons are inactive while model function is running.
                                  Check R console for progress.', type='message', duration=30)
                discretefish_subroutine(input$projectname, select.model=FALSE) #, name='discretefish_subroutine')              
        #    ), type='message', duration=10)
                showNotification('Model run is complete. Check the `Compare Models` subtab to view output', type='message', duration=10)
          toggle_inputs(input_list,T)
        }
      })
      
      
    ## Explore models sections
      #out_mod <- reactive({
      fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase())
      
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
          data.frame(t(DBI::dbExecute(DBI::dbConnect(RSQLite::SQLite(), locdatabase()), 
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
        data.frame(this_table(), select=shinyInput(checkboxInput,nrow(this_table()),"cbox_"))
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
#        if(overwrite_table==T){
          if(DBI::dbExistsTable(fishset_db, 'modelChosen')==TRUE){
            DBI::dbRemoveTable(DBI::dbConnect(RSQLite::SQLite(), locdatabase()), 'modelChosen')
          }
#        }
        
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
        DBI::dbExecute(fishset_db, query)
        
        showNotification("Table saved to database")
      })
      DBI::dbDisconnect(fishset_db)
      
    
      #Add in two more tables for model evaulations
      fishset_db <- suppressWarnings(DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
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
      
      
      
      #---
      #Run functions ----
      #---
      observeEvent(input$saveALT, {
              showNotification('Function can take a couple minutes. A message will appear when done.',
                               type='message', duration=20)
              q_test <- quietly_test(create_alternative_choice)
              q_test(values$dataset, project=input$projectname, gridfile=spatdat$dataset, alt_var=input$alt_var_ac, 
                                  occasion=input$occasion_ac, griddedDat=NULL, dist.unit=input$dist_ac, min.haul=input$min_haul_ac,
                                  cat=input$cat_altc, lon.dat=input$lon_dat_ac, lat.dat=input$lat_dat_ac,
                                  hull.polygon=input$hull_polygon_ac, lon.grid=input$long_grid_altc, lat.grid=input$lat_grid_altc, 
                                  closest.pt=input$closest_pt_ac,  weight.var=input$weight_var_ac )
              showNotification('Completed. Alternative choice matrix updated', type='message', duration=10)
      }, ignoreInit = F) 
      
      observeEvent(input$submitE, {
                q_test <- quietly_test(create_expectations)
                q_test(values$dataset, input$projectname, input$catche, price=input$price, 
                                    defineGroup=if(grepl('no group',input$group)){'fleet'} else {input$group},  
                            temp.var=input$temp_var, temporal = input$temporal, calc.method = input$calc_method, lag.method = input$lag_method,
                            empty.catch = input$empty_catch, empty.expectation = input$empty_expectation, temp.window = input$temp_window,  
                            temp.lag = input$temp_lag, year.lag=input$temp_year, dummy.exp = input$dummy_exp, replace.output = TRUE)
        showNotification('Create expectated catch function called', type='message', duration=10)
      }) 
      
     
       ####---
      ##Resetting inputs
      observeEvent(input$refresh1,{
        updateCheckboxInput(session, 'Outlier_Filter', value=FALSE)
      })
      ###---              
      
      ####---  
      #Save output----   
      ###---   
      observeEvent(input$saveData, {
        suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
        
        if (input$SelectDatasetExplore == "main") {
          DBI::dbWriteTable(fishset_db, paste0(input$projectname, 'MainDataTable'), values$dataset, overwrite=TRUE)
        } else if (input$SelectDatasetExplore == "port") {
          DBI::dbWriteTable(fishset_db, paste0(input$projectname, 'PortTable'), ptdat$dataset, overwrite=TRUE)
        } else if (input$SelectDatasetExplore == "grid") {
          DBI::dbWriteTable(fishset_db, paste0(input$projectname, input$GridName), grddat$dataset, overwrite=TRUE)
        } else if (input$SelectDatasetExplore == "auxiliary") {
          DBI::dbWriteTable(fishset_db, paste0(input$projectname, input$AuxName), aux$dataset, overwrite=TRUE)
        }
        DBI::dbDisconnect(fishset_db)
        showNotification('Data saved to FishSET database', type='message', duration=10)
      })
      
      observeEvent(input$saveDataQ, {
        suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
        DBI::dbWriteTable(fishset_db, paste0(input$projectname, 'MainDataTable'), values$dataset, overwrite=TRUE)
        DBI::dbDisconnect(fishset_db) 
        showNotification('Data saved to FishSET database', type='message', duration=10)
        
      })
      
      output$SaveButtons <- renderUI({
        tagList(
          #shinySaveButton(id = 'downloadplot', label ='Save plot to folder', title = "", filename = paste0(project,'_', input$checks, '_plot'), filetype = "png"),
          actionButton('downloadplot', label ='Save plot to folder'),
          downloadLink('downloadplotHIDE', label=''),
          actionButton('downloaddata', label ='Save table to folder as csv'),
         # downloadLink("downloadText", label=''),
          actionButton('callTextDownload','Save notes')
        )
      })
      
      output$SaveButtonsUpload <- renderUI({
        tagList(
        #  downloadLink("downloadTextUp", label=''),
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
          actionButton('downloadTableExplore', label ='Save table to folder as csv'))
        #  downloadLink("downloadTextExplore", label='')
        )
      })
      
      output$SaveButtonsAnal <- renderUI({
        tagList(
          downloadLink('downloadplotAnalHIDE', label =''),
          downloadLink('downloaddataAnalHIDE', label =''),
          actionButton('downloadplotAnal', label ='Save plot to folder'),#, title = "", filename = paste0(project,'_', input$corr_reg, '_plot'), filetype = "png"),
          actionButton('downloaddataAnal', label ='Save table to folder as csv'),
        #  downloadLink("downloadTextAnal", label=''),
          actionButton('callTextDownloadAnal','Save notes')
        )
      })
      
      output$SaveButtonsNew <- renderUI({
        tagList(
          downloadLink('downloadplotNew', label=''),
          actionButton('downloadplotNew', label ='Save plot to folder'),#, title = "", filename = paste0(project, input$plot_type , '_plot'), filetype = "png")
        #  downloadLink("downloadTextNew", label=''),
          actionButton('callTextDownloadNew','Save notes')
        )
      })
      
      
      

      #Downloads ====  
    savedText <- reactiveValues(answers = logical(0))
    
    observeEvent(c(input$callTextDownload,
                   input$callTextDownloadAnal,
                   input$callTextDownloadExplore,
                   input$callTextDownloadUp,
                   input$callTextDownloadNew,
                   input[["fleet-callTextDownload"]],
                   input$callTextDownloadZone,
                   input$callTextDownloadEC,
                   input$callTextDownloadModels,
                   input$callTextDownloadBook),{
                    
                     if (input$projectname == "") {
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
                       updateTextInput(session, 'notesZone', "Notes", value = "", 
                                       placeholder = 'Write notes to store in text output file. Text can be inserted into report later.')
                       updateTextInput(session, 'notesEC', "Notes", value = "",
                                       placeholder = 'Write notes to store in text output file. Text can be inserted into report later.')
                       updateTextInput(session, 'notesModel', "Notes", value = "", 
                                       placeholder = 'Write notes to store in text output file. Text can be inserted into report later.')
                       updateTextInput(session, 'notesBook', "Notes", value = "", placeholder = 'Paste bookmarked URL here.')
                       
                       showNotification("Note saved.", type = 'message', duration = 5)
                     }
                   }, ignoreInit = TRUE)
      
      
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
      
      # save grid plot in Explore tab
      plotSaveServ("grid_plot", reactive(input$projectname), "view_grid_dat", grid_values, in_list = TRUE)
      
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
      
      
      
      ##---
      
      #Stop shiny ----
      ##---
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
          #---
        updateSelectInput(session, "alt_var_ac", selected = bookmarkedstate()$alt_var_ac) 
        updateSelectInput(session, "calc_method", selected = bookmarkedstate()$calc_method) 
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
        updateTextInput(session, 'polyear', value = bookmarkedstate()$polyear)
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
    
#---
        })
      
      #Rerun log -----
      
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
      
      observeEvent(input$rerun_close, stopApp())
      ###---
     
      onStop(function() {
        
        if (isolate(input$projectname) != "") {
        
          if (sum(isolate(c(input$callTextDownload,
                    input$callTextDownloadAnal,
                    input$callTextDownloadExplore,
                    input[["fleet-callTextDownload"]],
                    input$callTextDownloadUp,
                    input$callTextDownloadNew,
                    input$callTextDownloadZone,
                    input$callTextDownloadEC,
                    input$callTextDownloadModels,
                    input$callTextDownloadBook))) > 0) {
  
            notes_out <- unlist(isolate(savedText$answers))
  
            filename <- paste0(locoutput(), isolate(input$projectname), "_notes_", Sys.Date(), ".txt")
    
            if (file.exists(filename)) {
    
              note_pd <- paste0(isolate(input$projectname), "_notes_", Sys.Date())
    
              note_int <- sum(grepl(note_pd, current_out()))
    
              writeLines(notes_out,
                         con = paste0(locoutput(), isolate(input$projectname),
                                      "_notes_", Sys.Date(), "(", (note_int + 1), ").txt"))
    
            } else {
              writeLines(notes_out, con = filename)
            }
          }
        }

        # map viewer -- add check to see if run w/ servr::daemon_list()
        servr::daemon_stop()
        
        #rm("dat_name", envir = rlang::global_env())
        
      }) 
       
    }
            
