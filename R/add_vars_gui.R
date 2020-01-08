# Data selection tool
# Add variables from raw data set back into the working data set.

# add_vars
#' View and select which variables to add to working data set
#'
#' @param working_dat Main data frame containing data on hauls or trips. Table in fishset_db database should contain the string `MainDataTable`.
#' @param raw_dat Main raw (unmodified data frame)
#' @param project Name of project. Parameter is used to generate meaningful table names in fishset_db database.
#' @importFrom DBI  dbDisconnect dbConnect dbListTables dbWriteTable 
#' @import shiny
#' @export add_vars
#' @details Opens an interactive table that allows uses to select which variables to included by clicking check boxes. 
#' Data should be loaded into the fishset_db database before running this function. Select variables that will be used to generate further variables, such as rates or cpue, and variables to be included in models.
#' @examples 
#' \dontrun{
#' select_vars('pcodMainDataTable')
#' }

#Selectbox is column names in raw data that are not in working data
#Show top five rows of working data
#Drop rows that are not in linkID

add_vars <- function(working_dat, raw_dat, project){
  requireNamespace(shiny)
  
  if(!exists('loc')){
    loc = getwd()
  } else {
    loc = loc
  }
  
  shinyApp(
    ui = fluidPage(
      # tweaks, a list object to set up multicols for checkboxGroupInput
      tags$head(tags$style(HTML("
                                .multicol { 
                                -webkit-column-count: 2; /* Chrome, Safari, Opera */ 
                                -moz-column-count: 2;    /* Firefox */ 
                                column-count: 2; 
                                -moz-column-fill: auto;
                                -column-fill: auto;
                                } 
                                ")) 
      ),
      tags$head(tags$style(HTML(
        ".checkbox { 
        margin-left: 0px;
        margin-right: 10px;
        }
        .checkbox+.checkbox {
        margin-left: 0px;
        margin-right: 10px;
        }
        "))),
      sidebarLayout(
        sidebarPanel(
          tags$br(),tags$br(),
          actionButton("submit", "Save selected data", style="color: #fff; background-color: #337ab7; border-color: #2e6da4;"),
          tags$br(),tags$br(),
          tags$button(
            id = 'close',
            type = "button",
            style="color: #fff; background-color: #FF6347; border-color: #800000;",
            class = "btn action-button",
            onclick = "setTimeout(function(){window.close();},500);",  # close browser
            "Close window"
          )),
        mainPanel(
          if("linkID" %in% names(working_dat)==FALSE){
            stop('Function could not run. Must use select_vars function to subset data set before adding variables back into working data set.')
          },
          if(length(col_show <- colnames(raw_dat)[-which(names(raw_dat) %in% names(working_dat))])==0){
            tags$h2('Please close window. No new variables to add.', style='color:red')
          } else {
          #Checkbox input widget 
             h3('Select variables to add back into the working data set')
          },
          if(length(col_show <- colnames(raw_dat)[-which(names(raw_dat) %in% names(working_dat))])!=0){   
          tags$div(align = 'left', 
                   class = 'multicol', checkboxGroupInput("columns", "", choices = col_show <- colnames(raw_dat)[-which(names(raw_dat) %in% names(working_dat))], 
                                                          selected = '', 
                                                          inline=FALSE))
            },
          tags$br(), tags$br(),
          h4('First five rows of working data set.'),
          tableOutput("data_table")
        ))),

##  Beging SERVER functions  
    server = function(input, output, session) {
      col_show <- 1
      suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase))
      if(is.character(working_dat)==TRUE){
        if(is.null(working_dat)==TRUE | table_exists(working_dat)==FALSE){
          print(DBI::dbListTables(fishset_db))
          stop(paste(working_dat, 'not defined or does not exist. Consider using one of the tables listed above that exist in the database.'))
        } else {
          working_dat <- table_view(working_dat)
        }
      } else {
        working_dat <- working_dat  
      }
      if(is.character(raw_dat)==TRUE){
        if(is.null(raw_dat)==TRUE | table_exists(raw_dat)==FALSE){
          print(DBI::dbListTables(fishset_db))
          stop(paste(raw_dat, 'not defined or does not exist. Consider using one of the tables listed above that exist in the database.'))
        } else {
          raw_dat <- table_view(raw_dat)
        }
      } else {
        raw_dat <- raw_dat  
      }
      
      DBI::dbDisconnect(fishset_db)
  
      col_show <- colnames(raw_dat)[-which(names(raw_dat) %in% names(working_dat))]
      #dcol_show <- recactive({
      #  col_show
      #})
      dInputraw <- reactive({
        raw_dat
      })
      
      dInputworking <- reactive({
        working_dat
      })
      
      data_table_raw <- reactive({
        # If missing input, return to avoid error later in function
        if(is.null(raw_dat))
          return(NULL)
        
        # Get the data set
        dataset <- dInputraw()
        
        # Keep the selected columns
        dataset <- dataset[, c(input$columns, 'linkID'), drop = FALSE]
        dataset <- merge(dataset, working_dat, by='linkID')
      })
      
      
      dInput <- reactive({
        working_dat
      })
      
      data_table_working <- reactive({
        # If missing input, return to avoid error later in function
        if(is.null(working_dat))
          return(NULL)
        
        # Get the data set
        dataset_working <- dInputworking()
        
        # Keep the selected columns
        #dataset[, c(input$columns, linkID), drop = FALSE]
        
      })
      
      output$data_table <- renderTable(head(data_table_working()))
      
      
      # When the Submit button is clicked, save the form data
      observeEvent(input$submit, {
        # Connect to the database
        suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), paste0(loc,"/fishset_db.sqlite")))
        DBI::dbWriteTable(fishset_db, paste0(project, 'MainDataTable',  format(Sys.Date(), format="%Y%m%d")), data_table(), overwrite=TRUE)
        
        showNotification(paste0("Table saved to database as ", project, 'MainDataTable',  format(Sys.Date(), format="%Y%m%d"), ". Please close the window."))
        DBI::dbDisconnect(fishset_db)
      })
      
      
      
      # stop shiny
      observe({
        if (input$close > 0) stopApp()                             
      })
      
      
    }
      )
  }

