# Validation checks selection tool
# Select which data validation checks to apply.

# select_checks
#' View and select which validation checks to run
#'
#' @param dat Main data frame containing data on hauls or trips. Table in fishset_db database should contain the string `MainDataTable`.
#' @param project Name of project. Parameter is used to generate meaningful table names in fishset_db database.
#' @importFrom DBI  dbDisconnect dbConnect dbListTables dbWriteTable 
#' @import shiny
#' @export select_checks
#' @details Opens an interactive table that allows uses to select which data validation checks to run by clicking check boxes. 
#' @examples 
#' \dontrun{
#' select_checks('pcodMainDataTable', 'pcod')
#' }

select_checks <- function(dat, project){
  library(shiny)
  
  runApp(list(
## USER INTERFACE    
    ui = fluidPage(

      sidebarLayout(
        sidebarPanel(
          tags$br(),tags$br(),
          actionButton("submit", "Save output to notebook file", style="color: #fff; background-color: #337ab7; border-color: #2e6da4;"),
          tags$br(),tags$br(),
          tags$button(
            id = 'close',
            type = "button",
            style="color: #fff; background-color: #FF6347; border-color: #800000;",
            class = "btn action-button",
            onclick = "setTimeout(function(){window.close();},500);",  # close browser
            "Close window"
          ),
        
          h4('Select data validation check functions to run'),
          #h5('Only selected variables will be available in the working data set. All variables will be retained in the saved raw data set.'),
          #Checkbox input widget  
          radioButtons("checks", "", choices = c('Summary table', 'Outliers', 'NAs', 'NaNs', 'Unique observations', 'Empty variables', 'Lat/Lon units')
                       )
       ),#END SIDEBAR LAYOUT

        #outlier_table    outlier_plot     nan_identify                   
         mainPanel(
           tags$br(), tags$br(),
           verbatimTextOutput("Case"),
          tableOutput("output_table"),
          tags$br(),tags$br(),
          #---> Add in a conditional panel here <----
          uiOutput('NA_NAN')
         ))),
    
## SERVER SIDE    
    server = function(input, output, session) {
      suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"))
      if(is.character(dat)==TRUE){
        if(is.null(dat)==TRUE | table_exists(dat)==FALSE){
          print(DBI::dbListTables(fishset_db))
          stop(paste(dat, 'not defined or does not exist. Consider using one of the tables listed above that exist in the database.'))
        } else {
          dat <- table_view(dat)
        }
      DBI::dbDisconnect(fishset_db)
      } else {
        dat <- dat  
      }
      
      output$showNA <- reactive ({
        val <- ifelse (any(apply(dat, 2, function(x) any(is.na(x))))==TRUE, 1, 0)
      })
      output$showNAN <- reactive ({
        val <- ifelse (any(apply(dat, 2, function(x) any(is.nan(x))))==TRUE, 1, 0)
      })
      
      summary <- FishSET:::summary_stats(dat)
      na <- function(x) { if (any(apply(dat, 2, function(x) any(is.na(x))))==TRUE) {
        cat("The", names(which(apply(dat, 2, function(x) any(is.na(x)))==TRUE)), "columns contain NAs. Consider using na_filter to replace or remove NAs")
      } else {
        cat("No columns in the dataframe contain NAs")
      }}
      nan <- function(x) { if (any(apply(dataset, 2, function(x) any(is.nan(x))))==TRUE) {
        cat("The", names(which(apply(dataset, 2, function(x) any(is.nan(x)))==TRUE)), "columns contain NaNs. Consider using nan_filter to replace or remove NaNs")
      } else {
        cat("No columns in the dataframe contain NaNs")
      }}
      
      
      output$Case<-renderPrint({
         if (input$checks=='Summary table') {
          "Summary table of variables in data set"
        } else  if (input$checks=='Outliers'){
          'Table to assess outliers'
        } else  if (input$checks=='NAs'){
          if (any(apply(dat, 2, function(x) any(is.na(x)))==TRUE)) {
            cat("The", names(which(apply(dat, 2, function(x) any(is.na(x)))==TRUE)), "columns contain NAs. Consider using na_filter to replace or remove NAs")
          } else {
            cat("No columns in the dataframe contain NAs")
          }
        } else if(input$checks=='NaNs') {
          print(nan(dat))
        } else if(input$checks=='Unique observations'){
          'test'
        } else if(input$checks=='Empty variables'){
           'test'
        } else if(input$checks=='Lat/Lon units'){
          'test'
        } else {
          'Make a selection in the left hand column'
        } 
      })
      
      #HERE I NEED EXECUTE THE FUNCTION
        output$output_table <- renderTable({
          if (input$checks=='Summary table') { 
          summary
          } else if (input$checks=='Outliers'){
            NULL
          } else  if (input$checks=='NAs'){
            NULL
          } else {
            NULL
          }
          })
    
      output$NA_NAN <- renderUI ({
        tagList(
        conditionalPanel(
          condition = "input.checks == 'NAs' && output.showNA == 1",
           radioButtons('NA_Filter', 'Filter NAs by', choices=c('Remove all', 'Replace with mean'), selected='')
        ),
        conditionalPanel(
          condition = "input.checks == 'NaNs'",
          radioButtons('NA_Filter', 'Filter NaNs by', choices=c('Remove all', 'Replace with mean'), selected='')
        )
        )
      }) 
        #output_table())
      
      #output$data_table <- renderDataTable(out))
      # When the Submit button is clicked, save the form data
      observeEvent(input$submit, {
        # Connect to the database
        suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"))
        DBI::dbWriteTable(fishset_db, paste0(project, 'MainDataTable',  format(Sys.Date(), format="%Y%m%d")), data_table(), overwrite=TRUE)
        
        showNotification(paste0("Table saved to database as ", project, 'MainDataTable',  format(Sys.Date(), format="%Y%m%d"), ". Please close the window."))
        DBI::dbDisconnect(fishset_db)
      })
      

      # stop shiny
      observe({
        if (input$close > 0) stopApp()                             
      })
      
      
    }
      ))
  }
