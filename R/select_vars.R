# Data selection tool
# Select which variables to include in the further analyses and modeling.

# select_vars
#' View and select which variables to include
#'
#' @param dat Main data frame containing data on hauls or trips. Table in fishset_db database should contain the string `MainDataTable`.
#' @param project Name of project. Parameter is used to generate meaningful table names in fishset_db database.
#' @importFrom DBI  dbDisconnect dbConnect dbListTables dbWriteTable 
#' @import shiny
#' @export select_vars
#' @details Opens an interactive table that allows uses to select which variables to included by clicking check boxes. 
#' Data should be loaded into the fishset_db database before running this function. Select variables that will be used to generate further variables, such as rates or cpue, and variables to be included in models.
#' @examples 
#' \dontrun{
#' select_vars('pcodMainDataTable')
#' }

select_vars <- function(dat, project){
  library(shiny)

  runApp(list(
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
          h3('Select variables to retain for analyses and modeling'),
          h5('Only selected variables will be available in the working data set. All variables will be retained in the saved raw data set.'),
          h5('Variables can be added back into the to working data set using the add_vars function.'),
        #Checkbox input widget  
          tags$div(align = 'left', 
                   class = 'multicol', checkboxGroupInput("columns", "", choices = colnames(dat), 
                                                          selected = colnames(dat)[grep('lat|long', colnames(dat), ignore.case=TRUE)], 
                                                          inline=FALSE)),
        tags$br(), tags$br(),
        h4('First five rows of data selected.'),
          tableOutput("data_table")
        ))),
    
    server = function(input, output, session) {
        suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"))
        if(is.character(dat)==TRUE){
          if(is.null(dat)==TRUE | table_exists(dat)==FALSE){
            print(DBI::dbListTables(fishset_db))
            stop(paste(dat, 'not defined or does not exist. Consider using one of the tables listed above that exist in the database.'))
          } else {
           dat <- table_view(dat)
          }
        } else {
          dat <- dat  
        }
        DBI::dbDisconnect(fishset_db)
      dat$linkID <- seq_along(dat[,1])  
      dInput <- reactive({
        dat
      })
                         
     data_table <- reactive({
     # If missing input, return to avoid error later in function
         if(is.null(dat))
            return(NULL)
                           
            # Get the data set
            dataset <- dInput()
                           
            # Keep the selected columns
            dataset[, c(input$columns, 'linkID'), drop = FALSE]
            
           })
                         
        output$data_table <- renderTable(head(data_table()))
                         
  
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

