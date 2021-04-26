# Data selection tool
# Add variables from raw data set back into the working data set - interactive version

# add_vars_gui
#' Add removed variables back into dataset
#' 
#' Add columns that have been removed from the primary dataset back into the primary dataset. 
#'
#' @param working_dat Primary data containing information on hauls or trips. Table in FishSET database contains the string 'MainDataTable'.
#' @param raw_dat Unmodified raw version of the primary dataset. Should be a character specifying a table from the FishSET database containing the string ‘MainDataTable’ and date table was created.
#' @param project String, name of project. 
#' @importFrom DBI  dbDisconnect dbConnect dbListTables dbWriteTable 
#' @import shiny
#' @export add_vars
#' @details Opens an interactive table that allows users to select which variables to be added back into the working dataset. \cr
#' The removed variables are obtained from the \code{raw_dat} and merged into the working data based on a row identifier. The row identifier is created when the variable is removed using the \code{\link{select_vars}} function. The row identifier is used to match the raw data variables to \code{working_dat}.
#' @examples 
#' \dontrun{
#' select_vars_gui(pcodMainDataTable)
#' add_vars_gui(pcodMainDataTable, 'pcodMainDataTable20100101', 'pcod')
#' }

#Selectbox is column names in raw data that are not in working data
#Show top five rows of working data
#Drop rows that are not in linkID

add_vars_gui <- function(working_dat, raw_dat, project){
  
  
  #Pull in data
  out <- data_pull(working_dat)
   working_dat <- out$dataset
  
  out <- data_pull(raw_dat)
  raw_dat <- out$dataset
  
  shinyApp(
    ui = fluidPage(
      # Style formatting
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
      
      data_table_update <- reactive({
        # If missing input, return to avoid error later in function
        if(is.null(raw_dat))
          return(NULL)
        
        # Get the data set
        dataset <- dInputraw()
        
        # Keep the selected columns
        dataset <- dataset[, c(input$columns, 'linkID'), drop = FALSE]
        dataset <- merge(dataset, working_dat, by='linkID')
      })
      
      
      output$data_table <- renderTable(head(data_table_update()))
      
      
      # When the Submit button is clicked, save the form data
      observeEvent(input$submit, {
        # Connect to the database
        suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
        DBI::dbWriteTable(fishset_db, paste0(project, 'MainDataTable',  format(Sys.Date(), format="%Y%m%d")), data_table_update(), overwrite=TRUE)
        
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

