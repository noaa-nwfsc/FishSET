# Data selection tool
# Select which variables to include in the further analyses and modeling.

# select_vars
#' Interactive application to select variables to include/exclude in primary dataset
#'
#' Opens an R Shiny web application. With the application select on
#'   variables in the primary dataset that should be retained.
#' @param dat Primary data containing information on hauls or trips.
#'   Table in the FishSET database contains the string 'MainDataTable'.
#' @param project String, name of project.
#' @importFrom DBI  dbDisconnect dbConnect dbListTables dbWriteTable
#' @import shiny
#' @export select_vars
#' @details Opens an interactive table that allows users to select which variables to be included by clicking check boxes.
#'   Data should be loaded into the FishSET database before running this function. Select variables that will be used to
#'   generate further variables, such as rates or cpue, and variables to be included in models.
#'   Removed variables can be added back into the dataset at a later date using the \code{\link{add_vars}} function.
#' @examples
#' \dontrun{
#' select_vars(pcodMainDataTable, "pcod")
#' }
#'
select_vars <- function(dat, project) {
  #  requireNamespace(shiny)


  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main")
  
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
                                 "))),
      tags$head(tags$style(HTML(
        ".checkbox { 
                      margin-left: 0px;
                      margin-right: 10px;
                    }
                    .checkbox+.checkbox {
                      margin-left: 0px;
                      margin-right: 10px;
                    }
                    "
      ))),
      sidebarLayout(
        sidebarPanel(
          tags$br(), tags$br(),
          actionButton("submit", "Save selected data", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4;"),
          tags$br(), tags$br(),
          tags$button(
            id = "close",
            type = "button",
            style = "color: #fff; background-color: #FF6347; border-color: #800000;",
            class = "btn action-button",
            onclick = "setTimeout(function(){window.close();},500);", # close browser
            "Close window"
          )
        ),
        mainPanel(
          h3("Select variables to retain for analyses and modeling"),
          h5("Only selected variables will be available in the working data set. All variables will be retained in the saved raw data set."),
          h5("Variables can be added back into the to working data set using the add_vars function."),
          # Checkbox input widget
          tags$div(
            align = "left",
            class = "multicol", checkboxGroupInput("columns", "",
              choices = colnames(dataset),
              selected = colnames(dataset),
              inline = FALSE
            )
          ),
          tags$br(), tags$br(),
          h4("First five rows of data selected."),
          tableOutput("data_table")
        )
      )
    ),

    server = function(input, output, session) {

      # dataset$linkID <- seq_along(dataset[,1])
      dInput <- reactive({
        dataset
      })
      # values <- reactiveValues(dataset=dataset)

      data_table <- reactive({
        # If missing input, return NULL to avoid error later in function
        if (is.null(dInput())) {
          return(NULL)
        }

        # Get the data set
        values <- dInput()

        # Keep the selected columns
        values$linkID <- seq_along(dataset[, 1])
        values[, c(input$columns, "linkID"), drop = FALSE]
      })

      output$data_table <- renderTable(head(data_table()))


      # When the Submit button is clicked, save the form data
      observeEvent(input$submit, {
        # Connect to the database
        print("Data table updated. ")
        suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project=project)))
        DBI::dbWriteTable(fishset_db, paste0(project, "MainDataTable"), data_table(), overwrite = TRUE)

        showNotification(paste0("Table saved to database as ", project, "MainDataTable.", ". 
                                Enter", paste0('"', project, 'MainDataTable"'), "as the dat parameter in future function calls or
                                load the updated data using", paste0('table_view("', project, 'MainDataTable",', project,')'), ". 
                                The app window can be closed."))
        DBI::dbDisconnect(fishset_db)
      })



      # stop shiny
      observe({
        if (input$close > 0) stopApp()
      })
    }
  )
}
