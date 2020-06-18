# select_model
#' View model metrics and record best model interactively
#' Model metrics are displayed as a table in an R Shiny application.Check boxes next to models allow users to record preferred or best model.
#' 
#' @param project String, name of project.
#' @importFrom DBI dbExistsTable dbDisconnect dbConnect dbRemoveTable dbExecute dbGetQuery 
#' @importFrom DT DTOutput renderDT JS
#' @import shiny
#' @export
#' @details Opens an interactive data table that displays model measures of fit for each model run saved in the model measures of fit table 
#' in the FishSETdatabase. The name of this table should contain the string 'out.mod'. Users can delete models from the table and select the 
#' preferred model by checking the 'selected' box. The table is then saved to the FishSET database with two new columns added, a TRUE/FALSE 
#' selected column and the date it was selected. The table is saved with the phrase 'modelChosen' in the FishSET database. The function can 
#' also be called indirectly in the \code{\link{discretefish_subroutine}} by specifying the \code{select.model} argument as TRUE. 
#' The 'modelChosen' table is not used in any functions. The purpose of this function and the 'modelChosen' table is to save a reference of the preferred model.
#' @examples 
#' \dontrun{
#' select_model('pollockmodelfit', overwrite.table=FALSE)
#' }

select_model <- function(project){
  
#  requireNamespace("shiny")
  

  shinyApp(
    ui = fluidPage(
     
      tabPanel('Select models', 
      sidebarLayout(
        sidebarPanel(
          tags$br(),tags$br(),
          actionButton("delete_btn", "Delete row"),
          h3(''),
          actionButton("submit_ms", "Save table", style="color: #fff; background-color: #337ab7; border-color: #2e6da4;"),
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
          h3('Model Output'),
          DT::DTOutput("mytable"),
          tags$script(HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
                           Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());})"))
          )
        ))),
    
    server = function(input, output, session) {
      # helper function for making checkbox
      
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
      
      this_table <- reactiveVal(data.frame(t(DBI::dbGetQuery(DBI::dbConnect(RSQLite::SQLite(), locdatabase()), 
                                                             paste0("SELECT * FROM ", paste0(project, "modelfit"))))))#,Select=shinyInput(checkboxInput,nrow(t(out.mod)),"cbox_")))
      
      observeEvent(input$delete_btn, {
        
        t = this_table()
        if (!is.null(input$mytable_rows_selected)) {
          t <- t[-as.numeric(input$mytable_rows_selected),]
        }
        this_table(t)
        session$sendCustomMessage('unbind-DT', 'mytable')
      })
      
      # datatable with checkbox
      output$mytable <- DT::renderDT({
        data.frame(this_table(),Select=shinyInput(checkboxInput,nrow(this_table()),"cbox_"))
      }, colnames=c('Model','AIC','AICc','BIC','PseudoR2','Selected'),  filter='top', server = FALSE, escape = FALSE, options = list( 
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
          if (is.null(value)) NA else value 
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
      # stop shiny
      observe({
        if (input$close > 0) stopApp()                             
      })
      
      
}
  )
}
