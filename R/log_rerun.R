
log_rerun <- function(log_file, dat = NULL, ind = NULL, run = FALSE) {
  #' Console function for rerunning project log
  #' 
  #' @param log_file String, name of the log file starting with the date (YYYY-MM-DD) and
  #'   ending in ".json". 
  #' @param dat String, new data table to rerun log with.
  #' @param ind Numeric, indices of function calls to rerun. 
  #' @param run Logical, whether to run the logged function calls (TRUE) or simply
  #'   list all function calls (FALSE). 
  #' @export
  #' @seealso \code{\link{log_rerun_gui}}
  #' @importFrom jsonlite fromJSON
  #' @importFrom shiny showNotification
  #' @importFrom rlang exprs call2
  #' @examples 
  #' \dontrun{
  #' log_rerun("2020-10-23.json", run = TRUE) # reruns entire log with original data table
  #' log_rerun("2020-10-23.json", dat = "pollockMainDataTable", run = TRUE) # runs log with new data table
  #' }
  
  out <- jsonlite::fromJSON(paste0(loclog(), log_file), simplifyVector = FALSE) # import as list, preserves class type
  
  out <- out$fishset_run[[2]]$function_calls # list of just function calls
  
  # check if logged args match function formals 
  fun_name <- vapply(seq_along(out), function(i) out[[i]]$functionID, character(1))
  
  log_arg_len <- vapply(seq_along(out), function(i) length(out[[i]]$args), numeric(1))
  fun_arg_len <- vapply(seq_along(out), function(i) length(names(formals(out[[i]]$functionID))),  numeric(1))
  
  if (any(log_arg_len != fun_arg_len)) {
    
    fun_name <- unique(fun_name[log_arg_len != fun_arg_len])
    
    if (isRunning()) { # if app is running
      
      shiny::showNotification("Logged arguments do not match function formals for the following functions: ", 
                       paste(fun_name, collapse = ", "), type = "warning", duration = 10)
    } 
    
    warning("Logged arguments do not match formals for the following functions: ", 
            paste(fun_name, collapse = ", "))
  }
  
  # add names to args
  for (i in seq_along(out)) {
    
    names(out[[i]]$args) <- names(formals(out[[i]]$functionID))
  }
  
  # concatenate kwargs list to args list
  for (i in seq_along(out)) {
    
    out[[i]]$args <- c(out[[i]]$args, out[[i]]$kwargs)
  }
  
  # convert args to call
  call_list <- 
    lapply(seq_along(out), function(i) {
      
      expr_list <- rlang::exprs(!!!out[[i]]$args)
      rlang::call2(out[[i]]$functionID, !!!expr_list)
    })
  
  # change data table
  if (!is.null(dat)) {
    
    for (i in seq_along(call_list)) {
      
      call_list[[i]]$dat <- dat
    }
  }
  
  # index of calls to run 
  if (!is.null(ind)) call_list <- call_list[ind]
  
  if (run == TRUE) lapply(call_list, eval)
  else call_list
}



log_rerun_gui <- function() {
  #' 
  #' Interactive function for rerunning project log
  #' 
  #' @export
  #' @import shiny
  #' @importFrom tibble rownames_to_column remove_rownames
  #' @importFrom DT renderDT DTOutput
  #' @importFrom shinycssloaders withSpinner
  #' @seealso \code{\link{log_rerun}}
  #' @examples 
  #' \dontrun{
  #' log_rerun_gui()
  #' }
  
  shinyApp(
    
    ui = fluidPage(
      
      sidebarLayout(
        
        sidebarPanel(
          
          tags$button(
            id = "close",
            type = "button",
            style="color: #fff; background-color: #FF6347; border-color: #800000;",
            class = "btn action-button",
            onclick = "setTimeout(function(){window.close();},500);",  # close browser
            "Close app"
          ),
          
          actionButton("run_log", "Rerun log",
                       style="color: #fff; background-color: #6da363; border-color: #800000;"),
          
          selectInput("log", "Select a log file", choices = list_logs()),
          
          checkboxInput("new_dat_cb", "Run log with different data table"),
          
          conditionalPanel("input.new_dat_cb",
                           
                           selectizeInput("new_dat", "Choose a new table", 
                                          choices = list_MainDataTables(), multiple = TRUE,
                                          options = list(maxItems = 1)) # sets dat to NULL by default
          ),
          
          p("Click on the table rows to run specific function calls.")
        ),
        
        mainPanel(
          
          shinycssloaders::withSpinner(DT::DTOutput("log_table"))
        )
      )
    ),
    
    server = function(input, output, session) {
      
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
        
        log_rerun(input$log, dat = input$new_dat, 
                   ind = input$log_table_rows_selected, run = TRUE)
        
        showNotification("Log has been successfully rerun.", type = "message", duration = 10)
      })
      
      observeEvent(input$close, stopApp())
    }
  )
}