
log_rerun <- function(log_file, dat = NULL, portTable = NULL, aux = NULL, 
                      gridfile = NULL, spat = NULL, ind = NULL, run = FALSE) {
  #' Console function for rerunning project log
  #' 
  #' @param log_file String, name of the log file starting with the date (YYYY-MM-DD) and
  #'   ending in ".json". 
  #' @param dat String, new primary data table to rerun log 
  #' @param portTable String, name of port table. Defualts to NULL.
  #' @param aux String, name of auxiliary table. Defaults to NULL.
  #' @param gridfile String, name of gridded data table. Defaults to NULL.
  #' @param spat String, name of spatial data table. Defaults to NULL.
  #' @param ind Numeric, indices of function calls to rerun. 
  #' @param run Logical, whether to run the logged function calls (TRUE) or simply
  #'   list all function calls (FALSE). 
  #' @export
  #' @seealso \code{\link{log_rerun_gui}}
  #' @importFrom jsonlite fromJSON
  #' @importFrom shiny showNotification isRunning
  #' @importFrom rlang call2
  #' @examples 
  #' \dontrun{
  #' log_rerun("pollock_2020-10-23.json", run = TRUE) # reruns entire log with original data table
  #' # runs log with new data table
  #' log_rerun("pollock_2020-10-23.json", dat = "pollockMainDataTable", run = TRUE) 
  #' }
  
  end <- FALSE
  
  project <- gsub("\\_.*", "", log_file)
  if (!file.exists(paste0(loclog(project=project), log_file))) {
    
    warning(log_file, " does not exist. Run list_logs() or project_logs().")
    end <- TRUE
  } 
  
  if (end == FALSE) {

    out <- jsonlite::fromJSON(paste0(loclog(project=project), log_file), simplifyVector = FALSE) # import as list, preserves class type
    
    out <- out$fishset_run[[2]]$function_calls # list of just function calls
    
    # check if logged args match function formals 
    fun_name <- vapply(seq_along(out), function(i) out[[i]]$functionID, character(1))
    
    log_arg_len <- vapply(seq_along(out), function(i) length(out[[i]]$args), numeric(1))
    fun_arg_len <- vapply(seq_along(out), function(i) {
      
      arg_nm <- names(formals(out[[i]]$functionID))
      length(arg_nm[arg_nm != "..."])
    },  numeric(1))
    
    if (any(log_arg_len != fun_arg_len)) {
      
      fun_name <- unique(fun_name[log_arg_len != fun_arg_len])
      
      if (shiny::isRunning()) { # if app is running
        
        shiny::showNotification("Logged arguments do not match function formals for the following functions: ", 
                                paste(fun_name, collapse = ", "), type = "warning", duration = 60)
      } 
      
      warning("Logged arguments do not match formals for the following functions: ", 
              paste(fun_name, collapse = ", "))
    }
    
    # add names to args
    for (i in seq_along(out)) {
      
      arg_names <- names(formals(out[[i]]$functionID))[1:length(out[[i]]$args)]
      names(out[[i]]$args) <- arg_names[arg_names != "..."] 
    }
    
    # handling args containing a vector or list 
    for (i in seq_along(out)) {
      
      for (j in names(out[[i]]$args)) {
        
        if (length(out[[i]]$args[[j]]) > 1) {
          
          if (all(vapply(out[[i]]$args[[j]], FUN = length, numeric(1)) == 1)) { # case for vectors
            
            out[[i]]$args[[j]] <- unlist(out[[i]]$args[[j]]) # collapse list to vector
            
          } else { # case for lists
            
            arg_list <-
              lapply(out[[i]]$args[[j]], function(x) {
                
                if (length(x) > 1) unlist(x) # collapse to single list
                else x
              })
            
            out[[i]]$args[[j]] <- arg_list
          }
        }
      }
    }
    
    # concatenate kwargs list to args list
    for (i in seq_along(out)) {
      
      out[[i]]$args <- c(out[[i]]$args, out[[i]]$kwargs)
    }
    
    # convert args to call
    call_list <- 
      lapply(seq_along(out), function(i) {
        
        rlang::call2(out[[i]]$functionID, !!!out[[i]]$args)
      })
    
    replace_dat <- function(clist, dat_type, new_dat) {
      
      for (i in seq_along(clist)) {
        
        for (j in names(clist[[i]])) {
          
          if (j == dat_type) {
            
            clist[[i]][j] <- new_dat
          }
        }
      }
      clist
    }
    
    if (!is.null(dat)) {
      
      call_list <- replace_dat(call_list, "dat", new_dat = dat)
    }
    
    # change port table
    if (!is.null(portTable)) {
      
      call_list <- replace_dat(call_list, "portTable", new_dat = portTable)
    }
    
    # change aux table
    if (!is.null(aux)) {
      
      call_list <- replace_dat(call_list, "aux", new_dat = aux)
    }
    
    # change gridded table
    if (!is.null(gridfile)) {
      
      call_list <- replace_dat(call_list, "gridfile", new_dat = gridfile)
    }
    
    # change spatial table
    if (!is.null(spat)) {
      
      call_list <- replace_dat(call_list, "spat", new_dat = spat)
    }
    
    # index of calls to run 
    if (!is.null(ind)) call_list <- call_list[ind]
    
    if (run == TRUE) lapply(call_list, eval)
    else call_list
  }
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
          uiOutput('projects'),
          uiOutput('logfilename'),
          checkboxInput("new_dat_cb", "Run log with different data table"),
          uiOutput('project_choices'),
          
          p("Click on the table rows to run specific function calls.")
        ),
        
        mainPanel(
          
          shinycssloaders::withSpinner(DT::DTOutput("log_table"))
        )
      )
    ),
    
    server = function(input, output, session) {
      
      output$projects <- renderUI({ 
        selectInput('project', 'Select a project', choices =list_dirs(path=locproject()))
        #print(projects())
      })
        
      output$logfilename <- renderUI({
        req(input$project)
        selectInput("log", "Select a log file", choices = list_logs(project = input$project))
      })
      
      
      fetch_log <- reactive(log_rerun(log_file=input$log, run = FALSE))
      
      
      output$project_choices <- renderUI({
      req(input$project)
      conditionalPanel("input.new_dat_cb",
                      
                       selectizeInput("new_dat", "Choose main table", 
                                      choices = main_tables(project=input$project), multiple = TRUE,
                                      options = list(maxItems = 1)), # sets dat to NULL by default
                       
                       selectizeInput("new_port", "Choose port table", 
                                      choices = list_tables(project=input$project, type = "port"), multiple = TRUE,
                                      options = list(maxItems = 1)),
                       
                       selectizeInput("new_aux", "Choose aux table", 
                                      choices = tables_database(project=input$project), multiple = TRUE,
                                      options = list(maxItems = 1)),
                       
                       selectizeInput("new_grid", "Choose gridded table", 
                                      choices = tables_database(input$project), multiple = TRUE,
                                      options = list(maxItems = 1)),
                       
                       selectizeInput("new_spat", "Choose spatial table", 
                                      choices = tables_database(project=input$project), multiple = TRUE,
                                      options = list(maxItems = 1))
      )
      })
      
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
        
        showNotification("Log has been successfully rerun", type = "message", duration = 60)
      })
      
      observeEvent(input$close, stopApp())
    }
  )
}
