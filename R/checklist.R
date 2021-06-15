checklist <- function(project, modDesignTab = NULL) {
  #' Model checklist
  #' 
  #' Determines whether final data passes quality checks, that occurrence points
  #' have been checked, that an alternative choice matrix has been created, and
  #' if an expected catch/revenue matrix is required. 
  #'
  #' @param project String, name of project.
  #' @param modDesignTab The model design table. Only used in the FishSET app.
  #' @export
  #' @import shiny
  #' @keywords internal
  #' 
  
  check <- list(qaqc = list(pass = FALSE, msg = NULL),
                occur_pnts = list(pass = FALSE, msg = NULL),
                alt_choice = list(pass = FALSE, msg = NULL),
                expect_catch = list(pass = FALSE, msg = NULL))
  
  # quality checks
  if (table_exists(paste0(project, "MainDataTable_final"))) {
    
    check$qaqc$pass <- TRUE
    check$qaqc$msg <- list(NAs = "No NAs, NaNs, or Inf values.",
                           UR = "All rows are unique.",
                           LL = "Latitude and Longitude variables are in decimal degrees.")
  } else {
    check$qaqc$msg <- paste("No final data set for project \"", project,"\" has been",
    "saved to FishSET Database. Run check_model_data() in the console or click",
    "\"Save final table to FishSET DB\" on the \"Compute New Variables\" tab before",
    "running model.")
  }
  
  # check that map_plot, map_kernel, or map_viewer has been run
  sum_list <- function_summary(project = project, date = NULL, type = "dat_exploration")
  nms <- names(sum_list)
  m_nms <- c("map_viewer", "map_plot", "map_kernel")
  
  if (any(m_nms %in% nms)) check$occur_pnts$pass <- TRUE
  else {
    
    check$occur_pnts$msg <- 
      paste("map_viewer, map_plot, or map_kernel functions have not been run.",
            "Run at least one of these functions to determine whether occurrence points",
            "are valid.")
  }
  
  # find alternative choice matrix
  alt_c <- list_tables(project, type = "altc")
  
  if (length(alt_c) > 0) check$alt_choice$pass <- TRUE 
  else {
    
    check$alt_choice$msg <-
      paste0("No alternative choice matrix found for project \"", project, "\".")
  }
  
  # find expected catch matrices (if logit_c used)
  ec_required <- FALSE
  e_catch <- list_tables(project, type = "ec") 
  ec_exists <- ifelse(length(e_catch) > 0, TRUE, FALSE)
  
  if (!is.null(modDesignTab)) { # app only
    # check if logit_c is used
    if (any(modDesignTab$likelihood %in% "logit_c")) ec_required <- TRUE
  }
  
 if (ec_required == TRUE & ec_exists == FALSE) {
   
   check$expect_catch$msg <-
     paste("ExpectedCatch matrix is required for conditional logit (logit_c) likelihood function.",
            "Run create_expectations() in the console or go to the \"Expected Catch/Revenue\"", 
            "tab of the app.")
   check$expect_catch$pass <- FALSE
 
  } else if (ec_required == FALSE & ec_exists == FALSE) {
    
    check$expect_catch$msg <- 
      paste0("ExpectedCatch matrix does not exist for project '", project,"'.",
             " (Only required for the logit_c likelihood function). To include an ",
             "ExpectedCatch matrix in your model, run create_expectations() in ", 
             "the console or go to the \"Expected Catch/Revenue\" tab of the app.")
    check$expect_catch$pass <- TRUE
  
  } else check$expect_catch$pass <- TRUE
  
  # app for discretefish_subroutine 
  if (!isRunning()) {
    
    runApp(
      list(
      ui = basicPage(
        
        tags$head(tags$style(HTML("
                                  div {
                                    margin: auto;
                                    width: 80%;
                                   }
                          
                                  ul {
                                    list-style-type: none;
                                    font-size: 16px;
                                    text-align: justify;
                                  }
                                  
                                  li {padding: 1%}
                                  
                                  .fa-check {
                                    color:green;
                                    font-size: 1.5em;
                                  }
                                  
                                  .fa-times {
                                    color:red;
                                    font-size: 1.5em;
                                  }
                                  
                                  .fa-exclamation-triangle {
                                    color: #FFCC00;
                                    font-size: 1.5em;
                                  }
                                  "))),
    
        tags$button(
          id = "close",
          type = "button",
          style="color: #fff; background-color: #FF6347; border-color: #800000;",
          class = "btn action-button",
          onclick = "setTimeout(function(){window.close();},500);",  # close browser
          "Close app"),
        
        uiOutput("checkList")
      ),
      
      server = function(input, output, session) {
        
        observeEvent(input$close, stopApp())
        
        # message functions
        passed <- function(type) check[[type]]$pass
        
        pass_icon <- function(type) {
          
          if (passed(type)) {
            if (type == "expect_catch" & ec_required==FALSE & ec_exists==FALSE) {
              
              icon("exclamation-triangle")
            } else icon("check")
            
          } else icon("times")
        }
        
        show_msg <- function(type) {
          if (!passed(type)) tags$ul(tags$li(check[[type]]$msg))
        }
        
        qaqc_msg <- function() {
          
          if (passed("qaqc")) {
            
            out <- lapply(check$qaqc$msg, function(x) tags$li(icon("check"), x))
            
            tags$ul(out)
          }
        }
        
        ec_msg <- function() {
          
          if (ec_required == FALSE & ec_exists == FALSE) {
            tags$ul(tags$li(check$expect_catch$msg))
          }
        }
   
        checklist_html <- reactive({
          
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
        
        output$checkList <- renderUI(checklist_html())
      }
      )
    )
  }
  
  check
}
