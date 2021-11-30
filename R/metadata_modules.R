# Modules used in metadata_gui and Main app

metaProjUI <- function(id) {
  #' Project UI for metadata gui
  #' @param id An ID string that matches the corresponding server function: 
  #'   \code{\link{metaCreateProjServ}} or  \code{\link{metaEditProjServ}}. 
  #' @keywords internal
  #' @import shiny
  #' @details This module is used in the \code{\link{metadata_gui}} function 
  #'   and in the FishSET app. It creates the UIs for project name and available
  #'   project tables. 
  
  ns <- NS(id)
  tagList(
    uiOutput(ns("proj_select")), 
    uiOutput(ns("proj_tabs_list")),
    actionButton(ns("refresh"), "Refresh tables")
  )
}


metaCreateProjServ <- function(id) {
  #' Project server for metadata gui (Create section)
  #' @param id An ID string that matches the corresponding UI function: 
  #'   \code{\link{metaProjUI}}. 
  #' @keywords internal
  #' @import shiny
  #' @details This module is used in the \code{\link{metadata_gui}} function 
  #'   and in the FishSET app. It creates the UIs for project name and available
  #'   project tables. 
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    proj_tabs <- reactive({
      input$refresh
      suppressWarnings(project_tables(input$project_name))
    })
    
    output$proj_select <- renderUI({
      
      if (length(projects()) > 0) {
        
        selectInput(ns("project_name"), "Select project", choices = projects())
        
      } else {
        
        p("No projects found in FishSET Database. Create a project by uploading a new file")
      }
    })
    
    
    output$proj_tabs_list <- renderUI({
      
      if (!is.null(input$project_name)) {
        
        selectInput(ns("project_tab"), "Select table", choices = proj_tabs())
      }
    })
  })
}


metaEditProjServ <- function(id) {
  #' Project server for metadata gui (Edit section)
  #' @param id An ID string that matches the corresponding UI function: 
  #'   \code{\link{metaProjUI}}. 
  #' @keywords internal
  #' @import shiny
  #' @details This module is used in the \code{\link{metadata_gui}} function 
  #'   and in the FishSET app. It creates the UIs for project name and available
  #'   project tables. 
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    meta_tabs <- reactive({
      input$refresh
      suppressWarnings(meta_tables(input$project_name))
    })
    
    output$proj_select <- renderUI({
      
      if (length(projects()) > 0) {
        
        selectInput(ns("project_name"), "Select project", choices = projects())
        
      } else {
        
        p("No projects found in FishSET Database. Create a project by uploading a new file")
      }
    })
    
    
    output$proj_tabs_list <- renderUI({
      
      if (!is.null(input$project_name)) {
          
        selectInput(ns("project_tab"), "select table", choices = meta_tabs())
      }
    })
  })
}


metaCreateSaveUI <- function(id) {
  #' Load and Save buttons for metadata gui (Create section)
  #' @param id An ID string that matches the corresponding server functions: 
  #'   \code{\link{metaCreateLoadServ}} and \code{\link{metaSaveServ}}. 
  #' @keywords internal
  #' @import shiny
  #' @details This module is used in the \code{\link{metadata_gui}} function 
  #'   and in the FishSET app. It creates the buttons needed to load and save 
  #'   metadata. 
  
  ns <- NS(id) 
  
  tagList(
    actionButton(ns("load"), "Load data", 
                 style = "color: white; background-color: blue;"),
    
    actionButton(ns("save"), "Create meta",
                 style = "color: white; background-color: blue;"),
    
    tags$br(),
    
    checkboxInput(ns("overwrite"), "Overwrite existing metadata?", value = FALSE)
  )
}


metaEditSaveUI <- function(id) {
  #' Load and Save buttons for metadata gui (Edit section)
  #' @param id An ID string that matches the corresponding server functions: 
  #'   \code{\link{metaEditLoadServ}} and \code{\link{metaSaveServ}}. 
  #' @keywords internal
  #' @import shiny
  #' @details This module is used in the \code{\link{metadata_gui}} function 
  #'   and in the FishSET app. It creates the buttons needed to load and save 
  #'   metadata. 
  
  ns <- NS(id) 
  
  tagList(
    actionButton(ns("load"), "Load meta", 
                 style = "color: white; background-color: blue;"),
    
    actionButton(ns("save"), "Save meta",
                 style = "color: white; background-color: blue;"),
    
    tags$br(),
    
    checkboxInput(ns("overwrite"), "Overwrite existing metadata?", value = FALSE)
  )
}


aboutUI <- function(id) {
  #' UIs for entering general metadata 
  #' @param id An ID string that matches the corresponding server functions: 
  #'   \code{\link{metaCreateLoadServ}} or \code{\link{metaEditLoadServ}} and 
  #'   \code{\link{metaSaveServ}}. 
  #' @keywords internal
  #' @import shiny
  #' @details This module is used in the \code{\link{metadata_gui}} function 
  #'   and in the FishSET app. It creates text boxes for general metadata. These
  #'   include author, date created, confidentiality, date modified, version, 
  #'   general description, source, collection method, and intended use. 
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      
      column(6,
             textAreaInput(ns("auth"), "Author(s)", width = "100%", resize = "vertical"),
             textAreaInput(ns("date_c"), "Date created", width = "100%", resize = "vertical"),
             textAreaInput(ns("confid"), "Confidentiality", width = "100%", resize = "vertical")),
      
      column(6,
             textAreaInput(ns("date_m"), "Last modified", width = "100%", resize = "vertical"),
             textAreaInput(ns("ver"), "Version", width = "100%", resize = "vertical"))
    ),
    
    h4(strong("About")),
    
    fluidRow(
      
      column(6,
             textAreaInput(ns("about"), "General description", width = "100%", resize = "vertical"),
             textAreaInput(ns("src"), "Source(s)", width = "100%", resize = "vertical"),
             textAreaInput(ns("coll_meth"), "Collection method", width = "100%", resize = "vertical"),
             textAreaInput(ns("intend"), "Intended use", width = "100%", resize = "vertical")),
      
      column(6,
             textAreaInput(ns("time"), "Timeframe", width = "100%", resize = "vertical"),
             textAreaInput(ns("geo"), "Geographic coverage", width = "100%", resize = "vertical"),
             textAreaInput(ns("species"), "Species", width = "100%", resize = "vertical"))
    )
  )
}


colDescUI <- function(id, nm, value = "") {
  #' Create a text box for each column in data
  #' @param id An ID string that matches the corresponding server functions: 
  #'   \code{\link{metaCreateLoadServ}} or \code{\link{metaEditLoadServ}} and 
  #'   \code{\link{metaSaveServ}}. 
  #' @param nm Column name passed to \code{textAreaInput} id.
  #' @param value Value passed to \code{textAreaInput}.
  #' @keywords internal
  #' @import shiny
  #' @details This module is used in the \code{\link{metadata_gui}} function 
  #'   and in the FishSET app. It creates a text box for each column in a FishSET
  #'   table.
  
  nm <- make.names(nm)
  ns <- NS(id)
 
    textAreaInput(ns(nm), nm, width = "100%", resize = "vertical", value = value)
}


contactUI <- function(id) {
  #' UIs for entering metadata contact info  
  #' @param id An ID string that matches the corresponding server functions: 
  #'   \code{\link{metaCreateLoadServ}} or \code{\link{metaEditLoadServ}} and 
  #'   \code{\link{metaSaveServ}}. 
  #' @keywords internal
  #' @import shiny
  #' @details This module is used in the \code{\link{metadata_gui}} function 
  #'   and in the FishSET app. It creates text boxes for contact info. These
  #'   include person, organization, address, phone number, email, license,
  #'   citation, and general purpose "other" category. 
  
  ns <- NS(id)
  
  tagList(
    
    h4(strong("Contact Info")),
    
    fluidRow(
      
      column(6,
             textAreaInput(ns("person"), "Person", width = "100%", resize = "vertical"),
             textAreaInput(ns("org"), "Organization", width = "100%", resize = "vertical"),
             textAreaInput(ns("address"), "Address", width = "100%", resize = "vertical"),
             textAreaInput(ns("phone"), "Phone", width = "100%", resize = "vertical")),
      
      column(6,
             textAreaInput(ns("email"), "Email", width = "100%", resize = "vertical"),
             textAreaInput(ns("license"), "License", width = "100%", resize = "vertical"),
             textAreaInput(ns("cite"), "Citation", width = "100%", resize = "vertical"),
             textAreaInput(ns("other"), "Other", width = "100%", resize = "vertical"))
    )
  )
}


metaColLayoutUI <- function(id) {
  #' Arrange column UIs
  #' @param id An ID string that matches the corresponding server function: 
  #'   \code{\link{metaColLayoutServ}}. 
  #' @keywords internal
  #' @import shiny
  #' @details This module is used in the \code{\link{metadata_gui}} function 
  #'   and in the FishSET app. It arranges column text boxes in the main panel.
  
  ns <- NS(id)
  
  uiOutput(ns("col_desc"))
}

# create col description layout
metaColLayoutServ <- function(id, cols) {
  #' Arrange column UIs
  #' @param id An ID string that matches the corresponding UI function: 
  #'   \code{\link{metaColLayoutUI}}.
  #' @keywords internal
  #' @import shiny
  #' @details This module is used in the \code{\link{metadata_gui}} function 
  #'   and in the FishSET app. It arranges column text boxes  in the main panel.
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    # works well for sidebar layout, others? Add type = "..." arg?
    output$col_desc <- renderUI({
      
      if (!is.null(cols$nms)) {
        
        nc <- length(cols$nms)
        
        cutoff <- ifelse(nc %% 2 == 0, nc/2, median(seq(nc)))
        
        col1 <- 1:cutoff
        col2 <- (cutoff + 1):nc
        
        fluidRow(
          
          column(6, cols$ui[col1]),
          column(6, cols$ui[col2]))
      }
    })
    
  })
}


metaOut <- function(id) {
  #' Output UI for metadata
  #' @param id An ID string that matches the corresponding server functions: 
  #'   \code{\link{metaCreateLoadServ}} or \code{\link{metaEditLoadServ}} and 
  #'   \code{\link{metaSaveServ}}.
  #' @keywords internal
  #' @import shiny
  #' @details This module is used in the \code{\link{metadata_gui}} function 
  #'   and in the FishSET app. It outputs metadata to the main panel.
  
  ns <- NS(id)
  
  uiOutput(ns("meta_out"))
  
}


metaRawUI <- function(id) {
  #' UIs for handling raw metadata
  #' @param id An ID string that matches the corresponding server functions: 
  #'   \code{\link{metaRawServ}} and \code{\link{metaRawOut}}. 
  #' @keywords internal
  #' @import shiny
  #' @details This module is used in the \code{\link{metadata_gui}} function 
  #'   and in the FishSET app. It allows users to import and save raw metadata
  #'   from file.
  #'   
  ns <- NS(id)
  
  tagList(
    h4(strong("Import raw metadata")),
    
    actionButton(ns("load_raw"), "Load raw meta",
                 style = "color: white; background-color: blue;"),
    
    actionButton(ns("clear_raw"), "Clear raw meta",
                 style = "color: #fff; background-color: red; border-color:#000000;"),
    
    fileInput(ns("raw_file"), "Download metadata file"),
    
    uiOutput(ns("readerUI"))
  )
}


metaRawOut <- function(id) {
  #' Output UI for raw metadata
  #' @param id An ID string that matches the corresponding server functions: 
  #'   \code{\link{metaRawUI}} and \code{\link{metaRawServ}}. 
  #' @keywords internal
  #' @import shiny
  #' @details This module is used in the \code{\link{metadata_gui}} function 
  #'   and in the FishSET app. It outputs raw metadata to the main panel.
  
  ns <- NS(id)
  
  uiOutput(ns("rawOut"))
}


metaRawServ <- function(id, meta) {
  #' Server for handling raw metadata
  #' @param id An ID string that matches the corresponding server functions: 
  #'   \code{\link{metaRawUI}} and \code{\link{metaRawOut}}. 
  #' @param meta Reactive values object to store metadata in. 
  #' @keywords internal
  #' @import shiny
  #' @importFrom rlang is_bare_list
  #' @details This module is used in the \code{\link{metadata_gui}} function 
  #'   and in the FishSET app. It imports and saves raw metadata.
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # load raw ----
    
    # raw file params
    output$readerUI <- renderUI({
      
      if (!is.null(input$raw_file$datapath)) {
        
        ext <- file_ext(input$raw_file$datapath)
        meta$ext <- ext
        
        tagList(
          
          checkboxInput(ns("parse"), "Parse from datafile?", value = FALSE),
          
          if (ext %in% c("csv", "tsv")) {
            
            tagList(
              numericInput(ns("nrows"), "Max # of rows (-1 = all)", min = -1, 
                           value = -1),
              checkboxInput(ns("header"), "First line contains column names?", 
                            value = TRUE),
              
              conditionalPanel("input.parse", ns = ns,
                  
                  checkboxInput(ns("is_list"), "List format", value = FALSE),
                  textInput(ns("comment_char"), "Comment character", value = "#")
              )
            )
            
          } else if (ext == "txt") {
            
            tagList(
              numericInput(ns("nlines"), "Max # of lines (-1 = all)", min = -1, 
                           value = -1),
              checkboxInput(ns("skipNul"), "Skip Nulls", value = FALSE),
              
              conditionalPanel("input.parse", ns = ns,
                               
                               checkboxInput(ns("txt_list"), "List format", 
                                             value = FALSE),
                               textInput(ns("txt_com"), "Comment character", 
                                         value = "/*")
              )
            )
            
          } else if (ext %in% c("xls", "xlsx")) {
            
            tagList(
              numericInput(ns("sheet"), "Sheet", value = 1, min = 1),
              textInput(ns("range"), "Cell range", value = NULL, 
                        placeholder = "e.g. A1:C3"),
              numericInput(ns("skip"), "Skip rows", value = 0, min = 0)
            )
            
          } else if (ext == "json") {
            
            tagList(
              checkboxInput(ns("simplify"), "Simplify", value = FALSE),
              checkboxInput(ns("flatten"), "Flatten", value = FALSE)
            )
          }
        )
      }  
      
    })
    
    
    # load raw
    observeEvent(input$load_raw, {
      
      req(input$raw_file)
      
      file <- input$raw_file$datapath
      read_fun <- ifelse(input$parse, parse_meta, read_dat)
      q_test <- quietly_test(read_fun)
      
      toNull <- function(x) if (is_value_empty(x)) NULL else x
      
      inputs <- function() {
        
        ext <- switch(meta$ext, csv = "csv", tsv = "tsv", json = "json", 
                      xls = "xls", "xlsx" = "xls", meta$ext)
        
        if (input$parse) {
          
          switch(ext,
                 txt = list(file, nrows = input$nlines, skipNul = input$skipNul,
                            is_list = input$txt_list, comment = input$txt_com),
                 csv = list(file, nrows = input$nrows, header = input$header, 
                            is_list = input$is_list, comment = input$comment_char),
                 xls = list(file, sheet = input$sheet, range = toNull(input$range), 
                            skip = input$skip),
                 json = list(file, simplifyVector = input$simplify, 
                             flatten = input$flatten),
                 list(file))
          
        } else {
          
          switch(ext,
                 txt = list(file, nrows = input$nlines, skipNul = input$skipNul),
                 csv = list(file, nrows = input$nrows, header = input$header),
                 tsv = list(file, nrows = input$nrows, header = input$header, 
                            data.type = "delim"),
                 xls = list(file, sheet = input$sheet, range = toNull(input$range), 
                            skip = input$skip),
                 json = list(file, simplifyVector = input$simplify, 
                             flatten = input$flatten),
                 list(file))
        }
      }
        
      meta_out <- do.call(q_test, inputs())
    
      if (is.null(meta_out)) {
        
        showNotification("Unable to load raw meta. Check reader parameters.", 
                         type = "warning")
        
      } else {
        
        meta$raw <- meta_out
        
        if (rlang::is_bare_list(meta_out)) {
          
          meta$raw_html <- list_to_html(meta_out)
          
        } else {
          
          meta$raw_html <- to_html_table(meta_out)
        }
      }
     
    })
    
    # raw output ----
    output$rawOut <- renderUI({
      
      if (!is.null(meta$raw_html)) {
        
        tagList(
          h4(strong("Raw Metadata")),
          meta$raw_html)
      } 
    })
    
    # clear raw
    observeEvent(input$clear_raw, {
      
      meta$raw <- NULL
      meta$raw_html <- NULL
    })
    
    # save raw ----
    observeEvent(input$save, {
      
      if (!is.null(meta$raw)) {
        
        q_test <- quietly_test(meta_log_call)
        raw <- q_test(project = input$project_name, meta = meta$raw, dataset = NULL,
                      tab_name = input$project_tab, tab_type = "main", 
                      overwrite = input$overwrite, raw = TRUE)
        
        if (raw) showNotification("Raw metadata saved.")
        else showNotification("Raw metadata was not saved.")
        
      } 
    })
  })
}


metaDeleteUI <- function(id) {
  #' UI for deleting metadata
  #' @param id An ID string that matches the corresponding server function: 
  #'   \code{\link{metaDeleteServ}}. 
  #' @keywords internal
  #' @import shiny
  #' @details This module is used in the \code{\link{metadata_gui}} function 
  #'   and in the FishSET app. It allows users to delete metadata from the 
  #'   project folder. 
  #'   
  ns <- NS(id)
  
  actionButton(ns("del_meta"), "Delete meta",
               style = "color: #fff; background-color: red; border-color:#000000;")
}


metaDeleteServ <- function(id, cols, meta) {
  #' Server for deleting metadata
  #' @param id An ID string that matches the corresponding UI function: 
  #'   \code{\link{metaDeleteUI}}. 
  #' @param cols Reactive values object containing column names from a FishSET 
  #'   table.
  #' @param meta Reactive values object to store metadata in. 
  #' @keywords internal
  #' @import shiny
  #' @details This module is used in the \code{\link{metadata_gui}} function 
  #'   and in the FishSET app. It allows users to delete metadata from the 
  #'   project folder. 
  #'   
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    observeEvent(input$del_meta, {
      
      req(input$project_tab)
      
      showModal(
        modalDialog(title = paste0("Delete metadata for ", input$project_tab, "?"),
                    
                    actionButton(ns("confirm_meta_delete"), "Delete", 
                                 style = "color: #fff; background-color: #FF6347; border-color: #800000;"),
                    actionButton(ns("meta_del_cancel"), "Cancel", 
                                 style = "color: white; background-color: #0073e6;"),
                    
                    footer = tagList(modalButton("Close")),
                    easyClose = FALSE, size = "s"))
    })
    
    observeEvent(input$confirm_meta_delete, {
      
      q_test <- quietly_test(delete_meta)
      q_test(input$project_name, tab.name = input$project_tab)
      removeModal()
      
      showNotification("Metadata deleted.", type = "message")
      
      meta$meta <- NULL
      meta$raw_html <- NULL
      cols$ui <- NULL
      cols$nms <- NULL
      
    })
    
    observeEvent(input$meta_del_cancel, removeModal())
  })
}


metaCreateLoadServ <- function(id, cols, meta) {
  #' Load server for metadata gui (Create section)
  #' Contains actions needed to create new metadata.
  #' @param id An ID string. It is recommended to use an informative and unique 
  #' string such as \code{"meta_create"}.
  #' @param cols Reactive values object containing column names from a FishSET 
  #'   table.
  #' @param meta Reactive values object to store metadata in. 
  #' @keywords internal
  #' @import shiny
  #' @details This module is used in the \code{\link{metadata_gui}} function 
  #'   and in the FishSET app. It loads FishSET tables and creates metadata
  #'   fields (text boxes) that users can edit. 
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    observeEvent(input$load, {
      
      req(input$project_tab)
       
      cols$nms <- 
        names(
          col_desc(
            table_view(project = input$project_name,
                       table = input$project_tab)))
      
      cols$nms_fix <- make.names(cols$nms)
      
      cols$ui <- lapply(cols$nms, function(x) colDescUI(id, x))
      
    })
    
    output$meta_out <- renderUI({
      
      if (!is.null(cols$nms)) {
        
        tagList(
          
          aboutUI(id),
          
          h4(strong("Column Description")),
          
          p("Include data type, unit, and value (if categorical)"),
          
          metaColLayoutUI(id),
          
          contactUI(id),
        )
      }
    })
    
  })
}


metaEditLoadServ <- function(id, cols, meta) {
  #' Load server for metadata gui (Edit section)
  #' Contains actions needed to view, edit, and delete metadata entries. 
  #' @param id An ID string. It is recommended to use an informative and unique 
  #' string such as \code{"meta_edit"}.
  #' @param cols Reactive values object containing column names from a FishSET 
  #'   table.
  #' @param meta Reactive values object to store metadata in. 
  #' @keywords internal
  #' @import shiny
  #' @details This module is used in the \code{\link{metadata_gui}} function 
  #'   and in the FishSET app. It loads FishSET tables and creates metadata
  #'   fields (text boxes) that users can edit. 
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    observeEvent(input$load, {
      
      req(input$project_tab)

      # clear previous meta
      meta$raw_html <- NULL
      
      meta$meta <- pull_meta(input$project_name, tab.name = input$project_tab)
      
      # template ----
      if (!is.null(meta$meta)) {
        
        m <- meta$meta$meta
        
        updateTextAreaInput(session, "auth", value = m$author)
        updateTextAreaInput(session, "date_c", value = m$date_created)
        updateTextAreaInput(session, "date_m", value = m$date_modified)
        updateTextAreaInput(session, "ver", value = m$dversion)
        updateTextAreaInput(session, "confid", value = m$confidentiality)
        updateTextAreaInput(session, "about", value = m$description$about)
        updateTextAreaInput(session, "src", value = m$description$source)
        updateTextAreaInput(session, "coll_meth", value = m$description$collection_method)
        updateTextAreaInput(session, "intend", value = m$description$intended_use)
        updateTextAreaInput(session, "time", value = m$description$time)
        updateTextAreaInput(session, "geo", value = m$description$geo)
        updateTextAreaInput(session, "species", value = m$description$species)
        
        updateTextAreaInput(session, "person", value = m$contact_info$person)
        updateTextAreaInput(session, "org", value = m$contact_info$org)
        updateTextAreaInput(session, "address", value = m$contact_info$address)
        updateTextAreaInput(session, "phone", value = m$contact_info$phone)
        updateTextAreaInput(session, "email", value = m$contact_info$email)
        
        updateTextAreaInput(session, "license", value = m$license)
        updateTextAreaInput(session, "cite", value = m$citation)
        updateTextAreaInput(session, "other", value = m$other)
        
        cols$nms <- names(m$description$column_desc)
        cols$nms_fix <- make.names(cols$nms)
        
        cols$ui <- lapply(cols$nms, function(x) {
          
          colDescUI(id, x, value = m$description$column_desc[[x]])
        })
        # raw ----
        if (!is.null(meta$meta$raw)) {
          
          meta$meta$raw <- simplify_list(meta$meta$raw)
          
          if (rlang::is_bare_list(meta$meta$raw)) {
            
            meta$raw_html <- list_to_html(meta$meta$raw)
            
          } else {
            
            meta$raw_html <- to_html_table(meta$meta$raw) 
          }
        }
      }
    })
    
    output$meta_out <- renderUI({
      
      if (!is.null(cols$nms)) {
        
        tagList(
          
          aboutUI(id),
          
          h4(strong("Column Description")),
          
          p("Include data type, unit, and value (if categorical)"),
          
          metaColLayoutUI(id),
          
          contactUI(id),
        )
      }
    })
    
  })
}


empty_meta <- function(input, col_nms) {
  #' Helper function
  #' Determines if metadata fields are empty in \code{metadata_gui}. 
  #' @param input Shiny input. 
  #' @param col_nms Column names. 
  #' @importFrom stringi stri_isempty
  
  about_id <- c("auth", "date_c", "confid", "date_m", "ver", "about", "src", 
                "coll_meth", "intend")
  
  contact_id <- c("person", "org", "address", "phone", "email", "license", 
                  "cite", "other")
  
  check <- c(about_id, col_nms, contact_id)
  
  check_list <- lapply(check, function(x) input[[x]])
  
  all(stringi::stri_isempty(check_list))
}


metaSaveServ <- function(id, cols) {
  #' Save server for metadata gui
  #' Contains actions needed to save metadata to meta log. 
  #' @param id An ID string. It is recommended to use an informative and unique 
  #' string such as \code{"meta_create"} or \code{"meta_edit"}.
  #' @param cols Reactive values object containing column names from a FishSET 
  #'   table.
  #' @keywords internal
  #' @import shiny
  #' @details This module is used in the \code{\link{metadata_gui}} function 
  #'   and in the FishSET app. It saves new or edited metadata to the project
  #'   folder. 
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    observeEvent(input$save, {
      
      if (!is.null(cols$nms)) {
        
        if (empty_meta(input, cols$nms) == FALSE) {
          
          cols_desc <- lapply(cols$nms, function(x) input[[x]])
          names(cols_desc) <- cols$nms
          
          type <- table_type(input$project_tab)
          
          meta_out <- list(author = input$auth,
                           date_created = input$date_c,
                           date_modified = input$date_m,
                           version = input$ver,
                           confidentiality = input$confid,
                           description = list(about = input$about,
                                              source = input$src,
                                              collection_method = input$coll_meth,
                                              intended_use = input$intend,
                                              column_desc = cols_desc,
                                              time = input$time,
                                              geo = input$geo,
                                              species = input$species),
                           contact_info = list(person = input$person,
                                               org = input$org,
                                               address = input$address,
                                               phone = input$phone,
                                               email = input$email),
                           license = input$license,
                           citation = input$cite,
                           other = input$other)
          
          q_test <- quietly_test(meta_log_call)
          
          out <- 
            q_test(project = input$project_name, meta = meta_out, dataset = NULL,
                   tab_name = input$project_tab, tab_type = type, 
                   overwrite = input$overwrite, raw = FALSE)
          
          if (out) showNotification("Metadata saved.", type = "message")
          else showNotification("Metadata not saved.", type = "warning")
          
        } else {
          
          showNotification("All fields are empty. Metadata not saved", type = "warning")
        }
        
      } else {
        
        showNotification("Select a table to load.")
      }
      
    })
  })
}


metaCreateServ <- function(id, cols, meta) {
  #' Run metadata servers (Create section)
  #' @param id An ID string. It is recommended to use an informative and unique 
  #' string such as \code{"meta_create"}.
  #' @param cols Reactive values object containing column names from a FishSET 
  #'   table.
  #' @param meta Reactive values object to store metadata in. 
  #' @keywords internal
  #' @details This module is used in the \code{\link{metadata_gui}} function 
  #'   and in the FishSET app. It runs the metadata server modules needed to save,
  #'   edit, and delete metadata. 

  # project
  metaCreateProjServ(id)
  
  # load
  metaCreateLoadServ(id, cols, meta)
  
  # display columns 
  metaColLayoutServ(id, cols)
  
  # save
  metaSaveServ(id, cols)
  
  # raw
  metaRawServ(id, meta)
} 


metaEditServ <- function(id, cols, meta) {
  #' Run metadata servers (Edit section)
  #' @param id An ID string. It is recommended to use an informative and unique 
  #' string such as \code{"meta_edit"}.
  #' @param cols Reactive values object containing column names from a FishSET 
  #'   table.
  #' @param meta Reactive values object to store metadata in. 
  #' @keywords internal
  #' @details This module is used in the \code{\link{metadata_gui}} function 
  #'   and in the FishSET app. It runs the metadata server modules needed to save,
  #'   edit, and delete metadata. 
  
  # project
  metaEditProjServ(id)
  
  # load
  metaEditLoadServ(id, cols, meta)
  
  # display columns 
  metaColLayoutServ(id, cols)
  
  # save
  metaSaveServ(id, cols)
  
  # raw
  metaRawServ(id, meta)
  
  # delete meta
  metaDeleteServ(id, cols, meta)
} 

