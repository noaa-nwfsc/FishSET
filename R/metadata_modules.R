# Modules used in metadata_gui and Main app

metaProjUI <- function(id) {
  #' Project UI for metadata gui
  #' @param id An ID string that matches the corresponding server function: 
  #'   \code{\link{metaProjServ}}. 
  #' @keywords internal
  #' @import shiny
  #' @details This module is used in the \code{\link{metadata_gui}} function 
  #'   and in the FishSET app. It creates the UIs for project name and available
  #'   project tables. 
  
  ns <- NS(id)
  tagList(
    uiOutput(ns("proj_select")), 
    uiOutput(ns("proj_tabs_list"))
  )
}


metaProjServ <- function(id) {
  #' Project server for metadata gui
  #' @param id An ID string that matches the corresponding UI function: 
  #'   \code{\link{metaProjUI}}. 
  #' @keywords internal
  #' @import shiny
  #' @details This module is used in the \code{\link{metadata_gui}} function 
  #'   and in the FishSET app. It creates the UIs for project name and available
  #'   project tables. 
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$proj_select <- renderUI({
      
      if (length(projects()) > 0) {
        
        selectInput(ns("project_name"), "Select project", choices = projects())
        
      } else {
        
        p("No projects found in FishSET Database. Create a project by uploading a new file")
      }
    })
    
    
    output$proj_tabs_list <- renderUI({
      
      if (!is.null(input$project_name)) {
        
        if (id == "meta_create") {
          
          selectInput(ns("project_tab"), "Select table", 
                      choices = project_tables(input$project_name))
          
        } else {
          
          selectInput(ns("project_tab"), "select table", 
                      choices = suppressWarnings(meta_tables(input$project_name)))
        }
      }
    })
  })
}


metaLoadSaveBttnUI <- function(id) {
  #' Load and Save buttons for metadata gui
  #' @param id An ID string that matches the corresponding server functions: 
  #'   \code{\link{metaLoadServ}} and \code{\link{metaSaveServ}}. 
  #' @keywords internal
  #' @import shiny
  #' @details This module is used in the \code{\link{metadata_gui}} function 
  #'   and in the FishSET app. It creates the buttons needed to load and save 
  #'   metadata. 
  
  m_load <- switch(id, "meta_create" = "Load data", "meta_edit" = "Load meta")
  m_save <- switch(id, "meta_create" = "Create meta", "meta_edit" = "Save meta")
  
  ns <- NS(id) 
  
  tagList(
    actionButton(ns("load"), m_load, 
                 style = "color: white; background-color: blue;"),
    
    actionButton(ns("save"), m_save,
                 style = "color: white; background-color: blue;"),
  )
}


aboutUI <- function(id) {
  #' UIs for entering general metadata 
  #' @param id An ID string that matches the corresponding server functions: 
  #'   \code{\link{metaLoadServ}} and \code{\link{metaSaveServ}}. 
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
             textAreaInput(ns("src"), "Source(s)", width = "100%", resize = "vertical")),
      
      column(6,
             textAreaInput(ns("coll_meth"), "Collection method", width = "100%", resize = "vertical"),
             textAreaInput(ns("intend"), "Intended use", width = "100%", resize = "vertical"))
    )
  )
}


colDescUI <- function(id, nm, value = "") {
  #' Create a text box for each column in data
  #' @param id An ID string that matches the corresponding server functions: 
  #'   \code{\link{metaLoadServ}} and \code{\link{metaSaveServ}}. 
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
  #'   \code{\link{metaLoadServ}} and \code{\link{metaSaveServ}}. 
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
    
    output$col_desc <- renderUI({
      
      col_nms <- switch(id, 
                        "meta_create" = cols$create_nms, 
                        "meta_edit" = cols$edit_nms)
      
      col_ui <- switch(id, 
                       "meta_create" = cols$create_ui, 
                       "meta_edit" = cols$edit_ui)
      
      if (!is.null(col_nms)) {
        
        nc <- length(col_nms)
        
        cutoff <- ifelse(nc %% 2 == 0, nc/2, median(seq(nc)))
        
        col1 <- 1:cutoff
        col2 <- (cutoff + 1):nc
        
        fluidRow(
          
          column(6, col_ui[col1]),
          column(6, col_ui[col2]))
      }
    })
    
  })
}


metaOut <- function(id) {
  #' Output UI for metadata
  #' @param id An ID string that matches the corresponding server functions: 
  #'   \code{\link{metaLoadServ}} and \code{\link{metaSaveServ}}. 
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
    
    actionButton(ns("save_raw"), "Save raw meta",
                 style = "color: white; background-color: blue;"),
    
    actionButton(ns("clear_raw"), "Clear raw meta",
                 style = "color: #fff; background-color: red; border-color:#000000;"),
    
    fileInput(ns("raw_file"), "Download metadata file"),
    
    textAreaInput(ns("read_par"), "Reader parameters"),
    
    fluidRow(actionButton(ns("par_add"), "Add parameter"),
             actionButton(ns("par_clear"), "Clear parameters")),
    
    uiOutput(ns("par_list")),
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
  #' @details This module is used in the \code{\link{metadata_gui}} function 
  #'   and in the FishSET app. It imports and saves raw metdata.
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # load raw ----
    
    # raw file params
    observeEvent(input$par_add, {
      
      meta$par <- c(meta$par, input$read_par)
    })
    
    
    observeEvent(input$par_clear, { meta$par <- NULL })
    
    
    output$par_caption <- renderText(meta$par)
    
    
    output$par_list <- renderUI({
      
      div(class = "well well-sm", style = "background-color: white",
          textOutput("par_caption"))
    })
    
    # load raw
    observeEvent(input$load_raw, {
      
      req(input$raw_file)
      
      if(!is.null(meta$par)) {
        
        meta_out <- do.call("parse_meta", c(list(file = input$raw_file$datapath),
                                            eval(parse(text=paste0("list(", meta$par, ")")))))
        
      } else {
        
        meta_out <- parse_meta(file = input$raw_file$datapath)
      }
      
      meta$create_raw <- meta_out
      meta$create_raw_html <- list_to_html(meta_out)
    })
    
    # raw output ----
    output$rawOut <- renderUI({
      
      raw_html <- switch(id, 
                         "meta_create" = meta$create_raw_html, 
                         "meta_edit" = meta$edit_raw_html)
      
      if (!is.null(raw_html)) {
        
        tagList(
          h4(strong("Raw Metadata")),
          raw_html)
      } 
    })
    
    # clear raw
    
    observeEvent(input$clear_raw, {
      
      meta$create_raw <- NULL
      meta$create_raw_html <- NULL
    })
    
    # save raw ----
    observeEvent(input$save_raw, {
      
      if (!is.null(meta$create_raw)) {
        
        q_test <- quietly_test(meta_log_call)
        q_test(project = input$project_name, meta = meta$create_raw, dataset = NULL,
               tab_name = input$project_tab, tab_type = "main", 
               overwrite = TRUE, raw = TRUE)
        
      } else {
        
        showNotification("Upload a meta file to save.")
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
      
      meta$edit_meta <- NULL
      meta$edit_raw_html <- NULL
      cols$edit_ui <- NULL
      cols$edit_nms <- NULL
      
    })
    
    observeEvent(input$meta_del_cancel, removeModal())
  })
}


metaLoadServ <- function(id, cols, meta) {
  #' Load server for metadata gui
  #' @param id An ID string. The two options are \code{"meta_create"} for creating 
  #'   new metadata and \code{"meta_edit"} for editing or deleting existing 
  #'   metadata. 
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
      
      if (id == "meta_create") {
        
        cols$create_nms <- 
          names(
            col_desc(
              table_view(project = input$project_name,
                         table = input$project_tab)))
        
        cols$create_nms_fix <- make.names(cols$create_nms)
        
        cols$create_ui <- lapply(cols$create_nms, function(x) colDescUI("meta_create", x))
        
      } else { # meta_edit section
        
        # clear previous meta
        meta$edit_raw_html <- NULL
        
        meta$edit_meta <- pull_meta(input$project_name, tab.name = input$project_tab)
        
        if (!is.null(meta$edit_meta)) {
          
          m <- meta$edit_meta$meta
          
          updateTextAreaInput(session, "auth", value = m$author)
          updateTextAreaInput(session, "date_c", value = m$date_created)
          updateTextAreaInput(session, "date_m", value = m$date_modified)
          updateTextAreaInput(session, "ver", value = m$dversion)
          updateTextAreaInput(session, "confid", value = m$confidentiality)
          updateTextAreaInput(session, "about", value = m$description$about)
          updateTextAreaInput(session, "src", value = m$description$source)
          updateTextAreaInput(session, "coll_meth", value = m$description$collection_method)
          updateTextAreaInput(session, "intend", value = m$description$intended_use)
          
          updateTextAreaInput(session, "person", value = m$contact_info$person)
          updateTextAreaInput(session, "org", value = m$contact_info$org)
          updateTextAreaInput(session, "address", value = m$contact_info$address)
          updateTextAreaInput(session, "phone", value = m$contact_info$phone)
          updateTextAreaInput(session, "email", value = m$contact_info$email)
          
          updateTextAreaInput(session, "license", value = m$license)
          updateTextAreaInput(session, "cite", value = m$citation)
          updateTextAreaInput(session, "other", value = m$other)
          
          cols$edit_nms <- names(m$description$column_desc)
          cols$edit_nms_fix <- make.names(cols$edit_nms)
          
          cols$edit_ui <- lapply(cols$edit_nms, function(x) {
            
            colDescUI("meta_edit", x, value = m$description$column_desc[[x]])
          })
          
          if (!is.null(meta$edit_meta$raw)) {
            
            meta$edit_raw_html <- list_to_html(meta$edit_meta$raw)
          }
          
        }
      }
    })
    
    output$meta_out <- renderUI({
      
      col_nms <- switch(id, 
                        "meta_create" = cols$create_nms, 
                        "meta_edit" = cols$edit_nms)
      
      if (!is.null(col_nms)) {
        
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


metaSaveServ <- function(id, cols) {
  #' Save server for metadata gui
  #' @param id An ID string. The two options are \code{"meta_create"} for creating 
  #'   new metadata and \code{"meta_edit"} for editing or deleting existing 
  #'   metadata. 
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
      
      col_nms <- switch(id, 
                        "meta_create" = cols$create_nms, 
                        "meta_edit" = cols$edit_nms)
      
      if (!is.null(col_nms)) {
        
        cols_desc <- lapply(col_nms, function(x) input[[x]])
        names(cols_desc) <- col_nms
        
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
                                            column_desc = cols_desc),
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
                 tab_name = input$project_tab, tab_type = type, overwrite = TRUE, 
                 raw = FALSE)
        
        if (out) showNotification("Metadata saved.", type = "message")
        else showNotification("Metadata not saved.", type = "warning")
        
      } else {
        
        showNotification("Load a meta file.")
      }
      
    })
  })
}


metaServ <- function(id, cols, meta) {
  #' Run metadata servers
  #' @param id An ID string. The two options are \code{"meta_create"} for creating 
  #'   new metadata and \code{"meta_edit"} for editing or deleting existing 
  #'   metadata.
  #' @param cols Reactive values object containing column names from a FishSET 
  #'   table.
  #' @param meta Reactive values object to store metadata in. 
  #' @keywords internal
  #' @details This module is used in the \code{\link{metadata_gui}} function 
  #'   and in the FishSET app. It runs the metadata server modules needed to save,
  #'   edit, and delete metadata. 

  # project
  metaProjServ(id)
  
  # load
  metaLoadServ(id, cols, meta)
  
  # display columns 
  metaColLayoutServ(id, cols)
  
  # save
  metaSaveServ(id, cols)
  
  # raw
  metaRawServ(id, meta)
  
  # delete meta
  metaDeleteServ(id, cols, meta)
} 



