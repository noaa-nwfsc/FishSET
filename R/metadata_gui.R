#' Import, create, and edit metadata 
#' 
#' \code{metadata_gui} allows users to import metadata from various file types,
#' create and save new metadata, and edit metadata in a shiny application. 
#' Metadata is stored in the user's project folder. 
#' @export
#' @import shiny
#' @seealso 
#' @details The app has two tabs: "Create" and "Edit". The Create tab allows users
#'   to create new metadata for a selected FishSET table. When a table is loaded,
#'   the app creates several text boxes that the user can fill. There are 
#'   four metadata sections: About, Column Description, Contact Info, and Other. 
#'   
#'   \describe{
#'     \item{\strong{About}}{
#'       \itemize{
#'       \item \strong{Author} The author of the data.
#'       \item \strong{Date created} The date data was created.
#'       \item \strong{Date modified} The last data the data was modified. 
#'       \item \strong{Version} The current version of the data.
#'       \item \strong{Confidentiality} Whether the data contains confidential 
#'         information.
#'       }
#'     } 
#'     \item{\strong{Column Description}}{A text box for each column in the data.
#'       Include the data type, unit, and values (if categorical)}.
#'     \item{\strong{Contact Info}}{
#'       \itemize{
#'       \item \strong{Person} The primary contact.
#'       \item \strong{Organization} The primary contact's organization.
#'       \item \strong{Address} The primary contact's and/or organization's address.
#'       \item \strong{Phone} The primary contact's work phone number. 
#'       \item \strong{Email} The primary contact's work email. 
#'       }
#'     } 
#'     \item{\strong{Other}}{
#'       \itemize{
#'       \item \strong{License} License for data. 
#'       \item \strong{Citation} Citation for data.
#'       \item \strong{Other} Other relevant information.    
#'       }
#'     } 
#'   }
#'   Users can also import a metadata file from the Create tab, for example,  
#'   an XML, CSV, or JSON file. This gets saved as "raw" metadata and is separate 
#'   from the user-created metadata. To see a comprehensive list of accepted file types,
#'   see \code{\link{parse_meta}} and \code{\link{read_dat}}. To extract metadata
#'   from a data file (i.e. the data and metadata are both in the same file, but 
#'   the metadata is not contained within the data itself), use the \code{Reader
#'   parameters} text box to selectively parse the file (see \code{\link{parse_meta}}
#'   for details). 
#'   
#'   The Edit tab allows users to view, edit, and/or delete metadata saved to 
#'   FishSET. 
#'   
#' @examples 
#' \dontrun{
#' metadata_gui()
#' }

metadata_gui <- function() {
  
  ui <- fluidPage(
    
    tabsetPanel(id = "tab",
                
                tabPanel("Create", value = "create_tab",
                         
                         sidebarLayout(
                           
                           sidebarPanel(width = 3, 
                                        # style = "position: fixed; width: 22%;",
                             
                             tags$button(
                               id = "close",
                               type = "button",
                               style="color: #fff; background-color: #FF6347; border-color: #800000;",
                               class = "btn action-button",
                               onclick = "setTimeout(function(){window.close();},500);",  # close browser
                               "Close app"),
                             
                             h4(strong("Create metadata")),
                             
                             p("Metadata can be created by loading a data table from FishSET DB and",
                               "typing into the text boxes in the main panel.",
                               "Import a metadata file by selecting \"Download",
                               "metadata file\" and clicking \"Load raw meta\".",
                               "See \"parse_meta\" in the Help Manual for instructions",
                               "on extracting metadata from a data file."),
                             
                             metaProjUI("meta_create"),
                             
                             metaCreateSaveUI("meta_create"),
                             
                             tags$hr(style = "border-top: 3px solid #bbb;"),
                             
                             metaRawUI("meta_create"),
                           ),
                           
                           
                             mainPanel(width = 9,
                                       
                                     metaOut("meta_create"),
                                     metaRawOut("meta_create") 
                           ),
                         )
                ),
                
                tabPanel("Edit", value = "edit_tab",
                         
                         sidebarLayout(
                           
                           sidebarPanel(width = 3,
                             
                             h4(strong("View, edit, and delete metadata")),
                             
                             p("To edit existing metadata, select a project and table", 
                               "and click \"Load meta\". Click \"Save meta\" after", 
                               "changes are added. To delete metadata, select a table",
                               "and click \"Delete meta\". Select \"Delete\" in the popup", 
                               " to confirm. "),
                             
                             metaProjUI("meta_edit"),
                             
                             metaEditSaveUI("meta_edit"),
                             
                             metaDeleteUI("meta_edit")
                           ),
                           
                           mainPanel(width = 9, 
                             
                             metaOut("meta_edit"),
                             metaRawOut("meta_edit")
                           )
                         )
                ) 
    ) 
  )
  
  server <- function(input, output, session) {
    
    # react vals ----
    create_meta <- reactiveValues(raw = NULL, raw_html = NULL, par = NULL, ext = NULL)
    edit_meta <- reactiveValues(raw = NULL, raw_html = NULL, meta = NULL)
    
    create_cols <- reactiveValues(nms = NULL, nms_fix = NULL, ui = NULL)
    edit_cols <- reactiveValues(nms = NULL, nms_fix = NULL, ui = NULL)

    # servers ----
    metaCreateServ("meta_create", create_cols, create_meta)
    metaEditServ("meta_edit", edit_cols, edit_meta)
    
    # close app ----
    observeEvent(input$close, {
      
      showModal(
        modalDialog(title = "Close app?",
                    
                    actionButton("confirm_meta_close", "Close", 
                                 style = "color: #fff; background-color: #FF6347; border-color: #800000;"),
                    actionButton("meta_close_cancel", "Cancel", 
                                 style = "color: white; background-color: #0073e6;"),
                    
                    footer = tagList(modalButton("Close")),
                    easyClose = FALSE, size = "s"))
    })
    
    observeEvent(input$confirm_meta_close, stopApp())
    
    observeEvent(input$meta_close_cancel, removeModal())
  }
  
  shinyApp(ui, server)
}
