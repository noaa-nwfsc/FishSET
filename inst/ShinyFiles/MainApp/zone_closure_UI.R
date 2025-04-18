# zone closure module UI code - sidebar, map, and table

### sidebar zone closure UI
zone_closure_sidebarUI <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns('zoneplot'), "Plot zones",
                 class = "btn-primary"),
    textInput(ns('scenarioname'), 'Scenario Name', value=''),
    actionButton(ns('addClose'), 'Add closure',
                 class = "btn-primary")
  )
}

### map and selected points zone closure UI
zone_closure_mapUI <- function(id){
  ns <- NS(id)
  
  tagList(
    bslib::card(
      height = 650,
      full_screen = TRUE,
      shinycssloaders::withSpinner(
        leaflet::leafletOutput(ns("zmap"), height = 600), 
        type = 6)
    )
  )
}

### table and closure viewer boxes zone closure UI
zone_closure_tableUI <- function(id){
  ns <- NS(id)
  tagList(
     bslib::card(
       height = 350,
       full_screen = TRUE,
       DT::dataTableOutput(ns("mod_table"))
       ),

     bslib::layout_column_wrap(
       width = 1/2,
       bslib::card(
         height = 350,
         full_screen = FALSE,
         bslib::card_header(class = "d-flex justify-content-between",
                            "New Closure Scenarios",
                            actionButton(ns('saveClose'), 'Save Closure', width = "20%",
                                         class = "btn-success")),
         bslib::card_body(
              verbatimTextOutput(ns("closureVTO1")),

              )),
       bslib::card(
         height = 350,
         full_screen = FALSE,
         bslib::card_header(class = "d-flex justify-content-between",
                            "Saved Closure Scenarios",
                            actionButton(ns('editClose'), 'Edit closure', width = "20%",
                                         class = "btn-success")),
         bslib::card_body(
           verbatimTextOutput(ns("closureVTO2")))
       )
       )
     )
}
