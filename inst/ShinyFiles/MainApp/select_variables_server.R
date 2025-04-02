### select variables server
sel_variablesServer <- function(id, project, spatdat, values, portdat){
   moduleServer(
      id,
      function(input, output, session){
         
         ns <- session$ns
 
         output$select_variables <- renderUI({
               tagList(
                  bslib::layout_column_wrap( fill = TRUE,
                                             width = 1/2,
                                             bslib::card(fill = TRUE,fillable = TRUE,
                                                bslib::card_header(strong("Primary data variables"), class = "bg-info"),
                                                bslib::card_body( 
                                                   selectizeInput(ns("primary_zone_id"), "Select zone ID from primary data",
                                                               choices = colnames(values), multiple = FALSE),
                                                   selectizeInput(ns("primary_zone_lon"), "Select Longitude from primary data",
                                                                  choices = find_lon(values), multiple = FALSE, 
                                                                  options = list(create = TRUE)),
                                                   selectizeInput(ns("primary_zone_lat"), "Select Latitude from primary data",
                                                                  choices = find_lat(values), multiple = FALSE, 
                                                                  options = list(create = TRUE)),
                                                   selectizeInput(ns("primary_zone_date"), "Select date variable", 
                                                                  choices = date_cols(values), multiple = FALSE, 
                                                                  options = list(create = TRUE)),
                                                   
                                                      )
                                                   ),
                                             layout_column_wrap(
                                                width = 1,
                                                heights_equal = "row",
                                                bslib::card(fill = TRUE,fillable = TRUE,
                                                   bslib::card_header(strong('Spatial data variables'),  class = "bg-info"),
                                                   bslib::card_body( 
                                                   selectizeInput(ns('spat_zone_id'),
                                                               'Select column containing zone ID in spatial data table',
                                                                choices = names(as.data.frame(spatdat)), multiple = FALSE)
                                                   )),
                                             bslib::card(fill = TRUE, fillable = TRUE,
                                                bslib::card_header(checkboxInput(inputId = ns("portdat_exist"), label = strong("Did you load a port data set?"),
                                                                                 value = FALSE),  class = "bg-info"),
                                                bslib::card_body( 
                                                   conditionalPanel(condition = 'input.portdat_exist', ns=ns,
                                                                    selectizeInput(ns("port_name"), "Select variable from port table with port names",
                                                                                   choices = colnames(portdat), 
                                                                                   multiple = FALSE),
                                                                    
                                                                    selectizeInput(ns("port_lon"), "Select variable from port table with port longitude",
                                                                                   choices = colnames(portdat), 
                                                                                   multiple = FALSE),
                                                                    
                                                                    selectizeInput(ns("port_lat"), "Select variable from port table with port latitude",
                                                                                   choices = colnames(portdat), 
                                                                                   multiple = FALSE)
                                                   )
                                                )
                                               )
                                             )
                                           ),
                                               
                                                  
                  actionButton(inputId = ns("save_all_variables"), label = "Save variables")
               )
         })
            observeEvent(input$save_all_variables, {
               showNotification(paste("variables saved"), type = "message", duration = 60)
            })
    
            reactive({
               list(
                  pz_id = input$primary_zone_id,
                  pz_lon = input$primary_zone_lon,
                  pz_lat = input$primary_zone_lat,
                  pz_date = input$primary_zone_date,
                 # pt_id = input$primary_trip_id,
                  sz_id = input$spat_zone_id,
                  port_name = input$port_name,
                  port_lon = input$port_lon,
                  port_lat = input$port_lat
               )
            })
      }
   )
}