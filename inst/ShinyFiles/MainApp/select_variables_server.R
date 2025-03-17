### select variables server
sel_variablesServer <- function(id, project, spatdat, values){
   moduleServer(
      id,
      function(input, output, session){
         
         ns <- session$ns
         
         
         output$select_variables <- renderUI({

            
               tagList(
                  selectInput(ns("primary_zone_id"), "Select zone ID from primary data",
                              choices = colnames(values), multiple = FALSE),
                  selectizeInput(ns("primary_zone_lon"), "Select Longitude from primary data",
                                 choices = find_lon(values), multiple = FALSE, 
                                 options = list(create = TRUE)),
                  selectizeInput(ns("primary_zone_lat"), "Select Latitude from primary data",
                                 choices = find_lat(values), multiple = FALSE, 
                                 options = list(create = TRUE)),
                  selectizeInput("primary_zone_date", "Select date variable", 
                                 choices = colnames(values), multiple = FALSE, 
                                 options = list(create = TRUE))
               )
         })
      }
   )
}