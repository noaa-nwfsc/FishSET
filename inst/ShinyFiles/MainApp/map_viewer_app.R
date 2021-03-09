# map viewer 

map_viewerUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    uiOutput(ns("var_select"))
  )
}  

map_viewer_serv <- function(id, dat, spatdat) {
  
  moduleServer(id, function(input, output, session) { 
    
    ns <- session$ns
    
    output$var_select <- renderUI({
      
      tagList(
        
       # shinycssloaders::withSpinner(
        htmlOutput(ns("iframe")),
        
        fluidRow(
          
          column(3,
                 
                 actionButton(ns("run"), "Run", style = "color: #fff; background-color: #6da363; border-color: #800000;"),
                 actionButton(ns("save"), "Save"),
                 
                 selectInput(ns("avd"), "Area variable (data)",
                             choices = colnames(dat$dataset), selected = "ZoneID"),
                 
                 selectInput(ns("avm"), "Area variable (map)",
                             choices = colnames(spatdat$dataset))),
          
          column(3,
                 
                 selectizeInput(ns("num"), "Numeric variables",
                                choices = numeric_cols(dat$dataset), multiple = TRUE),
                 
                 selectizeInput(ns("temp"), "Temporal variables",
                                choices = colnames(dat$dataset), multiple = TRUE),
                 
                 selectizeInput(ns("id_vars"), "ID variables",
                                choices = colnames(dat$dataset), multiple = TRUE)),
          
          column(3,
                 selectInput(ns("point_path"), "Show vessel points or path",
                             choices=c('Point', 'Path'), selected='Path'),
                                  selectInput(ns("lon_start"), label="Starting longitude or point",
                             choices = colnames(dat$dataset[ ,grep('lon', colnames(dat$dataset), ignore.case = TRUE)])),
                                  selectInput(ns("lat_start"), label="Starting latitude or point",
                             choices = colnames(dat$dataset[ ,grep('lat', colnames(dat$dataset), ignore.case = TRUE)]))
                     ),
                
            column(3,     
                   conditionalPanel(condition="input.point_path=='Path'", ns=ns,
                           selectInput(ns("lon_end"), "Ending longitude",
                             choices = c(colnames(dat$dataset[ ,grep('lon', colnames(dat$dataset), ignore.case = TRUE)]))),
                 
                           selectInput(ns("lat_end"), "Ending latitude", 
                             choices = c(colnames(dat$dataset[ ,grep('lat', colnames(dat$dataset), ignore.case = TRUE)])))
         
                   )
               )
        
      )
      )
    })
    
    
    
    
    url <- eventReactive(input$run, {
      if(input$point_path=="Path"){
      return(map_viewer(dat$dataset, gridfile = spatdat$dataset, avm = input$avm,
                 avd = input$avd, num_vars = input$num,
                 temp_vars = input$temp, id_vars = input$id_vars,
                 lon_start = input$lon_start, lat_start =  input$lat_start,
                 lon_end = input$lon_end, lat_end = input$lat_end))
      } else {
        return(map_viewer(dat$dataset, gridfile = spatdat$dataset, avm = input$avm,
                          avd = input$avd, num_vars = input$num,
                          temp_vars = input$temp, id_vars = input$id_vars,
                          lon_start = input$lon_start, lat_start =input$lat_start,
                          lon_end = NULL, lat_end = NULL))
      }
  })
    
    output$iframe <- renderUI({
      
      tags$iframe(src = url(),
                  style = "width: 100%; height: 80vh;",
                  frameborder = "0",
                  id = "map_iframe")
    })
  })
}