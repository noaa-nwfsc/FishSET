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
        
        shinycssloaders::withSpinner(htmlOutput(ns("iframe"))),
        
        fluidRow(
          
          column(3,
                 
                 actionButton(ns("run"), "run", style = "color: #fff; background-color: #6da363; border-color: #800000;"),
                 actionButton(ns("save"), "save"),
                 
                 selectInput(ns("avd"), "area variable (data)",
                             choices = colnames(dat$dataset), selected = "NMFS_AREA"),
                 
                 selectInput(ns("avm"), "area variable (map)",
                             choices = colnames(spatdat$dataset))),
          
          column(3,
                 
                 selectizeInput(ns("num"), "numeric variables",
                                choices = colnames(dat$dataset), multiple = TRUE),
                 
                 selectizeInput(ns("temp"), "temporal variables",
                                choices = colnames(dat$dataset), multiple = TRUE),
                 
                 selectizeInput(ns("id_vars"), "id variables",
                                choices = colnames(dat$dataset), multiple = TRUE)),
          
          column(3,
                 
                 selectInput(ns("lon_start"), "starting longitude",
                             choices = colnames(dat$dataset)),
                 
                 selectInput(ns("lat_start"), "starting latiitude",
                             choices = colnames(dat$dataset)),
                 
                 selectInput(ns("lon_end"), "ending longitude",
                             choices = colnames(dat$dataset))),
          
          column(3,
                 
                 selectInput(ns("lat_end"), "ending latiitude",
                             choices = colnames(dat$dataset)))
        )
      )
    })
    
    url <- eventReactive(input$run, {
      
      map_viewer(dat$dataset, gridfile = spatdat$dataset, avm = input$avm,
                 avd = input$avd, num_vars = input$num,
                 temp_vars = input$temp, id_vars = input$id_vars,
                 lon_start = input$lon_start, lat_start =  input$lat_start,
                 lon_end = input$lon_end, lat_end = input$lat_end)
    })
    
    output$iframe <- renderUI({
      
      tags$iframe(src = url(),
                  style = "width: 100%; height: 80vh;",
                  frameborder = "0",
                  id = "map_iframe")
    })
  })
}