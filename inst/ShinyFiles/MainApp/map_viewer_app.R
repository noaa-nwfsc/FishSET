# map viewer 

map_viewerUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    uiOutput(ns("var_select"))
  )
}  

map_viewer_serv <- function(id, dat, spatdat, project) {
  
  moduleServer(id, function(input, output, session) { 
    
    ns <- session$ns
  
    output$var_select <- renderUI({
      bslib::page_sidebar(
        sidebar = bslib::sidebar( width = 550,
          
          selectInput(ns("avd"), "Area ID variable (primary data)",
                      choices = colnames(dat$dataset), selected = "ZoneID"),
          
          selectInput(ns("avm"), "Area ID variable (spatial data)",
                      choices = colnames(spatdat$dataset)),
          
          selectizeInput(ns("num"), "Numeric variables",
                         choices = numeric_cols(dat$dataset), multiple = TRUE),
          
          selectizeInput(ns("temp"), "Temporal variable",
                         choices = date_cols(dat$dataset), multiple = TRUE),
          
          selectizeInput(ns("id_vars"), "Categorical variables to display for each zone",
                         choices = category_cols(dat$dataset), multiple = TRUE, 
                         options = list(placeholder = "Select or type value name",
                                        create = TRUE)),
          
          selectInput(ns("point_path"), "Show vessel points or path",
                      choices=c('Point', 'Path'), selected='Point'),
          
          selectInput(ns("lon_start"), label="Starting longitude or point",
                      choices = colnames(dat$dataset)[grep('lon', colnames(dat$dataset), ignore.case = TRUE)]),
          
          selectInput(ns("lat_start"), label="Starting latitude or point",
                      choices = colnames(dat$dataset)[grep('lat', colnames(dat$dataset), ignore.case = TRUE)]),
          
          conditionalPanel(condition="input.point_path=='Path'", ns=ns,
                           selectInput(ns("lon_end"), "Ending longitude",
                                       choices = c(colnames(dat$dataset[ ,grep('lon', colnames(dat$dataset), ignore.case = TRUE)]))),
                           
                           selectInput(ns("lat_end"), "Ending latitude", 
                                       choices = c(colnames(dat$dataset[ ,grep('lat', colnames(dat$dataset), ignore.case = TRUE)])))
                           
          ),
          
          actionButton(ns("run"), "Run", class = "btn-primary"),
          
          tags$br(),
          
          actionButton(ns("map_close"), "Close app", icon = icon("circle-xmark"),
                       width = "50%",                                                           
                       class = "btn-danger", 
                       onclick = "setTimeout(function(){window.close();},500);")
        ),
        
          shinycssloaders::withSpinner(
            htmlOutput(ns("iframe")),
            type = 6
          )
      )
      
      
    })
    
    observeEvent(input$map_close, {
      stopApp()
    })
    
    url <- eventReactive(input$run, {
      
      if(input$point_path=="Path"){
        return(map_viewer(dat$dataset, project = project(), spat = spatdat$dataset, 
                          avm = input$avm, avd = input$avd, num_vars = input$num,
                          temp_vars = input$temp, id_vars = input$id_vars,
                          lon_start = input$lon_start, lat_start =  input$lat_start,
                          lon_end = input$lon_end, lat_end = input$lat_end))
      } else {
        return(map_viewer(dat$dataset, project = project(), spat = spatdat$dataset, 
                          avm = input$avm, avd = input$avd, num_vars = input$num,
                          temp_vars = input$temp, id_vars = input$id_vars,
                          lon_start = input$lon_start, lat_start =input$lat_start,
                          lon_end = NULL, lat_end = NULL))
      }
    })
    
    output$iframe <- renderUI({
      
      tags$iframe(src = url(),
                  style = "width: 100%; height: 80vh;",
                  frameborder = "0",
                  id = "map_iframe",
                  scrolling="no")
    })
  })
}