#' Define zone closure scenarios
#' 
#' @param project Required, name of project.
#' @param gridfile Required, data file or character. 
#'   \code{gridfile} is a spatial data file containing information on fishery management or regulatory zones boundaries.
#'   Shape, json, geojson, and csv formats are supported. geojson is the preferred format. json files must be converted
#'   into geoson. This is done automatically when the file is loaded with \code{\link{read_dat}} with \code{is.map} set to true.
#'   \code{gridfile} cannot, at this time, be loaded from the FishSET database. \cr
#' @param cat Variable in \code{gridfile} that identifies the individual areas or zones.
#' @param secondgridfile Optional spatial data file containing information to aid in defining zone closures, such as location of windmills.
#' @param secondcat Optional, name of variable that identifies individual areas in \code{secondgridfile}
#' @param lon.grid Required for csv files. Variable or list from \code{gridfile} containing longitude data. Leave as NULL if \code{gridfile} is a shape or json file.
#' @param lat.grid Required for csv files. Variable or list from \code{gridfile} containing latitude data.  Leave as NULL if \code{gridfile} is a shape or json file.
#' @importFrom sf st_as_sf st_shift_longitude
#' @importFrom yaml write_yaml
#' @import leaflet
#' @import shiny
#' @import dplyr
#' @import sf
#' @details Define zone closure scenarios. Function opens an interactive map. Define zone closures by clicking on one or more zones and clicking the 'Close zones'
#'   button. To define another closure scenario, unclick zones and then click the desired zones. Press the 'Save closures' button to save choices.
#'   The saved choices are called in the policy scenario function.
#' @export
#' @return Returns a yaml file to the project output folder.


zone_closure <- function(project, gridfile, cat, secondgridfile=NULL, secondcat=NULL, lon.grid=NULL, lat.grid=NULL) {
  
  x <- 0
  
  gridfile <- gridcheck(spatialdat = gridfile, catdat=cat, londat=lon.grid, latdat=lat.grid)
  
  if(!is.null(secondgridfile)){
    secondgridfile <- gridcheck(spatialdat = secondgridfile, catdat=secondcat, londat=NULL, latdat=NULL)
    secondgridfile[[secondcat]] <- as.factor(secondgridfile[[secondcat]])
    qpal <- colorFactor(topo.colors(length(unique(secondgridfile[[secondcat]]))), secondgridfile[[secondcat]], 
                        unique(secondgridfile[[secondcat]]))
  }
  
  if(cat %in% names(gridfile)){
    
    gridfile$secondLocationID <- paste("Zone_", as.character(gridfile[[cat]]), sep="")
    names(gridfile)[which(names(gridfile) == cat)] <- 'zone'
    gridfile$zone <- as.character(gridfile$zone)
  } else if('zone' %in% names(gridfile)){
    gridfile$zone <- as.character(gridfile$zone)
    gridfile$secondLocationID <- paste("Zone_", as.character(gridfile$zone), sep="")
  } else {
    cat('Zone identifier not found.')
    x<-1
  }
  

  if(x == 0){
    shinyApp(
      ui = fluidPage(
        
        leafletOutput("map"),
        "Click on one or more zones to select closed zones.",
        "\nPress the 'Add closure' button to record choices.",
        "Repeat to add another closure.",
        "When done, press the 'Save closures' button.",
        selectizeInput(inputId = "clicked_locations",
                       label = "Clicked",
                       choices = gridfile$secondLocationID,
                       selected = NULL,
                       multiple = TRUE),
        textInput('scenarioname', 'Scenario Name', value=''),
        uiOutput("tac"),
        actionButton('addClose', 'Add closure'),
        verbatimTextOutput("closure"),
        actionButton('saveClose', 'Save Closure')
      ),
      
      server <- function(input, output, session){
        
        session$onSessionEnded(function() {
          stopApp()
        })
        
        #create empty vector to hold all click ids
        clicked_ids <- reactiveValues(ids = vector())
        closures <-  reactiveValues()
        
        #output$tac <- renderUI({
        #  numAssets <- length(input$clicked_locations)  
        #  if(numAssets != 0) { 
        #  lapply(1:numAssets, function(i) {
        #    div(style="display: inline-block;vertical-align:top; width: 200px;", 
        #        numericInput(paste0('tac', i), label = paste(input$clicked_locations, "TAC allowed (%)"), value=0, min=0, max=100))
        #  }) #end of lapply
        #  } else {
        #    numericInput('tac', 'TAC allowed (%)', value=100, min=0, max=100)
        #  }
        #}) # end of renderUI
        
        output$tac <- renderUI({
          req(input$clicked_locations)
          numInits <- length(input$clicked_locations)  
          i = 1:numInits
          numwidth <- rep((1/numInits*100), numInits)
          numwidth <- paste("'", as.character(numwidth),"%'", collapse=", ", sep="")
          UI <- paste0("splitLayout(",
                       "cellWidths = c(",
                       numwidth,
                       ")",
                       ",",
                       paste0("numericInput(",
                              "'int", i, "', ",
                              paste("'% TAC allowed", input$clicked_locations[i],"'"),
                              ",",
                              "value=0,min=0,max=100",
                              #"width='",1/numInits*100,"%'",#50px'",
                              ")",
                              collapse = ", "),
                       ")")
          eval(parse(text = UI))
        })
        
        
        #initial map output
        if(!is.null(secondgridfile)){   
          output$map <- renderLeaflet({
            
            leaflet() %>%
              addTiles() %>%
              addPolygons(data = gridfile,
                          fillColor = "white",
                          fillOpacity = 0.5,
                          color = "black",
                          stroke = TRUE,
                          weight = 1,
                          layerId = ~secondLocationID,
                          group = "regions",
                          label = ~secondLocationID) %>%
              addPolygons(data = secondgridfile,
                          fill = FALSE,
                          weight=2,
                          color = ~qpal(secondgridfile[[secondcat]]))
          }) #END RENDER LEAFLET
        } else {
          output$map <- renderLeaflet({
            
            leaflet() %>%
              addTiles() %>%
              addPolygons(data = gridfile,
                          fillColor = "white",
                          fillOpacity = 0.5,
                          color = "black",
                          stroke = TRUE,
                          weight = 1,
                          layerId = ~secondLocationID,
                          group = "regions",
                          label = ~secondLocationID)
          })
        }
        
        
        observeEvent(input$map_shape_click, {
          
          #create object for clicked polygon
          click <- input$map_shape_click
          
          #define leaflet proxy for second regional level map
          proxy <- leafletProxy("map")
          
          #append all click ids in empty vector
          clicked_ids$ids <- c(clicked_ids$ids, click$id) # name when clicked, id when unclicked
          
          #shapefile with all clicked polygons - original shapefile subsetted by all admin names from the click list
          clicked_polys <- gridfile %>% filter(secondLocationID %in% clicked_ids$ids)
          
          #if the current click ID [from GID_1] exists in the clicked polygon (if it has been clicked twice)
          if(click$id %in% clicked_polys$zone){
            
            #define vector that subsets NAME that matches GID_1 click ID - needs to be different to above
            name_match <- clicked_polys$secondLocationID[clicked_polys$zone == click$id]
            
            #remove the current click$id AND its name match from the clicked_polys shapefile
            clicked_ids$ids <- clicked_ids$ids[!clicked_ids$ids %in% click$id]
            clicked_ids$ids <- clicked_ids$ids[!clicked_ids$ids %in% name_match]
            
            
            # update
            updateSelectizeInput(session,
                                 inputId = "clicked_locations",
                                 label = "",
                                 choices = gridfile$secondLocationID,
                                 selected = clicked_ids$ids)
            
            #remove that highlighted polygon from the map
            proxy %>% removeShape(layerId = click$id)
            
          } else {
            
            #map highlighted polygons
            proxy %>% addPolygons(data = clicked_polys,
                                  fillColor = "red",
                                  fillOpacity = 0.5,
                                  weight = 1,
                                  color = "black",
                                  stroke = TRUE,
                                  layerId = clicked_polys$zone)
            
            # update
            updateSelectizeInput(session,
                                 inputId = "clicked_locations",
                                 label = "",
                                 choices = gridfile$secondLocationID,
                                 selected = clicked_ids$ids)
            
          } #END CONDITIONAL
        }) #END OBSERVE EVENT
        
        #access variable int outside of observer
        tac_name <- reactive({
          paste(lapply(1:length(input$clicked_locations), function(i) {
            inputName <- paste("int", i, sep = "")
            input[[inputName]]
          }))
        })
        
        
        observe({
          if(input$addClose > 0){
            closures$dList <- c(isolate(closures$dList), 
                                isolate(list(c(list(scenario=input$scenarioname), list(zone=input$clicked_locations), list(tac=tac_name())))))
          }
        })
        
        
        
        output$closure <- renderPrint({
          print(closures$dList)                # rv$value
        })
        
        observeEvent(input$saveClose, {
          
          yaml::write_yaml(closures$dList, paste0(locoutput(project), project, "_closures_", Sys.Date(),".yaml"))
          
        }) #END SAVE CLOSURE
        
      }) #END SHINYAPP
    
  }
}

