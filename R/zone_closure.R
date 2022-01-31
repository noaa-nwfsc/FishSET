#' Define zone closure scenarios
#' 
#' @param project Required, name of project.
#' @param gridfile Required, data file or character. 
#'   \code{gridfile} is a spatial data file containing information on fishery 
#'   management or regulatory zones boundaries. Shape, json, geojson, and csv 
#'   formats are supported. geojson is the preferred format. json files must be 
#'   converted into geoson. This is done automatically when the file is loaded 
#'   with \code{\link{read_dat}} with \code{is.map} set to true. \code{gridfile} 
#'   cannot, at this time, be loaded from the FishSET database. \cr
#' @param cat Variable in \code{gridfile} that identifies the individual areas or zones.
#' @param secondgridfile Optional spatial data file containing information to 
#'   aid in defining zone closures, such as location of windmills.
#' @param secondcat Optional, name of variable that identifies individual areas 
#'   in \code{secondgridfile}
#' @param lon.grid Required for csv files. Variable or list from \code{gridfile} 
#'   containing longitude data. Leave as NULL if \code{gridfile} is a shape or json file.
#' @param lat.grid Required for csv files. Variable or list from \code{gridfile} 
#'   containing latitude data.  Leave as NULL if \code{gridfile} is a shape or json file.
#' @param epsg EPSG number. Set the epsg to ensure that \code{gridfile} and 
#'   \code{secondgridfile} have the same projections. If epsg is not specified but is defined for 
#'   \code{gridfile}, then the \code{secondgridfile} epsg will be applied to \code{gridfile}. 
#'   See \url{http://spatialreference.org/} to help identify optimal epsg number.
#' @importFrom sf st_crs st_transform
#' @importFrom yaml write_yaml
#' @importFrom grDevices topo.colors
#' @import leaflet
#' @import shiny
#' @import dplyr
#' @details Define zone closure scenarios. Function opens an interactive map. 
#'   Define zone closures by clicking on one or more zones and clicking the 
#'   'Close zones' button. To define another closure scenario, unclick zones and 
#'   then click the desired zones. Press the 'Save closures' button to save choices.
#'   The saved choices are called in the policy scenario function.
#' @export
#' @return Returns a yaml file to the project output folder.

zone_closure <- function(project, gridfile, cat, secondgridfile = NULL, 
                         secondcat = NULL, lon.grid = NULL, lat.grid = NULL,
                         epsg = NULL) {
 
  pass <- TRUE
  

  x <- 0
  secondLocationID <- NULL

  grid_nm <- deparse(substitute(gridfile)) # won't work in main app
  close_nm <- deparse(substitute(secondgridfile))

  
  # leaflet requires WGS84
  gridfile <- sf::st_transform(gridfile, "+proj=longlat +datum=WGS84")

  if (!is.null(secondgridfile)) {

    secondgridfile <- sf::st_transform(secondgridfile, 
                                       "+proj=longlat +datum=WGS84")
  }
 
  gridfile <- check_spatdat(gridfile, id = cat, lon = lon.grid, lat = lat.grid)
  
  if(!is.null(secondgridfile)){
    
    secondgridfile <- check_spatdat(secondgridfile, id = secondcat)
    secondgridfile[[secondcat]] <- as.factor(secondgridfile[[secondcat]])
    secondgridfile$closure_id <- paste0("Closure_", 1:nrow(secondgridfile))
    # 
    # qpal <- colorFactor(grDevices::topo.colors(length(unique(secondgridfile[[secondcat]]))), 
    #                     secondgridfile[[secondcat]],unique(secondgridfile[[secondcat]]))
    
  }
  
  if (!is.null(secondgridfile)) {
    
    if (sf::st_crs(gridfile) != sf::st_crs(secondgridfile)) {
      
      warning("Projection does not match. The detected projection in the",
              " spatial file will be used unless epsg is specified.")
    }
    
    if (!is.null(epsg)) {
      
      gridfile <- sf::st_transform(gridfile, epsg)
      secondgridfile <- sf::st_transform(secondgridfile, epsg)
      
    } else {
      
      crs1 <- sf::st_crs(gridfile)
      crs2 <- sf::st_crs(secondgridfile)
      
      if (is.na(crs1) & is.na(crs2)) {
        
        gridfile <- sf::st_transform(gridfile, "+proj=longlat +datum=WGS84")
        secondgridfile <- sf::st_transform(secondgridfile, 
                                           "+proj=longlat +datum=WGS84")
        
      } else if (is.na(crs1) & !is.na(crs2)) {
        
        gridfile <- sf::st_transform(gridfile, crs2)
        
      } else if (!is.na(crs1) & is.na(crs2)) {
        
        secondgridfile <- sf::st_transform(secondgridfile, crs1)
        
      } else if (!is.na(crs1) & !is.na(crs2)) {
        
        if (crs1 != crs2) {
          
          gridfile <- sf::st_transform(gridfile, "+proj=longlat +datum=WGS84")
          secondgridfile <- sf::st_transform(secondgridfile, 
                                             "+proj=longlat +datum=WGS84")
        }
      }
    }

  }
  
  
  if (cat %in% names(gridfile)) {
    
    gridfile$secondLocationID <- paste("Zone_", as.character(gridfile[[cat]]), sep="")
    names(gridfile)[which(names(gridfile) == cat)] <- 'zone'
    gridfile$zone <- as.character(gridfile$zone)
    
  } else if ('zone' %in% names(gridfile)) {
    
    gridfile$zone <- as.character(gridfile$zone)
    gridfile$secondLocationID <- paste("Zone_", as.character(gridfile$zone), sep="")
    
  } else {
    
    cat('Zone identifier not found.')
    pass <- FALSE
  }
  

  if (pass) {
    # UI ----
    shinyApp(
      ui = fluidPage(
        
        shinyjs::useShinyjs(),
        
        leafletOutput("map"),
        "Click on one or more zones to select closed zones.",
        "\nPress the 'Add closure' button to record choices.",
        "Repeat to add another closure.",
        "When done, press the 'Save closures' button.",
        
        fluidRow(
          column(2, selectInput("mode", "Select mode", 
                                choices = c("normal", "combine"))),
          column(4,  uiOutput("modeMsg"))
        ),
        
        selectizeInput(inputId = "clicked_locations",
                       label = "Clicked",
                       choices = NULL,
                       selected = NULL,
                       multiple = TRUE),
        textInput('scenarioname', 'Scenario Name', value=''),
        uiOutput("tac"),
        
        fluidRow(
          column(width = 2,
                   actionButton('addClose', 'Add closure',
                                style = "color: white; background-color: blue;")),
          column(width = 4,        
                 uiOutput("combineUI")),
          column(width = 2,
                 actionButton('editClose', 'Edit closure',
                              style = "color: white; background-color: blue;"))
        ),
        
        
        tags$br(), tags$br(),
        
        fluidRow(
          column(width = 5, 
                 h4(strong("New Closure Scenarios")), 
                 verbatimTextOutput("closureVTO1")),
          column(width = 5, offset = 1,
                 h4(strong("Saved Closure Scenarios")), 
                 verbatimTextOutput("closureVTO2")
                 )
          ),
        
        tags$head(tags$style("#closureVTO1{max-height: 400px; overflow-y: scroll;}")),
        tags$head(tags$style("#closureVTO2{max-height: 400px; overflow-y: scroll;}")),
          
        actionButton('saveClose', 'Save Closure',
                     style = "color: white; background-color: blue;"),
        
        tags$br(), tags$br(),
        
        # temp
        textInput("exprUp", label = "Enter an R expression",
                  value = "values$dataset"),
        actionButton("runUp", "Run", class = "btn-success"),
        div(style = "margin-top: 2em;",
            uiOutput('resultUp')
        ) 
      ),
      
      server <- function(input, output, session) {
        
        session$onSessionEnded(function() {
          stopApp()
        })
        
        # values ----
        dat <- reactiveValues(gridfile = gridfile,
                              secondgridfile = secondgridfile,
                              combined = NULL)
        
        grid_cache <- reactiveValues(grid_1 = NULL)
        grid_info <- reactiveValues(grid_1 = NULL)
        
        rv <- reactiveValues(combined = FALSE, combined_areas = NULL,
                             edit = NULL, saved = get_closure_scenario(project))
        
        #create empty vector to hold all click ids
        clicked_ids <- reactiveValues(ids = vector())
        closures <- reactiveValues()
        
        cache_unique_grid <- function(grid, grid.nm, closure.nm = NULL,  
                                      combined = NULL) {
          
          if (is.null(grid_cache$grid_1)) {
            
            grid_cache[["grid_1"]] <- grid
            grid_info[["grid_1"]] <- list(grid_name = grid.nm,
                                          closure_name = closure.nm,
                                          combined_areas = sort(combined))
            
          } else {
            
            match <- 
              vapply(reactiveValuesToList(grid_cache), function(x) {
                
                identical(x$secondLocationID, grid$secondLocationID)
              }, logical(1))
            
            if (any(match) == FALSE) {
              
              g_ind <- paste0("grid_", length(names(grid_cache)) + 1)
              grid_cache[[g_ind]] <- grid
              grid_info[[g_ind]] <- list(grid_name = grid.nm,
                                         closure_name = closure.nm,
                                         combined_areas = sort(combined))
            }
          }
        }
        
        # mode ----
        
        output$modeMsg <- renderUI({
          
          if (is.null(secondgridfile) & input$mode == "combine") {
            
            div(style = "background-color: yellow; border: 1px solid #999; margin: 5px; margin-bottom: 2em;",
                p("Upload a second grid file to combine closure areas with grid zones."))
          }
        })
        
        observeEvent(input$mode,  {
          
          if (input$mode == "combine") {
            
            if (rv$combined) {
              
              updateSelectizeInput(session,
                                   inputId = "clicked_locations",
                                   label = "",
                                   choices = dat$combined$secondLocationID,
                                   selected = NULL,
                                   server = TRUE)
              
            } else {
              
              updateSelectizeInput(session,
                                   inputId = "clicked_locations",
                                   label = "",
                                   choices = dat$secondgridfile$secondLocationID,
                                   selected = NULL,
                                   server = TRUE)
            }
            
          } else {
            
            dat$combined <- NULL
            rv$combined <- FALSE
            rv$combined_areas <- NULL
            
            updateSelectizeInput(session,
                                 inputId = "clicked_locations",
                                 label = "",
                                 choices = dat$gridfile$secondLocationID,
                                 selected = NULL,
                                 server = TRUE)
          }
          
          clicked_ids$ids <- NULL
        })
        
        
        # edit closure ----
        
        observeEvent(input$editClose, {
          
          rv$edit <- closures$dList
          
          showModal(
            modalDialog(title = "Edit or delete closure scenario",
                    
                     uiOutput("editCloseUI"),
                     actionButton("editCloseDelete", "Delete"),
                    footer = tagList(modalButton("Close")),
                    easyClose = FALSE, size = "m"))
          
        })
        
        output$editCloseUI <- renderUI({
          
          close_vec <- vapply(closures$dList, function(x) x$scenario, character(1))
          
          checkboxGroupInput("editCloseGrp", "", choices = close_vec)
          
        })
        
        observeEvent(input$editCloseDelete, {
          
          req(input$editCloseGrp)
          
          del_ind <- vapply(closures$dList, 
                            function(x) x$scenario %in% input$editCloseGrp, 
                            logical(1))
          
          closures$dList[del_ind] <- NULL
        })
        
        
        output$combineUI <- renderUI({
          
          if (!is.null(secondgridfile)) {
            
            if (input$mode == "combine") {
              
              tagList(
                actionButton("combine_grids", "Combine spatial data",
                             style = "color: white; background-color: blue;"),
                
                if (rv$combined) {
                  
                  actionButton("restore", "Restore map",
                               style = "color: white; background-color: blue;")
                })
            }
          }
        })
        
        # combine ----
        observeEvent(input$combine_grids, {
          
          if (!isTruthy(input$clicked_locations)) {
            
            showNotification("Select one or more areas to combine", type = "message")
          }
          
          req(input$clicked_locations)
          
          closure <- secondgridfile %>% 
            dplyr::filter(.data[[secondcat]] %in% clicked_ids$ids)
          
          q_test <- quietly_test(combine_zone)
          dat$combined <- q_test(grid = dat$gridfile, 
                                 closure = closure, 
                                 grid.nm = "secondLocationID", 
                                 closure.nm = secondcat)
          
          dat$combined$zone <- gsub("Zone_", "", dat$combined$secondLocationID)
          rv$combined <- TRUE
          rv$combined_areas <- input$clicked_locations
          
          cache_unique_grid(dat$combined, 
                            grid.nm = grid_nm,
                            closure.nm = close_nm, 
                            combined = input$clicked_locations)
          
          updateSelectizeInput(session,
                               inputId = "clicked_locations",
                               label = "",
                               choices = dat$combined$secondLocationID,
                               selected = NULL,
                               server = TRUE)
  
        })
        
        # enable/disable addClose ----
        observe({
          
          if (is.null(secondgridfile)) {
            
            shinyjs::enable("addClose")
            
          } else {
            
            if (input$mode == "combine") {
              
              if (rv$combined) shinyjs::enable("addClose")
              else shinyjs::disable("addClose")
              
            } else shinyjs::enable("addClose")
          }
        })
        
       observeEvent(input$restore, {
         
         dat$combined <- NULL
         rv$combined <- FALSE
         rv$combined_areas <- NULL
         clicked_ids$ids <- NULL
         
         updateSelectizeInput(session,
                              inputId = "clicked_locations",
                              label = "",
                              choices = dat$gridfile$secondLocationID,
                              selected = NULL,
                              server = TRUE)
       })
        
       
        # splitLayout helper function
        split_ui_helper <- function(ui, width) {
          
          split_ui <- lapply(seq_along(ui), function(x) {
            
            tags$div(style = paste("width:", width), ui[[x]])
          })
          
          tags$div(class = "shiny-split-layout", split_ui)
        }
        
        
        output$tac <- renderUI({

          req(input$clicked_locations)

          if (!is.null(secondgridfile) & input$mode == "combine" & !rv$combined) {

            return(NULL)
            
          } else {
            
            numInits <- length(input$clicked_locations)
            i = 1:numInits
            numwidth <- (1/numInits*100)
            numwidth <- paste0(as.character(numwidth), "%")
            
            UI <- lapply(i, function(x) {
              
              numericInput(paste0("int", x),
                           paste("% TAC allowed", input$clicked_locations[x]),
                           value = 0, min = 0, max = 100)
            })
            
            split_ui_helper(UI, numwidth)
          }
        })
        
        # render map ----
        output$map <- renderLeaflet({
          
        if (!is.null(secondgridfile)) {
          
          if (input$mode == "combine") {
            
            if (rv$combined) {
              
              leaflet() %>%
                addTiles() %>%
                addPolygons(data = dat$combined,
                            fillColor = "white",
                            fillOpacity = 0.5,
                            color = "black",
                            stroke = TRUE,
                            weight = 1,
                            layerId = ~secondLocationID,
                            group = "regions",
                            label = ~secondLocationID)
              
            } else {
              
              leaflet() %>%
                addTiles() %>%
                addPolygons(data = dat$gridfile,
                            fill = FALSE,
                            weight = 1,
                            color = "black") %>% 
                addPolygons(data = secondgridfile,
                            weight = 2,
                            fillColor = "white",
                            fillOpacity = 0.5,
                            color = "black",
                            stroke = TRUE,
                            layerId = secondgridfile[[secondcat]],
                            group = "regions",
                            label = secondgridfile[[secondcat]])
            }
            
          } else { # normal mode
            
            leaflet() %>%
              addTiles() %>%
              addPolygons(data = secondgridfile,
                          fill = FALSE,
                          weight = 2,
                          color = "blue") %>% 
                          # color = ~qpal(secondgridfile[[secondcat]])) %>% 
              addPolygons(data = dat$gridfile,
                          fillColor = "white",
                          fillOpacity = 0.2,
                          color = "black",
                          stroke = TRUE,
                          weight = 1,
                          layerId = ~secondLocationID,
                          group = "regions",
                          label = ~secondLocationID) 
          }
            
        } else {
            
            leaflet() %>%
              addTiles() %>%
              addPolygons(data = dat$gridfile,
                          fillColor = "white",
                          fillOpacity = 0.5,
                          color = "black",
                          stroke = TRUE,
                          weight = 1,
                          layerId = ~secondLocationID,
                          group = "regions",
                          label = ~secondLocationID)
          }
        })
        
        
        observeEvent(input$map_shape_click, {
          
          # create object for clicked polygon
          click <- input$map_shape_click
          
          req(click$id)
          
          if (!is.null(secondgridfile)) {
            
            if (input$mode == "combine") {
              
              if (rv$combined) {
                
                temp_dat <- dat$combined
                z_id <- "zone"
                sec_id <- "secondLocationID"
                
              } else {
                
                temp_dat <- dat$secondgridfile
                z_id  <- "closure_id"
                sec_id <- secondcat
              }
              
            } else {
              
              temp_dat <- dat$gridfile
              z_id <- "zone"
              sec_id <- "secondLocationID"
            }
        
          } else {
            
            temp_dat <- dat$gridfile
            z_id <- "zone"
            sec_id <- "secondLocationID"
          }
          
          #define leaflet proxy for second regional level map
          proxy <- leafletProxy("map")
          
          #append all click ids in empty vector
          clicked_ids$ids <- c(clicked_ids$ids, click$id) # name when clicked, id when unclicked
          
          #shapefile with all clicked polygons - original shapefile subsetted by all admin names from the click list
          clicked_polys <- temp_dat %>% filter(.data[[sec_id]] %in% clicked_ids$ids)
          
          #if the current click ID [from GID_1] exists in the clicked polygon (if it has been clicked twice)
          if (click$id %in% clicked_polys[[z_id]]){
            
            #define vector that subsets NAME that matches GID_1 click ID - needs to be different to above
            name_match <- clicked_polys[[sec_id]][clicked_polys[[z_id]] == click$id]
            
            #remove the current click$id AND its name match from the clicked_polys shapefile
            clicked_ids$ids <- clicked_ids$ids[!clicked_ids$ids %in% click$id]
            clicked_ids$ids <- clicked_ids$ids[!clicked_ids$ids %in% name_match]
            
            # update
            updateSelectizeInput(session,
                                 inputId = "clicked_locations",
                                 label = "",
                                 choices = temp_dat[[sec_id]],
                                 selected = clicked_ids$ids,
                                 server=TRUE)
            
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
                                  layerId = clicked_polys[[z_id]])
            
            updateSelectizeInput(session,
                                 inputId = "clicked_locations",
                                 label = "",
                                 choices = temp_dat[[sec_id]],
                                 selected = clicked_ids$ids,
                                 server=TRUE)
          }
        })
        
        #access variable int outside of observer
        tac_name <- reactive({
          paste(lapply(1:length(input$clicked_locations), function(i) {
            inputName <- paste("int", i, sep = "")
            input[[inputName]]
          }))
        })
        
        # add closure ----
        observeEvent(input$addClose, {
          
          pass <- TRUE
          
          click <- input$map_shape_click
          
          req(click$id)
          
          if (!isTruthy(input$scenarioname)) { # check for unique scenario name
            
            showNotification("Enter a scenario name", type = "message")
          }
          
          close_new <- vapply(closures$dList, function(x) x$scenario, character(1))
          
          if (input$scenarioname %in% c(close_new, close_names(project))) {
            
            showNotification("Enter a unique scenario name", type = "message")
            pass <- FALSE
          }
          
          req(input$scenarioname)
          req(pass)
          
          closures$dList <- c(closures$dList, 
                              list(c(list(scenario = input$scenarioname), 
                                     list(date = as.character(Sys.Date())), 
                                     list(zone = input$clicked_locations), 
                                     list(tac = tac_name()),
                                     list(grid_name = grid_nm),
                                     list(closure_name = close_nm),
                                     list(combined_areas = rv$combined_areas))))
          
          if (is.null(secondgridfile) | input$mode == "normal") {
            
            cache_unique_grid(dat$gridfile, grid.nm = grid_nm,
                              closure.nm = close_nm, combined = NULL)
          }
          
        })
        
        
        output$closureVTO1 <- renderPrint({
          
          req(closures$dList)
          closures$dList
        })
        
        output$closureVTO2 <- renderPrint(rev(rv$saved))
        
        # save ----
        observeEvent(input$saveClose, { 
          
          if (!isTruthy(closures$dList)) {
            
            showNotification("Add a scenario", type = "message")
          }
          
          req(closures$dList)
          
          save_closure_scenario(project, closures$dList)
          
          # save unique grids 
          save_grid_cache(project, 
                          grid_list = reactiveValuesToList(grid_cache),
                          grid_info = reactiveValuesToList(grid_info))
          
          # log grid info
          log_grid_info(project, 
                        grid_info = reactiveValuesToList(grid_info))
          
          # reset closure list
          closures$dList <- NULL
          rv$saved <- get_closure_scenario(project)
        })
        
      }) #END SHINYAPP
    
  }
}

