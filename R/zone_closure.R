#' Define zone closure scenarios
#' 
#' @param project Required, name of project.
#' @param spat Required, data file or character. 
#'   \code{spat} is a spatial data file containing information on fishery 
#'   management or regulatory zones boundaries. Shape, json, geojson, and csv 
#'   formats are supported. geojson is the preferred format. json files must be 
#'   converted into geoson. This is done automatically when the file is loaded 
#'   with \code{\link{read_dat}} with \code{is.map} set to true. \code{spat} 
#'   cannot, at this time, be loaded from the FishSET database. \cr
#' @param cat Variable in \code{spat} that identifies the individual areas or zones.
#' @param secondspat Optional spatial data file containing information to 
#'   aid in defining zone closures, such as location of windmills.
#' @param secondcat Optional, name of variable that identifies individual areas 
#'   in \code{secondspat}
#' @param lon.spat Required for csv files. Variable or list from \code{spat} 
#'   containing longitude data. Leave as NULL if \code{spat} is a shape or json file.
#' @param lat.spat Required for csv files. Variable or list from \code{spat} 
#'   containing latitude data.  Leave as NULL if \code{spat} is a shape or json file.
#' @param epsg EPSG number. Set the epsg to ensure that \code{spat} and 
#'   \code{secondspat} have the same projections. If epsg is not specified but is defined for 
#'   \code{spat}, then the \code{secondspat} epsg will be applied to \code{spat}. 
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

zone_closure <- function(project, spat, cat, secondspat = NULL, 
                         secondcat = NULL, lon.spat = NULL, lat.spat = NULL,
                         epsg = NULL) {
 
  pass <- TRUE
  x <- 0
  secondLocationID <- NULL

  grid_nm <- deparse(substitute(spat)) # won't work in main app
  close_nm <- deparse(substitute(secondspat))

  # leaflet requires WGS84
  spat <- sf::st_transform(spat, "+proj=longlat +datum=WGS84")

  if (!is.null(secondspat)) {

    secondspat <- sf::st_transform(secondspat, 
                                       "+proj=longlat +datum=WGS84")
  }
 
  spat <- check_spatdat(spat, id = cat, lon = lon.spat, lat = lat.spat)
  
  if(!is.null(secondspat)){
    
    secondspat <- check_spatdat(secondspat, id = secondcat)
    secondspat[[secondcat]] <- as.factor(secondspat[[secondcat]])
    secondspat$closure_id <- paste0("Closure_", 1:nrow(secondspat))
    # 
    # qpal <- colorFactor(grDevices::topo.colors(length(unique(secondspat[[secondcat]]))), 
    #                     secondspat[[secondcat]],unique(secondspat[[secondcat]]))
    
  }
  
  if (!is.null(secondspat)) {
    
    if (sf::st_crs(spat) != sf::st_crs(secondspat)) {
      
      warning("Projection does not match. The detected projection in the",
              " spatial file will be used unless epsg is specified.")
    }
    
    if (!is.null(epsg)) {
      
      spat <- sf::st_transform(spat, epsg)
      secondspat <- sf::st_transform(secondspat, epsg)
      
    } else {
      
      crs1 <- sf::st_crs(spat)
      crs2 <- sf::st_crs(secondspat)
      
      if (is.na(crs1) & is.na(crs2)) {
        
        spat <- sf::st_transform(spat, "+proj=longlat +datum=WGS84")
        secondspat <- sf::st_transform(secondspat, 
                                           "+proj=longlat +datum=WGS84")
        
      } else if (is.na(crs1) & !is.na(crs2)) {
        
        spat <- sf::st_transform(spat, crs2)
        
      } else if (!is.na(crs1) & is.na(crs2)) {
        
        secondspat <- sf::st_transform(secondspat, crs1)
        
      } else if (!is.na(crs1) & !is.na(crs2)) {
        
        if (crs1 != crs2) {
          
          spat <- sf::st_transform(spat, "+proj=longlat +datum=WGS84")
          secondspat <- sf::st_transform(secondspat, 
                                             "+proj=longlat +datum=WGS84")
        }
      }
    }

  }
  
  
  if (cat %in% names(spat)) {
    
    spat$secondLocationID <- paste("Zone_", as.character(spat[[cat]]), sep="")
    names(spat)[which(names(spat) == cat)] <- 'zone'
    spat$zone <- as.character(spat$zone)
    
  } else if ('zone' %in% names(spat)) {
    
    spat$zone <- as.character(spat$zone)
    spat$secondLocationID <- paste("Zone_", as.character(spat$zone), sep="")
    
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
          column(4,  uiOutput("modeMsg")),
          column(2,  uiOutput("GridSelect"))
        ),
        
        selectizeInput(inputId = 'spat_var', 
                       label = 'Select area or zone ID variable',
                       choices = colnames(spat)),
        
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
        dat <- reactiveValues(spat = spat,
                              secondspat = secondspat,
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
                                          mod_type = "combine", 
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
                                         mod_type = "combine",
                                         closure_name = closure.nm,
                                         combined_areas = sort(combined))
            }
          }
        }
        
        add_close <- function(close, mode) {
          
          if (mode == "combine") close
          else return(NULL)
        }
        
        # mode ----
        
        output$modeMsg <- renderUI({
          
          if (is.null(secondspat) & input$mode == "combine") {
            
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
                                   choices = dat$secondspat$secondLocationID,
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
                                 choices = dat$spat$secondLocationID,
                                 selected = NULL,
                                 server = TRUE)
          }
          
          clicked_ids$ids <- NULL
        })
        
        observeEvent(input$spat_var, {
          cat <- input$spat_var
        })
        
        output$GridSelect <- renderUI({
          
          if (!is.null(secondspat) & input$mode == "combine") {
              
            selectInput("select_grid", "Select grid", 
                        choices = names(grid_cache))
          }
          
        })
        
        # select grid ----
        observeEvent(input$select_grid, {
          
          dat$combined <- grid_cache[[input$select_grid]]
          
          updateSelectizeInput(session,
                               inputId = "clicked_locations",
                               label = "",
                               choices = dat$combined$secondLocationID,
                               selected = NULL,
                               server = TRUE)
          
          rv$combined_areas <- grid_info[[input$select_grid]]$combined_areas
          
        }, ignoreNULL = TRUE)
        
        
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
        
        
        
        
        
        # combine ----
        
        output$combineUI <- renderUI({
          
          if (!is.null(secondspat)) {
            
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
        
        
        observeEvent(input$combine_grids, {
          
          if (!isTruthy(input$clicked_locations)) {
            
            showNotification("Select one or more areas to combine", type = "message")
          }
          
          req(input$clicked_locations)
          
          closure <- secondspat %>% 
            dplyr::filter(.data[[secondcat]] %in% clicked_ids$ids)
          
          q_test <- quietly_test(combine_zone)
          dat$combined <- q_test(grid = dat$spat, 
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
          
          updateSelectInput(session,
                            inputId = "select_grid",
                            choices = names(grid_cache),
                            selected = dplyr::last(names(grid_cache)))
  
        })
        
        # enable/disable addClose ----
        observe({
          
          if (is.null(secondspat)) {
            
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
                              choices = dat$spat$secondLocationID,
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

          if (!is.null(secondspat) & input$mode == "combine" & !rv$combined) {

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
          
        if (!is.null(secondspat)) {
          
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
                addPolygons(data = dat$spat,
                            fill = FALSE,
                            weight = 1,
                            color = "black") %>% 
                addPolygons(data = secondspat,
                            weight = 2,
                            fillColor = "white",
                            fillOpacity = 0.5,
                            color = "black",
                            stroke = TRUE,
                            layerId = secondspat[[secondcat]],
                            group = "regions",
                            label = secondspat[[secondcat]])
            }
            
          } else { # normal mode
            
            leaflet() %>%
              addTiles() %>%
              addPolygons(data = secondspat,
                          fill = FALSE,
                          weight = 2,
                          color = "blue") %>% 
                          # color = ~qpal(secondspat[[secondcat]])) %>% 
              addPolygons(data = dat$spat,
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
              addPolygons(data = dat$spat,
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
          
          if (!is.null(secondspat)) {
            
            if (input$mode == "combine") {
              
              if (rv$combined) {
                
                temp_dat <- dat$combined
                z_id <- "zone"
                sec_id <- "secondLocationID"
                
              } else {
                
                temp_dat <- dat$secondspat
                z_id  <- "closure_id"
                sec_id <- secondcat
              }
              
            } else {
              
              temp_dat <- dat$spat
              z_id <- "zone"
              sec_id <- "secondLocationID"
            }
        
          } else {
            
            temp_dat <- dat$spat
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
                                 server = TRUE)
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
          
          close_nm <- if (input$mode == "normal") NULL else close_nm
          
          closures$dList <- c(closures$dList, 
                              list(c(list(scenario = input$scenarioname), 
                                     list(date = as.character(Sys.Date())), 
                                     list(zone = input$clicked_locations), 
                                     list(tac = tac_name()),
                                     list(grid_name = grid_nm),
                                     list(closure_name = add_close(close_nm, input$mode)),
                                     list(combined_areas = rv$combined_areas))))
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
          
          
          if (input$mode == "combine") {
            
            # save unique grids to project data folder
            save_grid_cache(project, 
                            grid_list = reactiveValuesToList(grid_cache),
                            grid_info = reactiveValuesToList(grid_info),
                            mod_type = "combine")
            
            # log grid info
            log_grid_info(project, 
                          grid_info = reactiveValuesToList(grid_info),
                          mod_type = "combine")
          }
          
          # update closure list w/ new grid names
          g_info <- get_grid_log(project)
          
          new_grid_nms <- 
            vapply(closures$dList, function(cl) {
            
              if (!is.null(cl$combined_areas)) {
                
                cols <- c("grid_name", "closure_name", "combined_areas")
                
                ind <- vapply(g_info, function(gi) identical(gi[cols], cl[cols]), 
                              logical(1))
                
                names(g_info)[ind] # if no match?
                
              } else cl$grid_name
            
          }, character(1))
          
          close_list <- 
            purrr::map2(closures$dList, new_grid_nms, function(x, y) {
            
            if (x$grid_name != y) x$grid_name <- y
            x
          })
          
          # save closure scenarios to project output folder
          save_closure_scenario(project, close_list)
          
          # reset closure list
          closures$dList <- NULL
          rv$saved <- get_closure_scenario(project)
        })
        
      }) #END SHINYAPP
    
  }
}

