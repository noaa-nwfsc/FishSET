#' Map of predicted probabilities
#'
#' Create a map showing predicted probabilities by zone
#'
#' @param project Name of project
#' @param mod.name Name of model
#' @param policy.name Name of policy scenario
#' @param spat A spatial data file containing information on fishery management 
#'  or regulatory zones boundaries. `sf` objects are recommended, but `sp` objects 
#'  can be used as well. See [dat_to_sf()] to convert a spatial table read from 
#'  a csv file to an `sf` object. To upload your spatial data to the FishSETFolder 
#'  see [load_spatial()].
#' @param zone.spat Name of zone ID column in `spat`.
#' @param plot_type Character, \code{"dynamic"} for interactive leaflet plots and \code{"static"}
#'  for ggplot.
#' @param outsample Logical, indicating if \code{predict_map()} is being used for creating map of 
#'  out-of-sample predicted fishing probabilities \code{outsample = TRUE} or policy scenario 
#'  \code{outsample = FALSE}.
#' @param outsample_pred A dataframe with fishing location and predicted probabilities for 
#'  out-of-sample data.
#' \code{outsample_pred = NULL} by default and when plotting policy scenarios.
#' @return A map showing predicted probabilities
#' @details This function requires that model and prediction output tables exist in the FishSET 
#' database when plotting policy scenario maps.
#' @import ggplot2
#' @import sf
#' @importFrom dplyr left_join group_by across all_of summarize
#' @export
#' @examples 
#' \dontrun{
#'
#' predict_map(project = "scallop", mod.name = "logit_c_mod1", policy.name = "closure_1", 
#'             spat = spat, zone.spat = "TEN_ID")
#'
#' }

predict_map <- function(project, mod.name = NULL, policy.name = NULL, spat, 
                        zone.spat, plot_type = "dynamic",
                        outsample = FALSE, outsample_pred = NULL){
  
  # Policy map ----------------------------------------------------------------------------------------------------------------
  if(!outsample){
    # Create connection to database and remove connection on exit of this function
    fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project))
    on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
    
    ## Load prediction outputs -------------------------------------------------------------------------------------------------
    # make sure prediction table exist in the database
    # TODO: If this is running in the Shiny and one or more of the tables above do not exist then show error message and stop running function here
    tryCatch({
      model_idx <- which(lapply(model_design_list(project), "[[", "mod.name") == mod.name)
      model_output <- model_design_list(project)[[model_idx]]},
      
      error = function(err){message(paste0("Model output table not found in ", project))}
    )
    tryCatch({
      pred_out <- unserialize_table(paste0(project, "predictOutput"), project)
      
      get_mod_pred_out <- lapply(pred_out, function(x){
        mod_name <- x$modelDat$mod.name
        if(mod_name == mod.name){
          return(1)
        } else {
          return(0)
        }
      })
      
      pred_output <- pred_out[which(unlist(get_mod_pred_out) == 1)]
    },      
    error = function(err){message(paste0("Prediction output table not found in ", project))}  
    )
    
    # Get a list of model names from prediction output scenario names
    predict_n <- unlist(lapply(pred_output, function(x) x$scenario.name))
    mod_n <- unique(sapply(strsplit(predict_n, split = " "), "[", 1))
    model_policy_name <- paste0(mod_n, " ", policy.name)
    
    if (policy.name == mod_n) {
      # Get the index for the first prediction output for the model
      ind <- grep(policy.name, predict_n)[1]
      
      # Get predicted probabilities by zone for the model
      predProbs <- pred_output[[ind]]$prob[, 1]/100
      probs_df <- data.frame(ZoneID = as.character(pred_output[[ind]]$zoneID), 
                             Probability = predProbs)
      
    } else if (model_policy_name %in% predict_n) {
      # Get index for the prediction output for the policy
      ind <- which(predict_n %in% model_policy_name)
      
      # Get predicted probabilities by zone for the policy
      predProbs <- pred_output[[ind]]$prob[, 2]/100
      probs_df <- data.frame(ZoneID = as.character(pred_output[[ind]]$zoneID), 
                             Probability = predProbs)
      
      # Get zone ID for closured zones
      closure <- pred_output[[ind]]$zoneIdIn
    }
    
  } else {
    
    # Out-of-sample prediction map --------------------------------------------------------------------------------------------
    probs_df <- outsample_pred
  }
  
  # Create map --------------------------------------------------------------------------------------------------------------
  
  # Parse spatial dataset
  spatout <- data_pull(spat, project)
  
  spatdat <- spatout$dataset
  spat <- parse_data_name(spat, "spat", project)
  spatdat[[zone.spat]] <- as.character(spatdat[[zone.spat]])
  
  # Rename and re-type probs_df zone column
  names(probs_df) <- c(zone.spat, "Probability")
  probs_df[,1] <- as.character(probs_df[,1])
  
  # Merge spatial dataset with predicted probabilities
  spat_join <- dplyr::left_join(spatdat[zone.spat], probs_df, by = zone.spat) 
  
  
  spat_join <-  sf::st_transform(spat_join, "+proj=longlat +datum=WGS84")
  
  
  var_sym <- function() rlang::sym("Probability")
  
  # breaks ----
  prob_range <- range(probs_df$Probability)
  brks <- pretty(probs_df$Probability, n = 8)
  bin_colors <- fishset_viridis(length(brks))
  
  pal <- colorBin(
    bin_colors,
    bins = brks,
    # colors depend on the count variable
    domain = spat_join$Probability,
  )
  
  # Plot interactive leaflet plot
  if (plot_type == "dynamic"){
    out <- leaflet::leaflet() %>%
      leaflet::addProviderTiles("OpenStreetMap") %>% 
      leaflet::addPolygons(data =  spat_join,
                           fillColor = "white",
                           fillOpacity = 0.5,
                           color = "black",
                           stroke = TRUE,
                           weight = 0.5,
                           layerId = ~var_sym(),
                           group = "regions") %>% 
      leaflet::addPolygons(data = (spat_join %>% filter(!is.na(spat_join$Probability))),
                           fillColor = ~pal(Probability), 
                           color = "black",
                           fillOpacity = 1, 
                           stroke = TRUE,
                           weight = 1, 
                           smoothFactor = 0.2,
                           layerId = ~var_sym(),
                           label = ~paste0("Probability: ", round(Probability,2))) %>% 
      leaflet::addLegend(pal = pal, 
                         values = spat_join$Probability, 
                         position = "bottomright", 
                         title = "Probability")  
    
    # Plot static ggplot figure
  } else if (plot_type == "static") {
    # Save zones included in the model
    spat_probability <- (spat_join %>% filter(!is.na(spat_join$Probability)))
    
    # Get the bounding box
    bbox <- st_bbox(spat_probability)
    
    # Calculate buffer size based on the range of the coordinates
    x_buffer_ratio <- 0.6
    y_buffer_ratio <- 0.8
    x_range <- bbox$xmax - bbox$xmin
    y_range <- bbox$ymax - bbox$ymin
    x_buffer <- x_range * x_buffer_ratio
    y_buffer <- y_range * y_buffer_ratio
    
    # Apply buffer to the bounding box
    x_limits <- c(bbox$xmin - x_buffer, bbox$xmax + x_buffer)
    y_limits <- c(bbox$ymin - y_buffer, bbox$ymax + y_buffer)
    
    # Create base map for the first layer of the plot
    base_map <- ggplot2::map_data(map = "world",
                                  xlim = c(bbox["xmin"], bbox["xmax"]),
                                  ylim = c(bbox["ymin"], bbox["ymax"]))
    
    # Convert base map to sf object and convert points to polygons
    base_map <- sf::st_as_sf(base_map, coords = c("long", "lat"),
                             crs = sf::st_crs(spat_join)) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of("group"))) %>%
      dplyr::summarize(do_union = FALSE) %>%
      sf::st_cast("POLYGON")
    
    out <- ggplot() +
      geom_sf(data = base_map) +
      geom_sf(data = spat_probability, aes(fill = Probability)) +
      scale_fill_viridis_c() +
      coord_sf(xlim = x_limits, ylim = y_limits, expand = FALSE) +
      theme_classic() +
      theme(legend.position = c(0.85, 0.25))
  }
  
  return(out)
}