#' Map of predicted probabilities
#'
#' Create a map showing predicted probabilities by zone
#'
#' @param project Name of project
#' @param policy.name Name of policy scenario
#' @param spat A spatial data file containing information on fishery management 
#'  or regulatory zones boundaries. `sf` objects are recommended, but `sp` objects 
#'  can be used as well. See [dat_to_sf()] to convert a spatial table read from 
#'  a csv file to an `sf` object. To upload your spatial data to the FishSETFolder 
#'  see [load_spatial()].
#' @param zone.spat Name of zone ID column in `spat`.
#' @param outsample Logical, indicating if \code{predict_map()} is being used for creating map of out-of-sample
#' predicted fishing probabilities \code{outsample = TRUE} or policy scenario \code{outsample = FALSE}.
#' @param outsample_pred A dataframe with fishing location and predicted probabilities for out-of-sample data.
#' \code{outsample_pred = NULL} by default and when plotting policy scenarios.
#' @return A map showing predicted probabilities
#' @details This function requires that model and prediction output tables exist in the FishSET database when 
#' plotting policy scenario maps.
#' @import ggplot2
#' @import dplyr
#' @import sf
#' @export
#' @examples 
#' \dontrun{
#'
#' predict_map(project = "scallop", policy.name = "logit_c_mod1 closure_1", 
#'             spat = spat, zone.spat = "TEN_ID")
#'
#' }

predict_map <- function(project, policy.name = NULL, spat, zone.spat, outsample = FALSE, outsample_pred = NULL){
  
  # Policy map ----------------------------------------------------------------------------------------------------------------
  if(!outsample){
    # Create connection to database and remove connection on exit of this function
    fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project))
    on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
    
    ## Load prediction outputs -------------------------------------------------------------------------------------------------
    # make sure prediction table exist in the database
    # TODO: If this is running in the Shiny and one or more of the tables above do not exist then show error message and stop running function here
    tryCatch({
      model_output <- unserialize_table(paste0(project, "ModelOut"), project)},
      
      error = function(err){message(paste0("Model output table not found in ", project))}
    )
    tryCatch({
      pred_output <- unserialize_table(paste0(project, "PredictOutput"), project)},
      
      error = function(err){message(paste0("Prediction output table not found in ", project))}  
    )
    
    # Get a list of model names from prediction output scenario names
    predict_n <- unlist(lapply(pred_output, function(x) x$scenario.name))
    mod_n <- unique(sapply(strsplit(predict_n, split = " "), "[", 1))
    
    if(policy.name %in% mod_n){
      # Get the index for the first prediction output for the model
      ind <- grep(policy.name, predict_n)[1]
      
      # Get predicted probabilities by zone for the model
      predProbs <- pred_output[[ind]]$prob[, 1]/100
      probs_df <- data.frame(ZoneID = as.character(pred_output[[ind]]$zoneID), 
                             Probability = predProbs)
      
    } else if (policy.name %in% predict_n) {
      # Get index for the prediction output for the policy
      ind <- which(predict_n %in% policy.name)
      
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
  
  # use WGS 84 if crs is missing
  if(is.na(sf::st_crs(spatdat))) {
    spat_join <- sf::st_transform(spat_join, crs = 4326)
  } 
  
  if(any(!(sf::st_is_valid(spatdat)))) {
    spat_join <- sf::st_make_valid(spat_join)
  } 
  
  # create a bbox using zones that exist in dat
  z_ind <- spatdat[[zone.spat]] %in% unlist(probs_df[zone.spat])
  bbox <- sf::st_bbox(spatdat[z_ind, ]) # keeps shifted long
  
  # If this is na then input settings not correct
  if(is.na(bbox["xmin"])){
    return(1)
  }
  
  # world2 uses 0 - 360 lon format
  base_map <- ggplot2::map_data(map = ifelse(shift_long(spatdat), "world2", "world"),
                                xlim = c(bbox["xmin"], bbox["xmax"]),
                                ylim = c(bbox["ymin"], bbox["ymax"]))
  
  # convert data to sf for plotting purposes
  base_map <- sf::st_as_sf(base_map, coords = c("long", "lat"),
                           crs = sf::st_crs(spat_join))
  
  # convert points to polygon
  base_map <-
    base_map %>%
    dplyr::group_by(across(all_of("group"))) %>%
    dplyr::summarize(do_union = FALSE) %>%
    sf::st_cast("POLYGON")
  
  var_sym <- function() rlang::sym("Probability")
  
  # breaks ----
  prob_range <- range(probs_df$Probability)
  brks <- pretty(probs_df$Probability, n = 10)
  bin_colors <- fishset_viridis(length(brks))
  
  rescale_val <- scales::rescale(brks)
  
  # Plot
  out <- 
    ggplot2::ggplot() +  
    ggplot2::geom_sf(data = base_map) +  
    ggplot2::geom_sf(data = spat_join, 
                     ggplot2::aes(fill = !!var_sym()), color = "black", alpha = .8) +
    ggplot2::coord_sf(xlim = c(bbox[1]-1, bbox[3]+1), ylim = c(bbox[2]-1, bbox[4]+1),
                      expand = TRUE) +
    ggplot2::binned_scale(aesthetics = "fill",
                          scale_name = "stepsn", 
                          palette = function(x) bin_colors,
                          breaks = brks,
                          show.limits = TRUE,
                          guide = "colorsteps",
                          name = "Probability",
                          labels = scales::comma) +
    fishset_theme() +
    ggplot2::theme(legend.key.size = unit(1, "cm"), 
                   legend.background = ggplot2::element_rect(fill = "grey90"))
  
  return(out)
}