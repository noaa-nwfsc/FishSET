

# make_model_design <-
#   function(DONE - project,
#            DONE - catchID, wrapped in formula
#            DONE - likelihood = NULL, this will be implied from the formula
#            DONE - vars1 = NULL, wrapped in formula
#            DONE - vars2 = NULL, wrapped in formula
#            DONE - priceCol = NULL, wrapped in formula
#            DONE - polyn = NULL, wrapped in formula
#            
#            DONE - initparams = NULL, wrapped in ...
#            DONE - optimOpt = c(100, 1.0e-08, 1, 1), wrapped in ...
#            DONE - methodname = "BFGS", wrapped in ...
#
#            DONE - mod.name = NULL,
#
#            DONE - startloc = NULL, not used

#            expectcatchmodels = list('all'),
#            crs = NULL,

#            outsample = FALSE, - need to handle after
#            CV_dat = NULL) - need to handle after  


fishset_design <- function(project,
                           model_name,
                           alt_name,
                           formula,
                           unique_obs_id,
                           zone_id,
                           ...){
  
  # Load data from the FishSET project ------------------------------------------------------------
  dataset <- table_view(paste0(project, "MainDataTable"), project)
  altc_data <- unserialize_table(paste0(project,"AltMatrix"), project)

  # Filter data based on zones present in the alternative catch matrix ----------------------------
  unique_zones <- unique(altc_data$greaterNZ)
  dataset <- dataset %>%
    filter(!!sym(zone_id) %in% unique_zones)
  
  # Create a long format data frame for all possible choices --------------------------------------
  df <- data.frame(
    zones = rep(unique_zones, length(dataset[[unique_obs_id]])),
    obsID = rep(dataset[[unique_obs_id]], each = length(unique_zones))
  )
  
  # Identify the chosen zone for each trip/haul/observation ---------------------------------------
  df$zone_obs <- paste0(df$zones, df$obsID)
  selected <- paste0(dataset[[zone_id]], dataset[[unique_obs_id]])
  df$selected <- 0
  df$selected[which(df$zone_obs %in% selected)] <- 1
  df$zone_obs <- NULL # Clean up the helper column
  
  # Parse the formula -----------------------------------------------------------------------------
  
  
  # Reshape covariate data to long format and join ------------------------------------------------
  distance_long <- as.data.frame(mdf$distance) %>%
    mutate(haul_id = main_data$haul_id) %>%
    pivot_longer(
      cols = -c(haul_id),
      names_to = "zones",
      values_to = "distance_from_port")
  
  exp_catch_long <- as.data.frame(mdf$gridVaryingVariables$exp1) %>%
    mutate(haul_id = main_data$haul_id) %>%
    pivot_longer(
      cols = -c(haul_id),
      names_to = "zones",
      values_to = "expected_catch")
  
  # Join all data together
  df_long <- df %>%
    mutate(zones = as.character(zones)) %>%
    left_join(distance_long, by = c("zones" = "zones", "obsID" = "haul_id")) %>%
    left_join(exp_catch_long, by = c("zones" = "zones", "obsID" = "haul_id")) %>%
    rename(ZoneID = zones, haul_id = obsID) %>%
    select(ZoneID, haul_id, selected, distance_from_port, expected_catch) %>%
    mutate(distance_from_port = as.numeric(distance_from_port))
  
  # Final data cleaning: remove zones with NA distance values
  zones_to_remove <- unique(df_long[which(is.na(df_long$distance_from_port)),]$ZoneID)
  df_long <- df_long %>% 
    filter(!(ZoneID %in% zones_to_remove))
  
  # Convert data from long to wide matrix format for the model
  model_matrices <- pivot_to_wide_matrices(
    data = df_long,
    id_col = "haul_id",
    names_from_col = "ZoneID",
    values_to_spread = c("selected", "expected_catch", "distance_from_port")
  )
  
  Y <- model_matrices$selected
  catch <- model_matrices$expected_catch
  distance <- model_matrices$distance_from_port
  
  
  return(NULL)
  
}