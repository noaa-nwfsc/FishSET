

format_model_data <- function(project,
                              name,
                              alt_name,
                              zone_id,
                              unique_obs_id,
                              select_vars = NULL, # Recommended or else this will reformat entire dataset could take a while
                                                  # IMPORTANT NOTE: remember to included lagged zone ID var
                              aux_data = NULL,
                              gridded_data = NULL,
                              expectations = NULL, # Need to explicitly add dummy
                              distance = TRUE,
                              crs = NULL, # Only used if distance = TRUE
                              impute = NULL){
  
  # Load data -------------------------------------------------------------------------------------
  # Load main data table
  original_dataset <- table_view(paste0(project, "MainDataTable"), project)
  
  # Load alternative choice list
  alt_list <- unserialize_table(paste0(project, "AltMatrix"), project)
  
  # Load aux_data
  if(!is_empty(aux_data)){
    aux_df <- table_view(aux_data, project)
  }
  
  # Load gridded_data
  if(!is_empty(gridded_data)){
    gridded_df <- table_view(gridded_data, project)
  }
  
  # Load expectations
  if(!is_empty(expectations)){
    expect_list <- unserialize_table(paste0(project,"ExpectedCatch"), project)
  }
  
  # Select variables and filter main data ---------------------------------------------------------
  unique_zones <- unique(alt_list$greaterNZ)
  dataset <- original_dataset %>% filter(!!sym(zone_id) %in% unique_zones)
  
  if(length(select_vars) > 0){
    column_check(dataset, select_vars) # Check that columns are in the dataset
    
    dataset <- dataset %>% select(all_of(c(select_vars, zone_id, unique_obs_id)))
  }
  
  # Filter expect_list ----------------------------------------------------------------------------
  exp_mats <- expect_list[[which(names(expect_list) %in% expectations)]]
  
  # Create a long format data frame for all possible choices --------------------------------------
  # Save number of zones and observations (rows)
  n_zones <- length(unique_zones)
  n_obs <- nrow(dataset)
  
  # Long format of zones
  zones_lf = rep(unique_zones, n_obs)
  
  # Long format for observation ID and other selected vars
  if(length(select_vars) > 0){
    dataset_names <- c(unique_obs_id, select_vars)
  } else {
    dataset_names <- names(dataset)
  }
  
  dataset_lf <- lapply(dataset_names, 
                       function(var_name){
                         rep(dataset[[var_name]], each = n_zones)})
  
  # Convert list to a data frame and set names
  dataset_lf <- as.data.frame(dataset_lf)
  names(dataset_lf) <- dataset_names
  
  # Combine with zones
  df <- cbind(zones = zones_lf, dataset_lf)
  
  # Identify the chosen zone for each trip/haul/observation ---------------------------------------
  df$zone_obs <- paste0(df$zones, df[[unique_obs_id]])
  chosen <- paste0(dataset[[zone_id]], dataset[[unique_obs_id]])
  df$chosen <- 0
  df$chosen[which(df$zone_obs %in% chosen)] <- 1
  df$zone_obs <- NULL # Clean up the helper column
  
  # Generate distance matrix ----------------------------------------------------------------------
  if(distance){
    port <- NULL #initialize to NULL if no port included
    tryCatch({
      pt <- data_pull(paste0(project, 'PortTable'), project)
      ptname <- pt$dat # Note: ptname not used 
      port <- pt$dataset # used in create_distance_matrix()
    }, error = function(cond){
      message("Port table not used.")
    })
    
    if(alt_list$alt_var == "nearest point"){
      spatdat <- Alt$spat
      spatID <- Alt$spatID
    } else {
      spatdat <- NULL
      spatID <- NULL
    }
    
    dist_out <- create_dist_matrix(dataset = original_dataset,
                                   spat = spatdat,
                                   spatID = spatID,
                                   alt_var = alt_list$alt_var, 
                                   occasion = alt_list$occasion, 
                                   occasion_var = alt_list$occasion_var,
                                   dataZoneTrue = alt_list$dataZoneTrue, 
                                   zone_cent = alt_list$zone_cent, 
                                   fish_cent = alt_list$fish_cent, 
                                   choice = alt_list$choice, 
                                   units = alt_list$altChoiceUnits, 
                                   port = port, 
                                   zoneRow = alt_list$zoneRow, 
                                   zoneID = alt_list$zoneID, 
                                   crs = crs)
  }
  
  # Reshape matrices to long format and join ------------------------------------------------------
  
  
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
    select(ZoneID, haul_id, chosen, distance_from_port, expected_catch) %>%
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
    values_to_spread = c("chosen", "expected_catch", "distance_from_port")
  )
  
  Y <- model_matrices$chosen
  catch <- model_matrices$expected_catch
  distance <- model_matrices$distance_from_port
  
  
  return(NULL)
}