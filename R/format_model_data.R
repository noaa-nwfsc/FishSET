

format_model_data <- function(project, # project <- "sc_haul1"
                              name, # name <- "TEST1"
                              alt_name = NULL, # Temp default to NULL, but this should be included
                              zone_id, # zone_id <- "ZoneID"
                              unique_obs_id, # unique_obs_id <- "unique_row_id"
                              select_vars = NULL, # select_vars <- c("TRIPID","DATE_TRIP","GEARCODE","landed_thousands","lagged_zone_id")
                              # Recommended or else this will reformat entire dataset could take a while
                              # IMPORTANT NOTE: remember to included lagged zone ID var
                              aux_data = NULL, # aux_data <- "sc_haul1haul_vessel_charsAuxTable"
                              gridded_data = NULL, # gridded_data <- "sc_haul1haul_swell_heightGridTable"
                              expectations = NULL, # expectations <- c("exp_matrix_1","exp_matrix_2")
                              # Need to explicitly add dummy
                              distance = TRUE,
                              crs = NULL, # Only used if distance = TRUE
                              impute = NULL){ # mean, median, mode, remove (removes zones with NA values). Mode is automatically used for non-numeric data
  
  # First check if formatted table exists 
  if (table_exists(table_name, project)) {
    # load data and check names
    tmp_data <- unserialize_table(paste0(project, "LongFormatData"), project)
    names_tmp_data <- names(tmp_data)
    if (name %in% names_tmp_data) {
      stop(paste0("Formatted data with the name '", name, "' already exists. Enter a new name."))
    }
  }
  
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
  if(!is.null(expectations) & length(expectations) > 0){
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
  exp_mats <- expect_list[which(names(expect_list) %in% expectations)]
  
  # Create a long format data frame for all possible choices --------------------------------------
  # Save number of zones and observations (rows)
  n_zones <- length(unique_zones)
  n_obs <- nrow(dataset)
  
  # Long format of zones
  zones_lf = as.character(rep(unique_zones, n_obs))
  
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
  if (distance) {
    port <- NULL #initialize to NULL if no port included
    tryCatch({
      pt <- data_pull(paste0(project, 'PortTable'), project)
      ptname <- pt$dat # Note: ptname not used 
      port <- pt$dataset # used in create_dist_matrix()
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
    
    # Need to check if occasion_var is numeric
    if(alt_list$occasion %in% c("zonal centroid", "fishing centroid", "port")){
      if(is.numeric(original_dataset[[alt_list$occasion_var]])) {
        original_dataset[[alt_list$occasion_var]] <- 
          as.character(original_dataset[[alt_list$occasion_var]])
      }  
    }
    
    dist_out <- create_dist_matrix(dataset = original_dataset,
                                   unique_obs_id = unique_obs_id,
                                   spat = spatdat,
                                   spatID = spatID,
                                   port = port, 
                                   alt_var = alt_list$alt_var, 
                                   occasion = alt_list$occasion, 
                                   occasion_var = alt_list$occasion_var,
                                   dataZoneTrue = alt_list$dataZoneTrue, 
                                   zone_cent = alt_list$zone_cent, 
                                   fish_cent = alt_list$fish_cent, 
                                   choice = alt_list$choice, 
                                   units = alt_list$altChoiceUnits, 
                                   zoneID = alt_list$zoneID, 
                                   crs = crs)
    
    distance_wide <- as.data.frame(dist_out$dist_matrix)
    dist_row_names <- gsub("^X|\\.\\d+$", "", rownames(distance_wide))
    distance_wide$unique_obs_id <- dist_row_names
    distance_long <- distance_wide %>%
      pivot_longer(
        cols = -c(unique_obs_id),
        names_to = "chosen_zone",
        values_to = "distance")
    
    df <- merge(
      x = df,
      y = distance_long,
      by.x = c("zones", unique_obs_id),
      by.y = c("chosen_zone", "unique_obs_id"),
      all.x = TRUE, # all.x and sort keep data in same order as original df
      sort = FALSE)
  }
  
  # Reshape and join expectation matrix -----------------------------------------------------------
  for (i in 1:length(exp_mats)) {
    new_col_name <- expectations[i]
    
    tmp_df <- as.data.frame(exp_mats[[i]]) %>%
      mutate(unique_obs_id = dataset[[unique_obs_id]]) %>%
      pivot_longer(
        cols = -c(unique_obs_id),
        names_to = "zones",
        values_to = new_col_name
      )
    
    df <- merge(
      x = df,
      y = tmp_df,
      by.x = c("zones", unique_obs_id),
      by.y = c("zones", "unique_obs_id"),
      all.x = TRUE,
      sort = FALSE
    )
  }
  
  # Add aux data ----------------------------------------------------------------------------------
  
  # Add gridded data ------------------------------------------------------------------------------
  
  # Check NAs and impute --------------------------------------------------------------------------
  if (any(is.na(df))) {
    # Sum of NA values in each column
    na_counts <- colSums(is.na(df))
    # Identify column names with NAs
    columns_w_na <- names(na_counts[na_counts > 0])
    
    if (is.null(impute)) {
      columns_w_na <- paste(columns_w_na, collapse = ",")
      stop(
        paste0("NAs found in ", 
               columns_w_na, 
               ". Remove column(s) or specify method to impute data. If 'distance' contains NAs ",
               "the impute='remove' option will remove the zone(s) that contain missing values."))  
    }
    
    if (!impute %in% c("mean", "median", "mode", "remove")){
      stop(paste0("Impute method must be one of 'mean', 'median', or 'mode'."))
    }
    
    if (impute %in% c("mean", "median", "mode")) {
      # Helper function to calculate mode
      get_mode <- function(x) {
        ux <- unique(x[!is.na(x)])
        if (length(ux) == 0) return(NA)
        tab <- tabulate(match(x, ux))
        ux[tab == max(tab)][1] # Returns the first mode in case of ties
      }
      
      # Iterate over columns with missing values
      for (col in columns_w_na){
        # Check data type
        is_numeric_col <- is.numeric(df[[col]])
        
        fill_value <- NULL
        
        if (is_numeric_col) {
          if (impute == "mean") {
            fill_value <- mean(df[[col]], na.rm = TRUE)
          } else if (impute == "median") {
            fill_value <- median(df[[col]], na.rm = TRUE)
          } else if (impute == "mode") {
            fill_value <- get_mode(df[[col]])
          }
        } else {
          # For non-numeric data, force mode
          fill_value <- get_mode(df[[col]])
        }
        
        # Apply imputation
        df[[col]][is.na(df[[col]])] <- fill_value
      } 
      
    } else if (impute == "remove") {
      zones_to_remove <- df$zones[which(!complete.cases(df))]
      df <- df %>%
        filter(!(zones %in% zones_to_remove))
    }
  }
  
  # Rename columns and save -----------------------------------------------------------------------
  # Rename cols
  df <- df %>%
    rename(!!zone_id := zones)
  # Save settings
  settings <- as.list(match.call())[-1]
  # Save data and settings as a list
  df_list <- list(tmp_name = df,
                  tmp_settings = settings)
  names(df_list) <- c(name, paste0(name, "_settings"))
  
  # Save formated data, as well as settings
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  table_name <- paste0(project, "LongFormatData")
  
  if (table_exists(table_name, project)) {
    # save data and remove
    all_formatted_data <- unserialize_table(table_name, project)
    table_remove(table_name, project)
    df_list <- c(all_formatted_data, df_list)
  } 
  
  # Save table and insert data
  DBI::dbExecute(fishset_db, 
                 paste("CREATE TABLE IF NOT EXISTS", 
                       table_name, 
                       "(data df_list)"))
  DBI::dbExecute(fishset_db, 
                 paste("INSERT INTO", 
                       table_name, 
                       "VALUES (:data)"),
                 params = list(data = list(serialize(df_list, NULL))))
  
  # Log the function call -------------------------------------------------------------------------
  format_model_data_function <- list()
  format_model_data_function$functionID <- "format_model_data"
  format_model_data_function$args <- as.list(match.call())[-1]
  format_model_data_function$kwargs <- list()
  
  log_call(project, format_model_data_function)
}


