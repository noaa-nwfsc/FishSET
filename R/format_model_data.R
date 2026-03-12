#' Format Model Data
#' 
#' Reshapes the project datasets into a single long format suitable for discrete choice modeling
#' using RTMB. This function integrates various data sources-including distance matrices,
#' expectation (catch and/or revenue) matrices, auxiliary data, and gridded environmental data-
#' and performs optional missing data imputation.
#' 
#' The resulting formatted data is serialized and stored in the FishSET project database within a
#' table named '[project_name]LongFormatData'.
#' 
#' @param project Name of the project.
#' @param name Name for this specific formatted model data instance. Must be unique within the 
#'   project's formatted data list.
#' @param alt_name Name of the alternative choice matrix. 
#' @param zone_id Variable name in the dataset representing the zone identifier.
#' @param unique_obs_id Variable name in the dataset representing the unique observation
#'   identifier (unique rows in the main data table).
#' @param select_vars Character vector of variable names to retain from the main data table.
#'   Although this input is optional, it is recommended to limit the final format to necessary
#'   variables for computational efficiency. IMPORTANT NOTE: if modeling multi-haul data, 
#'   be sure to include the lagged zone ID (previous location) in this vector. 
#'   
#'   *IMPORTANT NOTE*: for expected profit models, the price and actual catch variables
#'   must be included here.
#' @param aux_data Name of the auxiliary data table to join. Use \code{\link{list_tables}}
#'   function to view the table name.
#' @param aux_key Variable name used to join the main data table with the auxiliary data.
#' @param gridded_data Character vector of the gridded data table(s) to join. 
#'   Use \code{\link{list_tables}} function and set \code{type = "grid"}  to view the table name.
#' @param expectations Character vector containing the names of expected catch or revenue matrices
#'   to merge into the dataset.
#' @param count_var Character representing name of variable containing counts for Poisson-
#'   equivalence model. NOTE: only use this input if runing a Poisson model.
#' @param distance Logical. If 'TRUE', calculates and merges a distance matrix between observations
#'   and zones. Defaults to 'TRUE'.
#' @param distance_units String representing the units of measurement for distance ("km" or "mi").
#' @param impute Method for imputing missing values (NAs). Options are `"mean"`, 
#'   `"median"`, `"mode"`, or `"remove"`. `"Remove"` will completely remove zones from the dataset
#'   that contain any NAs in corresponding data. If NULL, the function stops if NAs are detected.
#' @param crs Coordinate reference system. Only used if 'distance = TRUE' and spatial calculations
#'   are required.
#'  
#' @return A list containing the formatted data frame and the input settings. The list is saved to
#'  the project database.
#'  
#' @examples
#' \dontrun{
#'   # Basic usage: Formatting data with simple mean imputation for missing values
#'   format_model_data(
#'     project = "NewEnglandCod",
#'     name = "ModelData_Run1",
#'     alt_name = "altname_1",
#'     zone_id = "zone_code",
#'     unique_obs_id = "trip_id",
#'     select_vars = c("vessel_length", "month", "permit_type"),
#'     impute = "mean"
#'   )
#'
#'   # Advanced usage: Including distance calculations, auxiliary economic data, 
#'   # and revenue expectations
#'   format_model_data(
#'     project = "WestCoastGroundfish",
#'     name = "ModelData_Spatial",
#'     alt_name = "altname_1",
#'     zone_id = "grid_id",
#'     unique_obs_id = "haul_id",
#'     select_vars = c("vessel_len", "month", "fuel_price"),
#'     aux_data = "FuelCostIndex",
#'     aux_key = "year",
#'     expectations = c("ExpectedRevenue_2023"),
#'     distance = TRUE,
#'     crs = 4326,
#'     impute = "remove"
#'   )
#' }
#'
#' @export
#' @importFrom dplyr %>% filter select all_of mutate rename left_join sym
#' @importFrom tidyr pivot_longer
#' @importFrom DBI dbConnect dbDisconnect dbExecute
#' @importFrom RSQLite SQLite
#' @importFrom stats complete.cases

format_model_data <- function(project, 
                              name, 
                              alt_name, 
                              zone_id, 
                              unique_obs_id,
                              select_vars = NULL,
                              aux_data = NULL, 
                              aux_key = NULL, 
                              gridded_data = NULL, 
                              expectations = NULL, 
                              count_var = NULL,
                              distance = TRUE,
                              distance_units = NULL,
                              impute = NULL,
                              crs = NULL){ 
  
  # Grab the fully evaluated arguments right as the function starts
  settings <- as.list(environment())
  
  # Remove the project and name, as they are metadata, not formatting settings
  settings$project <- NULL
  settings$name <- NULL
  
  # Input argument validation ---------------------------------------------------------------------
  # Check name uniqueness in database
  table_name <- paste0(project, "LongFormatData")
  if (table_exists(table_name, project)) {
    # load data and check names
    tmp_data <- unserialize_table(table_name, project)
    names_tmp_data <- names(tmp_data)
    if (name %in% names_tmp_data) {
      stop(paste0("Formatted data with the name '", name, "' already exists. Enter a new name."))
    }
    rm(tmp_data)
  }
  
  # Check impute method
  if (!is.null(impute) && !(impute %in% c("mean", "median", "mode", "remove"))){
    stop(paste0("Impute method must be one of 'mean', 'median', or 'mode'."))
  }
  
  # Format main data ------------------------------------------------------------------------------
  # Load main data table
  original_dataset <- table_view(paste0(project, "MainDataTable"), project)
  
  # Check unique_obs_id (required for standard logit)
  if (is.null(count_var)) {
    if (!(nrow(unique(original_dataset[unique_obs_id])) == nrow(original_dataset))) {
      stop("The unique_obs_id is not unique for each observation (row). Select a new variable
         or create a new ID variable unique to each observation.")
    }  
  }
  
  # Load alternative choice list
  alt_list_all <- unserialize_table(paste0(project, "AltMatrix"), project)
  
  if (all(!(names(alt_list_all) %in% alt_name))) {
    stop("The 'alt_name' specified does not exist in the 'AltMatrix' table.")
  }
  
  alt_list <- alt_list_all[[which(names(alt_list_all) == alt_name)]]
  
  # Select variables and filter main data
  unique_zones <- unique(alt_list$greaterNZ)
  dataset <- original_dataset %>% filter(!!sym(zone_id) %in% unique_zones)
  
  # Check that select_vars columns are in the dataset and filter
  if (length(select_vars) > 0) {
    column_check(dataset, select_vars) # Check that columns are in the dataset
    select_vars_combined <- c(select_vars, zone_id, unique_obs_id)
    
    # Check if aux_key is in the dataset and add to columns to filter
    if (!is_empty(aux_key) && !(aux_key %in% select_vars_combined)) {
      column_check(dataset, aux_key)
      select_vars_combined <- c(select_vars_combined, aux_key)
    }
    
    dataset <- dataset %>% select(all_of(select_vars_combined))
  }
  
  ## Create a long format data frame for all possible choices ----
  # Data frame of unique zones
  zones_df <- dplyr::tibble(zones = unique_zones)
  
  if (is.null(count_var)) {
    # Standard logit (assumes 1 row = 1 choice)
    df <- dplyr::cross_join(dataset, zones_df) %>%
      mutate(chosen = as.integer(!!sym(zone_id) == zones)) %>%
      mutate(zones = as.character(zones)) %>%
      select(-all_of(zone_id))
    
  } else {
    # Poisson count data (aggregates multi-cell visits dynamically)
    # Extract occasion-level attributes
    occ_vars <- dataset %>%
      select(-all_of(c(zone_id, count_var))) %>%
      distinct(!!sym(unique_obs_id), .keep_all = TRUE)
    
    # Aggregate counts in case a fisher/fleet visited a cell multiple times
    agg_counts <- dataset %>%
      group_by(!!sym(unique_obs_id), !!sym(zone_id)) %>%
      summarize(!!count_var := sum(!!sym(count_var), na.rm = TRUE), .groups = "drop") %>%
      mutate(!!sym(zone_id) := as.character(!!sym(zone_id)))
    
    # Universal grid
    df <- dplyr::cross_join(occ_vars, zones_df) %>%
      mutate(zones = as.character(zones))
    
    # Final join
    join_by_vec <- setNames(c(unique_obs_id, zone_id), c(unique_obs_id, "zones"))
    df <- df %>%
      left_join(agg_counts, by = join_by_vec) %>%
      mutate(!!count_var := tidyr::replace_na(!!sym(count_var), 0))
  }
  
  # Generate distance matrix ----------------------------------------------------------------------
  if (distance) {
    # Check that units are specified
    if (is_empty(distance_units)) {
      stop("Distance units are required when distance = TRUE. Check format_model_data() help 
           documentation for acceptable distance_unit inputs.")
    }
    
    port <- NULL # initialize to NULL if no port included
    tryCatch({
      pt <- data_pull(paste0(project, 'PortTable'), project)
      ptname <- pt$dat # Note: ptname not used 
      port <- pt$dataset # used in create_dist_matrix()
    }, error = function(cond){
      message("Port table not used.")
    })
    
    if(alt_list$alt_var == "nearest point"){
      spatdat <- alt_list$spat
      spatID <- alt_list$spatID
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
                                   units = distance_units,
                                   zoneID = alt_list$zoneID, 
                                   crs = crs)
    
    distance_wide <- as.data.frame(dist_out$dist_matrix)
    dist_row_names <- gsub("^X|\\.\\d+$", "", rownames(distance_wide))
    distance_wide[[unique_obs_id]] <- dist_row_names
    
    distance_long <- distance_wide %>%
      pivot_longer(
        cols = -all_of(unique_obs_id),
        names_to = "zones",
        values_to = "distance")
    
    if (!is.null(count_var)) {
      distance_long <- distance_long %>%
        distinct()
    }
    
    if(!class(df[[unique_obs_id]]) == class(distance_long[[unique_obs_id]])) {
      class(distance_long[[unique_obs_id]]) <- class(df[[unique_obs_id]])
    }
    
    df <- left_join(df, distance_long, by = c("zones", unique_obs_id))
  }
  
  # Reshape and join expectation matrix -----------------------------------------------------------
  # Load expectations
  if(!is.null(expectations) & length(expectations) > 0){
    
    # Error check for loading the ExpectedCatch table
    expect_list <- tryCatch({
      unserialize_table(paste0(project,"ExpectedCatch"), project)
    }, error = function(cond) {
      stop(paste0("The 'ExpectedCatch' table does not exist for project '",
                  project, "'. Please ensure that expected catch matrices have been generated",
                  "before running format_model_data()."), call. = FALSE)
    })
    
    # Error check to ensure requested expectations exist in the loaded list
    missing_exps <- setdiff(expectations, names(expect_list))
    if (length(missing_exps) > 0) {
      stop(paste0("The following expectation(s) were not found in the project database: '",
                  paste(missing_exps, collapse = "', '"),
                  "'. \nAvailable expectations are: '",
                  paste(names(expect_list), collapse = "', '"), "'."), call. = FALSE)
    }
    
    # Filter expectation matrices
    exp_mats <- expect_list[which(names(expect_list) %in% expectations)]
    
    for (i in 1:length(exp_mats)) {
      new_col_name <- expectations[i]
      
      tmp_df <- as.data.frame(exp_mats[[i]]) %>%
        mutate(!!unique_obs_id := dataset[[unique_obs_id]]) %>%
        pivot_longer(cols = -all_of(unique_obs_id),
                     names_to = "zones",
                     values_to = new_col_name)
      
      df <- left_join(df, tmp_df, by = c("zones", unique_obs_id))
    }
  }
  
  # Add aux data ----------------------------------------------------------------------------------
  if (!is_empty(aux_data)) {
    # FIX: Stop execution if aux_data is present but aux_key is missing
    if (is.null(aux_key) || aux_key == "") {
      stop("Auxiliary data was selected, but the join key ('aux_key') is missing.
           Please select a variable to join on.")
    }
    
    # Load aux data and check the aux_key
    aux_df <- table_view(aux_data, project)
    column_check(aux_df, aux_key)
    df <- left_join(df, aux_df, by = aux_key)
  }
  
  # Add gridded data ------------------------------------------------------------------------------
  if (!all(is_empty(gridded_data))) {
    
    # Loop through each table name provided
    for (grid_table in gridded_data) {
      
      # Load the grid table from the database
      gridded_df <- table_view(grid_table, project, convert_dates = FALSE)
      
      # Align the spatial identifier
      if (zone_id %in% names(gridded_df)) {
        gridded_df <- gridded_df %>% rename(zones = !!sym(zone_id))
      }
      
      common_cols <- intersect(names(df), names(gridded_df))  
      
      if (length(common_cols) == 0) {
        stop(paste0("Could not join gridded table '", 
                    grid_table, 
                    "'. No matching column names found. Ensure your spatial",
                    " and temporal variables match."))
      }
      
      # Remove duplicate non-join columns to prevent .x/.y suffixes
      duplicate_vars <- setdiff(intersect(names(df), names(gridded_df)), common_cols)
      if (length(duplicate_vars) > 0) {
        gridded_df <- gridded_df %>% select(-all_of(duplicate_vars))
      }
      
      # Perform automated join
      df <- left_join(df, gridded_df, by = common_cols)
    }
  }
  
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
      zones_to_remove <- df$zones[which(!stats::complete.cases(df))]
      df <- df %>%
        filter(!(zones %in% zones_to_remove))
    }
  }
  
  # Rename columns and save -----------------------------------------------------------------------
  # Rename cols
  df <- df %>%
    rename(!!zone_id := zones)
  
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


