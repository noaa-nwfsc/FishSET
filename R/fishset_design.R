#' Create FishSET Model Design Object
#' 
#' Constructs the design object required for discrete choice model fitting within the FishSET 
#' framework. This function parses the model formula, validates the formatted data, and generates 
#' the design matrix (X) and choice vector (y). It handles both alternative-specific variables and 
#' trip- or haul-specific variables (automatically creating interactions with zone constants). The 
#' resulting design object is the primary input for the \code{\link{fishset_fit}} function, 
#' which performs the parameter estimation.
#' 
#' The resulting design object is saved as a compressed .rds file in the 'ModelDesigns' folder,
#' which is located inside the project folder.
#' 
#' @param formula A two-part formula specifying the model structure (e.g., 
#'   \code{chosen ~ expected_catch + distance | income}). The left-hand side specifies the 
#'   binary choice variable, which is always specified as \code{chosen} from the 
#'   \code{\link{format_model_data}} function. The right-hand side is separated by a pipe (|):
#'   Part 1 contains alternative-specific variables, and 
#'   Part 2 contains trip- or haul-specific variables (i.e., do not vary across fishing zones).
#' @param project Name of the project.
#' @param model_name Name for this specific model design instance. Must be unique within the 
#'   project's design list.
#' @param formatted_data_name Name of the formatted data object to use. This must correspond to a 
#'   name previously created by \code{\link{format_model_data}}.
#' @param unique_obs_id Variable name in the dataset representing the unique observation 
#'   identifier.
#' @param zone_id Variable name in the dataset representing the zone (alternative) identifier.
#' @param catch_formula A formula specifying the expected catch for an Expected Profit Model.
#' @param price_var Variable name in the dataset representing price. This input is only used for
#'   Expected Profit Models, and the price variable must be included in the formatted dataset 
#'   created in the \code{\link{format_model_data}} function.
#' @param model_type Model type: default is \code{"logit"} and will be used in most cases. Set
#'   to \code{"poisson"} if designing a Poisson-equivalence model.
#' @param scale Logical. Default = FALSE. If TRUE, numeric predictors in the design matrix (X) 
#'   are centered and scaled (z-score normalization) before saving. Scaling factors are stored to 
#'   allow unscaling of parameters after estimation. Recommended for numerical stability.
#' 
#' @return A list object of class 'fishset_design' containing the design matrices, choice vector, 
#'   and metadata. The list is saved as a compressed .rds file in the project folder.
#' 
#' @examples
#' \dontrun{
#' # 1. Standard Conditional Logit
#' # "chosen" is ALWAYS the response, "expected_catch" and "distance" are site attributes.
#' fishset_design(
#'   formula = chosen ~ expected_catch + distance,
#'   project = "MyProject",
#'   model_name = "clogit_model1",
#'   formatted_data_name = "my_formatted_data",
#'   unique_obs_id = "haul_id",
#'   zone_id = "zone_id"
#' )
#' 
#' # 2. Zonal Logit with Alternative-Specific Constants (ASCs)
#' # Add the zone_id variable ("zone_id") to the formula to generate fixed effects.
#' fishset_design(
#'   formula = chosen ~ expected_catch + distance + zone_id,
#'   project = "MyProject",
#'   model_name = "zonal_logit_model1",
#'   formatted_data_name = "my_formatted_data",
#'   unique_obs_id = "haul_id",
#'   zone_id = "zone_id",
#'   scale = TRUE
#' ) 
#'
#' # 3. Zonal Logit with trip- or haul-specific variables
#' # Add the variable that does not vary across zones (e.g., vessel length) after the pipe ("|").
#' fishset_design(
#'   formula = chosen ~ expected_catch + distance | vessel_length,
#'   project = "MyProject",
#'   model_name = "zonal_logit_model2",
#'   formatted_data_name = "my_formatted_data",
#'   unique_obs_id = "haul_id",
#'   zone_id = "zone_id",
#'   scale = TRUE
#' )
#' 
#' # 4. Expected profit model - normal distribution
#' # The expected catch (catch_var in this example) does not vary across zones in this 
#' # example (e.g., vessel length).
#' fishset_design(
#'   formula = chosen ~ distance | catch_var,
#'   project = "MyProject",
#'   model_name = "epm1",
#'   formatted_data_name = "my_formatted_data",
#'   unique_obs_id = "haul_id",
#'   zone_id = "ZoneID",
#'   catch_formula = actual_catch ~ catch_var:ZoneID,
#'   price_var = "price_var",
#'   scale = TRUE
#' ) 
#' }
#' 
#' @export
#' @importFrom Formula Formula model.part
#' @importFrom stats as.formula model.matrix
#' @importFrom DBI dbConnect dbDisconnect dbExecute
#' @importFrom RSQLite SQLite
#' @importFrom data.table setDT setorderv setDF
#' @importFrom Matrix sparse.model.matrix
#' @importFrom methods as

fishset_design <- function(formula,
                           project,
                           model_name,
                           formatted_data_name,
                           unique_obs_id,
                           zone_id,
                           catch_formula = NULL,
                           price_var = NULL,
                           model_type = "logit",
                           scale = FALSE){
  
  # Setup and validate data -----------------------------------------------------------------------
  # Check if design file exists
  design_names <- tryCatch({
     model_design_list(project)
  }, error = function(cond) {
    list()
  })
  
  if (model_name %in% design_names) {
    stop(paste0("Model design ", model_name, " already exists. Enter a new model name or ",
                "delete the old model design using remove_model_design()."))
  }
  
  # Load formatted data table
  tryCatch({
    full_lf_list <- unserialize_table(paste0(project,"LongFormatData"), project)  
  }, error = function(cond){
    message("Not able to load formatted data. Run format_model_data() prior to fishset_design().")
  })
  
  # Check that formatted data name exists
  if (!(formatted_data_name %in% names(full_lf_list))) {
    stop(paste0("Formatted data name not found in ", project,
                " database. Run format_model_data() first, or check formatted data names."))
  } 
  
  # Extract the specific dataframe
  data <- full_lf_list[[which(names(full_lf_list) == formatted_data_name)]]
  
  # Check that input args are available in data
  if (!all(c(unique_obs_id, zone_id) %in% names(data))) {
    stop("Specified 'unique_obs_id' or 'zone_id' columns not found in the dataset.")
  }
  
  # Use qs2 for saving if available - this will speed up the function
  use_qs2 <- requireNamespace("qs2", quietly = TRUE)
  
  # Sorting ---------------------------------------------------------------------------------------
  # Ensure data is ordered by observation, then by zone
  data.table::setDT(data) # Convert to data.table by reference
  data.table::setorderv(data, c(unique_obs_id, zone_id)) # Sort by reference
  data.table::setDF(data) # Convert back to data.frame
  
  # Force zone_id to a factor for correct dummy generation in formula
  data[[zone_id]] <- as.factor(data[[zone_id]])
  J_alts <- length(levels(data[[zone_id]]))
  
  # Formula parsing -------------------------------------------------------------------------------
  if (!inherits(formula, "formula")) formula <- as.formula(formula)
  F_formula <- Formula::Formula(formula) # Use Formula package to handle multi-part formulas
  
  # Create Y (chosen or count) --------------------------------------------------------------------
  y_frame <- Formula::model.part(F_formula, data = data, lhs = 1)
  y <- as.numeric(y_frame[[1]])
  
  # Validate Y is binary
  if (model_type == "logit" && !all(y %in% c(0, 1))) {
    stop("The choice variable (LHS of formula) must be binary (0/1).")
  } else if (model_type == "poisson" && any(y < 0)) {
    stop("The choice variable must be non-negative integers for Poisson equivalence.")
  }
  
  # Create X matrices (discrete) ------------------------------------------------------------------
  # Initialize scalers list
  scalers <- list()
  
  # Process matrix helper function
  process_matrix <- function(f_str, data_source, do_scale, scale_name) {
    if (is.null(f_str)) return(NULL)
    
    # Check variables exist in data_source
    req_vars <- all.vars(as.formula(f_str))
    missing_vars <- setdiff(req_vars, names(data_source))
    if (length(missing_vars) > 0) {
      stop(paste0(
        "Model design failed. The following variable(s) specified in the formula are missing",
        " from the formatted data: '",
        paste(missing_vars, collapse = "', '"),
        "'. Check your formula or your format_model_data() output."
      ), call. = FALSE)
    }
    
    # Generate sparse matrix
    mat <- Matrix::sparse.model.matrix(as.formula(f_str), data = data_source)
    # Drop intercept
    if ("(Intercept)" %in% colnames(mat)) {
      mat <- mat[, -which(colnames(mat) == "(Intercept)"), drop = FALSE]
    }
    # Scale
    if (do_scale && ncol(mat) > 0) {
      # Scale calcs (mean and standard deviation) - handles sparse and dense matrices
      s <- rcpp_calc_scale_stats(mat)
      names(s$mu) <- colnames(mat)
      names(s$sd) <- colnames(mat)
      # Apply scale
      mat <- rcpp_apply_scale(mat, s$mu, s$sd)
      # Save scalers to list
      scalers[[scale_name]] <<- s
      # Ensure dgCMatrix
      if (!inherits(mat, "Matrix")) mat <- methods::as(mat, "dgCMatrix")
    }
    return(mat)
  }
  
  ### Formula part 1 ###
  rhs1_vars <- attr(terms(F_formula, lhs = 0, rhs = 1), "term.labels")
  if (length(rhs1_vars) == 0) {
    X1 <- NULL
  } else {
    f1_str <- paste("~", paste(rhs1_vars, collapse = " + "))
    X1 <- process_matrix(f1_str, data, scale, "X1")
  }
  
  ### Formula part 2 ###
  # Trip- or haul-specific variables
  has_part_2 <- length(F_formula)[2] > 1
  
  if (!has_part_2) {
    # If no zone-specific vars, X is just Part 1
    X_final <- X1
    
  } else {
    rhs2_vars <- attr(terms(F_formula, lhs = 0, rhs = 2), "term.labels")
    f2_str <- paste("~", paste(rhs2_vars, collapse = " + "))
    
    # Get base matrix (scaled)
    X2_base <- process_matrix(f2_str, data, scale, "X2")
    
    # Zone interaction
    zone_int <- as.integer(data[[zone_id]])
    X2_interacted <- rcpp_sparse_interaction(X2_base, zone_int, J_alts)
    
    # Fix names
    var_names <- rhs2_vars
    zone_names <- levels(data[[zone_id]])[-1] # Drop ref (zone 1)
    int_names <- as.vector(outer(zone_names, var_names, function(z, v) paste0(v, ":", zone_id, z)))
    colnames(X2_interacted) <- int_names
    
    # Combine
    if (is.null(X1)) {
      X_final <- X2_interacted
    } else {
      X_final <- cbind(X1, X2_interacted)
    }
  }
  
  # Process EPM components ------------------------------------------------------------------------
  epm_components <- list()
  if (!is.null(catch_formula)) {
    if (is.null(price_var)) stop(paste("If 'catch_formula' is provided, 'price_var' must also be",
                                       "specified"))
    if (!(price_var %in% names(data))) stop(paste("Price variable '", price_var,
                                                  "' not found in data."))
    
    # Parse catch formula
    if (!inherits(catch_formula, "formula")) catch_formula <- as.formula(catch_formula)
    catch_formula <- Formula::Formula(catch_formula) # Handle multi-part formulas
    
    # Validation check: ensure catch predictors are in the main formula
    catch_rhs_vars <- all.vars(catch_formula[[3]]) 
    catch_rhs_vars <- catch_rhs_vars[which(catch_rhs_vars!= zone_id)]
    # Extract RHS from utility formula
    util_rhs_vars <- all.vars(formula[[3]])
    
    # Check for missing variables
    missing_vars <- setdiff(catch_rhs_vars, util_rhs_vars)
    if (length(missing_vars) > 0) {
      stop(paste("EPM Validation Error: The following predictor(s) in 'catch_formula' are missing",
                 "from the main 'formula': ", paste(missing_vars, collapse = ", "),
                 ". \nIn EPMs, the expected catch component must be included in both formulas."))
    }
    
    # Extract continuous response (actual catch)
    tmp_catch <- stats::model.frame(catch_formula, data = data, na.action = na.pass)
    Y_catch <- stats::model.response(tmp_catch)
    
    # Extract catch predictors part 1
    rhs1_vars <- attr(stats::terms(catch_formula, lhs = 0, rhs = 1), "term.labels")
    if (length(rhs1_vars) == 0) {
      X1_catch <- NULL
    } else {
      f1_str <- paste("~", paste(rhs1_vars, collapse = " + "))
      X1_catch <- process_matrix(f1_str, data, scale, "X1_catch")
    }
    
    if ("(Intercept)" %in% colnames(X1_catch)) {
      X1_catch <- X1_catch[, -which(colnames(X1_catch) == "(Intercept)"), drop = FALSE]
    }
    
    # Trip- or haul-specific variables
    has_part_2 <- length(catch_formula)[2] > 1
    
    if (!has_part_2) {
      # If no zone-specific vars, X is just Part 1
      X_catch_final <- X1_catch
      
    } else {
      rhs2_vars <- attr(terms(catch_formula, lhs = 0, rhs = 2), "term.labels")
      f2_str <- paste("~", paste(rhs2_vars, collapse = " + "))
      
      # Get base matrix (scaled)
      X2_catch_base <- process_matrix(f2_str, data, scale, "X2_catch")
      
      # Zone interaction
      zone_int <- as.integer(data[[zone_id]])
      X2_catch_interacted <- rcpp_sparse_interaction(X2_catch_base, zone_int, J_alts)
      
      # Fix names
      var_names <- rhs2_vars
      zone_names <- levels(data[[zone_id]])[-1] # Drop ref (zone 1)
      int_names <- as.vector(outer(zone_names, var_names, function(z, v) paste0(v, ":Zone", z)))
      colnames(X2_catch_interacted) <- int_names
      
      # Combine
      if (is.null(X1_catch)) {
        X_catch_final <- X2_catch_interacted
      } else {
        X_catch_final <- cbind(X1_catch, X2_catch_interacted)
      }
    }
    
    # Extract price vector
    price_vec <- data[[price_var]]
    
    epm_components <- list(
      Y_catch = Y_catch,
      X_catch = X_catch_final,
      price_vec = price_vec,
      is_epm = TRUE
    )
    
  } else {
    epm_components <- list(is_epm = FALSE)
  }
  
  # Package results -------------------------------------------------------------------------------
  # Create 0-indexed occasion IDs for RTMB
  occ_factor <- as.factor(data[[unique_obs_id]])
  occ_id <- as.numeric(occ_factor) - 1
  
  design_obj <- list(
    y = y,
    X = X_final,
    formula = F_formula,
    epm = epm_components,
    scalers = scalers,
    # Metadata used by the fit function
    settings = list(
      project = project,
      model_name = model_name,
      formatted_data_name = formatted_data_name,
      unique_obs_id = unique_obs_id,
      zone_id = zone_id,
      model_type = model_type,
      # Dimensions
      N_obs = length(unique(data[[unique_obs_id]])), # Number of choices made
      J_alts = length(unique(data[[zone_id]])), # Number of alternatives
      K_vars = ncol(X_final) # Number of parameters
    ),
    # Store ids for post-estimation/prediction
    ids = list(
      obs = data[[unique_obs_id]],
      zone = data[[zone_id]],
      occ_id = occ_id
    )
  )
  
  class(design_obj) <- "fishset_design"
  
  # Save to design folder -------------------------------------------------------------------------
  db_path <- locdatabase(project)
  project_dir <- dirname(db_path)
  designs_dir <- file.path(project_dir, "ModelDesigns")
  
  # Create a new ModelDesigns folder in the project folder if it doesn't exist yet
  if (!dir.exists(designs_dir)) dir.create(designs_dir, recursive = TRUE)
  
  # SOFT DEPENDENCY LOGIC for qs2
  if (use_qs2) {
    # Recommended: Use distinct extension so your reader knows to use qread
    file_name <- paste0(model_name, ".qs2")
    qs2::qs_save(design_obj, file = file.path(designs_dir, file_name))
  } else {
    # Fallback to standard RDS
    file_name <- paste0(model_name, ".rds")
    saveRDS(design_obj, file = file.path(designs_dir, file_name), compress = FALSE)
  }
  message("Design object saved to: ", file.path(designs_dir, file_name))

  # Log the function call -------------------------------------------------------------------------
  fishset_design_function <- list()
  fishset_design_function$functionID <- "fishset_design"
  fishset_design_function$args <- as.list(match.call())[-1]
  fishset_design_function$kwargs <- list()
  
  log_call(project, fishset_design_function)
}