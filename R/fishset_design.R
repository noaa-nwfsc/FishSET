#' Create FishSET Model Design Object
#' 
#' Constructs the design object required for discrete choice modeling within the FishSET framework.
#' This function parses the model formula, validates the formatted data, and generates the design
#' matrices (X) and choice vector (y). It handles both alternative-specific variables and 
#' individual-specific variables (automatically creating interactions with zone constants). The 
#' resulting design object is the primary input for the fishset_fit() function, which performs
#' the actual parameter estimation.
#' 
#' 
#' The resulting design object is serialized and stored in the FishSET project database within a
#' table named '[project_name]ModelDesigns'.
#' 
#' @param formula A two-part formula specifying the model structure (e.g., 
#'   \code{chosen ~ catch + distance | income}). The left-hand side specifies the binary choice 
#'   variable, which is always specified as \code{chosen} from the \code{\link{format_model_data}}
#'   function. The right-hand side is separated by a pipe (|):
#'   Part 1 contains alternative-specific variables, and 
#'   Part 2 contains individual-specific variables.
#' @param project Name of the project.
#' @param model_name Name for this specific model design instance. Must be unique within the 
#'   project's design list.
#' @param formatted_data_name Name of the formatted data object to use. This must correspond to a 
#'   name previously created by \code{\link{format_model_data}}.
#' @param unique_obs_id Variable name in the dataset representing the unique observation 
#'   identifier.
#' @param zone_id Variable name in the dataset representing the zone (alternative) identifier.
#' 
#' @return A list object of class 'fishset_design' containing the design matrices, choice vector, 
#'   and metadata. The list is saved to the project database.
#' 
#' @examples
#' \dontrun{
#' # 1. Standard Conditional Logit
#' # "chosen" is the response, "expected_catch" and "distance" are site attributes.
#' fishset_design(
#'   formula = chosen ~ expected_catch + distance,
#'   project = "MyProject",
#'   model_name = "clogit_model",
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
#'   model_name = "zonal_logit_model",
#'   formatted_data_name = "my_formatted_data",
#'   unique_obs_id = "haul_id",
#'   zone_id = "zone_id"
#' ) 
#' }
#' 
#' @export
#' @importFrom Formula Formula model.part
#' @importFrom stats as.formula model.matrix
#' @importFrom DBI dbConnect dbDisconnect dbExecute
#' @importFrom RSQLite SQLite

fishset_design <- function(formula,
                           project,
                           model_name,
                           formatted_data_name,
                           unique_obs_id,
                           zone_id,
                           catch_formula = NULL,
                           price_var = NULL){
  
  # Load data -------------------------------------------------------------------------------------
  tryCatch({
    full_lf_list <- unserialize_table(paste0(project,"LongFormatData"), project)  
  }, error = function(cond){
    message("Not able to load formatted data. Run format_model_data() prior to fishset_design().")
  })
  
  # Check that formatted data name exists
  if (!(formatted_data_name %in% names(full_lf_list))) {
    stop(paste0("Formatted data name not found in ",
                project,
                " database. Run format_model_data() first, or check formatted data names."))
  } 
  
  # Extract the specific dataframe
  data <- full_lf_list[[which(names(full_lf_list) == formatted_data_name)]]
  
  # Validation and sorting ------------------------------------------------------------------------
  if (!all(c(unique_obs_id, zone_id) %in% names(data))) {
    stop("Specified 'unique_obs_id' or 'zone_id' columns not found in the dataset.")
  }
  
  # Ensure data is ordered by observation, then by zone
  data <- data[order(data[[unique_obs_id]], data[[zone_id]]),]
  
  # Check if design file exists
  design_names <- model_design_list(project)
  if (model_name %in% design_names) {
    stop(paste0("Model design ", model_name, "already exists. Enter a new model name or ",
                "delete the old model design using remove_model_design()."))
  }
  
  # Formula parsing -------------------------------------------------------------------------------
  if (!inherits(formula, "formula")) {
    formula <- as.formula(formula)
  }
  
  # Use Formula package to handle multi-part formulas (y ~ x1 | x2)
  F_formula <- Formula::Formula(formula)
  
  # Create Y (chosen) -----------------------------------------------------------------------------
  y_frame <- Formula::model.part(F_formula, data = data, lhs = 1)
  y <- as.numeric(y_frame[[1]])
  
  # Validate Y is binary
  if (!all(y %in% c(0, 1))) {
    stop("The choice variable (LHS of formula) must be binary (0/1).")
  }
  
  # Create X matrices -----------------------------------------------------------------------------
  X1 <- model.matrix(F_formula, data = data, rhs = 1)
  
  # Remove intercept if present (standard for conditional logit)
  if ("(Intercept)" %in% colnames(X1)) {
    X1 <- X1[, -which(colnames(X1) == "(Intercept)"), drop = FALSE]
  }
  
  # Individual-specific variables
  has_part_2 <- length(F_formula)[2] > 1
  
  if (has_part_2) {
    X2_raw <- model.matrix(F_formula, data = data, rhs = 2)
    
    # Remove intercept from raw X2
    if ("(Intercept)" %in% colnames(X2_raw)) {
      X2_raw <- X2_raw[, -which(colnames(X2_raw) == "(Intercept)"), drop = FALSE]
    }
    
    # Create Interactions: X2 variables * Zone Dummies
    # This mathematically allows individual traits to influence specific site selection.
    X2_df <- as.data.frame(X2_raw)
    X2_df$zone_factor <- as.factor(data[[zone_id]])
    
    # Construct the interaction formula dynamically using the actual column names
    interact_vars <- colnames(X2_raw)
    interact_formula_str <- paste("~ (", 
                                  paste(interact_vars, collapse = " + "), 
                                  ") : zone_factor - 1")
    interact_formula <- as.formula(interact_formula_str)
    
    # The formula `~ X2_raw : zone_factor - 1` creates interaction columns
    # We use -1 to avoid a global intercept.
    # Note: This creates interactions for ALL zones. In estimation, one base zone
    # is usually dropped or constrained, but for the design matrix, we often keep all 
    # and handle identification in the fit function or TMB code.
    X2_interacted <- model.matrix(interact_formula, data = X2_df)
    
    # Clean up column names (optional, makes summary cleaner)
    # e.g., changes "X2_rawIncome:zone_factorZoneA" to "Income:ZoneA"
    colnames(X2_interacted) <- gsub("zone_factor", "", colnames(X2_interacted))
    
    # Combine Part 1 and Part 2
    X_final <- cbind(X1, X2_interacted)
    
  } else {
    # If no individual vars, X is just Part 1
    X_final <- X1
  }
  
  # Process EPM components ------------------------------------------------------------------------
  epm_components <- list()
  
  if (!is.null(catch_formula)) {
    if (is.null(price_var)) stop(paste("If 'catch_formula' is provided, 'price_var' must also be",
                                       "specified"))
    if (!(price_var %in% names(data))) stop(paste("Price variable '", price_var, 
                                                  "' not found in data."))
    
    # Parse catch formula
    if (!inherits(catch_formula, "formula")) {
      catch_formula <- as.formula(catch_formula)
    }
    
    # Validation check: ensure catch predictors are in the main formula
    catch_rhs_vars <- all.vars(catch_formula[[3]])
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
    tmp_catch <- model.frame(catch_formula, data = data, na.action = na.pass)
    Y_catch <- model.response(tmp_catch)
    
    # Extract catch predictors
    X_catch <- model.matrix(catch_formula, data = tmp_catch)
    if ("(Intercept)" %in% colnames(X_catch)) {
      X_catch <- X_catch[, -which(colnames(X_catch) == "(Intercept)"), drop = FALSE]
    }
    
    # Extract price vector
    price_vec <- data[[price_var]]
    
    epm_components <- list(
      Y_catch = Y_catch,
      X_catch = X_catch,
      price_vec = price_vec,
      is_epm = TRUE
    )
    
  } else {
    epm_components <- list(is_epm = FALSE)
  }
  
  # Package results -------------------------------------------------------------------------------
  design_obj <- list(
    y = y,
    X = X_final,
    formula = F_formula,
    epm = epm_components,
    # Metadata used by the fit function
    settings = list(
      project = project,
      model_name = model_name,
      formatted_data_name = formatted_data_name,
      unique_obs_id = unique_obs_id,
      zone_id = zone_id,
      # Dimensions
      N_obs = length(unique(data[[unique_obs_id]])), # Number of choices made
      J_alts = length(unique(data[[zone_id]])), # Number of alternatives
      K_vars = ncol(X_final) # Number of parameters
    ),
    # Store ids for post-estimation/prediction
    ids = list(
      obs = data[[unique_obs_id]],
      zone = data[[zone_id]]
    )
  )
  
  class(design_obj) <- "fishset_design"
  
  # Save to database ------------------------------------------------------------------------------
  # Hybrid storage
  db_path <- locdatabase(project)
  project_dir <- dirname(db_path)
  designs_dir <- file.path(project_dir, "ModelDesigns")
  
  # Create a new ModelDesigns folder in the project folder if it doesn't exist yet
  if (!dir.exists(designs_dir)) {
    dir.create(designs_dir, recursive = TRUE)
  }
  
  # Save the heavy object to disk (.rds)
  file_name <- paste0(model_name, ".rds")
  file_path <- file.path(designs_dir, file_name)
  
  saveRDS(design_obj, file = file_path, compress = "gzip")
  message("Design object saved to: ", file_path)
  
  # Log the function call -------------------------------------------------------------------------
  fishset_design_function <- list()
  fishset_design_function$functionID <- "fishset_design"
  fishset_design_function$args <- as.list(match.call())[-1]
  fishset_design_function$kwargs <- list()
  
  log_call(project, fishset_design_function)
}