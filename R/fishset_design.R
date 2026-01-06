#' Create FishSET Model Design Object
#' 
#' Constructs the design object required for discrete choice modeling within the FishSET framework.
#' This function parses the model formula, validates the formatted data, and generates the design
#' matrices (X) and choice vector (y). It handles both alternative-specific variables and 
#' individual-specific variables (automatically creating interactions with zone constants). The 
#' resulting design object is the primary input for the fishset_fit() function, which performs
#' the actual parameter estimation.
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
#'   project = ""MyProject",
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
#'   project = ""MyProject",
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
                           zone_id){
  
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
  
  # Package results -------------------------------------------------------------------------------
  design_obj <- list(
    y = y,
    X = X_final,
    formula = F_formula,
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
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  # Save this into a new table or append to a designs list
  table_name <- paste0(project, "ModelDesigns")
  
  # Create a named list wrapper
  design_wrapper <- list()
  design_wrapper[[model_name]] <- design_obj
  
  # Append or create a new table
  if (table_exists(table_name, project)) {
    existing_designs <- unserialize_table(table_name, project)
    # Remove if exists to overwrite
    if (model_name %in% names(existing_designs)) {
      existing_designs[[model_name]] <- NULL
    }
    table_remove(table_name, project)
    design_wrapper <- c(existing_designs, design_wrapper)
  }
  
  DBI::dbExecute(fishset_db, 
                 paste("CREATE TABLE IF NOT EXISTS", 
                       table_name, 
                       "(data design_wrapper)"))
  DBI::dbExecute(fishset_db, 
                 paste("INSERT INTO", 
                       table_name, 
                       "VALUES (:data)"),
                 params = list(data = list(serialize(design_wrapper, NULL))))
  
  # Log the function call -------------------------------------------------------------------------
  fishset_design_function <- list()
  fishset_design_function$functionID <- "fishset_design"
  fishset_design_function$args <- as.list(match.call())[-1]
  fishset_design_function$kwargs <- list()
  
  log_call(project, fishset_design_function)
  
  return(design_obj)
}