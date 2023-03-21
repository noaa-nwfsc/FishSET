check_exp <- function(ec, ec_names) {
#' Prepare expected catch matrix
#' 
#' Checks that specified expected catch matrices exists and is formatted correctly
#' when \code{\link{make_model_design}} is called.
#' 
#' @param ec Expected catch list created by \code{\link{create_expectations}}.
#' @param ec_names The names of the expected catch matrices to include in the
#'   model and how they are used (i.e. used together or separately). See the
#'   \code{expectcatchmodels} argument in \code{\link{make_model_design}}.
#' @returns Returns a list containing the filtered expected catch list and specified
#'   matrices to include in model.
#' @details Checks that \code{ec_names} is specified properly by identifying 
#'   invalid options such as combining 'individual' or 'all' with any other option 
#'   or including matrices that don't exist in \code{ec}. 
#' @keywords internal
#' @importFrom rlang is_bare_list
  
  # check for invalid args
  
  if (!rlang::is_bare_list(ec_names)) {
    
    stop("'expectcatchmodels' must be a list.", call. = FALSE)
  }
  
  lapply(ec_names, function(x) {
    
    if (length(x) > 1 && any(c("individual", "all") %in% x)) {
      
      stop("Invalid option for 'expectcatchmodels' argument. 'individual' and 'all' ",
           "cannot be combined with other options.", call. = FALSE)
    }
  })
  
  # drop units and scale from ec list
  ec_choices <- names(ec)[!names(ec) %in% c("scale", "units")]
  ec <- ec[ec_choices]
  
  # check for non-empty ec matrices
  ec_exists <- !vapply(ec, is_value_empty, logical(1))
  ec_exists <- names(ec_exists[ec_exists])
  # remove empty entries
  ec <- ec[ec_exists]
  
  exp_names <- list()
  invd <- FALSE # individual used?
  
  for (i in seq_along(ec_names)) {
    
    ec_len <- length(ec_names[[i]])
    
    if (ec_len == 1 && ec_names[[i]] == "individual") {
      
      y <- i + length(ec_exists) - 1
      invd <- TRUE
      
      exp_names[i:y] <- lapply(ec_exists, function(x) x)
      
      next
      
    } else if (ec_len == 1 && ec_names[[i]] == "all") {
      
      if (invd) {
        
        ii <- length(exp_names) + 1
        exp_names[[ii]] <- ec_exists
        
      } else {
        
        exp_names[[i]] <- ec_exists
      }
      
      next
      
    } else { # specific matrices to include
      
      out <- c()
      
      for (j in seq_along(ec_names[[i]])) {
        
        if (!ec_names[[i]][j] %in% names(ec)) {
          
          ec_dums <- grepl("_dummy", ec_choices)
          ec_exp <- gsub("_\\w+$", "", ec_choices[!ec_dums])
          ec_choices <- c(ec_exp, ec_choices[ec_dums])
          
          stop("Invalid option passed to 'expectcatchmodel'. Please select from the ",
               "following options: ", paste(c("all", "individual", ec_choices), collapse = ", "),
               call. = FALSE)
        }
        
        out <- c(out, ec_names[[i]][j])
      }
      
      if (invd) {
        
        ii <- length(exp_names) + 1
        exp_names[[ii]] <- out
        
      } else {
        
        exp_names[[i]] <- out
      }
    }
  }
  
  # check that specified matrices exist in ec list
  if (any(!unlist(exp_names) %in% c("all", "individual", ec_exists))) {
    
    missing_exp <- unlist(exp_names)
    missing_exp <- !missing_exp %in% c("all", "individual", ec_exists)
    missing_exp <- unique(unlist(exp_names)[missing_exp])
    
    stop("The follow expected catch matrices do not exist: ", 
         paste(missing_exp, collapse = ", "),". Re-run create_expectations() to ",
         "include missing matrices in model or remove from 'expectcatchmodels' argument.", 
         call. = FALSE)
  }
  
  # filter out unused matrices
  if (all(!unlist(ec_names) %in% c("all", "individual"))) {
    
    ec_drop <- ec_exists[!ec_exists %in% unlist(exp_names)]
    
    if (length(ec_drop > 0)) ec <- ec[!ec_exists %in% ec_drop]
  }
  
  list(exp = ec, exp_select = exp_names)
}