#' K-fold cross validation
#' 
#' @description K-fold cross validation for estimating model performance
#' 
#' @param project Name of project
#' @param mod_name Name of saved model to use. Argument can be the name of the model or can pull 
#'   the name of the saved "best" model. Leave \code{mod_name} empty to use the saved "best" 
#'   model. If more than one model is saved, \code{mod_name} should be the numeric indicator of 
#'   which model to use. Use \code{table_view("modelChosen", project)} to view a table of 
#'   saved models.
#' @param zone_dat String. Variable in primary data table that identifies the individual zones 
#'   or areas.
#' @param groups String. Determines how to subset the dataset into groups for training and 
#'   testing. \code{groups = 'Observations'} randomly splits observations. \code{groups = 'Years'}
#'   splits data by year, which requires the \code{time_var} input to be specified. In addition,
#'   data can be split into groups based on a categorical variable using 
#'   \code{groups = 'variable_name'}.
#' @param k Integer. Value required if \code{groups = 'Observations'} to determine the number 
#'   of groups for splitting data into training and testing datasets. The value of \code{k} 
#'   should be chosen to balance bias and variance and values of \code{k = 5 or 10} have been 
#'   found to be efficient standard values in the literature. Note that higher k values will 
#'   increase runtime and the computational cost of \code{cross_validation}. Leave-on-out cross
#'   validation is a type of k-fold cross validation in which \code{k = n} number of 
#'   observations, which can be useful for small datasets.
#' @param time_var Name of column for time variable. Required if \code{groups = 'Years'}.
#' @param use_scalers Input for \code{create_model_input()}. Logical, should data be normalized? 
#'   Defaults to \code{FALSE}. Rescaling factors are the mean of the numeric vector unless
#'   specified with \code{scaler_func}.
#' @param scaler_func Input for \code{create_model_input()}. Function to calculate 
#'   rescaling factors.
#' 
#' @details K-fold cross validation is a resampling procedure for evaluating the predictive 
#'   performance of a model. First the data are split into k groups, which can be split randomly 
#'   across observations (e.g., 5-fold cross validation where each group is randomly assigned 
#'   across observations) or split based on a particular variable (e.g., split groups based on 
#'   gear type). Each group takes turn being the 'hold-out' or 'test' data set, while the 
#'   remaining groups are the training dataset (parameters are estimated for the training 
#'   dataset). Finally the predictive performance of each iteration is calculated as the percent
#'   absolute prediction error.
#'   
#' @export
#' 
#' @examples
#' \dontrun{
#' model_design_outsample("scallop", "scallopModName")
#' }
#' 
cross_validation <- function(project, 
                             mod_name, 
                             zone_dat, 
                             groups, 
                             k = 5, 
                             time_var = NULL, 
                             use_scalers = FALSE, 
                             scaler_func = NULL){
  
  # Check if grouping by years but a time variable was not specified
  if (groups == "Years" && is_empty(time_var)) {
    stop("Provide time variable to split data into groups by year.")
  }
  
  # Get data
  dat <- table_view(paste0(project,"MainDataTable"), project)
  
  # Need to filter zones that were included in the model
  mdf <- model_design_list(project)
  
  # Get model names
  mdf_n <- model_names(project)
  
  # Get only info for selected model
  flag <- 0
  tryCatch(
    {mdf <- mdf[[which(mdf_n == mod_name)]]},
    error = function(e) {flag <<- 1}
  )
  
  if(flag == 1){
    stop('Model not found.')
  }
  
  zones <- unique(mdf$choice$choice)
  
  dat <- dat[which(dat[[zone_dat]] %in% zones), ]
  
  set.seed(42) # pseudo-random sampling seed for reproducibility
  
  # Split data into k groups ----------------------------------------------------------------------
  if(groups == 'Observations'){ # randomly split data into k groups
    sample <- rep(seq(1:k), times = ceiling(nrow(dat) / k))[1:nrow(dat)] 
    dat$k_groups <- as.factor(sample(sample, size = nrow(dat), replace = FALSE))
    
  } else if (groups == "Years") { # split data by years
    dat$k_groups <- as.factor(format(dat[[time_var]], format = '%Y'))
    
  } else { # split data based on variable
    dat$k_groups <- as.factor(dat[[groups]])
    
  }
  
  # Filter data for each iteration and save as a list
  cv_data <- lapply(levels(dat$k_groups), function(x){
    train_dat <- dat[dat$k_groups != x, ]
    # add name that will be assigned to model design below
    train_dat$k_name <- paste0("group_", x, "_train") 
    test_dat <- dat[dat$k_groups == x, ]
    test_dat$k_name <- paste0("group_", x, "_test")
    
    return(list(train_dat = train_dat, test_dat = test_dat))
  })
  
  # Name list elements
  names(cv_data) <- paste0('group_', as.character(levels(dat$k_groups)))
  
  # Save training and test data as a .rds file
  saveRDS(cv_data, file = paste0(locoutput(project), project, "cv_data.rds")) 
  
  # Remove cross validation model designs if they exist -------------------------------------------
  # First remove any existing cross validation model designs
  del_mods <- model_names(project)[grep("group", model_names(project))]
  if(length(del_mods) > 0){
    remove_model_design(project, del_mods)  
  }
  
  # Create model design for each iteration --------------------------------------------------------
  # Nested apply to create model designs for training and testing dataset for each group
  lapply(cv_data, function(a){ # iterate through groups
    lapply(a, function(b) { # iterate through training and testing datasets
      model_design_outsample(project = project,
                             mod_name = mod_name,
                             outsample_mod_name = b$k_name[1], # assign model design name
                             CV = TRUE,
                             CV_dat = b, # provide train/test dataframe
                             use_scalers = use_scalers,
                             scaler_func = scaler_func)
    })
  })
  
  
  # Estimate parameters for all training datasets -------------------------------------------------
  train_mods <- model_names(project)[grep("train", model_names(project))]
  
  # Clear existing cross validation tables from the sql database
  tables_to_rm <- list_tables(project, "cross valid")
  lapply(tables_to_rm, table_remove, project = project)
  
  # Run estimation model
  discretefish_subroutine(project, run = train_mods, CV = TRUE)
  
  # # Get parameter estimates ---------------------------------------------------------------------
  # filenames <- paste0(locoutput(project), project, "_", train_mods, "_", format(Sys.Date(), format = "%Y-%m-%d"), ".csv")
  # 
  # logitEq <- lapply(filenames, function(x){
  #   read_dat(x, show_col_types = FALSE)
  # })
  # 
  # names(logitEq) <- train_mods
  
  # Predict probabilities for test datasets -------------------------------------------------------
  test_mods <- model_names(project)[grep("test", model_names(project))]
  
  # create list with each element containing the training and testing names
  cv_pred <- lapply(split(cbind(train_mods,test_mods), seq(length(train_mods))), function(x){
    predict_outsample(project, mod_name = x[[1]], outsample_mod_name = x[[2]])
  })
  
  # get the percent absolute prediction error for each group
  cv_performance <- lapply(cv_pred, function(x) return(x[[2]]))
  cv_performance <- data.frame(test_group = names(unlist(cv_performance)), 
                               per_abs_pred_err = unlist(cv_performance, use.names = FALSE)) 
  
  # Merge model output csv files ------------------------------------------------------------------
  cv_files <- list.files(locoutput(project))[grep(paste(train_mods, collapse = "|"), 
                                                  list.files(locoutput(project)))]
  cv_files <- paste0(locoutput(project),cv_files)
  
  cv_mod_output <- lapply(cv_files, function(x){
    out <- read.csv(x)
    file.remove(x)
    return(out)
  })
  
  # add names
  names(cv_mod_output) <- paste0(names(cv_data),"_out")
  
  # Get model fit stats
  cv_mod_fit <- as.data.frame(table_view(paste0(project, "ModelFitCV"), project))
  names(cv_mod_fit) <- paste0(names(cv_data),"_out")
  cv_mod_fit$stat <- c("AIC", "AICc", "BIC", "PseudoR2")
  cv_mod_fit <- cv_mod_fit[,c(length(cv_mod_fit),1:length(cv_mod_fit)-1)]
  
  # Save all outputs in a csv file ----------------------------------------------------------------
  # Remove existing csv file for cross validation
  rm_files <- list.files(locoutput(project))[grep("CrossValidation", 
                                                  list.files(locoutput(project)))]
  rm_files <- paste0(locoutput(project), rm_files)
  file.remove(rm_files)
  
  cv_out_list <- list(cv_performance, cv_mod_fit)
  
  # Write output to a single csv file
  suppressWarnings(lapply(cv_out_list, function(x) {
    write.table(x, paste0(locoutput(project), project, "_CrossValidation", ".csv"), 
                append = TRUE, sep = ",", row.names = FALSE)
    write.table(" ", paste0(locoutput(project), project, "_CrossValidation", ".csv"), 
                append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
  }))
  
  suppressWarnings(lapply(
    seq_along(cv_mod_output), 
    function(x, n, i) {
      names(x[[i]])[1] <- n[[i]]
      write.table(x[[i]], 
                  paste0(locoutput(project), project, "_CrossValidation", ".csv"), 
                  append = TRUE, sep = ",", row.names = FALSE)
      write.table(" ", paste0(locoutput(project), project, "_CrossValidation", ".csv"), 
                  append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
    }, 
    x = cv_mod_output, n = names(cv_mod_output))
  )
  
  # Save outputs as an rds file -------------------------------------------------------------------
  saveRDS(list(cv_performance, cv_mod_fit, cv_mod_output), paste0(locoutput(project), 
                                                                  project, 
                                                                  "CrossValidationOutput.rds"))
  
  # Return percent absolute prediction error ------------------------------------------------------
  return(cv_performance)
}

