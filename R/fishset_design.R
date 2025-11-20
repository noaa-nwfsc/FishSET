

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
  
  
  
  
  # # Convert data from long to wide matrix format for the model
  # model_matrices <- pivot_to_wide_matrices(
  #   data = df_long,
  #   id_col = "haul_id",
  #   names_from_col = "ZoneID",
  #   values_to_spread = c("chosen", "expected_catch", "distance_from_port")
  # )
  # 
  # Y <- model_matrices$chosen
  # catch <- model_matrices$expected_catch
  # distance <- model_matrices$distance_from_port
  # 
  
  
  
  
  
  
  
  return(NULL)
  
}