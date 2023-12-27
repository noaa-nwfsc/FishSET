#' Reformat out-of-sample model coefficients
#' 
#' Reformat out-of-sample model coefficients by removing zones not included in the out-of-sample dataset
#'
#' @param in_zones Vector of zoneIDs in the in-sample dataset
#' @param out_zones Vector of zoneIDs in the out-of-sample dataset
#' @param Eq Tibble containing estimated model coefficients (including standard errors and t-values)
#' @param likelihood Character, name of the likelihood
#' @return Return a list with (1) vector of coefficients (zones not in the out-of-sample dataset removed) and 
#' (2) flag indicating if the first alt (in-sample dataset) is not included in the out-of-sample dataset.
#' @export

format_outsample_coefs <- function(in_zones, out_zones, Eq, likelihood){
  # Find which zones from original model that are NOT in the out-of-sample model
  inds <- which(!(in_zones %in% out_zones)) # indices for in-sample zones not in out-of-sample zones
  coef_IDs <- in_zones[inds]
  
  # If this is a zonal logit and the first alt (in-sample dataset) is not included in out-of-sample dataset then set flag
  z_flag <- 0
  if((1 %in% inds) & (likelihood == "logit_zonal")) z_flag <- 1
  
  # Remove coefficient for zone not in out-of-sample dataset
  coef_IDs <- paste0(coef_IDs, collapse="|")
  
  # Get index for removal of zoneID
  coef_IDs <- grep(coef_IDs, unlist(Eq[,1], use.names = FALSE))
  
  # If this is a zonal logit the zone could be the first alt, which is already not included in the coefficients
  if(length(coef_IDs) > 0){
    Eq <- Eq$estimate[-coef_IDs]
  } else {
    Eq <- Eq$estimate
  }
  
  return(list(Eq, z_flag))
}