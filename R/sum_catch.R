#' Aggregate species catch by unique vessel ID
#' 
#' @description \code{sum_catch} aggregates species catch by vessel ID and will output
#'   a logical vector or table.
#' @param dat Main data frame over which to apply function. Table in FishSET database should 
#'   contain the string `MainDataTable`.
#' @param project Name of project.
#' @param catch Variable name containing catch weight. A vector of variable names can be given 
#'   if data frame is in wide format (i.e. each species has it's own catch weight column). 
#' @param v_id Variable name containing unique vessel ID. 
#' @param species Variable name containing species name if data frame is in long format.
#' @param exp An expression wrapped in quotes which will be evaluated after the summary
#'   table has been created.   
#' @param val Whether to output raw count ("raw") or proportion ("per") of catch. If data frame
#'   is in wide format, the proportion of catch is calculated based on the variables provided
#'   to the \code{catch} argument. 
#' @param output Output; if "logical" function returns a vector of logical values based on 
#'   the \code{exp} argument. This can be used for subsetting tables or assigning values.
#'   "table" returns a summary table. 
#' @importFrom stats aggregate reformulate
#' @importFrom dplyr left_join
#' @export sum_catch
#' @return A summary table or vector of logical values based on the \code{exp} argument.
#' @examples 
#' \dontrun{
#' sum_catch("MainDataTable", "myProject", "LBS_270_POLLOCK_LBS",
#'      "LBS_270_POLLOCK_LBS > 3000", val = "raw", out = "table")
#' sum_catch("MainDataTable", "myProject", "catch", 
#'     "species == 'cod' & catch > .5", val = "per", out = "logical")
#' }
#' 

sum_catch <- function(dat, project, catch, v_id, species = NULL, exp, val = c("raw", "per"), 
                      output = c("logical", "table")){
  
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset
  
  dataset$temp_row_id <- 1:nrow(dataset)
  
  if (is.null(species)) {
    
    ag_sum <- stats::aggregate(dataset[catch], by = list(dataset[[v_id]]), sum)
    names(ag_sum)[1] <- v_id
    
    if (val == "raw") {
      
      ag_sum$value <- ifelse(with(ag_sum, eval(parse(text = exp))), TRUE, FALSE) 
      
      ag_df <- dplyr::left_join(dataset[c("temp_row_id", v_id)], ag_sum, by = v_id)
      
    } else if (val == "per") {
      
      ag_per <- reshape2::melt(ag_sum, id.vars = v_id, variable.name = "species", 
                              value.name = "catch")
      
      ag_per <- stats::aggregate(stats::reformulate(paste(v_id), "catch"), data = ag_per, 
                                 function(x) x/sum(x))
      
      sc <- as.data.frame(ag_per[["catch"]])
      
      colnames(sc) <- catch
      
      ag_per <- cbind(v_id = ag_per[[v_id]], sc)
      
      names(ag_per)[names(ag_per) == "v_id"] <- v_id
      
      ag_per$value <- ifelse(with(ag_per, eval(parse(text = exp))), TRUE, FALSE) 
      
      ag_df <- dplyr::left_join(dataset[c("temp_row_id", v_id)], ag_per, by = v_id)
    }
    
  } else if (!is.null(species)) {
    
    ag_sum <- stats::aggregate(stats::reformulate(paste(v_id, "+", species), catch), 
                               data = dataset, FUN = sum) 
    
    if (val == "raw") {
      
      ag_sum$value <- ifelse(with(ag_sum, eval(parse(text = exp))), TRUE, FALSE) 
      
      ag_df <- dplyr::left_join(dataset[c("temp_row_id", v_id, species)], ag_sum, 
                                by = c(v_id, species))
      
    } else if (val == "per") {
      
      ag_per <- stats::aggregate(stats::reformulate(v_id, catch), data = ag_sum, 
                                 FUN = function(x) x/sum(x))
      
      sc <- as.data.frame(ag_per[[catch]])
      
      colnames(sc) <- unique(ag_sum[[species]])
      
      ag_per <- cbind(v_id = ag_per[[v_id]], sc)
      
      names(ag_per)[names(ag_per) == "v_id"] <- v_id
      
      ag_per <- reshape2::melt(ag_per, id.vars = v_id, variable.name = species, 
                               value.name = catch)
      
      ag_per$value <- ifelse(with(ag_per, eval(parse(text = exp))), TRUE, FALSE) 
      
      ag_df <- dplyr::left_join(dataset[c("temp_row_id", v_id, species)], ag_per, 
                                by = c(v_id, species))
    }
  }
  
  ag_df <- ag_df[order(ag_df$temp_row_id), ]
  
  sum_catch_function <- list()
  sum_catch_function$functionID <- 'sum_catch'
  sum_catch_function$args <- c(dat, project, v_id, catch, species, val, output)
  log_call(sum_catch_function)
  
  if (output == "table") {
    
    if (val == "raw") {
      
      save_table(ag_sum, project, "sum_catch")
      ag_sum
      
    } else if (val == "per") {
      
      save_table(ag_per, project, "sum_catch")
      ag_per
    }
    
  } else if (output == "logical") {
    
    save_table(ag_df, project, "sum_catch")
    ag_df$value
  } 
}