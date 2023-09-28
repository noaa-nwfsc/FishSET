# create_logit_input
create_logit_input <- function(choice) {
  #' Creates a data matrix consistent with built-in model forms
  #'
  #' @param choice A dataframe with a single vector of chosen locations with length = number of observations
  #' @return \code{dataCompile}: A large data matrix.Number of rows = the number of observations, number of cols = square of number of alternatives. 
  #'    Each row contains a flattened identity matrix of size = number of alternatives x number of alternatives.
  #' @details Called in the discrete_fish_subroutine function
  #' @export
  #' @keywords internal

  # x9 <- diag(max(choice)) # makes matrix of choice possibilites 
  #x8 <- matrix(diag(max(choice)), 1, max(choice) * max(choice)) 
  #x7 <- matrix(rep(diag(max(choice)), each = dim(choice)[1]), nrow = dim(choice)[1])

  # options(fftempdir = getwd())#ff::ff(

  dataCompile <- matrix(rep(diag(max(choice)), each = nrow(choice)), nrow = nrow(choice))

  # dataCompile <- ff::ff(rep(diag(max(choice)), each = dim(choice)[1]), nrow = dim(choice)[1])

  return(dataCompile)
}
