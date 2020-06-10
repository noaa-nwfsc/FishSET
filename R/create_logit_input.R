create_logit_input <- function(choice) {
    # create_logit_input
    #' Creates a data matrix consistent with built-in model forms
    #'
    #' @param choice A (number of observations) x 1 vector of chosen locations
    #' @return dataCompile - a data matrix
    #' @details Called in the discrete_fish_subroutine function
    #' @export
    #'
    
    # x9 <- diag(max(choice)) # makes matrix of choice possibilites x8 <- matrix(diag(max(choice)), 1, max(choice) * max(choice)) x7 <-
    # matrix(rep(diag(max(choice)), each = dim(choice)[1]), nrow = dim(choice)[1])
    
    
    # options(fftempdir = getwd())#ff::ff(
    
    dataCompile <- matrix(rep(diag(max(choice)), each = dim(choice)[1]), nrow = dim(choice)[1])
    
    # dataCompile <- ff::ff(rep(diag(max(choice)), each = dim(choice)[1]), nrow = dim(choice)[1])
    
    return(dataCompile)
    
}
