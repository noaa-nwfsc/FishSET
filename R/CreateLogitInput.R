#' create_logit_input
#'
#' Creates a data matrix that is consistent with the built in model forms
#'
#' @param choice A (number of observations) x 1 vector of chosen locations
#' @return dataCompile - a data matrix
#' @details Called in zonalSubroutine
# @examples
#

create_logit_input <- function(choice) {
    choice <- as.data.frame(choice)
    x9 <- diag(dim(unique(choice))[1])  # makes matrix of choice possibilites
    # x8 <- matrix(x9, 1,length(unique(choice))*length(unique(choice)))
    x7 <- matrix(rep(x9, each = dim(choice)[1]), nrow = dim(choice)[1])
    
    dataCompile <- x7
    
    return(dataCompile)
    
}
