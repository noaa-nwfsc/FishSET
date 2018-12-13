#' Construct array with accumulation
#'
#' @param subs 
#' @param val 
#' @return an array
#' @details Based on the accumarray function in Matlab. Modified from the pracma package. Called in the createAlternativeChoice function. 

#' @examples 
#' accumarray(C, C)

accumarray <- function (subs, val, sz = NULL, func = sum, fillval = 0) {
     stopifnot(is.numeric(subs), is.numeric(val))
     subs <- floor(subs)
     val <- c(val)
     if (any(subs < 1)) 
          stop("Argument 'subs' must be a matrix of integer indices.")
     matrix_p <- TRUE
     if (is.vector(subs)) {
          subs <- as.matrix(subs)
          matrix_p <- FALSE
     }
     n <- nrow(subs)
     m <- ncol(subs)
     if (length(val) < n) 
          stop("Length of 'vals' must not be smaller than no. of rows of 'subs'.")
     dm <- apply(subs, 2, max)
     if (!is.null(sz)) {
          if (length(sz) != ncol(subs) || any(sz < dm)) 
               stop("Argument 'sz' does not fit with 'subs'.")
          dm <- sz
     }
     if (m == 1) {
          A <- rep(fillval, dm)
          for (i in unique(subs)) {
               A[i] <- func(val[subs == i])
          }
          if (matrix_p) 
               A <- as.matrix(A)
     }
     else {
          cm <- cumprod(dm[1:(m - 1)])
          A <- array(fillval, dim = dm)
          K <- numeric(n)
          for (i in 1:n) {
               K[i] <- subs[i, 1] + sum(cm * (subs[i, 2:m] - 1))
          }
          for (i in unique(K)) {
               A[i] <- func(val[K == i])
          }
     }
     return(A)
}
