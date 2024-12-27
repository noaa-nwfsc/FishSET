shift_sort_x <- function(x, ch, y, distance, alts, ab) {
  #' shift_sort_x
  #'
  #' Shifts choices so that the chosen zone will be automatically the first
  #'     one for choice possibilities and distances.
  #'
  #' @param x Matrix of choice possibilities from \code{\link{create_logit_input}}.
  #' @param ch Data corresponding to actual zonal choice.
  #' @param y Data corresponding to actual catch.
  #' @param distance Data corresponding to distance.
  #' @param alts Number of alternative choices in model.
  #' @param ab Number of cost parameters + number of alts.
  #' @return d: matrix of choice possibilities and distance. IMPORTANT NOTE: both choice probabilities AND distances are sorted even though the 
  #'  column names for distances remain unchanged.
  #' @export
  #'

  ch0 <- ch - 1
  n <- max(dim(ch))
  d <- vector(mode = "list", length = n)
  x <- as.matrix(cbind(x, distance))
  # starts as data.frame
  ch <- as.matrix(ch)
  y <- as.matrix(y)

  
  for (j in 1:n) {
   
    if (ch0[j, ] == 0) {
      
      xsorted <- x[j, , drop = FALSE]
      
    } else {
      
      xj <- x[j, , drop = FALSE]
      xj <- matrix(xj, ab, alts, byrow = TRUE)

      xsorted <- cbind(xj[, ch[j, ]:ncol(xj)], xj[, 1:(ch[j, ] - 1)])
      xsorted <- t(xsorted)
      xsorted <- matrix(xsorted, 1, alts * ab)
    }
    # the catch, chosen location, possible locations, and the distance to each 
    # location for an occasion
    d[[j]] <- cbind(as.matrix(y[j, ]), as.matrix(ch[j, ]), xsorted)
  }
  # note: no variation in first column in distance matrix (always 0), could 
  # cause problem w/ convergence 
  d <- do.call("rbind", d)

  return(d)
}
