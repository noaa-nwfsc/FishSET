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
#' @return d: matrix of choice possibilities and distance. IMPORTANT NOTE: both choice 
#'  probabilities AND distances are sorted even though the column names for distances 
#'  remain unchanged.
#' @export
#'

shift_sort_x <- function(x, ch, y, distance, alts, ab) {
  # Convert inputs to matrices once
  x_mat <- as.matrix(cbind(x, distance))
  ch_mat <- as.matrix(ch)
  y_mat <- as.matrix(y)
  ch0 <- ch_mat - 1
  n <- nrow(ch_mat)

  # Pre-allocate result matrix
  # d <- vector(mode = "list", length = n)
  result_ncol <- ncol(y_mat) + ncol(ch_mat) + alts * ab
  result <- matrix(0, nrow = n, ncol = result_ncol)

  # Fill first columns with y and ch data
  result[, 1:ncol(y_mat)] <- y_mat
  result[, (ncol(y_mat) + 1):(ncol(y_mat) + ncol(ch_mat))] <- ch_mat

  # Process x data efficiently
  x_start_col <- ncol(y_mat) + ncol(ch_mat) + 1

  # Vectorized approach for cases where ch0 == 0
  zero_mask <- ch0[, 1] == 0
  if (any(zero_mask)) {
    zero_rows <- which(zero_mask)
    result[zero_rows, x_start_col:(x_start_col + ncol(x_mat) - 1)] <-
      x_mat[zero_rows, , drop = FALSE]
  }

  # Handle non-zero cases
  non_zero_rows <- which(!zero_mask)
  if (length(non_zero_rows) > 0) {
    for (j in non_zero_rows) {
      xj <- matrix(x_mat[j, ], ab, alts, byrow = TRUE)
      ch_val <- ch_mat[j, 1]  # Assuming single column

      # Efficient column shifting
      if (ch_val <= ncol(xj)) {
        xsorted <- cbind(xj[, ch_val:ncol(xj), drop = FALSE],
                         xj[, 1:(ch_val - 1), drop = FALSE])
      } else {
        xsorted <- xj  # No shift needed if ch_val > ncol
      }

      # Flatten and store
      xsorted_flat <- as.vector(t(xsorted))
      end_col <- x_start_col + length(xsorted_flat) - 1
      result[j, x_start_col:end_col] <- xsorted_flat
    }
  }

  return(result)

}