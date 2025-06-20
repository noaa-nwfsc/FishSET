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
  # Convert to matrices only if needed
  if (!is.matrix(x)) {
    x_mat <- cbind(x, distance)
  } else {
    x_mat <- cbind(x, distance)
  }
  
  if (!is.matrix(ch)) ch_mat <- as.matrix(ch) else ch_mat <- ch
  if (!is.matrix(y)) y_mat <- as.matrix(y) else y_mat <- y
  
  n <- nrow(ch_mat)
  ch0 <- ch_mat[, 1] - 1L  # Use integer arithmetic and vectorize
  
  # Pre-allocate result matrix with correct dimensions
  result_ncol <- ncol(y_mat) + ncol(ch_mat) + alts * ab
  result <- matrix(0, nrow = n, ncol = result_ncol)
  
  # Efficiently copy y and ch data using direct assignment
  y_cols <- seq_len(ncol(y_mat))
  ch_cols <- seq(ncol(y_mat) + 1L, ncol(y_mat) + ncol(ch_mat))
  
  result[, y_cols] <- y_mat
  result[, ch_cols] <- ch_mat
  
  x_start_col <- ncol(y_mat) + ncol(ch_mat) + 1L
  
  # Handle zero cases efficiently
  zero_mask <- ch0 == 0L
  if (any(zero_mask)) {
    zero_end_col <- x_start_col + ncol(x_mat) - 1L
    result[zero_mask, x_start_col:zero_end_col] <- x_mat[zero_mask, , drop = FALSE]
  }
  
  # Optimize non-zero cases
  non_zero_idx <- which(!zero_mask)
  
  if (length(non_zero_idx) > 0) {
    # Pre-calculate column indices for shifting
    col_indices <- seq_len(alts)
    
    for (j in non_zero_idx) {
      # Reshape more efficiently
      xj <- matrix(x_mat[j, ], nrow = ab, ncol = alts, byrow = TRUE)
      ch_val <- ch0[j] + 1L  # Convert back to 1-based indexing
      
      # Efficient column shifting using modular arithmetic
      if (ch_val <= alts && ch_val > 1L) {
        # Use column reordering instead of cbind
        new_order <- c(ch_val:alts, 1L:(ch_val - 1L))
        xsorted <- xj[, new_order, drop = FALSE]
      } else {
        xsorted <- xj
      }
      
      # More efficient flattening using direct indexing
      xsorted_flat <- as.vector(t(xsorted))
      end_col <- x_start_col + length(xsorted_flat) - 1L
      result[j, x_start_col:end_col] <- xsorted_flat
    }
  }
  
  return(result)
}