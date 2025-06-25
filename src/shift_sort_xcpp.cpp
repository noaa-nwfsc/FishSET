#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix shift_sort_xcpp(NumericMatrix x, NumericMatrix ch, NumericVector y, 
                              NumericMatrix distance, int alts, int ab) {
  
  int n = ch.nrow();
  int x_ncol = x.ncol();
  int dist_ncol = distance.ncol();
  int ch_ncol = ch.ncol();
  
  // Combine x and distance matrices
  NumericMatrix x_mat(n, x_ncol + dist_ncol);
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < x_ncol; j++) {
      x_mat(i, j) = x(i, j);
    }
    for (int j = 0; j < dist_ncol; j++) {
      x_mat(i, x_ncol + j) = distance(i, j);
    }
  }
  
  // Create ch0 (ch - 1)
  IntegerMatrix ch0(n, ch_ncol);
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < ch_ncol; j++) {
      ch0(i, j) = (int)ch(i, j) - 1;
    }
  }
  
  // Calculate result matrix dimensions (y is single column)
  int result_ncol = 1 + ch_ncol + alts * ab;
  NumericMatrix result(n, result_ncol);
  
  // Fill first column with y data (each row gets its corresponding y value)
  for (int i = 0; i < n; i++) {
    result(i, 0) = y[i];
  }
  
  // Fill next columns with ch data
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < ch_ncol; j++) {
      result(i, 1 + j) = ch(i, j);
    }
  }
  
  // Process x data
  int x_start_col = 1 + ch_ncol;
  int x_mat_ncol = x_mat.ncol();
  
  for (int i = 0; i < n; i++) {
    if (ch0(i, 0) == 0) {
      // Case where ch0 == 0: copy x_mat row directly
      for (int j = 0; j < x_mat_ncol; j++) {
        result(i, x_start_col + j) = x_mat(i, j);
      }
    } else {
      // Non-zero case: need to reshape and shift
      int ch_val = (int)ch(i, 0); // Assuming single column for ch
      
      // Create xj matrix (ab x alts) from x_mat row
      NumericMatrix xj(ab, alts);
      for (int row = 0; row < ab; row++) {
        for (int col = 0; col < alts; col++) {
          xj(row, col) = x_mat(i, row * alts + col);
        }
      }
      
      // Create xsorted matrix with column shifting
      NumericMatrix xsorted(ab, alts);
      if (ch_val <= alts) {
        // Shift columns: move columns [ch_val:alts] to front, then [1:(ch_val-1)]
        for (int row = 0; row < ab; row++) {
          int col_idx = 0;
          // First, copy columns from ch_val to end
          for (int col = ch_val - 1; col < alts; col++) {
            xsorted(row, col_idx) = xj(row, col);
            col_idx++;
          }
          // Then, copy columns from start to ch_val-1
          for (int col = 0; col < ch_val - 1; col++) {
            xsorted(row, col_idx) = xj(row, col);
            col_idx++;
          }
        }
      } else {
        // No shift needed if ch_val > alts
        xsorted = clone(xj);
      }
      
      // Flatten xsorted (by row) and store in result
      int flat_idx = 0;
      for (int row = 0; row < ab; row++) {
        for (int col = 0; col < alts; col++) {
          result(i, x_start_col + flat_idx) = xsorted(row, col);
          flat_idx++;
        }
      }
    }
  }
  
  return result;
}

