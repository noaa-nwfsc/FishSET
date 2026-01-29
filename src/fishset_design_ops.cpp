#include <Rcpp.h>
#include <cmath>

using namespace Rcpp;

// Helper to detect binary columns efficiently
bool is_binary_col(double sum, double sum_sq) {
  return std::abs(sum_sq - sum) < 1e-9;
}

// [[Rcpp::export]]
List rcpp_calc_scale_stats_sparse(S4 mat) {
  // Handles dgCMatrix (Sparse Column Compressed)
  // Slots: "i" (row indices), "p" (column pointers), "x" (values), "Dim"
  
  IntegerVector i = mat.slot("i");
  IntegerVector p = mat.slot("p");
  NumericVector x = mat.slot("x");
  IntegerVector dims = mat.slot("Dim");
  
  int n_rows = dims[0];
  int n_cols = dims[1];
  
  NumericVector mus(n_cols);
  NumericVector sds(n_cols);
  
  for (int col = 0; col < n_cols; col++) {
    int start_idx = p[col];
    int end_idx = p[col + 1];
    int nnz = end_idx - start_idx; // Number of non-zeros in this column
    
    // Sum and SumSq of NON-ZERO elements
    double sum_nz = 0;
    double sum_sq_nz = 0;
    
    for (int k = start_idx; k < end_idx; k++) {
      double val = x[k];
      sum_nz += val;
      sum_sq_nz += val * val;
    }
    
    // Total Sum includes the zeros (which add nothing)
    double total_sum = sum_nz;
    double total_sum_sq = sum_sq_nz;
    
    // Calculate Mean
    double mean = total_sum / n_rows;
    
    // Calculate Variance (Moment method: E[X^2] - (E[X])^2)
    // Var = (SumSq - N*Mean^2) / (N-1)
    double var = (total_sum_sq - n_rows * mean * mean) / (n_rows - 1);
    if (var < 0) var = 0;
    double sd = std::sqrt(var);
    
    // Binary Check
    bool is_bin = is_binary_col(total_sum, total_sum_sq);
    
    if (sd == 0 || is_bin) {
      mus[col] = 0;
      sds[col] = 1;
    } else {
      mus[col] = mean;
      sds[col] = sd;
    }
  }
  
  return List::create(Named("mu") = mus, Named("sd") = sds);
}

// [[Rcpp::export]]
SEXP rcpp_apply_scale_sparse(S4 mat, NumericVector mu, NumericVector sd) {
  IntegerVector dims = mat.slot("Dim");
  int n_rows = dims[0];
  int n_cols = dims[1];
  
  // Check if any centering is required
  bool centering_needed = false;
  for(int j=0; j<n_cols; j++) {
    if(mu[j] != 0) {
      centering_needed = true;
      break;
    }
  }
  
  if (centering_needed) {
    // DESTROY SPARSITY: Return Dense Matrix (NumericMatrix)
    // We return this as SEXP, which handles any R object type
    NumericMatrix res(n_rows, n_cols);
    
    // Fill background (zeros became -mu/sd)
    for (int j = 0; j < n_cols; j++) {
      double fill_val = (0.0 - mu[j]) / sd[j];
      if (fill_val != 0) { 
        for (int i = 0; i < n_rows; i++) res(i, j) = fill_val; 
      }
    }
    
    IntegerVector i = mat.slot("i");
    IntegerVector p = mat.slot("p");
    NumericVector x = mat.slot("x");
    
    for (int col = 0; col < n_cols; col++) {
      double m = mu[col];
      double s = sd[col];
      
      for (int k = p[col]; k < p[col+1]; k++) {
        int row = i[k];
        double val = x[k];
        res(row, col) = (val - m) / s;
      }
    }
    
    return res;
    
  } else {
    // PRESERVE SPARSITY: Return S4 Matrix
    S4 res = clone(mat);
    NumericVector x_new = res.slot("x");
    IntegerVector p = res.slot("p");
    
    for (int col = 0; col < n_cols; col++) {
      double s = sd[col];
      if (s == 1) continue; 
      
      for (int k = p[col]; k < p[col+1]; k++) {
        x_new[k] = x_new[k] / s;
      }
    }
    
    return res;
  }
}

// Wrapper to handle dense matrices too (Standard Rcpp)
// [[Rcpp::export]]
List rcpp_calc_scale_stats(SEXP mat) {
  if (Rf_isS4(mat)) {
    return rcpp_calc_scale_stats_sparse(as<S4>(mat));
  } else {
    // Dense Matrix Logic (Original)
    NumericMatrix X(mat);
    int n_rows = X.nrow();
    int n_cols = X.ncol();
    NumericVector mus(n_cols);
    NumericVector sds(n_cols);
    
    for (int j = 0; j < n_cols; j++) {
      double sum = 0;
      double sum_sq = 0;
      int n_valid = 0;
      
      for (int i = 0; i < n_rows; i++) {
        double val = X(i, j);
        if (!NumericVector::is_na(val)) {
          sum += val;
          sum_sq += val * val;
          n_valid++;
        }
      }
      
      if (n_valid == 0) { mus[j]=0; sds[j]=1; continue; }
      
      double mean = sum / n_valid;
      double var = (sum_sq - (sum * sum) / n_valid) / (n_valid - 1);
      if (var < 0) var = 0;
      double sd = std::sqrt(var);
      bool is_bin = is_binary_col(sum, sum_sq);
      
      if (sd == 0 || is_bin) {
        mus[j] = 0; sds[j] = 1;
      } else {
        mus[j] = mean; sds[j] = sd;
      }
    }
    return List::create(Named("mu")=mus, Named("sd")=sds);
  }
}

// [[Rcpp::export]]
SEXP rcpp_apply_scale(SEXP mat, NumericVector mu, NumericVector sd) {
  if (Rf_isS4(mat)) {
    return rcpp_apply_scale_sparse(as<S4>(mat), mu, sd);
  } else {
    // Dense Logic
    NumericMatrix X(mat);
    int n_rows = X.nrow();
    int n_cols = X.ncol();
    NumericMatrix res(n_rows, n_cols);
    
    for (int j = 0; j < n_cols; j++) {
      double m = mu[j];
      double s = sd[j];
      for (int i = 0; i < n_rows; i++) {
        double val = X(i, j);
        if (NumericVector::is_na(val)) res(i, j) = NA_REAL;
        else res(i, j) = (val - m) / s;
      }
    }
    if (X.hasAttribute("dimnames")) res.attr("dimnames") = X.attr("dimnames");
    return res;
  }
}