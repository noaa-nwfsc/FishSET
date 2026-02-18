#include <Rcpp.h>
#include <map>
#include <vector>
#include <string>
#include <algorithm>

using namespace Rcpp;

// Helper to convert Year/Month to an absolute month index (0-based from Year 0)
// e.g., 2023, 1 -> 24276
int get_abs_month(int year, int month) {
  return year * 12 + (month - 1);
}

// [[Rcpp::export]]
NumericMatrix calculate_monthly_avg(IntegerVector unique_years,
                                    IntegerVector unique_months,
                                    CharacterVector unique_groups,
                                    IntegerVector obs_years,
                                    IntegerVector obs_months,
                                    CharacterVector obs_groups,
                                    NumericVector obs_values,
                                    int window_size,
                                    int month_lag,
                                    int year_lag = 0,
                                    bool weighted = false) {
  
  int n_times = unique_years.size();
  int n_groups = unique_groups.size();
  int n_obs = obs_years.size();
  
  // --- Map Setup ---
  // Map "Absolute Month" -> Row Index
  std::map<int, int> time_to_row_map;
  for(int i = 0; i < n_times; ++i) {
    int abs_m = get_abs_month(unique_years[i], unique_months[i]);
    time_to_row_map[abs_m] = i;
  }
  
  // Map "Group Name" -> Column Index
  std::map<String, int> group_to_col_map;
  for(int i = 0; i < n_groups; ++i) {
    group_to_col_map[unique_groups[i]] = i;
  }
  
  // --- Data Aggregation (Handle multiple obs per month/group) ---
  // Initialize matrices
  NumericMatrix values_matrix(n_times, n_groups);
  std::fill(values_matrix.begin(), values_matrix.end(), 0.0);
  
  IntegerMatrix count_matrix(n_times, n_groups);
  std::fill(count_matrix.begin(), count_matrix.end(), 0);
  
  for(int i = 0; i < n_obs; ++i) {
    int abs_m = get_abs_month(obs_years[i], obs_months[i]);
    
    // Safety check: ensure this time/group exists in our definition vectors
    if (time_to_row_map.find(abs_m) != time_to_row_map.end() && 
        group_to_col_map.find(obs_groups[i]) != group_to_col_map.end()) {
      
      int row = time_to_row_map[abs_m];
      int col = group_to_col_map[obs_groups[i]];
      
      if (!NumericVector::is_na(obs_values[i])) {
        values_matrix(row, col) += obs_values[i];
        count_matrix(row, col)++;
      }
    }
  }
  
  // Calculate raw monthly means (normalize the aggregated sums)
  for (int r = 0; r < n_times; ++r) {
    for (int c = 0; c < n_groups; ++c) {
      if (count_matrix(r, c) > 0) {
        values_matrix(r, c) = values_matrix(r, c) / count_matrix(r, c);
      } else {
        values_matrix(r, c) = NA_REAL;
      }
    }
  }
  
  // --- Sliding Window Calculation ---
  
  NumericMatrix result_matrix(n_times, n_groups);
  std::fill(result_matrix.begin(), result_matrix.end(), NA_REAL);
  
  for (int c = 0; c < n_groups; ++c) {
    for (int r = 0; r < n_times; ++r) {
      
      // Identify the anchor time (Current Time - Year Lag)
      int current_abs = get_abs_month(unique_years[r], unique_months[r]);
      int anchor_abs = current_abs - (year_lag * 12);
      
      double sum_product = 0.0;
      double sum_divisor = 0.0;
      
      // Define Window (based on Month Lag)
      // Loop through the offsets for the window size
      for (int w = 0; w < window_size; ++w) {
        int lag_amount = month_lag + w; 
        int target_abs = anchor_abs - lag_amount;
        
        // Check if this specific target month exists in our data
        auto it = time_to_row_map.find(target_abs);
        
        if (it != time_to_row_map.end()) {
          int target_row = it->second;
          double val = values_matrix(target_row, c);
          
          if (!NumericVector::is_na(val)) {
            if (weighted) {
              int weight = count_matrix(target_row, c);
              sum_product += val * weight;
              sum_divisor += weight;
            } else {
              sum_product += val;
              sum_divisor += 1.0;
            }
          }
        }
      }
      
      if (sum_divisor > 0) {
        result_matrix(r, c) = sum_product / sum_divisor;
      }
    }
  }
  
  // Set Names
  colnames(result_matrix) = unique_groups;
  
  return result_matrix;
}