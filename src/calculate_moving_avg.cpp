#include <Rcpp.h>
#include <map>
#include <vector>
#include <iostream> // Required for Rcpp::Rcout
#include <string> 
#include <algorithm> // Required for std::lower_bound
#include <iterator>  // Required for std::distance
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector calculate_moving_avg(DateVector unique_dates, 
                                   CharacterVector unique_groups, 
                                   DateVector obs_dates,
                                   CharacterVector obs_groups,
                                   NumericVector obs_values, 
                                   int window_size, 
                                   int lag,
                                   int year_lag = 0,
                                   bool temporal = true,
                                   bool weighted = false) {
  
  int n_dates = unique_dates.size();
  int n_groups = unique_groups.size();
  int n_trips = obs_dates.size();
  
  // Create a dense matrix of values (dates x groups)
  // This helps handle missing dates explicitly by filling with NA.
  
  // Create maps for quick lookups of date/zone to matrix indices.
  // This is much faster than searching through vectors repeatedly.
  std::map<int, int> date_to_row_map;
  for(int i = 0; i < n_dates; ++i) {
    date_to_row_map[unique_dates[i]] = i;
  }
  
  std::map<String, int> zone_to_col_map;
  for(int i = 0; i < n_groups; ++i) {
    zone_to_col_map[unique_groups[i]] = i;
  }
  
  // Initialize values_matrix to 0.0 to use it for summing.
  NumericMatrix values_matrix(n_dates, n_groups);
  std::fill(values_matrix.begin(), values_matrix.end(), 0.0);
  
  // Create a second matrix to store the count of observations.
  IntegerMatrix count_matrix(n_dates, n_groups);
  std::fill(count_matrix.begin(), count_matrix.end(), 0);
  
  // Populate both the sum (values_matrix) and count (count_matrix).
  for(int i = 0; i < n_trips; ++i) {
    int row = date_to_row_map[obs_dates[i]];
    int col = zone_to_col_map[obs_groups[i]];
    
    values_matrix(row, col) += obs_values[i]; // Add to sum
    count_matrix(row, col)++;                 // Increment count
  }
  
  // Calculate the mean for each day and set days with no data to NA.
  for (int r = 0; r < n_dates; ++r) {
    for (int c = 0; c < n_groups; ++c) {
      if (count_matrix(r, c) > 0) {
        // If there were observations, divide sum by count to get the mean.
        values_matrix(r, c) = values_matrix(r, c) / count_matrix(r, c);
      } else {
        // If there were no observations, set the value to NA.
        values_matrix(r, c) = NA_REAL;
      }
    }
  }
  
  // Sequential window for averaging
  std::map<int, std::vector<int>> obs_indices_by_group;
  if (!temporal) {
    for (int c = 0; c < n_groups; ++c) {
      for (int r = 0; r < n_dates; ++r) {
        if (!NumericVector::is_na(values_matrix(r, c))) {
          obs_indices_by_group[c].push_back(r);
        }
      }
    }
  }
  
  // Initialize the final result matrix.
  NumericMatrix result_matrix(n_dates, n_groups);
  std::fill(result_matrix.begin(), result_matrix.end(), NA_REAL);
  
  // Loop through each zone (column).
  for (int c = 0; c < n_groups; ++c) {
    // Loop through each date (row) to calculate the moving average at that point.
    for (int r = 0; r < n_dates; ++r) {
      double sum_product = 0.0; // Will hold sum(value) or sum(value*weight)
      double sum_divisor = 0.0; // Will hold count or sum(weight)
      
      Rcpp::Date current_date = unique_dates[r];
      // Calculate the target date by subtracting the year_lag
      Rcpp::Date lagged_year_date(current_date.getYear() - year_lag,
                                  current_date.getMonth(),
                                  current_date.getDay());
      
      // Find the row index of the year-lagged date
      auto it = date_to_row_map.find(lagged_year_date);
      if (it == date_to_row_map.end()) {
        // If the corresponding date from N years ago isn't in our dataset,
        // skip it. The result will remain NA.
        continue;
      }
      
      int base_index = it->second;
      
      // Daily average
      if (temporal) {
        // Define the start and end of the sliding window based on lag and window_size.
        int start_row = base_index - lag - window_size + 1;
        int end_row = base_index - lag;
        // Iterate through the time window for the current date 'r'.
        for (int k = start_row; k <= end_row; ++k) {
          
          // Ensure the window index 'k' is within the valid bounds of the matrix.
          if (k >= 0 && k < n_dates) {
            double val = values_matrix(k, c);
            
            // Check if the value is not NA before including it in the average.
            if (!NumericVector::is_na(val)) {
              
              if (weighted) {
                int weight = count_matrix(k, c);
                sum_product += val * weight;
                sum_divisor += weight;
              } else {
                sum_product += val;
                sum_divisor += 1; // For a simple average, the "weight" is 1
              }
            }
          }
        }
        
      // Sequential average
      } else {
        const std::vector<int>& obs_indices = obs_indices_by_group[c];
        if (obs_indices.empty()) continue;
        
        auto it_obs = std::lower_bound(obs_indices.begin(), obs_indices.end(), base_index);
        int current_obs_pos = std::distance(obs_indices.begin(), it_obs);
        
        int start_obs_pos = current_obs_pos - lag - window_size + 1;
        int end_obs_pos = current_obs_pos - lag;
        
        for (int obs_pos = start_obs_pos; obs_pos <= end_obs_pos; ++obs_pos) {
          if (obs_pos >= 0 && obs_pos < static_cast<int>(obs_indices.size())) {
            int k = obs_indices[obs_pos];
            double val = values_matrix(k, c);
            // This part is identical to the weighted/simple logic
            if (weighted) {
              int weight = count_matrix(k, c);
              sum_product += val * weight;
              sum_divisor += weight;
            } else {
              sum_product += val;
              sum_divisor += 1;
            }
          }
        }
      }
      
      if (sum_divisor > 0) {
        result_matrix(r, c) = sum_product / sum_divisor;
      }
      
      // If non_na_count is 0, the cell remains NA_REAL as initialized.
    }
  }
  
  // Final formatting
  colnames(result_matrix) = unique_groups;
  CharacterVector date_strings = as<CharacterVector>(unique_dates);
  rownames(result_matrix) = date_strings;
  
  return result_matrix;
}
