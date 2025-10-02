#include <Rcpp.h>
#include <map>
#include <vector>
#include <iostream> // Required for Rcpp::Rcout
#include <string>   // <-- Good practice to include the string header
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector calculate_moving_avg(DateVector unique_dates, 
                                   CharacterVector unique_groups, 
                                   DateVector obs_dates,
                                   CharacterVector obs_groups,
                                   NumericVector obs_values, 
                                   int window_size, 
                                   int lag) {
  
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
  
  // Initialize the final result matrix.
  NumericMatrix result_matrix(n_dates, n_groups);
  std::fill(result_matrix.begin(), result_matrix.end(), NA_REAL);
  
  // Loop through each zone (column).
  for (int c = 0; c < n_groups; ++c) {
    // Loop through each date (row) to calculate the moving average at that point.
    for (int r = 0; r < n_dates; ++r) {
      
      double window_sum = 0.0;
      int non_na_count = 0;
      
      // Define the start and end of the sliding window based on lag and window_size.
      int start_row = r - lag - window_size + 1;
      int end_row = r - lag;
      
      // Iterate through the time window for the current date 'r'.
      for (int k = start_row; k <= end_row; ++k) {
        
        // Ensure the window index 'k' is within the valid bounds of the matrix.
        if (k >= 0 && k < n_dates) {
          double val = values_matrix(k, c);
          
          // Check if the value is not NA before including it in the average.
          if (!NumericVector::is_na(val)) {
            window_sum += val;
            non_na_count++;
          }
        }
      }
      
      // If we found any non-NA values in the window, calculate the average.
      if (non_na_count > 0) {
        result_matrix(r, c) = window_sum / non_na_count;
      }
      // If non_na_count is 0, the cell remains NA_REAL as initialized.
    }
  }
  
  // Set the column names to be the unique groups
  colnames(result_matrix) = unique_groups;
  
  // Convert dates to strings and set as row names
  CharacterVector date_strings = as<CharacterVector>(unique_dates);
  rownames(result_matrix) = date_strings;
  
  return result_matrix;
}
