#include <Rcpp.h>
#include <map>
#include <vector>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector calculate_moving_avg(DateVector unique_dates, 
                                   CharacterVector unique_zones, 
                                   DateVector obs_dates,
                                   CharacterVector obs_zones,
                                   NumericVector obs_catches, 
                                   int window_size, 
                                   int lag) {
  
  int n_dates = unique_dates.size();
  int n_zones = unique_zones.size();
  int n_trips = obs_dates.size();
  
  // Create the expected matrix with zones as column names
  NumericMatrix result_matrix(n_trips, n_zones);
  colnames(result_matrix) = unique_zones;
  
  // Loop through each zone
  for (int i = 0; i < n_zones; ++i) {
    String current_zone = unique_zones[i];
    
    std::map<double, std::vector<double>> catches_for_zone;
    
    // Loop through all trips to find the ones matching the current zone.
    for (int k = 0; k < n_trips; ++k) {
      if (obs_zones[k] == current_zone) {
        // Check if catch is not NA before adding it.
        if (!NumericVector::is_na(obs_catches[k])) {
          // Add the catch and increment the trip count for that date.
          // If the date isn't in the map, it's created automatically.
          catches_for_zone[obs_dates[k]].push_back(obs_catches[k]);
        }
      }
    }
    
    
    
    // Now calculate the moving average for every single day in the continuous date sequence.
    for (int j = 0; j < n_dates; ++j) {
      double current_date = unique_dates[j];
      
      // Define the time window based on lag and window size.
      double end_date = current_date - lag;
      double start_date = end_date - window_size + 1;
      
      double total_catch = 0.0;
      int count = 0;
      
      // Efficiently find the first date in our map that is within the window.
      // .lower_bound() is very fast because the map is sorted.
      auto it_start = catches_for_zone.lower_bound(start_date);
      
      // Iterate only over the trips that fall within the valid date window.
      for (auto it = it_start; it != catches_for_zone.end() && it->first <= end_date; ++it) {
        // it->second is the vector of catches for a specific day.
        // We loop through it to add each individual catch value.
        for (double catch_value : it->second) {
          total_catch += catch_value;
          count++;
        }
      }
      
      // Calculate the average and store it in the matrix.
      if (count > 0) {
        result_matrix(j, i) = total_catch / count;
      } else {
        result_matrix(j, i) = NA_REAL; // Use NA if no trips were in the window.
      }
    }
    
  }
  
  return result_matrix;
}
