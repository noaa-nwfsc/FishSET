#include <Rcpp.h>
#include <map>
#include <vector>
#include <iostream> // Required for Rcpp::Rcout
#include <string>   // <-- Good practice to include the string header
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
    
    // --- NEW SECTION: Find and print matching observations ---
    // Loop through all observations to find matches for the current_zone
    for (int j = 0; j < n_trips; ++j) {
      // Check if the observation's zone matches the current unique zone
      if (obs_zones[j] == current_zone) {
        // If it matches, print the details from that row (index j)
        Rcpp::Rcout << "  -> Match at index " << j 
                    << ": Zone=" << std::string(obs_zones[j])
                    << ", Date=" << obs_dates[j] 
                    << ", Catch=" << obs_catches[j] << std::endl;
      }
    }
    
  }
  
  return result_matrix;
}
