#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <algorithm>

using namespace Rcpp;

// [[Rcpp::export]]
List join_gnps_cpp(NumericVector x, NumericVector y, 
               double xPrecursorMz = NA_REAL, double yPrecursorMz = NA_REAL,
               double tolerance = 0, double ppm = 0) {
  
  std::vector<int> x_matches, y_matches;
  double pdiff = yPrecursorMz - xPrecursorMz;
  
  // Track matched indices for checking unassigned values later
  std::vector<bool> x_used(x.size(), false);
  std::vector<bool> y_used(y.size(), false);
  
  // First pass: Direct matches
  for (size_t i = 0; i < x.size(); i++) {
    bool matched = false;
    for (size_t j = 0; j < y.size(); j++) {
      double diff = std::abs(x[i] - y[j]);
      double allowed_diff = tolerance + (ppm * x[i] * 1e-6);
      
      if (diff <= allowed_diff) {
        x_matches.push_back(i + 1);
        y_matches.push_back(j + 1);
        x_used[i] = true;
        y_used[j] = true;
        matched = true;
      }
    }
    if (!matched) {
      x_matches.push_back(i + 1);
      y_matches.push_back(NA_INTEGER);
    }
  }
  
  // Second pass: Apply precursor mass difference
  for (size_t i = 0; i < x.size(); i++) {
    for (size_t j = 0; j < y.size(); j++) {
      double adjusted_diff = std::abs((x[i] + pdiff) - y[j]);
      double allowed_diff = tolerance + (ppm * x[i] * 1e-6);
      
      if (adjusted_diff <= allowed_diff) {
        x_matches.push_back(i + 1);
        y_matches.push_back(j + 1);
      }
    }
  }
  
  // Final pass: Ensure all `y` values appear at least once
  for (size_t j = 0; j < y.size(); j++) {
    if (!y_used[j]) {
      x_matches.push_back(NA_INTEGER);
      y_matches.push_back(j + 1);
    }
  }
  
  return List::create(Named("x") = wrap(x_matches), Named("y") = wrap(y_matches));
}
