#include <Rcpp.h>
#include <vector>
#include <cmath>

using namespace Rcpp;

// [[Rcpp::export]]
List join_gnps_cpp(NumericVector x, NumericVector y, 
                   double xPrecursorMz = NA_REAL, double yPrecursorMz = NA_REAL,
                   double tolerance = 0, double ppm = 0) {
  
  std::vector<int> x_matches, y_matches;
  double pdiff = yPrecursorMz - xPrecursorMz;
  size_t x_size = x.size(), y_size = y.size();
  
  // Precompute allowed differences for x values
  std::vector<double> allowed_diffs(x_size);
  for (size_t i = 0; i < x_size; i++) {
    allowed_diffs[i] = tolerance + (ppm * x[i] * 1e-6);
  }
  
  // Reserve memory to avoid reallocations
  x_matches.reserve(x_size);
  y_matches.reserve(x_size);
  
  // Track matched indices for checking unassigned values later
  std::vector<bool> y_used(y_size, false);
  
  // First pass: Direct matches
  for (size_t i = 0; i < x_size; i++) {
    bool matched = false;
    for (size_t j = 0; j < y_size; j++) {
      if (std::abs(x[i] - y[j]) <= allowed_diffs[i]) {
        x_matches.push_back(i + 1);
        y_matches.push_back(j + 1);
        y_used[j] = true;
        matched = true;
        break;  // Exit inner loop early after first match
      }
    }
    if (!matched) {
      x_matches.push_back(i + 1);
      y_matches.push_back(NA_INTEGER);
    }
  }
  
  // Second pass: Apply precursor mass difference
  for (size_t i = 0; i < x_size; i++) {
    for (size_t j = 0; j < y_size; j++) {
      if (std::abs((x[i] + pdiff) - y[j]) <= allowed_diffs[i]) {
        x_matches.push_back(i + 1);
        y_matches.push_back(j + 1);
        y_used[j] = true;
      }
    }
  }
  
  // Final pass: Ensure all `y` values appear at least once
  for (size_t j = 0; j < y_size; j++) {
    if (!y_used[j]) {
      x_matches.push_back(NA_INTEGER);
      y_matches.push_back(j + 1);
    }
  }
  
  return List::create(Named("x") = wrap(x_matches), Named("y") = wrap(y_matches));
}
