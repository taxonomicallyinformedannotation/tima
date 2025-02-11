#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <algorithm>

using namespace Rcpp;

// [[Rcpp::export]]
List join_gnps_cpp(NumericVector x, NumericVector y, 
                   double xPrecursorMz = NA_REAL, double yPrecursorMz = NA_REAL,
                   double tolerance = 0, double ppm = 0) {
  
  const size_t x_size = x.size(), y_size = y.size();
  std::vector<std::pair<int, int>> matches;
  matches.reserve(x_size + y_size); // Preallocate for worst case
  
  const double pdiff = yPrecursorMz - xPrecursorMz;
  std::vector<bool> y_used(y_size, false);
  
  // Create sorted indices for y to enable early breaking of loops
  std::vector<size_t> y_sorted_idx(y_size);
  std::iota(y_sorted_idx.begin(), y_sorted_idx.end(), 0);
  std::sort(y_sorted_idx.begin(), y_sorted_idx.end(),
            [&y](size_t i1, size_t i2) { return y[i1] < y[i2]; });
  
  // First pass: Direct matches and precursor difference matches combined
  for (size_t i = 0; i < x_size; i++) {
    const double x_val = x[i];
    const double allowed_diff = tolerance + (ppm * x_val * 1e-6);
    bool matched = false;
    
    // Binary search for lower bound
    auto lower = std::lower_bound(y_sorted_idx.begin(), y_sorted_idx.end(), x_val - allowed_diff,
                                 [&y](size_t idx, double val) { return y[idx] < val; });
    
    // Check direct matches
    for (; lower != y_sorted_idx.end(); ++lower) {
      const size_t j = *lower;
      if (y[j] > x_val + allowed_diff) break; // Early exit if beyond range
      
      if (std::abs(x_val - y[j]) <= allowed_diff) {
        matches.emplace_back(i + 1, j + 1);
        y_used[j] = true;
        matched = true;
        break;
      }
    }
    
    // Check precursor difference matches
    const double x_pdiff = x_val + pdiff;
    lower = std::lower_bound(y_sorted_idx.begin(), y_sorted_idx.end(), x_pdiff - allowed_diff,
                            [&y](size_t idx, double val) { return y[idx] < val; });
    
    for (; lower != y_sorted_idx.end(); ++lower) {
      const size_t j = *lower;
      if (y[j] > x_pdiff + allowed_diff) break; // Early exit if beyond range
      
      if (std::abs(x_pdiff - y[j]) <= allowed_diff) {
        matches.emplace_back(i + 1, j + 1);
        y_used[j] = true;
      }
    }
    
    if (!matched) {
      matches.emplace_back(i + 1, NA_INTEGER);
    }
  }
  
  // Add unmatched y values
  for (size_t j = 0; j < y_size; j++) {
    if (!y_used[j]) {
      matches.emplace_back(NA_INTEGER, j + 1);
    }
  }
  
  // Convert matches to separate x and y vectors
  std::vector<int> x_matches(matches.size());
  std::vector<int> y_matches(matches.size());
  for (size_t i = 0; i < matches.size(); i++) {
    x_matches[i] = matches[i].first;
    y_matches[i] = matches[i].second;
  }
  
  return List::create(Named("x") = wrap(x_matches), Named("y") = wrap(y_matches));
}
