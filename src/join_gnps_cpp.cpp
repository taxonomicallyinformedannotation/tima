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
  std::vector<int> x_matches, y_matches;
  x_matches.reserve(x_size + y_size);
  y_matches.reserve(x_size + y_size);
  
  const double pdiff = yPrecursorMz - xPrecursorMz;
  
  // Sort y values and store indices
  std::vector<size_t> y_sorted_idx(y_size);
  std::iota(y_sorted_idx.begin(), y_sorted_idx.end(), 0);
  std::sort(y_sorted_idx.begin(), y_sorted_idx.end(),
            [&y](size_t i1, size_t i2) { return y[i1] < y[i2]; });

  std::vector<bool> y_matched(y_size, false);
  
  // First pass: Direct matches and precursor difference matches combined
  for (size_t i = 0; i < x_size; i++) {
    const double x_val = x[i];
    const double allowed_diff = tolerance + (ppm * x_val * 1e-6);
    bool found_match = false;
    
    // Custom binary search for lower bound
    size_t j_start = 0, j_end = y_size;
    while (j_start < j_end) {
      size_t mid = j_start + (j_end - j_start) / 2;
      if (y[y_sorted_idx[mid]] < x_val - allowed_diff) {
        j_start = mid + 1;
      } else {
        j_end = mid;
      }
    }
    
    // Scan forward from lower bound
    for (size_t j = j_start; j < y_size; j++) {
      size_t idx = y_sorted_idx[j];
      if (y[idx] > x_val + allowed_diff) break; // Early exit
      
      if (std::abs(x_val - y[idx]) <= allowed_diff) {
        x_matches.push_back(i + 1);
        y_matches.push_back(idx + 1);
        y_matched[idx] = true;
        found_match = true;
        break;
      }
    }
    
    // Check precursor difference matches
    const double x_pdiff = x_val + pdiff;
    
    j_start = 0, j_end = y_size;
    while (j_start < j_end) {
      size_t mid = j_start + (j_end - j_start) / 2;
      if (y[y_sorted_idx[mid]] < x_pdiff - allowed_diff) {
        j_start = mid + 1;
      } else {
        j_end = mid;
      }
    }
    
    for (size_t j = j_start; j < y_size; j++) {
      size_t idx = y_sorted_idx[j];
      if (y[idx] > x_pdiff + allowed_diff) break;
      
      if (std::abs(x_pdiff - y[idx]) <= allowed_diff) {
        x_matches.push_back(i + 1);
        y_matches.push_back(idx + 1);
        y_matched[idx] = true;
      }
    }
    
    if (!found_match) {
      x_matches.push_back(i + 1);
      y_matches.push_back(NA_INTEGER);
    }
  }
  
  // Second pass: Unmatched y values
  for (size_t j = 0; j < y_size; j++) {
    if (!y_matched[j]) {
      x_matches.push_back(NA_INTEGER);
      y_matches.push_back(j + 1);
    }
  }
  
  return List::create(Named("x") = wrap(x_matches), Named("y") = wrap(y_matches));
}
