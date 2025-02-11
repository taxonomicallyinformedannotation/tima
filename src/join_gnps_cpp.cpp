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
  x_matches.reserve(x_size);
  y_matches.reserve(x_size);

  const double pdiff = yPrecursorMz - xPrecursorMz;

  std::vector<size_t> y_sorted_idx(y_size);
  std::iota(y_sorted_idx.begin(), y_sorted_idx.end(), 0);
  std::sort(y_sorted_idx.begin(), y_sorted_idx.end(),
            [&y](size_t i1, size_t i2) { return y[i1] < y[i2]; });

  std::vector<bool> y_matched(y_size, false);

  for (size_t i = 0; i < x_size; i++) {
    const double x_val = x[i];
    const double allowed_diff = tolerance + (ppm * x_val * 1e-6);
    
    auto lower = std::lower_bound(y_sorted_idx.begin(), y_sorted_idx.end(), x_val - allowed_diff,
                                  [&y](size_t idx, double val) { return y[idx] < val; });

    bool found_match = false;

    for (auto it = lower; it != y_sorted_idx.end(); ++it) {
      size_t idx = *it;
      if (y[idx] > x_val + allowed_diff) break;

      if (std::abs(x_val - y[idx]) <= allowed_diff) {
        x_matches.push_back(i + 1);
        y_matches.push_back(idx + 1);
        y_matched[idx] = true;
        found_match = true;
        break;
      }
    }

    const double x_pdiff = x_val + pdiff;
    lower = std::lower_bound(y_sorted_idx.begin(), y_sorted_idx.end(), x_pdiff - allowed_diff,
                             [&y](size_t idx, double val) { return y[idx] < val; });

    for (auto it = lower; it != y_sorted_idx.end(); ++it) {
      size_t idx = *it;
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

  for (size_t j = 0; j < y_size; j++) {
    if (!y_matched[j]) {
      x_matches.push_back(NA_INTEGER);
      y_matches.push_back(j + 1);
    }
  }

  return List::create(Named("x") = wrap(x_matches), Named("y") = wrap(y_matches));
}
