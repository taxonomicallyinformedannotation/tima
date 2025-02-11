#include <RcppArmadillo.h>
#include <unordered_map>
#include <cmath>

using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
double gnps_cpp(NumericMatrix x, NumericMatrix y) {
  const int n = x.nrow();
  
  if (n != y.nrow()) {
    stop("'x' and 'y' must have the same number of rows.");
  }
  
  // Pre-allocate containers
  std::unordered_set<double> seen_x(n), seen_y(n);
  double x_sum = 0.0, y_sum = 0.0;

  // First pass: compute sums and track duplicates
  for (int i = 0; i < n; i++) {
    const double x_mz = x(i, 0), y_mz = y(i, 0);
    
    if (!NumericVector::is_na(x_mz) && seen_x.insert(x_mz).second) {
      x_sum += x(i, 1);
    }
    if (!NumericVector::is_na(y_mz) && seen_y.insert(y_mz).second) {
      y_sum += y(i, 1);
    }
  }

  if (x_sum == 0.0 || y_sum == 0.0) return 0.0;

  // Pre-allocate vectors for valid indices
  std::vector<int> keep_idx;
  keep_idx.reserve(n);
  
  // Second pass: collect valid indices
  for (int i = 0; i < n; i++) {
    if (!NumericVector::is_na(x(i, 0)) && !NumericVector::is_na(y(i, 0))) {
      keep_idx.push_back(i);
    }
  }
  
  const int l = keep_idx.size();
  if (l == 0) return 0.0;
  
  // Pre-compute normalization factors
  const double x_norm = std::sqrt(1.0 / x_sum);
  const double y_norm = std::sqrt(1.0 / y_sum);
  
  // Initialize data structures
  std::unordered_map<double, int> x_map(n), y_map(n);
  int x_count = 0, y_count = 0;
  arma::vec x_int_kept(l), y_int_kept(l);
  arma::umat assignments(2, l);
  
  // Process each valid index
  for (const int idx : keep_idx) {
    const double mz_x = x(idx, 0), mz_y = y(idx, 0);
    
    // Normalize intensities
    x_int_kept[x_count] = std::sqrt(x(idx, 1)) * x_norm;
    y_int_kept[y_count] = std::sqrt(y(idx, 1)) * y_norm;
    
    // Maintain original mapping logic
    auto x_it = x_map.find(mz_x);
    auto y_it = y_map.find(mz_y);
    
    if (x_it == x_map.end()) x_map[mz_x] = x_count++;
    if (y_it == y_map.end()) y_map[mz_y] = y_count++;
    
    assignments(0, x_count-1) = x_map[mz_x];
    assignments(1, y_count-1) = y_map[mz_y];
  }
  
  // Construct similarity matrix
  arma::mat score_mat(x_count, y_count, fill::zeros);
  
  for (int i = 0; i < l; i++) {
    score_mat(assignments(0, i), assignments(1, i)) = x_int_kept[i] * y_int_kept[i];
  }
  
  // Solve Hungarian assignment problem
  Environment clue = Environment::namespace_env("clue");
  Function solve_LSAP = clue["solve_LSAP"];
  IntegerVector best = solve_LSAP(score_mat, Named("maximum") = true);
  
  // Compute final score
  double total_score = 0.0;
  for (int i = 0; i < x_count; i++) {
    total_score += score_mat(i, best[i] - 1);
  }
  
  return total_score;
}