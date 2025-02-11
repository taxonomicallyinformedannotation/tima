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
  
  // Extract m/z and intensity values
  const NumericVector x_mz = x(_, 0), y_mz = y(_, 0);
  const NumericVector x_int = x(_, 1), y_int = y(_, 1);
  
  // Compute total intensity while preserving duplicate handling
  std::unordered_set<double> seen_x, seen_y;
  double x_sum = 0.0, y_sum = 0.0;

  for (int i = 0; i < n; i++) {
    if (!NumericVector::is_na(x_mz[i]) && seen_x.insert(x_mz[i]).second) {
      x_sum += x_int[i];
    }
    if (!NumericVector::is_na(y_mz[i]) && seen_y.insert(y_mz[i]).second) {
      y_sum += y_int[i];
    }
  }

  if (x_sum == 0.0 || y_sum == 0.0) return 0.0;

  // Pre-allocate vectors for valid indices
  std::vector<int> keep_idx;
  keep_idx.reserve(n);
  
  for (int i = 0; i < n; i++) {
    if (!NumericVector::is_na(x_mz[i]) && !NumericVector::is_na(y_mz[i])) {
      keep_idx.push_back(i);
    }
  }
  
  const int l = keep_idx.size();
  if (l == 0) return 0.0;
  
  // Create mapping of m/z values to indices, preserving order of first occurrence
  std::unordered_map<double, int> x_map, y_map;
  int x_count = 0, y_count = 0;
  arma::vec x_int_kept(l), y_int_kept(l);
  arma::umat assignments(2, l);
  
  for (const int idx : keep_idx) {
    const double mz_x = x_mz[idx], mz_y = y_mz[idx];
    
    // Normalize intensities
    x_int_kept[x_count] = std::sqrt(x_int[idx] / x_sum);
    y_int_kept[y_count] = std::sqrt(y_int[idx] / y_sum);
    
    // Preserve original duplicate handling
    auto x_it = x_map.find(mz_x);
    auto y_it = y_map.find(mz_y);
    
    if (x_it == x_map.end()) x_map[mz_x] = x_count++;
    if (y_it == y_map.end()) y_map[mz_y] = y_count++;
    
    assignments(0, x_count-1) = x_map[mz_x];
    assignments(1, y_count-1) = y_map[mz_y];
  }
  
  // Construct sparse similarity matrix
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