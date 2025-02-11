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

  // Pre-allocate hash tables with estimated sizes
  std::unordered_set<double> seen_x, seen_y;
  seen_x.reserve(n);
  seen_y.reserve(n);

  double x_sum = 0.0, y_sum = 0.0;

  // **First pass: Compute intensity sums and track unique m/z values**
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

  // **Precompute normalization factors**
  const double x_norm = std::sqrt(1.0 / x_sum);
  const double y_norm = std::sqrt(1.0 / y_sum);

  // **Collect valid indices**
  std::vector<int> keep_idx;
  keep_idx.reserve(n);

  for (int i = 0; i < n; i++) {
    if (!NumericVector::is_na(x(i, 0)) && !NumericVector::is_na(y(i, 0))) {
      keep_idx.push_back(i);
    }
  }
  
  const int l = keep_idx.size();
  if (l == 0) return 0.0;

  // **Pre-allocate data structures**
  std::unordered_map<double, int> x_map, y_map;
  x_map.reserve(l);
  y_map.reserve(l);
  
  arma::vec x_int_kept(l), y_int_kept(l);
  arma::umat assignments(2, l);

  int x_count = 0, y_count = 0;

  // **Process valid indices & normalize intensities**
  for (int i = 0; i < l; i++) {
    const int idx = keep_idx[i];
    const double mz_x = x(idx, 0), mz_y = y(idx, 0);

    x_int_kept[i] = std::sqrt(x(idx, 1)) * x_norm;
    y_int_kept[i] = std::sqrt(y(idx, 1)) * y_norm;

    // Efficiently insert into hash maps and get unique indices
    int& x_idx = x_map.try_emplace(mz_x, x_count).first->second;
    if (x_idx == x_count) x_count++;

    int& y_idx = y_map.try_emplace(mz_y, y_count).first->second;
    if (y_idx == y_count) y_count++;

    assignments(0, i) = x_idx;
    assignments(1, i) = y_idx;
  }

  // **Construct similarity matrix (sparse-friendly)**
  arma::mat score_mat(x_count, y_count, fill::zeros);
  
  for (int i = 0; i < l; i++) {
    score_mat(assignments(0, i), assignments(1, i)) = x_int_kept[i] * y_int_kept[i];
  }

  // **Solve Hungarian assignment problem efficiently**
  Environment clue = Environment::namespace_env("clue");
  Function solve_LSAP = clue["solve_LSAP"];
  IntegerVector best = solve_LSAP(score_mat, Named("maximum") = true);

  // **Compute final similarity score**
  double total_score = 0.0;
  for (int i = 0; i < x_count; i++) {
    total_score += score_mat(i, best[i] - 1);
  }

  return total_score;
}
