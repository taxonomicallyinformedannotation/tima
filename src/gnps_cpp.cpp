#include <RcppArmadillo.h>
#include <Rcpp.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
double gnps_cpp(NumericMatrix x, NumericMatrix y) {
  int n = x.nrow();
  
  if (n != y.nrow()) {
    stop("'x' and 'y' are expected to have the same number of rows.");
  }
  
  // **1. Extract values**
  NumericVector x_mz = x(_, 0);
  NumericVector y_mz = y(_, 0);
  NumericVector x_int = x(_, 1);
  NumericVector y_int = y(_, 1);
  
  // **2. Compute sums avoiding duplicates**
  std::unordered_set<double> seen_x, seen_y;
  double x_sum = 0.0, y_sum = 0.0;
  
  for (int i = 0; i < n; i++) {
    if (!NumericVector::is_na(x_mz[i]) && seen_x.find(x_mz[i]) == seen_x.end()) {
      x_sum += x_int[i];
      seen_x.insert(x_mz[i]);
    }
    if (!NumericVector::is_na(y_mz[i]) && seen_y.find(y_mz[i]) == seen_y.end()) {
      y_sum += y_int[i];
      seen_y.insert(y_mz[i]);
    }
  }
  
  if (x_sum == 0 || y_sum == 0) {
    return 0.0;
  }
  
  // **3. Keep only valid (non-NA) matched rows**
  std::vector<int> keep_idx;
  for (int i = 0; i < n; i++) {
    if (!NumericVector::is_na(x_mz[i]) && !NumericVector::is_na(y_mz[i])) {
      keep_idx.push_back(i);
    }
  }
  
  int l = keep_idx.size();
  if (l == 0) {
    return 0.0;
  }
  
  // **4. Subset matched values**
  NumericVector x_mz_kept(l), y_mz_kept(l), x_int_kept(l), y_int_kept(l);
  for (int i = 0; i < l; i++) {
    int idx = keep_idx[i];
    x_mz_kept[i] = x_mz[idx];
    y_mz_kept[i] = y_mz[idx];
    x_int_kept[i] = x_int[idx];
    y_int_kept[i] = y_int[idx];
  }
  
  // **5. Compute scores** using normalized intensity
  NumericVector scores(l);
  for (int i = 0; i < l; i++) {
    scores[i] = sqrt(x_int_kept[i] / x_sum) * sqrt(y_int_kept[i] / y_sum);
  }
  
  // **6. Convert m/z values into unique integer indices**
  std::map<double, int> x_map, y_map;
  int x_count = 0, y_count = 0;
  IntegerVector x_idx(l), y_idx(l);
  
  for (int i = 0; i < l; i++) {
    if (x_map.find(x_mz_kept[i]) == x_map.end()) x_map[x_mz_kept[i]] = ++x_count;
    if (y_map.find(y_mz_kept[i]) == y_map.end()) y_map[y_mz_kept[i]] = ++y_count;
    x_idx[i] = x_map[x_mz_kept[i]];
    y_idx[i] = y_map[y_mz_kept[i]];
  }
  
  // **7. Efficient matrix assignment (dense matrix)**
  arma::mat score_mat(l, l, fill::zeros);
  for (int i = 0; i < l; i++) {
    score_mat(x_idx[i] - 1, y_idx[i] - 1) = scores[i];
  }
  
  // **8. Solve Hungarian assignment problem**
  Environment clue = Environment::namespace_env("clue");
  Function solve_LSAP = clue["solve_LSAP"];
  
  IntegerVector best = solve_LSAP(score_mat, Named("maximum") = true);
  
  // **9. Compute final similarity score**
  double total_score = 0.0;
  for (int i = 0; i < l; i++) {
    total_score += score_mat(i, best[i] - 1);
  }
  
  return total_score;
}
