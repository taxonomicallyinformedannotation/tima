#include <R.h>
#include <Rinternals.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>
#ifdef _OPENMP
#include <omp.h>
#endif

/* --- Safe Memory Allocation Functions --- */
static inline void *safe_malloc(size_t size) {
    void *ptr = malloc(size);
    if (!ptr)
        error("Memory allocation of %zu bytes failed.", size);
    return ptr;
}

/* --- Helper: Compare doubles for qsort --- */
static inline int compare_doubles(const void *a, const void *b) {
    double da = *(const double*)a;
    double db = *(const double*)b;
    return (da < db) ? -1 : (da > db) ? 1 : 0;
}

/* --- Global comparator for indices based on m/z values --- */
static const double *global_mz = NULL;
static int cmp_indices(const void *a, const void *b) {
    const int ia = *(const int*)a;
    const int ib = *(const int*)b;
    double diff = global_mz[ia] - global_mz[ib];
    if(diff < 0) return -1;
    else if(diff > 0) return 1;
    else return (ia - ib);
}

/* --- Compute Unique Intensity Sum ---
     Given m/z and corresponding intensity (each of length n),
     computes the sum of intensities for the first occurrence of each unique m/z.
*/
static double compute_unique_intensity_sum(const double *mz, const double *intensity, int n) {
    int *indices = (int*) safe_malloc(n * sizeof(int));
    int count = 0;
    for (int i = 0; i < n; i++) {
        if (!ISNA(mz[i]) && !ISNA(intensity[i]))
            indices[count++] = i;
    }
    if (count == 0) {
        free(indices);
        return 0.0;
    }
    global_mz = mz;
    qsort(indices, count, sizeof(int), cmp_indices);
    double sum = intensity[indices[0]];
    for (int i = 1; i < count; i++) {
        if (mz[indices[i]] != mz[indices[i-1]])
            sum += intensity[indices[i]];
    }
    free(indices);
    return sum;
}

/* --- Helper to return score=0.0 and matches=0 --- */
static SEXP return_zero_result() {
    SEXP result = PROTECT(allocVector(VECSXP, 2));
    SET_VECTOR_ELT(result, 0, ScalarReal(0.0));
    SET_VECTOR_ELT(result, 1, ScalarInteger(0));

    SEXP names = PROTECT(allocVector(STRSXP, 2));
    SET_STRING_ELT(names, 0, mkChar("score"));
    SET_STRING_ELT(names, 1, mkChar("matches"));
    setAttrib(result, R_NamesSymbol, names);

    UNPROTECT(2);
    return result;
}

/* --- Helper for Binary Search for doubles ---
     Uses a linear search for very small arrays and binary search (with prefetch) otherwise.
*/
static inline int binary_search_double(const double* arr, int n, double val) {
    if(n <= 0)
        return -1;
    if(n < 16) {
        for (int i = 0; i < n; i++)
            if (arr[i] == val)
                return i;
        return -1;
    }
    int left = 0, right = n - 1;
    while (left <= right) {
        int mid = left + ((right - left) >> 1);
        double mid_val = arr[mid];
        if (mid + 16 < n)
            __builtin_prefetch(&arr[mid + 16], 0, 0);
        if (mid_val == val)
            return mid;
        else if (mid_val < val)
            left = mid + 1;
        else
            right = mid - 1;
    }
    return -1;
}

/* --- Optimized Generate Factor Indices ---
     Maps the values (for rows given by keep_idx) to factor levels (1-indexed)
     by copying the values, sorting and uniquifying them, then binary searching.
*/
static inline int* generate_factor_indices(const double* data, const int* keep_idx, int n) {
    double *temp = (double*) safe_malloc(n * sizeof(double));
    /* Parallelize copying; each iteration is independent. */
    #pragma omp parallel for
    for (int i = 0; i < n; i++)
        temp[i] = data[keep_idx[i]];

    qsort(temp, n, sizeof(double), compare_doubles);

    int unique_count = 0;
    for (int i = 0; i < n; i++) {
        if (i == 0 || temp[i] != temp[i-1])
            temp[unique_count++] = temp[i];
    }

    int *factors = (int*) safe_malloc(n * sizeof(int));
    /* Parallelize computing factor indices. */
    #pragma omp parallel for
    for (int i = 0; i < n; i++) {
        double val = data[keep_idx[i]];
        int pos = binary_search_double(temp, unique_count, val);
        factors[i] = pos + 1;
    }
    free(temp);
    return factors;
}

/* --- Auction Algorithm for the Assignment Problem ---
     Solves the LSAP for an n×n score matrix (profits) using an auction algorithm.
     The inner bidding loop is unrolled by 4.
     Returns a pointer to an int array (allocated with malloc) of length n,
     with 1-indexed column assignments for each row.
*/
static int* solve_assignment_auction(int n, const double *score) {
    double *price = (double*) safe_malloc(n * sizeof(double));
    int *assignment = (int*) safe_malloc(n * sizeof(int));
    int *unassigned = (int*) safe_malloc(n * sizeof(int));
    memset(price, 0, n * sizeof(double));
    for (int i = 0; i < n; i++) {
         assignment[i] = -1;
         unassigned[i] = i;
    }
    int unassigned_count = n;
    double epsilon = 1e-6;
    while (unassigned_count > 0) {
         int i = unassigned[unassigned_count - 1];
         unassigned_count--;
         double best_val = -INFINITY, second_val = -INFINITY;
         int best_j = -1;
         int j;
         for (j = 0; j <= n - 4; j += 4) {
              double v0 = score[i * n + j] - price[j];
              double v1 = score[i * n + j + 1] - price[j + 1];
              double v2 = score[i * n + j + 2] - price[j + 2];
              double v3 = score[i * n + j + 3] - price[j + 3];
              if (v0 > best_val) { second_val = best_val; best_val = v0; best_j = j; }
              else if (v0 > second_val) { second_val = v0; }
              if (v1 > best_val) { second_val = best_val; best_val = v1; best_j = j + 1; }
              else if (v1 > second_val) { second_val = v1; }
              if (v2 > best_val) { second_val = best_val; best_val = v2; best_j = j + 2; }
              else if (v2 > second_val) { second_val = v2; }
              if (v3 > best_val) { second_val = best_val; best_val = v3; best_j = j + 3; }
              else if (v3 > second_val) { second_val = v3; }
         }
         for (; j < n; j++){
              double v = score[i * n + j] - price[j];
              if (v > best_val) { second_val = best_val; best_val = v; best_j = j; }
              else if (v > second_val) { second_val = v; }
         }
         double bid = best_val - second_val + epsilon;
         price[best_j] += bid;
         int previous = -1;
         for (int k = 0; k < n; k++) {
              if (assignment[k] == best_j) {
                  previous = k;
                  break;
              }
         }
         if (previous != -1) {
              assignment[previous] = -1;
              unassigned[unassigned_count++] = previous;
         }
         assignment[i] = best_j;
    }
    free(price);
    free(unassigned);
    for (int i = 0; i < n; i++)
         assignment[i] = assignment[i] + 1;
    return assignment;
}

/* --- Main gnps Function ---
     Expects two numeric matrices 'x' and 'y' with the same number of rows.
     Computes normalized intensities, maps m/z values to factor levels,
     builds a score matrix, and solves the assignment problem using the auction algorithm.
*/
SEXP gnps(SEXP x, SEXP y) {
    if (!isReal(x) || !isReal(y))
        error("Inputs must be numeric matrices.");
    SEXP x_dim = getAttrib(x, R_DimSymbol);
    SEXP y_dim = getAttrib(y, R_DimSymbol);
    if (!isInteger(x_dim) || !isInteger(y_dim))
        error("Dimensions must be integer vectors.");
    int n = INTEGER(x_dim)[0];
    if (n != INTEGER(y_dim)[0])
        error("'x' and 'y' must have the same number of rows.");

    double *x_data = REAL(x);
    double *y_data = REAL(y);
    if (!x_data || !y_data)
        error("Null input data.");

    double x_sum = compute_unique_intensity_sum(x_data, x_data + n, n);
    double y_sum = compute_unique_intensity_sum(y_data, y_data + n, n);
    if (x_sum == 0.0 || y_sum == 0.0)
        return return_zero_result();

    int *keep_idx = (int*) safe_malloc(n * sizeof(int));
    int l = 0;
    for (int i = 0; i < n; i++) {
        if (!ISNA(x_data[i]) && !ISNA(y_data[i]))
            keep_idx[l++] = i;
    }
    if (l == 0) {
        free(keep_idx);
        return return_zero_result();
    }

    double *scores = (double*) safe_malloc(l * sizeof(double));
    // Parallelize score computation. Each iteration is independent.
    #pragma omp parallel for
    for (int i = 0; i < l; i++) {
        int idx = keep_idx[i];
        scores[i] = (sqrt(x_data[idx + n]) / sqrt(x_sum)) *
                    (sqrt(y_data[idx + n]) / sqrt(y_sum));
    }

    int *x_idx = generate_factor_indices(x_data, keep_idx, l);
    int *y_idx = generate_factor_indices(y_data, keep_idx, l);
    if (!x_idx || !y_idx) {
        free(keep_idx);
        free(scores);
        if (x_idx) free(x_idx);
        if (y_idx) free(y_idx);
        return return_zero_result();
    }

    int max_x = 0, max_y = 0;
    /* Parallel reduction to find maximum factor values */
    #pragma omp parallel for reduction(max:max_x) reduction(max:max_y)
    for (int i = 0; i < l; i++) {
        if (x_idx[i] > max_x) max_x = x_idx[i];
        if (y_idx[i] > max_y) max_y = y_idx[i];
    }
    int m = (max_x > max_y) ? max_x : max_y;

    double *score_mat = (double*) calloc(m * m, sizeof(double));
    if (!score_mat) {
        free(keep_idx);
        free(scores);
        free(x_idx);
        free(y_idx);
        error("Memory allocation failed.");
    }
    /* Parallelize filling the score matrix. */
    #pragma omp parallel for
    for (int i = 0; i < l; i++) {
        int row = y_idx[i] - 1;
        int col = x_idx[i] - 1;
        score_mat[row * m + col] = scores[i];
    }

    int *assignment = solve_assignment_auction(m, score_mat);
    if (!assignment) {
        free(keep_idx);
        free(scores);
        free(x_idx);
        free(y_idx);
        free(score_mat);
        return return_zero_result();
    }

    double total_score = 0.0;
    int matched_peaks = 0;
    /* Use OpenMP reduction to safely sum the total score in parallel */
    #pragma omp parallel for reduction(+:total_score, matched_peaks)
    for (int i = 0; i < m; i++) {
        int j = assignment[i] - 1;
        if (j >= 0 && j < m) {
            double val = score_mat[i * m + j];
            if (val > 0.0) {
                total_score += val;
                matched_peaks += 1;
            }
        }
    }

    free(keep_idx);
    free(scores);
    free(x_idx);
    free(y_idx);
    free(score_mat);
    free(assignment);

    SEXP result = PROTECT(allocVector(VECSXP, 2));
    SET_VECTOR_ELT(result, 0, ScalarReal(total_score));
    SET_VECTOR_ELT(result, 1, ScalarInteger(matched_peaks));
  
    SEXP names = PROTECT(allocVector(STRSXP, 2));
    SET_STRING_ELT(names, 0, mkChar("score"));
    SET_STRING_ELT(names, 1, mkChar("matches"));
    setAttrib(result, R_NamesSymbol, names);
  
    UNPROTECT(2); // result, names
    return result;
}
