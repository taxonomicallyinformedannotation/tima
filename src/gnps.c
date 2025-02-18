#include <R.h>
#include <Rinternals.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>

/* --- Helper: Compare doubles for qsort --- */
static inline int compare_doubles(const void *a, const void *b) {
    double da = *(const double*)a;
    double db = *(const double*)b;
    return (da < db) ? -1 : (da > db) ? 1 : 0;
}

/* --- Helper: Global comparator for indices based on m/z values --- */
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
     this computes the sum of intensities for the first occurrence of each unique m/z.
*/
static double compute_unique_intensity_sum(const double *mz, const double *intensity, int n) {
    int *indices = (int*) malloc(n * sizeof(int));
    if (!indices) error("Memory allocation failed in compute_unique_intensity_sum");
    int count = 0;
    for (int i = 0; i < n; i++) {
        if (!ISNA(mz[i]))
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

/* --- Helper for Binary Search for doubles --- */
static inline int binary_search_double(const double* arr, int n, double val) {
    int left = 0, right = n - 1;
    while (left <= right) {
        int mid = left + ((right - left) >> 1);
        double mid_val = arr[mid];
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
     This function maps the values (for rows given by keep_idx) to factor levels (1-indexed)
     by copying the values, sorting and uniquifying them, and then doing binary search.
*/
static int* generate_factor_indices(const double* data, const int* keep_idx, int n) {
    double *temp = (double*) malloc(n * sizeof(double));
    if (!temp) error("Memory allocation failed in generate_factor_indices");
    for (int i = 0; i < n; i++) {
        temp[i] = data[keep_idx[i]];
    }
    qsort(temp, n, sizeof(double), compare_doubles);
    int unique_count = 0;
    for (int i = 0; i < n; i++) {
        if (i == 0 || temp[i] != temp[i-1])
            temp[unique_count++] = temp[i];
    }
    int *factors = (int*) malloc(n * sizeof(int));
    if (!factors) {
        free(temp);
        error("Memory allocation failed in generate_factor_indices");
    }
    for (int i = 0; i < n; i++) {
        double val = data[keep_idx[i]];
        int pos = binary_search_double(temp, unique_count, val);
        factors[i] = pos + 1;
    }
    free(temp);
    return factors;
}

/* --- Hungarian Algorithm Implementation --- */

typedef struct {
    int n_rows;
    int n_cols;
    double * restrict costs;
    int * restrict block; /* one contiguous block for 4 int arrays */
    int * restrict row_covered;
    int * restrict col_covered;
    int * restrict starred_zeros;
    int * restrict primed_zeros;
} Hungarian;

static Hungarian* hungarian_new(int n_rows, int n_cols, const double * restrict costs) {
    if (!costs || n_rows <= 0 || n_cols <= 0)
        return NULL;
    int max_dim = (n_rows > n_cols) ? n_rows : n_cols;
    Hungarian* h = (Hungarian*) malloc(sizeof(Hungarian));
    if (!h) return NULL;
    h->n_rows = max_dim;
    h->n_cols = max_dim;
    h->costs = (double*) malloc(max_dim * max_dim * sizeof(double));
    if (!h->costs) {
        free(h);
        return NULL;
    }
    memset(h->costs, 0, max_dim * max_dim * sizeof(double));
    /* Allocate one contiguous block for 4 int arrays */
    h->block = (int*) calloc(4 * max_dim, sizeof(int));
    if (!h->block) {
        free(h->costs);
        free(h);
        return NULL;
    }
    h->row_covered   = h->block;
    h->col_covered   = h->block + max_dim;
    h->starred_zeros = h->block + 2 * max_dim;
    h->primed_zeros  = h->block + 3 * max_dim;
    for (int i = 0; i < max_dim; i++) {
        h->starred_zeros[i] = -1;
        h->primed_zeros[i] = -1;
    }
    /* Negate costs so that maximizing becomes minimizing */
    for (int i = 0; i < n_rows; i++) {
        for (int j = 0; j < n_cols; j++) {
            h->costs[i * max_dim + j] = -costs[i * n_cols + j];
        }
    }
    return h;
}

static void hungarian_free(Hungarian* h) {
    if (h) {
        if (h->costs) free(h->costs);
        if (h->block) free(h->block);
        free(h);
    }
}

static inline void step1(Hungarian* h) {
    int n = h->n_rows, m = h->n_cols;
    for (int i = 0; i < n; i++) {
        double min_val = h->costs[i * m];
        for (int j = 1; j < m; j++) {
            double cost = h->costs[i * m + j];
            if (cost < min_val)
                min_val = cost;
        }
        for (int j = 0; j < m; j++) {
            h->costs[i * m + j] -= min_val;
        }
    }
}

static inline void step2(Hungarian* h) {
    int n = h->n_rows, m = h->n_cols;
    for (int j = 0; j < m; j++) {
        double min_val = h->costs[j];
        for (int i = 1; i < n; i++) {
            double cost = h->costs[i * m + j];
            if (cost < min_val)
                min_val = cost;
        }
        for (int i = 0; i < n; i++) {
            h->costs[i * m + j] -= min_val;
        }
    }
}

static inline void step3(Hungarian* h) {
    int n = h->n_rows, m = h->n_cols;
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < m; j++) {
            if (h->costs[i * m + j] == 0 && !h->row_covered[i] && !h->col_covered[j]) {
                h->starred_zeros[i] = j;
                h->row_covered[i] = 1;
                h->col_covered[j] = 1;
                break;
            }
        }
    }
    memset(h->row_covered, 0, n * sizeof(int));
    memset(h->col_covered, 0, m * sizeof(int));
}

static inline int step4(Hungarian* h) {
    int count = 0;
    int n = h->n_rows;
    for (int i = 0; i < n; i++) {
        if (h->starred_zeros[i] != -1) {
            h->col_covered[h->starred_zeros[i]] = 1;
            count++;
        }
    }
    return count;
}

static inline int find_zero(Hungarian* h, int* row, int* col) {
    int n = h->n_rows, m = h->n_cols;
    for (int i = 0; i < n; i++) {
        if (!h->row_covered[i]) {
            for (int j = 0; j < m; j++) {
                if (!h->col_covered[j] && h->costs[i * m + j] == 0) {
                    *row = i;
                    *col = j;
                    return 1;
                }
            }
        }
    }
    return 0;
}

static void augment_path(Hungarian* h, int row, int col) {
    int m = h->n_cols;
    int max_path_length = 2 * m;
    int *path_row = (int*) malloc(max_path_length * sizeof(int));
    int *path_col = (int*) malloc(max_path_length * sizeof(int));
    if (!path_row || !path_col) {
        free(path_row);
        free(path_col);
        error("Memory allocation failed in augment_path");
    }
    int path_count = 0;
    path_row[path_count] = row;
    path_col[path_count] = col;
    
    while (1) {
        int star_row = -1;
        /* Find the starred zero in the column */
        for (int i = 0; i < h->n_rows; i++) {
            if (h->starred_zeros[i] == path_col[path_count]) {
                star_row = i;
                break;
            }
        }
        if (star_row == -1)
            break;
        path_count++;
        if (path_count >= max_path_length) break; /* safety check */
        path_row[path_count] = star_row;
        path_col[path_count] = path_col[path_count - 1];

        int prime_col = h->primed_zeros[star_row];
        path_count++;
        if (path_count >= max_path_length) break; /* safety check */
        path_row[path_count] = star_row;
        path_col[path_count] = prime_col;
    }
    for (int i = 0; i <= path_count; i++) {
        int r = path_row[i], c = path_col[i];
        if (h->starred_zeros[r] == c)
            h->starred_zeros[r] = -1;
        else
            h->starred_zeros[r] = c;
    }
    free(path_row);
    free(path_col);
    
    memset(h->row_covered, 0, h->n_rows * sizeof(int));
    memset(h->col_covered, 0, h->n_cols * sizeof(int));
    for (int i = 0; i < h->n_rows; i++) {
        h->primed_zeros[i] = -1;
    }
}

static inline double find_smallest_uncovered(Hungarian* h) {
    double min_val = INFINITY;
    int n = h->n_rows, m = h->n_cols;
    for (int i = 0; i < n; i++) {
        if (!h->row_covered[i]) {
            for (int j = 0; j < m; j++) {
                double cost = h->costs[i * m + j];
                if (!h->col_covered[j] && cost < min_val)
                    min_val = cost;
            }
        }
    }
    return min_val;
}

int* solve_hungarian(int n_rows, int n_cols, const double * restrict costs) {
    Hungarian* h = hungarian_new(n_rows, n_cols, costs);
    if (!h) error("Failed to initialize Hungarian algorithm");
    
    int* assignment = (int*) malloc(n_rows * sizeof(int));
    if (!assignment) {
        hungarian_free(h);
        error("Memory allocation failed in solve_hungarian");
    }
    
    step1(h);
    step2(h);
    step3(h);
    while (1) {
        int covered_count = step4(h);
        if (covered_count >= h->n_rows || covered_count >= h->n_cols)
            break;
        while (1) {
            int row, col;
            if (!find_zero(h, &row, &col)) {
                double min_val = find_smallest_uncovered(h);
                int n = h->n_rows, m = h->n_cols;
                for (int i = 0; i < n; i++) {
                    for (int j = 0; j < m; j++) {
                        if (h->row_covered[i])
                            h->costs[i * m + j] += min_val;
                        if (!h->col_covered[j])
                            h->costs[i * m + j] -= min_val;
                    }
                }
                break;
            }
            h->primed_zeros[row] = col;
            int star_col = h->starred_zeros[row];
            if (star_col == -1) {
                augment_path(h, row, col);
                break;
            } else {
                h->row_covered[row] = 1;
                h->col_covered[star_col] = 0;
            }
        }
    }
    for (int i = 0; i < n_rows; i++) {
        assignment[i] = h->starred_zeros[i] + 1;
    }
    hungarian_free(h);
    return assignment;
}

/* --- Main gnps Function --- */
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
    
    double* x_data = REAL(x);
    double* y_data = REAL(y);
    if (!x_data || !y_data)
        error("Null input data.");

    /* Compute sums for unique m/z (first occurrence) using the optimized method */
    double x_sum = compute_unique_intensity_sum(x_data, x_data + n, n);
    double y_sum = compute_unique_intensity_sum(y_data, y_data + n, n);
    if (x_sum == 0.0 || y_sum == 0.0)
        return ScalarReal(0.0);

    /* Keep complete cases. */
    int* keep_idx = (int*) malloc(n * sizeof(int));
    if (!keep_idx) error("Memory allocation failed.");
    int l = 0;
    for (int i = 0; i < n; i++) {
        if (!ISNA(x_data[i]) && !ISNA(y_data[i]))
            keep_idx[l++] = i;
    }
    if (l == 0) {
        free(keep_idx);
        return ScalarReal(0.0);
    }

    /* Compute normalized intensities. */
    double* scores = (double*) malloc(l * sizeof(double));
    if (!scores) {
        free(keep_idx);
        error("Memory allocation failed.");
    }
    for (int i = 0; i < l; i++) {
        int idx = keep_idx[i];
        scores[i] = (sqrt(x_data[idx + n]) / sqrt(x_sum)) *
                    (sqrt(y_data[idx + n]) / sqrt(y_sum));
    }

    /* Generate factor indices. */
    int* x_idx = generate_factor_indices(x_data, keep_idx, l);
    int* y_idx = generate_factor_indices(y_data, keep_idx, l);
    if (!x_idx || !y_idx) {
        if (x_idx) free(x_idx);
        if (y_idx) free(y_idx);
        free(keep_idx);
        free(scores);
        return ScalarReal(0.0);
    }

    int max_x = 0, max_y = 0;
    for (int i = 0; i < l; i++) {
        if (x_idx[i] > max_x) max_x = x_idx[i];
        if (y_idx[i] > max_y) max_y = y_idx[i];
    }
    int m = (max_x > max_y) ? max_x : max_y;

    /* Allocate and fill score matrix directly in row-major order */
    double* score_mat = (double*) calloc(m * m, sizeof(double));
    if (!score_mat) {
        free(keep_idx); free(scores); free(x_idx); free(y_idx);
        error("Memory allocation failed.");
    }
    for (int i = 0; i < l; i++) {
        int row = y_idx[i] - 1;
        int col = x_idx[i] - 1;
        score_mat[row * m + col] = scores[i];
    }

    int* assignment = solve_hungarian(m, m, score_mat);
    if (!assignment) {
        free(keep_idx); free(scores); free(x_idx); free(y_idx); free(score_mat);
        return ScalarReal(0.0);
    }

    double total_score = 0.0;
    for (int i = 0; i < m; i++) {
        int j = assignment[i] - 1;
        if (j >= 0 && j < m)
            total_score += score_mat[i * m + j];
    }

    free(keep_idx);
    free(scores);
    free(x_idx);
    free(y_idx);
    free(score_mat);
    free(assignment);

    return ScalarReal(total_score);
}
