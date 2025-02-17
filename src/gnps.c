#include <R.h>
#include <Rinternals.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

// Helper function to check for exact equality.
static int find_value(double value, double* array, int size) {
    if (!array) return 0;
    for (int i = 0; i < size; i++) {
        if (array[i] == value) return 1;
    }
    return 0;
}

/* --- Hungarian Algorithm Implementation --- */

typedef struct {
    int n_rows;
    int n_cols;
    double* costs;
    int* row_covered;
    int* col_covered;
    int* starred_zeros;
    int* primed_zeros;
} Hungarian;

Hungarian* hungarian_new(int n_rows, int n_cols, double* costs) {
    if (!costs || n_rows <= 0 || n_cols <= 0) return NULL;
    int max_dim = (n_rows > n_cols) ? n_rows : n_cols;
    Hungarian* h = (Hungarian*)malloc(sizeof(Hungarian));
    if (!h) return NULL;
    
    // Allocate memory for costs
    h->costs = (double*)malloc(max_dim * max_dim * sizeof(double));
    if (!h->costs) {
        free(h);
        return NULL;
    }
    
    // Allocate memory for arrays
    h->row_covered = (int*)calloc(max_dim, sizeof(int));
    h->col_covered = (int*)calloc(max_dim, sizeof(int));
    h->starred_zeros = (int*)malloc(max_dim * sizeof(int));
    h->primed_zeros = (int*)malloc(max_dim * sizeof(int));
    
    if (!h->row_covered || !h->col_covered || 
        !h->starred_zeros || !h->primed_zeros) {
        if (h->row_covered) free(h->row_covered);
        if (h->col_covered) free(h->col_covered);
        if (h->starred_zeros) free(h->starred_zeros);
        if (h->primed_zeros) free(h->primed_zeros);
        free(h->costs);
        free(h);
        return NULL;
    }
    
    h->n_rows = max_dim;
    h->n_cols = max_dim;
    
    // Initialize arrays
    memset(h->costs, 0, max_dim * max_dim * sizeof(double));
    for (int i = 0; i < max_dim; i++) {
        h->starred_zeros[i] = -1;
        h->primed_zeros[i] = -1;
    }
    
    /* Negate costs so that maximizing score becomes minimizing cost */
    for (int i = 0; i < n_rows; i++) {
        for (int j = 0; j < n_cols; j++) {
            h->costs[i * max_dim + j] = -costs[i * n_cols + j];
        }
    }
    return h;
}

void hungarian_free(Hungarian* h) {
    if (h) {
        if (h->costs) free(h->costs);
        if (h->row_covered) free(h->row_covered);
        if (h->col_covered) free(h->col_covered);
        if (h->starred_zeros) free(h->starred_zeros);
        if (h->primed_zeros) free(h->primed_zeros);
        free(h);
    }
}

static void step1(Hungarian* h) {
    for (int i = 0; i < h->n_rows; i++) {
        double min_val = h->costs[i * h->n_cols];
        for (int j = 1; j < h->n_cols; j++) {
            if (h->costs[i * h->n_cols + j] < min_val)
                min_val = h->costs[i * h->n_cols + j];
        }
        for (int j = 0; j < h->n_cols; j++) {
            h->costs[i * h->n_cols + j] -= min_val;
        }
    }
}

static void step2(Hungarian* h) {
    for (int j = 0; j < h->n_cols; j++) {
        double min_val = h->costs[j];
        for (int i = 1; i < h->n_rows; i++) {
            if (h->costs[i * h->n_cols + j] < min_val)
                min_val = h->costs[i * h->n_cols + j];
        }
        for (int i = 0; i < h->n_rows; i++) {
            h->costs[i * h->n_cols + j] -= min_val;
        }
    }
}

static void step3(Hungarian* h) {
    for (int i = 0; i < h->n_rows; i++) {
        for (int j = 0; j < h->n_cols; j++) {
            if (h->costs[i * h->n_cols + j] == 0 &&
                !h->row_covered[i] && !h->col_covered[j]) {
                h->starred_zeros[i] = j;
                h->row_covered[i] = 1;
                h->col_covered[j] = 1;
                break;
            }
        }
    }
    memset(h->row_covered, 0, h->n_rows * sizeof(int));
    memset(h->col_covered, 0, h->n_cols * sizeof(int));
}

static int step4(Hungarian* h) {
    int count = 0;
    for (int i = 0; i < h->n_rows; i++) {
        if (h->starred_zeros[i] != -1) {
            h->col_covered[h->starred_zeros[i]] = 1;
            count++;
        }
    }
    return count;
}

static int find_zero(Hungarian* h, int* row, int* col) {
    for (int i = 0; i < h->n_rows; i++) {
        if (!h->row_covered[i]) {
            for (int j = 0; j < h->n_cols; j++) {
                if (!h->col_covered[j] && h->costs[i * h->n_cols + j] == 0) {
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
    // Allocate path arrays on heap instead of stack
    int* path_row = (int*)malloc(h->n_rows * h->n_cols * sizeof(int));
    int* path_col = (int*)malloc(h->n_rows * h->n_cols * sizeof(int));
    
    if (!path_row || !path_col) {
        if (path_row) free(path_row);
        if (path_col) free(path_col);
        error("Memory allocation failed in augment_path");
        return;
    }
    
    int path_count = 0;
    path_row[path_count] = row;
    path_col[path_count] = col;
    
    while (1) {
        int star_row = -1;
        for (int i = 0; i < h->n_rows; i++) {
            if (h->starred_zeros[i] == path_col[path_count]) {
                star_row = i;
                break;
            }
        }
        if (star_row == -1) break;
        path_count++;
        path_row[path_count] = star_row;
        path_col[path_count] = path_col[path_count - 1];
        int prime_col = h->primed_zeros[star_row];
        path_count++;
        path_row[path_count] = path_row[path_count - 1];
        path_col[path_count] = prime_col;
    }
    
    for (int i = 0; i <= path_count; i++) {
        if (h->starred_zeros[path_row[i]] == path_col[i])
            h->starred_zeros[path_row[i]] = -1;
        else
            h->starred_zeros[path_row[i]] = path_col[i];
    }
    
    free(path_row);
    free(path_col);
    
    memset(h->row_covered, 0, h->n_rows * sizeof(int));
    memset(h->col_covered, 0, h->n_cols * sizeof(int));
    memset(h->primed_zeros, -1, h->n_rows * sizeof(int));
}

static double find_smallest_uncovered(Hungarian* h) {
    double min_val = INFINITY;
    for (int i = 0; i < h->n_rows; i++) {
        if (!h->row_covered[i]) {
            for (int j = 0; j < h->n_cols; j++) {
                if (!h->col_covered[j] && h->costs[i * h->n_cols + j] < min_val)
                    min_val = h->costs[i * h->n_cols + j];
            }
        }
    }
    return min_val;
}

int* solve_hungarian(int n_rows, int n_cols, double* costs) {
    Hungarian* h = hungarian_new(n_rows, n_cols, costs);
    if (!h) {
        error("Failed to initialize Hungarian algorithm");
        return NULL;
    }
    
    int* assignment = (int*)malloc(n_rows * sizeof(int));
    if (!assignment) {
        hungarian_free(h);
        error("Memory allocation failed in solve_hungarian");
        return NULL;
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
                for (int i = 0; i < h->n_rows; i++) {
                    for (int j = 0; j < h->n_cols; j++) {
                        if (h->row_covered[i])
                            h->costs[i * h->n_cols + j] += min_val;
                        if (!h->col_covered[j])
                            h->costs[i * h->n_cols + j] -= min_val;
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

/* --- End Hungarian Algorithm --- */

/* --- Helper for Factor-like Behavior --- */

/* Comparison function for qsort */
static int compare_doubles(const void *a, const void *b) {
    double da = *(const double*)a;
    double db = *(const double*)b;
    return (da < db) ? -1 : (da > db) ? 1 : 0;
}

/* Generate factor indices (1-indexed) for a numeric vector using R's default behavior.
   'data' has length n and 'keep_idx' holds the indices of complete cases.
   Returns a newly allocated array of length n (to be freed later). */
static int* generate_factor_indices(double *data, int *keep_idx, int n) {
    // Build the unique array from data[keep_idx]
    double *unique = (double*) malloc(n * sizeof(double));
    if (!unique) {
        error("Memory allocation failed in generate_factor_indices");
        return NULL;
    }
    
    int unique_count = 0;
    for (int i = 0; i < n; i++) {
        double val = data[keep_idx[i]];
        int exists = 0;
        for (int j = 0; j < unique_count; j++) {
            if (unique[j] == val) { exists = 1; break; }
        }
        if (!exists) {
            unique[unique_count++] = val;
        }
    }
    // For numeric vectors, factor() sorts the levels.
    qsort(unique, unique_count, sizeof(double), compare_doubles);

    int *factors = (int*) malloc(n * sizeof(int));
    if (!factors) {
        free(unique);
        error("Memory allocation failed in generate_factor_indices");
        return NULL;
    }
    
    for (int i = 0; i < n; i++) {
        double val = data[keep_idx[i]];
        int factor = 0;
        for (int j = 0; j < unique_count; j++) {
            if (unique[j] == val) {
                factor = j + 1; // 1-indexed
                break;
            }
        }
        factors[i] = factor;
    }
    free(unique);
    return factors;
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
    
    double *x_data = REAL(x);
    double *y_data = REAL(y);
    
    if (!x_data || !y_data)
        error("Null input data.");

    /* --- Calculate sums for unique m/z values (using first occurrence) --- */
    double x_sum = 0.0, y_sum = 0.0;
    double *seen_x = (double*)malloc(n * sizeof(double));
    double *seen_y = (double*)malloc(n * sizeof(double));
    
    if (!seen_x || !seen_y) {
        if (seen_x) free(seen_x);
        if (seen_y) free(seen_y);
        error("Memory allocation failed.");
    }
    
    int seen_x_count = 0, seen_y_count = 0;
    
    for (int i = 0; i < n; i++) {
        double x_mz = x_data[i];
        if (!ISNA(x_mz)) {
            if (!find_value(x_mz, seen_x, seen_x_count)) {
                seen_x[seen_x_count++] = x_mz;
                x_sum += x_data[i + n];
            }
        }
    }
    
    for (int i = 0; i < n; i++) {
        double y_mz = y_data[i];
        if (!ISNA(y_mz)) {
            if (!find_value(y_mz, seen_y, seen_y_count)) {
                seen_y[seen_y_count++] = y_mz;
                y_sum += y_data[i + n];
            }
        }
    }
    
    free(seen_x);
    free(seen_y);
    
    if (x_sum == 0.0 || y_sum == 0.0)
        return ScalarReal(0.0);

    /* --- Keep only complete cases (non-NA for m/z values) --- */
    int *keep_idx = (int*)malloc(n * sizeof(int));
    if (!keep_idx) error("Memory allocation failed.");
    
    int l = 0;
    for (int i = 0; i < n; i++) {
        if (!ISNA(x_data[i]) && !ISNA(y_data[i])) {
            keep_idx[l++] = i;
        }
    }
    
    if (l == 0) {
        free(keep_idx);
        return ScalarReal(0.0);
    }

    /* --- Calculate normalized intensities --- */
    double *scores = (double*)malloc(l * sizeof(double));
    if (!scores) {
        free(keep_idx);
        error("Memory allocation failed.");
    }
    
    for (int i = 0; i < l; i++) {
        int idx = keep_idx[i];
        scores[i] = (sqrt(x_data[idx + n]) / sqrt(x_sum)) *
                    (sqrt(y_data[idx + n]) / sqrt(y_sum));
    }

    /* --- Generate factor indices for x and y m/z values --- */
    int *x_idx = generate_factor_indices(x_data, keep_idx, l);
    int *y_idx = generate_factor_indices(y_data, keep_idx, l);
    
    if (!x_idx || !y_idx) {
        if (x_idx) free(x_idx);
        if (y_idx) free(y_idx);
        free(keep_idx);
        free(scores);
        return ScalarReal(0.0);
    }

    /* --- Determine matrix dimension (number of unique levels) --- */
    int max_x = 0, max_y = 0;
    for (int i = 0; i < l; i++) {
        if (x_idx[i] > max_x) max_x = x_idx[i];
        if (y_idx[i] > max_y) max_y = y_idx[i];
    }
    int m = (max_x > max_y) ? max_x : max_y;

    /* --- Create and fill score matrix in R's column-major order --- */
    double *score_mat_R = (double*)calloc(m * m, sizeof(double));
    if (!score_mat_R) {
        free(keep_idx); free(scores); free(x_idx); free(y_idx);
        error("Memory allocation failed.");
    }
    
    for (int i = 0; i < l; i++) {
        score_mat_R[(y_idx[i] - 1) * m + (x_idx[i] - 1)] = scores[i];
    }

    /* Convert the R-style (column-major) matrix to row-major order */
    double *score_mat = (double*)calloc(m * m, sizeof(double));
    if (!score_mat) {
        free(keep_idx); free(scores); free(x_idx); free(y_idx); free(score_mat_R);
        error("Memory allocation failed.");
    }
    
    for (int i = 0; i < m; i++) {
        for (int j = 0; j < m; j++) {
            score_mat[i * m + j] = score_mat_R[i + j * m];
        }
    }
    free(score_mat_R);

    /* --- Solve assignment problem on an m x m matrix --- */
    int* assignment = solve_hungarian(m, m, score_mat);
    
    if (!assignment) {
        free(keep_idx); free(scores); free(x_idx); free(y_idx); free(score_mat);
        return ScalarReal(0.0);
    }

    /* --- Calculate final score --- */
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
