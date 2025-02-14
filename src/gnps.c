#include <R.h>
#include <Rinternals.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    int n_rows;
    int n_cols;
    double* costs;
    int* row_covered;
    int* col_covered;
    int* starred_zeros;
    int* primed_zeros;
} Hungarian;

// Helper function to find if value exists in array
static int find_value(double value, double* array, int size) {
    if (!array) return 0;
    for (int i = 0; i < size; i++) {
        if (array[i] == value) return 1;
    }
    return 0;
}

Hungarian* hungarian_new(int n_rows, int n_cols, double* costs) {
    if (!costs || n_rows <= 0 || n_cols <= 0) return NULL;

    int max_dim = (n_rows > n_cols) ? n_rows : n_cols;
    size_t total_size = (max_dim * max_dim * sizeof(double)) + (4 * max_dim * sizeof(int));

    Hungarian* h = (Hungarian*)malloc(sizeof(Hungarian));
    if (!h) return NULL;

    char* mem = (char*)malloc(total_size);
    if (!mem) {
        free(h);
        return NULL;
    }

    h->n_rows = max_dim;
    h->n_cols = max_dim;
    h->costs = (double*)mem;
    h->row_covered = (int*)(mem + (max_dim * max_dim * sizeof(double)));
    h->col_covered = h->row_covered + max_dim;
    h->starred_zeros = h->col_covered + max_dim;
    h->primed_zeros = h->starred_zeros + max_dim;

    memset(h->costs, 0, max_dim * max_dim * sizeof(double));
    memset(h->row_covered, 0, 4 * max_dim * sizeof(int));

    for (int i = 0; i < max_dim; i++) {
        h->starred_zeros[i] = -1;
        h->primed_zeros[i] = -1;
    }

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
        free(h);
    }
}

static void step1(Hungarian* h) {
    for (int i = 0; i < h->n_rows; i++) {
        double min_val = h->costs[i * h->n_cols];
        for (int j = 1; j < h->n_cols; j++) {
            if (h->costs[i * h->n_cols + j] < min_val) {
                min_val = h->costs[i * h->n_cols + j];
            }
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
            if (h->costs[i * h->n_cols + j] < min_val) {
                min_val = h->costs[i * h->n_cols + j];
            }
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
    int path_count = 0;
    int path_row[h->n_rows * h->n_cols];
    int path_col[h->n_rows * h->n_cols];

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
        if (h->starred_zeros[path_row[i]] == path_col[i]) {
            h->starred_zeros[path_row[i]] = -1;
        } else {
            h->starred_zeros[path_row[i]] = path_col[i];
        }
    }

    memset(h->row_covered, 0, h->n_rows * sizeof(int));
    memset(h->col_covered, 0, h->n_cols * sizeof(int));
    memset(h->primed_zeros, -1, h->n_rows * sizeof(int));
}

static double find_smallest_uncovered(Hungarian* h) {
    double min_val = INFINITY;
    for (int i = 0; i < h->n_rows; i++) {
        if (!h->row_covered[i]) {
            for (int j = 0; j < h->n_cols; j++) {
                if (!h->col_covered[j] && h->costs[i * h->n_cols + j] < min_val) {
                    min_val = h->costs[i * h->n_cols + j];
                }
            }
        }
    }
    return min_val;
}

int* solve_hungarian(int n_rows, int n_cols, double* costs) {
    Hungarian* h = hungarian_new(n_rows, n_cols, costs);
    int* assignment = (int*)malloc(n_rows * sizeof(int));

    step1(h);
    step2(h);
    step3(h);

    while (1) {
        int covered_count = step4(h);
        if (covered_count >= h->n_rows || covered_count >= h->n_cols) {
            break;
        }

        while (1) {
            int row, col;
            if (!find_zero(h, &row, &col)) {
                double min_val = find_smallest_uncovered(h);
                for (int i = 0; i < h->n_rows; i++) {
                    for (int j = 0; j < h->n_cols; j++) {
                        if (h->row_covered[i]) {
                            h->costs[i * h->n_cols + j] += min_val;
                        }
                        if (!h->col_covered[j]) {
                            h->costs[i * h->n_cols + j] -= min_val;
                        }
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

SEXP gnps(SEXP x, SEXP y) {
    if (!isReal(x) || !isReal(y)) error("Inputs must be numeric matrices.");
    SEXP x_dim = getAttrib(x, R_DimSymbol);
    SEXP y_dim = getAttrib(y, R_DimSymbol);
    if (!isInteger(x_dim) || !isInteger(y_dim)) error("Dimensions must be integer vectors.");

    int n = INTEGER(x_dim)[0];
    if (n != INTEGER(y_dim)[0]) error("'x' and 'y' must have the same number of rows.");

    double *x_data = REAL(x);
    double *y_data = REAL(y);
    if (!x_data || !y_data) error("Null input data.");

    // Arrays for tracking unique values
    double *seen_x = (double*)malloc(n * sizeof(double));
    double *seen_y = (double*)malloc(n * sizeof(double));
    if (!seen_x || !seen_y) {
        free(seen_x);
        free(seen_y);
        error("Memory allocation failed.");
    }

    int seen_x_count = 0, seen_y_count = 0;
    double x_sum = 0.0, y_sum = 0.0;

    // First pass: find unique values and calculate sums (like the original hash table)
    for (int i = 0; i < n; i++) {
        double x_mz = x_data[i], y_mz = y_data[i];
        if (!ISNA(x_mz)) {
            if (!find_value(x_mz, seen_x, seen_x_count)) {
                seen_x[seen_x_count++] = x_mz;
                x_sum += x_data[i + n];
            }
        }
        if (!ISNA(y_mz)) {
            if (!find_value(y_mz, seen_y, seen_y_count)) {
                seen_y[seen_y_count++] = y_mz;
                y_sum += y_data[i + n];
            }
        }
    }

    if (x_sum == 0.0 || y_sum == 0.0) {
        free(seen_x);
        free(seen_y);
        return ScalarReal(0.0);
    }

    double x_norm = sqrt(1.0 / x_sum);
    double y_norm = sqrt(1.0 / y_sum);

    // Find non-NA pairs for assignment
    int *keep_idx = (int*)malloc(n * sizeof(int));
    if (!keep_idx) {
        free(seen_x);
        free(seen_y);
        error("Memory allocation failed.");
    }

    int l = 0;
    for (int i = 0; i < n; i++) {
        if (!ISNA(x_data[i]) && !ISNA(y_data[i])) {
            keep_idx[l++] = i;
        }
    }

    if (l == 0) {
        free(seen_x);
        free(seen_y);
        free(keep_idx);
        return ScalarReal(0.0);
    }

    double *score_mat = (double*)calloc(l * l, sizeof(double));
    if (!score_mat) {
        free(seen_x);
        free(seen_y);
        free(keep_idx);
        error("Memory allocation failed.");
    }

    for (int i = 0; i < l; i++) {
        int idx = keep_idx[i];
        score_mat[i * l + i] = sqrt(x_data[idx + n]) * x_norm * sqrt(y_data[idx + n]) * y_norm;
    }

    int* best = solve_hungarian(l, l, score_mat);
    if (!best) {
        free(score_mat);
        free(seen_x);
        free(seen_y);
        free(keep_idx);
        return ScalarReal(0.0);
    }

    double total_score = 0.0;
    for (int i = 0; i < l; i++) {
        int best_index = best[i] - 1;
        if (best_index >= 0 && best_index < l) {
            total_score += score_mat[i * l + best_index];
        }
    }

    free(best);
    free(score_mat);
    free(seen_x);
    free(seen_y);
    free(keep_idx);

    return ScalarReal(total_score);
}
