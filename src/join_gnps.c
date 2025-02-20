#include <R.h>
#include <Rinternals.h>
#include <stdlib.h>
#include <math.h>

// Structure to hold mass and index for sorting
typedef struct {
    double mass;
    R_xlen_t index;
} MassIndex;

// Comparison function for qsort
static int compare_mass(const void* a, const void* b) {
    double mass_a = ((const MassIndex*)a)->mass;
    double mass_b = ((const MassIndex*)b)->mass;
    return (mass_a > mass_b) - (mass_a < mass_b);
}

// Inline binary search to find the first index with mass >= lower_bound
static inline R_xlen_t binary_search_mass(const MassIndex* restrict arr, R_xlen_t n, double lower_bound) {
    R_xlen_t left = 0, right = n - 1, mid, start_idx = n;
    while (left <= right) {
        mid = left + (right - left) / 2;
        if (arr[mid].mass >= lower_bound) {
            start_idx = mid;
            right = mid - 1;
        } else {
            left = mid + 1;
        }
    }
    return start_idx;
}

SEXP join_gnps(SEXP x, SEXP y, SEXP xPrecursorMz, SEXP yPrecursorMz, 
               SEXP tolerance, SEXP ppm) {
    /* Input validation */
    if (!isReal(x) || !isReal(y)) {
        error("x and y must be numeric vectors");
    }
    
    /* Extract pointers and parameters (with restrict for optimization) */
    double *restrict x_ptr = REAL(x);
    double *restrict y_ptr = REAL(y);
    double x_precursor = asReal(xPrecursorMz);
    double y_precursor = asReal(yPrecursorMz);
    double tol = asReal(tolerance);
    double ppm_val = asReal(ppm);

    R_xlen_t x_size = xlength(x);
    R_xlen_t y_size = xlength(y);
    double pdiff = y_precursor - x_precursor;

    /* Allocate arrays for sorting */
    MassIndex *restrict x_sorted = (MassIndex*) calloc(x_size, sizeof(MassIndex));
    MassIndex *restrict y_sorted = (MassIndex*) calloc(y_size, sizeof(MassIndex));
    if (!x_sorted || !y_sorted) {
        free(x_sorted);
        free(y_sorted);
        error("Memory allocation failed");
    }

    /* Filter valid values into sorted arrays */
    R_xlen_t valid_x = 0, valid_y = 0, na_count_x = 0;
    for (R_xlen_t i = 0; i < x_size; i++) {
        if (!ISNAN(x_ptr[i])) {
            x_sorted[valid_x].mass = x_ptr[i];
            x_sorted[valid_x].index = i;
            valid_x++;
        } else {
            na_count_x++;
        }
    }
    for (R_xlen_t i = 0; i < y_size; i++) {
        if (!ISNAN(y_ptr[i])) {
            y_sorted[valid_y].mass = y_ptr[i];
            y_sorted[valid_y].index = i;
            valid_y++;
        }
    }

    /* Sort valid entries */
    qsort(x_sorted, valid_x, sizeof(MassIndex), compare_mass);
    qsort(y_sorted, valid_y, sizeof(MassIndex), compare_mass);

    /* Estimate maximum matches (may overestimate but ensures enough space) */
    R_xlen_t max_matches = (valid_x * valid_y * 2) + na_count_x + y_size;

    int protect_count = 0;
    SEXP matches_x = allocVector(INTSXP, max_matches); PROTECT(matches_x); protect_count++;
    SEXP matches_y = allocVector(INTSXP, max_matches); PROTECT(matches_y); protect_count++;
    int *restrict matches_x_ptr = INTEGER(matches_x);
    int *restrict matches_y_ptr = INTEGER(matches_y);
    char *restrict y_used = (char*) calloc(y_size, sizeof(char));
    if (!y_used) {
        free(x_sorted);
        free(y_sorted);
        error("Memory allocation failed");
    }

    R_xlen_t match_count = 0;

    /* Record NA matches from x */
    for (R_xlen_t i = 0; i < x_size; i++) {
        if (ISNAN(x_ptr[i])) {
            matches_x_ptr[match_count] = i + 1;
            matches_y_ptr[match_count] = NA_INTEGER;
            match_count++;
        }
    }

    /* Find matches for each valid x */
    for (R_xlen_t i = 0; i < valid_x; i++) {
        double x_mass = x_sorted[i].mass;
        double allowed_diff = tol + (ppm_val * x_mass * 1e-6);
        double lower_bound = x_mass - allowed_diff;
        double upper_bound = x_mass + allowed_diff;

        R_xlen_t start_idx = binary_search_mass(y_sorted, valid_y, lower_bound);
        int found_match = 0;
        for (R_xlen_t j = start_idx; j < valid_y && y_sorted[j].mass <= upper_bound; j++) {
            matches_x_ptr[match_count] = x_sorted[i].index + 1;
            matches_y_ptr[match_count] = y_sorted[j].index + 1;
            y_used[y_sorted[j].index] = 1;
            match_count++;
            found_match = 1;
        }
        if (!found_match) {
            matches_x_ptr[match_count] = x_sorted[i].index + 1;
            matches_y_ptr[match_count] = NA_INTEGER;
            match_count++;
        }
    }

    /* Precursor mass difference matching */
    if (!ISNAN(x_precursor) && !ISNAN(y_precursor)) {
        for (R_xlen_t i = 0; i < valid_x; i++) {
            double x_mass = x_sorted[i].mass;
            double x_adjusted = x_mass + pdiff;
            double allowed_diff = tol + (ppm_val * x_adjusted * 1e-6);
            double lower_bound = x_adjusted - allowed_diff;
            double upper_bound = x_adjusted + allowed_diff;

            R_xlen_t start_idx = binary_search_mass(y_sorted, valid_y, lower_bound);
            for (R_xlen_t j = start_idx; j < valid_y && y_sorted[j].mass <= upper_bound; j++) {
                matches_x_ptr[match_count] = x_sorted[i].index + 1;
                matches_y_ptr[match_count] = y_sorted[j].index + 1;
                match_count++;
            }
        }
    }

    /* Add unmatched y values */
    for (R_xlen_t i = 0; i < y_size; i++) {
        if (!ISNAN(y_ptr[i]) && !y_used[i]) {
            matches_x_ptr[match_count] = NA_INTEGER;
            matches_y_ptr[match_count] = i + 1;
            match_count++;
        }
    }

    /* Allocate final result vectors and copy the matches */
    SEXP result_x = allocVector(INTSXP, match_count); PROTECT(result_x); protect_count++;
    SEXP result_y = allocVector(INTSXP, match_count); PROTECT(result_y); protect_count++;
    memcpy(INTEGER(result_x), matches_x_ptr, match_count * sizeof(int));
    memcpy(INTEGER(result_y), INTEGER(matches_y), match_count * sizeof(int));

    SEXP result = allocVector(VECSXP, 2); PROTECT(result); protect_count++;
    SET_VECTOR_ELT(result, 0, result_x);
    SET_VECTOR_ELT(result, 1, result_y);
    
    SEXP names = allocVector(STRSXP, 2); PROTECT(names); protect_count++;
    SET_STRING_ELT(names, 0, mkChar("x"));
    SET_STRING_ELT(names, 1, mkChar("y"));
    setAttrib(result, R_NamesSymbol, names);

    UNPROTECT(protect_count);

    free(x_sorted);
    free(y_sorted);
    free(y_used);

    return result;
}
