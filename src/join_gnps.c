#include <R.h>
#include <Rinternals.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

// Structure to hold mass and index for sorting
typedef struct {
    double mass;
    R_xlen_t index;
} MassIndex;

// Comparison function for qsort
static int compare_mass(const void* a, const void* b) {
    const double mass_a = ((const MassIndex*)a)->mass;
    const double mass_b = ((const MassIndex*)b)->mass;
    return (mass_a > mass_b) - (mass_a < mass_b);
}

// Inline binary search to find the first index with mass >= lower_bound
static inline R_xlen_t binary_search_mass(const MassIndex* arr, R_xlen_t n, double lower_bound) {
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
    
    /* Extract pointers and parameters */
    double *x_ptr = REAL(x);
    double *y_ptr = REAL(y);
    const double x_precursor = asReal(xPrecursorMz);
    const double y_precursor = asReal(yPrecursorMz);
    const double tol = asReal(tolerance);
    const double ppm_val = asReal(ppm);

    const R_xlen_t x_size = xlength(x);
    const R_xlen_t y_size = xlength(y);
    const double pdiff = y_precursor - x_precursor;

    /* Allocate arrays for sorting */
    MassIndex *x_sorted = (MassIndex*) calloc((size_t)x_size, sizeof(MassIndex));
    MassIndex *y_sorted = (MassIndex*) calloc((size_t)y_size, sizeof(MassIndex));
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
    qsort(x_sorted, (size_t)valid_x, sizeof(MassIndex), compare_mass);
    qsort(y_sorted, (size_t)valid_y, sizeof(MassIndex), compare_mass);

    /* Estimate maximum matches (may overestimate but ensures enough space) */
    const R_xlen_t max_matches = (valid_x * valid_y * 2) + na_count_x + y_size;

    int protect_count = 0;
    SEXP matches_x = allocVector(INTSXP, max_matches); PROTECT(matches_x); protect_count++;
    SEXP matches_y = allocVector(INTSXP, max_matches); PROTECT(matches_y); protect_count++;
    int *matches_x_ptr = INTEGER(matches_x);
    int *matches_y_ptr = INTEGER(matches_y);
    char *y_used = (char*) calloc((size_t)y_size, sizeof(char));
    if (!y_used) {
        free(x_sorted);
        free(y_sorted);
        error("Memory allocation failed");
    }

    R_xlen_t match_count = 0;

    /* Record NA matches from x */
    for (R_xlen_t i = 0; i < x_size; i++) {
        if (ISNAN(x_ptr[i])) {
            matches_x_ptr[match_count] = (int)(i + 1);
            matches_y_ptr[match_count] = NA_INTEGER;
            match_count++;
        }
    }

    /* Find matches for each valid x */
    for (R_xlen_t i = 0; i < valid_x; i++) {
        const double x_mass = x_sorted[i].mass;
        const double allowed_diff = tol + (ppm_val * x_mass * 1e-6);
        const double lower_bound = x_mass - allowed_diff;
        const double upper_bound = x_mass + allowed_diff;

        const R_xlen_t start_idx = binary_search_mass(y_sorted, valid_y, lower_bound);
        int found_match = 0;
        for (R_xlen_t j = start_idx; j < valid_y && y_sorted[j].mass <= upper_bound; j++) {
            matches_x_ptr[match_count] = (int)(x_sorted[i].index + 1);
            matches_y_ptr[match_count] = (int)(y_sorted[j].index + 1);
            y_used[y_sorted[j].index] = 1;
            match_count++;
            found_match = 1;
        }
        if (!found_match) {
            matches_x_ptr[match_count] = (int)(x_sorted[i].index + 1);
            matches_y_ptr[match_count] = NA_INTEGER;
            match_count++;
        }
    }

    /* Precursor mass difference matching */
    if (!ISNAN(x_precursor) && !ISNAN(y_precursor)) {
        for (R_xlen_t i = 0; i < valid_x; i++) {
            const double x_mass = x_sorted[i].mass;
            const double x_adjusted = x_mass + pdiff;
            const double allowed_diff = tol + (ppm_val * x_adjusted * 1e-6);
            const double lower_bound = x_adjusted - allowed_diff;
            const double upper_bound = x_adjusted + allowed_diff;

            const R_xlen_t start_idx = binary_search_mass(y_sorted, valid_y, lower_bound);
            for (R_xlen_t j = start_idx; j < valid_y && y_sorted[j].mass <= upper_bound; j++) {
                matches_x_ptr[match_count] = (int)(x_sorted[i].index + 1);
                matches_y_ptr[match_count] = (int)(y_sorted[j].index + 1);
                match_count++;
            }
        }
    }

    /* Add unmatched y values */
    for (R_xlen_t i = 0; i < y_size; i++) {
        if (!ISNAN(y_ptr[i]) && !y_used[i]) {
            matches_x_ptr[match_count] = NA_INTEGER;
            matches_y_ptr[match_count] = (int)(i + 1);
            match_count++;
        }
    }

    /* Allocate final result vectors and copy the matches */
    SEXP result_x = allocVector(INTSXP, match_count); PROTECT(result_x); protect_count++;
    SEXP result_y = allocVector(INTSXP, match_count); PROTECT(result_y); protect_count++;
    memcpy(INTEGER(result_x), matches_x_ptr, (size_t)match_count * sizeof(int));
    memcpy(INTEGER(result_y), INTEGER(matches_y), (size_t)match_count * sizeof(int));

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
