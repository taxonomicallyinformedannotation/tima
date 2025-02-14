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
    double mass_a = ((MassIndex*)a)->mass;
    double mass_b = ((MassIndex*)b)->mass;
    return (mass_a > mass_b) - (mass_a < mass_b);
}

SEXP join_gnps(SEXP x, SEXP y, SEXP xPrecursorMz, SEXP yPrecursorMz, SEXP tolerance, SEXP ppm) {
    if (!isReal(x) || !isReal(y)) {
        error("x and y must be numeric vectors");
    }

    double* x_ptr = REAL(x);
    double* y_ptr = REAL(y);
    double x_precursor = asReal(xPrecursorMz);
    double y_precursor = asReal(yPrecursorMz);
    double tol = asReal(tolerance);
    double ppm_val = asReal(ppm);

    R_xlen_t x_size = xlength(x);
    R_xlen_t y_size = xlength(y);
    double pdiff = y_precursor - x_precursor;

    // Allocate sorting structures
    MassIndex* x_sorted = (MassIndex*)calloc(x_size, sizeof(MassIndex));
    MassIndex* y_sorted = (MassIndex*)calloc(y_size, sizeof(MassIndex));

    if (!x_sorted || !y_sorted) {
        free(x_sorted);
        free(y_sorted);
        error("Memory allocation failed");
    }

    // Filter valid values and initialize sorting arrays
    R_xlen_t valid_x = 0, valid_y = 0;
    R_xlen_t na_count_x = 0;
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

    // Sort arrays
    qsort(x_sorted, valid_x, sizeof(MassIndex), compare_mass);
    qsort(y_sorted, valid_y, sizeof(MassIndex), compare_mass);

    // Calculate maximum possible matches
    // Each x can match with all y values in worst case, plus precursor matches
    R_xlen_t max_matches = (valid_x * valid_y * 2) + na_count_x + y_size;

    // Allocate result vectors
    SEXP matches_x = PROTECT(allocVector(INTSXP, max_matches));
    SEXP matches_y = PROTECT(allocVector(INTSXP, max_matches));
    int* matches_x_ptr = INTEGER(matches_x);
    int* matches_y_ptr = INTEGER(matches_y);
    char* y_used = (char*)calloc(y_size, sizeof(char));

    if (!y_used) {
        free(x_sorted);
        free(y_sorted);
        error("Memory allocation failed");
    }

    R_xlen_t match_count = 0;

    // Handle NAs in x first
    for (R_xlen_t i = 0; i < x_size; i++) {
        if (ISNAN(x_ptr[i])) {
            matches_x_ptr[match_count] = i + 1;
            matches_y_ptr[match_count] = NA_INTEGER;
            match_count++;
        }
    }

    // Find matches using binary search
    for (R_xlen_t i = 0; i < valid_x; i++) {
        double x_mass = x_sorted[i].mass;
        double allowed_diff = tol + (ppm_val * x_mass * 1e-6);
        double lower_bound = x_mass - allowed_diff;
        double upper_bound = x_mass + allowed_diff;

        // Binary search for lower bound
        R_xlen_t left = 0, right = valid_y - 1;
        R_xlen_t start_idx = valid_y;
        while (left <= right) {
            R_xlen_t mid = left + (right - left) / 2;
            if (y_sorted[mid].mass >= lower_bound) {
                start_idx = mid;
                right = mid - 1;
            } else {
                left = mid + 1;
            }
        }

        // Scan through potential matches
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

    // Handle precursor mass difference matches
    if (!ISNAN(x_precursor) && !ISNAN(y_precursor)) {
        for (R_xlen_t i = 0; i < valid_x; i++) {
            double x_adjusted = x_sorted[i].mass + pdiff;
            double allowed_diff = tol + (ppm_val * x_sorted[i].mass * 1e-6);
            double lower_bound = x_adjusted - allowed_diff;
            double upper_bound = x_adjusted + allowed_diff;

            // Binary search for lower bound
            R_xlen_t left = 0, right = valid_y - 1, start_idx = valid_y;
            while (left <= right) {
                R_xlen_t mid = left + (right - left) / 2;
                if (y_sorted[mid].mass >= lower_bound) {
                    start_idx = mid;
                    right = mid - 1;
                } else {
                    left = mid + 1;
                }
            }

            // Scan through potential matches
            for (R_xlen_t j = start_idx; j < valid_y && y_sorted[j].mass <= upper_bound; j++) {
                matches_x_ptr[match_count] = x_sorted[i].index + 1;
                matches_y_ptr[match_count] = y_sorted[j].index + 1;
                match_count++;
            }
        }
    }

    // Add unmatched y values
    for (R_xlen_t i = 0; i < y_size; i++) {
        if (!ISNAN(y_ptr[i]) && !y_used[i]) {
            matches_x_ptr[match_count] = NA_INTEGER;
            matches_y_ptr[match_count] = i + 1;
            match_count++;
        }
    }

    // Create final result vectors
    SEXP result_x = PROTECT(allocVector(INTSXP, match_count));
    SEXP result_y = PROTECT(allocVector(INTSXP, match_count));
    memcpy(INTEGER(result_x), matches_x_ptr, match_count * sizeof(int));
    memcpy(INTEGER(result_y), matches_y_ptr, match_count * sizeof(int));

    // Create named list for return
    SEXP result = PROTECT(allocVector(VECSXP, 2));
    SET_VECTOR_ELT(result, 0, result_x);
    SET_VECTOR_ELT(result, 1, result_y);
    UNPROTECT(5);

    free(x_sorted);
    free(y_sorted);
    free(y_used);

    return result;
}