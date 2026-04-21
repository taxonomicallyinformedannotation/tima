#ifndef GNPS_PORTABLE_H
#define GNPS_PORTABLE_H

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
  double score;
  int matches;
  double score_forward;
  double score_reverse;
} GnpsCoreResult;

typedef struct {
  int *x_idx;
  int *y_idx;
  int n_rows;
} GnpsJoinResult;

/*
 * Portable GNPS chain-DP API.
 * Returns 0 on success, non-zero on invalid inputs.
 */
int gnps_chain_dp_core_api(
    const double *x_mz, const double *x_int, int nx,
    const double *y_mz, const double *y_int, int ny,
    double x_pre, double y_pre,
    double tol, double ppm_val,
    GnpsCoreResult *out);

/*
 * Portable pre-aligned GNPS scoring API.
 * `x_mz`/`y_mz` may contain NaN to indicate missing side for a row.
 * Returns 0 on success, non-zero on invalid inputs.
 */
int gnps_aligned_core_api(
    const double *x_mz, const double *x_int,
    const double *y_mz, const double *y_int,
    int n,
    GnpsCoreResult *out);

/*
 * Portable join API matching MsCoreUtils::join_gnps semantics.
 * Indices in result are 0-based, unmatched side uses -1.
 * Output arrays are heap-allocated by this library and must be released
 * with gnps_free_ptr().
 * Returns 0 on success, non-zero on invalid inputs/allocation failures.
 */
int gnps_join_core_api(
    const double *x, int nx,
    const double *y, int ny,
    double x_pre, double y_pre,
    double tol, double ppm_val,
    GnpsJoinResult *out);

/* Releases buffers returned by gnps_join_core_api. */
void gnps_free_ptr(void *ptr);

#ifdef __cplusplus
}
#endif

#endif

