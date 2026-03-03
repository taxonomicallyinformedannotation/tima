/**
 * @file gnps.c
 * @brief GNPS modified cosine similarity — mathematically exact.
 *
 * IMPORTANT: Input spectra MUST be sanitized before calling these functions.
 * Sanitized spectra have:
 *   - Unique m/z values (no two peaks within matching tolerance)
 *   - Non-negative intensities, no NaN/NA intensities
 *   - Peaks sorted by m/z in ascending order
 *
 * In tima, this is guaranteed by sanitize_spectra() (called from
 * import_spectra(sanitize = TRUE)), which applies Spectra::reduceSpectra(),
 * Spectra::combinePeaks(), and Spectra::scalePeaks().
 *
 * Algorithm (gnps_compute, hot path):
 *   1. Y-centric closest-match for direct and shifted passes, replicating
 *      MsCoreUtils::join(type="outer") one-to-one semantics.
 *   2. If no shifted match conflicts with a direct match: O(n+m) greedy
 *      scoring (pick the better of direct/shifted per x peak).
 *   3. If conflicts exist: build a sparse score matrix from matched peaks
 *      and solve with the exact shortest-augmenting-path Hungarian O(k³),
 *      where k = number of involved peaks (typically very small).
 *
 * Scoring formula (matches MsCoreUtils::gnps exactly):
 *   score_ij = sqrt(x_int_i) / sqrt(sum_unique_x_int)
 *            * sqrt(y_int_j) / sqrt(sum_unique_y_int)
 *   total = sum of score_ij for optimally assigned pairs
 *
 * Exports:
 *   gnps_compute  — fused join+score from raw peak matrices (hot path)
 *   gnps          — score pre-aligned matrices (backward compat)
 *   join_gnps     — peak matching only (backward compat)
 */

#include <R.h>
#include <Rinternals.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>

/* ── helpers ─────────────────────────────────────────────────────────────── */

static void *xmalloc(size_t n) {
  void *p = malloc(n);
  if (!p) error("malloc(%zu)", n);
  return p;
}

static void *xcalloc(size_t n, size_t s) {
  void *p = calloc(n, s);
  if (!p) error("calloc(%zu)", n * s);
  return p;
}

/* ── bitset (index cast to unsigned to silence -Wsign-conversion) ────────── */

#define BS_SET(b, i)                                                           \
  ((b)[(unsigned)(i) >> 3] |= (unsigned char)(1u << ((unsigned)(i) & 7u)))
#define BS_TEST(b, i)                                                          \
  ((b)[(unsigned)(i) >> 3] & (unsigned char)(1u << ((unsigned)(i) & 7u)))

/* ── MassIndex for join_gnps ─────────────────────────────────────────────── */

typedef struct { double mass; int idx; } MI;

static int mi_cmp(const void *a, const void *b) {
  double da = ((const MI *)a)->mass, db = ((const MI *)b)->mass;
  return (da < db) ? -1 : (da > db) ? 1 : 0;
}

/* ── (key,pos) sort for gnps() backward compat ───────────────────────────── */

typedef struct { double key; int pos; } KP;

static int kp_cmp(const void *a, const void *b) {
  double da = ((const KP *)a)->key, db = ((const KP *)b)->key;
  return (da < db) ? -1 : (da > db) ? 1 : 0;
}

/* ── SEXP result builder ─────────────────────────────────────────────────── */

static SEXP make_result(double score, int matches) {
  SEXP r  = PROTECT(allocVector(VECSXP, 2));
  SEXP nm = PROTECT(allocVector(STRSXP, 2));
  SET_VECTOR_ELT(r, 0, ScalarReal(score));
  SET_VECTOR_ELT(r, 1, ScalarInteger(matches));
  SET_STRING_ELT(nm, 0, mkChar("score"));
  SET_STRING_ELT(nm, 1, mkChar("matches"));
  setAttrib(r, R_NamesSymbol, nm);
  UNPROTECT(2);
  return r;
}

/* ── scoring helper ──────────────────────────────────────────────────────── */

static inline double score_pair(double xi, double yj,
                                double inv_sx, double inv_sy) {
  return sqrt(xi) * inv_sx * sqrt(yj) * inv_sy;
}

/* ══════════════════════════════════════════════════════════════════════════ *
 * OPTIMAL ASSIGNMENT SCORER                                                *
 *                                                                          *
 * Y-centric closest-match for direct + shifted, then:                      *
 *   - Fast path (no conflicts): greedy O(n+m)                             *
 *   - Conflict path: exact Hungarian on involved peaks                     *
 * All workspace is heap-allocated to exact size — portable and fast.        *
 * ══════════════════════════════════════════════════════════════════════════ */

static double chain_dp_score(
    const double *x_mz, const double *x_int, int nx,
    const double *y_mz, const double *y_int, int ny,
    double inv_sx, double inv_sy,
    double pdiff, int do_shift,
    double tol, double ppm_val,
    int *out_matched)
{
  /* Single allocation for all workspace: dm[nx] sm[nx] bd[ny] */
  size_t int_need  = (size_t)(nx + nx + ny);
  size_t total_sz  = int_need * sizeof(int);
  char *buf = (char *)xmalloc(total_sz);

  int *dm_arr = (int *)buf;
  int *sm_arr = dm_arr + nx;
  int *bd_arr = sm_arr + nx;

  memset(dm_arr, 0xFF, (size_t)nx * sizeof(int));
  memset(sm_arr, 0xFF, (size_t)nx * sizeof(int));
  memset(bd_arr, 0xFF, (size_t)ny * sizeof(int));

  const double eps_tol = sqrt(DBL_EPSILON);

  /* ── Step 1: direct matching ──────────────────────────────────────────
   * Replicate MsCoreUtils::C_join_outer: for each y[j], find the closest
   * x[i] within tolerance (= tol + ppm(x[i]) + sqrt(eps)).  When two x
   * compete for the same y, the closer one wins.  Iterate y in order;
   * because both arrays are sorted, a sliding window on x suffices.     */
  {
    int ilo = 0;
    for (int j = 0; j < ny; j++) {
      double ym = y_mz[j];
      /* We need to check which x[i] values have ym within their own
       * per-element tolerance band: |x[i] - ym| <= tol+ppm(x[i])+eps.
       * Advance ilo past x values whose upper tolerance bound is below ym. */
      while (ilo < nx && x_mz[ilo] + tol + ppm_val * x_mz[ilo] * 1e-6 + eps_tol < ym)
        ilo++;
      int best_i = -1;
      double best_d = DBL_MAX;
      for (int i = ilo; i < nx; i++) {
        double xm = x_mz[i];
        if (xm - tol - ppm_val * xm * 1e-6 - eps_tol > ym) break;
        double ai = tol + ppm_val * xm * 1e-6 + eps_tol;
        double dij = fabs(xm - ym);
        if (dij <= ai && dij < best_d) { best_d = dij; best_i = i; }
      }
      if (best_i >= 0 && dm_arr[best_i] < 0) {
        dm_arr[best_i] = j; bd_arr[j] = best_i;
      } else if (best_i >= 0) {
        /* x[best_i] already matched to another y — keep the closer one */
        int prev_j = dm_arr[best_i];
        double prev_d = fabs(x_mz[best_i] - y_mz[prev_j]);
        if (best_d < prev_d) {
          bd_arr[prev_j] = -1;     /* release previous y */
          dm_arr[best_i] = j; bd_arr[j] = best_i;
        }
      }
    }
  }

  /* ── Step 2: shifted matching ──────────────────────────────────────── */
  /* x_mz[i]+pdiff vs y_mz[j], same closest-match from y's perspective.
   * Shifted matches don't conflict with each other (one-to-one forward
   * scan) but may conflict with direct matches (resolved in Step 3).    */
  if (do_shift) {
    int ilo = 0;
    for (int j = 0; j < ny; j++) {
      double ym = y_mz[j];
      /* Advance past shifted x values too far below ym */
      while (ilo < nx) {
        double shifted = x_mz[ilo] + pdiff;
        double ai = tol + ppm_val * fabs(shifted) * 1e-6 + eps_tol;
        if (shifted + ai >= ym) break;
        ilo++;
      }
      int best_i = -1;
      double best_d = DBL_MAX;
      for (int i = ilo; i < nx; i++) {
        double shifted = x_mz[i] + pdiff;
        double ai = tol + ppm_val * fabs(shifted) * 1e-6 + eps_tol;
        if (shifted - ai > ym) break;
        double dij = fabs(shifted - ym);
        if (dij <= ai && dij < best_d) {
          best_d = dij; best_i = i;
        }
      }
      if (best_i >= 0 && sm_arr[best_i] < 0) {
        sm_arr[best_i] = j;
      } else if (best_i >= 0) {
        /* x[best_i] already has a shifted match — keep closer one */
        int prev_j = sm_arr[best_i];
        double prev_shifted = x_mz[best_i] + pdiff;
        double prev_d = fabs(prev_shifted - y_mz[prev_j]);
        if (best_d < prev_d) sm_arr[best_i] = j;
      }
    }
  }

  /* ── Step 3: score optimally ─────────────────────────────────────────── *
   *                                                                       *
   * Check if any shifted match conflicts with a direct match (same y).    *
   * If no conflicts: score all matches directly in O(n+m).                *
   * If conflicts: build score matrix for involved peaks, run Hungarian.   *
   * ───────────────────────────────────────────────────────────────────── */

  int has_conflict = 0;
  for (int i = 0; i < nx && !has_conflict; i++)
    if (sm_arr[i] >= 0 && bd_arr[sm_arr[i]] >= 0 && bd_arr[sm_arr[i]] != i)
      has_conflict = 1;

  double total = 0.0;
  int matched = 0;

  if (!has_conflict) {
    /* Fast path: no conflicts — each y peak is used at most once.
     * For each x, pick the better of direct/shifted (both can't share y). */
    for (int i = 0; i < nx; i++) {
      int dm = dm_arr[i], sm = sm_arr[i];
      if (dm < 0 && sm < 0) continue;
      double ds = (dm >= 0) ? score_pair(x_int[i], y_int[dm], inv_sx, inv_sy) : 0.0;
      double ss = (sm >= 0) ? score_pair(x_int[i], y_int[sm], inv_sx, inv_sy) : 0.0;
      total += (ds >= ss) ? ds : ss;
      matched++;
    }
  } else {
    /* Conflict path: build score matrix and solve with Hungarian.
     * Collect all x-peaks and y-peaks involved in any match.             */
    int n_xm = 0, n_ym = 0;

    int *xi_map = (int *)xmalloc((size_t)nx * sizeof(int));
    memset(xi_map, 0xFF, (size_t)nx * sizeof(int));
    for (int i = 0; i < nx; i++)
      if (dm_arr[i] >= 0 || sm_arr[i] >= 0) xi_map[i] = n_xm++;

    int *yi_map = (int *)xmalloc((size_t)ny * sizeof(int));
    memset(yi_map, 0xFF, (size_t)ny * sizeof(int));
    for (int i = 0; i < nx; i++) {
      if (dm_arr[i] >= 0 && yi_map[dm_arr[i]] < 0) yi_map[dm_arr[i]] = n_ym++;
      if (sm_arr[i] >= 0 && yi_map[sm_arr[i]] < 0) yi_map[sm_arr[i]] = n_ym++;
    }

    int N = (n_xm > n_ym) ? n_xm : n_ym;
    if (N == 0) { free(xi_map); free(yi_map); free(buf);
                  *out_matched = 0; return 0.0; }

    double *smat = (double *)xcalloc((size_t)N * (size_t)N, sizeof(double));
    for (int i = 0; i < nx; i++) {
      int r = xi_map[i]; if (r < 0) continue;
      if (dm_arr[i] >= 0) {
        int c = yi_map[dm_arr[i]];
        double sc = score_pair(x_int[i], y_int[dm_arr[i]], inv_sx, inv_sy);
        if (sc > smat[(size_t)r * (size_t)N + (size_t)c])
          smat[(size_t)r * (size_t)N + (size_t)c] = sc;
      }
      if (sm_arr[i] >= 0) {
        int c = yi_map[sm_arr[i]];
        double sc = score_pair(x_int[i], y_int[sm_arr[i]], inv_sx, inv_sy);
        if (sc > smat[(size_t)r * (size_t)N + (size_t)c])
          smat[(size_t)r * (size_t)N + (size_t)c] = sc;
      }
    }
    free(xi_map); free(yi_map);

    /* Exact shortest-augmenting-path Hungarian (1-indexed) */
    size_t N1 = (size_t)(N + 1);
    char *hbuf = (char *)xmalloc(
      N1 * 2 * sizeof(double) + N1 * 2 * sizeof(int) +
      N1 * sizeof(double) + N1 * sizeof(int));
    double *hu   = (double *)hbuf;
    double *hv   = hu + N1;
    int    *hp   = (int *)(hv + N1);
    int    *hway = hp + (int)N1;
    double *hmin = (double *)(hway + (int)N1);
    int    *hused = (int *)(hmin + N1);

    memset(hu, 0, N1 * sizeof(double));
    memset(hv, 0, N1 * sizeof(double));
    memset(hp, 0, N1 * sizeof(int));

    for (int i = 1; i <= N; i++) {
      hp[0] = i; int j0 = 0;
      for (int j = 0; j <= N; j++) { hmin[j] = DBL_MAX; hused[j] = 0; }
      do {
        hused[j0] = 1;
        int i0 = hp[j0], j1 = 0;
        double delta = DBL_MAX;
        for (int j = 1; j <= N; j++) {
          if (hused[j]) continue;
          double cur = -smat[(size_t)(i0-1) * (size_t)N + (size_t)(j-1)]
                       - hu[i0] - hv[j];
          if (cur < hmin[j]) { hmin[j] = cur; hway[j] = j0; }
          if (hmin[j] < delta) { delta = hmin[j]; j1 = j; }
        }
        for (int j = 0; j <= N; j++) {
          if (hused[j]) { hu[hp[j]] += delta; hv[j] -= delta; }
          else          { hmin[j] -= delta; }
        }
        j0 = j1;
      } while (hp[j0] != 0);
      do { int j1 = hway[j0]; hp[j0] = hp[j1]; j0 = j1; } while (j0);
    }

    for (int j = 1; j <= N; j++) {
      double sc = smat[(size_t)(hp[j]-1) * (size_t)N + (size_t)(j-1)];
      if (sc > 0.0) { total += sc; matched++; }
    }
    free(hbuf); free(smat);
  }

  free(buf);

  *out_matched = matched;
  return total;
}

/* ══════════════════════════════════════════════════════════════════════════ *
 * gnps_compute — fused join+score (hot path for sanitized spectra)         *
 *                                                                          *
 * Sanitized input: sorted, no NAs, unique m/z → skip sort/filter/copy.     *
 * ══════════════════════════════════════════════════════════════════════════ */

SEXP gnps_compute(SEXP x, SEXP y,
                  SEXP xPrecursorMz, SEXP yPrecursorMz,
                  SEXP tolerance, SEXP ppm)
{
  if (!isReal(x) || !isReal(y)) error("x and y must be numeric matrices");
  SEXP xd = getAttrib(x, R_DimSymbol), yd = getAttrib(y, R_DimSymbol);
  if (!isInteger(xd) || !isInteger(yd)) error("dim must be integer");

  int nx = INTEGER(xd)[0], ny = INTEGER(yd)[0];
  const double *x_mz = REAL(x), *x_int = x_mz + nx;
  const double *y_mz = REAL(y), *y_int = y_mz + ny;
  double x_pre = asReal(xPrecursorMz), y_pre = asReal(yPrecursorMz);
  double tol = asReal(tolerance), ppm_val = asReal(ppm);

  if (nx == 0 || ny == 0) return make_result(0.0, 0);

  /* intensity sums (sanitized → no duplicates) */
  double xsum = 0.0, ysum = 0.0;
  for (int i = 0; i < nx; i++) xsum += x_int[i];
  for (int i = 0; i < ny; i++) ysum += y_int[i];
  if (xsum == 0.0 || ysum == 0.0) return make_result(0.0, 0);

  double inv_sx = 1.0 / sqrt(xsum), inv_sy = 1.0 / sqrt(ysum);

  /* pdiff = y_pre - x_pre; shifted matching uses x_mz + pdiff vs y_mz,
   * exactly matching MsCoreUtils::join_gnps which does join(x+pdiff, y).
   * No swap: always x on the left, y on the right.                      */
  int do_shift = (!ISNA(x_pre) && !ISNA(y_pre));
  double pdiff = 0.0;

  if (do_shift) {
    pdiff = y_pre - x_pre;
    double mp = (x_pre > y_pre) ? x_pre : y_pre;
    if (fabs(pdiff) <= tol + ppm_val * mp * 1e-6) do_shift = 0;
  }

  int matched = 0;
  double score = chain_dp_score(x_mz, x_int, nx, y_mz, y_int, ny,
                                inv_sx, inv_sy, pdiff, do_shift,
                                tol, ppm_val, &matched);
  return make_result(score, matched);
}

/* ══════════════════════════════════════════════════════════════════════════ *
 * gnps — backward compatible with MsCoreUtils pre-aligned matrices         *
 *                                                                          *
 * Exact shortest-augmenting-path Hungarian on the score matrix.            *
 * O(n³) but n is tiny (number of unique mz groups in matched peaks).       *
 * Single allocation for all workspace to minimize overhead.                *
 * ══════════════════════════════════════════════════════════════════════════ */

SEXP gnps(SEXP x, SEXP y)
{
  if (!isReal(x) || !isReal(y)) error("Inputs must be numeric matrices");
  SEXP xd = getAttrib(x, R_DimSymbol), yd = getAttrib(y, R_DimSymbol);
  if (!isInteger(xd) || !isInteger(yd)) error("dim must be integer");
  int n = INTEGER(xd)[0];
  if (n != INTEGER(yd)[0]) error("row count mismatch");
  const double *xmz = REAL(x), *xin = xmz + n;
  const double *ymz = REAL(y), *yin = ymz + n;

  /* unique-intensity sums */
  KP *buf = (KP *)xmalloc((size_t)n * sizeof(KP));
  int m = 0;
  for (int i = 0; i < n; i++)
    if (!ISNA(xmz[i])) { buf[m].key = xmz[i]; buf[m].pos = i; m++; }
  qsort(buf, (size_t)m, sizeof(KP), kp_cmp);
  double xs_sum = 0.0;
  for (int i = 0; i < m; i++)
    if (i == 0 || buf[i].key != buf[i - 1].key) xs_sum += xin[buf[i].pos];

  m = 0;
  for (int i = 0; i < n; i++)
    if (!ISNA(ymz[i])) { buf[m].key = ymz[i]; buf[m].pos = i; m++; }
  qsort(buf, (size_t)m, sizeof(KP), kp_cmp);
  double ys_sum = 0.0;
  for (int i = 0; i < m; i++)
    if (i == 0 || buf[i].key != buf[i - 1].key) ys_sum += yin[buf[i].pos];
  free(buf);

  if (xs_sum == 0.0 || ys_sum == 0.0) return make_result(0.0, 0);

  /* build kept arrays */
  int l = 0;
  for (int i = 0; i < n; i++)
    if (!ISNA(xmz[i]) && !ISNA(ymz[i])) l++;
  if (!l) return make_result(0.0, 0);

  double *keep_xmz = (double *)xmalloc((size_t)l * sizeof(double));
  double *keep_ymz = (double *)xmalloc((size_t)l * sizeof(double));
  double *keep_xin = (double *)xmalloc((size_t)l * sizeof(double));
  double *keep_yin = (double *)xmalloc((size_t)l * sizeof(double));
  { int k = 0;
    for (int i = 0; i < n; i++) {
      if (!ISNA(xmz[i]) && !ISNA(ymz[i])) {
        keep_xmz[k] = xmz[i]; keep_ymz[k] = ymz[i];
        keep_xin[k] = xin[i];  keep_yin[k] = yin[i];
        k++;
      }
    }
  }

  /* compute factor indices */
  int *x_fac = (int *)xmalloc((size_t)l * sizeof(int));
  int *y_fac = (int *)xmalloc((size_t)l * sizeof(int));
  { KP *kpb = (KP *)xmalloc((size_t)l * sizeof(KP));
    for (int i = 0; i < l; i++) { kpb[i].key = keep_xmz[i]; kpb[i].pos = i; }
    qsort(kpb, (size_t)l, sizeof(KP), kp_cmp);
    int rank = 0;
    for (int i = 0; i < l; i++) {
      if (i == 0 || kpb[i].key != kpb[i - 1].key) rank++;
      x_fac[kpb[i].pos] = rank;
    }
    for (int i = 0; i < l; i++) { kpb[i].key = keep_ymz[i]; kpb[i].pos = i; }
    qsort(kpb, (size_t)l, sizeof(KP), kp_cmp);
    rank = 0;
    for (int i = 0; i < l; i++) {
      if (i == 0 || kpb[i].key != kpb[i - 1].key) rank++;
      y_fac[kpb[i].pos] = rank;
    }
    free(kpb);
  }

  int n_xg = 0, n_yg = 0;
  for (int i = 0; i < l; i++) {
    if (x_fac[i] > n_xg) n_xg = x_fac[i];
    if (y_fac[i] > n_yg) n_yg = y_fac[i];
  }

  double inv_sxs = 1.0 / sqrt(xs_sum), inv_sys = 1.0 / sqrt(ys_sum);

  int N = (n_xg > n_yg) ? n_xg : n_yg;
  if (N == 0) {
    free(x_fac); free(y_fac);
    free(keep_xmz); free(keep_ymz); free(keep_xin); free(keep_yin);
    return make_result(0.0, 0);
  }

  /* score matrix */
  double *sm = (double *)xcalloc((size_t)N * (size_t)N, sizeof(double));
  for (int i = 0; i < l; i++) {
    size_t r = (size_t)(x_fac[i] - 1), c = (size_t)(y_fac[i] - 1);
    sm[r * (size_t)N + c] = sqrt(keep_xin[i]) * inv_sxs
                           * sqrt(keep_yin[i]) * inv_sys;
  }
  free(x_fac); free(y_fac);
  free(keep_xmz); free(keep_ymz); free(keep_xin); free(keep_yin);

  /* Exact shortest-augmenting-path Hungarian (maximize via negation).
   * Single allocation for all workspace: u[N+1] v[N+1] p[N+1] way[N+1]
   * minv[N+1] used[N+1].  1-indexed internally (row 0 = virtual). */
  {
    size_t N1 = (size_t)(N + 1);
    size_t buf_sz = N1 * 2 * sizeof(double)   /* u, v */
                  + N1 * 2 * sizeof(int)       /* p, way */
                  + N1 * sizeof(double)         /* minv */
                  + N1 * sizeof(int);           /* used */
    char *hbuf = (char *)xmalloc(buf_sz);

    double *u   = (double *)hbuf;
    double *v   = u + N1;
    int    *p   = (int *)(v + N1);
    int    *way = p + (int)N1;
    double *minv = (double *)(way + (int)N1);
    int    *used = (int *)(minv + N1);

    memset(u, 0, N1 * sizeof(double));
    memset(v, 0, N1 * sizeof(double));
    memset(p, 0, N1 * sizeof(int));

    for (int i = 1; i <= N; i++) {
      p[0] = i;
      int j0 = 0;
      for (int j = 0; j <= N; j++) { minv[j] = DBL_MAX; used[j] = 0; }
      do {
        used[j0] = 1;
        int i0 = p[j0], j1 = 0;
        double delta = DBL_MAX;
        for (int j = 1; j <= N; j++) {
          if (used[j]) continue;
          double cur = -sm[(size_t)(i0 - 1) * (size_t)N + (size_t)(j - 1)]
                       - u[i0] - v[j];
          if (cur < minv[j]) { minv[j] = cur; way[j] = j0; }
          if (minv[j] < delta) { delta = minv[j]; j1 = j; }
        }
        for (int j = 0; j <= N; j++) {
          if (used[j]) { u[p[j]] += delta; v[j] -= delta; }
          else         { minv[j] -= delta; }
        }
        j0 = j1;
      } while (p[j0] != 0);
      do { int j1 = way[j0]; p[j0] = p[j1]; j0 = j1; } while (j0);
    }

    double total = 0.0;
    int matched = 0;
    for (int j = 1; j <= N; j++) {
      double sc = sm[(size_t)(p[j] - 1) * (size_t)N + (size_t)(j - 1)];
      if (sc > 0.0) { total += sc; matched++; }
    }

    free(hbuf); free(sm);
    return make_result(total, matched);
  }
}

/* ══════════════════════════════════════════════════════════════════════════ *
 * join_gnps — standalone peak matching (backward compat)                   *
 *                                                                          *
 * Replicates MsCoreUtils::join_gnps outer-join semantics:                  *
 *   direct pass  = join(x, y, type="outer")          — closest one-to-one *
 *   shifted pass = join(x + pdiff, y, type="outer")  — closest one-to-one *
 * Then merge both into a single outer-join result.                         *
 * ══════════════════════════════════════════════════════════════════════════ */

SEXP join_gnps(SEXP x, SEXP y,
               SEXP xPrecursorMz, SEXP yPrecursorMz,
               SEXP tolerance, SEXP ppm)
{
  if (!isReal(x) || !isReal(y)) error("x and y must be numeric vectors");
  const double *xp = REAL(x), *yp = REAL(y);
  const double x_pre = asReal(xPrecursorMz), y_pre = asReal(yPrecursorMz);
  const double tol = asReal(tolerance), ppm_val = asReal(ppm);
  const int nx = (int)xlength(x), ny = (int)xlength(y);
  const double pdiff = y_pre - x_pre;
  const int do_pdiff = (!ISNA(x_pre) && !ISNA(y_pre));
  const double eps_tol = sqrt(DBL_EPSILON);

  /* Sort x and y by mass (keeping original indices) */
  MI *xs_arr = (MI *)xmalloc((size_t)nx * sizeof(MI));
  MI *ys_arr = (MI *)xmalloc((size_t)ny * sizeof(MI));
  int vx = 0, vy = 0;
  for (int i = 0; i < nx; i++)
    if (!ISNA(xp[i])) { xs_arr[vx].mass = xp[i]; xs_arr[vx].idx = i; vx++; }
  for (int i = 0; i < ny; i++)
    if (!ISNA(yp[i])) { ys_arr[vy].mass = yp[i]; ys_arr[vy].idx = i; vy++; }
  qsort(xs_arr, (size_t)vx, sizeof(MI), mi_cmp);
  qsort(ys_arr, (size_t)vy, sizeof(MI), mi_cmp);

  /* ── Direct matching: closest one-to-one (y-centric) ─────────────── */
  int *dm_x = (int *)xmalloc((size_t)vx * sizeof(int)); /* dm_x[xi] = yj or -1 */
  int *dm_y = (int *)xmalloc((size_t)vy * sizeof(int)); /* dm_y[yj] = xi or -1 */
  memset(dm_x, 0xFF, (size_t)vx * sizeof(int));
  memset(dm_y, 0xFF, (size_t)vy * sizeof(int));

  {
    int ilo = 0;
    for (int j = 0; j < vy; j++) {
      double ym = ys_arr[j].mass;
      while (ilo < vx) {
        double xm = xs_arr[ilo].mass;
        if (xm + tol + ppm_val * xm * 1e-6 + eps_tol >= ym) break;
        ilo++;
      }
      int best_i = -1;
      double best_d = DBL_MAX;
      for (int i = ilo; i < vx; i++) {
        double xm = xs_arr[i].mass;
        if (xm - tol - ppm_val * xm * 1e-6 - eps_tol > ym) break;
        double ai = tol + ppm_val * xm * 1e-6 + eps_tol;
        double dij = fabs(xm - ym);
        if (dij <= ai && dij < best_d) { best_d = dij; best_i = i; }
      }
      if (best_i >= 0 && dm_x[best_i] < 0) {
        dm_x[best_i] = j; dm_y[j] = best_i;
      } else if (best_i >= 0) {
        int prev_j = dm_x[best_i];
        double prev_d = fabs(xs_arr[best_i].mass - ys_arr[prev_j].mass);
        if (best_d < prev_d) {
          dm_y[prev_j] = -1;
          dm_x[best_i] = j; dm_y[j] = best_i;
        }
      }
    }
  }

  /* ── Shifted matching: closest one-to-one (y-centric) ────────────── */
  int *sm_x = (int *)xmalloc((size_t)vx * sizeof(int)); /* sm_x[xi] = yj or -1 */
  memset(sm_x, 0xFF, (size_t)vx * sizeof(int));

  if (do_pdiff) {
    int ilo = 0;
    for (int j = 0; j < vy; j++) {
      double ym = ys_arr[j].mass;
      while (ilo < vx) {
        double shifted = xs_arr[ilo].mass + pdiff;
        double ai = tol + ppm_val * fabs(shifted) * 1e-6 + eps_tol;
        if (shifted + ai >= ym) break;
        ilo++;
      }
      int best_i = -1;
      double best_d = DBL_MAX;
      for (int i = ilo; i < vx; i++) {
        double shifted = xs_arr[i].mass + pdiff;
        double ai = tol + ppm_val * fabs(shifted) * 1e-6 + eps_tol;
        if (shifted - ai > ym) break;
        double dij = fabs(shifted - ym);
        if (dij <= ai && dij < best_d) { best_d = dij; best_i = i; }
      }
      if (best_i >= 0 && sm_x[best_i] < 0) {
        sm_x[best_i] = j;
      } else if (best_i >= 0) {
        int prev_j = sm_x[best_i];
        double prev_d = fabs(xs_arr[best_i].mass + pdiff - ys_arr[prev_j].mass);
        if (best_d < prev_d) sm_x[best_i] = j;
      }
    }
  }

  /* ── Build outer-join result ─────────────────────────────────────── */
  /* Track which y indices appear in any match */
  const size_t bsz = ((size_t)ny + 7u) >> 3;
  unsigned char *y_used = (unsigned char *)xcalloc(bsz, 1);

  /* Count output rows */
  int cnt = 0;
  for (int i = 0; i < nx; i++) if (ISNA(xp[i])) cnt++;  /* NA x entries */
  for (int i = 0; i < vx; i++) {
    if (dm_x[i] >= 0) {
      BS_SET(y_used, (unsigned)ys_arr[dm_x[i]].idx);
      cnt++;
    } else {
      cnt++;  /* unmatched x */
    }
  }
  for (int i = 0; i < vx; i++) {
    if (sm_x[i] >= 0) cnt++;  /* shifted matches are additional rows */
  }
  for (int i = 0; i < ny; i++)
    if (!ISNA(yp[i]) && !BS_TEST(y_used, (unsigned)i)) cnt++;  /* unmatched y */

  /* Fill output */
  SEXP rx = PROTECT(allocVector(INTSXP, cnt));
  SEXP ry = PROTECT(allocVector(INTSXP, cnt));
  int *ox = INTEGER(rx), *oy = INTEGER(ry), pos = 0;

  /* NA x entries first */
  for (int i = 0; i < nx; i++)
    if (ISNA(xp[i])) { ox[pos] = i + 1; oy[pos] = NA_INTEGER; pos++; }

  /* Direct matches and unmatched x */
  for (int i = 0; i < vx; i++) {
    if (dm_x[i] >= 0) {
      ox[pos] = xs_arr[i].idx + 1;
      oy[pos] = ys_arr[dm_x[i]].idx + 1;
      pos++;
    } else {
      ox[pos] = xs_arr[i].idx + 1;
      oy[pos] = NA_INTEGER;
      pos++;
    }
  }

  /* Shifted matches */
  for (int i = 0; i < vx; i++) {
    if (sm_x[i] >= 0) {
      ox[pos] = xs_arr[i].idx + 1;
      oy[pos] = ys_arr[sm_x[i]].idx + 1;
      pos++;
    }
  }

  /* Unmatched y */
  for (int i = 0; i < ny; i++)
    if (!ISNA(yp[i]) && !BS_TEST(y_used, (unsigned)i)) {
      ox[pos] = NA_INTEGER; oy[pos] = i + 1; pos++;
    }

  free(xs_arr); free(ys_arr); free(y_used);
  free(dm_x); free(dm_y); free(sm_x);

  SEXP result = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(result, 0, rx);
  SET_VECTOR_ELT(result, 1, ry);
  SEXP names = PROTECT(allocVector(STRSXP, 2));
  SET_STRING_ELT(names, 0, mkChar("x"));
  SET_STRING_ELT(names, 1, mkChar("y"));
  setAttrib(result, R_NamesSymbol, names);
  UNPROTECT(4);
  return result;
}
