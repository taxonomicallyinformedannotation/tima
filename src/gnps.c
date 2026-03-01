/**
 * @file gnps.c
 * @brief GNPS modified cosine similarity — mathematically exact, near-linear.
 *
 * IMPORTANT: Input spectra MUST be sanitized before calling these functions.
 * Sanitized spectra have:
 *   - Unique m/z values (no two peaks within matching tolerance of each other)
 *   - Non-negative intensities, no NaN/NA intensities
 *   - Peaks sorted by m/z in ascending order
 *
 * In tima, this is guaranteed by sanitize_spectra() (called from
 * import_spectra(sanitize = TRUE)), which applies Spectra::reduceSpectra(),
 * Spectra::combinePeaks(), and Spectra::scalePeaks().
 *
 * The chain-DP algorithm assumes at most one direct match and one shifted
 * match per peak — a property that holds exactly when peaks within each
 * spectrum are well-separated (> tolerance apart). Unsanitized spectra with
 * duplicate or near-duplicate m/z values will produce incorrect scores.
 *
 * Algorithm: chain-DP optimal assignment inspired by Sirius/FastCosine
 * (Dührkop et al., Jena). Instead of O(n³) Hungarian/LAPJV, we exploit the
 * structure of mass-tolerance matching: after sorting both spectra by m/z,
 * the direct and shifted match assignments form chains where each peak has
 * at most one direct match and one shifted match. Conflicts along these chains
 * are resolved optimally by dynamic programming in O(n+m) total time.
 *
 * The scoring formula matches MsCoreUtils::gnps exactly:
 *   score_ij = sqrt(x_int_i) / sqrt(sum_unique_x_int)
 *            * sqrt(y_int_j) / sqrt(sum_unique_y_int)
 *   total = sum of score_ij for optimally assigned pairs
 *
 * Exports:
 *   gnps_compute(x, y, xPrecursorMz, yPrecursorMz, tolerance, ppm)
 *     — Fused join+score, takes raw peak matrices. Fast path.
 *   gnps(x, y)
 *     — Score pre-aligned matrices (backward compat with MsCoreUtils).
 *   join_gnps(x, y, xPrecursorMz, yPrecursorMz, tolerance, ppm)
 *     — Peak matching only (backward compat).
 */

#include <R.h>
#include <Rinternals.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>

/* ── helpers ─────────────────────────────────────────────────────────────── */
static void *xmalloc(size_t n) {
  void *p = malloc(n); if (!p) error("malloc(%zu)", n); return p; }
static void *xcalloc(size_t n, size_t s) {
  void *p = calloc(n, s); if (!p) error("calloc(%zu)", n * s); return p; }

/* ── bitset ──────────────────────────────────────────────────────────────── */
#define BS_SET(b,i)  ((b)[(i)>>3] |=  (unsigned char)(1u << ((i) & 7u)))
#define BS_TEST(b,i) ((b)[(i)>>3] &   (unsigned char)(1u << ((i) & 7u)))

/* ── MassIndex for join ──────────────────────────────────────────────────── */
typedef struct { double mass; int idx; } MI;
static int mi_cmp(const void *a, const void *b) {
  double da = ((const MI *)a)->mass, db = ((const MI *)b)->mass;
  return (da < db) ? -1 : (da > db) ? 1 : 0;
}
static int lower_bound(const MI *arr, int n, double lb) {
  int lo = 0, hi = n;
  while (lo < hi) {
    int mid = lo + ((hi - lo) >> 1);
    if (arr[mid].mass < lb) lo = mid + 1; else hi = mid;
  }
  return lo;
}

/* ── (key,pos) sort ──────────────────────────────────────────────────────── */
typedef struct { double key; int pos; } KP;
static int kp_cmp(const void *a, const void *b) {
  double da = ((const KP *)a)->key, db = ((const KP *)b)->key;
  return (da < db) ? -1 : (da > db) ? 1 : 0;
}

/* ── zero result ─────────────────────────────────────────────────────────── */
static SEXP zero_result(void) {
  SEXP r = PROTECT(allocVector(VECSXP, 2));
  SEXP nm = PROTECT(allocVector(STRSXP, 2));
  SET_VECTOR_ELT(r, 0, ScalarReal(0.0));
  SET_VECTOR_ELT(r, 1, ScalarInteger(0));
  SET_STRING_ELT(nm, 0, mkChar("score"));
  SET_STRING_ELT(nm, 1, mkChar("matches"));
  setAttrib(r, R_NamesSymbol, nm);
  UNPROTECT(2);
  return r;
}

/* ══════════════════════════════════════════════════════════════════════════
 * CHAIN-DP OPTIMAL ASSIGNMENT
 *
 * Given two sorted spectra where peaks are matched by m/z tolerance,
 * each left peak has at most one DIRECT match and one SHIFTED match
 * to right peaks. Conflicts (two left peaks wanting the same right peak
 * via different match types) form chains. We resolve each chain by DP.
 *
 * This is mathematically equivalent to solve_LSAP on the score matrix
 * built by MsCoreUtils::gnps, but runs in O(n+m) instead of O(l³).
 *
 * Algorithm (following Sirius ModifiedCosine by Dührkop et al.):
 *
 * 1. Two-pointer direct matching: left[i] <-> right[j] if |mz_i - mz_j| < tol
 * 2. Two-pointer shifted matching (reverse scan): left[i] <-> right[j]
 *    if |mz_i + delta - mz_j| < tol, where delta = precursorRight - precursorLeft
 *    (We enforce delta >= 0 by swapping if needed)
 * 3. For each left peak:
 *    - If only direct match: assign it
 *    - If only shifted match: assign it
 *    - If both, and no conflict: pick the better one
 *    - If conflict (shifted match's right peak is also direct-matched by
 *      another left peak): follow the chain and solve optimally by DP
 *
 * Each right peak is assigned to at most one left peak (1-to-1).
 * ══════════════════════════════════════════════════════════════════════════ */

/* ── scoring helper ──────────────────────────────────────────────────────── */
static inline double score_pair(double x_int, double y_int,
                                double inv_sqrt_xsum, double inv_sqrt_ysum) {
  return sqrt(x_int) * inv_sqrt_xsum * sqrt(y_int) * inv_sqrt_ysum;
}

/**
 * @brief Compute GNPS modified cosine using chain-DP.
 *
 * @param x_mz, x_int  Sorted x spectrum (length nx), no NAs
 * @param y_mz, y_int  Sorted y spectrum (length ny), no NAs
 * @param nx, ny        Lengths
 * @param inv_sqrt_xsum 1/sqrt(sum of unique x intensities)
 * @param inv_sqrt_ysum 1/sqrt(sum of unique y intensities)
 * @param delta         yPrecursorMz - xPrecursorMz (>= 0)
 * @param do_shift      Whether to do shifted matching
 * @param tol           Absolute tolerance in Da
 * @param ppm_val       PPM tolerance
 * @param out_matched   [out] number of matched pairs
 * @return              The GNPS similarity score
 */
static double chain_dp_score(
    const double *x_mz, const double *x_int, int nx,
    const double *y_mz, const double *y_int, int ny,
    double inv_sqrt_xsum, double inv_sqrt_ysum,
    double delta, int do_shift,
    double tol, double ppm_val,
    int *out_matched)
{
  /* direct_match[i] = index in y matched directly to x[i], or -1 */
  /* shift_match[i]  = index in y matched via shift to x[i], or -1 */
  /* back_direct[j]  = index in x that directly matched y[j], or -1 */
  int *direct_match = (int *)xmalloc((size_t)nx * sizeof(int));
  int *shift_match  = (int *)xmalloc((size_t)nx * sizeof(int));
  int *back_direct  = (int *)xmalloc((size_t)ny * sizeof(int));
  memset(direct_match, 0xFF, (size_t)nx * sizeof(int)); /* -1 */
  memset(shift_match,  0xFF, (size_t)nx * sizeof(int));
  memset(back_direct,  0xFF, (size_t)ny * sizeof(int));

  /* ── Step 1: Direct matching (forward two-pointer) ───────────────── */
  {
    int i = 0, j = 0;
    while (i < nx && j < ny) {
      double ml = x_mz[i], mr = y_mz[j];
      double allowed = tol + ppm_val * ((ml < mr) ? ml : mr) * 1e-6;
      double d = ml - mr;
      if (d < -allowed) { ++i; }
      else if (d > allowed) { ++j; }
      else {
        direct_match[i] = j;
        back_direct[j] = i;
        ++i; ++j;
      }
    }
  }

  /* ── Step 2: Shifted matching (reverse two-pointer) ──────────────── */
  if (do_shift) {
    int i = nx - 1, j = ny - 1;
    while (i >= 0 && j >= 0) {
      double ml_shifted = x_mz[i] + delta;
      double mr = y_mz[j];
      double allowed = tol + ppm_val * ((ml_shifted < mr) ? ml_shifted : mr) * 1e-6;
      double d = ml_shifted - mr;
      if (d > allowed) { --i; }
      else if (d < -allowed) { --j; }
      else {
        /* Only record shifted match if it doesn't point to the same pair
         * as the direct match (degenerate case when delta ~ 0) */
        if (back_direct[j] != i) {
          shift_match[i] = j;
        }
        --i; --j;
      }
    }
  }

  /* ── Step 3: Optimal assignment via chain-DP ─────────────────────── */
  unsigned char *visited = (unsigned char *)xcalloc((size_t)((nx + 7) >> 3), 1);
  double total = 0.0;
  int matched = 0;

  /* DP workspace — reused across chains */
  /* Each chain step has 3 states: (matched_direct, matched_shift, skip) */
  int dp_cap = 64;
  double *dp_buf = (double *)xmalloc((size_t)dp_cap * 3 * sizeof(double));

  for (int k = 0; k < nx; ++k) {
    if (BS_TEST(visited, k)) continue;

    int dm = direct_match[k];
    int sm = shift_match[k];

    if (dm < 0 && sm < 0) continue; /* no match at all */

    if (sm < 0) {
      /* Only direct match, no conflict possible */
      total += score_pair(x_int[k], y_int[dm], inv_sqrt_xsum, inv_sqrt_ysum);
      matched++;
      continue;
    }

    if (dm < 0) {
      /* Only shifted match */
      int conflict = back_direct[sm];
      if (conflict < 0) {
        /* No conflict — just assign shifted */
        total += score_pair(x_int[k], y_int[sm], inv_sqrt_xsum, inv_sqrt_ysum);
        matched++;
        continue;
      }
      /* Fall through to chain-DP below */
    }

    /* Check if shifted match conflicts with another peak's direct match */
    int conflict = back_direct[sm];
    if (conflict < 0) {
      /* No conflict — pick better of direct vs shifted */
      double ds = (dm >= 0) ? score_pair(x_int[k], y_int[dm], inv_sqrt_xsum, inv_sqrt_ysum) : 0.0;
      double ss = score_pair(x_int[k], y_int[sm], inv_sqrt_xsum, inv_sqrt_ysum);
      if (ds >= ss) {
        total += ds;
      } else {
        total += ss;
      }
      matched++;
      continue;
    }

    /* ── Chain-DP: resolve conflict chain ──────────────────────────── */
    /* Chain structure: k has shift_match -> sm, which is direct_match of
     * `conflict`. `conflict` may itself have a shift_match pointing to some
     * other y-peak, which may be direct_match of yet another x-peak, etc.
     *
     * States per chain node:
     *   state 0: assign direct match to this node
     *   state 1: assign shifted match to this node
     *   state 2: skip this node (no assignment)
     *
     * Constraint: If node i uses state 1 (shift -> y[j]), and y[j] is the
     * direct match of node i+1, then node i+1 CANNOT use state 0.
     * After state 0 or 2: next node can use any state.
     */
    int chain_len = 0;

    /* First node is k */
    if (chain_len >= dp_cap) {
      dp_cap *= 2;
      dp_buf = (double *)realloc(dp_buf, (size_t)dp_cap * 3 * sizeof(double));
      if (!dp_buf) error("realloc dp");
    }
    {
      double ds = (dm >= 0)
        ? score_pair(x_int[k], y_int[dm], inv_sqrt_xsum, inv_sqrt_ysum)
        : -1.0;
      double ss = score_pair(x_int[k], y_int[sm], inv_sqrt_xsum, inv_sqrt_ysum);
      dp_buf[0] = ds;   /* direct */
      dp_buf[1] = ss;   /* shift */
      dp_buf[2] = 0.0;  /* skip */
    }
    BS_SET(visited, k);
    chain_len++;

    /* Follow the chain */
    int u = conflict;
    while (u >= 0 && !BS_TEST(visited, u)) {
      BS_SET(visited, u);

      if (chain_len >= dp_cap) {
        dp_cap *= 2;
        dp_buf = (double *)realloc(dp_buf, (size_t)dp_cap * 3 * sizeof(double));
        if (!dp_buf) error("realloc dp");
      }

      int u_dm = direct_match[u];
      int u_sm = shift_match[u];

      double u_ds = (u_dm >= 0)
        ? score_pair(x_int[u], y_int[u_dm], inv_sqrt_xsum, inv_sqrt_ysum)
        : -1.0;
      double u_ss = (u_sm >= 0)
        ? score_pair(x_int[u], y_int[u_sm], inv_sqrt_xsum, inv_sqrt_ysum)
        : -1.0;

      /* DP transitions from previous node's states */
      double prev_d = dp_buf[(chain_len - 1) * 3 + 0];
      double prev_s = dp_buf[(chain_len - 1) * 3 + 1];
      double prev_n = dp_buf[(chain_len - 1) * 3 + 2];

      /* State 0 (direct match for u):
       * Previous cannot have been shift (which consumed u's direct y-peak).
       * So: max(prev_d, prev_n) + u_ds */
      double best_not_shift = (prev_d > prev_n) ? prev_d : prev_n;
      dp_buf[chain_len * 3 + 0] = (u_ds >= 0.0)
        ? best_not_shift + u_ds : -1.0;

      /* State 1 (shift match for u):
       * Previous can be anything: max(prev_d, prev_s, prev_n) + u_ss */
      double best_any = prev_d;
      if (prev_s > best_any) best_any = prev_s;
      if (prev_n > best_any) best_any = prev_n;
      dp_buf[chain_len * 3 + 1] = (u_ss >= 0.0)
        ? best_any + u_ss : -1.0;

      /* State 2 (skip u):
       * Previous can be anything */
      dp_buf[chain_len * 3 + 2] = best_any;

      chain_len++;

      /* Follow chain: u's shift_match points to some y-peak, which may
       * be direct-matched by another x-peak */
      if (u_sm >= 0) {
        u = back_direct[u_sm];
      } else {
        u = -1;
      }
    }

    /* Find maximum over last chain node's states */
    double chain_score = -1.0;
    int last = chain_len - 1;
    for (int s = 0; s < 3; s++) {
      if (dp_buf[last * 3 + s] > chain_score)
        chain_score = dp_buf[last * 3 + s];
    }

    if (chain_score > 0.0) {
      total += chain_score;

      /* Count matched pairs by backtracing through the DP */
      int best_s = 2;
      for (int s = 0; s < 3; s++) {
        if (dp_buf[last * 3 + s] > dp_buf[last * 3 + best_s])
          best_s = s;
      }
      if (best_s < 2) matched++;

      for (int t = last - 1; t >= 0; t--) {
        int next_state = best_s;
        if (next_state == 0) {
          /* Direct: previous was not shift -> max(prev_d, prev_n) */
          double d_val = dp_buf[t * 3 + 0];
          double n_val = dp_buf[t * 3 + 2];
          best_s = (d_val >= 0.0 && d_val >= n_val) ? 0 : 2;
        } else {
          /* Shift or skip: previous was anything -> max all 3 */
          best_s = 2;
          for (int s = 0; s < 3; s++) {
            if (dp_buf[t * 3 + s] >= 0.0 &&
                (dp_buf[t * 3 + best_s] < 0.0 ||
                 dp_buf[t * 3 + s] > dp_buf[t * 3 + best_s]))
              best_s = s;
          }
        }
        if (best_s < 2) matched++;
      }
    }
  }

  free(dp_buf);
  free(visited);
  free(direct_match); free(shift_match); free(back_direct);

  *out_matched = matched;
  return total;
}

/* ══════════════════════════════════════════════════════════════════════════
 * FUSED: join_gnps + gnps in one call
 * ══════════════════════════════════════════════════════════════════════════ */
static SEXP gnps_fused(const double *x_mz, const double *x_in, int nx,
                       const double *y_mz, const double *y_in, int ny,
                       double x_pre, double y_pre,
                       double tol, double ppm_val)
{
  /* ── 1. Sort non-NA peaks ──────────────────────────────────────────── */
  MI *xs = (MI *)xmalloc((size_t)nx * sizeof(MI));
  MI *ys = (MI *)xmalloc((size_t)ny * sizeof(MI));
  int vx = 0, vy = 0;
  for (int i = 0; i < nx; i++)
    if (!ISNA(x_mz[i])) { xs[vx].mass = x_mz[i]; xs[vx].idx = i; vx++; }
  for (int i = 0; i < ny; i++)
    if (!ISNA(y_mz[i])) { ys[vy].mass = y_mz[i]; ys[vy].idx = i; vy++; }
  qsort(xs, (size_t)vx, sizeof(MI), mi_cmp);
  qsort(ys, (size_t)vy, sizeof(MI), mi_cmp);

  /* ── 2. Unique-intensity sums ──────────────────────────────────────── */
  double xs_sum = 0.0, ys_sum = 0.0;
  for (int i = 0; i < vx; i++)
    if (i == 0 || xs[i].mass != xs[i - 1].mass)
      xs_sum += x_in[xs[i].idx];
  for (int i = 0; i < vy; i++)
    if (i == 0 || ys[i].mass != ys[i - 1].mass)
      ys_sum += y_in[ys[i].idx];

  if (xs_sum == 0.0 || ys_sum == 0.0) {
    free(xs); free(ys); return zero_result();
  }

  double inv_sxs = 1.0 / sqrt(xs_sum);
  double inv_sys = 1.0 / sqrt(ys_sum);

  /* ── 3. Build sorted mz/int arrays ─────────────────────────────────── */
  double *sx_mz  = (double *)xmalloc((size_t)vx * sizeof(double));
  double *sx_int = (double *)xmalloc((size_t)vx * sizeof(double));
  double *sy_mz  = (double *)xmalloc((size_t)vy * sizeof(double));
  double *sy_int = (double *)xmalloc((size_t)vy * sizeof(double));
  for (int i = 0; i < vx; i++) {
    sx_mz[i]  = xs[i].mass;
    sx_int[i] = x_in[xs[i].idx];
  }
  for (int i = 0; i < vy; i++) {
    sy_mz[i]  = ys[i].mass;
    sy_int[i] = y_in[ys[i].idx];
  }
  free(xs); free(ys);

  /* ── 4. Determine delta and shift flag ─────────────────────────────── */
  int do_shift = (!ISNA(x_pre) && !ISNA(y_pre));
  double delta = 0.0;
  const double *left_mz, *left_int, *right_mz, *right_int;
  int nleft, nright;

  if (do_shift) {
    delta = y_pre - x_pre;
    if (delta < 0) {
      /* Swap left/right so delta >= 0 */
      left_mz = sy_mz; left_int = sy_int; nleft = vy;
      right_mz = sx_mz; right_int = sx_int; nright = vx;
      delta = -delta;
      double tmp = inv_sxs; inv_sxs = inv_sys; inv_sys = tmp;
    } else {
      left_mz = sx_mz; left_int = sx_int; nleft = vx;
      right_mz = sy_mz; right_int = sy_int; nright = vy;
    }
    /* If delta is within tolerance of zero, skip shift matching */
    double max_pre = (x_pre > y_pre) ? x_pre : y_pre;
    double shift_tol = tol + ppm_val * max_pre * 1e-6;
    if (delta <= shift_tol) do_shift = 0;
  } else {
    left_mz = sx_mz; left_int = sx_int; nleft = vx;
    right_mz = sy_mz; right_int = sy_int; nright = vy;
  }

  int matched = 0;
  double score = chain_dp_score(
    left_mz, left_int, nleft,
    right_mz, right_int, nright,
    inv_sxs, inv_sys,
    delta, do_shift,
    tol, ppm_val,
    &matched
  );

  free(sx_mz); free(sx_int); free(sy_mz); free(sy_int);

  SEXP r = PROTECT(allocVector(VECSXP, 2));
  SEXP nm = PROTECT(allocVector(STRSXP, 2));
  SET_VECTOR_ELT(r, 0, ScalarReal(score));
  SET_VECTOR_ELT(r, 1, ScalarInteger(matched));
  SET_STRING_ELT(nm, 0, mkChar("score"));
  SET_STRING_ELT(nm, 1, mkChar("matches"));
  setAttrib(r, R_NamesSymbol, nm);
  UNPROTECT(2);
  return r;
}

/* ══════════════════════════════════════════════════════════════════════════
 * R entry: gnps_compute(x_mat, y_mat, xPrecursorMz, yPrecursorMz, tol, ppm)
 * ══════════════════════════════════════════════════════════════════════════ */
SEXP gnps_compute(SEXP x, SEXP y,
                  SEXP xPrecursorMz, SEXP yPrecursorMz,
                  SEXP tolerance, SEXP ppm)
{
  if (!isReal(x) || !isReal(y)) error("x and y must be numeric matrices");
  SEXP xd = getAttrib(x, R_DimSymbol), yd = getAttrib(y, R_DimSymbol);
  if (!isInteger(xd) || !isInteger(yd)) error("dim must be integer");
  int nx = INTEGER(xd)[0], ny = INTEGER(yd)[0];
  const double *xmz = REAL(x), *xin = xmz + nx;
  const double *ymz = REAL(y), *yin = ymz + ny;
  return gnps_fused(xmz, xin, nx, ymz, yin, ny,
                    asReal(xPrecursorMz), asReal(yPrecursorMz),
                    asReal(tolerance), asReal(ppm));
}

/* ══════════════════════════════════════════════════════════════════════════
 * R entry: gnps(x_aligned, y_aligned)  — backward compatible
 *
 * Replicates MsCoreUtils::gnps exactly:
 *   1. Sum unique x/y intensities
 *   2. Keep rows where both x and y have non-NA mz
 *   3. Factor x_mz and y_mz into integer group indices
 *   4. Build score_mat[x_idx, y_idx] = sqrt(x_int)/sqrt(x_sum) * ...
 *   5. Solve optimal 1-to-1 assignment
 *
 * Uses clean shortest-augmenting-path Hungarian (O(n³) but n is typically
 * tiny — number of unique mz groups in matched peaks).
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

  /* ── unique-intensity sums ──────────────────────────────────────────── */
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

  if (xs_sum == 0.0 || ys_sum == 0.0) return zero_result();

  /* ── build kept arrays ─────────────────────────────────────────────── */
  int l = 0;
  for (int i = 0; i < n; i++)
    if (!ISNA(xmz[i]) && !ISNA(ymz[i])) l++;
  if (!l) return zero_result();

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

  /* ── compute factor indices ────────────────────────────────────────── */
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

  int n_xgroups = 0, n_ygroups = 0;
  for (int i = 0; i < l; i++) {
    if (x_fac[i] > n_xgroups) n_xgroups = x_fac[i];
    if (y_fac[i] > n_ygroups) n_ygroups = y_fac[i];
  }

  double inv_sxs = 1.0 / sqrt(xs_sum);
  double inv_sys = 1.0 / sqrt(ys_sum);

  int mat_dim = (n_xgroups > n_ygroups) ? n_xgroups : n_ygroups;
  if (mat_dim == 0) {
    free(x_fac); free(y_fac);
    free(keep_xmz); free(keep_ymz); free(keep_xin); free(keep_yin);
    return zero_result();
  }

  /* Build square score matrix — last write wins (matching R) */
  double *score_mat = (double *)xcalloc((size_t)mat_dim * (size_t)mat_dim,
                                        sizeof(double));
  for (int i = 0; i < l; i++) {
    int r = x_fac[i] - 1;
    int c = y_fac[i] - 1;
    double sc = sqrt(keep_xin[i]) * inv_sxs * sqrt(keep_yin[i]) * inv_sys;
    score_mat[(size_t)r * mat_dim + c] = sc;
  }

  free(x_fac); free(y_fac);
  free(keep_xmz); free(keep_ymz); free(keep_xin); free(keep_yin);

  /* ── Shortest-augmenting-path Hungarian (maximize via negation) ────── */
  {
    int N = mat_dim;
    double *u = (double *)xcalloc((size_t)(N + 1), sizeof(double));
    double *v = (double *)xcalloc((size_t)(N + 1), sizeof(double));
    int    *p = (int *)xmalloc((size_t)(N + 1) * sizeof(int));
    int  *way = (int *)xmalloc((size_t)(N + 1) * sizeof(int));
    double *minv = (double *)xmalloc((size_t)(N + 1) * sizeof(double));
    int  *used = (int *)xcalloc((size_t)(N + 1), sizeof(int));

    for (int j = 0; j <= N; j++) p[j] = 0;

    for (int i = 1; i <= N; i++) {
      p[0] = i;
      int j0 = 0;
      for (int j = 0; j <= N; j++) { minv[j] = DBL_MAX; used[j] = 0; }
      do {
        used[j0] = 1;
        int i0 = p[j0], j1 = 0;
        double del = DBL_MAX;
        for (int j = 1; j <= N; j++) {
          if (used[j]) continue;
          double cur = -score_mat[(size_t)(i0 - 1) * N + (j - 1)] - u[i0] - v[j];
          if (cur < minv[j]) { minv[j] = cur; way[j] = j0; }
          if (minv[j] < del) { del = minv[j]; j1 = j; }
        }
        for (int j = 0; j <= N; j++) {
          if (used[j]) { u[p[j]] += del; v[j] -= del; }
          else { minv[j] -= del; }
        }
        j0 = j1;
      } while (p[j0] != 0);
      do { int j1 = way[j0]; p[j0] = p[j1]; j0 = j1; } while (j0);
    }

    double total = 0.0;
    int matched = 0;
    for (int j = 1; j <= N; j++) {
      int row = p[j] - 1;
      int col = j - 1;
      double sc = score_mat[(size_t)row * N + col];
      if (sc > 0.0) { total += sc; matched++; }
    }

    free(u); free(v); free(p); free(way); free(minv); free(used);
    free(score_mat);

    SEXP r = PROTECT(allocVector(VECSXP, 2));
    SEXP nm = PROTECT(allocVector(STRSXP, 2));
    SET_VECTOR_ELT(r, 0, ScalarReal(total));
    SET_VECTOR_ELT(r, 1, ScalarInteger(matched));
    SET_STRING_ELT(nm, 0, mkChar("score"));
    SET_STRING_ELT(nm, 1, mkChar("matches"));
    setAttrib(r, R_NamesSymbol, nm);
    UNPROTECT(2);
    return r;
  }
}

/* ══════════════════════════════════════════════════════════════════════════
 * join_gnps — standalone peak matching (backward compat)
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

  MI *xs_arr = (MI *)xmalloc((size_t)nx * sizeof(MI));
  MI *ys_arr = (MI *)xmalloc((size_t)ny * sizeof(MI));
  int vx = 0, vy = 0;
  for (int i = 0; i < nx; i++)
    if (!ISNA(xp[i])) { xs_arr[vx].mass = xp[i]; xs_arr[vx].idx = i; vx++; }
  for (int i = 0; i < ny; i++)
    if (!ISNA(yp[i])) { ys_arr[vy].mass = yp[i]; ys_arr[vy].idx = i; vy++; }
  qsort(xs_arr, (size_t)vx, sizeof(MI), mi_cmp);
  qsort(ys_arr, (size_t)vy, sizeof(MI), mi_cmp);

  const size_t bsz = ((size_t)ny + 7u) >> 3;
  unsigned char *y_used = (unsigned char *)xcalloc(bsz, 1);

  /* count pass */
  int cnt = 0;
  for (int i = 0; i < nx; i++) if (ISNA(xp[i])) cnt++;
  for (int i = 0; i < vx; i++) {
    const double xm = xs_arr[i].mass, half = tol + ppm_val * xm * 1e-6;
    int start = lower_bound(ys_arr, vy, xm - half), found = 0;
    for (int j = start; j < vy && ys_arr[j].mass <= xm + half; j++) {
      BS_SET(y_used, ys_arr[j].idx); found++; cnt++;
    }
    if (!found) cnt++;
  }
  if (do_pdiff) {
    for (int i = 0; i < vx; i++) {
      const double xa = xs_arr[i].mass + pdiff, half = tol + ppm_val * xa * 1e-6;
      for (int j = lower_bound(ys_arr, vy, xa - half);
           j < vy && ys_arr[j].mass <= xa + half; j++)
        cnt++;
    }
  }
  for (int i = 0; i < ny; i++)
    if (!ISNA(yp[i]) && !BS_TEST(y_used, i)) cnt++;

  memset(y_used, 0, bsz);

  SEXP rx = PROTECT(allocVector(INTSXP, cnt));
  SEXP ry = PROTECT(allocVector(INTSXP, cnt));
  int *ox = INTEGER(rx), *oy = INTEGER(ry), pos = 0;

  for (int i = 0; i < nx; i++)
    if (ISNA(xp[i])) { ox[pos] = i + 1; oy[pos] = NA_INTEGER; pos++; }
  for (int i = 0; i < vx; i++) {
    const double xm = xs_arr[i].mass, half = tol + ppm_val * xm * 1e-6;
    int start = lower_bound(ys_arr, vy, xm - half), found = 0;
    for (int j = start; j < vy && ys_arr[j].mass <= xm + half; j++) {
      BS_SET(y_used, ys_arr[j].idx);
      ox[pos] = xs_arr[i].idx + 1; oy[pos] = ys_arr[j].idx + 1; pos++; found++;
    }
    if (!found) { ox[pos] = xs_arr[i].idx + 1; oy[pos] = NA_INTEGER; pos++; }
  }
  if (do_pdiff) {
    for (int i = 0; i < vx; i++) {
      const double xa = xs_arr[i].mass + pdiff, half = tol + ppm_val * xa * 1e-6;
      for (int j = lower_bound(ys_arr, vy, xa - half);
           j < vy && ys_arr[j].mass <= xa + half; j++) {
        ox[pos] = xs_arr[i].idx + 1; oy[pos] = ys_arr[j].idx + 1; pos++;
      }
    }
  }
  for (int i = 0; i < ny; i++)
    if (!ISNA(yp[i]) && !BS_TEST(y_used, i)) {
      ox[pos] = NA_INTEGER; oy[pos] = i + 1; pos++;
    }

  free(xs_arr); free(ys_arr); free(y_used);

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
