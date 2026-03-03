/**
 * @file gnps.c
 * @brief GNPS modified cosine similarity — mathematically exact, near-linear.
 *
 * Input spectra MUST be sanitized (import_spectra(sanitize = TRUE)):
 *   - Unique m/z values, sorted ascending, no NA/NaN intensities.
 *
 * Algorithm: chain-DP optimal assignment (Sirius/FastCosine, Dührkop et al.)
 * exploits mass-tolerance structure for O(n+m) instead of O(n³) Hungarian.
 *
 * Scoring: sqrt(x_i)/sqrt(Σx) · sqrt(y_j)/sqrt(Σy)  (matches MsCoreUtils).
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

static int lower_bound(const MI *arr, int n, double lb) {
  int lo = 0, hi = n;
  while (lo < hi) {
    int mid = lo + ((hi - lo) >> 1);
    if (arr[mid].mass < lb) lo = mid + 1; else hi = mid;
  }
  return lo;
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
 * CHAIN-DP OPTIMAL ASSIGNMENT                                              *
 *                                                                          *
 * Two-pointer direct + shifted matching, then DP on conflict chains.       *
 * All workspace is heap-allocated to exact size — portable and fast.        *
 * ══════════════════════════════════════════════════════════════════════════ */

static double chain_dp_score(
    const double *x_mz, const double *x_int, int nx,
    const double *y_mz, const double *y_int, int ny,
    double inv_sx, double inv_sy,
    double delta, int do_shift,
    double tol, double ppm_val,
    int *out_matched)
{
  /* Single allocation for all workspace: dm[nx] sm[nx] bd[ny] dp[nx*3] vis[] */
  size_t vis_bytes = (size_t)(((unsigned)nx + 7u) >> 3);
  size_t int_need  = (size_t)(nx + nx + ny);
  size_t dbl_need  = (size_t)nx * 3;
  size_t total_sz  = int_need * sizeof(int)
                   + dbl_need * sizeof(double)
                   + vis_bytes;
  char *buf = (char *)xmalloc(total_sz);

  int *dm_arr = (int *)buf;
  int *sm_arr = dm_arr + nx;
  int *bd_arr = sm_arr + nx;
  double *dp  = (double *)(bd_arr + ny);
  unsigned char *vis = (unsigned char *)(dp + dbl_need);

  memset(dm_arr, 0xFF, (size_t)nx * sizeof(int));
  memset(sm_arr, 0xFF, (size_t)nx * sizeof(int));
  memset(bd_arr, 0xFF, (size_t)ny * sizeof(int));
  memset(vis, 0, vis_bytes);

  /* ── Step 1: direct matching (forward two-pointer) ─────────────────── */
  for (int i = 0, j = 0; i < nx && j < ny; ) {
    double ml = x_mz[i], mr = y_mz[j];
    double a = tol + ppm_val * ((ml < mr) ? ml : mr) * 1e-6;
    double d = ml - mr;
    if      (d < -a) ++i;
    else if (d >  a) ++j;
    else { dm_arr[i] = j; bd_arr[j] = i; ++i; ++j; }
  }

  /* ── Step 2: shifted matching (reverse two-pointer) ────────────────── */
  if (do_shift) {
    for (int i = nx - 1, j = ny - 1; i >= 0 && j >= 0; ) {
      double ml = x_mz[i] + delta, mr = y_mz[j];
      double a = tol + ppm_val * ((ml < mr) ? ml : mr) * 1e-6;
      double d = ml - mr;
      if      (d >  a) --i;
      else if (d < -a) --j;
      else {
        if (bd_arr[j] != i) sm_arr[i] = j;
        --i; --j;
      }
    }
  }

  /* ── Step 3: resolve assignments via chain-DP ──────────────────────── */
  double total = 0.0;
  int matched = 0;

  for (int k = 0; k < nx; ++k) {
    if (BS_TEST(vis, k)) continue;
    int dm = dm_arr[k], sm = sm_arr[k];

    if (dm < 0 && sm < 0) continue;

    if (sm < 0) {
      total += score_pair(x_int[k], y_int[dm], inv_sx, inv_sy);
      matched++;
      continue;
    }

    if (dm < 0 && bd_arr[sm] < 0) {
      total += score_pair(x_int[k], y_int[sm], inv_sx, inv_sy);
      matched++;
      continue;
    }

    int conf = bd_arr[sm];
    if (conf < 0) {
      double ds = (dm >= 0) ? score_pair(x_int[k], y_int[dm], inv_sx, inv_sy) : 0.0;
      double ss = score_pair(x_int[k], y_int[sm], inv_sx, inv_sy);
      total += (ds >= ss) ? ds : ss;
      matched++;
      continue;
    }

    /* chain-DP */
    int clen = 0;
    dp[0] = (dm >= 0) ? score_pair(x_int[k], y_int[dm], inv_sx, inv_sy) : -1.0;
    dp[1] = score_pair(x_int[k], y_int[sm], inv_sx, inv_sy);
    dp[2] = 0.0;
    BS_SET(vis, k);
    clen = 1;

    for (int u = conf; u >= 0 && !BS_TEST(vis, u); ) {
      BS_SET(vis, u);
      int udm = dm_arr[u], usm = sm_arr[u];
      double uds = (udm >= 0) ? score_pair(x_int[u], y_int[udm], inv_sx, inv_sy) : -1.0;
      double uss = (usm >= 0) ? score_pair(x_int[u], y_int[usm], inv_sx, inv_sy) : -1.0;

      double pd = dp[(clen-1)*3], ps = dp[(clen-1)*3+1], pn = dp[(clen-1)*3+2];
      double bns = (pd > pn) ? pd : pn;
      double ba = pd; if (ps > ba) ba = ps; if (pn > ba) ba = pn;

      dp[clen*3]   = (uds >= 0.0) ? bns + uds : -1.0;
      dp[clen*3+1] = (uss >= 0.0) ? ba  + uss : -1.0;
      dp[clen*3+2] = ba;
      clen++;
      u = (usm >= 0) ? bd_arr[usm] : -1;
    }

    int last = clen - 1;
    double cs = dp[last*3+2]; int bs = 2;
    for (int s = 0; s < 2; s++)
      if (dp[last*3+s] > cs) { cs = dp[last*3+s]; bs = s; }

    if (cs > 0.0) {
      total += cs;
      if (bs < 2) matched++;
      for (int t = last - 1; t >= 0; t--) {
        if (bs == 0) {
          double dv = dp[t*3], nv = dp[t*3+2];
          bs = (dv >= 0.0 && dv >= nv) ? 0 : 2;
        } else {
          bs = 2;
          for (int s = 0; s < 3; s++)
            if (dp[t*3+s] >= 0.0 &&
                (dp[t*3+bs] < 0.0 || dp[t*3+s] > dp[t*3+bs]))
              bs = s;
        }
        if (bs < 2) matched++;
      }
    }
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

  /* orient so delta >= 0 */
  int do_shift = (!ISNA(x_pre) && !ISNA(y_pre));
  double delta = 0.0;
  const double *lm = x_mz, *li = x_int, *rm = y_mz, *ri = y_int;
  int nl = nx, nr = ny;

  if (do_shift) {
    delta = y_pre - x_pre;
    if (delta < 0.0) {
      lm = y_mz; li = y_int; nl = ny;
      rm = x_mz; ri = x_int; nr = nx;
      delta = -delta;
      double t = inv_sx; inv_sx = inv_sy; inv_sy = t;
    }
    double mp = (x_pre > y_pre) ? x_pre : y_pre;
    if (delta <= tol + ppm_val * mp * 1e-6) do_shift = 0;
  }

  int matched = 0;
  double score = chain_dp_score(lm, li, nl, rm, ri, nr,
                                inv_sx, inv_sy, delta, do_shift,
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
      const double xa = xs_arr[i].mass + pdiff,
                   half = tol + ppm_val * xa * 1e-6;
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
      ox[pos] = xs_arr[i].idx + 1; oy[pos] = ys_arr[j].idx + 1;
      pos++; found++;
    }
    if (!found) { ox[pos] = xs_arr[i].idx + 1; oy[pos] = NA_INTEGER; pos++; }
  }
  if (do_pdiff) {
    for (int i = 0; i < vx; i++) {
      const double xa = xs_arr[i].mass + pdiff,
                   half = tol + ppm_val * xa * 1e-6;
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
