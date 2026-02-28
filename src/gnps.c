/**
 * @brief GNPS Spectral Similarity — Optimized Exact LAPJV
 *
 * Optimizations over the prior version (tagged inline):
 *
 *  [OPT-A]  Fused factorization + unique-intensity-sum: one sort per axis,
 *           no second sort, no binary-search-per-element loop.
 *  [OPT-B]  Thread-safe sorting via (key,idx) pair arrays: eliminates the
 *           global `global_mz` pointer entirely.
 *  [OPT-C]  Single-block workspace in lapjv_dense: 1 malloc instead of 6;
 *           tighter cache footprint, zero heap fragmentation.
 *  [OPT-D]  Score matrix stored pre-negated: inner Dijkstra loop drops the
 *           unary minus on every arc evaluation.
 *  [OPT-E]  Generation-counter "reset" for the visited set in lapjv_sparse:
 *           O(1) per iteration instead of O(m) memset.
 *  [OPT-F]  Exact lazy-deletion in sparse heap (no fragile 1e-12 epsilon).
 *  [OPT-G]  Post-LAPJV score lookup in sparse path is O(log row_len) via
 *           the already-sorted CSR; col_sc[] keeps the positive values so
 *           un-negating is free.
 *  [OPT-H]  Pre-computed reciprocal-sqrt denominators: two multiplies
 *           instead of two divides per score element.
 *  [OPT-I]  Settled-stack in lapjv_sparse: potential-update iterates only
 *           over the settled set, not all N columns.
 *  [OPT-J]  Heap buffer sized to l+m+2 (exact worst-case), no realloc.
 *  [OPT-K]  Single-block workspace in lapjv_sparse (same idea as OPT-C).
 *
 * Algorithm: Jonker-Volgenant Shortest Augmenting Paths (exact LSAP).
 *   m <= DENSE_THRESHOLD : O(m²),       linear-scan Dijkstra, no heap.
 *   m >  DENSE_THRESHOLD : O(l·log m),  binary-heap Dijkstra.
 * Dummy column at index m guarantees feasibility without padding.
 *
 * Reference: Jonker & Volgenant (1987), Computing 38:325-340.
 */

#include <R.h>
#include <Rinternals.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>
#ifdef _OPENMP
#  include <omp.h>
#endif

#if defined(__GNUC__) || defined(__clang__)
#  define PREFETCH(p) __builtin_prefetch((p), 0, 1)
#else
#  define PREFETCH(p) ((void)0)
#endif

#define DENSE_THRESHOLD 128

/* ── Allocation helpers ──────────────────────────────────────────────────── */
static void *xmalloc(size_t n)
{
    void *p = malloc(n);
    if (!p) error("malloc(%zu) failed", n);
    return p;
}
static void *xcalloc(size_t n, size_t sz)
{
    void *p = calloc(n, sz);
    if (!p) error("calloc(%zu) failed", n * sz);
    return p;
}

/* ══════════════════════════════════════════════════════════════════════════
 * [OPT-A, OPT-B]  Fused factorization + unique-intensity-sum
 *
 *  Combines what was formerly:
 *    • compute_unique_intensity_sum   (sort + linear scan)
 *    • generate_factor_indices        (sort + binary search × l)
 *  into a single O(l log l) pass with no global state.
 *
 *  mz_col    : pointer to first element of the m/z column in the matrix.
 *  int_col   : pointer to first element of the intensity column.
 *  keep      : indices of valid rows (length l).
 *  l         : number of valid rows.
 *  fac[0..l) : output — 1-based factor (rank) index per keep entry.
 *  buf[0..l) : scratch (key, position) pairs; caller provides it to avoid
 *              double-allocation when called for x and y back-to-back.
 *
 *  Returns the unique-m/z intensity sum.
 * ══════════════════════════════════════════════════════════════════════════ */
typedef struct { double key; int pos; } KP;

static int kp_cmp(const void *a, const void *b)
{
    double da = ((const KP*)a)->key, db = ((const KP*)b)->key;
    return (da < db) ? -1 : (da > db) ? 1 : 0;
}

static double fused_factor_sum(const double *mz_col,
                               const double *int_col,
                               const int    *keep,
                               int           l,
                               int          *fac,
                               KP           *buf)
{
    for (int i = 0; i < l; i++) {
        buf[i].key = mz_col[keep[i]];
        buf[i].pos = i;
    }
    qsort(buf, (size_t)l, sizeof(KP), kp_cmp);

    double sum  = 0.0;
    int    rank = 0;
    for (int i = 0; i < l; i++) {
        if (i == 0 || buf[i].key != buf[i-1].key) {
            ++rank;
            sum += int_col[keep[buf[i].pos]];
        }
        fac[buf[i].pos] = rank;
    }
    return sum;
}

/* ── Zero result ─────────────────────────────────────────────────────────── */
static SEXP zero_result(void)
{
    SEXP r  = PROTECT(allocVector(VECSXP, 2));
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
 * DENSE LAPJV  [OPT-C, OPT-D]
 *
 *  score[r*m + c] : NEGATED similarity (minimisation).
 *  On exit: row2col[r] = assigned column (0-based); r→m means dummy.
 *
 *  Workspace packed into one malloc [OPT-C]:
 *    doubles : u[N]  v[N]  d[N]           (3 × N × 8 bytes)
 *    ints    : p[N]  prev[N]  done[N]      (3 × N × 4 bytes)
 * ══════════════════════════════════════════════════════════════════════════ */
static void lapjv_dense(int m, const double *score, int *row2col)
{
    int    N  = m + 1;
    size_t db = (size_t)N * sizeof(double);
    size_t ib = (size_t)N * sizeof(int);

    /* [OPT-C] single allocation */
    char   *blk  = (char*) xmalloc(3*db + 3*ib);
    double *u    = (double*)(blk);
    double *v    = (double*)(blk +   db);
    double *d    = (double*)(blk + 2*db);
    int    *p    = (int*)   (blk + 3*db);
    int    *prev = (int*)   (blk + 3*db +   ib);
    int    *done = (int*)   (blk + 3*db + 2*ib);

    memset(blk, 0, 3*db + 3*ib);   /* zeros u, v, d, p (0==-1? no: set p below) */
    for (int j = 0; j < N; j++) p[j] = -1;
    for (int i = 0; i < m; i++) row2col[i] = -1;

    for (int i = 0; i < m; i++) {
        /* Reset per-row state */
        for (int j = 0; j < N; j++) { d[j] = DBL_MAX; prev[j] = -1; done[j] = 0; }

        /* [OPT-D] score already negated; just apply potentials */
        const double  ui    = u[i];
        const double *row_i = score + (size_t)i * m;
        for (int j = 0; j < m; j++) {
            double red = row_i[j] - ui - v[j];
            if (red < d[j]) d[j] = red;   /* prev[j] already -1 */
        }
        { double red = -ui - v[m]; if (red < d[m]) d[m] = red; }

        int j_aug = -1;

        /* Dijkstra — linear scan, unrolled ×4 */
        for (int iter = 0; iter < N; iter++) {
            double dmin = DBL_MAX; int jmin = -1;
            int j = 0;
            for (; j <= N-4; j += 4) {
                if (!done[j]   && d[j]   < dmin) { dmin = d[j];   jmin = j;   }
                if (!done[j+1] && d[j+1] < dmin) { dmin = d[j+1]; jmin = j+1; }
                if (!done[j+2] && d[j+2] < dmin) { dmin = d[j+2]; jmin = j+2; }
                if (!done[j+3] && d[j+3] < dmin) { dmin = d[j+3]; jmin = j+3; }
            }
            for (; j < N; j++)
                if (!done[j] && d[j] < dmin) { dmin = d[j]; jmin = j; }

            if (jmin < 0 || dmin == DBL_MAX) break;
            done[jmin] = 1;
            if (p[jmin] == -1) { j_aug = jmin; break; }

            /* Relax through row p[jmin] */
            int           i2     = p[jmin];
            const double  ui2    = u[i2];
            const double  vjmin  = v[jmin];
            const double  base   = dmin - ui2 + vjmin;
            const double *row_i2 = score + (size_t)i2 * m;
            for (int j2 = 0; j2 < m; j2++) {
                if (done[j2]) continue;
                PREFETCH(row_i2 + j2 + 16);
                /* [OPT-D] no negate here */
                double red = base + row_i2[j2] - v[j2];
                if (red < d[j2]) { d[j2] = red; prev[j2] = jmin; }
            }
            if (!done[m]) {
                double red = base - v[m];
                if (red < d[m]) { d[m] = red; prev[m] = jmin; }
            }
        }

        if (j_aug < 0) error("LAPJV dense: infeasible (bug)");

        /* Potential update */
        double d_aug = d[j_aug];
        u[i] += d_aug;
        for (int j = 0; j < N; j++) {
            if (!done[j]) continue;
            if (p[j] != -1) u[p[j]] += d[j] - d_aug;
            v[j] -= d[j] - d_aug;
        }

        /* Augment */
        for (int jc = j_aug; jc != -1; ) {
            int jp = prev[jc];
            int ic = (jp == -1) ? i : p[jp];
            p[jc] = ic; row2col[ic] = jc;
            jc = jp;
        }
    }

    free(blk);
}

/* ══════════════════════════════════════════════════════════════════════════
 * BINARY HEAP  (embedded, no realloc)  [OPT-J]
 * ══════════════════════════════════════════════════════════════════════════ */
typedef struct { double key; int val; } HNode;

static inline void h_push(HNode *h, int *sz, double key, int val)
{
    int i = (*sz)++;
    h[i] = (HNode){key, val};
    while (i > 0) {
        int par = (i-1) >> 1;
        if (h[par].key <= h[i].key) break;
        HNode t = h[par]; h[par] = h[i]; h[i] = t; i = par;
    }
}
static inline HNode h_pop(HNode *h, int *sz)
{
    HNode top = h[0];
    h[0] = h[--(*sz)];
    int i = 0;
    for (;;) {
        int l = 2*i+1, r = 2*i+2, s = i;
        if (l < *sz && h[l].key < h[s].key) s = l;
        if (r < *sz && h[r].key < h[s].key) s = r;
        if (s == i) break;
        HNode t = h[s]; h[s] = h[i]; h[i] = t; i = s;
    }
    return top;
}

/* ══════════════════════════════════════════════════════════════════════════
 * SPARSE LAPJV  [OPT-E, OPT-F, OPT-I, OPT-J, OPT-K]
 *
 *  CSR: first[0..m+1],  kk[0..l),  cc[0..l)  (cc negated).
 *  l: total arc count (for heap sizing).
 *  On exit: row2col[r] = c; c==m → dummy (unmatched).
 *
 *  Visited-set reset: generation counter [OPT-E]  — O(1) per iteration.
 *  Settled-set iteration: explicit stack [OPT-I]  — potential update is
 *    O(|settled|) not O(N).
 *  Lazy deletion: exact (no epsilon) [OPT-F].
 *  Heap buffer: pre-sized to l+m+2, no realloc [OPT-J].
 *  Workspace: single allocation [OPT-K].
 * ══════════════════════════════════════════════════════════════════════════ */
static void lapjv_sparse(int m,
                         const double *cc, const int *kk, const int *first,
                         int l,
                         int *row2col)
{
    int    N   = m + 1;
    size_t db  = (size_t)N * sizeof(double);
    size_t ib  = (size_t)N * sizeof(int);
    size_t hb  = (size_t)(l + m + 2) * sizeof(HNode);  /* [OPT-J] */

    /* [OPT-K] single block:
     *   double: u[N]  v[N]  dist[N]
     *   int:    p[N]  prev[N]  vis_gen[N]  stk[N]
     *   HNode:  heap[l+m+2]                              */
    char   *blk     = (char*) xmalloc(3*db + 4*ib + hb);
    double *u        = (double*)(blk);
    double *v        = (double*)(blk +   db);
    double *dist     = (double*)(blk + 2*db);
    int    *p        = (int*)   (blk + 3*db);
    int    *prev     = (int*)   (blk + 3*db +   ib);
    int    *vis_gen  = (int*)   (blk + 3*db + 2*ib);  /* [OPT-E] */
    int    *stk      = (int*)   (blk + 3*db + 3*ib);  /* [OPT-I] settled stack */
    HNode  *heap     = (HNode*) (blk + 3*db + 4*ib);

    memset(blk, 0, 3*db + 4*ib);   /* zeros u, v, dist, vis_gen, stk; p set below */
    for (int j = 0; j < N; j++) p[j] = -1;
    for (int i = 0; i < m; i++) row2col[i] = -1;

    /* vis_gen values:
     *   0            : not yet touched this iteration (after memset)
     *   +cur_gen     : touched (in open set / relaxed), dist[] valid
     *   -cur_gen     : settled (in closed set S)
     * Requires cur_gen > 0 always, which is guaranteed by pre-increment.   */
    int cur_gen = 0;

    for (int i = 0; i < m; i++) {
        cur_gen++;
        int hsz = 0, nstk = 0;
        const double ui = u[i];

        /* Seed from row i's arcs */
        for (int e = first[i]; e < first[i+1]; e++) {
            int    jj  = kk[e];
            double red = cc[e] - ui - v[jj];   /* cc already negated */
            if (vis_gen[jj] != cur_gen) {
                vis_gen[jj] = cur_gen; dist[jj] = red; prev[jj] = -1;
                h_push(heap, &hsz, red, jj);
            } else if (red < dist[jj]) {
                dist[jj] = red; prev[jj] = -1;
                h_push(heap, &hsz, red, jj);
            }
        }
        {   /* Dummy column */
            double red = -ui - v[m];
            if (vis_gen[m] != cur_gen) {
                vis_gen[m] = cur_gen; dist[m] = red; prev[m] = -1;
                h_push(heap, &hsz, red, m);
            } else if (red < dist[m]) {
                dist[m] = red; prev[m] = -1;
                h_push(heap, &hsz, red, m);
            }
        }

        int j_aug = -1;

        while (hsz > 0) {
            HNode hn = h_pop(heap, &hsz);
            int   jj = hn.val;
            /* [OPT-F] exact lazy deletion */
            if (hn.key > dist[jj]) continue;
            /* Settle jj */
            vis_gen[jj] = -cur_gen;
            stk[nstk++] = jj;          /* [OPT-I] record for potential update */

            if (p[jj] == -1) { j_aug = jj; break; }   /* free column */

            int    i2   = p[jj];
            double base = dist[jj] - u[i2] + v[jj];

            for (int e = first[i2]; e < first[i2+1]; e++) {
                PREFETCH(&cc[e+8]);
                int    jj2 = kk[e];
                if (vis_gen[jj2] == -cur_gen) continue;  /* already settled */
                double red = base + cc[e] - v[jj2];
                if (vis_gen[jj2] != cur_gen) {
                    vis_gen[jj2] = cur_gen; dist[jj2] = red; prev[jj2] = jj;
                    h_push(heap, &hsz, red, jj2);
                } else if (red < dist[jj2]) {
                    dist[jj2] = red; prev[jj2] = jj;
                    h_push(heap, &hsz, red, jj2);
                }
            }
            /* Dummy column relax */
            if (vis_gen[m] != -cur_gen) {
                double red = base - v[m];
                if (vis_gen[m] != cur_gen) {
                    vis_gen[m] = cur_gen; dist[m] = red; prev[m] = jj;
                    h_push(heap, &hsz, red, m);
                } else if (red < dist[m]) {
                    dist[m] = red; prev[m] = jj;
                    h_push(heap, &hsz, red, m);
                }
            }
        }

        if (j_aug < 0) error("LAPJV sparse: infeasible (bug)");

        /* [OPT-I] Potential update — only settled set, not all N */
        double d_aug = dist[j_aug];
        u[i] += d_aug;
        for (int k = 0; k < nstk; k++) {
            int j = stk[k];
            if (p[j] != -1) u[p[j]] += dist[j] - d_aug;
            v[j] -= dist[j] - d_aug;
        }

        /* Augment */
        for (int jc = j_aug; jc != -1; ) {
            int jp = prev[jc];
            int ic = (jp == -1) ? i : p[jp];
            p[jc] = ic; row2col[ic] = jc;
            jc = jp;
        }
    }

    free(blk);
}

/* ══════════════════════════════════════════════════════════════════════════
 * R entry point
 * ══════════════════════════════════════════════════════════════════════════ */
SEXP gnps(SEXP x, SEXP y)
{
    if (!isReal(x) || !isReal(y)) error("Inputs must be numeric matrices.");
    SEXP xd = getAttrib(x, R_DimSymbol);
    SEXP yd = getAttrib(y, R_DimSymbol);
    if (!isInteger(xd) || !isInteger(yd))
        error("Inputs must have integer dim attributes.");
    int n = INTEGER(xd)[0];
    if (n != INTEGER(yd)[0]) error("'x' and 'y' must have the same row count.");

    const double *xmz = REAL(x);          /* column 0: m/z */
    const double *ymz = REAL(y);
    const double *xin = xmz + n;          /* column 1: intensity */
    const double *yin = ymz + n;

    /* --- Build keep[] -------------------------------------------------- */
    int *keep = (int*) xmalloc((size_t)n * sizeof(int));
    int  l    = 0;
    for (int i = 0; i < n; i++)
        if (!ISNA(xmz[i]) && !ISNA(ymz[i])) keep[l++] = i;
    if (!l) { free(keep); return zero_result(); }

    /* --- Factorization + unique-intensity sums [OPT-A, OPT-B] ---------- */
    int *x_fac = (int*) xmalloc((size_t)l * sizeof(int));
    int *y_fac = (int*) xmalloc((size_t)l * sizeof(int));
    KP  *kpbuf = (KP*)  xmalloc((size_t)l * sizeof(KP));

    double xs = fused_factor_sum(xmz, xin, keep, l, x_fac, kpbuf);
    double ys = fused_factor_sum(ymz, yin, keep, l, y_fac, kpbuf);
    free(kpbuf);

    if (xs == 0.0 || ys == 0.0) {
        free(keep); free(x_fac); free(y_fac);
        return zero_result();
    }

    /* --- Scores [OPT-H]: pre-computed reciprocal sqrts ----------------- */
    const double inv_xs = 1.0 / sqrt(xs);
    const double inv_ys = 1.0 / sqrt(ys);
    double *scores = (double*) xmalloc((size_t)l * sizeof(double));

#ifdef _OPENMP
#   pragma omp parallel for schedule(static)
#endif
    for (int i = 0; i < l; i++) {
        int k = keep[i];
        scores[i] = sqrt(xin[k]) * inv_xs * sqrt(yin[k]) * inv_ys;
    }
    free(keep);

    /* --- Problem size --------------------------------------------------- */
    int mx = 0, my = 0;
    for (int i = 0; i < l; i++) {
        if (x_fac[i] > mx) mx = x_fac[i];
        if (y_fac[i] > my) my = y_fac[i];
    }
    int m = (mx > my) ? mx : my;

    double total   = 0.0;
    int    matched = 0;

    /* ════════════════════════════════════════════════════════════════════
     * DENSE PATH (m <= DENSE_THRESHOLD)
     * ════════════════════════════════════════════════════════════════════ */
    if (m <= DENSE_THRESHOLD) {
        double *smat = (double*) xcalloc((size_t)m * (size_t)m, sizeof(double));

        /* [OPT-D] store negated scores for direct use in lapjv_dense */
        for (int i = 0; i < l; i++)
            smat[(size_t)(y_fac[i]-1) * m + (size_t)(x_fac[i]-1)] = -scores[i];

        int *row2col = (int*) xmalloc((size_t)(m+1) * sizeof(int));
        lapjv_dense(m, smat, row2col);

        for (int r = 0; r < m; r++) {
            int c = row2col[r];
            if (c >= 0 && c < m) {
                double sv = -smat[(size_t)r * m + (size_t)c];  /* un-negate */
                if (sv > 0.0) { total += sv; matched++; }
            }
        }
        free(smat); free(row2col);

    /* ════════════════════════════════════════════════════════════════════
     * SPARSE PATH (m > DENSE_THRESHOLD)
     * ════════════════════════════════════════════════════════════════════ */
    } else {
        /* Row counts */
        int *rowcnt = (int*) xcalloc((size_t)m, sizeof(int));
        for (int i = 0; i < l; i++) rowcnt[y_fac[i]-1]++;

        int *first = (int*) xmalloc((size_t)(m+1) * sizeof(int));
        first[0] = 0;
        for (int r = 0; r < m; r++) first[r+1] = first[r] + rowcnt[r];
        free(rowcnt);

        /* CSR fill [OPT-D] + col_sc for score readback [OPT-G] */
        double *cc     = (double*) xmalloc((size_t)l * sizeof(double));
        int    *kk     = (int*)    xmalloc((size_t)l * sizeof(int));
        double *col_sc = (double*) xmalloc((size_t)l * sizeof(double));
        int    *cur    = (int*)    xmalloc((size_t)m  * sizeof(int));
        memcpy(cur, first, (size_t)m * sizeof(int));

        for (int i = 0; i < l; i++) {
            int pos    = cur[y_fac[i]-1]++;
            cc[pos]    = -scores[i];   /* negated for LAPJV [OPT-D] */
            kk[pos]    = x_fac[i]-1;
            col_sc[pos] = scores[i];   /* positive for result [OPT-G] */
        }
        free(cur);

        /* Sort columns within each CSR row (insertion sort; rows ≪ m) */
        for (int r = 0; r < m; r++) {
            for (int a = first[r]+1; a < first[r+1]; a++) {
                double cv = cc[a], sv = col_sc[a];
                int    ck = kk[a], b  = a-1;
                while (b >= first[r] && kk[b] > ck) {
                    cc[b+1] = cc[b]; kk[b+1] = kk[b]; col_sc[b+1] = col_sc[b]; b--;
                }
                cc[b+1] = cv; kk[b+1] = ck; col_sc[b+1] = sv;
            }
        }

        int *row2col = (int*) xmalloc((size_t)(m+1) * sizeof(int));
        lapjv_sparse(m, cc, kk, first, l, row2col);

        /* [OPT-G] O(log row_len) binary search in sorted CSR row */
        for (int r = 0; r < m; r++) {
            int c = row2col[r];
            if (c < 0 || c >= m) continue;
            int lo = first[r], hi = first[r+1]-1;
            while (lo <= hi) {
                int mid = (lo+hi) >> 1;
                if (kk[mid] == c) {
                    if (col_sc[mid] > 0.0) { total += col_sc[mid]; matched++; }
                    break;
                }
                if (kk[mid] < c) lo = mid+1; else hi = mid-1;
            }
        }
        free(first); free(cc); free(kk); free(col_sc); free(row2col);
    }

    free(scores); free(x_fac); free(y_fac);

    SEXP result = PROTECT(allocVector(VECSXP, 2));
    SEXP names  = PROTECT(allocVector(STRSXP, 2));
    SET_VECTOR_ELT(result, 0, ScalarReal(total));
    SET_VECTOR_ELT(result, 1, ScalarInteger(matched));
    SET_STRING_ELT(names, 0, mkChar("score"));
    SET_STRING_ELT(names, 1, mkChar("matches"));
    setAttrib(result, R_NamesSymbol, names);
    UNPROTECT(2);
    return result;
}
