/**
 * @file gnps.c
 * @brief GNPS modified cosine similarity - chain-DP matching, thread-local
 *        reusable scratch buffers, optional OpenMP batch scoring.
 *
 * Same algorithm as the original file (chain-DP matching described below),
 * with the memory management and hot-loop arithmetic reworked so this can
 * be called many times in a row - or from multiple OpenMP threads at once
 * - without the per-call malloc/free churn and redundant recomputation
 * that made the original slow on platforms with a weaker allocator.
 *
 * PREREQUISITES - spectra must be sanitized before calling any of this:
 *   - unique m/z values (no two peaks in the same spectrum within
 *     tolerance of each other)
 *   - non-negative intensities, no NaN/NA/Inf
 *   - sorted ascending by m/z
 * Use Spectra::reduceSpectra() / combinePeaks() / scalePeaks() upstream.
 * The matching algorithm assumes at most one direct and one shifted match
 * per peak, which only holds when peaks are well-separated within each
 * spectrum. An unsanitized spectrum won't error out, it'll just give you a
 * wrong score silently.
 *
 * ─────────────────────────────────────────────────────────────────────────
 * WHY THIS IS FASTER THAN THE ORIGINAL
 * ─────────────────────────────────────────────────────────────────────────
 *
 * 1. No more per-call heap traffic. score_matched() used to malloc/calloc
 *    its workspace fresh on every invocation and free it at the end - fine
 *    on Linux where glibc's allocator eats that for breakfast, much less
 *    fine on macOS where the system allocator is noticeably slower for
 *    this alloc/free-a-lot-of-small-things pattern. Here the workspace
 *    lives in a scratch struct that grows (via realloc, ~1.5x amortized)
 *    the first few times it's needed and then just gets reused. Once
 *    everything's grown to the largest spectrum size you'll see, calls do
 *    zero allocation.
 *
 * 2. The conflict bitsets (x_dirty/y_dirty) used to be allocated on every
 *    call even though, per the comments below, conflicts only happen on
 *    maybe 1% of pairs - so 99% of that allocation was pure waste, since
 *    the bitsets are never even read on the no-conflict path. Now we do a
 *    cheap allocation-free scan first to check whether a conflict exists
 *    at all, and only touch the bitset scratch if it does.
 *
 * 3. The per-element tolerance (tol + ppm*mz*1e-6 + eps) and the shifted
 *    m/z value used to get recomputed every time the sliding window
 *    revisited a peak - which, depending on how much the windows overlap
 *    across different y[j], could mean the same multiplication happening
 *    several times. Now it's computed once per peak into a small array and
 *    reused for the rest of the match. Same for the score itself: sqrt(x)
 *    and sqrt(y) used to get recomputed per candidate match; now each is
 *    computed once (scaled by 1/sqrt(sum) up front) and scoring a pair is
 *    just one multiply.
 *
 * Numerically this should agree with the original to within floating-point
 * rounding (same formula, same order of operations for the match decisions
 * - the only reassociation is in the score itself: sqrt(xi)*inv_sx*sqrt(yj)
 * *inv_sy computed as (sqrt(xi)*inv_sx) * (sqrt(yj)*inv_sy) instead of one
 * long left-to-right chain). Expect agreement to ~1e-14 relative, not
 * necessarily bit-identical.
 *
 * ─────────────────────────────────────────────────────────────────────────
 * THREADING
 * ─────────────────────────────────────────────────────────────────────────
 *
 * All scratch state below is thread-local (_Thread_local / __thread), so
 * each OpenMP or pthread worker gets its own copy automatically - no locks,
 * no false sharing between threads, and each thread's buffers grow and
 * settle independently the same way they would in a single-threaded run.
 * Forked worker processes (future::multicore, mclapply) were already fine
 * before this change, since fork() duplicates the whole address space.
 *
 * The one thing to watch: R's C API (error(), allocVector(), etc.) is not
 * thread-safe and must only ever be called from R's main thread. None of
 * the functions meant to run inside a parallel region here (score_matched,
 * gnps_chain_dp_core_api, gnps_chain_dp_batch_core_api, and the allocation
 * helpers they use) touch the R API - allocation failures in that path
 * just print to stderr and abort() the process rather than longjmp'ing
 * through R's error handling from a foreign thread. The SEXP-facing
 * wrappers (C_gnps_chain_dp and friends) are unchanged and remain
 * main-thread-only, as they always were.
 *
 * gnps_chain_dp_batch_core_api() below is the actual hook for parallelism:
 * given one query spectrum and a batch of library spectra, it runs the
 * per-pair comparisons under `#pragma omp parallel for`. Compiling with
 * OpenMP disabled is harmless - the pragma is just ignored and it runs
 * serially, same as before. To turn OpenMP on for real, the package's
 * Makevars needs something like:
 *   PKG_CFLAGS += $(SHLIB_OPENMP_CFLAGS)
 *   PKG_LIBS   += $(SHLIB_OPENMP_CFLAGS)
 * (per CRAN's portable-OpenMP recommendation in Writing R Extensions).
 *
 * ─────────────────────────────────────────────────────────────────────────
 * ALGORITHM - chain-DP optimal assignment
 * ─────────────────────────────────────────────────────────────────────────
 *
 * When spectra are sanitized, the bipartite match graph between two
 * spectra decomposes into simple chains rather than an arbitrary network.
 * Example: x = [100, 200, 300], y = [100.01, 200.02, 300.01], tol = 0.05.
 * Direct pass matches x[0]-y[0], x[1]-y[1], x[2]-y[2]: three independent
 * chains, no branching. That's because if two x-peaks could both reach
 * the same y-peak, they'd have to be within tolerance of each other - but
 * sanitized spectra rule that out by construction. So each y-peak has at
 * most one direct claimant and at most one shifted claimant, giving:
 *
 *   Step 1 - direct match   [O(n+m)] - closest one-to-one, y-centric
 *            sliding window on sorted x, tolerance = tol + ppm*mz*1e-6
 *   Step 2 - shifted match  [O(n+m)] - same thing on x+pdiff vs y,
 *            skipped entirely if the precursor difference is within
 *            measurement noise (no real neutral loss to look for)
 *   Step 3 - scoring        [O(n) typical, O(n + k^3) worst]
 *            no conflicts (~99% of real pairs): greedy max(direct,shifted)
 *            per x-peak is already the global optimum, since no y-peak is
 *            claimed twice.
 *            conflicts (~1%): a shifted match and a direct match land on
 *            the same y-peak. Score everything outside that cluster
 *            greedily (still safe), then solve the small (k~3-5) cluster
 *            exactly with the Hungarian algorithm - O(k^3) on a tiny
 *            matrix instead of O(n^3) on the whole thing.
 *
 * score(i,j) = sqrt(x_int[i])/sqrt(sum_x) * sqrt(y_int[j])/sqrt(sum_y)
 *
 * References:
 *   Wang M, Carver JJ, Phelan VV, et al. (2016). Sharing and community
 *   curation of mass spectrometry data with GNPS. Nat Biotechnol 34:828.
 *   Dührkop K et al. (2019). SIRIUS 4. Nat Methods 16:299. (chain-DP idea
 *   taken from SIRIUS's FastCosine implementation.)
 *
 * Exported:
 *   gnps_chain_dp(x, y, xPrecursorMz, yPrecursorMz, tolerance, ppm)
 *     single-pair fused join+score, the hot path above.
 *   gnps_chain_dp_batch(x, xPrecursorMz, y_list, yPrecursorMz, tol, ppm)
 *     one query against a batch of library spectra, OpenMP-parallel.
 *   gnps(x, y) / join_gnps(x, y, ...)
 *     backward-compatible pre-aligned scoring / peak-matching-only paths.
 *
 * @author TIMA Development Team
 * @license GPL-3+
 * @see https://github.com/taxonomicallyinformedannotation/tima
 */

#ifndef GNPS_HAS_R_API
#if defined(__has_include)
#  if __has_include(<R.h>) && __has_include(<Rinternals.h>)
#    define GNPS_HAS_R_API 1
#  endif
#endif
#endif
#ifndef GNPS_HAS_R_API
#  define GNPS_HAS_R_API 0
#endif

#if GNPS_HAS_R_API
#include <R.h>
#include <Rinternals.h>
#endif
#ifdef _OPENMP
#include <omp.h>
#endif
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>
#include <stdio.h>

#include "gnps_portable.h"

/* Thread-local storage qualifier. Falls back to a plain static if the
 * compiler has neither C11 _Thread_local nor the GNU __thread extension -
 * at that point you're single-threaded anyway (no OpenMP support without
 * one of these), so a plain global is equivalent. */
#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L && !defined(__STDC_NO_THREADS__)
#  define GNPS_TLS _Thread_local
#elif defined(__GNUC__) || defined(__clang__)
#  define GNPS_TLS __thread
#else
#  define GNPS_TLS
#endif

/* Whether it's currently safe to call R's error(). R's C API - including
 * error(), which unwinds via longjmp - is only safe to call from R's main
 * thread. Safe whenever we're compiled against the R API and not
 * currently inside an OpenMP parallel region; without OpenMP compiled in
 * there's no "inside a parallel region" to worry about, so always safe
 * then; without the R API at all, there's no error() to call regardless.
 *
 * This is what keeps allocation-failure handling both CRAN-compliant (no
 * abort()/stderr in the compiled object - see "Writing R Extensions") and
 * safe under OpenMP (no error() from a worker thread). */

/**
 * @brief Report an allocation failure and terminate the process.
 *
 * Deliberately does NOT call R's `error()`. This path may run on any
 * OpenMP/pthread worker thread, and R's C API (including its longjmp-based
 * error handling) is only safe to call from R's main thread - calling
 * `error()` from a worker thread would corrupt R's internal state rather
 * than cleanly reporting the problem. An allocation failure is already a
 * fatal, essentially unrecoverable condition, so printing to stderr and
 * aborting is the safer choice here regardless of which thread hit it.
 *
 * @param msg Short description of what failed.
 * @param n   Size in bytes that was requested (for diagnostics only).
 */
static int gnps_safe_to_call_r_error(void) {
#if GNPS_HAS_R_API
#ifdef _OPENMP
    return !omp_in_parallel();
#else
    return 1;
#endif
#else
    return 0;
#endif
}

/* malloc()/calloc() that report failure via R's error() when that's safe
 * (see above) and return NULL otherwise, for the caller to handle. */

/**
 * @brief `malloc()` that never returns NULL - aborts via gnps_fatal() on
 *        failure instead.
 * @param n Bytes to allocate.
 * @return Newly allocated, uninitialized memory of size `n`.
 */
static void *xmalloc(size_t n) {
    void *p = malloc(n);
    if (p) return p;
#if GNPS_HAS_R_API
    if (gnps_safe_to_call_r_error())
        error("gnps: memory allocation failed (%zu bytes)", n);
#endif
    return NULL;
}

/**
 * @brief `calloc()` that never returns NULL - aborts via gnps_fatal() on
 *        failure instead.
 * @param n Number of elements.
 * @param s Size of each element in bytes.
 * @return Newly allocated, zero-initialized memory of size `n * s`.
 */
static void *xcalloc(size_t n, size_t s) {
    void *p = calloc(n, s);
    if (p) return p;
#if GNPS_HAS_R_API
    if (gnps_safe_to_call_r_error())
        error("gnps: memory allocation failed (%zu bytes)", n * s);
#endif
    return NULL;
}

/* Grow *ptr to at least need_bytes via realloc, ~1.5x amortized so we're
 * not reallocating every call as spectrum sizes creep up. No-op (instant
 * success) once the buffer's already big enough - that's the
 * steady-state, zero-allocation path this whole rewrite exists for.
 *
 * On failure: reports via R's error() when safe to do so, and always
 * returns 0 - the existing buffer/capacity are left untouched, so the
 * caller must check the return value and bail rather than assume it grew. */

/**
 * @brief Grow a scratch buffer in place, reusing it across calls.
 *
 * Grows `*ptr` to at least `need_bytes` via `realloc()`, using ~1.5x
 * amortized growth so repeated calls with slowly increasing sizes don't
 * reallocate on every single call. A no-op (instant return) once the
 * buffer is already big enough - that's the steady-state, zero-allocation
 * path this whole rewrite exists to reach.
 *
 * @param ptr        Address of the pointer to grow (in/out).
 * @param cap_bytes  Address of the tracked capacity of `*ptr`, in bytes
 *                   (in/out).
 * @param need_bytes Minimum capacity required after this call.
 */
static int scratch_ensure(void **ptr, size_t *cap_bytes, size_t need_bytes) {
    if (need_bytes <= *cap_bytes) return 1;
    size_t new_cap = *cap_bytes ? *cap_bytes : (size_t) 64;
    while (new_cap < need_bytes) new_cap = new_cap + new_cap / 2 + 64;
    void *newp = realloc(*ptr, new_cap);
    if (newp) {
        *ptr = newp;
        *cap_bytes = new_cap;
        return 1;
    }
#if GNPS_HAS_R_API
    if (gnps_safe_to_call_r_error())
        error("gnps: scratch allocation failed (%zu bytes)", new_cap);
#endif
    return 0;
}

#define BS_SET(b, i)                                                           \
  ((b)[(unsigned)(i) >> 3] |= (unsigned char)(1u << ((unsigned)(i) & 7u)))
#define BS_TEST(b, i)                                                          \
  ((b)[(unsigned)(i) >> 3] & (unsigned char)(1u << ((unsigned)(i) & 7u)))

/** @brief (mass, original index) pair, used to sort peaks by m/z while
 *         keeping track of where they came from - needed by the
 *         backward-compat join path. */
typedef struct {
    double mass;
    int idx;
} MI;

/** @brief Ascending comparator for MI, for use with qsort(). */
static int mi_cmp(const void *a, const void *b) {
    double da = ((const MI *) a)->mass, db = ((const MI *) b)->mass;
    return (da < db) ? -1 : (da > db) ? 1 : 0;
}

/** @brief (key, position) pair used for the unique-value / ranking sorts
 *         in the backward-compat scoring paths. */
typedef struct {
    double key;
    int pos;
} KP;

/** @brief Ascending comparator for KP, for use with qsort(). */
static int kp_cmp(const void *a, const void *b) {
    double da = ((const KP *) a)->key, db = ((const KP *) b)->key;
    return (da < db) ? -1 : (da > db) ? 1 : 0;
}

#if GNPS_HAS_R_API
/**
 * @brief Build the standard 4-element result vector returned to R.
 * @param score       Main GNPS score.
 * @param matches     Number of matched peak pairs.
 * @param score_fwd   Score normalized by all-query + matched-library.
 * @param score_rev   Score normalized by matched-query + all-library.
 * @return A protected-then-unprotected REALSXP of length 4:
 *         c(score, matches, score_forward, score_reverse).
 */
static SEXP make_result(double score, int matches,
                        double score_fwd, double score_rev) {
    SEXP r = PROTECT(allocVector(REALSXP, 4));
    REAL(r)[0] = score;
    REAL(r)[1] = (double) matches;
    REAL(r)[2] = score_fwd;
    REAL(r)[3] = score_rev;
    UNPROTECT(1);
    return r;
}
#endif

/* ── per-thread scratch for the hot path ─────────────────────────────────
 *
 * Everything score_matched() needs across calls, sized to the largest
 * spectra seen so far and reused from then on. The conflict-cluster
 * fields (x_dirty onward) only ever get touched on the rare path where a
 * shifted match collides with a direct match. */

/**
 * @brief Per-thread reusable scratch state for score_matched().
 *
 * Every field grows (via scratch_ensure()) to the largest size requested
 * so far and is then reused across calls without being freed in between.
 * The conflict-cluster fields (x_dirty onward) only ever get touched on
 * the rare path where a shifted match collides with a direct match - see
 * score_matched() for details.
 */
typedef struct {
    int    *idx_buf;       /* dm_arr[nx] + sm_arr[nx] + bd_arr[ny] packed */
    size_t  idx_buf_cap;

    double *x_tol;         /* tol + ppm*x_mz[i]*1e-6 + eps, per peak */
    size_t  x_tol_cap;
    double *x_shift_val;   /* x_mz[i] + pdiff, only computed if do_shift */
    size_t  x_shift_val_cap;
    double *x_shift_tol;   /* tolerance on the shifted value */
    size_t  x_shift_tol_cap;

    double *scaled_x;      /* sqrt(x_int[i]) * inv_sx */
    size_t  scaled_x_cap;
    double *scaled_y;      /* sqrt(y_int[j]) * inv_sy */
    size_t  scaled_y_cap;

    unsigned char *x_dirty;
    size_t         x_dirty_cap;
    unsigned char *y_dirty;
    size_t         y_dirty_cap;

    int    *xi_map, *xi_rmap, *yi_map, *yi_rmap;
    size_t  xi_map_cap, xi_rmap_cap, yi_map_cap, yi_rmap_cap;
    double *smat;
    size_t  smat_cap;
    char   *hbuf;
    size_t  hbuf_cap;
} GnpsScratch;

/** @brief The scratch instance itself - one per thread, see GNPS_TLS. */
static GNPS_TLS GnpsScratch g_scratch = {0};

/* Optional: call from a package unload hook if you want the scratch
 * memory released before process exit. Not calling it is harmless - the
 * OS reclaims everything anyway - but if you do call it, remember it only
 * frees the calling thread's copy. */

/**
 * @brief Release all scratch memory held by the calling thread.
 *
 * Optional. Call this from a package unload hook if you want the memory
 * released before process exit; not calling it is harmless, since the OS
 * reclaims everything at exit anyway. Only frees the calling thread's
 * copy - each thread that has run scoring code owns its own scratch and
 * would need to call this itself.
 */
void gnps_scratch_free_all(void) {
    free(g_scratch.idx_buf);
    free(g_scratch.x_tol);
    free(g_scratch.x_shift_val);
    free(g_scratch.x_shift_tol);
    free(g_scratch.scaled_x);
    free(g_scratch.scaled_y);
    free(g_scratch.x_dirty);
    free(g_scratch.y_dirty);
    free(g_scratch.xi_map);
    free(g_scratch.xi_rmap);
    free(g_scratch.yi_map);
    free(g_scratch.yi_rmap);
    free(g_scratch.smat);
    free(g_scratch.hbuf);
    memset(&g_scratch, 0, sizeof(g_scratch));
}

/* ══════════════════════════════════════════════════════════════════════
 * score_matched - the actual matching + scoring work. Same result as the
 * original for every input; see the file header for what changed and why.
 * ══════════════════════════════════════════════════════════════════════ */

/**
 * @brief Match and score two sanitized spectra (the chain-DP hot path).
 *
 * Implements Steps 1-3 of the algorithm described in the file header:
 * direct matching, shifted matching, then greedy or Hungarian scoring
 * depending on whether any conflict cluster arises. Produces the same
 * result as a naive re-implementation would for any sanitized input; see
 * the file header for what's different here (scratch reuse, precomputed
 * tolerances, precomputed scaled intensities) and why.
 *
 * @param x_mz, x_int   Query spectrum m/z and intensity arrays, length nx.
 * @param nx            Number of peaks in the query spectrum.
 * @param y_mz, y_int   Library spectrum m/z and intensity arrays, length ny.
 * @param ny            Number of peaks in the library spectrum.
 * @param inv_sx, inv_sy 1/sqrt(sum of unique intensities) for x and y.
 * @param pdiff         Precursor mass difference (y_pre - x_pre).
 * @param do_shift      Whether to run the shifted-matching pass at all.
 * @param tol           Absolute matching tolerance, in Da.
 * @param ppm_val       Relative matching tolerance, in ppm.
 * @param[out] out_matched      Number of matched peak pairs.
 * @param[out] out_matched_xsum Sum of matched query intensities.
 * @param[out] out_matched_ysum Sum of matched library intensities.
 * @return The raw (un-normalized) GNPS similarity score.
 */
static double score_matched(
    const double *x_mz, const double *x_int, int nx,
    const double *y_mz, const double *y_int, int ny,
    double inv_sx, double inv_sy,
    double pdiff, int do_shift,
    double tol, double ppm_val,
    int *out_matched,
    double *out_matched_xsum, double *out_matched_ysum) {

    GnpsScratch *sc = &g_scratch;
    const double eps_tol = sqrt(DBL_EPSILON);

    size_t int_need = (size_t) (nx + nx + ny) * sizeof(int);
    scratch_ensure((void **) &sc->idx_buf, &sc->idx_buf_cap, int_need);
    int *dm_arr = sc->idx_buf;
    int *sm_arr = dm_arr + nx;
    int *bd_arr = sm_arr + nx;
    memset(dm_arr, 0xFF, (size_t) nx * sizeof(int));
    memset(sm_arr, 0xFF, (size_t) nx * sizeof(int));
    memset(bd_arr, 0xFF, (size_t) ny * sizeof(int));

    /* per-peak tolerance, computed once instead of on every window check */
    scratch_ensure((void **) &sc->x_tol, &sc->x_tol_cap, (size_t) nx * sizeof(double));
    double *x_tol = sc->x_tol;
    for (int i = 0; i < nx; i++)
        x_tol[i] = tol + ppm_val * x_mz[i] * 1e-6 + eps_tol;

    /* Step 1: direct matching, y-centric closest-match, sliding window */
    {
        int ilo = 0;
        for (int j = 0; j < ny; j++) {
            double ym = y_mz[j];
            while (ilo < nx && x_mz[ilo] + x_tol[ilo] < ym) ilo++;
            int best_i = -1;
            double best_d = DBL_MAX;
            for (int i = ilo; i < nx; i++) {
                double xm = x_mz[i], ai = x_tol[i];
                if (xm - ai > ym) break;
                double dij = fabs(xm - ym);
                if (dij <= ai && dij < best_d) {
                    best_d = dij;
                    best_i = i;
                }
            }
            if (best_i >= 0 && dm_arr[best_i] < 0) {
                dm_arr[best_i] = j;
                bd_arr[j] = best_i;
            } else if (best_i >= 0) {
                int prev_j = dm_arr[best_i];
                double prev_d = fabs(x_mz[best_i] - y_mz[prev_j]);
                if (best_d < prev_d) {
                    bd_arr[prev_j] = -1;
                    dm_arr[best_i] = j;
                    bd_arr[j] = best_i;
                }
            }
        }
    }

    /* Step 2: shifted matching, same idea on x+pdiff vs y */
    if (do_shift) {
        scratch_ensure((void **) &sc->x_shift_val, &sc->x_shift_val_cap,
                       (size_t) nx * sizeof(double));
        scratch_ensure((void **) &sc->x_shift_tol, &sc->x_shift_tol_cap,
                       (size_t) nx * sizeof(double));
        double *shift_val = sc->x_shift_val;
        double *shift_tol = sc->x_shift_tol;
        for (int i = 0; i < nx; i++) {
            shift_val[i] = x_mz[i] + pdiff;
            shift_tol[i] = tol + ppm_val * fabs(shift_val[i]) * 1e-6 + eps_tol;
        }

        int ilo = 0;
        for (int j = 0; j < ny; j++) {
            double ym = y_mz[j];
            while (ilo < nx && shift_val[ilo] + shift_tol[ilo] < ym) ilo++;
            int best_i = -1;
            double best_d = DBL_MAX;
            for (int i = ilo; i < nx; i++) {
                double shifted = shift_val[i], ai = shift_tol[i];
                if (shifted - ai > ym) break;
                double dij = fabs(shifted - ym);
                if (dij <= ai && dij < best_d) {
                    best_d = dij;
                    best_i = i;
                }
            }
            if (best_i >= 0 && sm_arr[best_i] < 0) {
                sm_arr[best_i] = j;
            } else if (best_i >= 0) {
                int prev_j = sm_arr[best_i];
                double prev_d = fabs(shift_val[best_i] - y_mz[prev_j]);
                if (best_d < prev_d) sm_arr[best_i] = j;
            }
        }
    }

    /* scaled intensities: sqrt(x)*inv_sx and sqrt(y)*inv_sy computed once
     * each instead of per candidate pair - scoring a pair is now just one
     * multiply, no sqrt in the loop at all */
    scratch_ensure((void **) &sc->scaled_x, &sc->scaled_x_cap, (size_t) nx * sizeof(double));
    scratch_ensure((void **) &sc->scaled_y, &sc->scaled_y_cap, (size_t) ny * sizeof(double));
    double *scaled_x = sc->scaled_x;
    double *scaled_y = sc->scaled_y;
    for (int i = 0; i < nx; i++) scaled_x[i] = sqrt(x_int[i]) * inv_sx;
    for (int j = 0; j < ny; j++) scaled_y[j] = sqrt(y_int[j]) * inv_sy;

    double total = 0.0;
    int matched = 0;
    double matched_xsum = 0.0, matched_ysum = 0.0;

    /* cheap, allocation-free check for whether a conflict exists at all -
     * the bitsets below only get built if one actually does (~1% of the
     * time going by real spectral-library data) */
    int has_conflict = 0;
    for (int i = 0; i < nx; i++) {
        int sj = sm_arr[i];
        if (sj < 0) continue;
        int k = bd_arr[sj];
        if (k >= 0 && k != i) { has_conflict = 1; break; }
    }

    if (!has_conflict) {
        for (int i = 0; i < nx; i++) {
            int dm = dm_arr[i], sm = sm_arr[i];
            if (dm < 0 && sm < 0) continue;
            double ds = (dm >= 0) ? scaled_x[i] * scaled_y[dm] : 0.0;
            double ss = (sm >= 0) ? scaled_x[i] * scaled_y[sm] : 0.0;
            if (ds >= ss) {
                total += ds;
                matched_xsum += x_int[i];
                matched_ysum += y_int[dm];
            } else {
                total += ss;
                matched_xsum += x_int[i];
                matched_ysum += y_int[sm];
            }
            matched++;
        }
    } else {
        /* now that we know we need it, build the dirty bitset by redoing
         * the same scan with marking turned on */
        size_t x_bs = (size_t) (((unsigned) nx + 7u) >> 3);
        size_t y_bs = (size_t) (((unsigned) ny + 7u) >> 3);
        scratch_ensure((void **) &sc->x_dirty, &sc->x_dirty_cap, x_bs);
        scratch_ensure((void **) &sc->y_dirty, &sc->y_dirty_cap, y_bs);
        unsigned char *x_dirty = sc->x_dirty;
        unsigned char *y_dirty = sc->y_dirty;
        memset(x_dirty, 0, x_bs);
        memset(y_dirty, 0, y_bs);

        for (int i = 0; i < nx; i++) {
            int sj = sm_arr[i];
            if (sj < 0) continue;
            int k = bd_arr[sj];
            if (k >= 0 && k != i) {
                BS_SET(x_dirty, (unsigned)i);
                BS_SET(x_dirty, (unsigned)k);
                BS_SET(y_dirty, (unsigned)sj);
                if (dm_arr[i] >= 0) BS_SET(y_dirty, (unsigned)dm_arr[i]);
                if (sm_arr[k] >= 0) BS_SET(y_dirty, (unsigned)sm_arr[k]);
                if (dm_arr[k] >= 0) BS_SET(y_dirty, (unsigned)dm_arr[k]);
                if (sm_arr[i] >= 0 && sm_arr[i] != sj) BS_SET(y_dirty, (unsigned)sm_arr[i]);
            }
        }

        /* clean peaks scored greedily, same as the no-conflict path */
        for (int i = 0; i < nx; i++) {
            if (BS_TEST(x_dirty, (unsigned)i)) continue;
            int dm = dm_arr[i], sm = sm_arr[i];
            if (dm >= 0 && BS_TEST(y_dirty, (unsigned)dm)) dm = -1;
            if (sm >= 0 && BS_TEST(y_dirty, (unsigned)sm)) sm = -1;
            if (dm < 0 && sm < 0) continue;
            double ds = (dm >= 0) ? scaled_x[i] * scaled_y[dm] : 0.0;
            double ss = (sm >= 0) ? scaled_x[i] * scaled_y[sm] : 0.0;
            if (ds >= ss) {
                total += ds;
                matched_xsum += x_int[i];
                matched_ysum += y_int[dm];
            } else {
                total += ss;
                matched_xsum += x_int[i];
                matched_ysum += y_int[sm];
            }
            matched++;
        }

        /* Hungarian on the dirty cluster only - all workspace reused */
        scratch_ensure((void **) &sc->xi_map, &sc->xi_map_cap, (size_t) nx * sizeof(int));
        scratch_ensure((void **) &sc->xi_rmap, &sc->xi_rmap_cap, (size_t) nx * sizeof(int));
        int *xi_map = sc->xi_map, *xi_rmap = sc->xi_rmap;
        memset(xi_map, 0xFF, (size_t) nx * sizeof(int));

        int n_xm = 0, n_ym = 0;
        for (int i = 0; i < nx; i++)
            if (BS_TEST(x_dirty, (unsigned)i)) {
                xi_rmap[n_xm] = i;
                xi_map[i] = n_xm++;
            }

        scratch_ensure((void **) &sc->yi_map, &sc->yi_map_cap, (size_t) ny * sizeof(int));
        scratch_ensure((void **) &sc->yi_rmap, &sc->yi_rmap_cap, (size_t) ny * sizeof(int));
        int *yi_map = sc->yi_map, *yi_rmap = sc->yi_rmap;
        memset(yi_map, 0xFF, (size_t) ny * sizeof(int));

        for (int i = 0; i < nx; i++) {
            if (!BS_TEST(x_dirty, (unsigned)i)) continue;
            if (dm_arr[i] >= 0 && BS_TEST(y_dirty, (unsigned)dm_arr[i]) &&
                yi_map[dm_arr[i]] < 0) {
                yi_rmap[n_ym] = dm_arr[i];
                yi_map[dm_arr[i]] = n_ym++;
            }
            if (sm_arr[i] >= 0 && BS_TEST(y_dirty, (unsigned)sm_arr[i]) &&
                yi_map[sm_arr[i]] < 0) {
                yi_rmap[n_ym] = sm_arr[i];
                yi_map[sm_arr[i]] = n_ym++;
            }
        }

        int N = (n_xm > n_ym) ? n_xm : n_ym;
        if (N > 0) {
            scratch_ensure((void **) &sc->smat, &sc->smat_cap,
                           (size_t) N * (size_t) N * sizeof(double));
            double *smat = sc->smat;
            memset(smat, 0, (size_t) N * (size_t) N * sizeof(double));

            for (int i = 0; i < nx; i++) {
                int r = xi_map[i];
                if (r < 0) continue;
                if (dm_arr[i] >= 0 && yi_map[dm_arr[i]] >= 0) {
                    int c = yi_map[dm_arr[i]];
                    double sc_val = scaled_x[i] * scaled_y[dm_arr[i]];
                    if (sc_val > smat[(size_t) r * (size_t) N + (size_t) c])
                        smat[(size_t) r * (size_t) N + (size_t) c] = sc_val;
                }
                if (sm_arr[i] >= 0 && yi_map[sm_arr[i]] >= 0) {
                    int c = yi_map[sm_arr[i]];
                    double sc_val = scaled_x[i] * scaled_y[sm_arr[i]];
                    if (sc_val > smat[(size_t) r * (size_t) N + (size_t) c])
                        smat[(size_t) r * (size_t) N + (size_t) c] = sc_val;
                }
            }

            /* exact shortest-augmenting-path Hungarian, 1-indexed,
             * workspace reused across calls the same way as everything
             * else above */
            size_t N1 = (size_t) (N + 1);
            size_t hneed = N1 * 2 * sizeof(double) + N1 * 2 * sizeof(int) +
                           N1 * sizeof(double) + N1 * sizeof(int);
            scratch_ensure((void **) &sc->hbuf, &sc->hbuf_cap, hneed);
            char *hbuf = sc->hbuf;

            double *hu = (double *) hbuf;
            double *hv = hu + N1;
            int *hp = (int *) (hv + N1);
            int *hway = hp + (int) N1;
            double *hmin = (double *) (hway + (int) N1);
            int *hused = (int *) (hmin + N1);

            memset(hu, 0, N1 * sizeof(double));
            memset(hv, 0, N1 * sizeof(double));
            memset(hp, 0, N1 * sizeof(int));

            for (int i = 1; i <= N; i++) {
                hp[0] = i;
                int j0 = 0;
                for (int j = 0; j <= N; j++) {
                    hmin[j] = DBL_MAX;
                    hused[j] = 0;
                }
                do {
                    hused[j0] = 1;
                    int i0 = hp[j0], j1 = 0;
                    double delta = DBL_MAX;
                    for (int j = 1; j <= N; j++) {
                        if (hused[j]) continue;
                        double cur = -smat[(size_t) (i0 - 1) * (size_t) N + (size_t) (j - 1)]
                                     - hu[i0] - hv[j];
                        if (cur < hmin[j]) {
                            hmin[j] = cur;
                            hway[j] = j0;
                        }
                        if (hmin[j] < delta) {
                            delta = hmin[j];
                            j1 = j;
                        }
                    }
                    for (int j = 0; j <= N; j++) {
                        if (hused[j]) {
                            hu[hp[j]] += delta;
                            hv[j] -= delta;
                        } else { hmin[j] -= delta; }
                    }
                    j0 = j1;
                } while (hp[j0] != 0);
                do {
                    int j1 = hway[j0];
                    hp[j0] = hp[j1];
                    j0 = j1;
                } while (j0);
            }

            for (int j = 1; j <= N; j++) {
                double sc_val = smat[(size_t) (hp[j] - 1) * (size_t) N + (size_t) (j - 1)];
                if (sc_val > 0.0) {
                    total += sc_val;
                    matched++;
                    if (hp[j] - 1 < n_xm) matched_xsum += x_int[xi_rmap[hp[j] - 1]];
                    if (j - 1 < n_ym) matched_ysum += y_int[yi_rmap[j - 1]];
                }
            }
        }
    }

    *out_matched = matched;
    *out_matched_xsum = matched_xsum;
    *out_matched_ysum = matched_ysum;
    return total;
}

/**
 * @brief Score one query spectrum against one library spectrum.
 *
 * The portable (non-R-API-dependent) single-pair entry point - safe to
 * call from a worker thread, since it never touches R's C API. Computes
 * the unique-intensity sums and precursor shift, then delegates the
 * actual matching/scoring to score_matched().
 *
 * @param x_mz, x_int  Query spectrum m/z and intensity arrays, length nx.
 * @param nx           Number of peaks in the query spectrum.
 * @param y_mz, y_int  Library spectrum m/z and intensity arrays, length ny.
 * @param ny           Number of peaks in the library spectrum.
 * @param x_pre, y_pre Precursor m/z of the query and library spectrum.
 *                     NaN on either side disables shifted matching.
 * @param tol          Absolute matching tolerance, in Da.
 * @param ppm_val      Relative matching tolerance, in ppm.
 * @param[out] out     Result: score, matches, score_forward, score_reverse.
 * @return 0 on success, -1 on invalid arguments (null pointer or negative
 *         length). Never fails due to allocation - see gnps_fatal().
 */
int gnps_chain_dp_core_api(
    const double *x_mz, const double *x_int, int nx,
    const double *y_mz, const double *y_int, int ny,
    double x_pre, double y_pre,
    double tol, double ppm_val,
    GnpsCoreResult *out) {
    if (!x_mz || !x_int || !y_mz || !y_int || !out || nx < 0 || ny < 0)
        return -1;

    out->score = 0.0;
    out->matches = 0;
    out->score_forward = 0.0;
    out->score_reverse = 0.0;

    if (nx == 0 || ny == 0) return 0;

    double xsum = 0.0, ysum = 0.0;
    for (int i = 0; i < nx; i++) xsum += x_int[i];
    for (int i = 0; i < ny; i++) ysum += y_int[i];
    if (xsum == 0.0 || ysum == 0.0) return 0;

    double inv_sx = 1.0 / sqrt(xsum), inv_sy = 1.0 / sqrt(ysum);

    int do_shift = (!isnan(x_pre) && !isnan(y_pre));
    double pdiff = 0.0;
    if (do_shift) {
        pdiff = y_pre - x_pre;
        double mp = (x_pre > y_pre) ? x_pre : y_pre;
        if (fabs(pdiff) <= tol + ppm_val * mp * 1e-6) do_shift = 0;
    }

    int matched = 0;
    double matched_xsum = 0.0, matched_ysum = 0.0;
    double score = score_matched(x_mz, x_int, nx, y_mz, y_int, ny,
                                 inv_sx, inv_sy, pdiff, do_shift,
                                 tol, ppm_val, &matched,
                                 &matched_xsum, &matched_ysum);

    double score_fwd = 0.0;
    double score_rev = 0.0;
    if (matched_ysum > 0.0) {
        score_fwd = score * sqrt(ysum) / sqrt(matched_ysum);
        if (score_fwd > 1.0) score_fwd = 1.0;
    }
    if (matched_xsum > 0.0) {
        score_rev = score * sqrt(xsum) / sqrt(matched_xsum);
        if (score_rev > 1.0) score_rev = 1.0;
    }

    out->score = score;
    out->matches = matched;
    out->score_forward = score_fwd;
    out->score_reverse = score_rev;
    return 0;
}

/* ══════════════════════════════════════════════════════════════════════
 * gnps_chain_dp_batch_core_api - one query spectrum against a batch of
 * library spectra, run in parallel across OpenMP threads if enabled.
 *
 * This is the "actually call it in a loop from R thousands of times"
 * problem solved at the C level instead: one .Call() per batch/chunk
 * rather than one per pair, which also cuts the R<->C marshaling overhead
 * that a per-pair loop in R pays every single time. Each thread gets its
 * own score_matched() scratch automatically (thread-local), so no locking
 * is needed here.
 * ══════════════════════════════════════════════════════════════════════ */

/**
 * @brief Score one query spectrum against a batch of library spectra,
 *        optionally in parallel across OpenMP threads.
 *
 * This is the "actually call it in a loop from R thousands of times"
 * problem solved at the C level instead: one call per batch/chunk rather
 * than one per pair, which also cuts the R<->C marshaling overhead that a
 * per-pair loop in R pays every single time. Each thread gets its own
 * score_matched() scratch automatically (thread-local, see GNPS_TLS), so
 * no locking is needed here - just `#pragma omp parallel for`.
 *
 * @param x_mz, x_int  Query spectrum m/z and intensity arrays, length nx.
 * @param nx           Number of peaks in the query spectrum.
 * @param x_pre        Precursor m/z of the query spectrum.
 * @param y_mz_arr     Array of `n_lib` pointers to library m/z arrays.
 * @param y_int_arr    Array of `n_lib` pointers to library intensity
 *                     arrays (parallel to y_mz_arr).
 * @param ny_arr       Array of `n_lib` peak counts, one per library entry.
 * @param y_pre_arr    Array of `n_lib` library precursor m/z values.
 * @param n_lib        Number of library spectra to score against.
 * @param tol          Absolute matching tolerance, in Da.
 * @param ppm_val      Relative matching tolerance, in ppm.
 * @param[out] out     Array of `n_lib` results, one per library entry.
 * @return 0 if every pair scored successfully, -1 if any one failed
 *         (invalid arguments) or if the top-level arguments themselves
 *         are invalid.
 * @note Requires `-fopenmp` (or the package's `SHLIB_OPENMP_CFLAGS`
 *       equivalent) at compile time to actually run in parallel; without
 *       it the pragma is simply ignored and this runs serially.
 */
int gnps_chain_dp_batch_core_api(
    const double *x_mz, const double *x_int, int nx, double x_pre,
    const double **y_mz_arr, const double **y_int_arr, const int *ny_arr,
    const double *y_pre_arr, int n_lib,
    double tol, double ppm_val,
    GnpsCoreResult *out) {
    if (!x_mz || !x_int || !y_mz_arr || !y_int_arr || !ny_arr || !y_pre_arr || !out || n_lib < 0)
        return -1;

    int failures = 0;
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) reduction(+:failures) if(n_lib > 1)
    #endif
    for (int k = 0; k < n_lib; k++) {
        int rc = gnps_chain_dp_core_api(
            x_mz, x_int, nx,
            y_mz_arr[k], y_int_arr[k], ny_arr[k],
            x_pre, y_pre_arr[k], tol, ppm_val,
            &out[k]);
        if (rc != 0) failures++;
    }
    return failures ? -1 : 0;
}

/**
 * @brief Free memory previously returned across the C API boundary
 *        (e.g. GnpsJoinResult::x_idx / y_idx).
 * @param ptr Pointer to free; safe to pass NULL.
 */
void gnps_free_ptr(void *ptr) {
    free(ptr);
}

/**
 * @brief Check whether a double should be treated as "missing".
 *
 * Under the R API, this means R's NA_real_ or a plain NaN; without it,
 * just NaN (there's no separate NA representation in portable C).
 */
static int gnps_is_missing(double v) {
#if GNPS_HAS_R_API
    return ISNA(v) || isnan(v);
#else
    return isnan(v);
#endif
}

/* ══════════════════════════════════════════════════════════════════════
 * gnps_aligned_core_api / gnps_join_core_api - backward-compat paths, not
 * called in a tight per-pair loop the way gnps_chain_dp is, so left as
 * plain one-shot malloc/free like the original. Worth revisiting the same
 * way if profiling ever shows these matter too.
 * ══════════════════════════════════════════════════════════════════════ */

/**
 * @brief Score two already peak-aligned spectra (backward-compatible
 *        path - the caller has already done the join).
 *
 * Unlike gnps_chain_dp_core_api(), this takes two spectra of the *same*
 * length `n` where row `i` in each represents a candidate matched pair
 * (missing/NA on one side for an unmatched peak). Builds a full score
 * matrix over unique m/z groups and solves it exactly with the Hungarian
 * algorithm - O(n^3) in the number of unique groups, but that's small
 * here because the input is already filtered down to matched peaks.
 *
 * @param x_mz, x_int  Query-side aligned m/z and intensity, length n.
 * @param y_mz, y_int  Library-side aligned m/z and intensity, length n.
 * @param n            Number of aligned rows.
 * @param[out] out     Result: score, matches, score_forward ==
 *                     score_reverse == score (pre-aligned inputs have no
 *                     separate forward/reverse normalization).
 * @return 0 on success, -1 on invalid arguments.
 */
int gnps_aligned_core_api(
    const double *x_mz, const double *x_int,
    const double *y_mz, const double *y_int,
    int n,
    GnpsCoreResult *out) {
    if (!x_mz || !x_int || !y_mz || !y_int || !out || n < 0) return -1;

    out->score = 0.0;
    out->matches = 0;
    out->score_forward = 0.0;
    out->score_reverse = 0.0;
    if (n == 0) return 0;

    KP *buf = (KP *) xmalloc((size_t) n * sizeof(KP));
    int m = 0;
    for (int i = 0; i < n; i++) {
        if (!gnps_is_missing(x_mz[i])) {
            buf[m].key = x_mz[i];
            buf[m].pos = i;
            m++;
        }
    }
    qsort(buf, (size_t) m, sizeof(KP), kp_cmp);
    double xs_sum = 0.0;
    for (int i = 0; i < m; i++) {
        if (i == 0 || buf[i].key != buf[i - 1].key) xs_sum += x_int[buf[i].pos];
    }

    m = 0;
    for (int i = 0; i < n; i++) {
        if (!gnps_is_missing(y_mz[i])) {
            buf[m].key = y_mz[i];
            buf[m].pos = i;
            m++;
        }
    }
    qsort(buf, (size_t) m, sizeof(KP), kp_cmp);
    double ys_sum = 0.0;
    for (int i = 0; i < m; i++) {
        if (i == 0 || buf[i].key != buf[i - 1].key) ys_sum += y_int[buf[i].pos];
    }
    free(buf);

    if (xs_sum == 0.0 || ys_sum == 0.0) return 0;

    int l = 0;
    for (int i = 0; i < n; i++) {
        if (!gnps_is_missing(x_mz[i]) && !gnps_is_missing(y_mz[i])) l++;
    }
    if (l == 0) return 0;

    double *keep_xmz = (double *) xmalloc((size_t) l * sizeof(double));
    double *keep_ymz = (double *) xmalloc((size_t) l * sizeof(double));
    double *keep_xin = (double *) xmalloc((size_t) l * sizeof(double));
    double *keep_yin = (double *) xmalloc((size_t) l * sizeof(double));
    {
        int k = 0;
        for (int i = 0; i < n; i++) {
            if (!gnps_is_missing(x_mz[i]) && !gnps_is_missing(y_mz[i])) {
                keep_xmz[k] = x_mz[i];
                keep_ymz[k] = y_mz[i];
                keep_xin[k] = x_int[i];
                keep_yin[k] = y_int[i];
                k++;
            }
        }
    }

    int *x_fac = (int *) xmalloc((size_t) l * sizeof(int));
    int *y_fac = (int *) xmalloc((size_t) l * sizeof(int));
    {
        KP *kpb = (KP *) xmalloc((size_t) l * sizeof(KP));
        for (int i = 0; i < l; i++) {
            kpb[i].key = keep_xmz[i];
            kpb[i].pos = i;
        }
        qsort(kpb, (size_t) l, sizeof(KP), kp_cmp);
        int rank = 0;
        for (int i = 0; i < l; i++) {
            if (i == 0 || kpb[i].key != kpb[i - 1].key) rank++;
            x_fac[kpb[i].pos] = rank;
        }
        for (int i = 0; i < l; i++) {
            kpb[i].key = keep_ymz[i];
            kpb[i].pos = i;
        }
        qsort(kpb, (size_t) l, sizeof(KP), kp_cmp);
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

    int N = (n_xg > n_yg) ? n_xg : n_yg;
    if (N == 0) {
        free(x_fac);
        free(y_fac);
        free(keep_xmz);
        free(keep_ymz);
        free(keep_xin);
        free(keep_yin);
        return 0;
    }

    double inv_sxs = 1.0 / sqrt(xs_sum), inv_sys = 1.0 / sqrt(ys_sum);
    double *sm = (double *) xcalloc((size_t) N * (size_t) N, sizeof(double));
    for (int i = 0; i < l; i++) {
        size_t r = (size_t) (x_fac[i] - 1), c = (size_t) (y_fac[i] - 1);
        sm[r * (size_t) N + c] = sqrt(keep_xin[i]) * inv_sxs
                                 * sqrt(keep_yin[i]) * inv_sys;
    }
    free(x_fac);
    free(y_fac);
    free(keep_xmz);
    free(keep_ymz);
    free(keep_xin);
    free(keep_yin);

    size_t N1 = (size_t) (N + 1);
    size_t buf_sz = N1 * 2 * sizeof(double)
                    + N1 * 2 * sizeof(int)
                    + N1 * sizeof(double)
                    + N1 * sizeof(int);
    char *hbuf = (char *) xmalloc(buf_sz);

    double *u = (double *) hbuf;
    double *v = u + N1;
    int *p = (int *) (v + N1);
    int *way = p + (int) N1;
    double *minv = (double *) (way + (int) N1);
    int *used = (int *) (minv + N1);

    memset(u, 0, N1 * sizeof(double));
    memset(v, 0, N1 * sizeof(double));
    memset(p, 0, N1 * sizeof(int));

    for (int i = 1; i <= N; i++) {
        p[0] = i;
        int j0 = 0;
        for (int j = 0; j <= N; j++) {
            minv[j] = DBL_MAX;
            used[j] = 0;
        }
        do {
            used[j0] = 1;
            int i0 = p[j0], j1 = 0;
            double delta = DBL_MAX;
            for (int j = 1; j <= N; j++) {
                if (used[j]) continue;
                double cur = -sm[(size_t) (i0 - 1) * (size_t) N + (size_t) (j - 1)]
                             - u[i0] - v[j];
                if (cur < minv[j]) {
                    minv[j] = cur;
                    way[j] = j0;
                }
                if (minv[j] < delta) {
                    delta = minv[j];
                    j1 = j;
                }
            }
            for (int j = 0; j <= N; j++) {
                if (used[j]) {
                    u[p[j]] += delta;
                    v[j] -= delta;
                } else { minv[j] -= delta; }
            }
            j0 = j1;
        } while (p[j0] != 0);
        do {
            int j1 = way[j0];
            p[j0] = p[j1];
            j0 = j1;
        } while (j0);
    }

    double total = 0.0;
    int matched = 0;
    for (int j = 1; j <= N; j++) {
        double sc = sm[(size_t) (p[j] - 1) * (size_t) N + (size_t) (j - 1)];
        if (sc > 0.0) {
            total += sc;
            matched++;
        }
    }

    free(hbuf);
    free(sm);

    out->score = total;
    out->matches = matched;
    out->score_forward = total;
    out->score_reverse = total;
    return 0;
}

/**
 * @brief Peak matching only, no scoring (backward-compatible path).
 *
 * Replicates the outer-join semantics of the original MsCoreUtils-style
 * join: direct pass matches x against y, shifted pass matches x+pdiff
 * against y (skipped if the precursor difference is within tolerance),
 * and the result merges direct matches, shifted matches, and unmatched
 * entries from either side (NA on the missing side).
 *
 * @param x    Query m/z values, length nx.
 * @param nx   Number of query values.
 * @param y    Library m/z values, length ny.
 * @param ny   Number of library values.
 * @param x_pre, y_pre Precursor m/z of the query and library spectrum.
 * @param tol      Absolute matching tolerance, in Da.
 * @param ppm_val  Relative matching tolerance, in ppm.
 * @param[out] out Result: parallel `x_idx`/`y_idx` arrays (caller-owned,
 *                 free with gnps_free_ptr()) and `n_rows`, the number of
 *                 rows in the outer join. -1 in either index marks "no
 *                 match on this side" for that row.
 * @return 0 on success, -1 on invalid arguments.
 */
int gnps_join_core_api(
    const double *x, int nx,
    const double *y, int ny,
    double x_pre, double y_pre,
    double tol, double ppm_val,
    GnpsJoinResult *out) {
    if (!x || !y || !out || nx < 0 || ny < 0) return -1;

    out->x_idx = NULL;
    out->y_idx = NULL;
    out->n_rows = 0;

    const double pdiff = y_pre - x_pre;
    const double max_pre = (x_pre > y_pre) ? x_pre : y_pre;
    const double pdiff_tol = tol + ppm_val * fabs(max_pre) * 1e-6;
    const int do_pdiff = (!gnps_is_missing(x_pre) && !gnps_is_missing(y_pre) &&
                          fabs(pdiff) > pdiff_tol);
    const double eps_tol = sqrt(DBL_EPSILON);

    MI *xs_arr = (MI *) xmalloc((size_t) nx * sizeof(MI));
    MI *ys_arr = (MI *) xmalloc((size_t) ny * sizeof(MI));
    int vx = 0, vy = 0;
    for (int i = 0; i < nx; i++) {
        if (!gnps_is_missing(x[i])) {
            xs_arr[vx].mass = x[i];
            xs_arr[vx].idx = i;
            vx++;
        }
    }
    for (int i = 0; i < ny; i++) {
        if (!gnps_is_missing(y[i])) {
            ys_arr[vy].mass = y[i];
            ys_arr[vy].idx = i;
            vy++;
        }
    }
    qsort(xs_arr, (size_t) vx, sizeof(MI), mi_cmp);
    qsort(ys_arr, (size_t) vy, sizeof(MI), mi_cmp);

    int *dm_x = (int *) xmalloc((size_t) vx * sizeof(int));
    int *dm_y = (int *) xmalloc((size_t) vy * sizeof(int));
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
                if (dij <= ai && dij < best_d) {
                    best_d = dij;
                    best_i = i;
                }
            }
            if (best_i >= 0 && dm_x[best_i] < 0) {
                dm_x[best_i] = j;
                dm_y[j] = best_i;
            } else if (best_i >= 0) {
                int prev_j = dm_x[best_i];
                double prev_d = fabs(xs_arr[best_i].mass - ys_arr[prev_j].mass);
                if (best_d < prev_d) {
                    dm_y[prev_j] = -1;
                    dm_x[best_i] = j;
                    dm_y[j] = best_i;
                }
            }
        }
    }

    int *sm_x = (int *) xmalloc((size_t) vx * sizeof(int));
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
                if (dij <= ai && dij < best_d) {
                    best_d = dij;
                    best_i = i;
                }
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

    const size_t bsz = ((size_t) ny + 7u) >> 3;
    unsigned char *y_used = (unsigned char *) xcalloc(bsz, 1);

    int cnt = 0;
    for (int i = 0; i < nx; i++) if (gnps_is_missing(x[i])) cnt++;
    for (int i = 0; i < vx; i++) {
        if (dm_x[i] >= 0) {
            BS_SET(y_used, (unsigned)ys_arr[dm_x[i]].idx);
            cnt++;
        } else {
            cnt++;
        }
    }
    for (int i = 0; i < vx; i++) {
        if (sm_x[i] >= 0) cnt++;
    }
    for (int i = 0; i < ny; i++) {
        if (!gnps_is_missing(y[i]) && !BS_TEST(y_used, (unsigned)i)) cnt++;
    }

    int *ox = (int *) xmalloc((size_t) cnt * sizeof(int));
    int *oy = (int *) xmalloc((size_t) cnt * sizeof(int));
    int pos = 0;

    for (int i = 0; i < nx; i++) {
        if (gnps_is_missing(x[i])) {
            ox[pos] = i;
            oy[pos] = -1;
            pos++;
        }
    }

    for (int i = 0; i < vx; i++) {
        ox[pos] = xs_arr[i].idx;
        oy[pos] = (dm_x[i] >= 0) ? ys_arr[dm_x[i]].idx : -1;
        pos++;
    }

    for (int i = 0; i < vx; i++) {
        if (sm_x[i] >= 0) {
            ox[pos] = xs_arr[i].idx;
            oy[pos] = ys_arr[sm_x[i]].idx;
            pos++;
        }
    }

    for (int i = 0; i < ny; i++) {
        if (!gnps_is_missing(y[i]) && !BS_TEST(y_used, (unsigned)i)) {
            ox[pos] = -1;
            oy[pos] = i;
            pos++;
        }
    }

    free(xs_arr);
    free(ys_arr);
    free(y_used);
    free(dm_x);
    free(dm_y);
    free(sm_x);

    out->x_idx = ox;
    out->y_idx = oy;
    out->n_rows = cnt;
    return 0;
}

#if GNPS_HAS_R_API

/**
 * @brief R entry point: score one query spectrum against one library
 *        spectrum (the hot path).
 * @param x, y              nx2 numeric matrices, columns [mz, intensity],
 *                          sorted by mz (sanitized spectra).
 * @param xPrecursorMz      Scalar, query precursor m/z.
 * @param yPrecursorMz      Scalar, library precursor m/z.
 * @param tolerance         Scalar, absolute tolerance in Da.
 * @param ppm               Scalar, relative tolerance in ppm.
 * @return Numeric vector of length 4:
 *         c(score, matches, score_forward, score_reverse).
 */
SEXP C_gnps_chain_dp(SEXP x, SEXP y,
                     SEXP xPrecursorMz, SEXP yPrecursorMz,
                     SEXP tolerance, SEXP ppm) {
    if (!isReal(x) || !isReal(y)) error("x and y must be numeric matrices");
    SEXP xd = getAttrib(x, R_DimSymbol), yd = getAttrib(y, R_DimSymbol);
    if (!isInteger(xd) || !isInteger(yd)) error("dim must be integer");

    int nx = INTEGER(xd)[0], ny = INTEGER(yd)[0];
    const double *x_mz = REAL(x), *x_int = x_mz + nx;
    const double *y_mz = REAL(y), *y_int = y_mz + ny;
    double x_pre = asReal(xPrecursorMz), y_pre = asReal(yPrecursorMz);
    double tol = asReal(tolerance), ppm_val = asReal(ppm);

    GnpsCoreResult out;
    int rc = gnps_chain_dp_core_api(
        x_mz, x_int, nx,
        y_mz, y_int, ny,
        x_pre, y_pre, tol, ppm_val,
        &out
    );

    if (rc != 0) error("gnps_chain_dp_core_api failed");
    return make_result(out.score, out.matches, out.score_forward, out.score_reverse);
}

/**
 * @brief R entry point: score one query spectrum against a batch of
 *        library spectra, potentially in parallel via OpenMP.
 *
 * All R API access (unpacking `y_list`, reading precursor m/z, etc.)
 * happens here on the main thread before any parallel work starts; the
 * batch loop itself only ever touches raw C arrays. See
 * gnps_chain_dp_batch_core_api() for the actual parallel loop.
 *
 * @param x             nx2 numeric matrix, query spectrum [mz, intensity].
 * @param xPrecursorMz  Scalar, query precursor m/z.
 * @param y_list        List of n-by-2 numeric matrices, one per library
 *                      spectrum (same layout as `y` in C_gnps_chain_dp).
 * @param yPrecursorMz  Numeric vector, one precursor m/z per y_list entry.
 * @param tolerance     Scalar, absolute tolerance in Da.
 * @param ppm           Scalar, relative tolerance in ppm.
 * @return An `n_lib` x 4 numeric matrix: columns score, matches,
 *         score_forward, score_reverse, one row per y_list entry.
 */
SEXP C_gnps_chain_dp_batch(SEXP x, SEXP xPrecursorMz,
                           SEXP y_list, SEXP yPrecursorMz,
                           SEXP tolerance, SEXP ppm) {
    if (!isReal(x)) error("x must be a numeric matrix");
    if (!isNewList(y_list)) error("y_list must be a list of matrices");
    SEXP xd = getAttrib(x, R_DimSymbol);
    if (!isInteger(xd)) error("x must have a dim attribute");

    int nx = INTEGER(xd)[0];
    const double *x_mz = REAL(x), *x_int = x_mz + nx;
    double x_pre = asReal(xPrecursorMz);
    double tol = asReal(tolerance), ppm_val = asReal(ppm);

    int n_lib = LENGTH(y_list);
    if (LENGTH(yPrecursorMz) != n_lib)
        error("yPrecursorMz must have one entry per y_list element");
    const double *y_pre_arr = REAL(yPrecursorMz);

    /* All R API access happens here, on the main thread, before any
     * parallel work starts - the batch loop below only ever touches raw
     * double/int arrays. */
    const double **y_mz_arr  = (const double **) xmalloc((size_t) n_lib * sizeof(double *));
    const double **y_int_arr = (const double **) xmalloc((size_t) n_lib * sizeof(double *));
    int *ny_arr = (int *) xmalloc((size_t) n_lib * sizeof(int));

    for (int k = 0; k < n_lib; k++) {
        SEXP yk = VECTOR_ELT(y_list, k);
        if (!isReal(yk)) error("y_list[[%d]] is not a numeric matrix", k + 1);
        SEXP ykd = getAttrib(yk, R_DimSymbol);
        if (!isInteger(ykd)) error("y_list[[%d]] has no dim attribute", k + 1);
        int nyk = INTEGER(ykd)[0];
        ny_arr[k] = nyk;
        y_mz_arr[k] = REAL(yk);
        y_int_arr[k] = y_mz_arr[k] + nyk;
    }

    GnpsCoreResult *results =
        (GnpsCoreResult *) xmalloc((size_t) n_lib * sizeof(GnpsCoreResult));

    int rc = gnps_chain_dp_batch_core_api(
        x_mz, x_int, nx, x_pre,
        y_mz_arr, y_int_arr, ny_arr, y_pre_arr, n_lib,
        tol, ppm_val, results);

    free(y_mz_arr);
    free(y_int_arr);
    free(ny_arr);

    if (rc != 0) {
        free(results);
        error("gnps_chain_dp_batch_core_api failed");
    }

    SEXP out = PROTECT(allocMatrix(REALSXP, n_lib, 4));
    double *o = REAL(out);
    for (int k = 0; k < n_lib; k++) {
        o[k] = results[k].score;
        o[k + n_lib] = (double) results[k].matches;
        o[k + 2 * n_lib] = results[k].score_forward;
        o[k + 3 * n_lib] = results[k].score_reverse;
    }
    free(results);
    UNPROTECT(1);
    return out;
}

/**
 * @brief R entry point: score two already peak-aligned spectra
 *        (backward-compatible path).
 * @param x, y  nx2 numeric matrices from an outer join - row `i` in each
 *              represents a candidate matched pair (NA on one side if
 *              unmatched).
 * @return Numeric vector of length 4: c(score, matches, score, score) -
 *         forward and reverse normalization are identical for pre-aligned
 *         input.
 */
SEXP C_gnps(SEXP x, SEXP y) {
    if (!isReal(x) || !isReal(y)) error("Inputs must be numeric matrices");
    SEXP xd = getAttrib(x, R_DimSymbol), yd = getAttrib(y, R_DimSymbol);
    if (!isInteger(xd) || !isInteger(yd)) error("dim must be integer");
    int n = INTEGER(xd)[0];
    if (n != INTEGER(yd)[0]) error("number of rows");
    const double *xmz = REAL(x), *xin = xmz + n;
    const double *ymz = REAL(y), *yin = ymz + n;

    KP *buf = (KP *) xmalloc((size_t) n * sizeof(KP));
    int m = 0;
    for (int i = 0; i < n; i++)
        if (!ISNA(xmz[i])) {
            buf[m].key = xmz[i];
            buf[m].pos = i;
            m++;
        }
    qsort(buf, (size_t) m, sizeof(KP), kp_cmp);
    double xs_sum = 0.0;
    for (int i = 0; i < m; i++)
        if (i == 0 || buf[i].key != buf[i - 1].key) xs_sum += xin[buf[i].pos];

    m = 0;
    for (int i = 0; i < n; i++)
        if (!ISNA(ymz[i])) {
            buf[m].key = ymz[i];
            buf[m].pos = i;
            m++;
        }
    qsort(buf, (size_t) m, sizeof(KP), kp_cmp);
    double ys_sum = 0.0;
    for (int i = 0; i < m; i++)
        if (i == 0 || buf[i].key != buf[i - 1].key) ys_sum += yin[buf[i].pos];
    free(buf);

    if (xs_sum == 0.0 || ys_sum == 0.0) return make_result(0.0, 0, 0.0, 0.0);

    int l = 0;
    for (int i = 0; i < n; i++)
        if (!ISNA(xmz[i]) && !ISNA(ymz[i])) l++;
    if (!l) return make_result(0.0, 0, 0.0, 0.0);

    double *keep_xmz = (double *) xmalloc((size_t) l * sizeof(double));
    double *keep_ymz = (double *) xmalloc((size_t) l * sizeof(double));
    double *keep_xin = (double *) xmalloc((size_t) l * sizeof(double));
    double *keep_yin = (double *) xmalloc((size_t) l * sizeof(double));
    {
        int k = 0;
        for (int i = 0; i < n; i++) {
            if (!ISNA(xmz[i]) && !ISNA(ymz[i])) {
                keep_xmz[k] = xmz[i];
                keep_ymz[k] = ymz[i];
                keep_xin[k] = xin[i];
                keep_yin[k] = yin[i];
                k++;
            }
        }
    }

    int *x_fac = (int *) xmalloc((size_t) l * sizeof(int));
    int *y_fac = (int *) xmalloc((size_t) l * sizeof(int));
    {
        KP *kpb = (KP *) xmalloc((size_t) l * sizeof(KP));
        for (int i = 0; i < l; i++) {
            kpb[i].key = keep_xmz[i];
            kpb[i].pos = i;
        }
        qsort(kpb, (size_t) l, sizeof(KP), kp_cmp);
        int rank = 0;
        for (int i = 0; i < l; i++) {
            if (i == 0 || kpb[i].key != kpb[i - 1].key) rank++;
            x_fac[kpb[i].pos] = rank;
        }
        for (int i = 0; i < l; i++) {
            kpb[i].key = keep_ymz[i];
            kpb[i].pos = i;
        }
        qsort(kpb, (size_t) l, sizeof(KP), kp_cmp);
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
        free(x_fac);
        free(y_fac);
        free(keep_xmz);
        free(keep_ymz);
        free(keep_xin);
        free(keep_yin);
        return make_result(0.0, 0, 0.0, 0.0);
    }

    double *sm = (double *) xcalloc((size_t) N * (size_t) N, sizeof(double));
    for (int i = 0; i < l; i++) {
        size_t r = (size_t) (x_fac[i] - 1), c = (size_t) (y_fac[i] - 1);
        sm[r * (size_t) N + c] = sqrt(keep_xin[i]) * inv_sxs
                                 * sqrt(keep_yin[i]) * inv_sys;
    }
    free(x_fac);
    free(y_fac);
    free(keep_xmz);
    free(keep_ymz);
    free(keep_xin);
    free(keep_yin);

    {
        size_t N1 = (size_t) (N + 1);
        size_t buf_sz = N1 * 2 * sizeof(double)
                        + N1 * 2 * sizeof(int)
                        + N1 * sizeof(double)
                        + N1 * sizeof(int);
        char *hbuf = (char *) xmalloc(buf_sz);

        double *u = (double *) hbuf;
        double *v = u + N1;
        int *p = (int *) (v + N1);
        int *way = p + (int) N1;
        double *minv = (double *) (way + (int) N1);
        int *used = (int *) (minv + N1);

        memset(u, 0, N1 * sizeof(double));
        memset(v, 0, N1 * sizeof(double));
        memset(p, 0, N1 * sizeof(int));

        for (int i = 1; i <= N; i++) {
            p[0] = i;
            int j0 = 0;
            for (int j = 0; j <= N; j++) {
                minv[j] = DBL_MAX;
                used[j] = 0;
            }
            do {
                used[j0] = 1;
                int i0 = p[j0], j1 = 0;
                double delta = DBL_MAX;
                for (int j = 1; j <= N; j++) {
                    if (used[j]) continue;
                    double cur = -sm[(size_t) (i0 - 1) * (size_t) N + (size_t) (j - 1)]
                                 - u[i0] - v[j];
                    if (cur < minv[j]) {
                        minv[j] = cur;
                        way[j] = j0;
                    }
                    if (minv[j] < delta) {
                        delta = minv[j];
                        j1 = j;
                    }
                }
                for (int j = 0; j <= N; j++) {
                    if (used[j]) {
                        u[p[j]] += delta;
                        v[j] -= delta;
                    } else { minv[j] -= delta; }
                }
                j0 = j1;
            } while (p[j0] != 0);
            do {
                int j1 = way[j0];
                p[j0] = p[j1];
                j0 = j1;
            } while (j0);
        }

        double total = 0.0;
        int matched = 0;
        for (int j = 1; j <= N; j++) {
            double sc = sm[(size_t) (p[j] - 1) * (size_t) N + (size_t) (j - 1)];
            if (sc > 0.0) {
                total += sc;
                matched++;
            }
        }

        free(hbuf);
        free(sm);
        return make_result(total, matched, total, total);
    }
}

/**
 * @brief R entry point: peak matching only, no scoring (backward-
 *        compatible path).
 * @param x, y             Numeric vectors of m/z values.
 * @param xPrecursorMz     Scalar, query precursor m/z.
 * @param yPrecursorMz     Scalar, library precursor m/z.
 * @param tolerance        Scalar, absolute tolerance in Da.
 * @param ppm              Scalar, relative tolerance in ppm.
 * @return A list with elements `x` and `y`: parallel 1-based integer
 *         index vectors forming the outer join (NA on the missing side).
 */
SEXP C_join_gnps(SEXP x, SEXP y,
                 SEXP xPrecursorMz, SEXP yPrecursorMz,
                 SEXP tolerance, SEXP ppm) {
    if (!isReal(x) || !isReal(y)) error("x and y must be numeric vectors");
    const double *xp = REAL(x), *yp = REAL(y);
    const double x_pre = asReal(xPrecursorMz), y_pre = asReal(yPrecursorMz);
    const double tol = asReal(tolerance), ppm_val = asReal(ppm);
    const int nx = (int) xlength(x), ny = (int) xlength(y);
    const double pdiff = y_pre - x_pre;
    const double max_pre = (x_pre > y_pre) ? x_pre : y_pre;
    const double pdiff_tol = tol + ppm_val * fabs(max_pre) * 1e-6;
    const int do_pdiff = (!ISNA(x_pre) && !ISNA(y_pre) && fabs(pdiff) > pdiff_tol);
    const double eps_tol = sqrt(DBL_EPSILON);

    MI *xs_arr = (MI *) xmalloc((size_t) nx * sizeof(MI));
    MI *ys_arr = (MI *) xmalloc((size_t) ny * sizeof(MI));
    int vx = 0, vy = 0;
    for (int i = 0; i < nx; i++)
        if (!ISNA(xp[i])) {
            xs_arr[vx].mass = xp[i];
            xs_arr[vx].idx = i;
            vx++;
        }
    for (int i = 0; i < ny; i++)
        if (!ISNA(yp[i])) {
            ys_arr[vy].mass = yp[i];
            ys_arr[vy].idx = i;
            vy++;
        }
    qsort(xs_arr, (size_t) vx, sizeof(MI), mi_cmp);
    qsort(ys_arr, (size_t) vy, sizeof(MI), mi_cmp);

    int *dm_x = (int *) xmalloc((size_t) vx * sizeof(int));
    int *dm_y = (int *) xmalloc((size_t) vy * sizeof(int));
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
                if (dij <= ai && dij < best_d) {
                    best_d = dij;
                    best_i = i;
                }
            }
            if (best_i >= 0 && dm_x[best_i] < 0) {
                dm_x[best_i] = j;
                dm_y[j] = best_i;
            } else if (best_i >= 0) {
                int prev_j = dm_x[best_i];
                double prev_d = fabs(xs_arr[best_i].mass - ys_arr[prev_j].mass);
                if (best_d < prev_d) {
                    dm_y[prev_j] = -1;
                    dm_x[best_i] = j;
                    dm_y[j] = best_i;
                }
            }
        }
    }

    int *sm_x = (int *) xmalloc((size_t) vx * sizeof(int));
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
                if (dij <= ai && dij < best_d) {
                    best_d = dij;
                    best_i = i;
                }
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

    const size_t bsz = ((size_t) ny + 7u) >> 3;
    unsigned char *y_used = (unsigned char *) xcalloc(bsz, 1);

    int cnt = 0;
    for (int i = 0; i < nx; i++) if (ISNA(xp[i])) cnt++;
    for (int i = 0; i < vx; i++) {
        if (dm_x[i] >= 0) {
            BS_SET(y_used, (unsigned)ys_arr[dm_x[i]].idx);
            cnt++;
        } else {
            cnt++;
        }
    }
    for (int i = 0; i < vx; i++) {
        if (sm_x[i] >= 0) cnt++;
    }
    for (int i = 0; i < ny; i++)
        if (!ISNA(yp[i]) && !BS_TEST(y_used, (unsigned)i)) cnt++;

    SEXP rx = PROTECT(allocVector(INTSXP, cnt));
    SEXP ry = PROTECT(allocVector(INTSXP, cnt));
    int *ox = INTEGER(rx), *oy = INTEGER(ry), pos = 0;

    for (int i = 0; i < nx; i++)
        if (ISNA(xp[i])) {
            ox[pos] = i + 1;
            oy[pos] = NA_INTEGER;
            pos++;
        }

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

    for (int i = 0; i < vx; i++) {
        if (sm_x[i] >= 0) {
            ox[pos] = xs_arr[i].idx + 1;
            oy[pos] = ys_arr[sm_x[i]].idx + 1;
            pos++;
        }
    }

    for (int i = 0; i < ny; i++)
        if (!ISNA(yp[i]) && !BS_TEST(y_used, (unsigned)i)) {
            ox[pos] = NA_INTEGER;
            oy[pos] = i + 1;
            pos++;
        }

    free(xs_arr);
    free(ys_arr);
    free(y_used);
    free(dm_x);
    free(dm_y);
    free(sm_x);

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
#endif
