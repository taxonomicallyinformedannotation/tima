/**
 * @brief GNPS Spectral Similarity — Optimized Exact LAPJV
 *
 * Mathematically equivalent to MsCoreUtils::gnps:
 *
 *   x_sum <- sum(x[!duplicated(x[,1]), 2], na.rm=TRUE)
 *   y_sum <- sum(y[!duplicated(y[,1]), 2], na.rm=TRUE)
 *   keep  <- complete.cases(x[,1], y[,1])
 *   scores <- sqrt(x[keep,2])/sqrt(x_sum) * sqrt(y[keep,2])/sqrt(y_sum)
 *   x_idx  <- as.integer(factor(x[keep,1]))   # 1..mx
 *   y_idx  <- as.integer(factor(y[keep,1]))   # 1..my
 *   # score_mat is mx x my; last write wins for duplicate (x_idx,y_idx)
 *   score_mat[x_idx[i], y_idx[i]] <- scores[i]
 *   m <- max(mx, my)   # square up for LSAP
 *   solve_LSAP on padded m x m matrix
 *
 * LAPJV (Jonker-Volgenant) replaces solve_LSAP (Hungarian); same optimal.
 *   m <= DENSE_THRESHOLD : O(m^2) linear-scan Dijkstra
 *   m >  DENSE_THRESHOLD : O(arc_count * log m) binary-heap Dijkstra
 *
 * Accepts the raw output of join_gnps (NA rows included) — no R-side
 * pre-filtering or sorting required.
 */

#include <R.h>
#include <Rinternals.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>

#define DENSE_THRESHOLD 16

static void *xmalloc(size_t n) {
  void *p = malloc(n);
  if (!p) error("malloc(%zu) failed", n);
  return p;
}
static void *xcalloc(size_t n, size_t sz) {
  void *p = calloc(n, sz);
  if (!p) error("calloc(%zu) failed", n*sz);
  return p;
}

/* ── (key,pos) sort ─────────────────────────────────────────────────────── */
typedef struct { double key; int pos; } KP;
static int kp_cmp(const void *a, const void *b) {
  double da = ((const KP*)a)->key, db = ((const KP*)b)->key;
  return (da < db) ? -1 : (da > db) ? 1 : 0;
}

/*
 * sum(col[!duplicated(col)], na.rm=TRUE)
 * Walks rows in original order; first occurrence of each unique non-NA mz
 * contributes its intensity. NAs excluded (na.rm=TRUE).
 */
static double unique_sum(const double *mz, const double *inten, int n) {
  KP *buf = (KP*) xmalloc((size_t)n * sizeof(KP));
  int m = 0;
  for (int i = 0; i < n; i++)
    if (!ISNA(mz[i])) { buf[m].key = mz[i]; buf[m].pos = i; m++; }
    qsort(buf, (size_t)m, sizeof(KP), kp_cmp);
    /* Walk sorted; first occurrence = smallest original index among equals */
    /* But R's !duplicated marks by first occurrence in original order.
     * After sorting by mz, ties are not in original order — need the
     * minimum pos among each group.                                        */
    double s = 0.0;
    int i = 0;
    while (i < m) {
      int j = i;
      int min_pos = buf[i].pos;
      while (j < m && buf[j].key == buf[i].key) {
        if (buf[j].pos < min_pos) min_pos = buf[j].pos;
        j++;
      }
      s += inten[min_pos];
      i = j;
    }
    free(buf);
    return s;
}

/*
 * Build factor indices (1-based rank by sorted unique mz) for kept rows,
 * and return count l, mx (max x factor), my (max y factor).
 */
static int build_factors(
    const double *xmz, const double *ymz, int n,
    int *x_fac, int *y_fac,
    int *out_mx, int *out_my,
    KP *buf)
{
  /* find kept rows */
  int l = 0;
  for (int i = 0; i < n; i++)
    if (!ISNA(xmz[i]) && !ISNA(ymz[i])) l++;
    if (!l) { *out_mx = 0; *out_my = 0; return 0; }
    
    /* x factors */
    { int k = 0;
      for (int i = 0; i < n; i++)
        if (!ISNA(xmz[i]) && !ISNA(ymz[i])) { buf[k].key = xmz[i]; buf[k].pos = k; k++; }
        qsort(buf, (size_t)l, sizeof(KP), kp_cmp);
        int rank = 0;
        for (int i = 0; i < l; i++) {
          if (i == 0 || buf[i].key != buf[i-1].key) rank++;
          x_fac[buf[i].pos] = rank;
        }
        *out_mx = rank;
    }
    /* y factors */
    { int k = 0;
      for (int i = 0; i < n; i++)
        if (!ISNA(xmz[i]) && !ISNA(ymz[i])) { buf[k].key = ymz[i]; buf[k].pos = k; k++; }
        qsort(buf, (size_t)l, sizeof(KP), kp_cmp);
        int rank = 0;
        for (int i = 0; i < l; i++) {
          if (i == 0 || buf[i].key != buf[i-1].key) rank++;
          y_fac[buf[i].pos] = rank;
        }
        *out_my = rank;
    }
    return l;
}

/* ── zero result ────────────────────────────────────────────────────────── */
static SEXP zero_result(void) {
  SEXP r = PROTECT(allocVector(VECSXP, 2));
  SEXP nm = PROTECT(allocVector(STRSXP, 2));
  SET_VECTOR_ELT(r, 0, ScalarReal(0.0));
  SET_VECTOR_ELT(r, 1, ScalarInteger(0));
  SET_STRING_ELT(nm, 0, mkChar("score"));
  SET_STRING_ELT(nm, 1, mkChar("matches"));
  setAttrib(r, R_NamesSymbol, nm);
  UNPROTECT(2); return r;
}

/* ══════════════════════════════════════════════════════════════════════════
 * DENSE LAPJV
 * score[r*m+c] NEGATED. m = problem size (= max(mx,my)).
 * row2col[r] = assigned col (0..m-1), or m for dummy.
 * ══════════════════════════════════════════════════════════════════════════ */
static void lapjv_dense(int m, const double *score, int *row2col)
{
  int N = m + 1;
  size_t db = (size_t)N * sizeof(double);
  size_t ib = (size_t)N * sizeof(int);
  char *blk = (char*) xmalloc(3*db + 3*ib);
  double *u    = (double*)(blk);
  double *v    = (double*)(blk +   db);
  double *d    = (double*)(blk + 2*db);
  int    *p    = (int*)   (blk + 3*db);
  int    *prev = (int*)   (blk + 3*db +   ib);
  int    *done = (int*)   (blk + 3*db + 2*ib);

  memset(blk, 0, 3*db + 3*ib);
  for (int j = 0; j < N; j++) p[j] = -1;
  for (int i = 0; i < m; i++) row2col[i] = -1;

  for (int i = 0; i < m; i++) {
    for (int j = 0; j < N; j++) { d[j] = DBL_MAX; prev[j] = -1; done[j] = 0; }
    const double ui = u[i];
    const double *ri = score + (size_t)i * m;
    for (int j = 0; j < m; j++) { double r = ri[j]-ui-v[j]; if (r < d[j]) d[j] = r; }
    { double r = -ui-v[m]; if (r < d[m]) d[m] = r; }

    int j_aug = -1;
    for (int iter = 0; iter < N; iter++) {
      double dmin = DBL_MAX; int jmin = -1;
      for (int j = 0; j < N; j++)
        if (!done[j] && d[j] < dmin) { dmin = d[j]; jmin = j; }
        if (jmin < 0) break;
        done[jmin] = 1;
        if (p[jmin] == -1) { j_aug = jmin; break; }
        int i2 = p[jmin];
        const double *ri2 = score + (size_t)i2 * m;
        double base = dmin - u[i2] + v[jmin];
        for (int j2 = 0; j2 < m; j2++) {
          if (done[j2]) continue;
          double r = base + ri2[j2] - v[j2];
          if (r < d[j2]) { d[j2] = r; prev[j2] = jmin; }
        }
        if (!done[m]) { double r = base-v[m]; if (r < d[m]) { d[m]=r; prev[m]=jmin; } }
    }
    if (j_aug < 0) error("LAPJV dense: infeasible");
    double da = d[j_aug];
    u[i] += da;
    for (int j = 0; j < N; j++) {
      if (!done[j]) continue;
      if (p[j] != -1) u[p[j]] += d[j] - da;
      v[j] -= d[j] - da;
    }
    for (int jc = j_aug; jc != -1;) {
      int jp = prev[jc], ic = (jp==-1) ? i : p[jp];
      p[jc] = ic; row2col[ic] = jc; jc = jp;
    }
  }
  free(blk);
}

/* ══════════════════════════════════════════════════════════════════════════
 * BINARY HEAP
 * ══════════════════════════════════════════════════════════════════════════ */
typedef struct { double key; int val; } HNode;
static inline void h_push(HNode *h, int *sz, double key, int val) {
  int i = (*sz)++; h[i] = (HNode){key,val};
  while (i > 0) { int p=(i-1)>>1; if (h[p].key<=h[i].key) break;
  HNode t=h[p]; h[p]=h[i]; h[i]=t; i=p; }
}
static inline HNode h_pop(HNode *h, int *sz) {
  HNode top=h[0]; h[0]=h[--(*sz)]; int i=0;
  for(;;){ int l=2*i+1,r=2*i+2,s=i;
    if(l<*sz&&h[l].key<h[s].key)s=l;
    if(r<*sz&&h[r].key<h[s].key)s=r;
    if(s==i)break; HNode t=h[s];h[s]=h[i];h[i]=t;i=s; }
  return top;
}

/* ══════════════════════════════════════════════════════════════════════════
 * SPARSE LAPJV
 * CSR over unique (row,col) arcs; arc_count = number of unique cells.
 * ══════════════════════════════════════════════════════════════════════════ */
static void lapjv_sparse(int m,
                         const double *cc, const int *kk, const int *first,
                         int arc_count, int *row2col)
{
  int N = m + 1;
  size_t db = (size_t)N * sizeof(double);
  size_t ib = (size_t)N * sizeof(int);
  size_t hb = (size_t)(arc_count + m + 2) * sizeof(HNode);
  char *blk = (char*) xmalloc(3*db + 4*ib + hb);
  double *u       = (double*)(blk);
  double *v       = (double*)(blk +   db);
  double *dist    = (double*)(blk + 2*db);
  int    *p       = (int*)   (blk + 3*db);
  int    *prev    = (int*)   (blk + 3*db +   ib);
  int    *vis_gen = (int*)   (blk + 3*db + 2*ib);
  int    *stk     = (int*)   (blk + 3*db + 3*ib);
  HNode  *heap    = (HNode*) (blk + 3*db + 4*ib);

  memset(blk, 0, 3*db + 4*ib);
  for (int j = 0; j < N; j++) p[j] = -1;
  for (int i = 0; i < m; i++) row2col[i] = -1;

  int cur_gen = 0;
  for (int i = 0; i < m; i++) {
    cur_gen++;
    int hsz = 0, nstk = 0;
    const double ui = u[i];
    for (int e = first[i]; e < first[i+1]; e++) {
      int jj = kk[e]; double red = cc[e]-ui-v[jj];
      if (vis_gen[jj]!=cur_gen) {
        vis_gen[jj]=cur_gen; dist[jj]=red; prev[jj]=-1;
        h_push(heap,&hsz,red,jj);
      } else if (red<dist[jj]) { dist[jj]=red; prev[jj]=-1; h_push(heap,&hsz,red,jj); }
    }
    { double red=-ui-v[m];
      if (vis_gen[m]!=cur_gen) { vis_gen[m]=cur_gen; dist[m]=red; prev[m]=-1; h_push(heap,&hsz,red,m); }
      else if (red<dist[m]) { dist[m]=red; prev[m]=-1; h_push(heap,&hsz,red,m); }
    }
    int j_aug=-1;
    while (hsz>0) {
      HNode hn=h_pop(heap,&hsz); int jj=hn.val;
      if (hn.key>dist[jj]) continue;
      vis_gen[jj]=-cur_gen; stk[nstk++]=jj;
      if (p[jj]==-1) { j_aug=jj; break; }
      int i2=p[jj]; double base=dist[jj]-u[i2]+v[jj];
      for (int e=first[i2]; e<first[i2+1]; e++) {
        int jj2=kk[e]; if (vis_gen[jj2]==-cur_gen) continue;
        double red=base+cc[e]-v[jj2];
        if (vis_gen[jj2]!=cur_gen) { vis_gen[jj2]=cur_gen; dist[jj2]=red; prev[jj2]=jj; h_push(heap,&hsz,red,jj2); }
        else if (red<dist[jj2]) { dist[jj2]=red; prev[jj2]=jj; h_push(heap,&hsz,red,jj2); }
      }
      if (vis_gen[m]!=-cur_gen) {
        double red=base-v[m];
        if (vis_gen[m]!=cur_gen) { vis_gen[m]=cur_gen; dist[m]=red; prev[m]=jj; h_push(heap,&hsz,red,m); }
        else if (red<dist[m]) { dist[m]=red; prev[m]=jj; h_push(heap,&hsz,red,m); }
      }
    }
    if (j_aug<0) error("LAPJV sparse: infeasible");
    double da=dist[j_aug]; u[i]+=da;
    for (int k=0; k<nstk; k++) { int j=stk[k]; if (p[j]!=-1) u[p[j]]+=dist[j]-da; v[j]-=dist[j]-da; }
    for (int jc=j_aug; jc!=-1;) { int jp=prev[jc],ic=(jp==-1)?i:p[jp]; p[jc]=ic; row2col[ic]=jc; jc=jp; }
  }
  free(blk);
}

/* ── Arc type and comparator for sparse dedup ──────────────────────────── */
typedef struct { int row, col; double sc; } Arc;
static int arc_cmp(const void *a, const void *b) {
  const Arc *aa = (const Arc*)a, *bb = (const Arc*)b;
  if (aa->row != bb->row) return aa->row - bb->row;
  return aa->col - bb->col;
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

  const double *xmz = REAL(x);
  const double *ymz = REAL(y);
  const double *xin = xmz + n;
  const double *yin = ymz + n;

  /* x_sum, y_sum */
  double xs = unique_sum(xmz, xin, n);
  double ys = unique_sum(ymz, yin, n);
  if (xs == 0.0 || ys == 0.0) return zero_result();

  /* factor indices for kept rows */
  int *x_fac = (int*) xmalloc((size_t)n * sizeof(int));
  int *y_fac = (int*) xmalloc((size_t)n * sizeof(int));
  KP  *buf   = (KP*)  xmalloc((size_t)n * sizeof(KP));
  int mx, my;
  int l = build_factors(xmz, ymz, n, x_fac, y_fac, &mx, &my, buf);
  free(buf);

  if (!l) { free(x_fac); free(y_fac); return zero_result(); }
  
  /* per-pair scores */
  const double sqrt_xs = sqrt(xs);
  const double sqrt_ys = sqrt(ys);
  double *scores = (double*) xmalloc((size_t)l * sizeof(double));
  { int idx = 0;
    for (int i = 0; i < n && idx < l; i++)
      if (!ISNA(xmz[i]) && !ISNA(ymz[i]))
        scores[idx++] = (sqrt(xin[i]) / sqrt_xs) * (sqrt(yin[i]) / sqrt_ys);
  }
  
  /*
   * Problem size: m = max(mx, my).
   * Score matrix is mx × my logically, but we square it to m × m
   * (extra rows/cols stay zero = dummy, handled by LAPJV dummy column).
   * Last-write wins for duplicate (x_fac, y_fac) — matches reference.
   */
  int m = (mx > my) ? mx : my;
  double total = 0.0;
  int matched  = 0;

  if (m <= DENSE_THRESHOLD) {
    double *smat = (double*) xcalloc((size_t)m * m, sizeof(double));
    for (int i = 0; i < l; i++)
      smat[(size_t)(x_fac[i]-1)*m + (y_fac[i]-1)] = -scores[i];

    int *row2col = (int*) xmalloc((size_t)(m+1) * sizeof(int));
    lapjv_dense(m, smat, row2col);
    for (int r = 0; r < m; r++) {
      int c = row2col[r];
      if (c >= 0 && c < m) {
        double sv = -smat[(size_t)r*m + c];
        if (sv > 0.0) { total += sv; matched++; }
      }
    }
    free(smat); free(row2col);

  } else {
    /*
     * Build CSR over UNIQUE (x_fac-1, y_fac-1) cells.
     * Duplicate input pairs map to the same cell; last score wins
     * (matching dense path and reference behavior).
     * We first write all pairs into a flat (row,col,score) list,
     * sort by (row,col), then deduplicate keeping last occurrence.
     */
    Arc *arcs = (Arc*) xmalloc((size_t)l * sizeof(Arc));
    for (int i = 0; i < l; i++) {
      arcs[i].row = x_fac[i]-1;
      arcs[i].col = y_fac[i]-1;
      arcs[i].sc  = scores[i];
    }
    /* Sort by (row, col) — uses file-scope arc_cmp */
    qsort(arcs, (size_t)l, sizeof(Arc), arc_cmp);
    
    /* Deduplicate: for equal (row,col) keep last (= last in original
     * iteration order = largest i, matching reference last-write).
     * After sorting equal (row,col) are adjacent; last one wins.     */
    int u_count = 0;
    for (int i = 0; i < l; i++) {
      if (i+1 < l && arcs[i].row==arcs[i+1].row && arcs[i].col==arcs[i+1].col)
        continue; /* next entry overwrites this cell, skip */
    arcs[u_count++] = arcs[i];
    }
    
    /* Build CSR from deduplicated arcs */
    int *rowcnt = (int*) xcalloc((size_t)m, sizeof(int));
    for (int i = 0; i < u_count; i++) rowcnt[arcs[i].row]++;
    int *first = (int*) xmalloc((size_t)(m+1) * sizeof(int));
    first[0] = 0;
    for (int r = 0; r < m; r++) first[r+1] = first[r] + rowcnt[r];
    free(rowcnt);

    double *cc     = (double*) xmalloc((size_t)u_count * sizeof(double));
    int    *kk     = (int*)    xmalloc((size_t)u_count * sizeof(int));
    double *col_sc = (double*) xmalloc((size_t)u_count * sizeof(double));
    int    *cur    = (int*)    xcalloc((size_t)m, sizeof(int));
    memcpy(cur, first, (size_t)m * sizeof(int));
    for (int i = 0; i < u_count; i++) {
      int pos = cur[arcs[i].row]++;
      cc[pos]     = -arcs[i].sc;
      kk[pos]     =  arcs[i].col;
      col_sc[pos] =  arcs[i].sc;
    }
    free(cur); free(arcs);
    /* Arcs were sorted by (row,col) so CSR columns are already sorted */

    int *row2col = (int*) xmalloc((size_t)(m+1) * sizeof(int));
    lapjv_sparse(m, cc, kk, first, u_count, row2col);

    for (int r = 0; r < m; r++) {
      int c = row2col[r];
      if (c < 0 || c >= m) continue;
      int lo = first[r], hi = first[r+1]-1;
      while (lo <= hi) {
        int mid = (lo+hi)>>1;
        if (kk[mid]==c) { if (col_sc[mid]>0.0) { total+=col_sc[mid]; matched++; } break; }
        if (kk[mid]<c) lo=mid+1; else hi=mid-1;
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
