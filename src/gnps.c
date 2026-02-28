/**
 * @brief Fused GNPS join + score — single C call, zero R-level matrix alloc.
 *
 * gnps_compute(x, y, xPrecursorMz, yPrecursorMz, tolerance, ppm)
 *
 *   Takes the raw peak matrices x and y directly (no pre-alignment needed).
 *   Performs join_gnps matching and gnps scoring internally in one pass.
 *   Returns list(score, matches).
 *
 * Also exports the standalone:
 *   gnps(x, y)          — score aligned matrices (backward compat)
 *   join_gnps(x,y,...)  — peak matching only (backward compat)
 */

#include <R.h>
#include <Rinternals.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>

/* ── tunables ────────────────────────────────────────────────────────────── */
#define DENSE_THRESHOLD 16   /* use dense LAPJV when m <= this */

/* ── helpers ─────────────────────────────────────────────────────────────── */
static void *xmalloc(size_t n) {
  void *p = malloc(n); if (!p) error("malloc(%zu)", n); return p; }
static void *xcalloc(size_t n, size_t s) {
  void *p = calloc(n,s); if (!p) error("calloc(%zu)", n*s); return p; }

/* ── bitset ──────────────────────────────────────────────────────────────── */
#define BS_SET(b,i)  ((b)[(i)>>3] |=  (unsigned char)(1u<<((i)&7u)))
#define BS_TEST(b,i) ((b)[(i)>>3] &   (unsigned char)(1u<<((i)&7u)))

/* ── (key,pos) sort ──────────────────────────────────────────────────────── */
typedef struct { double key; int pos; } KP;
static int kp_cmp(const void *a, const void *b) {
  double da=((const KP*)a)->key, db=((const KP*)b)->key;
  return (da<db)?-1:(da>db)?1:0; }

/* ── (row,col,score) arc for dedup ──────────────────────────────────────── */
typedef struct { int row,col; double sc; } Arc;
static int arc_cmp(const void *a, const void *b) {
  const Arc *aa=(const Arc*)a, *bb=(const Arc*)b;
  return (aa->row!=bb->row) ? aa->row-bb->row : aa->col-bb->col; }

/* ── MassIndex for join ──────────────────────────────────────────────────── */
typedef struct { double mass; int idx; } MI;
static int mi_cmp(const void *a, const void *b) {
  double da=((const MI*)a)->mass, db=((const MI*)b)->mass;
  return (da<db)?-1:(da>db)?1:0; }
static int lower_bound(const MI *arr, int n, double lb) {
  int lo=0,hi=n;
  while(lo<hi){ int mid=lo+((hi-lo)>>1); if(arr[mid].mass<lb) lo=mid+1; else hi=mid; }
  return lo; }

/* ── zero result ─────────────────────────────────────────────────────────── */
static SEXP zero_result(void) {
  SEXP r=PROTECT(allocVector(VECSXP,2)), nm=PROTECT(allocVector(STRSXP,2));
  SET_VECTOR_ELT(r,0,ScalarReal(0.0)); SET_VECTOR_ELT(r,1,ScalarInteger(0));
  SET_STRING_ELT(nm,0,mkChar("score")); SET_STRING_ELT(nm,1,mkChar("matches"));
  setAttrib(r,R_NamesSymbol,nm); UNPROTECT(2); return r; }

/* ══════════════════════════════════════════════════════════════════════════
 * DENSE LAPJV  — score[r*m+c] negated, m×m with dummy col at m
 * ══════════════════════════════════════════════════════════════════════════ */
static void lapjv_dense(int m, const double *sc, int *row2col) {
  int N=m+1;
  size_t db=(size_t)N*sizeof(double), ib=(size_t)N*sizeof(int);
  char *blk=(char*)xmalloc(3*db+3*ib);
  double *u=(double*)blk, *v=(double*)(blk+db), *d=(double*)(blk+2*db);
  int *p=(int*)(blk+3*db), *prev=(int*)(blk+3*db+ib), *done=(int*)(blk+3*db+2*ib);
  memset(blk,0,3*db+3*ib);
  for(int j=0;j<N;j++) p[j]=-1;
  for(int i=0;i<m;i++) row2col[i]=-1;
  for(int i=0;i<m;i++){
    for(int j=0;j<N;j++){d[j]=DBL_MAX;prev[j]=-1;done[j]=0;}
    const double ui=u[i]; const double *ri=sc+(size_t)i*m;
    for(int j=0;j<m;j++){double r=ri[j]-ui-v[j];if(r<d[j])d[j]=r;}
    {double r=-ui-v[m];if(r<d[m])d[m]=r;}
    int j_aug=-1;
    for(int it=0;it<N;it++){
      double dmin=DBL_MAX; int jmin=-1;
      for(int j=0;j<N;j++) if(!done[j]&&d[j]<dmin){dmin=d[j];jmin=j;}
      if(jmin<0) break; done[jmin]=1;
      if(p[jmin]==-1){j_aug=jmin;break;}
      int i2=p[jmin]; const double *ri2=sc+(size_t)i2*m;
      double base=dmin-u[i2]+v[jmin];
      for(int j2=0;j2<m;j2++){if(done[j2])continue;
      double r=base+ri2[j2]-v[j2]; if(r<d[j2]){d[j2]=r;prev[j2]=jmin;}}
      if(!done[m]){double r=base-v[m];if(r<d[m]){d[m]=r;prev[m]=jmin;}}
    }
    if(j_aug<0) error("LAPJV dense: infeasible");
    double da=d[j_aug]; u[i]+=da;
    for(int j=0;j<N;j++){if(!done[j])continue;
    if(p[j]!=-1)u[p[j]]+=d[j]-da; v[j]-=d[j]-da;}
    for(int jc=j_aug;jc!=-1;){int jp=prev[jc],ic=(jp==-1)?i:p[jp];p[jc]=ic;row2col[ic]=jc;jc=jp;}
  }
  free(blk);
}

/* ══════════════════════════════════════════════════════════════════════════
 * BINARY HEAP
 * ══════════════════════════════════════════════════════════════════════════ */
typedef struct { double key; int val; } HN;
static inline void h_push(HN *h,int *sz,double k,int v){
  int i=(*sz)++; h[i]=(HN){k,v};
  while(i>0){int p=(i-1)>>1;if(h[p].key<=h[i].key)break;HN t=h[p];h[p]=h[i];h[i]=t;i=p;}}
static inline HN h_pop(HN *h,int *sz){
  HN top=h[0]; h[0]=h[--(*sz)]; int i=0;
  for(;;){int l=2*i+1,r=2*i+2,s=i;
    if(l<*sz&&h[l].key<h[s].key)s=l; if(r<*sz&&h[r].key<h[s].key)s=r;
    if(s==i)break; HN t=h[s];h[s]=h[i];h[i]=t;i=s;} return top;}

/* ══════════════════════════════════════════════════════════════════════════
 * SPARSE LAPJV — CSR over unique arcs, dummy col at m
 * ══════════════════════════════════════════════════════════════════════════ */
static void lapjv_sparse(int m, const double *cc, const int *kk,
                         const int *first, int arc_count, int *row2col) {
  int N=m+1;
  size_t db=(size_t)N*sizeof(double), ib=(size_t)N*sizeof(int);
  size_t hb=(size_t)(arc_count+m+2)*sizeof(HN);
  char *blk=(char*)xmalloc(3*db+4*ib+hb);
  double *u=(double*)blk,*v=(double*)(blk+db),*dist=(double*)(blk+2*db);
  int *p=(int*)(blk+3*db),*prev=(int*)(blk+3*db+ib);
  int *vg=(int*)(blk+3*db+2*ib),*stk=(int*)(blk+3*db+3*ib);
  HN  *heap=(HN*)(blk+3*db+4*ib);
  memset(blk,0,3*db+4*ib);
  for(int j=0;j<N;j++) p[j]=-1;
  for(int i=0;i<m;i++) row2col[i]=-1;
  int cg=0;
  for(int i=0;i<m;i++){
    cg++; int hsz=0,nstk=0; const double ui=u[i];
    for(int e=first[i];e<first[i+1];e++){
      int jj=kk[e]; double red=cc[e]-ui-v[jj];
      if(vg[jj]!=cg){vg[jj]=cg;dist[jj]=red;prev[jj]=-1;h_push(heap,&hsz,red,jj);}
      else if(red<dist[jj]){dist[jj]=red;prev[jj]=-1;h_push(heap,&hsz,red,jj);}
    }
    {double red=-ui-v[m];
      if(vg[m]!=cg){vg[m]=cg;dist[m]=red;prev[m]=-1;h_push(heap,&hsz,red,m);}
      else if(red<dist[m]){dist[m]=red;prev[m]=-1;h_push(heap,&hsz,red,m);}}
    int j_aug=-1;
    while(hsz>0){
      HN hn=h_pop(heap,&hsz); int jj=hn.val;
      if(hn.key>dist[jj]) continue;
      vg[jj]=-cg; stk[nstk++]=jj;
      if(p[jj]==-1){j_aug=jj;break;}
      int i2=p[jj]; double base=dist[jj]-u[i2]+v[jj];
      for(int e=first[i2];e<first[i2+1];e++){
        int jj2=kk[e]; if(vg[jj2]==-cg) continue;
        double red=base+cc[e]-v[jj2];
        if(vg[jj2]!=cg){vg[jj2]=cg;dist[jj2]=red;prev[jj2]=jj;h_push(heap,&hsz,red,jj2);}
        else if(red<dist[jj2]){dist[jj2]=red;prev[jj2]=jj;h_push(heap,&hsz,red,jj2);}
      }
      if(vg[m]!=-cg){double red=base-v[m];
        if(vg[m]!=cg){vg[m]=cg;dist[m]=red;prev[m]=jj;h_push(heap,&hsz,red,m);}
        else if(red<dist[m]){dist[m]=red;prev[m]=jj;h_push(heap,&hsz,red,m);}}
    }
    if(j_aug<0) error("LAPJV sparse: infeasible");
    double da=dist[j_aug]; u[i]+=da;
    for(int k=0;k<nstk;k++){int j=stk[k];if(p[j]!=-1)u[p[j]]+=dist[j]-da;v[j]-=dist[j]-da;}
    for(int jc=j_aug;jc!=-1;){int jp=prev[jc],ic=(jp==-1)?i:p[jp];p[jc]=ic;row2col[ic]=jc;jc=jp;}
  }
  free(blk);
}

/* ══════════════════════════════════════════════════════════════════════════
 * FUSED: join_gnps + gnps in one call
 *
 * x_mz, x_in : raw x spectrum columns (length nx)
 * y_mz, y_in : raw y spectrum columns (length ny)
 * x_pre, y_pre: precursor m/z (may be NA)
 * tol, ppm   : matching tolerances
 * ══════════════════════════════════════════════════════════════════════════ */
static SEXP gnps_fused(const double *x_mz, const double *x_in, int nx,
                       const double *y_mz, const double *y_in, int ny,
                       double x_pre, double y_pre,
                       double tol, double ppm_val)
{
  /* ── 1. Sort non-NA peaks ──────────────────────────────────────────── */
  MI *xs = (MI*)xmalloc((size_t)nx*sizeof(MI));
  MI *ys = (MI*)xmalloc((size_t)ny*sizeof(MI));
  int vx=0, vy=0;
  for(int i=0;i<nx;i++) if(!ISNA(x_mz[i])){xs[vx].mass=x_mz[i];xs[vx].idx=i;vx++;}
  for(int i=0;i<ny;i++) if(!ISNA(y_mz[i])){ys[vy].mass=y_mz[i];ys[vy].idx=i;vy++;}
  qsort(xs,(size_t)vx,sizeof(MI),mi_cmp);
  qsort(ys,(size_t)vy,sizeof(MI),mi_cmp);
  
  /* ── 2. unique-intensity sums (walk sorted arrays, sum first occ) ── */
  double xs_sum=0.0, ys_sum=0.0;
  for(int i=0;i<vx;i++) if(i==0||xs[i].mass!=xs[i-1].mass) xs_sum+=x_in[xs[i].idx];
  for(int i=0;i<vy;i++) if(i==0||ys[i].mass!=ys[i-1].mass) ys_sum+=y_in[ys[i].idx];
  
  if(xs_sum==0.0||ys_sum==0.0){free(xs);free(ys);return zero_result();}
  
  const double sxs=sqrt(xs_sum), sys=sqrt(ys_sum);
  const int do_pdiff = (!ISNA(x_pre)&&!ISNA(y_pre));
  const double pdiff = do_pdiff ? y_pre-x_pre : 0.0;
  
  /* ── 3. Match peaks → collect (xf,yf,score) arcs ─────────────────── */
  /* Upper bound: each x can match all y in direct + all y in shifted window.
   * Allocate conservatively: vx*vy arcs max (rare). */
  int arc_cap = vx + vy + 2;  /* minimum; will realloc if needed */
  Arc *arcs = (Arc*)xmalloc((size_t)arc_cap*sizeof(Arc));
  int n_arcs = 0;
  
  /* We need factor indices for x and y mz.
   * Since xs/ys are sorted, rank = position of first occurrence. */
  /* Build x-factor map: sorted unique x mz → rank */
  int *xf_sorted = (int*)xmalloc((size_t)vx*sizeof(int));  /* factor for sorted xs[i] */
  { int rank=0;
    for(int i=0;i<vx;i++){
      if(i==0||xs[i].mass!=xs[i-1].mass) rank++;
      xf_sorted[i]=rank;
    }
  }
  int *yf_sorted = (int*)xmalloc((size_t)vy*sizeof(int));
  { int rank=0;
    for(int i=0;i<vy;i++){
      if(i==0||ys[i].mass!=ys[i-1].mass) rank++;
      yf_sorted[i]=rank;
    }
  }
  
  /* Helper: ensure arc array capacity */
#define ARC_PUSH(xrow, ycol, score_val) do {                 \
  if (n_arcs >= arc_cap) {                                   \
    arc_cap *= 2;                                            \
    arcs = (Arc*)realloc(arcs, (size_t)arc_cap*sizeof(Arc)); \
    if (!arcs) error("realloc arcs");                        \
  }                                                          \
  arcs[n_arcs].row = (xrow);                                 \
  arcs[n_arcs].col = (ycol);                                 \
  arcs[n_arcs].sc  = (score_val);                            \
  n_arcs++;                                                  \
} while(0)

/* Direct matches */
for(int i=0;i<vx;i++){
  const double xm=xs[i].mass;
  const double half=tol+ppm_val*xm*1e-6;
  const int start=lower_bound(ys,vy,xm-half);
  for(int j=start;j<vy&&ys[j].mass<=xm+half;j++){
    double sc=(sqrt(x_in[xs[i].idx])/sxs)*(sqrt(y_in[ys[j].idx])/sys);
    ARC_PUSH(xf_sorted[i]-1, yf_sorted[j]-1, sc);
  }
}
/* Precursor-shifted matches */
if(do_pdiff){
  for(int i=0;i<vx;i++){
    const double xadj=xs[i].mass+pdiff;
    const double half=tol+ppm_val*xadj*1e-6;
    const int start=lower_bound(ys,vy,xadj-half);
    for(int j=start;j<vy&&ys[j].mass<=xadj+half;j++){
      double sc=(sqrt(x_in[xs[i].idx])/sxs)*(sqrt(y_in[ys[j].idx])/sys);
      ARC_PUSH(xf_sorted[i]-1, yf_sorted[j]-1, sc);
    }
  }
}

free(xf_sorted); free(yf_sorted); free(xs); free(ys);

if(n_arcs==0){ free(arcs); return zero_result(); }

/* ── 4. Dedup: sort by (row,col), keep last (= max score) ─────────── */
/* For equal (row,col), keep highest score (best pair, per GNPS spec) */
qsort(arcs,(size_t)n_arcs,sizeof(Arc),arc_cmp);
int u_count=0;
for(int i=0;i<n_arcs;i++){
  if(i+1<n_arcs&&arcs[i].row==arcs[i+1].row&&arcs[i].col==arcs[i+1].col){
    /* keep higher score */
    if(arcs[i].sc>arcs[i+1].sc) arcs[i+1].sc=arcs[i].sc;
    continue;
  }
  arcs[u_count++]=arcs[i];
}

/* ── 5. Compact rows and cols to contiguous indices ──────────────────
 * arcs[].row and arcs[].col are factor ranks (0-based) from the full
 * sorted unique mz lists. After dedup, only a subset of those ranks
 * may appear. We remap to contiguous 0..nr-1 and 0..nc-1 so the
 * LAPJV matrix has no empty rows/cols that would make it infeasible.
 *
 * After compaction: m = max(nr, nc).
 * ──────────────────────────────────────────────────────────────────── */
/* find max raw indices */
int raw_nr=0, raw_nc=0;
for(int i=0;i<u_count;i++){
  if(arcs[i].row>=raw_nr) raw_nr=arcs[i].row+1;
  if(arcs[i].col>=raw_nc) raw_nc=arcs[i].col+1;
}
/* remap rows: mark which raw row indices appear, assign compact index */
int *row_map=(int*)xmalloc((size_t)raw_nr*sizeof(int));
int *col_map=(int*)xmalloc((size_t)raw_nc*sizeof(int));
memset(row_map,-1,(size_t)raw_nr*sizeof(int));
memset(col_map,-1,(size_t)raw_nc*sizeof(int));
for(int i=0;i<u_count;i++){
  row_map[arcs[i].row]=0; /* mark present */
col_map[arcs[i].col]=0;
}
int nr=0,nc=0;
for(int i=0;i<raw_nr;i++) if(row_map[i]==0) row_map[i]=nr++;
for(int i=0;i<raw_nc;i++) if(col_map[i]==0) col_map[i]=nc++;
/* apply remapping */
for(int i=0;i<u_count;i++){
  arcs[i].row=row_map[arcs[i].row];
  arcs[i].col=col_map[arcs[i].col];
}
free(row_map); free(col_map);
int m=(nr>nc)?nr:nc;

/* ── 6. LAPJV ─────────────────────────────────────────────────────── */
double total=0.0; int matched=0;
int *row2col=(int*)xmalloc((size_t)(m+1)*sizeof(int));

if(m<=DENSE_THRESHOLD){
  double *smat=(double*)xcalloc((size_t)m*m,sizeof(double));
  for(int i=0;i<u_count;i++)
    smat[(size_t)arcs[i].row*m+arcs[i].col]=-arcs[i].sc;
  lapjv_dense(m,smat,row2col);
  for(int r=0;r<m;r++){
    int c=row2col[r];
    if(c>=0&&c<m){double sv=-smat[(size_t)r*m+c];if(sv>0){total+=sv;matched++;}}
  }
  free(smat);
} else {
  /* CSR — arcs already sorted by (row,col) */
  int *rowcnt=(int*)xcalloc((size_t)m,sizeof(int));
  for(int i=0;i<u_count;i++) rowcnt[arcs[i].row]++;
  int *first=(int*)xmalloc((size_t)(m+1)*sizeof(int));
  first[0]=0; for(int r=0;r<m;r++) first[r+1]=first[r]+rowcnt[r];
  free(rowcnt);
  
  double *cc=(double*)xmalloc((size_t)u_count*sizeof(double));
  int    *kk=(int*)   xmalloc((size_t)u_count*sizeof(int));
  double *csc=(double*)xmalloc((size_t)u_count*sizeof(double));
  int    *cur=(int*)  xcalloc((size_t)m,sizeof(int));
  memcpy(cur,first,(size_t)m*sizeof(int));
  for(int i=0;i<u_count;i++){
    int pos=cur[arcs[i].row]++;
    cc[pos]=-arcs[i].sc; kk[pos]=arcs[i].col; csc[pos]=arcs[i].sc;
  }
  free(cur);
  /* columns already sorted (arcs sorted by row then col) */
  
  lapjv_sparse(m,cc,kk,first,u_count,row2col);
  
  for(int r=0;r<m;r++){
    int c=row2col[r]; if(c<0||c>=m) continue;
    int lo=first[r],hi=first[r+1]-1;
    while(lo<=hi){int mid=(lo+hi)>>1;
      if(kk[mid]==c){if(csc[mid]>0){total+=csc[mid];matched++;}break;}
      if(kk[mid]<c) lo=mid+1; else hi=mid-1;}
  }
  free(first);free(cc);free(kk);free(csc);
}

free(arcs); free(row2col);

SEXP r=PROTECT(allocVector(VECSXP,2)),nm=PROTECT(allocVector(STRSXP,2));
SET_VECTOR_ELT(r,0,ScalarReal(total)); SET_VECTOR_ELT(r,1,ScalarInteger(matched));
SET_STRING_ELT(nm,0,mkChar("score")); SET_STRING_ELT(nm,1,mkChar("matches"));
setAttrib(r,R_NamesSymbol,nm); UNPROTECT(2); return r;
}

/* ══════════════════════════════════════════════════════════════════════════
 * R entry: gnps_compute(x_mat, y_mat, xPrecursorMz, yPrecursorMz, tol, ppm)
 * x_mat / y_mat: full peak matrices (mz | intensity), NOT pre-aligned.
 * This is the fast path — no R-level join needed.
 * ══════════════════════════════════════════════════════════════════════════ */
SEXP gnps_compute(SEXP x, SEXP y,
                  SEXP xPrecursorMz, SEXP yPrecursorMz,
                  SEXP tolerance, SEXP ppm)
{
  if(!isReal(x)||!isReal(y)) error("x and y must be numeric matrices");
  SEXP xd=getAttrib(x,R_DimSymbol), yd=getAttrib(y,R_DimSymbol);
  if(!isInteger(xd)||!isInteger(yd)) error("dim must be integer");
  int nx=INTEGER(xd)[0], ny=INTEGER(yd)[0];
  const double *xmz=REAL(x), *xin=xmz+nx;
  const double *ymz=REAL(y), *yin=ymz+ny;
  return gnps_fused(xmz,xin,nx, ymz,yin,ny,
                    asReal(xPrecursorMz), asReal(yPrecursorMz),
                    asReal(tolerance), asReal(ppm));
}

/* ══════════════════════════════════════════════════════════════════════════
 * R entry: gnps(x_aligned, y_aligned)  — backward compatible
 * x/y are pre-aligned matrices from join_gnps (may have NA rows).
 * ══════════════════════════════════════════════════════════════════════════ */
SEXP gnps(SEXP x, SEXP y)
{
  if(!isReal(x)||!isReal(y)) error("Inputs must be numeric matrices");
  SEXP xd=getAttrib(x,R_DimSymbol), yd=getAttrib(y,R_DimSymbol);
  if(!isInteger(xd)||!isInteger(yd)) error("dim must be integer");
  int n=INTEGER(xd)[0];
  if(n!=INTEGER(yd)[0]) error("row count mismatch");
  const double *xmz=REAL(x), *xin=xmz+n;
  const double *ymz=REAL(y), *yin=ymz+n;
  
  /* unique-intensity sums — walk in original order, first occ of each mz */
  KP *buf=(KP*)xmalloc((size_t)n*sizeof(KP));
  int m=0;
  for(int i=0;i<n;i++) if(!ISNA(xmz[i])) {buf[m].key=xmz[i];buf[m].pos=i;m++;}
  qsort(buf,(size_t)m,sizeof(KP),kp_cmp);
  double xs=0.0; for(int i=0;i<m;i++) if(i==0||buf[i].key!=buf[i-1].key) xs+=xin[buf[i].pos];
  
  m=0;
  for(int i=0;i<n;i++) if(!ISNA(ymz[i])) {buf[m].key=ymz[i];buf[m].pos=i;m++;}
  qsort(buf,(size_t)m,sizeof(KP),kp_cmp);
  double ys=0.0; for(int i=0;i<m;i++) if(i==0||buf[i].key!=buf[i-1].key) ys+=yin[buf[i].pos];
  free(buf);
  
  if(xs==0.0||ys==0.0) return zero_result();
  
  /* build keep[], x_fac[], y_fac[] */
  int *x_fac=(int*)xmalloc((size_t)n*sizeof(int));
  int *y_fac=(int*)xmalloc((size_t)n*sizeof(int));
  KP  *kpb  =(KP*) xmalloc((size_t)n*sizeof(KP));
  
  int l=0;
  for(int i=0;i<n;i++) if(!ISNA(xmz[i])&&!ISNA(ymz[i])) l++;
  if(!l){free(x_fac);free(y_fac);free(kpb);return zero_result();}
  
  /* x factors */
  { int k=0;
    for(int i=0;i<n;i++) if(!ISNA(xmz[i])&&!ISNA(ymz[i])){kpb[k].key=xmz[i];kpb[k].pos=k;k++;}
    qsort(kpb,(size_t)l,sizeof(KP),kp_cmp);
    int rank=0;
    for(int i=0;i<l;i++){if(i==0||kpb[i].key!=kpb[i-1].key)rank++; x_fac[kpb[i].pos]=rank;}
  }
  /* y factors */
  { int k=0;
    for(int i=0;i<n;i++) if(!ISNA(xmz[i])&&!ISNA(ymz[i])){kpb[k].key=ymz[i];kpb[k].pos=k;k++;}
    qsort(kpb,(size_t)l,sizeof(KP),kp_cmp);
    int rank=0;
    for(int i=0;i<l;i++){if(i==0||kpb[i].key!=kpb[i-1].key)rank++; y_fac[kpb[i].pos]=rank;}
  }
  free(kpb);
  
  const double sxs=sqrt(xs), sys=sqrt(ys);
  
  /* build arc list (l entries, one per kept row) */
  Arc *arcs=(Arc*)xmalloc((size_t)l*sizeof(Arc));
  { int k=0;
    for(int i=0;i<n;i++){
      if(!ISNA(xmz[i])&&!ISNA(ymz[i])){
        arcs[k].row=x_fac[k]-1; arcs[k].col=y_fac[k]-1;
        arcs[k].sc=(sqrt(xin[i])/sxs)*(sqrt(yin[i])/sys);
        k++;
      }
    }
  }
  free(x_fac); free(y_fac);
  
  /* dedup: sort by (row,col), keep max score per cell */
  qsort(arcs,(size_t)l,sizeof(Arc),arc_cmp);
  int u_count=0;
  for(int i=0;i<l;i++){
    if(i+1<l&&arcs[i].row==arcs[i+1].row&&arcs[i].col==arcs[i+1].col){
      if(arcs[i].sc>arcs[i+1].sc) arcs[i+1].sc=arcs[i].sc;
      continue;
    }
    arcs[u_count++]=arcs[i];
  }
  
  /* compact: remap to contiguous row/col indices (only those present in arcs) */
  int raw_nr2=0, raw_nc2=0;
  for(int i=0;i<u_count;i++){
    if(arcs[i].row>=raw_nr2) raw_nr2=arcs[i].row+1;
    if(arcs[i].col>=raw_nc2) raw_nc2=arcs[i].col+1;
  }
  int *rmap=(int*)xmalloc((size_t)raw_nr2*sizeof(int));
  int *cmap=(int*)xmalloc((size_t)raw_nc2*sizeof(int));
  memset(rmap,-1,(size_t)raw_nr2*sizeof(int));
  memset(cmap,-1,(size_t)raw_nc2*sizeof(int));
  for(int i=0;i<u_count;i++){rmap[arcs[i].row]=0;cmap[arcs[i].col]=0;}
  int nr2=0,nc2=0;
  for(int i=0;i<raw_nr2;i++) if(rmap[i]==0) rmap[i]=nr2++;
  for(int i=0;i<raw_nc2;i++) if(cmap[i]==0) cmap[i]=nc2++;
  for(int i=0;i<u_count;i++){arcs[i].row=rmap[arcs[i].row];arcs[i].col=cmap[arcs[i].col];}
  free(rmap); free(cmap);
  int msize=(nr2>nc2)?nr2:nc2;
  
  double total=0.0; int matched=0;
  int *row2col=(int*)xmalloc((size_t)(msize+1)*sizeof(int));
  
  if(msize<=DENSE_THRESHOLD){
    double *smat=(double*)xcalloc((size_t)msize*msize,sizeof(double));
    for(int i=0;i<u_count;i++)
      smat[(size_t)arcs[i].row*msize+arcs[i].col]=-arcs[i].sc;
    lapjv_dense(msize,smat,row2col);
    for(int r=0;r<msize;r++){
      int c=row2col[r];
      if(c>=0&&c<msize){double sv=-smat[(size_t)r*msize+c];if(sv>0){total+=sv;matched++;}}
    }
    free(smat);
  } else {
    int *rowcnt=(int*)xcalloc((size_t)msize,sizeof(int));
    for(int i=0;i<u_count;i++) rowcnt[arcs[i].row]++;
    int *first=(int*)xmalloc((size_t)(msize+1)*sizeof(int));
    first[0]=0; for(int r=0;r<msize;r++) first[r+1]=first[r]+rowcnt[r];
    free(rowcnt);
    double *cc=(double*)xmalloc((size_t)u_count*sizeof(double));
    int    *kk=(int*)   xmalloc((size_t)u_count*sizeof(int));
    double *csc=(double*)xmalloc((size_t)u_count*sizeof(double));
    int    *cur=(int*)  xcalloc((size_t)msize,sizeof(int));
    memcpy(cur,first,(size_t)msize*sizeof(int));
    for(int i=0;i<u_count;i++){
      int pos=cur[arcs[i].row]++;
      cc[pos]=-arcs[i].sc; kk[pos]=arcs[i].col; csc[pos]=arcs[i].sc;
    }
    free(cur);
    lapjv_sparse(msize,cc,kk,first,u_count,row2col);
    for(int r=0;r<msize;r++){
      int c=row2col[r]; if(c<0||c>=msize) continue;
      int lo=first[r],hi=first[r+1]-1;
      while(lo<=hi){int mid=(lo+hi)>>1;
        if(kk[mid]==c){if(csc[mid]>0){total+=csc[mid];matched++;}break;}
        if(kk[mid]<c)lo=mid+1;else hi=mid-1;}
    }
    free(first);free(cc);free(kk);free(csc);
  }
  free(arcs); free(row2col);
  
  SEXP r=PROTECT(allocVector(VECSXP,2)),nm=PROTECT(allocVector(STRSXP,2));
  SET_VECTOR_ELT(r,0,ScalarReal(total)); SET_VECTOR_ELT(r,1,ScalarInteger(matched));
  SET_STRING_ELT(nm,0,mkChar("score")); SET_STRING_ELT(nm,1,mkChar("matches"));
  setAttrib(r,R_NamesSymbol,nm); UNPROTECT(2); return r;
}

/* ══════════════════════════════════════════════════════════════════════════
 * join_gnps — standalone peak matching (backward compat)
 * ══════════════════════════════════════════════════════════════════════════ */
#define BITSET_SET(bs,i)  ((bs)[(i)>>3]|=(unsigned char)(1u<<((i)&7u)))
#define BITSET_TEST(bs,i) ((bs)[(i)>>3]& (unsigned char)(1u<<((i)&7u)))

SEXP join_gnps(SEXP x, SEXP y,
               SEXP xPrecursorMz, SEXP yPrecursorMz,
               SEXP tolerance, SEXP ppm)
{
  if(!isReal(x)||!isReal(y)) error("x and y must be numeric vectors");
  const double *xp=REAL(x),*yp=REAL(y);
  const double x_pre=asReal(xPrecursorMz), y_pre=asReal(yPrecursorMz);
  const double tol=asReal(tolerance), ppm_val=asReal(ppm);
  const int nx=(int)xlength(x), ny=(int)xlength(y);
  const double pdiff=y_pre-x_pre;
  const int do_pdiff=(!ISNA(x_pre)&&!ISNA(y_pre));
  
  MI *xs=(MI*)xmalloc((size_t)nx*sizeof(MI));
  MI *ys=(MI*)xmalloc((size_t)ny*sizeof(MI));
  int vx=0,vy=0;
  for(int i=0;i<nx;i++) if(!ISNA(xp[i])){xs[vx].mass=xp[i];xs[vx].idx=i;vx++;}
  for(int i=0;i<ny;i++) if(!ISNA(yp[i])){ys[vy].mass=yp[i];ys[vy].idx=i;vy++;}
  qsort(xs,(size_t)vx,sizeof(MI),mi_cmp);
  qsort(ys,(size_t)vy,sizeof(MI),mi_cmp);
  
  const size_t bsz=((size_t)ny+7u)>>3;
  unsigned char *y_used=(unsigned char*)xcalloc(bsz,1);
  
  /* count pass */
  int cnt=0;
  /* NA x rows */
  for(int i=0;i<nx;i++) if(ISNA(xp[i])) cnt++;
  /* direct matches */
  for(int i=0;i<vx;i++){
    const double xm=xs[i].mass, half=tol+ppm_val*xm*1e-6;
    int start=lower_bound(ys,vy,xm-half), found=0;
    for(int j=start;j<vy&&ys[j].mass<=xm+half;j++){BS_SET(y_used,ys[j].idx);found++;cnt++;}
    if(!found) cnt++;
  }
  /* shifted matches */
  if(do_pdiff){
    for(int i=0;i<vx;i++){
      const double xa=xs[i].mass+pdiff, half=tol+ppm_val*xa*1e-6;
      for(int j=lower_bound(ys,vy,xa-half);j<vy&&ys[j].mass<=xa+half;j++) cnt++;
    }
  }
  /* unmatched y */
  for(int i=0;i<ny;i++) if(!ISNA(yp[i])&&!BS_TEST(y_used,i)) cnt++;
  
  memset(y_used,0,bsz);
  
  SEXP rx=PROTECT(allocVector(INTSXP,cnt));
  SEXP ry=PROTECT(allocVector(INTSXP,cnt));
  int *ox=INTEGER(rx), *oy=INTEGER(ry), pos=0;
  
  /* fill pass — same order */
  for(int i=0;i<nx;i++) if(ISNA(xp[i])){ox[pos]=i+1;oy[pos]=NA_INTEGER;pos++;}
  for(int i=0;i<vx;i++){
    const double xm=xs[i].mass, half=tol+ppm_val*xm*1e-6;
    int start=lower_bound(ys,vy,xm-half), found=0;
    for(int j=start;j<vy&&ys[j].mass<=xm+half;j++){
      BS_SET(y_used,ys[j].idx);
      ox[pos]=xs[i].idx+1; oy[pos]=ys[j].idx+1; pos++; found++;
    }
    if(!found){ox[pos]=xs[i].idx+1;oy[pos]=NA_INTEGER;pos++;}
  }
  if(do_pdiff){
    for(int i=0;i<vx;i++){
      const double xa=xs[i].mass+pdiff, half=tol+ppm_val*xa*1e-6;
      for(int j=lower_bound(ys,vy,xa-half);j<vy&&ys[j].mass<=xa+half;j++){
        ox[pos]=xs[i].idx+1; oy[pos]=ys[j].idx+1; pos++;
      }
    }
  }
  for(int i=0;i<ny;i++) if(!ISNA(yp[i])&&!BS_TEST(y_used,i)){ox[pos]=NA_INTEGER;oy[pos]=i+1;pos++;}
  
  free(xs);free(ys);free(y_used);
  
  SEXP result=PROTECT(allocVector(VECSXP,2));
  SET_VECTOR_ELT(result,0,rx); SET_VECTOR_ELT(result,1,ry);
  SEXP names=PROTECT(allocVector(STRSXP,2));
  SET_STRING_ELT(names,0,mkChar("x")); SET_STRING_ELT(names,1,mkChar("y"));
  setAttrib(result,R_NamesSymbol,names);
  UNPROTECT(4); return result;
}
