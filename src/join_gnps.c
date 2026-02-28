#include <R.h>
#include <Rinternals.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* --------------------------------------------------------------------------
 Bitset helpers – 1 bit per y index to track matched y entries.
 We use unsigned char arrays (8 bits each).
 -------------------------------------------------------------------------- */
#define BITSET_SET(bs, i)   ((bs)[(i) >> 3] |=  (unsigned char)(1u << ((i) & 7u)))
#define BITSET_TEST(bs, i)  ((bs)[(i) >> 3] &   (unsigned char)(1u << ((i) & 7u)))

/* --------------------------------------------------------------------------
 MassIndex: pairs a sorted mass with its original 0-based index.
 -------------------------------------------------------------------------- */
typedef struct {
  double    mass;
  R_xlen_t  index;
} MassIndex;

static int compare_mass(const void *a, const void *b) {
  const double ma = ((const MassIndex *)a)->mass;
  const double mb = ((const MassIndex *)b)->mass;
  return (ma > mb) - (ma < mb);
}

/* First index in sorted arr[0..n) with mass >= lower_bound */
static R_xlen_t lower_bound_idx(const MassIndex *arr, R_xlen_t n, double lb) {
  R_xlen_t lo = 0, hi = n;
  while (lo < hi) {
    R_xlen_t mid = lo + ((hi - lo) >> 1);
    if (arr[mid].mass < lb) lo = mid + 1;
    else                    hi = mid;
  }
  return lo;
}

/* --------------------------------------------------------------------------
 Core matching kernel – called twice: once to COUNT, once to FILL.
 
 When out_x / out_y are NULL the function just returns the match count.
 When they are non-NULL it writes 1-based indices into them starting at
 *pos, then advances *pos.
 
 Matches produced (in order):
 1. NA rows from x  → (i+1, NA)
 2. Direct m/z matches for each valid x:
 found  → (xi+1, yj+1) for every y in window
 none   → (xi+1, NA)
 y entries that are matched here are flagged in y_used.
 3. Precursor-shifted matches (if both precursors are non-NA):
 (xi+1, yj+1) for every y in shifted window
 (intentionally may overlap with pass 2, per GNPS spec)
 4. Unmatched y entries → (NA, yj+1)
 -------------------------------------------------------------------------- */
static R_xlen_t matching_kernel(
    const double  *x_ptr,   R_xlen_t x_size,
    const double  *y_ptr,   R_xlen_t y_size,
    const MassIndex *xs,    R_xlen_t valid_x,
    const MassIndex *ys,    R_xlen_t valid_y,
    double tol, double ppm_val, double pdiff,
    int do_pdiff,
    unsigned char *y_used,  /* bitset, length ceil(y_size/8), pre-zeroed    */
int *out_x, int *out_y, R_xlen_t *pos)
{
  const int filling = (out_x != NULL);
  R_xlen_t count = 0;
  
#define EMIT(xi, yi)                        \
  do {                                      \
    if (filling) {                          \
      out_x[*pos] = (xi);                   \
      out_y[*pos] = (yi);                   \
      (*pos)++;                             \
    } else {                                \
      count++;                              \
    }                                       \
  } while (0)

/* --- Pass 1: NA rows from x --- */
for (R_xlen_t i = 0; i < x_size; i++) {
  if (ISNAN(x_ptr[i])) {
    EMIT((int)(i + 1), NA_INTEGER);
  }
}

/* --- Pass 2: direct m/z matching --- */
for (R_xlen_t i = 0; i < valid_x; i++) {
  const double xm   = xs[i].mass;
  const double half = tol + ppm_val * xm * 1e-6;
  const double lb   = xm - half;
  const double ub   = xm + half;
  
  const R_xlen_t start = lower_bound_idx(ys, valid_y, lb);
  int found = 0;
  for (R_xlen_t j = start; j < valid_y && ys[j].mass <= ub; j++) {
    EMIT((int)(xs[i].index + 1), (int)(ys[j].index + 1));
    BITSET_SET(y_used, ys[j].index);
    found = 1;
  }
  if (!found) {
    EMIT((int)(xs[i].index + 1), NA_INTEGER);
  }
}

/* --- Pass 3: precursor-shift matching --- */
if (do_pdiff) {
  for (R_xlen_t i = 0; i < valid_x; i++) {
    const double xadj = xs[i].mass + pdiff;
    const double half = tol + ppm_val * xadj * 1e-6;
    const double lb   = xadj - half;
    const double ub   = xadj + half;
    
    const R_xlen_t start = lower_bound_idx(ys, valid_y, lb);
    for (R_xlen_t j = start; j < valid_y && ys[j].mass <= ub; j++) {
      EMIT((int)(xs[i].index + 1), (int)(ys[j].index + 1));
    }
  }
}

/* --- Pass 4: unmatched y entries --- */
for (R_xlen_t i = 0; i < y_size; i++) {
  if (!ISNAN(y_ptr[i]) && !BITSET_TEST(y_used, i)) {
    EMIT(NA_INTEGER, (int)(i + 1));
  }
}

#undef EMIT
return count;
}

/* --------------------------------------------------------------------------
 R entry point
 -------------------------------------------------------------------------- */
SEXP join_gnps(SEXP x, SEXP y,
               SEXP xPrecursorMz, SEXP yPrecursorMz,
               SEXP tolerance, SEXP ppm)
{
  if (!isReal(x) || !isReal(y))
    error("x and y must be numeric vectors");
  
  const double *x_ptr   = REAL(x);
  const double *y_ptr   = REAL(y);
  const double  x_pre   = asReal(xPrecursorMz);
  const double  y_pre   = asReal(yPrecursorMz);
  const double  tol     = asReal(tolerance);
  const double  ppm_val = asReal(ppm);
  const R_xlen_t x_size = xlength(x);
  const R_xlen_t y_size = xlength(y);
  const double  pdiff   = y_pre - x_pre;
  const int do_pdiff    = (!ISNAN(x_pre) && !ISNAN(y_pre));
  
  /* --- Build sorted MassIndex arrays (skip NaN) ----------------------- */
  MassIndex *xs = (MassIndex *)malloc((size_t)x_size * sizeof(MassIndex));
  MassIndex *ys = (MassIndex *)malloc((size_t)y_size * sizeof(MassIndex));
  if (!xs || !ys) { free(xs); free(ys); error("Memory allocation failed"); }
  
  R_xlen_t valid_x = 0, valid_y = 0;
  for (R_xlen_t i = 0; i < x_size; i++)
    if (!ISNAN(x_ptr[i])) { xs[valid_x].mass = x_ptr[i]; xs[valid_x].index = i; valid_x++; }
    for (R_xlen_t i = 0; i < y_size; i++)
      if (!ISNAN(y_ptr[i])) { ys[valid_y].mass = y_ptr[i]; ys[valid_y].index = i; valid_y++; }
      
      qsort(xs, (size_t)valid_x, sizeof(MassIndex), compare_mass);
      qsort(ys, (size_t)valid_y, sizeof(MassIndex), compare_mass);
      
      /* --- Bitset for matched y entries (8× smaller than char array) ------ */
      const size_t bitset_bytes = ((size_t)y_size + 7u) >> 3;
      unsigned char *y_used = (unsigned char *)calloc(bitset_bytes, 1u);
      if (!y_used) { free(xs); free(ys); error("Memory allocation failed"); }
      
      /* --- Count pass: exact number of matches ---------------------------- */
      R_xlen_t n_matches = matching_kernel(
        x_ptr, x_size, y_ptr, y_size,
        xs, valid_x, ys, valid_y,
        tol, ppm_val, pdiff, do_pdiff,
        y_used, NULL, NULL, NULL);
      
      /* Reset bitset for fill pass */
      memset(y_used, 0, bitset_bytes);
      
      /* --- Allocate exactly-sized result vectors -------------------------- */
      SEXP result_x = PROTECT(allocVector(INTSXP, n_matches));
      SEXP result_y = PROTECT(allocVector(INTSXP, n_matches));
      int *rx = INTEGER(result_x);
      int *ry = INTEGER(result_y);
      
      /* --- Fill pass ------------------------------------------------------ */
      R_xlen_t pos = 0;
      matching_kernel(
        x_ptr, x_size, y_ptr, y_size,
        xs, valid_x, ys, valid_y,
        tol, ppm_val, pdiff, do_pdiff,
        y_used, rx, ry, &pos);
      
      free(xs);
      free(ys);
      free(y_used);
      
      /* --- Pack into named list ------------------------------------------- */
      SEXP result = PROTECT(allocVector(VECSXP, 2));
      SET_VECTOR_ELT(result, 0, result_x);
      SET_VECTOR_ELT(result, 1, result_y);
      
      SEXP names = PROTECT(allocVector(STRSXP, 2));
      SET_STRING_ELT(names, 0, mkChar("x"));
      SET_STRING_ELT(names, 1, mkChar("y"));
      setAttrib(result, R_NamesSymbol, names);
      
      UNPROTECT(4);
      return result;
}