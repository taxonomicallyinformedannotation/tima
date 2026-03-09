#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

/* Function declarations */
extern SEXP join_gnps(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP gnps(SEXP, SEXP);
extern SEXP gnps_chain_dp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

/* Registration table */
static const R_CallMethodDef callMethods[] = {
  {"join_gnps",    (DL_FUNC) &join_gnps,    6},
  {"gnps",         (DL_FUNC) &gnps,         2},
  {"gnps_chain_dp", (DL_FUNC) &gnps_chain_dp, 6},
  {NULL, NULL, 0}
};

/* Package initialization */
void R_init_tima(DllInfo *dll) {
  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
