#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP join_gnps(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP gnps(SEXP, SEXP);

static const R_CallMethodDef callMethods[] = {
    {"join_gnps", (DL_FUNC) &join_gnps, 6},
    {"gnps",      (DL_FUNC) &gnps,      2},
    {NULL, NULL, 0}
};

void R_init_tima(DllInfo *dll) {
    R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
