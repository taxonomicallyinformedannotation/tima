#include "tima.h"

/* Registration table */
static const R_CallMethodDef callMethods[] = {
  {"C_join_gnps",    (DL_FUNC) &C_join_gnps,    6},
  {"C_gnps",         (DL_FUNC) &C_gnps,         2},
  {"C_gnps_chain_dp", (DL_FUNC) &C_gnps_chain_dp, 6},
  {NULL, NULL, 0}
};

/* Package initialization */
void R_init_tima(DllInfo *dll) {
  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
