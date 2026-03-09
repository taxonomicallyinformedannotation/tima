#ifndef TIMA_H
#define TIMA_H

#define R_NO_REMAP /* don't allow to use R API without Rf_ prefix */

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

/* Function declarations */
extern SEXP C_join_gnps(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP C_gnps(SEXP, SEXP);
extern SEXP C_gnps_chain_dp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

#endif /* end of TIMA_H */
