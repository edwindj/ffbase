#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/*
  The following name(s) appear with different usages
  e.g., with different numbetables and any declarations.
*/

/* .C calls */
extern void grouprunningcumsum(void *, void *, void *);
extern void grouprunningcumsumindex(void *, void *, void *, void *);

/* .Call calls */
extern SEXP binned_sum(SEXP, SEXP, SEXP, SEXP);
extern SEXP binned_sumsq(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP binned_tabulate(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP bySum(SEXP, SEXP, SEXP, SEXP);

static const R_CMethodDef CEntries[] = {
    {"grouprunningcumsum",      (DL_FUNC) &grouprunningcumsum,      3},
    {"grouprunningcumsumindex", (DL_FUNC) &grouprunningcumsumindex, 4},
    {NULL, NULL, 0}
};

static const R_CallMethodDef CallEntries[] = {
    {"binned_sum",      (DL_FUNC) &binned_sum,      4},
    {"binned_sumsq",    (DL_FUNC) &binned_sumsq,    5},
    {"binned_tabulate", (DL_FUNC) &binned_tabulate, 5},
    {"bySum",           (DL_FUNC) &bySum,           4},
    {NULL, NULL, 0}
};

void R_init_ffbase(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
