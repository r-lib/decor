#include <R.h>
#include <Rinternals.h>

#include <R_ext/Rdynload.h>

SEXP parse_cpp_function(SEXP signature_);

R_CallMethodDef callMethods[]  = {
  {"decor_parse_cpp_function", (DL_FUNC) &parse_cpp_function, 1},
  {NULL, NULL, 0}
};

void R_init_decor(DllInfo *info) {
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
}
