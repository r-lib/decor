#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>

SEXP parse_cpp_function(SEXP signature_);
SEXP r_blank_comments(SEXP filename_);

R_CallMethodDef callMethods[] = {
    {"decor_parse_cpp_function", (DL_FUNC)&parse_cpp_function, 1},
    {"blank_comments", (DL_FUNC)&r_blank_comments, 1},
    {NULL, NULL, 0}};

void R_init_decor(DllInfo* info) {
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
}
