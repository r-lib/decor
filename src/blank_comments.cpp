#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include <string>

/* blanks all C and C++ style comments in a file, preserving newlines */
std::string blank_comments(std::string data) {
  enum state_t {
    NORMAL,
    SINGLE_LINE_COMMENT,
    MULTI_LINE_COMMENT,
    QUOTE,
    DOUBLE_QUOTE,
  };
  /* state table,
     _ | " | ' | /\* | // | *\/ | \n |
     N | D | Q | M   | S  | N   | N  |
     S | S | S | S   | S  | S   | N  |
     M | M | M | -   | M  | N   | M  |
     Q | Q | N | Q   | Q  | Q   | Q  |
     D | N | D | D   | D  | D   | D  |
  */

  state_t state = NORMAL;

  const size_t len = data.size();
  for (size_t i = 0; i < len; ++i) {
    switch (state) {
      case NORMAL:
        switch (data[i]) {
          case '\'':
            state = QUOTE;
            break;
          case '\"':
            state = DOUBLE_QUOTE;
          case '\\':
            break;
          case '/':
            if (i < len - 1 && data[i + 1] == '*') {
              state = MULTI_LINE_COMMENT;
              data[i] = ' ';
              ++i;
              data[i] = ' ';
              break;
            }
            if (i < len - 1 && data[i + 1] == '/') {
              state = SINGLE_LINE_COMMENT;
              data[i] = ' ';
              ++i;
              data[i] = ' ';
              break;
            }
            break;
        }
        break;
      case SINGLE_LINE_COMMENT:
        if (data[i] == '\n') {
          state = NORMAL;
          break;
        }
        data[i] = ' ';
        break;
      case MULTI_LINE_COMMENT:
        if (i < len - 1 && data[i] == '*' && data[i + 1] == '/') {
          state = NORMAL;
          data[i] = ' ';
          data[i + 1] = ' ';
          break;
        }
        if (data[i] != '\n' && data[i] != '\r') {
          data[i] = ' ';
        }
      case QUOTE:
        if (data[i] == '\'') {
          state = NORMAL;
          break;
        }
        break;
      case DOUBLE_QUOTE:
        if (data[i] == '\"') {
          state = NORMAL;
          break;
        }
        break;
    }
  }
  return data;
}

extern "C" SEXP r_blank_comments(SEXP data_) {
  const std::string data = blank_comments(CHAR(STRING_ELT(data_, 0)));

  SEXP out = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(out, 0, Rf_mkCharLenCE(data.data(), data.size(), CE_UTF8));
  UNPROTECT(1);
  return out;
}

