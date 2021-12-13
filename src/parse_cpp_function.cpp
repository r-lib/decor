#include <R.h>
#include <Rinternals.h>

#include <string>
#include <vector>

static const char* const kWhitespaceChars = " \f\n\r\t\v";

void set_rownames(SEXP x, int n) {
  SEXP rownames = PROTECT(Rf_allocVector(INTSXP, 2));
  INTEGER(rownames)[0] = NA_INTEGER;
  INTEGER(rownames)[1] = -n;
  Rf_setAttrib(x, R_RowNamesSymbol, rownames);
  UNPROTECT(1);
}

void set_tibble(SEXP x) {
  SEXP classes = PROTECT(Rf_allocVector(STRSXP, 3));
  SET_STRING_ELT(classes, 0, Rf_mkChar("tbl_df"));
  SET_STRING_ELT(classes, 1, Rf_mkChar("tbl"));
  SET_STRING_ELT(classes, 2, Rf_mkChar("data.frame"));
  Rf_classgets(x, classes);
  UNPROTECT(1);
}

void trimWhitespace(std::string& s) {
  // skip empty case
  if (s.empty()) return;

  // trim right
  std::string::size_type pos = s.find_last_not_of(kWhitespaceChars);
  if (pos != std::string::npos) s.erase(pos + 1);

  // trim left
  pos = s.find_first_not_of(kWhitespaceChars);
  s.erase(0, pos);
}

SEXP parse_arguments(const std::string& args) {
  std::vector<std::string> arguments;

  int templateCount = 0;
  int parenCount = 0;
  bool insideQuotes = false;
  std::string currentArg;

  char prevChar = 0;
  for (std::string::const_iterator it = args.begin(); it != args.end(); ++it) {
    char ch = *it;
    if (ch == '"' && prevChar != '\\') {
      insideQuotes = !insideQuotes;
    }

    if (ch == ',' && !templateCount && !parenCount && !insideQuotes) {
      arguments.push_back(currentArg);
      currentArg.clear();
    } else {
      currentArg.push_back(ch);
      switch (ch) {
        case '<':
          templateCount++;
          break;
        case '>':
          templateCount--;
          break;
        case '(':
          parenCount++;
          break;
        case ')':
          parenCount--;
          break;
      }
    }

    prevChar = ch;
  }

  if (!currentArg.empty() && currentArg != "void") {
    arguments.push_back(currentArg);
  }

  int n = arguments.size();

  SEXP type = PROTECT(Rf_allocVector(STRSXP, n));
  SEXP def = PROTECT(Rf_allocVector(STRSXP, n));
  SEXP name = PROTECT(Rf_allocVector(STRSXP, n));

  for (int i = 0; i < n; i++) {
    std::string arg = arguments[i];

    std::string::size_type start = arg.find_first_not_of(kWhitespaceChars);
    std::string::size_type end = arg.find_last_not_of(kWhitespaceChars);

    // find default value (if any).
    std::string::size_type eqPos = arg.find_first_of('=', start);

    if (eqPos != std::string::npos) {
      std::string::size_type default_start =
          arg.find_first_not_of(kWhitespaceChars, eqPos + 1);
      SET_STRING_ELT(def, i,
                     Rf_mkCharLen(arg.data() + default_start, end - default_start + 1));
      arg.erase(eqPos);
    } else {
      SET_STRING_ELT(def, i, NA_STRING);
    }

    // only keep (trimmed) part before the '='
    arg.erase(0, start);
    end = arg.find_last_not_of(kWhitespaceChars);
    if (end != std::string::npos) {
      arg.erase(end + 1);
    }

    // where does the type end
    end = arg.find_last_of(kWhitespaceChars);

    // name
    SET_STRING_ELT(name, i, Rf_mkCharLen(arg.data() + end + 1, arg.size() - end - 1));

    // type
    SET_STRING_ELT(type, i, Rf_mkCharLen(arg.data(), end));
  }

  SEXP tbl_args = PROTECT(Rf_allocVector(VECSXP, 3));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, 3));

  SET_VECTOR_ELT(tbl_args, 0, type);
  SET_STRING_ELT(names, 0, Rf_mkChar("type"));

  SET_VECTOR_ELT(tbl_args, 1, name);
  SET_STRING_ELT(names, 1, Rf_mkChar("name"));

  SET_VECTOR_ELT(tbl_args, 2, def);
  SET_STRING_ELT(names, 2, Rf_mkChar("default"));
  Rf_namesgets(tbl_args, names);
  set_tibble(tbl_args);

  set_rownames(tbl_args, n);

  UNPROTECT(5);
  return tbl_args;
}

extern "C" SEXP parse_cpp_function(SEXP signature_) {
  std::string signature = CHAR(STRING_ELT(signature_, 0));

  // find last ')' and first '('
  std::string::size_type endParenLoc = signature.find_last_of(')');
  std::string::size_type beginParenLoc = signature.find_first_of('(');

  // find name of the function and return type
  std::string preamble = signature.substr(0, signature.find_last_not_of(kWhitespaceChars, beginParenLoc - 1) + 1);
  std::string::size_type sep = preamble.find_last_of(kWhitespaceChars);
  std::string name = preamble.substr(sep + 1);
  std::string return_type = preamble.substr(0, sep);

  std::string args = signature.substr(beginParenLoc + 1, endParenLoc - beginParenLoc - 1);
  trimWhitespace(args);

  SEXP res = PROTECT(Rf_allocVector(VECSXP, 3));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, 3));

  SET_VECTOR_ELT(res, 0, Rf_mkString(name.c_str()));
  SET_STRING_ELT(names, 0, Rf_mkChar("name"));

  SET_VECTOR_ELT(res, 1, Rf_mkString(return_type.c_str()));
  SET_STRING_ELT(names, 1, Rf_mkChar("return_type"));

  SEXP args_lst = PROTECT(Rf_allocVector(VECSXP, 1));
  SET_VECTOR_ELT(args_lst, 0, parse_arguments(args));

  SET_VECTOR_ELT(res, 2, args_lst);
  SET_STRING_ELT(names, 2, Rf_mkChar("args"));

  set_rownames(res, 1);
  set_tibble(res);
  Rf_setAttrib(res, R_NamesSymbol, names);
  UNPROTECT(3);
  return res;
}
