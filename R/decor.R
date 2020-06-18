#' C++ files from a package
#'
#' @param pkg See [devtools::as.package()]
#'
#' @export
cpp_files <- function(pkg = ".") {
  if (length(pkg) == 0) {
    return(character())
  }

  src <- file.path(pkg, "src")
  if (dir.exists(src)) {
    return(list.files(src, full.names = TRUE, pattern = "[.](cc|cpp|h|hpp)$"))
  }

  return(character())
}

#' Decorations in a C++ file
#'
#' @param pkg A package, see [devtools::as.package()]
#' @param files C++ files
#' @param is_attribute set to true if the decorations are C++11 attributes
#'
#' @export
cpp_decorations <- function(pkg = ".", files = cpp_files(pkg = pkg), is_attribute = FALSE) {

  cpp_attribute_pattern <-  paste0(
    "^[[:blank:]]*",                     ## allow for indentation
    if (!is_attribute) "//[[:blank:]]*", ## the comment should be started by //, with potential spaces following
    "\\[\\[",                            ## the opening square brackets
    "[[:space:]]*(.*?)[[:space:]]*",     ## the material within
    "\\]\\].*$"                          ## closing brackets
  )

  map_dfr(files, function(file){
    lines <- readLines(file)

    start <- grep(cpp_attribute_pattern, lines)
    if (length(start)) {
      end <- c(tail(start, -1L) - 1L, length(lines))

      n <- length(start)
      text <- lines[start]
      content <- sub(cpp_attribute_pattern, "\\1", text)

      decoration <- sub("\\(.*$", "", content)

      has_args <- grepl("\\(", content)
      params <- map_if(content, has_args, ~{
        call_args(parse(text = .x)[[1]])
      })

      context <- map2(start, end, ~lines[seq2(.x, .y)])

      tibble(file, line = start, decoration, params, context)
    }

  })

}


parse_cpp_function <- function(context) {
  context <- grep("^[[:space:]]*//", context, value = TRUE, invert = TRUE)
  first_brace_or_statement <- grep("[{;]", context)[1L]
  # If not a first brace assume it is just a declaration.
  signature <- sub("[[:space:]]*[{].*$", "", paste(context[seq2(1L, first_brace_or_statement)], collapse = " "))

  .Call(decor_parse_cpp_function, signature)
}
