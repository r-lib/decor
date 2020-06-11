#' C++ files from a package
#'
#' @param pkg See `devtools::as.package()`
#'
#' @export
cpp_files <- function(pkg = ".") {
  src <- path(pkg, "src")
  if (dir_exists(src)) {
    dir_ls(src, regexp = "[.](cc|cpp)$")
  } else {
    as_fs_path(chr())
  }

}

#' Decorations in a C++ file
#'
#' @param pkg A package, see `devtools::as.package()`
#' @param files C++ files
#'
#' @export
cpp_decorations <- function(pkg = ".", files = cpp_files(pkg = pkg)) {

  cpp_attribute_pattern <-  paste0(
    "^[[:blank:]]*",                 ## allow for indentation
    "//[[:blank:]]*",                ## the comment should be started by //, with potential spaces following
    "\\[\\[",                        ## the opening square brackets
    "[[:space:]]*(.*?)[[:space:]]*", ## the material within
    "\\]\\].*$"                      ## closing brackets
  )

  map_dfr(files, function(file){
    lines <- read_lines(file)

    start <- str_which(lines, cpp_attribute_pattern)
    if (length(start)) {
      end <- c(tail(start, -1L) - 1L, length(lines))

      n <- length(start)
      text <- lines[start]
      content <- str_replace(text, cpp_attribute_pattern, "\\1")

      decoration <- str_replace(content, "\\(.*$", "")

      has_args <- str_detect(content, "\\(")
      params <- map_if(content, has_args, ~{
        call_args(parse(text = .x)[[1]])
      })

      context <- map2(start, end, ~lines[seq2(.x, .y)])

      tibble(file, line = start, decoration, params, context)
    }

  })

}


parse_cpp_function <- function(context) {
  context <- str_subset(context, "^[[:space:]]*//", negate = TRUE)
  first_brace <- str_which(context, "[{]")[1L]
  signature <- str_replace(paste(context[seq2(1L, first_brace)], collapse = " "), "[[:space:]]*[{].*$", "")

  .Call(decor_parse_cpp_function, signature)
}
