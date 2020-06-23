#' C++ files from a package
#'
#' @param pkg The path to a package's root directory.
#'
#' @return A character vector of C++ files found in the package.
#' @export
cpp_files <- function(pkg = ".") {
  if (length(pkg) == 0 || !nzchar(pkg[[1L]])) {
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
#' @inheritParams cpp_files
#' @param files Paths to C++ files. If given, `pkg` will not be used.
#' @param is_attribute If `TRUE` the decorations are C++11 attributes, if `FALSE` they are comments.
#' @return A tibble with the decorations found, containing fields:
#' - file - The filename for the decoration
#' - line - The line the decoration was found
#' - decoration - The name of the decoration
#' - params - Any parameters given with the decoration
#' - context - The text of the decoration line and all lines until the next decoration (or the end of the file).
#' @export
cpp_decorations <- function(pkg = ".", files = cpp_files(pkg = pkg), is_attribute = FALSE) {

  res <- lapply(files, function(file) {
    if (!file.exists(file)) {
      return(tibble(file = character(), line = integer(), decoration = character(), params = list(), context = list()))
    }
    lines <- readLines(file)

    start <- grep(cpp_attribute_pattern(is_attribute), lines)
    if (!length(start)) {
      return(tibble(file = character(), line = integer(), decoration = character(), params = list(), context = list()))
    }
    end <- c(tail(start, -1L) - 1L, length(lines))

    text <- lines[start]
    content <- sub(paste0(cpp_attribute_pattern(is_attribute), ".*"), "\\1", text)

    decoration <- sub("\\(.*$", "", content)

    has_args <- grepl("\\(", content)
    params <- map_if(content, has_args, function(.x) {
      set_names(as.list(parse(text = .x)[[1]][-1]))
    })

    context <- mapply(function(.x, .y) lines[seq(.x, .y)], start, end, SIMPLIFY = FALSE)

    tibble(file, line = start, decoration, params, context)
  })

  vctrs::vec_rbind(!!!res);
}

cpp_attribute_pattern <- function(is_attribute) {
  paste0(
    "^[[:blank:]]*",                     ## allow for indentation
    if (!is_attribute) "//[[:blank:]]*", ## the comment should be started by //, with potential spaces following
    "\\[\\[",                            ## the opening square brackets
    "[[:space:]]*(.*?)[[:space:]]*",     ## the material within
    "\\]\\]",                            ## closing brackets
    "[[:space:]]*"                        ## trailing spaces
  )
}

#' Parse a C++ function
#'
#' Parses a C++ function returning a tibble with the function name and return
#' type and a list column with the arguments of the function.
#' @inheritParams cpp_decorations
#' @param context The function context, as obtained by the `context` column from [cpp_decorations()]
#' @return A tibble with the following fields:
#' - name - The name of the function
#' - return_type - The return type of the function
#' - args - A list column containing a tibble of the functions arguments
#'   - type - The type of the argument
#'   - name - The name of the argument
#'   - default - The default value of the argument (if any).
#' @export
parse_cpp_function <- function(context, is_attribute = FALSE) {
  if (length(context) == 0 || !nzchar(context[[1L]])) {
    return(
      tibble(
        name = character(),
        return_type = character(),
        args = list(
          tibble(
            type = character(),
            name = character(),
            default = character()
          )
        )
      )
    )
  }

  # Remove the decoration line if it exists
  context <- grep(paste0(cpp_attribute_pattern(is_attribute), "$"), context, value = TRUE, invert = TRUE)

  if (is_attribute) {
    # non-comment attributes may also be on the first line, they need to be removed
    context <- sub(cpp_attribute_pattern(is_attribute), "", context)
  }

  first_brace_or_statement <- grep("[{;]", context)[[1L]]

  # If not a first brace assume it is just a declaration.
  signature <- sub("[[:space:]]*[{].*$", "", paste(context[seq(1L, first_brace_or_statement)], collapse = " "))

  .Call(decor_parse_cpp_function, signature)
}
