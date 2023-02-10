#' C++ files from a package
#'
#' @param pkg The path to a package's root directory.
#'
#' @return A character vector of C++ files found in the package.
#' @export
#' @examples
#' # Setup
#' pkg <- tempfile()
#' dir.create(file.path(pkg, "src"), recursive = TRUE)
#' file.create(file.path(pkg, "src", "code.c"))
#' file.create(file.path(pkg, "src", "code.cpp"))
#'
#' # List the files, only the C++ file will be listed
#' cpp_files(pkg)
#'
#' # Cleanup
#' unlink(pkg, recursive = TRUE)
cpp_files <- function(pkg = ".") {
  if (length(pkg) == 0 || !nzchar(pkg[[1L]])) {
    return(character())
  }

  src <- file.path(pkg, "src")
  if (!dir.exists(src)) {
    return(character())
  }

  out <- list.files(src, full.names = TRUE, pattern = "[.](cc|cpp|h|hpp)$")
  # always sort these paths according to the C locale to avoid nuisance changes
  # in files generated downstream
  # TODO: switch to vctrs::vec_sort_radix() or vctrs::vec_sort() when possible
  out[order(vctrs::vec_rank(out))]
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
#' @examples
#' # Setup
#' f <- tempfile()
#' writeLines("[[cpp11::register]] int fun(int x = 1) { return x + 1; }", f)
#'
#' # Retrieve the decorations in the file
#' cpp_decorations(files = f, is_attribute = TRUE)
#'
#' # Cleanup
#' unlink(f)
cpp_decorations <- function(pkg = ".", files = cpp_files(pkg = pkg), is_attribute = FALSE) {

  res <- lapply(files, function(file) {
    if (!file.exists(file)) {
      return(tibble(file = character(), line = integer(), decoration = character(), params = list(), context = list()))
    }
    if (is_attribute) {
      lines <- read_lines(file)
    } else {
      lines <- readLines(file)
    }

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

read_lines <- function(file, content = readChar(file, file.size(file))) {
  if (length(content) == 0) {
    return(character())
  }
  without_comments <- .Call(blank_comments, content)

  strsplit(without_comments, "\r?\n")[[1]]
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
#' @examples
#' # Setup
#' context <- "int fun(int x) { return x + 1; }"
#'
#' # Parse the function
#' parse_cpp_function(context)
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
