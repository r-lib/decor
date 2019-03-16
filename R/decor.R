#' C++ files from a package
#'
#' @param pkg See [devtools::as.package()]
#'
#' @export
cpp_files <- function(pkg = ".") {
  pkg <- as.package(pkg)

  src <- path(pkg$path, "src")
  if (dir_exists(src)) {
    dir_ls(src, regexp = "[.](cc|cpp)$")
  } else {
    as_fs_path(chr())
  }

}

#' Decorations in a C++ file
#'
#' @param pkg A package, see [devtools::as.package()]
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

    line <- str_which(lines, cpp_attribute_pattern)
    n <- length(line)
    text <- lines[line]
    content <- str_replace(text, cpp_attribute_pattern, "\\1")

    name <- str_replace(content, "\\(.*$", "")

    has_args <- str_detect(content, "\\(")
    args <- map_if(content, has_args, ~{
      call_args(parse(text = .x)[[1]])
    })

    tibble(file, line, name, args, content)
  })

}
