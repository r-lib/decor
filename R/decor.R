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
