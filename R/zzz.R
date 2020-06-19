#' @useDynLib decor, .registration = TRUE
#' @importFrom tibble tibble
#' @importFrom utils head tail
NULL

map_if <- function(.x, .p, .f, ...) {
  if (is.function(.p)) {
    sel <- vapply(.x, .p, logical(1))
  } else {
    sel <- .p
  }

  out <- vector("list", length(.x))
  out[sel] <- lapply(.x[sel], .f, ...)
  out[!sel] <- .x[!sel]
  out
}

set_names <- function(x, nms = names2(x)) {
  names(x) <- nms
  x
}

names2 <- function(x) {
  if (is.null(names(x))) {
    names(x) <- rep("", length(x))
  }
  names(x)
}
