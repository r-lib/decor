test_cpp_decorations <- function(content, results, is_attribute = FALSE) {
  f <- tempfile()
  on.exit(unlink(f))
  writeLines(content, f)

  res <- cpp_decorations(files = f, is_attribute = is_attribute)
  if (NROW(res) > 0) {
    res$file <- NA_character_
  }
  #return(res)
  expect_equal(res, results)
}
