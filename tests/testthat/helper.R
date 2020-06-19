test_cpp_decorations <- function(content, results, is_attribute = FALSE) {
  files <- vapply(content, function(x) {
    f <- tempfile()
    writeLines(x, f)
    f
  }, character(1))

  on.exit(unlink(files))

  res <- cpp_decorations(files = files, is_attribute = is_attribute)
   if (NROW(res) > 0) {
     res$file <- NA_character_
   }
  #return(res)
  expect_equal(res, results)
}
