test_that("cpp_files works", {
  expect_equal(cpp_files(character()), character())

  expect_equal(cpp_files(""), character())

  d <- tempfile()
  expect_equal(cpp_files(d), character())

  dir.create(d)
  expect_equal(cpp_files(d), character())

  dir.create(file.path(d, "src"))
  expect_equal(cpp_files(d), character())

  file.create(file.path(d, "src", "foo"))
  expect_equal(cpp_files(d), character())

  file.create(file.path(d, "src", "foo.c"))
  expect_equal(cpp_files(d), character())

  file.create(file.path(d, "src", "foo"))
  expect_equal(cpp_files(d), character())

  file.create(file.path(d, "src", "foo.cc"))
  expect_equal(basename(cpp_files(d)), "foo.cc")

  file.create(file.path(d, "src", "foo.cpp"))
  expect_equal(basename(cpp_files(d)), c("foo.cc", "foo.cpp"))

  file.create(file.path(d, "src", "foo.h"))
  expect_equal(basename(cpp_files(d)), c("foo.cc", "foo.cpp", "foo.h"))

  file.create(file.path(d, "src", "foo.hpp"))
  expect_equal(basename(cpp_files(d)), c("foo.cc", "foo.cpp", "foo.h", "foo.hpp"))
})
