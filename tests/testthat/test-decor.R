test_that("cpp_files works", {
  expect_equal(cpp_files(character()), character())

  expect_equal(cpp_files(""), character())

  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))

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

test_that("cpp_decorations_work", {
  test_cpp_decorations(
    "",
    tibble(
      file = NA_character_,
      line = integer(),
      decoration = character(),
      params = list(),
      context = character()
    )
  )

  test_cpp_decorations(
    "// [[pkg::export]] void foo() { }",
    tibble(
      file = NA_character_,
      line = 1L,
      decoration = "pkg::export",
      params = list("pkg::export"),
      context = list("// [[pkg::export]] void foo() { }")
    )
  )

  test_cpp_decorations(
    "// [[pkg::export]]\nvoid foo()\n{\n}",
    tibble(
      file = NA_character_,
      line = 1L,
      decoration = "pkg::export",
      params = list("pkg::export"),
      context = list(c("// [[pkg::export]]", "void foo()", "{", "}"))
    )
  )

  test_cpp_decorations(
    "// [[pkg::include(Bar)]] void foo() { }",
    tibble(
      file = NA_character_,
      line = 1L,
      decoration = "pkg::include",
      params = list(setNames(list(as.symbol("Bar")), "")),
      context = list("// [[pkg::include(Bar)]] void foo() { }")
    )
  )

  test_cpp_decorations(
    "// [[pkg::include('Bar')]] void foo() { }",
    tibble(
      file = NA_character_,
      line = 1L,
      decoration = "pkg::include",
      params = list(setNames(list("Bar"), "")),
      context = list("// [[pkg::include('Bar')]] void foo() { }")
    )
  )

  test_cpp_decorations(
    "// [[pkg::include(foo = 'Bar')]] void foo() { }",
    tibble(
      file = NA_character_,
      line = 1L,
      decoration = "pkg::include",
      params = list(setNames(list("Bar"), "foo")),
      context = list("// [[pkg::include(foo = 'Bar')]] void foo() { }")
    )
  )

  # If is_attribute == FALSE don't detect attributes
  test_cpp_decorations(
    "[[pkg::export]] void foo() { }",
    tibble(
      file = NA_character_,
      line = integer(),
      decoration = character(),
      params = list(),
      context = character()
    )
  )

  test_cpp_decorations(is_attribute = TRUE,
    "[[pkg::export]] void foo() { }",
    tibble(
      file = NA_character_,
      line = 1L,
      decoration = "pkg::export",
      params = list("pkg::export"),
      context = list("[[pkg::export]] void foo() { }")
    )
  )

  test_cpp_decorations(is_attribute = TRUE,
    "[[pkg::include(foo = 'Bar')]] void foo() { }",
    tibble(
      file = NA_character_,
      line = 1L,
      decoration = "pkg::include",
      params = list(setNames(list("Bar"), "foo")),
      context = list("[[pkg::include(foo = 'Bar')]] void foo() { }")
    )
  )
})

test_that("parse_cpp_function works", {
  expect_equal(
    parse_cpp_function("void foo() {  }"),
    tibble(
      name = "foo",
      return_type = "void",
      args = list(
        tibble(
          type = character(),
          name = character(),
          default = character()
        )
      )
    )
  )

  expect_equal(
    parse_cpp_function(c("void foo()", "{", "}")),
    tibble(
      name = "foo",
      return_type = "void",
      args = list(
        tibble(
          type = character(),
          name = character(),
          default = character()
        )
      )
    )
  )

  expect_equal(
    parse_cpp_function(c("double foo(int bar)", "{", "}")),
    tibble(
      name = "foo",
      return_type = "double",
      args = list(
        tibble(
          type = "int",
          name = "bar",
          default = NA_character_
        )
      )
    )
  )

  expect_equal(
    parse_cpp_function(c("double foo(int bar)", "{", "}")),
    tibble(
      name = "foo",
      return_type = "double",
      args = list(
        tibble(
          type = "int",
          name = "bar",
          default = NA_character_
        )
      )
    )
  )

  expect_equal(
    parse_cpp_function(c("double foo(int bar, const char* baz)", "{", "}")),
    tibble(
      name = "foo",
      return_type = "double",
      args = list(
        tibble(
          type = c("int", "const char*"),
          name = c("bar", "baz"),
          default = c(NA_character_, NA_character_)
        )
      )
    )
  )

  expect_equal(
    parse_cpp_function(c("double foo(int bar = 1, const char* baz = \"hi\")", "{", "}")),
    tibble(
      name = "foo",
      return_type = "double",
      args = list(
        tibble(
          type = c("int", "const char*"),
          name = c("bar", "baz"),
          default = c("1", '"hi"')
        )
      )
    )
  )
})
