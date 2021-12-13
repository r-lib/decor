describe("cpp_files", {
  it("returns an empty character if there are no C++ files", {
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
  })

  it("returns the C++ files if they exist", {
    d <- tempfile()
    on.exit(unlink(d, recursive = TRUE))
    dir.create(d)
    dir.create(file.path(d, "src"))

    file.create(file.path(d, "src", "foo.cc"))
    expect_equal(basename(cpp_files(d)), "foo.cc")

    file.create(file.path(d, "src", "foo.cpp"))
    expect_equal(basename(cpp_files(d)), c("foo.cc", "foo.cpp"))

    file.create(file.path(d, "src", "foo.h"))
    expect_equal(basename(cpp_files(d)), c("foo.cc", "foo.cpp", "foo.h"))

    file.create(file.path(d, "src", "foo.hpp"))
    expect_equal(basename(cpp_files(d)), c("foo.cc", "foo.cpp", "foo.h", "foo.hpp"))
  })

  it("returns the files ordered in the C locale", {
    d <- tempfile()
    on.exit(unlink(d, recursive = TRUE))
    dir.create(d)
    dir.create(file.path(d, "src"))

    nms <- c("B.cc", "a.cc", "c.cc", "D.cc")
    file.create(file.path(d, "src", nms))
    expect_equal(basename(cpp_files(d)), c("B.cc", "D.cc", "a.cc", "c.cc"))
  })
})

describe("cpp_decorations", {
  it("returns an 0 row tibble on empty inputs", {
    expect_equal(
      cpp_decorations(files = tempfile()),
      tibble(
        file = NA_character_,
        line = integer(),
        decoration = character(),
        params = list(),
        context = list()
      )
    )

    test_cpp_decorations(
      "",
      tibble(
        file = NA_character_,
        line = integer(),
        decoration = character(),
        params = list(),
        context = list()
      )
    )
  })

  it("works with single commented decorations without parameters", {
    test_cpp_decorations(
      "// [[pkg::export]]\nvoid foo() { }",
      tibble(
        file = NA_character_,
        line = 1L,
        decoration = "pkg::export",
        params = list("pkg::export"),
        context = list(c("// [[pkg::export]]", "void foo() { }"))
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
  })

  it("works with multiple commented decorations without parameters", {
    test_cpp_decorations(
      "// [[pkg::export]]\nvoid foo() { }\n// [[pkg::export]]\nvoid bar() { }",
      tibble(
        file = c(NA_character_, NA_character_),
        line = c(1L, 3L),
        decoration = c("pkg::export", "pkg::export"),
        params = list("pkg::export", "pkg::export"),
        context = list(c("// [[pkg::export]]", "void foo() { }"), c("// [[pkg::export]]", "void bar() { }"))
      )
    )
  })

  it("works with multiple commented decorations in multiple files without parameters", {
    test_cpp_decorations(
      c(
        "// [[pkg::export]]\nvoid foo() { }\n// [[pkg::export]]\nvoid bar() { }",
        "// [[pkg::export]]\nvoid foo2() { }\n// [[pkg::export]]\nvoid bar2() { }"
      ),
      tibble(
        file = c(
          NA_character_,
          NA_character_,
          NA_character_,
          NA_character_
        ),
        line = c(1L, 3L, 1L, 3L),
        decoration = c("pkg::export", "pkg::export", "pkg::export", "pkg::export"),
        params = list("pkg::export", "pkg::export", "pkg::export", "pkg::export"),
        context = list(
          c("// [[pkg::export]]", "void foo() { }"),
          c("// [[pkg::export]]", "void bar() { }"),
          c("// [[pkg::export]]", "void foo2() { }"),
          c("// [[pkg::export]]", "void bar2() { }")
        )
      )
    )
  })

  it("works with single commented decorations with parameters", {
    test_cpp_decorations(
      "// [[pkg::include(Bar)]]\nvoid foo() { }",
      tibble(
        file = NA_character_,
        line = 1L,
        decoration = "pkg::include",
        params = list(setNames(list(as.symbol("Bar")), "")),
        context = list(c("// [[pkg::include(Bar)]]", "void foo() { }"))
      )
    )

    test_cpp_decorations(
      "// [[pkg::include('Bar')]]\nvoid foo() { }",
      tibble(
        file = NA_character_,
        line = 1L,
        decoration = "pkg::include",
        params = list(setNames(list("Bar"), "")),
        context = list(c("// [[pkg::include('Bar')]]", "void foo() { }"))
      )
    )
  })

  it("works with single commented decorations with named parameters", {
    test_cpp_decorations(
      "// [[pkg::include(foo = 'Bar')]]\nvoid foo() { }",
      tibble(
        file = NA_character_,
        line = 1L,
        decoration = "pkg::include",
        params = list(setNames(list("Bar"), "foo")),
        context = list(c("// [[pkg::include(foo = 'Bar')]]", "void foo() { }"))
      )
    )
  })

  it("works with multiple commented decorations with named parameters", {
    test_cpp_decorations(
      "// [[pkg::include(foo = 'Bar')]]\nvoid foo() { }\n// [[pkg::include(foo = 'Baz')]]\nvoid bar() { }",
      tibble(
        file = c(NA_character_, NA_character_),
        line = c(1L, 3L),
        decoration = c("pkg::include", "pkg::include"),
        params = list(
          setNames(list("Bar"), "foo"),
          setNames(list("Baz"), "foo")
        ),
        context = list(
          c("// [[pkg::include(foo = 'Bar')]]", "void foo() { }"),
          c("// [[pkg::include(foo = 'Baz')]]", "void bar() { }")
        )
      )
    )
  })

  it("ignores non-commented decorations if is_attribute is FALSE", {
    test_cpp_decorations(
      "[[pkg::export]] void foo() { }",
      tibble(
        file = NA_character_,
        line = integer(),
        decoration = character(),
        params = list(),
        context = list()
      )
    )
  })

  it("works with non-commented decorations", {
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
  })

  it("works with non-commented decorations", {
    test_cpp_decorations(is_attribute = TRUE,
      "[[pkg::export]] void foo () { }",
      tibble(
        file = NA_character_,
        line = 1L,
        decoration = "pkg::export",
        params = list("pkg::export"),
        context = list("[[pkg::export]] void foo () { }")
      )
    )
  })

  it("works with multiple non-commented decorations with named parameters", {
    test_cpp_decorations(is_attribute = TRUE,
      "[[pkg::include(foo = 'Bar')]]\nvoid foo() { }\n[[pkg::include(foo = 'Baz')]]\nvoid bar() { }",
      tibble(
        file = c(NA_character_, NA_character_),
        line = c(1L, 3L),
        decoration = c("pkg::include", "pkg::include"),
        params = list(
          setNames(list("Bar"), "foo"),
          setNames(list("Baz"), "foo")
        ),
        context = list(
          c("[[pkg::include(foo = 'Bar')]]", "void foo() { }"),
          c("[[pkg::include(foo = 'Baz')]]", "void bar() { }")
        )
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

})

describe("parse_cpp_function", {
  it("returns an 0 row tibble for empty inputs", {
    expect_equal(
      parse_cpp_function(character()),
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

    expect_equal(
      parse_cpp_function(""),
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
  })

  it("works with zero argument functions without decorations", {

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
  })

  it("works with zero argument functions with commented decorations", {
    expect_equal(
      parse_cpp_function(c("// [[pkg::export]]", "void foo()", "{", "}")),
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
  })

  it("works with zero argument functions with non-commented decorations", {
    expect_equal(
      parse_cpp_function(c("[[pkg::export]]", "void foo()", "{", "}"), is_attribute = TRUE),
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
      parse_cpp_function(c("[[pkg::export]] void foo() {  }"), is_attribute = TRUE),
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
  })

  it("works with functions taking arguments", {
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
  })

  it("works with functions with default arguments", {
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

  it("works with complex arguments", {
    expect_equal(
      parse_cpp_function(c("foo::bar foo(const char[] bar, const std::string& baz = \"hi\")", "{", "}")),
      tibble(
        name = "foo",
        return_type = "foo::bar",
        args = list(
          tibble(
            type = c("const char[]", "const std::string&"),
            name = c("bar", "baz"),
            default = c(NA_character_, '"hi"')
          )
        )
      )
    )

    expect_equal(
      parse_cpp_function(c("foo::bar foo(std::vector<int>& bar, int baz = foo2())", "{", "}")),
      tibble(
        name = "foo",
        return_type = "foo::bar",
        args = list(
          tibble(
            type = c("std::vector<int>&", "int"),
            name = c("bar", "baz"),
            default = c(NA_character_, 'foo2()')
          )
        )
      )
    )
  })

  it("works with declarations", {
    expect_equal(
      parse_cpp_function(c("double foo(int bar = 1, const char* baz = \"hi\");")),
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

  describe("read_lines()", {
    it("works with empty files", {
      expect_equal(read_lines(content = character()), character())
    })
    it("returns the content if there are no comments", {
      expect_equal(read_lines(content = "'x' \"y\"\n z ** 2 / 1"), c("'x' \"y\"", " z ** 2 / 1"))
    })
    it("returns the content with blanked comments for single line comments", {
      expect_equal(read_lines(content = "foo\n// bar\nbaz"), c("foo", "      ", "baz"))
    })
    it("returns the content with blanked comments for multi-line comments", {
      expect_equal(read_lines(content = "/* foo\n//' */bar\nbaz"), c("      ", "      bar", "baz"))
    })

    it("quoted comments are ignored", {
      expect_equal(read_lines(content = '"/*" foo\n\'// */\'bar\nbaz'), c('"/*" foo', "\'// */\'bar", "baz"))
    })
  })
})
