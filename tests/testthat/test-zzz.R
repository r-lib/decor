test_that("map_if works with function predicates", {
  expect_equal(
    map_if(letters[1:5], function(x) identical(x, "c"), toupper),
    list("a", "b", "C", "d", "e")
  )
})
