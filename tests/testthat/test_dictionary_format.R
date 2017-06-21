context("Dictionary format")

test_that("No repeated long_names", {
  dict_names <- browse_elements(".")
  expect_false(any(duplicated(dict_names$long_name)))
  expect_false(any(duplicated(dict_names$orig_name)))
})


