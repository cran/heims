context("utils")

test_that("Utilities work as expected", {
  expect_true(AND(TRUE, TRUE))
  expect_false(AND(FALSE, stop("This should never be encountered ",
                               "due to short-circuiting.")))
  expect_true(OR(TRUE, stop("This should never be encountered ",
                            "due to short-circuiting.")))
  expect_false(OR(FALSE, FALSE))

  expect_false(any(never(letters)))
  expect_true(all(every(letters)))
  expect_true(always(""))


  expect_true(is.Date(20150404))
  expect_true(is.Date("20150404"))

  expect_true(is.YearMonth(201503))

  i <- sample(1:9, size = 1)
  expect_equal(nth_digit_of(987654321, i), i)

  expect_identical(rm_leading_0s("0001"), "1")
  expect_identical(rm_leading_0s("ok as-is: 0001"), "ok as-is: 0001")

  xn <- rnorm(10)
  expect_identical(rm_leading_0s(xn), xn)

  expect_identical(force_integer(c("500", "boo")),
                   c(500L, NA_integer_))

})
