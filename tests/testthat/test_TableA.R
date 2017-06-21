context("Table A")

test_that("Table A providers even if retired", {
  # University of Tasmania
  expect_true(decode_heims(data.table(E306 = 1052L))[["TableA"]])
})
