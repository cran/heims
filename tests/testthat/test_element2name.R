context("element2name")

test_that("element2name", {
  expect_equal(element2name("E346"), "Country_of_birth")
  expect_equal(element2name("E306"), "HE_Provider_name")
  expect_equal(element2name(c("E306", "E346")), c("HE_Provider_name", "Country_of_birth"))
})
