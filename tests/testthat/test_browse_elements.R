context("browse_elements")

test_that("Browse elements works on length-2", {
  out <- browse_elements(c("Provider", "Maj"))
  expect_true("Maj_course_ind" %in% out[["long_name"]])
  expect_true("HE_Provider_name" %in% out[["long_name"]])
})
