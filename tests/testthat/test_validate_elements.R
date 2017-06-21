context("Validate elements")

test_that("validate_elements returns TRUE when valid", {
  # http://heimshelp.education.gov.au/sites/heimshelp/2016_data_requirements/2016dataelements/pages/306
  # says that a value, x, in variable E306 is valid iff x = 0 or x \in (1000, 9999)
  X <- data.frame(E306 = as.integer(c(0, 1011, 9999, 9998, 4500)),
                  E347 = c("0001", "A998", "1987", "1980", "2020"),
                  stringsAsFactors = FALSE)
  expect_true(all(validate_elements(X)))
})

test_that("validate_elements returns FALSE when invalid", {
  X <- data.frame(E306 = as.integer(c(0, 1011, 999, 9998)))
  expect_false(all(validate_elements(X, .progress_cat = TRUE)))
})

test_that("prop_elements and count_elements", {
  X <- data.table(E319 = c("X1200", "X999", "99999", "A9998"),
                  E327 = c(1L, 3L, 31L, 33L),
                  E339 = c(-1, -2, -3, -4),
                  # for ad_hoc_prepare
                  E355 = c(1L, 1L, 1L, 5L))

  expected_prop <- c(0.75, 1, 0, 1)
  names(expected_prop) <- names(X)
  expect_identical(prop_elements_valid(X),
                   expected_prop)

  expected_count <- c(1, 0, 4, 0)
  names(expected_count) <- names(X)
  expect_equal(count_elements_invalid(X),
               expected_count)



})


test_that("Element E493 prepared as expected", {
  expect_identical(heims_data_dict$E493$ad_hoc_prepare(as.integer(c(c(0, 10e3),
                                                                    c(seq.int(2, 11) * 10e3 + 2002),
                                                                    c(20004, 30017)))),
                   as.integer(c(c(0, 10e3),
                                c(seq.int(2, 11) * 10e3 + 2002),
                                29999, 39999)))

})

test_that("DOB less than current year", {
  skip("Not yet implemented")
  skip_if_not(file.exists("~/Students/cache/enrol_2005_2015.fst"))
  library(fst)
  library(data.table)
  enrols <- setDT(read.fst("~/Students/cache/enrol_2005_2015.fst"))
  rename_heims(enrols)
  enrols[, .(DOB, Ref_year)] %>%
    .[, DOB := as.Date()]
})

test_that("Valid elements for TER return TRUE or FALSE as expected", {
  x <- c(31L, 29L)
  y <- heims_data_dict$E369$ad_hoc_prepare(x)
  expect_true(heims_data_dict$E369$valid(y[1]))
  expect_true(heims_data_dict$E369$mark_missing(y[2]))
})
