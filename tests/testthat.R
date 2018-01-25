if (requireNamespace("testthat", quietly = TRUE)) {
  library(testthat)
  library(data.table)
  library(magrittr)
  library(heims)

  test_check("heims")
}
