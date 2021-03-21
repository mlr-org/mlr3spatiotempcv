if (requireNamespace("testthat", quietly = TRUE)) {
  library(checkmate)
  library(testthat)
  library(mlr3spatiotempcv)

  test_check("mlr3spatiotempcv")
}
