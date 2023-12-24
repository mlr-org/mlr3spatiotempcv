test_that("unloading leaves no trace", {
  library(mlr3spatiotempcv)
  n_resamplings = length(resamplings)
  n_total = length(mlr_resamplings$keys())
  unloadNamespace("mlr3spatiotempcv")
  n_mlr = length(mlr_resamplings$keys())
  expect_true(n_resamplings == n_total - n_mlr)
})
