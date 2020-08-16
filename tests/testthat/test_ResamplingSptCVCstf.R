context("ResamplingSptCVCstf")

test_that("mlr3spatiotempcv implementation is equal to CAST: time + space", {
  task = tsk("cookfarm")
  rsp = rsmp("spcv-cstf", folds = 10)
  set.seed(42)
  rsp$instantiate(task, time_var = "Date", space_var = "SOURCEID")
  rsp$test_set(1)

  set.seed(42)
  indices = CAST::CreateSpacetimeFolds(task$data(), "SOURCEID", "Date")
  indices$indexOut[[1]]

  expect_equal(rsp$instance$test, indices$indexOut)
})

test_that("mlr3spatiotempcv implementation is equal to CAST: time ", {
  task = tsk("cookfarm")
  rsp = rsmp("spcv-cstf", folds = 10)
  set.seed(42)
  rsp$instantiate(task, time_var = "Date")
  rsp$test_set(1)

  set.seed(42)
  indices = CAST::CreateSpacetimeFolds(task$data(), timevar = "Date")
  indices$indexOut[[1]]

  expect_equal(rsp$instance$test, indices$indexOut)
})

test_that("mlr3spatiotempcv implementation is equal to CAST: space", {
  task = tsk("cookfarm")
  rsp = rsmp("spcv-cstf", folds = 10)
  set.seed(42)
  rsp$instantiate(task, space_var = "SOURCEID")
  rsp$test_set(1)

  set.seed(42)
  indices = CAST::CreateSpacetimeFolds(task$data(), spacevar = "SOURCEID")
  indices$indexOut[[1]]

  expect_equal(rsp$instance$test, indices$indexOut)
})

test_that("mlr3spatiotempcv implementation is equal to CAST: space", {
  task = tsk("cookfarm")
  rsp = rsmp("spcv-cstf", folds = 10)
  set.seed(42)
  rsp$instantiate(task, space_var = "SOURCEID", class = "TAXSUSDA")
  rsp$test_set(1)

  set.seed(42)
  indices = CAST::CreateSpacetimeFolds(task$data(),
    spacevar = "SOURCEID",
    seed = 42, class = "TAXSUSDA")
  indices$indexOut[[1]]

  expect_equal(rsp$instance$test, indices$indexOut)
})
