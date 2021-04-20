test_that("mlr3spatiotempcv is equal to sperrorest", {
  task = tsk("cookfarm")
  data = task$data()
  data_with_coords = cbind(data, task$coordinates())

  set.seed(42)
  rsp = rsmp("cv_factor", factor = "TAXSUSDA")
  rsp$instantiate(task)

  # sperrorest
  set.seed(42)
  sperr_out = sperrorest::partition_factor(data_with_coords,
    fac = "TAXSUSDA")

  expect_identical(rsp$test_set(1), sperr_test[[1]], ignore_attr = "names")
  expect_identical(rsp$test_set(2), sperr_test[[2]], ignore_attr = "names")
})

test_that("mlr3spatiotempcv is equal to sperrorest - factor_as_feature = TRUE", {
  task = tsk("cookfarm")
  set.seed(42)
  rsp = rsmp("cv_factor", factor = "TAXSUSDA", factor_as_feature = TRUE)
  rsp$instantiate(task)

  data = task$data()
  data_with_coords = cbind(data, task$coordinates())

  # sperrorest
  set.seed(42)
  sperr_out = sperrorest::partition_factor(data_with_coords,
    fac = "TAXSUSDA")

  expect_identical(rsp$test_set(1), sperr_test[[1]], ignore_attr = "names")
  expect_identical(rsp$test_set(2), sperr_test[[2]], ignore_attr = "names")

  # check if factor is still within 'features' in task
  expect_subset("TAXSUSDA", task$col_roles$feature)
})
