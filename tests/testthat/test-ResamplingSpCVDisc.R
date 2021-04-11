test_that("mlr3spatiotempcv is equal to sperrorest (repeated)", {
  task = tsk("ecuador")
  set.seed(42)
  rsp = rsmp("spcv_disc", folds = 3L, radius = 200L, buffer = 200L)
  rsp$instantiate(task)

  data = task$data()
  data_with_coords = cbind(data, task$coordinates())


  # sperrorest
  set.seed(42)
  sperr_out = sperrorest::partition_disc(data_with_coords,
    ndisc = 3, radius = 200,
    buffer = 200)

  sperr_train = mlr3misc::map(sperr_out[[1]], function(x) x$train)
  sperr_test = mlr3misc::map(sperr_out[[1]], function(x) x$test)

  expect_identical(rsp$instance$test, sperr_test, ignore_attr = "names")
  expect_identical(rsp$instance$train, sperr_train, ignore_attr = "names")
})

test_that("error when number of desired folds is larger than number possible discs", {
  task = test_make_twoclass_task()
  rsp = rsmp("spcv_disc", folds = 30L, radius = 200L, buffer = 200L)

  expect_error(rsp$instantiate(task))
})
