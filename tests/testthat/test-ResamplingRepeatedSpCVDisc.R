test_that("folds can be printed", {
  task = tsk("ecuador")
  rsp = rsmp("repeated_spcv_disc",
    folds = 3, repeats = 5, radius = 200L,
    buffer = 200L)
  rsp$instantiate(task)

  expect_equal(rsp$folds(4:8), c(1, 2, 3, 1, 2))
})

test_that("reps can be printed", {
  task = tsk("ecuador")
  rsp = rsmp("repeated_spcv_disc",
    folds = 3, repeats = 5, radius = 200L,
    buffer = 200L)
  rsp$instantiate(task)

  expect_equal(rsp$repeats(4:8), c(2, 2, 2, 3, 3))
})

test_that("resampling iterations equals folds * repeats", {
  task = tsk("ecuador")
  rsp = rsmp("repeated_spcv_disc",
    folds = 3, repeats = 2, radius = 200L,
    buffer = 200L)
  rsp$instantiate(task)

  expect_equal(rsp$iters, 6)
})

test_that("mlr3spatiotempcv is equal to sperrorest (repeated) - radius + buffer", {
  task = tsk("ecuador")
  set.seed(42)
  rsp = rsmp("repeated_spcv_disc",
    folds = 3L, radius = 200L, buffer = 200L,
    repeats = 2)
  rsp$instantiate(task)

  data = task$data()
  data_with_disc = cbind(data, task$coordinates())


  # sperrorest
  set.seed(42)
  sperr_out = sperrorest::partition_disc(data_with_disc,
    ndisc = 3, radius = 200,
    buffer = 200,
    repetition = 1:2)

  sperr_train = mlr3misc::map(sperr_out[[2]], function(x) x$train)
  sperr_test = mlr3misc::map(sperr_out[[2]], function(x) x$test)

  expect_equal(rsp$instance[[2]]$test, sperr_test, ignore_attr = "names")
  expect_equal(rsp$instance[[2]]$train, sperr_train, ignore_attr = "names")
})

test_that("mlr3spatiotempcv is equal to sperrorest (repeated) - radius", {
  task = tsk("ecuador")
  set.seed(42)
  rsp = rsmp("repeated_spcv_disc",
    # FIXME: setting buffer = 0 should not be necessary here - fix in sperrorest
    folds = 3L, radius = 200L, buffer = 0,
    repeats = 2)
  rsp$instantiate(task)

  data = task$data()
  data_with_disc = cbind(data, task$coordinates())

  # sperrorest
  set.seed(42)
  sperr_out = sperrorest::partition_disc(data_with_disc,
    ndisc = 3, radius = 200, buffer = 0,
    repetition = 1:2)

  sperr_train = mlr3misc::map(sperr_out[[2]], function(x) x$train)
  sperr_test = mlr3misc::map(sperr_out[[2]], function(x) x$test)

  expect_equal(rsp$instance[[2]]$test, sperr_test, ignore_attr = "names")
  expect_equal(rsp$instance[[2]]$train, sperr_train, ignore_attr = "names")
})

test_that("mlr3spatiotempcv is equal to sperrorest (repeated) - replace", {
  task = tsk("ecuador")
  set.seed(42)
  rsp = rsmp("repeated_spcv_disc",
    # FIXME: setting buffer = 0 should not be necessary here - fix in sperrorest
    folds = 3L, radius = 200L, buffer = 200, replace = TRUE,
    repeats = 2)
  rsp$instantiate(task)

  data = task$data()
  data_with_disc = cbind(data, task$coordinates())

  # sperrorest
  set.seed(42)
  sperr_out = sperrorest::partition_disc(data_with_disc,
    ndisc = 3, radius = 200, buffer = 200, replace = TRUE,
    repetition = 1:2)

  sperr_train = mlr3misc::map(sperr_out[[2]], function(x) x$train)
  sperr_test = mlr3misc::map(sperr_out[[2]], function(x) x$test)

  expect_equal(rsp$instance[[2]]$test, sperr_test, ignore_attr = "names")
  expect_equal(rsp$instance[[2]]$train, sperr_train, ignore_attr = "names")
})
