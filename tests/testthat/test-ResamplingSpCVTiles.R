test_that("mlr3spatiotempcv is equal to sperrorest (repeated)", {
  task = tsk("ecuador")
  set.seed(42)
  rsp = rsmp("spcv_tiles", reassign = FALSE, nsplit = c(4L, 3L))
  rsp$instantiate(task)

  data = task$data()
  data_with_coords = cbind(data, task$coordinates())


  # sperrorest
  set.seed(42)
  sperr_out = sperrorest::partition_tiles(data_with_coords,
    reassign = FALSE, nsplit = c(4, 3))

  sperr_train = mlr3misc::map(sperr_out[[1]], function(x) x$train)
  sperr_test = mlr3misc::map(sperr_out[[1]], function(x) x$test)

  expect_identical(rsp$instance$test, sperr_test, ignore_attr = "names")
  expect_identical(rsp$instance$train, sperr_train, ignore_attr = "names")
})

test_that("mlr3spatiotempcv is equal to sperrorest (repeated) - nsplit + dsplit", {
  task = tsk("ecuador")
  set.seed(42)
  rsp = rsmp("spcv_tiles",
    reassign = FALSE, nsplit = c(4L, 3L), dsplit = c(1L, 2L))
  rsp$instantiate(task)

  data = task$data()
  data_with_tiles = cbind(data, task$coordinates())

  # sperrorest
  set.seed(42)
  sperr_out = sperrorest::partition_tiles(data_with_tiles,
    reassign = FALSE, nsplit = c(4L, 3L), dsplit = c(1L, 2L),
    repetition = 1:2)

  sperr_train = mlr3misc::map(sperr_out[[1]], function(x) x$train)
  sperr_test = mlr3misc::map(sperr_out[[1]], function(x) x$test)

  expect_equal(rsp$instance$test, sperr_test, ignore_attr = "names")
  expect_equal(rsp$instance$train, sperr_train, ignore_attr = "names")
})

test_that("mlr3spatiotempcv is equal to sperrorest (repeated) - reassign", {
  task = tsk("ecuador")
  set.seed(42)
  rsp = rsmp("spcv_tiles",
    reassign = TRUE, nsplit = c(4L, 3L))
  rsp$instantiate(task)

  data = task$data()
  data_with_tiles = cbind(data, task$coordinates())

  # sperrorest
  set.seed(42)
  sperr_out = sperrorest::partition_tiles(data_with_tiles,
    reassign = TRUE, nsplit = c(4L, 3L),
    repetition = 1:2)

  sperr_train = mlr3misc::map(sperr_out[[1]], function(x) x$train)
  sperr_test = mlr3misc::map(sperr_out[[1]], function(x) x$test)

  expect_equal(rsp$instance$test, sperr_test, ignore_attr = "names")
  expect_equal(rsp$instance$train, sperr_train, ignore_attr = "names")
})

test_that("mlr3spatiotempcv is equal to sperrorest (repeated) - user_rotation", {
  task = tsk("ecuador")
  set.seed(42)
  rsp = rsmp("spcv_tiles",
    user_rotation = 30, nsplit = c(4L, 3L),
    rotation = "user")
  rsp$instantiate(task)

  data = task$data()
  data_with_tiles = cbind(data, task$coordinates())

  # sperrorest
  set.seed(42)
  sperr_out = sperrorest::partition_tiles(data_with_tiles,
    user_rotation = 30, nsplit = c(4L, 3L),
    rotation = "user",
    repetition = 1:2)

  sperr_train = mlr3misc::map(sperr_out[[1]], function(x) x$train)
  sperr_test = mlr3misc::map(sperr_out[[1]], function(x) x$test)

  expect_equal(rsp$instance$test, sperr_test, ignore_attr = "names")
  expect_equal(rsp$instance$train, sperr_train, ignore_attr = "names")
})

test_that("mlr3spatiotempcv is equal to sperrorest (repeated) - random rotation", {
  task = tsk("ecuador")
  set.seed(42)
  rsp = rsmp("spcv_tiles",
    nsplit = c(4L, 3L),
    rotation = "random")
  rsp$instantiate(task)

  data = task$data()
  data_with_tiles = cbind(data, task$coordinates())

  # sperrorest
  set.seed(42)
  sperr_out = sperrorest::partition_tiles(data_with_tiles,
    nsplit = c(4L, 3L),
    rotation = "random",
    repetition = 1:2)

  sperr_train = mlr3misc::map(sperr_out[[1]], function(x) x$train)
  sperr_test = mlr3misc::map(sperr_out[[1]], function(x) x$test)

  expect_equal(rsp$instance$test, sperr_test, ignore_attr = "names")
  expect_equal(rsp$instance$train, sperr_train, ignore_attr = "names")
})

test_that("mlr3spatiotempcv is equal to sperrorest (repeated) - random offset", {
  task = tsk("ecuador")
  set.seed(42)
  rsp = rsmp("spcv_tiles",
    nsplit = c(4L, 3L),
    offset = "random")
  rsp$instantiate(task)

  data = task$data()
  data_with_tiles = cbind(data, task$coordinates())

  # sperrorest
  set.seed(42)
  sperr_out = sperrorest::partition_tiles(data_with_tiles,
    nsplit = c(4L, 3L),
    offset = "random",
    repetition = 1)

  sperr_train = mlr3misc::map(sperr_out[[1]], function(x) x$train)
  sperr_test = mlr3misc::map(sperr_out[[1]], function(x) x$test)

  expect_equal(rsp$instance$test[[1]], sperr_test[[1]], ignore_attr = "names")
  expect_equal(rsp$instance$train[[1]], sperr_train[[1]], ignore_attr = "names")
})

test_that("mlr3spatiotempcv is equal to sperrorest (repeated) - user offset", {
  task = tsk("ecuador")
  set.seed(42)
  rsp = rsmp("spcv_tiles",
    nsplit = c(4L, 3L),
    offset = "user", user_offset = c(0.5, 0.9))
  rsp$instantiate(task)

  data = task$data()
  data_with_tiles = cbind(data, task$coordinates())

  # sperrorest
  set.seed(42)
  sperr_out = sperrorest::partition_tiles(data_with_tiles,
    nsplit = c(4L, 3L),
    offset = "user", user_offset = c(0.5, 0.9),
    repetition = 1:2)

  sperr_train = mlr3misc::map(sperr_out[[1]], function(x) x$train)
  sperr_test = mlr3misc::map(sperr_out[[1]], function(x) x$test)

  expect_equal(rsp$instance$test[[1]], sperr_test[[1]], ignore_attr = "names")
  expect_equal(rsp$instance$train[[1]], sperr_train[[1]], ignore_attr = "names")
})
