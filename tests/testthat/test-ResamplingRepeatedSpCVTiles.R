test_that("folds can be printed", {
  task = tsk("ecuador")
  rsp = rsmp("repeated_spcv_tiles",
    repeats = 2, nsplit = c(4L, 3L), reassign = FALSE)
  rsp$instantiate(task)

  expect_equal(rsp$folds(10:12), c(10, 11, 1))
})

test_that("reps can be printed", {
  task = tsk("ecuador")
  rsp = rsmp("repeated_spcv_tiles",
    repeats = 5, nsplit = c(4L, 3L), reassign = FALSE)
  rsp$instantiate(task)

  expect_equal(rsp$repeats(10:12), c(1, 1, 2))
})

test_that("resampling iterations equals folds * repeats", {
  task = tsk("ecuador")
  rsp = rsmp("repeated_spcv_tiles",
    repeats = 2, reassign = FALSE, nsplit = c(4L, 3L))
  rsp$instantiate(task)

  expect_equal(rsp$iters, 22)
})

test_that("mlr3spatiotempcv is equal to sperrorest (repeated) - nsplit + dsplit", {
  task = tsk("ecuador")
  set.seed(42)
  rsp = rsmp("repeated_spcv_tiles",
    repeats = 2, reassign = FALSE, nsplit = c(4L, 3L), dsplit = c(1L, 2L))
  rsp$instantiate(task)

  data = task$data()
  data_with_tiles = cbind(data, task$coordinates())


  # sperrorest
  set.seed(42)
  sperr_out = sperrorest::partition_tiles(data_with_tiles,
    reassign = FALSE, nsplit = c(4L, 3L), dsplit = c(1L, 2L),
    repetition = 1:2)

  sperr_train = mlr3misc::map(sperr_out[[2]], function(x) x$train)
  sperr_test = mlr3misc::map(sperr_out[[2]], function(x) x$test)

  expect_equal(rsp$instance[[2]]$test, sperr_test, ignore_attr = "names")
  expect_equal(rsp$instance[[2]]$train, sperr_train, ignore_attr = "names")
})

test_that("mlr3spatiotempcv is equal to sperrorest (repeated) - reassign", {
  task = tsk("ecuador")
  set.seed(42)
  rsp = rsmp("repeated_spcv_tiles",
    repeats = 2, reassign = TRUE, nsplit = c(4L, 3L))
  rsp$instantiate(task)

  data = task$data()
  data_with_tiles = cbind(data, task$coordinates())

  # sperrorest
  set.seed(42)
  sperr_out = sperrorest::partition_tiles(data_with_tiles,
    reassign = TRUE, nsplit = c(4L, 3L),
    repetition = 1:2)

  sperr_train = mlr3misc::map(sperr_out[[2]], function(x) x$train)
  sperr_test = mlr3misc::map(sperr_out[[2]], function(x) x$test)

  expect_equal(rsp$instance[[2]]$test, sperr_test, ignore_attr = "names")
  expect_equal(rsp$instance[[2]]$train, sperr_train, ignore_attr = "names")
})

test_that("mlr3spatiotempcv is equal to sperrorest (repeated) - user_rotation", {
  task = tsk("ecuador")
  set.seed(42)
  rsp = rsmp("repeated_spcv_tiles",
    repeats = 2, user_rotation = 30, nsplit = c(4L, 3L),
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

  sperr_train = mlr3misc::map(sperr_out[[2]], function(x) x$train)
  sperr_test = mlr3misc::map(sperr_out[[2]], function(x) x$test)

  expect_equal(rsp$instance[[2]]$test, sperr_test, ignore_attr = "names")
  expect_equal(rsp$instance[[2]]$train, sperr_train, ignore_attr = "names")
})

test_that("mlr3spatiotempcv is equal to sperrorest (repeated) - random rotation", {
  task = tsk("ecuador")
  set.seed(42)
  rsp = rsmp("repeated_spcv_tiles",
    repeats = 2, nsplit = c(4L, 3L),
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

  sperr_train = mlr3misc::map(sperr_out[[2]], function(x) x$train)
  sperr_test = mlr3misc::map(sperr_out[[2]], function(x) x$test)

  expect_equal(rsp$instance[[2]]$test, sperr_test, ignore_attr = "names")
  expect_equal(rsp$instance[[2]]$train, sperr_train, ignore_attr = "names")
})

test_that("mlr3spatiotempcv is equal to sperrorest (repeated) - random offset", {
  task = tsk("ecuador")
  set.seed(42)
  rsp = rsmp("repeated_spcv_tiles",
    repeats = 2, nsplit = c(4L, 3L),
    offset = "random")
  rsp$instantiate(task)

  data = task$data()
  data_with_tiles = cbind(data, task$coordinates())

  # sperrorest
  set.seed(42)
  sperr_out = sperrorest::partition_tiles(data_with_tiles,
    nsplit = c(4L, 3L),
    offset = "random",
    repetition = 1:2)

  sperr_train = mlr3misc::map(sperr_out[[2]], function(x) x$train)
  sperr_test = mlr3misc::map(sperr_out[[2]], function(x) x$test)

  expect_equal(rsp$instance[[2]]$test, sperr_test, ignore_attr = "names")
  expect_equal(rsp$instance[[2]]$train, sperr_train, ignore_attr = "names")
})

test_that("mlr3spatiotempcv is equal to sperrorest (repeated) - user offset", {
  task = tsk("ecuador")
  set.seed(42)
  rsp = rsmp("repeated_spcv_tiles",
    repeats = 2, nsplit = c(4L, 3L),
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

  sperr_train = mlr3misc::map(sperr_out[[2]], function(x) x$train)
  sperr_test = mlr3misc::map(sperr_out[[2]], function(x) x$test)

  expect_equal(rsp$instance[[2]]$test, sperr_test, ignore_attr = "names")
  expect_equal(rsp$instance[[2]]$train, sperr_train, ignore_attr = "names")
})
