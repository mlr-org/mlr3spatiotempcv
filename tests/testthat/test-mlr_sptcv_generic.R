library(mlr3)

test_that("no duplicated ids", {
  skip_if_not_installed("blockCV")
  skip_if_not_installed("sf")
  spcv_rsp = mlr_resamplings$mget(
    as.data.table(mlr_resamplings)[map_lgl(key, grepl, pattern = "spcv"), key]
  )

  for (i in spcv_rsp) {
    expect_identical(i$duplicated_ids, FALSE)
  }

  sptcv_rsp = rsmps(c("sptcv_cstf"), folds = 2)

  for (i in sptcv_rsp) {
    expect_identical(i$duplicated_ids, FALSE)
  }
})

test_that("grouping throws errors when 'groups' is set", {
  skip_if_not_installed("blockCV")
  skip_if_not_installed("sf")
  task = test_make_twoclass_task(group = TRUE)

  spcv_rsp = rsmps(c("spcv_coords", "spcv_env"), folds = 2)
  spcv_rsp = append(spcv_rsp, rsmp("spcv_block", folds = 2, rows = 2, cols = 2))
  spcv_rsp = append(spcv_rsp, rsmp("spcv_buffer", theRange = 1000))

  for (i in spcv_rsp) {
    expect_error(
      i$instantiate(task),
      "Grouping is not supported for spatial resampling methods")
  }
})

test_that("train and test set getter functions are working", {
  skip_if_not_installed("blockCV")
  skip_if_not_installed("sf")
  task = test_make_twoclass_task()

  spcv_rsp = rsmps(c("spcv_coords", "spcv_env"), folds = 2)
  spcv_rsp = append(spcv_rsp, rsmp("spcv_block", folds = 2, rows = 2, cols = 2))
  spcv_rsp = append(spcv_rsp, rsmp("spcv_buffer", theRange = 1000))

  for (i in spcv_rsp) {
    i$instantiate(task)
    expect_silent(i$train_set(1))
    expect_silent(i$test_set(1))
  }
})

test_that("train and test set getter functions are working", {
  skip_if_not_installed("blockCV")
  skip_if_not_installed("sf")
  task = test_make_twoclass_task()

  spcv_rsp = rsmps(c("repeated_spcv_coords", "repeated_spcv_env"), folds = 2)
  spcv_rsp = append(spcv_rsp, rsmp("repeated_spcv_block",
    folds = 2, rows = 2, cols = 2))

  for (i in spcv_rsp) {
    i$instantiate(task)
    expect_silent(i$train_set(1))
    expect_silent(i$test_set(1))
  }
})

test_that("partitioning resamplings reject 'folds = 1'", {
  # a single fold cannot be partitioned into train and test (#253)
  keys = c(
    "spcv_block", "spcv_coords", "spcv_env", "sptcv_cstf",
    "repeated_spcv_block", "repeated_spcv_coords", "repeated_spcv_env",
    "repeated_sptcv_cstf"
  )

  for (key in keys) {
    # paradox reports the widened integer bound, so only match on the id
    expect_error(rsmp(key, folds = 1), "folds")
  }
})

test_that("'spcv_disc' allows 'folds = 1'", {
  # here 'folds' is the number of sampled discs, not a partition of the data,
  # so a single fold still yields a non-empty training set
  task = test_make_twoclass_task()

  # the test coordinates form a 6x6 grid with 1 m spacing
  resampling = rsmp("spcv_disc", folds = 1, radius = 2, buffer = 1)
  resampling$instantiate(task)

  expect_equal(resampling$iters, 1)
  expect_gt(length(resampling$train_set(1)), 0)
  expect_gt(length(resampling$test_set(1)), 0)
})

test_that("cloning works", {
  skip_if_not_installed("skmeans")

  spcv_rsp = rsmps(c("spcv_coords", "spcv_env"), folds = 2)
  spcv_rsp = append(spcv_rsp, rsmp("spcv_block", folds = 2, rows = 2, cols = 2))
  spcv_rsp = append(spcv_rsp, rsmp("spcv_buffer", theRange = 1000))

  for (i in spcv_rsp) {
    clone = i$clone(deep = TRUE)
    expect_true(all.equal(i, clone))
  }

  sptcv_rsp = rsmps(c("sptcv_cstf"), folds = 2)

  for (i in sptcv_rsp) {
    clone = i$clone(deep = TRUE)
    expect_true(all.equal(i, clone))
  }
})
