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
