context("mlr_resampling_generic")

# run tests --------------------------------------------------------------------

test_that("spcv has no duplicated ids", {
  spcv_rsp = mlr_resamplings$mget(
    as.data.table(mlr_resamplings)[map_lgl(key, grepl, pattern = "spcv"), key]
  )
  for (i in spcv_rsp) {
    expect_identical(i$duplicated_ids, FALSE)
  }
})

test_that("stratification throws errors", {
  spcv_rsp = mlr_resamplings$mget(
    as.data.table(mlr_resamplings)[map_lgl(key, grepl, pattern = "spcv"), key]
  )
  # because 'spcv-buffer' has no fold argument, we need to create an extra group
  # for some tests
  spcv_rsp_no_buffer = spcv_rsp
  spcv_rsp_no_buffer$`spcv-buffer` = NULL

  for (i in spcv_rsp_no_buffer) {
    i$param_set$values = list(folds = 5, stratify = TRUE)
    expect_error(i$instantiate(task))
  }
  spcv_rsp$`spcv-buffer`$param_set$values = list(stratify = TRUE, range = 100)
  expect_error(spcv_rsp$`spcv-buffer`$instantiate(task))
})

test_that("grouping throws errors when 'groups' is set", {
  spcv_rsp = mlr_resamplings$mget(
    as.data.table(mlr_resamplings)[map_lgl(key, grepl, pattern = "spcv"), key]
  )
  spcv_rsp_no_buffer = spcv_rsp
  spcv_rsp_no_buffer$`spcv-buffer` = NULL

  for (i in spcv_rsp) {
    expect_error(i$instantiate(task_grp),
      "Grouping is not supported for spatial resampling methods")
  }
})

test_that("grouping throws errors when 'groups' and 'stratify' is set", {
  spcv_rsp = mlr_resamplings$mget(
    as.data.table(mlr_resamplings)[map_lgl(key, grepl, pattern = "spcv"), key]
  )
  spcv_rsp_no_buffer = spcv_rsp
  spcv_rsp_no_buffer$`spcv-buffer` = NULL

  for (i in spcv_rsp_no_buffer) {
    i$param_set$values = list(folds = 5, stratify = TRUE)
    expect_error(i$instantiate(task_grp),
      "Grouping is not supported for spatial resampling methods")
  }

  spcv_rsp$`spcv-buffer`$param_set$values = list(stratify = TRUE, range = 100)
  expect_error(spcv_rsp$`spcv-buffer`$instantiate(task_grp),
    "Grouping is not supported for spatial resampling methods")
})

test_that("train and test set getter functions are working", {
  spcv_rsp = mlr_resamplings$mget(
    as.data.table(mlr_resamplings)[map_lgl(key, grepl, pattern = "spcv"), key]
  )

  spcv_rsp_no_buffer = spcv_rsp
  spcv_rsp_no_buffer$`spcv-buffer` = NULL

  for (i in spcv_rsp_no_buffer) {
    i$instantiate(task)
    expect_silent(i$train_set(1))
    expect_silent(i$test_set(1))
  }

  spcv_rsp$`spcv-buffer`$instantiate(task)
  expect_silent(spcv_rsp$`spcv-buffer`$train_set(1))
  expect_silent(spcv_rsp$`spcv-buffer`$test_set(1))
})

test_that("cloning works", {
  spcv_rsp = mlr_resamplings$mget(
    as.data.table(mlr_resamplings)[map_lgl(key, grepl, pattern = "spcv"), key]
  )

  for (i in spcv_rsp) {
    clone = i$clone(deep = TRUE)
    expect_true(all.equal(i, clone))
  }
})
