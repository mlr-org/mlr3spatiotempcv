context("mlr_resampling_spcv-all")

# run tests --------------------------------------------------------------------

test_that("spcv has no duplicated ids", {
  spcv_rsp = mlr_resamplings$mget(
    as.data.table(mlr_resamplings)[map_lgl(key, grepl, pattern = "spcv"), key]
  )
  spcv_rsp$`repeated-spcv-coords` = NULL
  for (i in spcv_rsp) {
    expect_identical(i$duplicated_ids, FALSE)
  }
})

test_that("grouping throws errors when 'groups' is set", {
  spcv_rsp = mlr_resamplings$mget(
    as.data.table(mlr_resamplings)[map_lgl(key, grepl, pattern = "spcv"), key]
  )
  spcv_rsp$`repeated-spcv-coords` = NULL
  spcv_rsp$`repeated-spcv-env` = NULL

  for (i in spcv_rsp) {
    expect_error(i$instantiate(task_grp),
      "Grouping is not supported for spatial resampling methods")
  }
})

test_that("train and test set getter functions are working", {
  spcv_rsp = mlr_resamplings$mget(
    as.data.table(mlr_resamplings)[map_lgl(key, grepl, pattern = "spcv"), key]
  )
  spcv_rsp$`spcv-buffer` = NULL

  for (i in spcv_rsp) {
    # FIXME: seting folds=5 is only needed because of #17
    if (grepl("Repeated", class(i)[1])) {
      i$param_set$values = list(folds = 4, repeats = 2)
    } else {
      i$param_set$values = list(folds = 4)
    }
    i$instantiate(task)
    expect_silent(i$train_set(1))
    expect_silent(i$test_set(1))
  }
})

test_that("cloning works", {
  spcv_rsp = mlr_resamplings$mget(
    as.data.table(mlr_resamplings)[map_lgl(key, grepl, pattern = "spcv"), key]
  )
  spcv_rsp$`repeated-spcv-coords` = NULL

  for (i in spcv_rsp) {
    clone = i$clone(deep = TRUE)
    expect_true(all.equal(i, clone))
  }
})
