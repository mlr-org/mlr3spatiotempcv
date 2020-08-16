context("mlr_resampling_spcv-all")

test_that("spcv has no duplicated ids", {
  spcv_rsp = mlr_resamplings$mget(
    as.data.table(mlr_resamplings)[map_lgl(key, grepl, pattern = "spcv"), key]
  )

  for (i in spcv_rsp) {
    expect_identical(i$duplicated_ids, FALSE)
  }
})

test_that("grouping throws errors when 'groups' is set", {
  task = test_make_twoclass(group = TRUE)

  spcv_rsp = mlr_resamplings$mget(
    as.data.table(mlr_resamplings)[map_lgl(key, grepl, pattern = "spcv"), key]
  )
  # nolint start
  spcv_rsp$`repeated-spcv-coords` = NULL
  spcv_rsp$`repeated-spcv-env` = NULL
  spcv_rsp$`repeated-spcv-block` = NULL
  spcv_rsp$`spcv-block` = NULL
  # nolint end

  for (i in spcv_rsp) {
    expect_error(
      i$instantiate(task),
      "Grouping is not supported for spatial resampling methods")
  }
})

test_that("train and test set getter functions are working", {
  task = test_make_twoclass()

  spcv_rsp = mlr_resamplings$mget(
    as.data.table(mlr_resamplings)[map_lgl(key, grepl, pattern = "spcv"), key]
  )

  # nolint start
  spcv_rsp$`spcv-buffer` = NULL
  spcv_rsp$`repeated-spcv-block` = NULL
  spcv_rsp$`repeated-spcv-coords` = NULL
  spcv_rsp$`repeated-spcv-env` = NULL
  spcv_rsp$`spcv-block` = NULL
  spcv_rsp$`sptcv-cluto` = NULL
  spcv_rsp$`sptcv-cstf` = NULL
  spcv_rsp$`repeated-sptcv-cluto` = NULL
  # nolint end

  for (i in spcv_rsp) {
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

  # nolint start
  spcv_rsp$`repeated-spcv-coords` = NULL
  spcv_rsp$`repeated-spcv-env` = NULL
  spcv_rsp$`repeated-spcv-block` = NULL
  spcv_rsp$`sptcv-cluto` = NULL
  spcv_rsp$`repeated-sptcv-cluto` = NULL
  # nolint end

  for (i in spcv_rsp) {
    clone = i$clone(deep = TRUE)
    expect_true(all.equal(i, clone))
  }
})
