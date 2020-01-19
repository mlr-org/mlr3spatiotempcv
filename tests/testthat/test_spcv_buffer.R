context("mlr_resampling_spcv-buffer")

# run tests --------------------------------------------------------------------

test_that("grouping throws errors when 'groups' is set", {
  spcv_rsp_buffer = mlr_resamplings$get("spcv-buffer")

  expect_error(spcv_rsp_buffer$instantiate(task_grp),
    "Grouping is not supported for spatial resampling methods")
})

test_that("grouping throws errors when 'groups' and 'stratify' is set", {
  spcv_rsp_buffer = mlr_resamplings$get("spcv-buffer")

  expect_error(spcv_rsp_buffer$instantiate(task_grp),
    "Grouping is not supported for spatial resampling methods")
})

test_that("train and test set getter functions are working", {
  spcv_rsp_buffer = mlr_resamplings$get("spcv-buffer")

  spcv_rsp_buffer$instantiate(task)
  expect_silent(spcv_rsp_buffer$train_set(1))
  expect_silent(spcv_rsp_buffer$test_set(1))
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
