context("ResamplingSpCVBlock")

test_that("resampling iterations equals folds", {
  task = test_make_twoclass()
  rsp = rsmp("spcv_block", folds = 2, range = 2)
  rsp$instantiate(task)

  expect_equal(rsp$iters, 2)
})

test_that("error when number of desired folds is larger than number possible blocks", {
  task = test_make_twoclass()
  rsp = rsmp("spcv_block", folds = 10, range = 4)

  expect_error(rsp$instantiate(task))
})

test_that("error when neither cols & rows | range is specified", {
  task = test_make_twoclass()
  rsp = rsmp("spcv_block")

  expect_error(
    rsp$instantiate(task),
    "Either 'range' or 'cols' & 'rows' need to be set.")
})

test_that("error when only one of rows or cols is set", {
  task = test_make_twoclass()
  rsp = rsmp("spcv_block", rows = 4)

  expect_error(
    rsp$instantiate(task),
    "Either 'range' or 'cols' & 'rows' need to be set."
  )
})

test_that("Error when length(range) >= 2", {
  expect_error(rsmp("spcv_block", range = c(500, 1000)))
})
