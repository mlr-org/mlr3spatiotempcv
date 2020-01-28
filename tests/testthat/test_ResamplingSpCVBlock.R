context("ResamplingSpCVBlock")

test_that("resampling iterations equals folds", {
  task = TEST_MAKE_TWOCLASS()
  rsp = rsmp("spcv-block", folds = 2, range = 2)
  rsp$instantiate(task)

  expect_equal(rsp$iters, 2)
})

test_that("cols and rows are ignored when range is given", {
  task = TEST_MAKE_TWOCLASS()
  rsp = rsmp("spcv-block", cols = 2, rows = 3, range = 2, folds = 2)

  expect_warning(rsp$instantiate(task), "Cols and rows are ignored. Range is used to generated blocks")
})

test_that("error when number of desired folds is larger than number possible blocks", {
  task = TEST_MAKE_TWOCLASS()
  rsp = rsmp("spcv-block", folds = 10, range = 4)

  expect_error(rsp$instantiate(task))
})

test_that("error when neither cols & rows | range is specified", {
  task = TEST_MAKE_TWOCLASS()
  rsp = rsmp("spcv-block")
  expect_error(rsp$instantiate(task),
    "Either 'range' or 'cols' & 'rows' need to be set.")
})

test_that("error when only one of rows or cols is set", {
  rsp = rsmp("spcv-block", rows = 4)
  expect_error(rsp$instantiate(task),
    "Either 'range' or 'cols' & 'rows' need to be set."
  )
})

test_that("Error when length(range) >= 2", {
  expect_error(rsmp("spcv-block", range = c(500, 1000)))
})
