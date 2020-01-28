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
