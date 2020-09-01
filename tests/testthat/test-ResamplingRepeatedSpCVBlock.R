context("ResamplingRepeatedSpCVBlock")

test_that("folds can be printed", {
  task = test_make_twoclass()
  rsp = rsmp("repeated-spcv-block", folds = 3, repeats = 2, range = c(2, 4))
  rsp$instantiate(task)

  expect_equal(rsp$folds(1:6), c(1, 2, 1, 2, 1, 2))
})

test_that("reps can be printed", {
  task = test_make_twoclass()
  rsp = rsmp("repeated-spcv-block", folds = 3, repeats = 2, range = c(2, 4))
  rsp$instantiate(task)

  expect_equal(rsp$repeats(1:9), c(1, 1, 1, 2, 2, 2, 3, 3, 3))
})

test_that("resampling iterations equals folds * repeats", {
  task = test_make_twoclass()
  rsp = rsmp("repeated-spcv-block", folds = 3, repeats = 2, range = c(2, 4))
  rsp$instantiate(task)

  expect_equal(rsp$iters, 6)
})

test_that("error when neither cols & rows | range is specified", {
  task = test_make_twoclass()
  rsmp = rsmp("repeated-spcv-block", repeats = 2)
  expect_error(
    rsmp$instantiate(task),
    "Either 'range' or 'cols' & 'rows' need to be set.")
})

test_that("error when length(range) != length(repeats)", {
  task = test_make_twoclass()
  rsp = rsmp("repeated-spcv-block", repeats = 2, range = 5)
  expect_error(
    rsp$instantiate(task),
    ".*to be the same length as 'range'."
  )
})

test_that("no error when length(range) == repeats", {
  task = test_make_twoclass()
  rsp = rsmp("repeated-spcv-block", folds = 3, repeats = 2, range = c(2, 4))
  expect_silent(rsp$instantiate(task))
})

test_that("error when number of desired folds is larger than number possible blocks", {
  task = test_make_twoclass()
  rsp = rsmp("repeated-spcv-block", folds = 10, repeats = 2, range = c(2, 4))
  expect_error(rsp$instantiate(task))
})
