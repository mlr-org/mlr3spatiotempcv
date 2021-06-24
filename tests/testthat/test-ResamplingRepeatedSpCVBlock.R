test_that("folds can be printed", {
  task = test_make_twoclass_task()
  rsp = rsmp("repeated_spcv_block", folds = 3, repeats = 2, range = c(2L, 4L))
  rsp$instantiate(task)

  expect_equal(rsp$folds(1:6), c(1, 2, 3, 1, 2, 3))
})

test_that("reps can be printed", {
  task = test_make_twoclass_task()
  rsp = rsmp("repeated_spcv_block", folds = 3, repeats = 2, range = c(2L, 4L))
  rsp$instantiate(task)

  expect_equal(rsp$repeats(1:9), c(1, 1, 1, 2, 2, 2, 3, 3, 3))
})

test_that("resampling iterations equals folds * repeats", {
  task = test_make_twoclass_task()
  rsp = rsmp("repeated_spcv_block", folds = 3, repeats = 2, range = c(2L, 4L))
  rsp$instantiate(task)

  expect_equal(rsp$iters, 6)
})

test_that("error when neither cols & rows | range is specified", {
  task = test_make_twoclass_task()
  rsmp = rsmp("repeated_spcv_block", repeats = 2)
  expect_error(
    rsmp$instantiate(task),
    "Either 'range' or 'cols' & 'rows' need to be set.")
})

test_that("error when length(range) != length(repeats)", {
  task = test_make_twoclass_task()
  rsp = rsmp("repeated_spcv_block", repeats = 2, range = 5L)
  expect_error(
    rsp$instantiate(task),
    ".*to be the same length as 'range'."
  )
})

test_that("no error when length(range) == repeats", {
  task = test_make_twoclass_task()
  rsp = rsmp("repeated_spcv_block", folds = 3, repeats = 2, range = c(2L, 4L))
  expect_silent(rsp$instantiate(task))
})

test_that("error when number of desired folds is larger than number possible blocks", {
  task = test_make_twoclass_task()
  rsp = rsmp("repeated_spcv_block", folds = 10, repeats = 2, range = c(2L, 4L))
  expect_error(rsp$instantiate(task))
})

test_that("mlr3spatiotempcv indices are the same as blockCV indices: cols and rows", {
  task = test_make_blockCV_test_task()

  sf::sf_use_s2(use_s2 = FALSE)

  set.seed(42)
  rsmp = rsmp("repeated_spcv_block",
    repeats = 2,
    folds = 5,
    rows = 3,
    cols = 4)
  rsmp$instantiate(task)

  testSF = test_make_blockCV_test_df()

  set.seed(42)
  capture.output(testBlock <- suppressMessages(
    blockCV::spatialBlock(
      speciesData = testSF,
      k = 5,
      rows = 3,
      cols = 4,
      showBlocks = FALSE,
      verbose = FALSE,
      progress = FALSE)
  ))

  # only use the first iteration
  expect_equal(rsmp$instance$fold[1:5000], testBlock$foldID)
})
