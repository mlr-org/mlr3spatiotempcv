test_that("folds can be printed", {
  skip_if_not_installed("blockCV")

  task = test_make_twoclass_task()
  rsp = rsmp("repeated_spcv_block", folds = 3, repeats = 2, range = c(2L, 4L))
  rsp$instantiate(task)

  expect_equal(rsp$folds(1:6), c(1, 2, 3, 1, 2, 3))
})

test_that("reps can be printed", {
  skip_if_not_installed("blockCV")

  task = test_make_twoclass_task()
  rsp = rsmp("repeated_spcv_block", folds = 3, repeats = 2, range = c(2L, 4L))
  rsp$instantiate(task)

  expect_equal(rsp$repeats(1:9), c(1, 1, 1, 2, 2, 2, 3, 3, 3))
})

test_that("resampling iterations equals folds * repeats", {
  skip_if_not_installed("blockCV")

  task = test_make_twoclass_task()
  rsp = rsmp("repeated_spcv_block", folds = 3, repeats = 2, range = c(2L, 4L))
  rsp$instantiate(task)

  expect_equal(rsp$iters, 6)
})

test_that("error when neither cols & rows | range is specified", {
  skip_if_not_installed("blockCV")

  task = test_make_twoclass_task()
  rsmp = rsmp("repeated_spcv_block", repeats = 2)
  expect_error(
    rsmp$instantiate(task),
    "Either 'range' or 'cols' & 'rows' need to be set.")
})

test_that("error when length(range) != length(repeats)", {
  skip_if_not_installed("blockCV")

  task = test_make_twoclass_task()
  rsp = rsmp("repeated_spcv_block", repeats = 2, range = 5L)
  expect_error(
    rsp$instantiate(task),
    ".*to be the same length as 'range'."
  )
})

test_that("no error when length(range) == repeats", {
  skip_if_not_installed("blockCV")

  task = test_make_twoclass_task()
  rsp = rsmp("repeated_spcv_block", folds = 3, repeats = 2, range = c(2L, 4L))
  rsp$instantiate(task)
  expect_class(rsp, "ResamplingRepeatedSpCVBlock")
})

test_that("error when number of desired folds is larger than number possible blocks", {
  skip_if_not_installed("blockCV")

  task = test_make_twoclass_task()
  rsp = rsmp("repeated_spcv_block", folds = 20, repeats = 2, range = c(2L, 4L))
  expect_error(rsp$instantiate(task))
})

test_that("mlr3spatiotempcv indices are the same as blockCV indices: cols and rows", {
  skip_if_not_installed("blockCV")

  task = test_make_blockCV_test_task()
  testSF = test_make_blockCV_test_df()

  sf::sf_use_s2(use_s2 = FALSE)

  set.seed(42)
  rsmp = rsmp("repeated_spcv_block",
    repeats = 2,
    folds = 4,
    rows = 3,
    cols = 4)
  suppressMessages(rsmp$instantiate(task))

  testBlock_new = blockCV::cv_spatial(
    x = testSF,
    k = 4,
    rows_cols = c(3, 4),
    hexagon = FALSE,
    report = FALSE,
    plot = FALSE,
    verbose = FALSE,
    progress = FALSE,
    seed = 42
  )

  testBlock_old = blockCV::spatialBlock(
    speciesData = testSF,
    k = 4,
    rows = 3,
    cols = 4,
    showBlocks = FALSE,
    progress = FALSE,
    verbose = FALSE
  )

  # tempcv vs. blockCV_new  only use the first iteration
  expect_equal(rsmp$instance$fold[1:1000], testBlock_new$folds_ids)
  # tempcv vs. blockCV_old
  expect_equal(rsmp$instance$fold[1:1000], testBlock_old$foldID)
})

test_that("Error when selection = checkboard and folds > 2", {
  skip_if_not_installed("blockCV")

  task = test_make_blockCV_test_task()
  expect_error(rsmp("repeated_spcv_block", range = 10000L,
    selection = "checkerboard", folds = 5)$instantiate(task))
})
