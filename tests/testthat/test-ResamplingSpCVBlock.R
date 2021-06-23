test_that("resampling iterations equals folds", {
  task = test_make_twoclass_task()
  rsp = rsmp("spcv_block", folds = 2, range = 2L)
  rsp$instantiate(task)

  expect_equal(rsp$iters, 2)
})

test_that("error when number of desired folds is larger than number possible blocks", {
  task = test_make_twoclass_task()
  rsp = rsmp("spcv_block", folds = 10, range = 4L)

  expect_error(rsp$instantiate(task))
})

test_that("error when neither cols & rows | range is specified", {
  task = test_make_twoclass_task()
  rsp = rsmp("spcv_block")

  expect_error(
    rsp$instantiate(task),
    "Either 'range' or 'cols' & 'rows' need to be set.")
})

test_that("error when only one of rows or cols is set", {
  task = test_make_twoclass_task()
  rsp = rsmp("spcv_block", rows = 4)

  expect_error(
    rsp$instantiate(task),
    "Either 'range' or 'cols' & 'rows' need to be set."
  )
})

test_that("Error when length(range) >= 2", {
  expect_error(rsmp("spcv_block", range = c(500L, 1000L)))
})


test_that("mlr3spatiotempcv indices are the same as blockCV indices: selection = checkerboard", {

  # see https://github.com/mlr-org/mlr3spatiotempcv/issues/92

  task = test_make_blockCV_test_task()

  rsmp = rsmp("spcv_block",
    range = 50000L,
    selection = "checkerboard")
  rsmp$instantiate(task)

  testSF = test_make_blockCV_test_df()

  capture.output(testBlock <- suppressMessages(
    suppressWarnings(blockCV::spatialBlock(
      speciesData = testSF,
      theRange = 50000L,
      selection = "checkerboard",
      showBlocks = FALSE))))

  expect_equal(rsmp$instance$fold, testBlock$foldID)
})

test_that("mlr3spatiotempcv indices are the same as blockCV indices: cols and rows", {
  task = test_make_blockCV_test_task()

  sf::sf_use_s2(use_s2 = FALSE)

  set.seed(42)
  rsmp = rsmp("spcv_block",
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

  expect_equal(rsmp$instance$fold, testBlock$foldID)
})

test_that("mlr3spatiotempcv indices are the same as blockCV indices: rasterLayer", {

  # cran test failure (Error: no arguments in initialization list)
  testthat::skip_on_os("solaris")
  # same issue on macOS 3.6
  testthat::skip_if(as.numeric(R.version$major) < 4)

  set.seed(42)

  task = test_make_blockCV_test_task()
  testSF = test_make_blockCV_test_df()

  rl = raster::raster(raster::extent(testSF), crs = raster::crs(testSF))
  vals = seq_len(raster::ncell(rl))
  rl = raster::setValues(rl, vals)

  rsmp1 = rsmp("spcv_block",
    range = 50000L,
    selection = "checkerboard",
    rasterLayer = rl)
  rsmp1$instantiate(task)

  # blockCV
  capture.output(testBlock <- suppressMessages(
    blockCV::spatialBlock(
      speciesData = testSF,
      theRange = 50000L,
      selection = "checkerboard",
      rasterLayer = rl,
      showBlocks = FALSE,
      verbose = FALSE,
      progress = FALSE)
  ))

  expect_equal(rsmp1$instance$fold, testBlock$foldID)
})
