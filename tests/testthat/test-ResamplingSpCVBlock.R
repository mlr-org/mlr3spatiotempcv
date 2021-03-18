test_that("resampling iterations equals folds", {
  task = test_make_twoclass_task()
  rsp = rsmp("spcv_block", folds = 2, range = 2)
  rsp$instantiate(task)

  expect_equal(rsp$iters, 2)
})

test_that("error when number of desired folds is larger than number possible blocks", {
  task = test_make_twoclass_task()
  rsp = rsmp("spcv_block", folds = 10, range = 4)

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
  expect_error(rsmp("spcv_block", range = c(500, 1000)))
})


test_that("mlr3spatiotempcv indices are the same as blockCV indices: selection = checkerboard", {

  # see https://github.com/mlr-org/mlr3spatiotempcv/issues/92

  task = test_make_blockCV_test_task()

  rsmp <- rsmp("spcv_block",
    range = 50000,
    selection = "checkerboard")
  rsmp$instantiate(task)

  testSF = test_make_blockCV_test_df()

  capture.output(testBlock <- suppressMessages(
    suppressWarnings(blockCV::spatialBlock(
      speciesData = testSF,
      theRange = 50000,
      selection = "checkerboard",
      showBlocks = FALSE)
  )))

  expect_equal(rsmp$instance$fold, testBlock$foldID)
})

test_that("mlr3spatiotempcv indices are the same as blockCV indices: cols and rows", {
  task = test_make_blockCV_test_task()

  set.seed(42)
  rsmp <- rsmp("spcv_block",
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
  set.seed(42)

  task = test_make_blockCV_test_task()
  testSF = test_make_blockCV_test_df()

  r <- raster::raster(raster::extent(testSF), crs = "EPSG:4326")
  r[] <- 10

  rsmp <- rsmp("spcv_block",
    range = 50000,
    selection = "checkerboard",
    rasterLayer = r)
  rsmp$instantiate(task)


  # blockCV
  capture.output(testBlock <- suppressMessages(
    blockCV::spatialBlock(
      speciesData = testSF,
      theRange = 50000,
      selection = "checkerboard",
      rasterLayer = r,
      showBlocks = FALSE,
      verbose = FALSE,
      progress = FALSE)
  ))

  expect_equal(rsmp$instance$fold, testBlock$foldID)
})
