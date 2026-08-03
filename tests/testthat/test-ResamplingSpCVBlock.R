test_that("resampling iterations equals folds", {
  skip_if_not_installed("blockCV")

  task = test_make_twoclass_task()
  rsp = rsmp("spcv_block", folds = 2, range = 2L)
  rsp$instantiate(task)

  expect_equal(rsp$iters, 2)
})

test_that("error when number of desired folds is larger than number possible blocks", {
  skip_if_not_installed("blockCV")

  task = test_make_twoclass_task()
  rsp = rsmp("spcv_block", folds = 10, range = 4L)

  expect_error(rsp$instantiate(task))
})

test_that("error when neither cols & rows | range is specified", {
  skip_if_not_installed("blockCV")

  task = test_make_twoclass_task()
  rsp = rsmp("spcv_block")

  expect_error(
    rsp$instantiate(task),
    "Either 'range' or 'cols' & 'rows' need to be set.")
})

test_that("error when only one of rows or cols is set", {
  skip_if_not_installed("blockCV")

  task = test_make_twoclass_task()
  rsp = rsmp("spcv_block", rows = 4)

  expect_error(
    rsp$instantiate(task),
    "Either 'range' or 'cols' & 'rows' need to be set."
  )
})

test_that("Error when length(range) >= 2", {
  skip_if_not_installed("blockCV")

  expect_error(rsmp("spcv_block", range = c(500L, 1000L)))
})


test_that("mlr3spatiotempcv indices are the same as blockCV indices: selection = checkerboard", {
  skip_if_not_installed("blockCV")

  # see https://github.com/mlr-org/mlr3spatiotempcv/issues/92

  task = test_make_blockCV_test_task()

  set.seed(42)
  rsmp = rsmp("spcv_block",
    range = 50000L,
    selection = "checkerboard",
    folds = 2)
  rsmp$instantiate(task)

  testSF = test_make_blockCV_test_df()

  # set.seed(42)
  testBlock = suppressWarnings(suppressMessages(
    blockCV::cv_spatial(
      x = testSF,
      size = 50000L,
      k = 2,
      selection = "checkerboard",
      hexagon = FALSE,
      plot = FALSE,
      report = FALSE,
      progress = FALSE,
      verbose = FALSE
  )))

  expect_equal(rsmp$instance$fold, testBlock$folds_ids)
})

test_that("mlr3spatiotempcv indices are the same as blockCV indices: cols and rows", {
  skip_if_not_installed("sf")
  skip_if_not_installed("blockCV")
  task = test_make_blockCV_test_task()

  sf::sf_use_s2(use_s2 = FALSE)

  rsmp = rsmp("spcv_block",
    folds = 5,
    rows = 3,
    cols = 4)
  rsmp$instantiate(task)

  testSF = test_make_blockCV_test_df()

  testBlock = suppressMessages(
    blockCV::cv_spatial(
      x = testSF,
      k = 5,
      rows_cols = c(3, 4),
      hexagon = FALSE,
      plot = FALSE,
      verbose = FALSE,
      report = FALSE,
      progress = FALSE
  ))

  expect_equal(rsmp$instance$fold, testBlock$folds_ids)
})

test_that("mlr3spatiotempcv indices are the same as blockCV indices: spatRaster", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  # same issue on macOS 3.6
  testthat::skip_if(as.numeric(R.version$major) < 4)

  task = test_make_blockCV_test_task()
  testSF = test_make_blockCV_test_df()

  spatras = terra::rast(testSF)
  vals = seq_len(terra::ncell(spatras))
  terra::setValues(spatras, vals)

  set.seed(42)
  rsmp1 = rsmp("spcv_block",
    range = 50000L,
    folds = 2,
    selection = "random",
    rasterLayer = spatras)
  rsmp1$instantiate(task)

  set.seed(42)
  # blockCV
  testBlock = suppressMessages(
    blockCV::cv_spatial(
      x = testSF,
      size = 50000L,
      selection = "random",
      r = spatras,
      k = 2,
      hexagon = FALSE,
      plot = FALSE,
      report = FALSE,
      verbose = FALSE,
      progress = FALSE
  ))

  # tempcv vs blockCV
  expect_equal(rsmp1$instance$fold, testBlock$folds_ids)
})

test_that("Error when selection = checkboard and folds > 2", {
  skip_if_not_installed("sf")
  task = test_make_blockCV_test_task()
  expect_error(rsmp("spcv_block", range = 10000L,
    selection = "checkerboard", folds = 5)$instantiate(task))
})

test_that("fold balancing defaults do not change the fold assignment", {
  skip_if_not_installed("sf")
  skip_if_not_installed("blockCV")

  task = test_make_blockCV_test_task()
  testSF = test_make_blockCV_test_df()

  sf::sf_use_s2(use_s2 = FALSE)

  rsp = rsmp("spcv_block", folds = 4, rows = 3, cols = 4, seed = 42)
  suppressMessages(rsp$instantiate(task))

  testBlock = suppressMessages(blockCV::cv_spatial(
    x = testSF,
    k = 4,
    rows_cols = c(3, 4),
    hexagon = FALSE,
    plot = FALSE,
    report = FALSE,
    progress = FALSE,
    verbose = FALSE,
    seed = 42
  ))

  expect_equal(rsp$instance$fold, testBlock$folds_ids)
})

test_that("balance_on_target passes the task target to blockCV", {
  skip_if_not_installed("sf")
  skip_if_not_installed("blockCV")

  task = test_make_blockCV_test_task()
  testSF = test_make_blockCV_test_df()

  sf::sf_use_s2(use_s2 = FALSE)

  rsp = rsmp("spcv_block",
    folds = 4, rows = 3, cols = 4, seed = 42,
    balance_on_target = TRUE)
  suppressMessages(rsp$instantiate(task))

  testBlock = suppressMessages(blockCV::cv_spatial(
    x = testSF,
    column = "label",
    k = 4,
    rows_cols = c(3, 4),
    hexagon = FALSE,
    plot = FALSE,
    report = FALSE,
    progress = FALSE,
    verbose = FALSE,
    seed = 42
  ))

  expect_equal(rsp$instance$fold, testBlock$folds_ids)
})

test_that("balance = FALSE skips the balancing search", {
  skip_if_not_installed("sf")
  skip_if_not_installed("blockCV")

  task = test_make_blockCV_test_task()
  testSF = test_make_blockCV_test_df()

  sf::sf_use_s2(use_s2 = FALSE)

  rsp = rsmp("spcv_block",
    folds = 4, rows = 3, cols = 4, seed = 42,
    balance = FALSE)
  suppressMessages(rsp$instantiate(task))

  testBlock = suppressMessages(blockCV::cv_spatial(
    x = testSF,
    k = 4,
    rows_cols = c(3, 4),
    hexagon = FALSE,
    balance = FALSE,
    plot = FALSE,
    report = FALSE,
    progress = FALSE,
    verbose = FALSE,
    seed = 42
  ))

  expect_equal(rsp$instance$fold, testBlock$folds_ids)
})

test_that("iteration is passed to the balancing search", {
  skip_if_not_installed("sf")
  skip_if_not_installed("blockCV")

  task = test_make_blockCV_test_task()
  testSF = test_make_blockCV_test_df()

  sf::sf_use_s2(use_s2 = FALSE)

  rsp = rsmp("spcv_block",
    folds = 4, rows = 3, cols = 4, seed = 42,
    iteration = 5L)
  suppressMessages(rsp$instantiate(task))

  testBlock = suppressMessages(blockCV::cv_spatial(
    x = testSF,
    k = 4,
    rows_cols = c(3, 4),
    hexagon = FALSE,
    iteration = 5L,
    plot = FALSE,
    report = FALSE,
    progress = FALSE,
    verbose = FALSE,
    seed = 42
  ))

  expect_equal(rsp$instance$fold, testBlock$folds_ids)
})

test_that("presence_bg translates a two-class target to presence-background", {
  skip_if_not_installed("sf")
  skip_if_not_installed("blockCV")

  task = test_make_blockCV_pb_test_task()
  testSF = test_make_blockCV_pb_test_df()
  # blockCV wants the presences as numeric 1s, the task target is a factor with
  # the positive class "1"
  testSF$presence = as.integer(testSF$occ == "1")

  sf::sf_use_s2(use_s2 = FALSE)

  rsp = rsmp("spcv_block",
    folds = 4, rows = 3, cols = 4, seed = 42,
    balance_on_target = TRUE, presence_bg = TRUE)
  suppressWarnings(suppressMessages(rsp$instantiate(task)))

  cv_spatial_pb = function(presence_bg) {
    suppressWarnings(suppressMessages(blockCV::cv_spatial(
      x = testSF,
      column = "presence",
      presence_bg = presence_bg,
      k = 4,
      rows_cols = c(3, 4),
      hexagon = FALSE,
      plot = FALSE,
      report = FALSE,
      progress = FALSE,
      verbose = FALSE,
      seed = 42
    )))
  }

  expect_equal(rsp$instance$fold, cv_spatial_pb(TRUE)$folds_ids)
  # guard against 'presence_bg' silently not being passed on
  expect_false(identical(rsp$instance$fold, cv_spatial_pb(FALSE)$folds_ids))
})

test_that("num_bins is passed on for continuous targets", {
  skip_if_not_installed("sf")
  skip_if_not_installed("blockCV")

  task = test_make_blockCV_regr_test_task()
  testSF = test_make_blockCV_regr_test_df()

  sf::sf_use_s2(use_s2 = FALSE)

  rsp = rsmp("spcv_block",
    folds = 4, rows = 3, cols = 4, seed = 42,
    balance_on_target = TRUE, num_bins = 2L)
  suppressMessages(rsp$instantiate(task))

  testBlock = suppressMessages(blockCV::cv_spatial(
    x = testSF,
    column = "response",
    num_bins = 2L,
    k = 4,
    rows_cols = c(3, 4),
    hexagon = FALSE,
    plot = FALSE,
    report = FALSE,
    progress = FALSE,
    verbose = FALSE,
    seed = 42
  ))

  expect_equal(rsp$instance$fold, testBlock$folds_ids)
})

test_that("error when presence_bg is set without balance_on_target", {
  skip_if_not_installed("sf")
  skip_if_not_installed("blockCV")

  task = test_make_blockCV_test_task()
  rsp = rsmp("spcv_block", folds = 4, rows = 3, cols = 4, presence_bg = TRUE)

  expect_error(
    rsp$instantiate(task),
    "requires 'balance_on_target = TRUE'",
    fixed = TRUE
  )
})

test_that("error when presence_bg is used with a multiclass target", {
  skip_if_not_installed("sf")
  skip_if_not_installed("blockCV")

  task = test_make_multiclass()
  rsp = rsmp("spcv_block",
    folds = 2, range = 2L,
    balance_on_target = TRUE, presence_bg = TRUE)

  expect_error(
    rsp$instantiate(task),
    "requires a two-class target",
    fixed = TRUE
  )
})
