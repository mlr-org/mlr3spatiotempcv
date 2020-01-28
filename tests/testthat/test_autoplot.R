context("autoplot")

test_that("autoplot works for SpCVCoords", {
  set.seed(1)

  task = TEST_MAKE_TWOCLASS()
  rsp = rsmp("spcv-coords", folds = 4)
  rsp$instantiate(task)
  coords1 = autoplot(rsp, task)
  coords2 = autoplot(rsp, task, 1)
  coords3 = autoplot(rsp, task, c(1, 2, 3, 4))

  expect_true(is.ggplot(coords1))
  expect_true(is.ggplot(coords2))
  expect_true(is.ggplot(coords3))

  skip_on_ci()
  vdiffr::expect_doppelganger("SpCVCoords all test sets", coords1)
  vdiffr::expect_doppelganger("SpCVCoords - Fold 1", coords2)
  vdiffr::expect_doppelganger("SpCVCoords - Fold 1-4", coords3)
})

test_that("autoplot works for RepeatedSpCVCoords", {
  set.seed(1)

  task = TEST_MAKE_TWOCLASS()
  rsp = rsmp("repeated-spcv-coords", folds = 4, repeats = 2)
  rsp$instantiate(task)
  rcoords1 = autoplot(rsp, task)
  rcoords2 = autoplot(rsp, task, repeats_id = 2)
  rcoords3 = autoplot(rsp, task, fold_id = 1)
  rcoords4 = autoplot(rsp, task, fold_id = 1, repeats_id = 2)
  rcoords5 = autoplot(rsp, task, fold_id = c(1, 2, 3, 4))

  expect_true(is.ggplot(rcoords1))
  expect_true(is.ggplot(rcoords2))
  expect_true(is.ggplot(rcoords3))
  expect_true(is.ggplot(rcoords4))
  expect_true(is.ggplot(rcoords5))

  skip_on_ci()
  vdiffr::expect_doppelganger("RepSpCVCoords all test sets", rcoords1)
  vdiffr::expect_doppelganger("RepSpCVCoords all test sets, Rep 2", rcoords2)
  vdiffr::expect_doppelganger("RepSpCVCoords - Fold 1, Rep 2", rcoords3)
  vdiffr::expect_doppelganger("RepSpCVCoords - Fold 1, Rep 1", rcoords4)
  vdiffr::expect_doppelganger("RepSpCVCoords - Fold 1-4", rcoords5)
})

test_that("autoplot works for SpCVBlock", {
  set.seed(1)

  task = TEST_MAKE_TWOCLASS()
  rsp = rsmp("spcv-block", cols = 2, rows = 2, folds = 4)
  rsp$instantiate(task)
  block1 = autoplot(rsp, task)
  block2 = autoplot(rsp, task, 1)
  block3 = autoplot(rsp, task, c(1, 2, 3, 4))

  expect_true(is.ggplot(block1))
  expect_true(is.ggplot(block2))
  expect_true(is.ggplot(block3))

  # these error checks apply to all resampling methods.
  expect_error(autoplot(rsp, task, 20))
  expect_error(autoplot(rsp, task, c(1, 20)))
  expect_list(autoplot(rsp, task, c(1, 2, 3, 4), grid = FALSE))

  skip_on_ci()
  vdiffr::expect_doppelganger("SpCVBlock all test sets", block1)
  vdiffr::expect_doppelganger("SpCVBlock - Fold 1", block2)
  vdiffr::expect_doppelganger("SpCVBlock - Fold 1-4", block3)
})

test_that("autoplot works for RepeatedSpCVBlock", {
  set.seed(1)

  task = TEST_MAKE_TWOCLASS()
  rsp = rsmp("repeated-spcv-block", folds = 4, repeats = 2, range = c(2, 4))
  rsp$instantiate(task)
  rblock1 = autoplot(rsp, task)
  rblock2 = autoplot(rsp, task, repeats_id = 2)
  rblock3 = autoplot(rsp, task, fold_id = 1)
  rblock4 = autoplot(rsp, task, fold_id = 1, repeats_id = 2)
  rblock5 = autoplot(rsp, task, fold_id = c(1, 2, 3, 4))

  expect_true(is.ggplot(rblock1))
  expect_true(is.ggplot(rblock2))
  expect_true(is.ggplot(rblock3))
  expect_true(is.ggplot(rblock4))
  expect_true(is.ggplot(rblock5))

  # these error checks apply to all resampling methods.
  expect_error(autoplot(rsp, task, 30))
  expect_error(autoplot(rsp, task, c(1, 30)))
  expect_list(autoplot(rsp, task, c(1, 2, 3, 4), grid = FALSE))

  skip_on_ci()
  vdiffr::expect_doppelganger("RepSpCVBlock all test sets", rblock1)
  vdiffr::expect_doppelganger("RepSpCVBlock all test sets, Rep 2", rblock2)
  vdiffr::expect_doppelganger("RepSpCVBlock - Fold 1, Rep 2", rblock3)
  vdiffr::expect_doppelganger("RepSpCVBlock - Fold 1, Rep 1", rblock4)
  vdiffr::expect_doppelganger("RepSpCVBlock - Fold 1-4", rblock5)
})

test_that("autoplot works for SpCVBuffer", {
  set.seed(1)

  task = TEST_MAKE_TWOCLASS()
  rsp = rsmp("spcv-buffer", theRange = 2)
  rsp$instantiate(task)
  # one does not want to see nrow plots in a grid
  expect_error(autoplot(rsp, task))
  buffer2 = autoplot(rsp, task, 1)
  buffer3 = autoplot(rsp, task, c(1, 2, 3, 4))

  skip_on_ci()
  vdiffr::expect_doppelganger("SpCVBuffer - Fold 1", buffer2)
  vdiffr::expect_doppelganger("SpCVBuffer - Fold 1-4", buffer3)
})

test_that("autoplot works for SpCVEnv", {
  set.seed(1)

  task = TEST_MAKE_TWOCLASS()
  rsp = rsmp("spcv-env", features = "p_1", folds = 2)
  rsp$instantiate(task)
  env1 = autoplot(rsp, task)
  env2 = autoplot(rsp, task, 1)
  env3 = autoplot(rsp, task, c(1, 2))

  skip_on_ci()
  vdiffr::expect_doppelganger("SpCVEnv all test sets", env1)
  vdiffr::expect_doppelganger("SpCVEnv - Fold 1", env2)
  vdiffr::expect_doppelganger("SpCVEnv - Fold 1-2", env3)
})

test_that("autoplot works for RepeatedSpCVEnv", {
  set.seed(1)

  task = TEST_MAKE_TWOCLASS()
  rsp = rsmp("repeated-spcv-env", features = "p_1", folds = 2, repeats = 2)
  rsp$instantiate(task)
  renv1 = autoplot(rsp, task)
  renv2 = autoplot(rsp, task, repeats_id = 2)
  renv3 = autoplot(rsp, task, fold_id = 1)
  renv4 = autoplot(rsp, task, fold_id = 1, repeats_id = 2)
  renv5 = autoplot(rsp, task, fold_id = c(1, 2))

  expect_true(is.ggplot(renv1))
  expect_true(is.ggplot(renv2))
  expect_true(is.ggplot(renv3))
  expect_true(is.ggplot(renv4))
  expect_true(is.ggplot(renv5))

  skip_on_ci()
  vdiffr::expect_doppelganger("RepSpCVEnv all test sets", renv1)
  vdiffr::expect_doppelganger("RepSpCVEnv all test sets, Rep 2", renv2)
  vdiffr::expect_doppelganger("RepSpCVEnv - Fold 1, Rep 2", renv3)
  vdiffr::expect_doppelganger("RepSpCVEnv - Fold 1, Rep 1", renv4)
  vdiffr::expect_doppelganger("RepSpCVEnv - Fold 1-2", renv5)
})
