context("autoplot")

test_that("autoplot works for SpCVCoords", {

  # SpCVCoords -----------------------------------------------------------------

  set.seed(42)

  task = tsk("ecuador")
  resa_coords = rsmp("spcv-coords")
  resa_coords$instantiate(task)
  coords1 = autoplot(resa_coords, task)
  coords2 = autoplot(resa_coords, task, 1)
  coords3 = autoplot(resa_coords, task, c(1, 2, 3, 4))

  expect_true(is.ggplot(coords1))
  expect_true(is.ggplot(coords2))
  expect_true(is.ggplot(coords3))

  skip_on_ci()
  vdiffr::expect_doppelganger("SpCVCoords all test sets", coords1)
  vdiffr::expect_doppelganger("SpCVCoords - Fold 1", coords2)
  vdiffr::expect_doppelganger("SpCVCoords - Fold 1-4", coords3)
})

test_that("autoplot works for RepeatedSpCVCoords", {

  # RepeatedSpCVCoords -----------------------------------------------------------------

  set.seed(42)

  task = tsk("diplodia")
  rresa_coords = rsmp("repeated-spcv-coords", folds = 10, repeats = 2)
  rresa_coords$instantiate(task)
  rcoords1 = autoplot(rresa_coords, task)
  rcoords2 = autoplot(rresa_coords, task, repeats_id = 2)
  rcoords3 = autoplot(rresa_coords, task, fold_id = 1)
  rcoords4 = autoplot(rresa_coords, task, fold_id = 1, repeats_id = 2)
  rcoords5 = autoplot(rresa_coords, task, fold_id = c(1, 2, 3, 4))

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

  # SpCVBlock ------------------------------------------------------------------

  set.seed(42)

  task = tsk("ecuador")
  resa_block = rsmp("spcv-block", cols = 4, rows = 4)
  resa_block$instantiate(task)
  block1 = autoplot(resa_block, task)
  block2 = autoplot(resa_block, task, 1)
  block3 = autoplot(resa_block, task, c(1, 2, 3, 4))

  expect_true(is.ggplot(block1))
  expect_true(is.ggplot(block2))
  expect_true(is.ggplot(block3))

  # these error checks apply to all resampling methods.
  expect_error(autoplot(resa_block, task, 20))
  expect_error(autoplot(resa_block, task, c(1, 20)))
  expect_list(autoplot(resa_block, task, c(1, 2, 3, 4), grid = FALSE))

  skip_on_ci()
  vdiffr::expect_doppelganger("SpCVBlock all test sets", block1)
  vdiffr::expect_doppelganger("SpCVBlock - Fold 1", block2)
  vdiffr::expect_doppelganger("SpCVBlock - Fold 1-4", block3)
})

test_that("autoplot works for SpCVBlock", {

  # Repeated SpCVBlock ------------------------------------------------------------------

  set.seed(42)

  task = tsk("ecuador")
  rresa_block = rsmp("repeated-spcv-block", repeats = 2, range = c(500, 1000))
  rresa_block$instantiate(task)
  rblock1 = autoplot(rresa_block, task)
  rblock2 = autoplot(rresa_block, task, repeats_id = 2)
  rblock3 = autoplot(rresa_block, task, fold_id = 1)
  rblock4 = autoplot(rresa_block, task, fold_id = 1, repeats_id = 2)
  rblock5 = autoplot(rresa_block, task, fold_id = c(1, 2, 3, 4))

  expect_true(is.ggplot(rblock1))
  expect_true(is.ggplot(rblock2))
  expect_true(is.ggplot(rblock3))
  expect_true(is.ggplot(rblock4))
  expect_true(is.ggplot(rblock5))

  # these error checks apply to all resampling methods.
  expect_error(autoplot(rresa_block, task, 30))
  expect_error(autoplot(rresa_block, task, c(1, 30)))
  expect_list(autoplot(rresa_block, task, c(1, 2, 3, 4), grid = FALSE))

  skip_on_ci()
  vdiffr::expect_doppelganger("RepSpCVBlock all test sets", rblock1)
  vdiffr::expect_doppelganger("RepSpCVBlock all test sets, Rep 2", rblock2)
  vdiffr::expect_doppelganger("RepSpCVBlock - Fold 1, Rep 2", rblock3)
  vdiffr::expect_doppelganger("RepSpCVBlock - Fold 1, Rep 1", rblock4)
  vdiffr::expect_doppelganger("RepSpCVBlock - Fold 1-4", rblock5)
})

test_that("autoplot works for SpCVBuffer", {

  # SpCVBuffer -----------------------------------------------------------------

  set.seed(42)

  task = tsk("ecuador")
  resa_buffer = rsmp("spcv-buffer")
  resa_buffer$instantiate(task)
  # one does not want to see nrow plots in a grid
  expect_error(autoplot(resa_buffer, task))
  buffer2 = autoplot(resa_buffer, task, 1)
  buffer3 = autoplot(resa_buffer, task, c(1, 2, 3, 4))

  skip_on_ci()
  vdiffr::expect_doppelganger("SpCVBuffer - Fold 1", buffer2)
  vdiffr::expect_doppelganger("SpCVBuffer - Fold 1-4", buffer3)
})

test_that("autoplot works for SpCVEnv", {

  # SpCVEnv --------------------------------------------------------------------

  set.seed(42)

  task = tsk("ecuador")
  resa_env = rsmp("spcv-env")
  resa_env$instantiate(task)
  env1 = autoplot(resa_env, task)
  env2 = autoplot(resa_env, task, 1)
  env3 = autoplot(resa_env, task, c(1, 2, 3, 4))

  skip_on_ci()
  vdiffr::expect_doppelganger("SpCVEnv all test sets", env1)
  vdiffr::expect_doppelganger("SpCVEnv - Fold 1", env2)
  vdiffr::expect_doppelganger("SpCVEnv - Fold 1-4", env3)
})

test_that("autoplot works for RepeatedSpCVEnv", {

  # RepeatedSpCVEnv ------------------------------------------------------------

  set.seed(42)

  task = tsk("ecuador")
  rresa_env = rsmp("repeated-spcv-env", folds = 10, repeats = 2)
  rresa_env$instantiate(task)
  renv1 = autoplot(rresa_env, task)
  renv2 = autoplot(rresa_env, task, repeats_id = 2)
  renv3 = autoplot(rresa_env, task, fold_id = 1)
  renv4 = autoplot(rresa_env, task, fold_id = 1, repeats_id = 2)
  renv5 = autoplot(rresa_env, task, fold_id = c(1, 2, 3, 4))

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
  vdiffr::expect_doppelganger("RepSpCVEnv - Fold 1-4", renv5)
})


test_that("autoplot works for SpCVEnv", {

  # CV -------------------------------------------------------------------------

  set.seed(42)

  task = tsk("ecuador")
  resa_cv = rsmp("cv")
  resa_cv$instantiate(task)
  cv1 = autoplot(resa_cv, task)
  cv2 = autoplot(resa_cv, task, 1)
  cv3 = autoplot(resa_cv, task, c(1, 2, 3, 4))

  skip_on_ci()
  vdiffr::expect_doppelganger("CV all test sets", cv1)
  vdiffr::expect_doppelganger("CV - Fold 1", cv2)
  vdiffr::expect_doppelganger("CV - Fold 1-4", cv3)
})

test_that("autoplot works for RepeatedSpCVEnv", {

  # RepeatedCV -----------------------------------------------------------------

  set.seed(42)

  task = tsk("ecuador")
  rresa_cv = rsmp("repeated_cv", folds = 10, repeats = 2)
  rresa_cv$instantiate(task)
  rcv1 = autoplot(rresa_cv, task)
  rcv2 = autoplot(rresa_cv, task, repeats_id = 2)
  rcv3 = autoplot(rresa_cv, task, fold_id = 1)
  rcv4 = autoplot(rresa_cv, task, fold_id = 1, repeats_id = 2)
  rcv5 = autoplot(rresa_cv, task, fold_id = c(1, 2, 3, 4))

  expect_true(is.ggplot(rcv1))
  expect_true(is.ggplot(rcv2))
  expect_true(is.ggplot(rcv3))
  expect_true(is.ggplot(rcv4))
  expect_true(is.ggplot(rcv5))

  skip_on_ci()
  vdiffr::expect_doppelganger("RepCV all test sets", rcv1)
  vdiffr::expect_doppelganger("RepCV all test sets, Rep 2", rcv2)
  vdiffr::expect_doppelganger("RepCV - Fold 1, Rep 2", rcv3)
  vdiffr::expect_doppelganger("RepCV - Fold 1, Rep 1", rcv4)
  vdiffr::expect_doppelganger("RepCV - Fold 1-4", rcv5)
})
