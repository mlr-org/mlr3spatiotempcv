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
  rcoords2 = autoplot(rresa_coords, task, 1)
  rcoords3 = autoplot(rresa_coords, task, c(1, 2, 3, 4))

  expect_true(is.ggplot(rcoords1))
  expect_true(is.ggplot(rcoords2))
  expect_true(is.ggplot(rcoords3))

  skip_on_ci()
  vdiffr::expect_doppelganger("RepeatedSpCVCoords all test sets", rcoords1)
  vdiffr::expect_doppelganger("RepeatedSpCVCoords - Fold 1", rcoords2)
  vdiffr::expect_doppelganger("RepeatedSpCVCoords - Fold 1-4", rcoords3)
})

test_that("autoplot works for SpCVBlock", {

  # SpCVBlock ------------------------------------------------------------------

  set.seed(42)

  task = tsk("ecuador")
  resa_block = rsmp("spcv-block")
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
  renv2 = autoplot(rresa_env, task, 1)
  renv3 = autoplot(rresa_env, task, c(1, 2, 3, 4))

  expect_true(is.ggplot(renv1))
  expect_true(is.ggplot(renv2))
  expect_true(is.ggplot(renv3))

  skip_on_ci()
  vdiffr::expect_doppelganger("RepeatedSpCVEnv all test sets", renv1)
  vdiffr::expect_doppelganger("RepeatedSpCVEnv - Fold 1", renv2)
  vdiffr::expect_doppelganger("RepeatedSpCVEnv - Fold 1-4", renv3)
})
