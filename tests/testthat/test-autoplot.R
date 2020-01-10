context("autoplot")

set.seed(42)

test_that("autoplot works for SpCVCoords", {

  # SpCVCoords -----------------------------------------------------------------

  resa_coords = mlr_resamplings$get("spcv-coords")
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

test_that("autoplot works for SpCVBlock", {

  # SpCVBlock ------------------------------------------------------------------

  resa_block = mlr_resamplings$get("spcv-block")
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

  resa_buffer = mlr_resamplings$get("spcv-buffer")
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

  resa_Env = mlr_resamplings$get("spcv-env")
  resa_Env$instantiate(task)
  env1 = autoplot(resa_Env, task)
  env2 = autoplot(resa_Env, task, 1)
  env3 = autoplot(resa_Env, task, c(1, 2, 3, 4))

  skip_on_ci()
  vdiffr::expect_doppelganger("SpCVEnv all test sets", env1)
  vdiffr::expect_doppelganger("SpCVEnv - Fold 1", env2)
  vdiffr::expect_doppelganger("SpCVEnv - Fold 1-4", env3)
})
