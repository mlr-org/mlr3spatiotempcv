context("autoplot")

test_that("autoplot works for SpCVCoords", {

  # SpCVCoords -------------------------------------------------------------------

  resa_coords = mlr_resamplings$get("spcv-coords")
  resa_coords$instantiate(task)
  coords1 = autoplot(resa_coords, task)
  coords2 = autoplot(resa_coords, task, 1)
  coords3 = autoplot(resa_coords, task, c(1, 2, 3, 4))

  vdiffr::expect_doppelganger("SpCVCoords all test sets", coords1)
  vdiffr::expect_doppelganger("SpCVCoords - Fold 1", coords2)
  vdiffr::expect_doppelganger("SpCVCoords - Fold 1-4", coords3)
})


test_that("autoplot works for SpCVBlock", {
  # SpCVBlock --------------------------------------------------------------------

  resa_block = mlr_resamplings$get("spcv-block")
  resa_block$instantiate(task)
  Block1 = autoplot(resa_block, task)
  Block2 = autoplot(resa_block, task, 1)
  Block3 = autoplot(resa_block, task, c(1, 2, 3, 4))

  vdiffr::expect_doppelganger("SpCVBlock all test sets", Block1)
  vdiffr::expect_doppelganger("SpCVBlock - Fold 1", Block2)
  vdiffr::expect_doppelganger("SpCVBlock - Fold 1-4", Block3)

  # these checks apply to all resampling methods.
  expect_error(autoplot(resa_block, task, 20))
  expect_error(autoplot(resa_block, task, c(1, 20)))
  expect_list(autoplot(resa_block, task, c(1, 2, 3, 4), grid = FALSE))
})


test_that("autoplot works for SpCVBuffer", {
  # SpCVBuffer -------------------------------------------------------------------
  resa_buffer = mlr_resamplings$get("spcv-buffer")
  resa_buffer$instantiate(task)
  # one does not want to see nrow plots in a grid
  expect_error(autoplot(resa_buffer, task))
  Buffer2 = autoplot(resa_buffer, task, 1)
  Buffer3 = autoplot(resa_buffer, task, c(1, 2, 3, 4))

  ## The following tests never finish with vdiffr::manage_cases()
  ## might be related to https://github.com/r-lib/vdiffr#debugging
  vdiffr::expect_doppelganger("SpCVBuffer - Fold 1", Buffer2)
  vdiffr::expect_doppelganger("SpCVBuffer - Fold 1-4", Buffer3)
})

test_that("autoplot works for SpCVEnv", {
  # SpCVEnv ----------------------------------------------------------------------

  resa_Env = mlr_resamplings$get("spcv-env")
  resa_Env$instantiate(task)
  Env1 = autoplot(resa_Env, task)
  Env2 = autoplot(resa_Env, task, 1)
  Env3 = autoplot(resa_Env, task, c(1, 2, 3, 4))

  vdiffr::expect_doppelganger("SpCVEnv all test sets", Env1)
  vdiffr::expect_doppelganger("SpCVEnv - Fold 1", Env2)
  vdiffr::expect_doppelganger("SpCVEnv - Fold 1-4", Env3)
})
