# generic tests

test_that("errors are thrown for non-valid argument settings", {
  task = test_make_twoclass_task()
  # type does not matter here
  rsp = rsmp("spcv_coords")
  rsp$instantiate(task)

  # these error checks apply to all resampling methods.
  expect_message(plot(rsp, task))
  expect_error(plot(rsp, task, 30))
  expect_error(plot(rsp, task, c(1, 30)))
  expect_list(plot(rsp, task, c(1, 2), plot_as_grid = FALSE))
})

# cv ---------------------------------------------------------------------------

test_that("plot() works for 'cv'", {
  skip_if_not_installed("vdiffr")
  set.seed(42)

  plots = prepare_autoplot("cv", folds = 4)

  # autoplot() is used instead of plot() to prevent side-effect plotting
  p1 = autoplot(plots$rsp, plots$task, crs = 4326)
  p2 = autoplot(plots$rsp, plots$task, 1, crs = 4326)
  p3 = autoplot(plots$rsp, plots$task, c(1, 2), crs = 4326)

  expect_true(is.ggplot(p1))
  expect_true(is.ggplot(p2))
  expect_true(is.ggplot(p2))

  vdiffr::expect_doppelganger("CV all test sets", p1)
  vdiffr::expect_doppelganger("CV - Fold 1", p2)
  vdiffr::expect_doppelganger("CV - Fold 1-2", p3)
})

test_that("plot() works for 'repeated-cv'", {
  skip_if_not_installed("vdiffr")
  set.seed(42)

  plots = prepare_autoplot("repeated_cv",
    folds = 4, repeats = 2)

  # autoplot() is used instead of plot() to prevent side-effect plotting
  p1 = autoplot(plots$rsp, plots$task, crs = 4326)
  p2 = autoplot(plots$rsp, plots$task, 1, crs = 4326)
  p3 = autoplot(plots$rsp, plots$task, c(1, 2), crs = 4326)

  expect_true(is.ggplot(p1))
  expect_true(is.ggplot(p2))
  expect_true(is.ggplot(p2))

  p4 = autoplot(plots$rsp, plots$task, crs = 4326, repeats_id = 2)
  p5 = autoplot(plots$rsp, plots$task, crs = 4326, fold_id = 1, repeats_id = 2)

  expect_true(is.ggplot(p4))
  expect_true(is.ggplot(p5))

  vdiffr::expect_doppelganger("RepCV all test sets", p1)
  vdiffr::expect_doppelganger("RepCV Fold 1 Rep 1", p2)
  vdiffr::expect_doppelganger("RepCV - Fold 1-2 Rep 1", p3)
  vdiffr::expect_doppelganger("RepCV - Fold 1-2, Rep 2", p4)
  vdiffr::expect_doppelganger("RepCV - Fold 1, Rep 2", p5)
})

# spcv_coords ------------------------------------------------------------------

test_that("plot() works for 'spcv_coords'", {
  skip_if_not_installed("vdiffr")
  set.seed(42)

  plots = prepare_autoplot("spcv_coords", folds = 4)

  # autoplot() is used instead of plot() to prevent side-effect plotting
  p1 = autoplot(plots$rsp, plots$task, crs = 4326)
  p2 = autoplot(plots$rsp, plots$task, 1, crs = 4326)
  p3 = autoplot(plots$rsp, plots$task, c(1, 2), crs = 4326)

  expect_true(is.ggplot(p1))
  expect_true(is.ggplot(p2))
  expect_true(is.ggplot(p2))

  vdiffr::expect_doppelganger("SpCVCoords all test sets", p1)
  vdiffr::expect_doppelganger("SpCVCoords - Fold 1", p2)
  vdiffr::expect_doppelganger("SpCVCoords - Fold 1-2", p3)
})

test_that("plot() works for 'repeated_spcv_coords'", {
  skip_if_not_installed("vdiffr")
  set.seed(42)

  plots = prepare_autoplot("repeated_spcv_coords",
    folds = 4, repeats = 2)

  # autoplot() is used instead of plot() to prevent side-effect plotting
  p1 = autoplot(plots$rsp, plots$task, crs = 4326)
  p2 = autoplot(plots$rsp, plots$task, 1, crs = 4326)
  p3 = autoplot(plots$rsp, plots$task, c(1, 2), crs = 4326)

  expect_true(is.ggplot(p1))
  expect_true(is.ggplot(p2))
  expect_true(is.ggplot(p2))

  p4 = autoplot(plots$rsp, plots$task, crs = 4326, repeats_id = 2)
  p5 = autoplot(plots$rsp, plots$task, crs = 4326, fold_id = 1, repeats_id = 2)

  expect_true(is.ggplot(p4))
  expect_true(is.ggplot(p5))

  vdiffr::expect_doppelganger("RepSpCVCoords all test sets", p1)
  vdiffr::expect_doppelganger("RepSpCVCoords Fold 1 Rep 1", p2)
  vdiffr::expect_doppelganger("RepSpCVCoords - Fold 1-2 Rep 1", p3)
  vdiffr::expect_doppelganger("RepSpCVCoords - Fold 1-2, Rep 2", p4)
  vdiffr::expect_doppelganger("RepSpCVCoords - Fold 1, Rep 2", p5)
})

# spcv_block -------------------------------------------------------------------

test_that("plot() works for 'spcv_block'", {
  skip_if_not_installed("vdiffr")
  set.seed(42)

  plots = prepare_autoplot("spcv_block", cols = 2, rows = 2, folds = 4)

  # autoplot() is used instead of plot() to prevent side-effect plotting
  p1 = autoplot(plots$rsp, plots$task, crs = 4326)
  p2 = autoplot(plots$rsp, plots$task, 1, crs = 4326)
  p3 = autoplot(plots$rsp, plots$task, c(1, 2), crs = 4326)

  expect_true(is.ggplot(p1))
  expect_true(is.ggplot(p2))
  expect_true(is.ggplot(p2))

  vdiffr::expect_doppelganger("SpCVBlock all test sets", p1)
  vdiffr::expect_doppelganger("SpCVBlock - Fold 1", p2)
  vdiffr::expect_doppelganger("SpCVBlock - Fold 1-2", p3)
})

test_that("plot() works for 'repeated_spcv_block'", {
  skip_if_not_installed("vdiffr")
  set.seed(42)

  plots = prepare_autoplot("repeated_spcv_block",
    folds = 4, range = c(2, 4), repeats = 2)

  # autoplot() is used instead of plot() to prevent side-effect plotting
  p1 = autoplot(plots$rsp, plots$task, crs = 4326)
  p2 = autoplot(plots$rsp, plots$task, 1, crs = 4326)
  p3 = autoplot(plots$rsp, plots$task, c(1, 2), crs = 4326)

  expect_true(is.ggplot(p1))
  expect_true(is.ggplot(p2))
  expect_true(is.ggplot(p2))

  p4 = autoplot(plots$rsp, plots$task, crs = 4326, repeats_id = 2)
  p5 = autoplot(plots$rsp, plots$task, crs = 4326, fold_id = 1, repeats_id = 2)

  expect_true(is.ggplot(p4))
  expect_true(is.ggplot(p5))

  vdiffr::expect_doppelganger("RepSpCVBlock all test sets", p1)
  vdiffr::expect_doppelganger("RepSpCVBlock Fold 1 Rep 1", p2)
  vdiffr::expect_doppelganger("RepSpCVBlock - Fold 1-2 Rep 1", p3)
  vdiffr::expect_doppelganger("RepSpCVBlock - Fold 1-2, Rep 2", p4)
  vdiffr::expect_doppelganger("RepSpCVBlock - Fold 1, Rep 2", p5)
})

# spcv_env ---------------------------------------------------------------------

test_that("plot() works for 'spcv_env'", {
  skip_if_not_installed("vdiffr")
  set.seed(42)

  plots = prepare_autoplot("spcv_env", folds = 4)

  # autoplot() is used instead of plot() to prevent side-effect plotting
  p1 = autoplot(plots$rsp, plots$task, crs = 4326)
  p2 = autoplot(plots$rsp, plots$task, 1, crs = 4326)
  p3 = autoplot(plots$rsp, plots$task, c(1, 2), crs = 4326)

  expect_true(is.ggplot(p1))
  expect_true(is.ggplot(p2))
  expect_true(is.ggplot(p2))

  vdiffr::expect_doppelganger("SpCVEnv all test sets", p1)
  vdiffr::expect_doppelganger("SpCVEnv - Fold 1", p2)
  vdiffr::expect_doppelganger("SpCVEnv - Fold 1-2", p3)
})

test_that("plot() works for 'repeated_spcv_env'", {
  skip_if_not_installed("vdiffr")
  set.seed(42)

  plots = prepare_autoplot("repeated_spcv_env",
    folds = 4, repeats = 2)

  # autoplot() is used instead of plot() to prevent side-effect plotting
  p1 = autoplot(plots$rsp, plots$task, crs = 4326)
  p2 = autoplot(plots$rsp, plots$task, 1, crs = 4326)
  p3 = autoplot(plots$rsp, plots$task, c(1, 2), crs = 4326)

  expect_true(is.ggplot(p1))
  expect_true(is.ggplot(p2))
  expect_true(is.ggplot(p2))

  p4 = autoplot(plots$rsp, plots$task, crs = 4326, repeats_id = 2)
  p5 = autoplot(plots$rsp, plots$task, crs = 4326, fold_id = 1, repeats_id = 2)

  expect_true(is.ggplot(p4))
  expect_true(is.ggplot(p5))

  vdiffr::expect_doppelganger("RepSpCVEnv all test sets", p1)
  vdiffr::expect_doppelganger("RepSpCVEnv Fold 1 Rep 1", p2)
  vdiffr::expect_doppelganger("RepSpCVEnv - Fold 1-2 Rep 1", p3)
  vdiffr::expect_doppelganger("RepSpCVEnv - Fold 1-2, Rep 2", p4)
  vdiffr::expect_doppelganger("RepSpCVEnv - Fold 1, Rep 2", p5)
})

# sptcv_cstf -------------------------------------------------------------------

test_that("plot() works for 'sptcv_cstf'", {
  skip_if_not_installed("vdiffr")
  set.seed(42)

  task = tsk("cookfarm")
  rsp = rsmp("sptcv_cstf", folds = 4, time_var = "Date")
  rsp$instantiate(task)

  p1 = plot(rsp, task = task, crs = 4326)
  p2 = plot(rsp, task, 1, crs = 4326)
  # plot() would force image printing here
  p3 = autoplot(rsp, task, c(1, 2), crs = 4326)

  expect_s3_class(p1, "plotly")
  expect_s3_class(p2, "plotly")
  expect_list(p3)

  vdiffr::expect_doppelganger("SptCVCstf all test sets", p1)
  vdiffr::expect_doppelganger("SptCVCstf - Fold 1", p2)
  vdiffr::expect_doppelganger("SptCVCstf - Fold 1-2", p3)
})

test_that("plot() works for 'repeated_spcv_cstf'", {
  skip_if_not_installed("vdiffr")
  set.seed(42)

  task = tsk("cookfarm")
  rsp = rsmp("repeated_sptcv_cstf", folds = 4, repeats = 2, time_var = "Date")
  rsp$instantiate(task)

  p1 = autoplot(rsp, task = task, crs = 4326)
  p2 = autoplot(rsp, task, 1) # missing on purpose for codecov reasons
  # plot() would force image printing here
  p3 = autoplot(rsp, task, c(1, 2), crs = 4326)
  p4 = autoplot(rsp, task, c(1, 2), crs = 4326, plot_as_grid = FALSE)

  expect_s3_class(p1, "plotly")
  expect_s3_class(p2, "plotly")
  expect_list(p3)

  p4 = autoplot(rsp, task, repeats_id = 2, crs = 4326)
  p5 = autoplot(rsp, task, fold_id = 1, repeats_id = 2, crs = 4326)

  expect_s3_class(p4, "plotly")
  expect_s3_class(p5, "plotly")

  vdiffr::expect_doppelganger("RepSptCVCstf all test sets", p1)
  vdiffr::expect_doppelganger("RepSptCVCstf Fold 1 Rep 1", p2)
  vdiffr::expect_doppelganger("RepSptCVCstf - Fold 1-2 Rep 1", p3)
  vdiffr::expect_doppelganger("RepSptCVCstf - Fold 1-2, Rep 2", p4)
  vdiffr::expect_doppelganger("RepSptCVCstf - Fold 1, Rep 2", p5)
})
