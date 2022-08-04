# generic tests

test_that("errors are thrown for non-valid argument settings", {
  skip_if_not_installed("sf")
  skip_if_not_installed("patchwork")
  skip_if_not_installed("ggtext")
  task = test_make_twoclass_task()
  # type does not matter here
  rsp = rsmp("spcv_coords")
  rsp$instantiate(task)

  # these error checks apply to all resampling methods.
  expect_error(plot(rsp, task, 30))
  expect_error(plot(rsp, task, c(1, 30)))
  expect_list(autoplot(rsp, task, c(1, 2), plot_as_grid = FALSE))
})

# cv ---------------------------------------------------------------------------

test_that("plot() works for 'cv'", {
  skip_if_not_installed("vdiffr")
  set.seed(42)

  plots = prepare_autoplot("cv", folds = 4)

  # autoplot() is used instead of plot() to prevent side-effect plotting
  p1 = autoplot(plots$rsp, plots$task)
  p2 = autoplot(plots$rsp, plots$task, 1)
  p3 = autoplot(plots$rsp, plots$task, c(1, 2))

  ### sample_fold_n
  p7 = autoplot(plots$rsp, plots$task, sample_fold_n = 3L)
  p8 = autoplot(plots$rsp, plots$task, fold_id = 1, sample_fold_n = 3L)
  p9 = autoplot(plots$rsp, plots$task, fold_id = c(1, 2), sample_fold_n = 3L)

  expect_true(is.ggplot(p1))
  expect_true(is.ggplot(p2))
  expect_true(is.ggplot(p2))

  vdiffr::expect_doppelganger("CV all test sets", p1)
  vdiffr::expect_doppelganger("CV - Fold 1", p2)
  vdiffr::expect_doppelganger("CV - Fold 1-2", p3)

  ### sample_fold_n
  vdiffr::expect_doppelganger("CV - sample_fold_n", p7)
  vdiffr::expect_doppelganger("CV - Fold 1 - sample_fold_n", p8)
  vdiffr::expect_doppelganger("CV - Fold 1-2 - sample_fold_n", p9)
})

test_that("plot() works for 'repeated-cv'", {
  skip_if_not_installed("vdiffr")
  set.seed(42)

  plots = prepare_autoplot("repeated_cv",
    folds = 4, repeats = 2)

  # autoplot() is used instead of plot() to prevent side-effect plotting
  p1 = autoplot(plots$rsp, plots$task)
  p2 = autoplot(plots$rsp, plots$task, 1)
  p3 = autoplot(plots$rsp, plots$task, c(1, 2))

  expect_true(is.ggplot(p1))
  expect_true(is.ggplot(p2))
  expect_true(is.ggplot(p2))

  p4 = autoplot(plots$rsp, plots$task, repeats_id = 2)
  p5 = autoplot(plots$rsp, plots$task, fold_id = 1, repeats_id = 2)

  expect_true(is.ggplot(p4))
  expect_true(is.ggplot(p5))

  vdiffr::expect_doppelganger("RepCV all test sets", p1)
  vdiffr::expect_doppelganger("RepCV Fold 1 Rep 1", p2)
  vdiffr::expect_doppelganger("RepCV - Fold 1-2 Rep 1", p3)
  vdiffr::expect_doppelganger("RepCV - Fold 1-2, Rep 2", p4)
  vdiffr::expect_doppelganger("RepCV - Fold 1, Rep 2", p5)
})

test_that("plot() works for 'cv' with 'groups' col role", {
  skip_if_not_installed("vdiffr")
  set.seed(42)

  # create dataset with blocking from example dataset
  task = tsk("ecuador")
  data_raw = task$backend$data(1:task$nrow, task$feature_names)
  group = as.factor(sample(c("class1", "class2", "class3", "class4", "class5",
    "class6", "class7", "class8"),
  task$nrow, replace = TRUE))
  task$cbind(data.table::data.table("group" = group))
  task$set_col_roles("group", roles = "group")

  cv = rsmp("cv", folds = 4)$instantiate(task)

  p1 = autoplot(cv, task)
  p2 = autoplot(cv, task, fold_id = 1)
  p3 = autoplot(cv, task, fold_id = c(1, 2))

  vdiffr::expect_doppelganger("CV - All test sets - groups col role", p1)
  vdiffr::expect_doppelganger("CV - Fold 1 - groups col role", p2)
  vdiffr::expect_doppelganger("CV - Fold 1,2 - groups col role", p3)
})

# custom cv --------------------------------------------------------------------

test_that("plot() works for 'custom_cv'", {
  skip_if_not_installed("vdiffr")
  set.seed(42)

  task = tsk("ecuador")
  breaks = quantile(task$data()$dem, seq(0, 1, length = 6))
  zclass = cut(task$data()$dem, breaks, include.lowest = TRUE)

  rsp = rsmp("custom_cv")
  rsp$instantiate(task, f = zclass)

  # autoplot() is used instead of plot() to prevent side-effect plotting
  p1 = autoplot(rsp, task)
  p2 = autoplot(rsp, task, 1)
  p3 = autoplot(rsp, task, c(1, 2))

  ### sample_fold_n
  p7 = autoplot(rsp, task, sample_fold_n = 3L)
  p8 = autoplot(rsp, task, fold_id = 1, sample_fold_n = 3L)
  p9 = autoplot(rsp, task, fold_id = c(1, 2), sample_fold_n = 3L)

  expect_true(is.ggplot(p1))
  expect_true(is.ggplot(p2))
  expect_true(is.ggplot(p3))

  vdiffr::expect_doppelganger("Custom-CV all test sets", p1)
  vdiffr::expect_doppelganger("Custom-CV - Fold 1", p2)
  vdiffr::expect_doppelganger("Custom-CV - Fold 1-2", p3)

  ### sample_fold_n
  vdiffr::expect_doppelganger("Custom-CV - sample_fold_n", p7)
  vdiffr::expect_doppelganger("Custom-CV - Fold 1 - sample_fold_n", p8)
  vdiffr::expect_doppelganger("Custom-CV - Fold 1-2 - sample_fold_n", p9)
})

# spcv_coords ------------------------------------------------------------------

test_that("plot() works for 'spcv_coords'", {
  skip_if_not_installed("vdiffr")
  set.seed(42)

  plots = prepare_autoplot("spcv_coords", folds = 4)

  # autoplot() is used instead of plot() to prevent side-effect plotting
  p1 = autoplot(plots$rsp, plots$task)
  p2 = autoplot(plots$rsp, plots$task, 1)
  p3 = autoplot(plots$rsp, plots$task, c(1, 2))

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
  p1 = autoplot(plots$rsp, plots$task)
  p2 = autoplot(plots$rsp, plots$task, 1)
  p3 = autoplot(plots$rsp, plots$task, c(1, 2))

  expect_true(is.ggplot(p1))
  expect_true(is.ggplot(p2))
  expect_true(is.ggplot(p2))

  p4 = autoplot(plots$rsp, plots$task, repeats_id = 2)
  p5 = autoplot(plots$rsp, plots$task, fold_id = 1, repeats_id = 2)

  ### sample_fold_n
  p7 = autoplot(plots$rsp, plots$task, sample_fold_n = 3L)
  p8 = autoplot(plots$rsp, plots$task, fold_id = 1, sample_fold_n = 3L)
  p9 = autoplot(plots$rsp, plots$task, fold_id = c(1, 2), sample_fold_n = 3L)

  expect_true(is.ggplot(p4))
  expect_true(is.ggplot(p5))

  vdiffr::expect_doppelganger("RepSpCVCoords all test sets", p1)
  vdiffr::expect_doppelganger("RepSpCVCoords Fold 1 Rep 1", p2)
  vdiffr::expect_doppelganger("RepSpCVCoords - Fold 1-2 Rep 1", p3)
  vdiffr::expect_doppelganger("RepSpCVCoords - Fold 1-2, Rep 2", p4)
  vdiffr::expect_doppelganger("RepSpCVCoords - Fold 1, Rep 2", p5)

  ### sample_fold_n
  vdiffr::expect_doppelganger("RepSpCVCoords - sample_fold_n", p7)
  vdiffr::expect_doppelganger("RepSpCVCoords - Fold 1 - sample_fold_n", p8)
  vdiffr::expect_doppelganger("RepSpCVCoords - Fold 1-2 - sample_fold_n", p9)
})

# spcv_block -------------------------------------------------------------------

test_that("plot() works for 'spcv_block'", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("blockCV")
  set.seed(42)

  plots = prepare_autoplot("spcv_block", cols = 2, rows = 2, folds = 4)

  # autoplot() is used instead of plot() to prevent side-effect plotting
  p1 = autoplot(plots$rsp, plots$task)
  p2 = autoplot(plots$rsp, plots$task, 1)
  p3 = autoplot(plots$rsp, plots$task, c(1, 2))

  ### sample_fold_n
  p7 = autoplot(plots$rsp, plots$task, sample_fold_n = 3L)
  p8 = autoplot(plots$rsp, plots$task, fold_id = 1, sample_fold_n = 3L)
  p9 = autoplot(plots$rsp, plots$task, fold_id = c(1, 2), sample_fold_n = 3L)

  expect_true(is.ggplot(p1))
  expect_true(is.ggplot(p2))
  expect_true(is.ggplot(p2))

  vdiffr::expect_doppelganger("SpCVBlock all test sets", p1)
  vdiffr::expect_doppelganger("SpCVBlock - Fold 1", p2)
  vdiffr::expect_doppelganger("SpCVBlock - Fold 1-2", p3)

  ### sample_fold_n
  vdiffr::expect_doppelganger("SpCVBlock - sample_fold_n", p7)
  vdiffr::expect_doppelganger("SpCVBlock - Fold 1 - sample_fold_n", p8)
  vdiffr::expect_doppelganger("SpCVBlock - Fold 1-2 - sample_fold_n", p9)
})

test_that("plot() works for 'repeated_spcv_block'", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("blockCV")
  set.seed(42)

  plots = prepare_autoplot("repeated_spcv_block",
    folds = 4, range = c(2L, 4L), repeats = 2)

  # autoplot() is used instead of plot() to prevent side-effect plotting
  p1 = autoplot(plots$rsp, plots$task)
  p2 = autoplot(plots$rsp, plots$task, 1)
  p3 = autoplot(plots$rsp, plots$task, c(1, 2))

  expect_true(is.ggplot(p1))
  expect_true(is.ggplot(p2))
  expect_true(is.ggplot(p2))

  p4 = autoplot(plots$rsp, plots$task, repeats_id = 2)
  p5 = autoplot(plots$rsp, plots$task, fold_id = 1, repeats_id = 2)

  expect_true(is.ggplot(p4))
  expect_true(is.ggplot(p5))

  vdiffr::expect_doppelganger("RepSpCVBlock all test sets", p1)
  vdiffr::expect_doppelganger("RepSpCVBlock Fold 1 Rep 1", p2)
  vdiffr::expect_doppelganger("RepSpCVBlock - Fold 1-2 Rep 1", p3)
  vdiffr::expect_doppelganger("RepSpCVBlock - Fold 1-2, Rep 2", p4)
  vdiffr::expect_doppelganger("RepSpCVBlock - Fold 1, Rep 2", p5)
})

test_that("autplot blockCV shows correct blocks", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("blockCV")
  # file size of resulting svg is too large
  skip_on_cran()

  # otherwise some points get removed and trigger warnings
  sf::sf_use_s2(use_s2 = FALSE)

  task = test_make_blockCV_test_task()

  set.seed(42)
  rsmp = rsmp("spcv_block",
    folds = 5,
    rows = 3,
    cols = 4)
  rsmp$instantiate(task)

  p1 = autoplot(rsmp, task,
    fold_id = 1, size = 0.7,
    show_blocks = TRUE)

  vdiffr::expect_doppelganger(
    "autoplot show_blocks = TRUE show_labels = TRUE",
    p1)
})

test_that("autplot blockCV shows correct blocks for repeated_cv", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("blockCV")
  task = test_make_blockCV_test_task()

  # geom_sf() is causing troubles with vdiffr
  # https://github.com/r-lib/vdiffr/issues/63
  skip_on_ci()

  # otherwise some points get removed and trigger warnings
  sf::sf_use_s2(use_s2 = FALSE)

  set.seed(42)
  rsmp = rsmp("repeated_spcv_block",
    folds = 5,
    repeats = 2,
    rows = 3,
    cols = 4)
  rsmp$instantiate(task)

  p1 = autoplot(rsmp, task,
    fold_id = 1, size = 0.7, repeats_id = 2,
    show_blocks = TRUE, show_labels = TRUE)

  vdiffr::expect_doppelganger(
    "autoplot repeated show_blocks = TRUE show_labels = TRUE",
    p1)
})

# spcv_env ---------------------------------------------------------------------

test_that("plot() works for 'spcv_env'", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("blockCV")
  set.seed(42)

  plots = prepare_autoplot("spcv_env", folds = 4)

  # autoplot() is used instead of plot() to prevent side-effect plotting
  p1 = autoplot(plots$rsp, plots$task)
  p2 = autoplot(plots$rsp, plots$task, 1)
  p3 = autoplot(plots$rsp, plots$task, c(1, 2))

  ### sample_fold_n
  p7 = autoplot(plots$rsp, plots$task, sample_fold_n = 2L)
  p8 = autoplot(plots$rsp, plots$task, fold_id = 1, sample_fold_n = 2L)
  p9 = autoplot(plots$rsp, plots$task, fold_id = c(1, 2), sample_fold_n = 2L)

  expect_true(is.ggplot(p1))
  expect_true(is.ggplot(p2))
  expect_true(is.ggplot(p2))

  vdiffr::expect_doppelganger("SpCVEnv all test sets", p1)
  vdiffr::expect_doppelganger("SpCVEnv - Fold 1", p2)
  vdiffr::expect_doppelganger("SpCVEnv - Fold 1-2", p3)

  ### sample_fold_n
  vdiffr::expect_doppelganger("SpCVEnv - sample_fold_n", p7)
  vdiffr::expect_doppelganger("SpCVEnv - Fold 1 - sample_fold_n", p8)
  vdiffr::expect_doppelganger("SpCVEnv - Fold 1-2 - sample_fold_n", p9)
})

test_that("plot() works for 'repeated_spcv_env'", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("blockCV")

  set.seed(42)

  plots = prepare_autoplot("repeated_spcv_env",
    folds = 4, repeats = 2)

  # autoplot() is used instead of plot() to prevent side-effect plotting
  p1 = autoplot(plots$rsp, plots$task)
  p2 = autoplot(plots$rsp, plots$task, 1)
  p3 = autoplot(plots$rsp, plots$task, c(1, 2))

  expect_true(is.ggplot(p1))
  expect_true(is.ggplot(p2))
  expect_true(is.ggplot(p2))

  p4 = autoplot(plots$rsp, plots$task, repeats_id = 2)
  p5 = autoplot(plots$rsp, plots$task, fold_id = 1, repeats_id = 2)

  expect_true(is.ggplot(p4))
  expect_true(is.ggplot(p5))

  vdiffr::expect_doppelganger("RepSpCVEnv all test sets", p1)
  vdiffr::expect_doppelganger("RepSpCVEnv Fold 1 Rep 1", p2)
  vdiffr::expect_doppelganger("RepSpCVEnv - Fold 1-2 Rep 1", p3)
  vdiffr::expect_doppelganger("RepSpCVEnv - Fold 1-2, Rep 2", p4)
  vdiffr::expect_doppelganger("RepSpCVEnv - Fold 1, Rep 2", p5)
})
