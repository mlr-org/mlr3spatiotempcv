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

  resampling = rsmp("custom_cv")
  resampling$instantiate(task, f = zclass)

  # autoplot() is used instead of plot() to prevent side-effect plotting
  p1 = autoplot(resampling, task)
  p2 = autoplot(resampling, task, 1)
  p3 = autoplot(resampling, task, c(1, 2))

  expect_true(is.ggplot(p1))
  expect_true(is.ggplot(p2))
  expect_true(is.ggplot(p3))

  vdiffr::expect_doppelganger("Custom-CV all test sets", p1)
  vdiffr::expect_doppelganger("Custom-CV - Fold 1", p2)
  vdiffr::expect_doppelganger("Custom-CV - Fold 1-2", p3)
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
  skip_if_not_installed("blockCV")
  set.seed(42)

  plots = prepare_autoplot("spcv_block", cols = 2, rows = 2, folds = 4)

  # autoplot() is used instead of plot() to prevent side-effect plotting
  p1 = autoplot(plots$rsp, plots$task)
  p2 = autoplot(plots$rsp, plots$task, 1)
  p3 = autoplot(plots$rsp, plots$task, c(1, 2))

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

# sptcv_cstf -------------------------------------------------------------------

# 2D ---------------------------------------------------------------------------
test_that("plot() works for 'sptcv_cstf' 2D - time_var", {
  skip_if_not_installed("vdiffr")

  # for some reason linux and windows svgs differ
  skip_on_os("linux")
  skip_on_os("windows")

  tsk_cookfarm_sub$set_col_roles("Date", roles = "time")
  tsk_cookfarm_sub$col_roles$space = character()
  rsp = rsmp("sptcv_cstf", folds = 4)
  rsp$instantiate(tsk_cookfarm_sub)

  p1 = autoplot(rsp, task = tsk_cookfarm_sub)
  p2 = autoplot(rsp, tsk_cookfarm_sub, 1, plot3D = FALSE)
  # plot() would force image printing here
  p3 = suppressMessages(autoplot(rsp, tsk_cookfarm_sub, c(1, 2)))

  expect_true(is.ggplot(p1))
  expect_true(is.ggplot(p2))
  expect_list(p3)

  p4 = autoplot(rsp, tsk_cookfarm_sub, repeats_id = 2)
  p5 = autoplot(rsp, tsk_cookfarm_sub, fold_id = 1, repeats_id = 2)

  vdiffr::expect_doppelganger("SptCVCstf 2D time_var all test sets", p1)
  vdiffr::expect_doppelganger("SptCVCstf 2D time_var - Fold 1", p2)
  vdiffr::expect_doppelganger("SptCVCstf 2D time_var - Fold 1-2", p3)
  vdiffr::expect_doppelganger("RepSptCVCstf 2D time_var - Fold 1-2, Rep 2", p4)
  vdiffr::expect_doppelganger("RepSptCVCstf 2D time_var - Fold 1, Rep 2", p5)
})

test_that("plot() works for 'sptcv_cstf' 2D - space_var", {
  skip_if_not_installed("vdiffr")

  # for some reason linux and windows svgs differ
  skip_on_os("linux")
  skip_on_os("windows")

  tsk_cookfarm_sub$col_roles$time = character()
  tsk_cookfarm_sub$set_col_roles("SOURCEID", "space")
  rsp = rsmp("sptcv_cstf", folds = 4)
  rsp$instantiate(tsk_cookfarm_sub)

  p1 = autoplot(rsp, task = tsk_cookfarm_sub)
  p2 = autoplot(rsp, tsk_cookfarm_sub, 1, plot3D = FALSE)
  # plot() would force image printing here
  p3 = autoplot(rsp, tsk_cookfarm_sub, c(1, 2))

  expect_true(is.ggplot(p1))
  expect_true(is.ggplot(p2))
  expect_list(p3)

  p4 = autoplot(rsp, tsk_cookfarm_sub, repeats_id = 2)
  p5 = autoplot(rsp, tsk_cookfarm_sub, fold_id = 1, repeats_id = 2)

  vdiffr::expect_doppelganger("SptCVCstf 2D space_var all test sets", p1)
  vdiffr::expect_doppelganger("SptCVCstf 2D space_var - Fold 1", p2)
  vdiffr::expect_doppelganger("SptCVCstf 2D space_var - Fold 1-2", p3)
  vdiffr::expect_doppelganger("RepSptCVCstf 2D space_var - Fold 1-2, Rep 2", p4)
  vdiffr::expect_doppelganger("RepSptCVCstf 2D space_var - Fold 1, Rep 2", p5)
})

# 3D ---------------------------------------------------------------------------

test_that("plot() works for 'sptcv_cstf'", {
  skip_if_not_installed("vdiffr")

  # only use Date for plotting, not for partitioning
  tsk_cookfarm_sub$set_col_roles("Date", roles = "plot")
  tsk_cookfarm_sub$col_roles$time = character()
  rsp = rsmp("sptcv_cstf", folds = 4)
  rsp$instantiate(tsk_cookfarm_sub)

  expect_error(autoplot(rsp, task = tsk_cookfarm_sub, plot3D = TRUE))
  p2 = suppressWarnings(
    autoplot(rsp, tsk_cookfarm_sub, 1, plot3D = TRUE)
  )
  # plot() would force image printing here
  p3 = suppressWarnings(
    suppressMessages(autoplot(rsp, tsk_cookfarm_sub, c(1, 2), plot3D = TRUE))
  )

  # test error if neither time nor plot col roles are defined
  # only use Date for plotting, not for partitioning
  tsk_cookfarm_sub$col_roles$time = character()
  tsk_cookfarm_sub$col_roles$plot = character()
  expect_error(autoplot(rsp, tsk_cookfarm_sub, 1, plot3D = TRUE))

  expect_s3_class(p2, "plotly")
  expect_list(p3)

  # vdiffr::expect_doppelganger("SptCVCstf all test sets", p1)
  suppressWarnings(vdiffr::expect_doppelganger("SptCVCstf - Fold 1", p2))
  suppressWarnings(vdiffr::expect_doppelganger("SptCVCstf - Fold 1-2", p3))
})

test_that("plot() works for 'repeated_spcv_cstf'", {
  skip_if_not_installed("vdiffr")

  tsk_cookfarm_sub$col_roles$time = "Date"
  tsk_cookfarm_sub$col_roles$space = "SOURCEID"
  rsp = rsmp("repeated_sptcv_cstf", folds = 4, repeats = 2)
  rsp$instantiate(tsk_cookfarm_sub)

  expect_error(autoplot(rsp, task = tsk_cookfarm_sub, plot3D = TRUE))
  p2 = suppressWarnings(
    autoplot(rsp, tsk_cookfarm_sub, 1,
      show_omitted = TRUE, plot3D = TRUE,
      repeats_id = 2) # missing on purpose for codecov reasons
  )
  p3 = suppressWarnings(
    suppressMessages(autoplot(rsp, tsk_cookfarm_sub, c(1, 2), plot3D = TRUE))
  )
  p4 = autoplot(rsp, tsk_cookfarm_sub, c(1, 2),
    crs = 4326, plot_as_grid = FALSE,
    repeats_id = 2,
    plot3D = TRUE)

  expect_s3_class(p2, "plotly")
  expect_list(p3)

  p5 = suppressWarnings(
    autoplot(rsp, tsk_cookfarm_sub,
      fold_id = 1, repeats_id = 2,
      plot3D = TRUE)
  )

  expect_s3_class(p5, "plotly")

  # vdiffr::expect_doppelganger("RepSptCVCstf all test sets", p1)
  suppressWarnings(vdiffr::expect_doppelganger("RepSptCVCstf Fold 1 Rep 1", p2))
  suppressWarnings(vdiffr::expect_doppelganger("RepSptCVCstf - Fold 1-2 Rep 1", p3))
  suppressWarnings(vdiffr::expect_doppelganger("RepSptCVCstf - Fold 1-2, Rep 2", p4))
  suppressWarnings(vdiffr::expect_doppelganger("RepSptCVCstf - Fold 1, Rep 2", p5))
})

test_that("autoplot time + space", {
  skip_if_not_installed("sf")
  skip_if_not_installed("patchwork")
  skip_if_not_installed("ggtext")

  tsk_cookfarm_sub$set_col_roles("Date", roles = "time")
  tsk_cookfarm_sub$set_col_roles("SOURCEID", roles = "space")
  rsp = rsmp("sptcv_cstf", folds = 4)
  rsp$instantiate(tsk_cookfarm_sub)

  # without omitted, we have no values on the y-axis and the plot is not shown
  p1 = suppressWarnings(
    autoplot(rsp, tsk_cookfarm_sub, fold_id = 4, show_omitted = TRUE, plot3D = TRUE)
  )

  suppressWarnings(vdiffr::expect_doppelganger("SptCVCstf - + omitted points, 3D", p1))
})

# spcv_disc --------------------------------------------------------------------

test_that("plot() works for 'spcv_disc'", {
  skip_if_not_installed("vdiffr")

  # geom_sf() is causing troubles with vdiffr
  # https://github.com/r-lib/vdiffr/issues/63
  skip_on_ci()

  set.seed(42)

  task = tsk("ecuador")
  rsp = rsmp("repeated_spcv_disc",
    folds = 4, radius = 200, buffer = 200,
    repeats = 2)
  rsp$instantiate(task)

  p1 = autoplot(rsp, task = task)
  p2 = autoplot(rsp, task, 1)
  # plot() would force image printing here
  p3 = autoplot(rsp, task, c(1, 2))

  p4 = suppressWarnings(
    autoplot(rsp, task, 1, show_omitted = TRUE)
  )

  expect_true(is.ggplot(p1))
  expect_true(is.ggplot(p2))
  expect_list(p3)
  expect_true(is.ggplot(p4))

  p5 = autoplot(rsp, task, repeats_id = 2)
  p6 = autoplot(rsp, task, fold_id = 1, repeats_id = 2)

  vdiffr::expect_doppelganger("SpCVDisc all test sets", p1)
  vdiffr::expect_doppelganger("SpCVDisc Fold 1", p2)
  vdiffr::expect_doppelganger("SpCVDisc Fold 1-2", p3)
  vdiffr::expect_doppelganger("SpCVDisc show_omitted", p4)

  vdiffr::expect_doppelganger("RepSpCVDisc - Fold 1-2, Rep 2", p5)
  vdiffr::expect_doppelganger("RepSpCVDisc - Fold 1, Rep 2", p6)
})

# spcv_tiles --------------------------------------------------------------------

test_that("plot() works for 'spcv_tiles'", {
  skip_if_not_installed("vdiffr")

  # geom_sf() is causing troubles with vdiffr
  # https://github.com/r-lib/vdiffr/issues/63
  skip_on_ci()

  set.seed(42)

  task = tsk("ecuador")
  rsp = rsmp("repeated_spcv_tiles",
    nsplit = c(4L, 3L), min_n = 50, reassign = FALSE,
    repeats = 2)
  rsp$instantiate(task)

  p1 = autoplot(rsp, task = task)
  p2 = autoplot(rsp, task, 1)
  # plot() would force image printing here
  p3 = autoplot(rsp, task, c(1, 2))

  p4 = suppressWarnings(
    autoplot(rsp, task, 1, show_omitted = TRUE)
  )

  expect_true(is.ggplot(p1))
  expect_true(is.ggplot(p2))
  expect_list(p3)
  expect_true(is.ggplot(p4))

  p5 = autoplot(rsp, task, c(1, 2), repeats_id = 2)
  p6 = autoplot(rsp, task, fold_id = 1, repeats_id = 2)

  vdiffr::expect_doppelganger("SpCVTiles all test sets", p1)
  vdiffr::expect_doppelganger("SpCVTiles Fold 1", p2)
  vdiffr::expect_doppelganger("SpCVTiles Fold 1-2", p3)
  vdiffr::expect_doppelganger("SpCVTiles show_omitted", p4)

  vdiffr::expect_doppelganger("RepSpCVTiles - Fold 1-2, Rep 2", p5)
  vdiffr::expect_doppelganger("RepSpCVTiles - Fold 1, Rep 2", p6)
})
