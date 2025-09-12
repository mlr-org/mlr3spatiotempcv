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

  p1 = autoplot(rsp, task = tsk_cookfarm_sub, sample_fold_n = 3L)
  p2 = autoplot(rsp, tsk_cookfarm_sub, 1, plot3D = FALSE, sample_fold_n = 3L)
  # plot() would force image printing here
  p3 = suppressMessages(
    autoplot(rsp, tsk_cookfarm_sub, fold_id = c(1, 2),
      sample_fold_n = 3L)
  )

  expect_true(is_ggplot(p1))
  expect_true(is_ggplot(p2))
  expect_s3_class(p3, "patchwork")

  p4 = autoplot(rsp, tsk_cookfarm_sub, repeats_id = 2, sample_fold_n = 3L)
  p5 = autoplot(rsp, tsk_cookfarm_sub, fold_id = 1, repeats_id = 2,
    sample_fold_n = 3L)

  ### sample_fold_nj
  p7 = autoplot(rsp, tsk_cookfarm_sub, sample_fold_n = 3L)
  p8 = autoplot(rsp, tsk_cookfarm_sub, fold_id = 1, sample_fold_n = 3L)
  p9 = autoplot(rsp, tsk_cookfarm_sub, fold_id = c(1, 2), sample_fold_n = 3L)

  vdiffr::expect_doppelganger("SptCVCstf 2D time_var all test sets", p1)
  vdiffr::expect_doppelganger("SptCVCstf 2D time_var - Fold 1", p2)
  vdiffr::expect_doppelganger("SptCVCstf 2D time_var - Fold 1-2", p3)
  vdiffr::expect_doppelganger("SptCVCstf 2D time_var - Fold 1-2, Rep 2", p4)
  vdiffr::expect_doppelganger("SptCVCstf 2D time_var - Fold 1, Rep 2", p5)

  ### sample_fold_n
  vdiffr::expect_doppelganger("SptCVCstf 2D time_var - sample_fold_n", p7)
  vdiffr::expect_doppelganger("SptCVCstf 2D time_var - Fold 1 - sample_fold_n", p8)
  vdiffr::expect_doppelganger("SptCVCstf 2D time_var - Fold 1-2 - sample_fold_n", p9)

})

test_that("plot() works for 'sptcv_cstf' 2D - space_var", {
  skip_if_not_installed("vdiffr")

  # for some reason linux and windows svgs differ
  skip_on_os("linux")
  skip_on_os("windows")

  tsk_cookfarm_sub$col_roles$time = character()
  tsk_cookfarm_sub$set_col_roles("SOURCEID", "space")
  rsp = rsmp("repeated_sptcv_cstf", folds = 4, repeats = 2)
  rsp$instantiate(tsk_cookfarm_sub)

  p1 = autoplot(rsp, task = tsk_cookfarm_sub, sample_fold_n = 3L)
  p2 = autoplot(rsp, tsk_cookfarm_sub, 1, plot3D = FALSE, sample_fold_n = 3L)
  # plot() would force image printing here
  p3 = autoplot(rsp, tsk_cookfarm_sub, c(1, 2), sample_fold_n = 3L)

  expect_true(is_ggplot(p1))
  expect_true(is_ggplot(p2))
  expect_s3_class(p3, "patchwork")

  p4 = autoplot(rsp, tsk_cookfarm_sub, repeats_id = 2, sample_fold_n = 3L)
  p5 = autoplot(rsp, tsk_cookfarm_sub, fold_id = 1, repeats_id = 2,
    sample_fold_n = 3L)

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
  tsk_cookfarm_sub$col_roles$feature = setdiff(tsk_cookfarm_sub$col_roles$feature, "Date")
  tsk_cookfarm_sub$col_roles$time = character()
  rsp = rsmp("sptcv_cstf", folds = 4)

  expect_error(autoplot(rsp, task = tsk_cookfarm_sub, plot3D = TRUE))
  p2 = suppressWarnings(
    autoplot(rsp, tsk_cookfarm_sub, 1, plot3D = TRUE, plot_time_var = "Date")
  )
  # plot() would force image printing here
  p3 = suppressWarnings(
    suppressMessages(autoplot(rsp, tsk_cookfarm_sub, c(1, 2), plot3D = TRUE,
      plot_time_var = "Date"))
  )

  p4 = suppressWarnings(
    suppressMessages(autoplot(rsp, tsk_cookfarm_sub, 1, plot3D = TRUE,
      plot_time_var = "Date", sample_fold_n = 200L))
  )

  # test error if neither time nor plot col roles are defined
  # only use Date for plotting, not for partitioning
  tsk_cookfarm_sub$col_roles$time = character()
  expect_error(autoplot(rsp, tsk_cookfarm_sub, 1, plot3D = TRUE))

  expect_s3_class(p2, "plotly")
  expect_list(p3)
  expect_s3_class(p4, "plotly")

  ### sample_fold_n
  # p7 omitted because fold_id is required for this option
  # p7 = autoplot(rsp, tsk_cookfarm_sub, sample_fold_n = 3L, plot3D = TRUE)
  p8 = suppressWarnings(
    autoplot(rsp, tsk_cookfarm_sub, fold_id = 1, sample_fold_n = 3L,
      plot3D = TRUE, plot_time_var = "Date")
  )
  p9 = suppressWarnings(
    autoplot(rsp, tsk_cookfarm_sub, fold_id = c(1, 2), sample_fold_n = 3L,
      plot3D = TRUE, , plot_time_var = "Date")
  )

  # vdiffr::expect_doppelganger("SptCVCstf all test sets", p1)
  suppressWarnings(vdiffr::expect_doppelganger("SptCVCstf 3d time var - Fold 1", p2))
  suppressWarnings(vdiffr::expect_doppelganger("SptCVCstf 3d time var - Fold 1-2", p3))
  suppressWarnings(vdiffr::expect_doppelganger("SptCVCstf 3d time var - Fold 1 - sample_fold_n", p4))

  ### sample_fold_n
  expect_s3_class(p8, "plotly")
  expect_list(p9)
  expect_class(p9[[1]], "plotly")
  expect_class(p9[[2]], "plotly")
  vdiffr::expect_doppelganger("SptCVCstf 3D time_var - Fold 1 - sample_fold_n", p8)
  vdiffr::expect_doppelganger("SptCVCstf 3D time_var - Fold 1-2 - sample_fold_n", p9)

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
      repeats_id = 1) # missing on purpose for codecov reasons
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

  p6 = suppressWarnings(
    autoplot(rsp, tsk_cookfarm_sub, 1,
      show_omitted = TRUE, plot3D = TRUE, sample_fold_n = 200L)
  )

  expect_s3_class(p5, "plotly")

  # vdiffr::expect_doppelganger("RepSptCVCstf all test sets", p1)
  suppressWarnings(vdiffr::expect_doppelganger("RepSptCVCstf Fold 1 Rep 1 - show omitted", p2))
  suppressWarnings(vdiffr::expect_doppelganger("RepSptCVCstf - Fold 1-2 Rep 1", p3))
  suppressWarnings(vdiffr::expect_doppelganger("RepSptCVCstf - Fold 1-2, Rep 2", p4))
  suppressWarnings(vdiffr::expect_doppelganger("RepSptCVCstf - Fold 1, Rep 2", p5))
  suppressWarnings(vdiffr::expect_doppelganger("RepSptCVCstf Fold 1 Rep 1 - sample_fold_n", p6))
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

  expect_true(is_ggplot(p1))
  expect_true(is_ggplot(p2))
  expect_s3_class(p3, "patchwork")
  expect_true(is_ggplot(p4))

  p5 = autoplot(rsp, task, repeats_id = 2)
  p6 = autoplot(rsp, task, fold_id = 1, repeats_id = 2)

  ### sample_fold_n
  p7 = autoplot(rsp, task, sample_fold_n = 3L)
  p8 = autoplot(rsp, task, fold_id = 1, sample_fold_n = 3L)
  p9 = autoplot(rsp, task, fold_id = c(1, 2), sample_fold_n = 3L)

  vdiffr::expect_doppelganger("SpCVDisc all test sets", p1)
  vdiffr::expect_doppelganger("SpCVDisc Fold 1", p2)
  vdiffr::expect_doppelganger("SpCVDisc Fold 1-2", p3)
  vdiffr::expect_doppelganger("SpCVDisc show_omitted", p4)

  vdiffr::expect_doppelganger("RepSpCVDisc - Fold 1-2, Rep 2", p5)
  vdiffr::expect_doppelganger("RepSpCVDisc - Fold 1, Rep 2", p6)
  vdiffr::expect_doppelganger("RepSpCVDisc - Fold 1, Rep 1 - sample_n_fold", p7)

  ### sample_fold_n
  vdiffr::expect_doppelganger("RepSpCVDisc - sample_fold_n", p7)
  vdiffr::expect_doppelganger("RepSpCVDisc - Fold 1 - sample_fold_n", p8)
  vdiffr::expect_doppelganger("RepSpCVDisc - Fold 1-2 - sample_fold_n", p9)
})

test_that("train_color and test_color parameters work for resampling methods", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("sf")
  skip_if_not_installed("patchwork")
  skip_if_not_installed("ggtext")
  
  set.seed(42)
  
  task = tsk("ecuador")
  
  # Test spcv_disc
  rsp_disc = rsmp("spcv_disc", folds = 4, radius = 200, buffer = 200)
  rsp_disc$instantiate(task)
  
  p_disc = autoplot(rsp_disc, task, fold_id = 1, train_color = "red", test_color = "green")
  
  expect_true(is_ggplot(p_disc))
  
  # Extract the color scale to verify colors
  build_disc = ggplot2::ggplot_build(p_disc)
  colors_disc = build_disc$plot$scales$scales[[1]]$palette(2)
  
  expect_equal(unname(colors_disc[1]), "red")  # Train color
  expect_equal(unname(colors_disc[2]), "green") # Test color
  
  # Test spcv_tiles
  rsp_tiles = rsmp("spcv_tiles", nsplit = c(2L, 2L))
  rsp_tiles$instantiate(task)
  
  p_tiles = autoplot(rsp_tiles, task, fold_id = 1, train_color = "blue", test_color = "orange")
  
  expect_true(is_ggplot(p_tiles))
  
  build_tiles = ggplot2::ggplot_build(p_tiles)
  colors_tiles = build_tiles$plot$scales$scales[[1]]$palette(2)
  
  expect_equal(unname(colors_tiles[1]), "blue")  # Train color
  expect_equal(unname(colors_tiles[2]), "orange") # Test color
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

  expect_true(is_ggplot(p1))
  expect_true(is_ggplot(p2))
  expect_s3_class(p3, "patchwork")
  expect_true(is_ggplot(p4))

  p5 = autoplot(rsp, task, c(1, 2), repeats_id = 2)
  p6 = autoplot(rsp, task, fold_id = 1, repeats_id = 2)

  ### sample_fold_n
  p7 = autoplot(rsp, task, sample_fold_n = 3L)
  p8 = autoplot(rsp, task, fold_id = 1, sample_fold_n = 3L)
  p9 = autoplot(rsp, task, fold_id = c(1, 2), sample_fold_n = 3L)

  vdiffr::expect_doppelganger("RepSpCVTiles all test sets", p1)
  vdiffr::expect_doppelganger("RepSpCVTiles Fold 1", p2)
  vdiffr::expect_doppelganger("RepSpCVTiles Fold 1-2", p3)
  vdiffr::expect_doppelganger("RepSpCVTiles show_omitted", p4)

  vdiffr::expect_doppelganger("RepSpCVTiles - Fold 1-2, Rep 2", p5)
  vdiffr::expect_doppelganger("RepSpCVTiles - Fold 1, Rep 2", p6)

  vdiffr::expect_doppelganger("RepSpCVTiles - sample_fold_n", p7)
  vdiffr::expect_doppelganger("RepSpCVTiles - Fold 1 - sample_fold_n", p8)
  vdiffr::expect_doppelganger("RepSpCVTiles - Fold 1-2 - sample_fold_n", p9)
})

# spcv_knndm --------------------------------------------------------------------

test_that("plot() works for 'spcv_knndm'", {
  skip_if_not_installed("vdiffr")

  set.seed(42)

  simarea = list(matrix(c(0, 0, 0, 100, 100, 100, 100, 0, 0, 0),
    ncol = 2, byrow = TRUE))
  simarea = sf::st_polygon(simarea)
  train_points = sf::st_sample(simarea, 1000, type = "random")
  train_points = sf::st_as_sf(train_points)
  train_points$target = as.factor(sample(c("TRUE", "FALSE"), 1000, replace = TRUE))
  pred_points = sf::st_sample(simarea, 1000, type = "regular")

  task = mlr3spatial::as_task_classif_st(sf::st_as_sf(train_points),
    "target", positive = "TRUE")
  rsp = rsmp("repeated_spcv_knndm", folds = 3, repeats = 5, predpoints = pred_points)
  suppressMessages(suppressWarnings(rsp$instantiate(task)))
  set.seed(42)

  p1 = autoplot(rsp, task = task)
  p2 = autoplot(rsp, task, 1)
  # plot() would force image printing here
  p3 = autoplot(rsp, task, c(1, 2))


  expect_true(is_ggplot(p1))
  expect_true(is_ggplot(p2))
  expect_s3_class(p3, "patchwork")

  p5 = autoplot(rsp, task, repeats_id = 2)
  p6 = autoplot(rsp, task, fold_id = 1, repeats_id = 2)

  ### sample_fold_n
  p7 = autoplot(rsp, task, sample_fold_n = 3L)
  p8 = autoplot(rsp, task, fold_id = 1, sample_fold_n = 3L)
  p9 = autoplot(rsp, task, fold_id = c(1, 2), sample_fold_n = 3L)

  vdiffr::expect_doppelganger("SpCVKnndm all test sets", p1)
  vdiffr::expect_doppelganger("SpCVKnndm Fold 1", p2)
  vdiffr::expect_doppelganger("SpCVKnndm Fold 1-2", p3)

  vdiffr::expect_doppelganger("RepSpCVKnndm - Fold 1-2, Rep 2", p5)
  vdiffr::expect_doppelganger("RepSpCVKnndm - Fold 1, Rep 2", p6)
  vdiffr::expect_doppelganger("RepSpCVKnndm - Fold 1, Rep 1 - sample_n_fold", p7)

  ### sample_fold_n
  vdiffr::expect_doppelganger("RepSpCVKnndm - sample_fold_n", p7)
  vdiffr::expect_doppelganger("RepSpCVKnndm - Fold 1 - sample_fold_n", p8)
  vdiffr::expect_doppelganger("RepSpCVKnndm - Fold 1-2 - sample_fold_n", p9)
})

# spcv_disc custom colors test ------------------------------------------------

test_that("train_color and test_color parameters work for 'spcv_disc'", {
  skip_if_not_installed("blockCV")
  set.seed(42)

  task = tsk("ecuador")
  rsmp_disc = rsmp("spcv_disc", folds = 100, radius = 200L, buffer = 400L)
  rsmp_disc$instantiate(task)

  # Test custom colors - this should work without errors
  p_custom = autoplot(rsmp_disc, task = task, fold_id = 1, show_omitted = FALSE,
                     train_color = "red", test_color = "green")
  
  expect_true(is_ggplot(p_custom))
  
  # Check that the plot has the correct color scales
  expect_true("ScaleDiscrete" %in% class(p_custom$scales$scales[[1]]))
  
  # Test that the plot builds without error
  expect_no_error(ggplot_build(p_custom))
  
  # Test multiple folds with custom colors
  p_multi = autoplot(rsmp_disc, task = task, fold_id = c(1, 2), 
                    train_color = "red", test_color = "green")
  
  expect_true(is_ggplot(p_multi))
  expect_no_error(ggplot_build(p_multi))
})
