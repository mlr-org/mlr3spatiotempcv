# spcv_buffer ------------------------------------------------------------------

test_that("plot() works for 'spcv_buffer'", {
  skip_if_not_installed("blockCV")
  skip_if_not_installed("vdiffr")
  set.seed(42)

  plots = prepare_autoplot("spcv_buffer", theRange = 2)

  # autoplot() is used instead of plot() to prevent side-effect plotting
  expect_error(autoplot(plots$rsp, plots$task))
  p2 = autoplot(plots$rsp, plots$task, 1)
  p3 = autoplot(plots$rsp, plots$task, c(1, 2))
  p4 = autoplot(plots$rsp, plots$task, c(1, 2), plot_as_grid = FALSE)

  expect_true(is_ggplot(p2))
  expect_true(is_ggplot(p2))

  vdiffr::expect_doppelganger("SpCVBuffer - Fold 1", p2)
  vdiffr::expect_doppelganger("SpCVBuffer - Fold 1-2", p3)
})

test_that("train_color and test_color parameters work for 'spcv_buffer'", {
  skip_if_not_installed("blockCV")
  set.seed(42)

  plots = prepare_autoplot("spcv_buffer", theRange = 2)

  # Test custom colors
  p_custom = autoplot(plots$rsp, plots$task, 1, 
                     train_color = "red", test_color = "green")
  
  expect_true(is_ggplot(p_custom))
  
  # Check that the plot has the correct color scales
  expect_true("ScaleDiscrete" %in% class(p_custom$scales$scales[[1]]))
  
  # Test that the plot builds without error
  expect_no_error(ggplot_build(p_custom))
})
