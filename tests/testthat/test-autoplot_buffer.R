# spcv_buffer ------------------------------------------------------------------

test_that("plot() works for 'spcv_buffer'", {
  set.seed(42)

  plots = prepare_autoplot("spcv_buffer", theRange = 2)

  # autoplot() is used instead of plot() to prevent side-effect plotting
  expect_error(autoplot(plots$rsp, plots$task, crs = 4326))
  p2 = autoplot(plots$rsp, plots$task, 1, crs = 4326)
  p3 = autoplot(plots$rsp, plots$task, c(1, 2))
  p4 = autoplot(plots$rsp, plots$task, c(1, 2),
    crs = 4326, plot_as_grid = FALSE)

  expect_true(is.ggplot(p2))
  expect_true(is.ggplot(p2))

  vdiffr::expect_doppelganger("SpCVBuffer - Fold 1", p2)
  vdiffr::expect_doppelganger("SpCVBuffer - Fold 1-2", p3)
})
