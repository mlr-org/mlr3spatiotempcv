context("plot")

# spcv-buffer ------------------------------------------------------------------

test_that("plot() works for 'spcv-buffer'", {
  set.seed(42)

  plots = prepare_autoplot("spcv-buffer", theRange = 2)

  # autoplot() is used instead of plot() to prevent side-effect plotting
  expect_error(autoplot(plots$rsp, plots$task, crs = 4326))
  p2 = autoplot(plots$rsp, plots$task, 1, crs = 4326)
  p3 = autoplot(plots$rsp, plots$task, c(1, 2), crs = 4326)

  expect_true(is.ggplot(p1))
  expect_true(is.ggplot(p2))
  expect_true(is.ggplot(p2))

  skip_on_ci()
  vdiffr::expect_doppelganger("SpCVBuffer all test sets", p1)
  vdiffr::expect_doppelganger("SpCVBuffer - Fold 1", p2)
  vdiffr::expect_doppelganger("SpCVBuffer - Fold 1-4", p3)
})
