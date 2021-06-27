# sptcv_Cluto ------------------------------------------------------------------

test_that("plot() works for 'sptcv_cluto'", {
  skip_if_not_installed("vdiffr")
  skip_on_cran()
  skip_on_os("mac")
  set.seed(42)

  task = tsk("cookfarm")
  rsp = rsmp("sptcv_cluto", folds = 4, time_var = "Date")
  rsp$instantiate(task)

  # autoplot() is used instead of plot() to prevent side-effect plotting
  p1 = autoplot(rsp, task)
  p2 = autoplot(rsp, task, 1)
  p3 = autoplot(rsp, task, c(1, 2))
  p4 = autoplot(rsp, task, c(1, 2), plot_as_grid = FALSE)

  expect_s3_class(p1, "plotly")
  expect_s3_class(p2, "plotly")
  expect_list(p3)

  vdiffr::expect_doppelganger("SptCVCluto all test sets", p1)
  vdiffr::expect_doppelganger("SptCVCluto - Fold 1", p2)
  vdiffr::expect_doppelganger("SptCVCluto - Fold 1-2", p3)
})

test_that("plot() works for 'repeated_sptcv_cluto'", {
  skip_if_not_installed("vdiffr")
  skip_on_cran()
  skip_on_os("mac")

  set.seed(42)

  task = tsk("cookfarm")
  rsp = rsmp("repeated_sptcv_cluto", folds = 4, time_var = "Date")
  rsp$instantiate(task)

  # autoplot() is used instead of plot() to prevent side-effect plotting
  p1 = autoplot(rsp, task)
  p2 = autoplot(rsp, task, 1)
  p3 = autoplot(rsp, task, c(1, 2))

  expect_s3_class(p1, "plotly")
  expect_s3_class(p2, "plotly")
  expect_list(p3)

  p4 = autoplot(rsp, task, repeats_id = 2)
  p5 = autoplot(rsp, task, fold_id = 1, repeats_id = 2)

  expect_s3_class(p4, "plotly")
  expect_s3_class(p5, "plotly")

  vdiffr::expect_doppelganger("RepSptCVCluto all test sets", p1)
  vdiffr::expect_doppelganger("RepSptCVCluto all test sets, Rep 2", p2)
  vdiffr::expect_doppelganger("RepSptCVCluto - Fold 1, Rep 2", p3)
  vdiffr::expect_doppelganger("RepSptCVCluto - Fold 1-2, Rep 2", p4)
  vdiffr::expect_doppelganger("RepSptCVCluto - Fold 1, Rep 2", p5)
})
