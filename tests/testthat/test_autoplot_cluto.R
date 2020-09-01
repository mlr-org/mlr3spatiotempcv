context("plot")

# SptCV-Cluto ------------------------------------------------------------------

test_that("plot() works for 'sptcv-cluto'", {
  skip_on_os("mac")
  set.seed(42)

  task = tsk("cookfarm")
  rsp = rsmp("sptcv-cluto", folds = 4)
  rsp$instantiate(task, time_var = "Date")

  # autoplot() is used instead of plot() to prevent side-effect plotting
  p1 = autoplot(rsp, task, crs = 4326)
  p2 = autoplot(rsp, task, 1, crs = 4326)
  p3 = autoplot(rsp, task, c(1, 2, 3, 4), crs = 4326)

  expect_s3_class(p1, "plotly")
  expect_s3_class(p2, "plotly")
  expect_list(p3)

  skip_on_ci()
  vdiffr::expect_doppelganger("SptCVCluto all test sets", p1)
  vdiffr::expect_doppelganger("SptCVCluto - Fold 1", p2)
  vdiffr::expect_doppelganger("SptCVCluto - Fold 1-4", p3)
})

test_that("plot() works for 'repeated-sptcv-cluto'", {
  skip_on_os("mac")

  set.seed(42)

  task = tsk("cookfarm")
  rsp = rsmp("repeated-sptcv-cluto", folds = 4)
  rsp$instantiate(task, time_var = "Date")

  # autoplot() is used instead of plot() to prevent side-effect plotting
  p1 = autoplot(rsp, task, crs = 4326)
  p2 = autoplot(rsp, task, 1, crs = 4326)
  p3 = autoplot(rsp, task, c(1, 2, 3, 4), crs = 4326)

  expect_true(is.ggplot(p1))
  expect_true(is.ggplot(p2))
  expect_true(is.ggplot(p2))

  p4 = autoplot(plots$rsp, plots$task, crs = 4326, repeats_id = 2)
  p5 = autoplot(plots$rsp, plots$task, crs = 4326, fold_id = 1, repeats_id = 2)

  expect_true(is.ggplot(p4))
  expect_true(is.ggplot(p5))

  skip_on_ci()
  vdiffr::expect_doppelganger("RepSptCVCluto all test sets", p1)
  vdiffr::expect_doppelganger("RepSptCVCluto all test sets, Rep 2", p2)
  vdiffr::expect_doppelganger("RepSptCVCluto - Fold 1, Rep 2", p3)
  vdiffr::expect_doppelganger("RepSptCVCluto - Fold 1-4, Rep 2", p4)
  vdiffr::expect_doppelganger("RepSptCVCluto - Fold 1, Rep 2", p5)
})
