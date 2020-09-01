context("ResamplingSptCVCluto")

test_that("resampling iterations equals folds", {
  skip_on_os("mac")
  skip_if(Sys.getenv("CI") != "true")

  task = tsk("cookfarm")
  rsp = rsmp("sptcv-cluto", folds = 2)
  rsp$instantiate(task, time_var = "Date")

  expect_equal(rsp$iters, 2)
})

test_that("reps can be printed", {
  skip_on_os("mac")
  skip_if(Sys.getenv("CI") != "true")

  task = tsk("cookfarm")
  rsp = rsmp("repeated-sptcv-cluto", folds = 3, repeats = 5)
  rsp$instantiate(task, time_var = "Date")

  expect_equal(rsp$repeats(4:8), c(2, 2, 2, 3, 3))
})

test_that("resampling iterations equals folds * repeats", {
  skip_on_os("mac")
  skip_if(Sys.getenv("CI") != "true")
  task = tsk("cookfarm")
  rsp = rsmp("repeated-sptcv-cluto", folds = 3, repeats = 2)
  rsp$instantiate(task, time_var = "Date")

  expect_equal(rsp$iters, 6)
})
