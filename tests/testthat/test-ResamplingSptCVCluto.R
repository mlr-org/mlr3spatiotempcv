context("ResamplingSptCVCluto")

test_that("resampling iterations equals folds", {
  skip_on_os("mac")

  task = tsk("cookfarm")
  rsp = rsmp("sptcv_cluto", folds = 2, time_var = "Date")
  rsp$instantiate(task)

  expect_equal(rsp$iters, 2)
})

test_that("reps can be printed", {
  skip_on_os("mac")

  task = tsk("cookfarm")
  rsp = rsmp("repeated_sptcv_cluto",
    folds = 3, repeats = 5,
    time_var = "Date")
  rsp$instantiate(task)

  expect_equal(rsp$repeats(4:8), c(2, 2, 2, 3, 3))
})

test_that("resampling iterations equals folds * repeats", {
  skip_on_os("mac")

  task = tsk("cookfarm")
  rsp = rsmp("repeated_sptcv_cluto",
    folds = 3, repeats = 2,
    time_var = "Date")
  rsp$instantiate(task)

  expect_equal(rsp$iters, 6)
})
