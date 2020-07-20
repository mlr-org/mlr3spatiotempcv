context("ResamplingSptCVskmeans")

test_that("resampling iterations equals folds", {
  skip_on_os("mac")

  task = tsk("cookfarm")
  rsp = rsmp("spcv-cluto", folds = 2)
  rsp$instantiate(task, time_var = "Date")

  expect_equal(rsp$iters, 2)
})

test_that("reps can be printed", {
  task = tsk("cookfarm")
  rsp = rsmp("repeated-spcv-cluto", folds = 3, repeats = 5)
  rsp$instantiate(task, time_var = "Date")

  expect_equal(rsp$repeats(4:8), c(2, 2, 2, 3, 3))
})

test_that("resampling iterations equals folds * repeats", {
  task = test_make_twoclass()
  rsp = rsmp("repeated-spcv-coords", folds = 3, repeats = 2)
  rsp$instantiate(task)

  expect_equal(rsp$iters, 6)
})
