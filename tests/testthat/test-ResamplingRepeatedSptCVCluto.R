test_that("folds can be printed", {
  skip_on_cran()
  skip_on_os("mac")
  skip_if_not_installed("skmeans")
  task = tsk("cookfarm")
  task$set_col_roles("Date", "time")
  rsp = rsmp("repeated_sptcv_cluto", folds = 3, repeats = 5)
  rsp$instantiate(task)

  expect_equal(rsp$folds(4:8), c(1, 2, 3, 1, 2))
})

test_that("reps and folds can be printed", {
  skip_on_cran()
  skip_on_os("mac")
  skip_if_not_installed("skmeans")
  task = tsk("cookfarm")
  task$set_col_roles("Date", "time")
  rsp = rsmp("repeated_sptcv_cluto", folds = 3, repeats = 5)
  rsp$instantiate(task)

  expect_equal(rsp$repeats(4:8), c(2, 2, 2, 3, 3))
  expect_equal(rsp$folds(10), 1)
})

test_that("resampling iterations equals folds * repeats", {
  skip_on_cran()
  skip_on_os("mac")
  skip_if_not_installed("skmeans")
  task = tsk("cookfarm")
  task$set_col_roles("Date", "time")
  rsp = rsmp("repeated_sptcv_cluto", folds = 3, repeats = 5)
  rsp$instantiate(task)

  expect_equal(rsp$iters, 15)
})

test_that("resampling iterations equals folds * repeats", {
  skip_on_cran()
  skip_on_os("mac")
  skip_if_not_installed("skmeans")
  task = tsk("cookfarm")
  task$set_col_roles("Date", "time")
  rsp = rsmp("repeated_sptcv_cluto", folds = 3, repeats = 5)
  rsp$instantiate(task)

  expect_equal(rsp$iters, 15)
})
