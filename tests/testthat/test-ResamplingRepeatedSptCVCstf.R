test_that("folds can be printed", {
  task = test_make_twoclass_task()
  rsp = rsmp("repeated_sptcv_cstf", folds = 3, repeats = 5)
  rsp$instantiate(task)

  expect_equal(rsp$folds(4:8), c(1, 2, 3, 1, 2))
})

test_that("reps can be printed", {
  task = tsk("cookfarm")
  rsp = rsmp("repeated_sptcv_cstf",
    folds = 3, repeats = 5,
    space_var = "SOURCEID")
  rsp$instantiate(task)

  expect_equal(rsp$repeats(4:8), c(2, 2, 2, 3, 3))
  expect_equal(rsp$folds(10), 1)
})

test_that("resampling iterations equals folds * repeats", {
  task = tsk("cookfarm")
  rsp = rsmp("repeated_sptcv_cstf",
    folds = 3, repeats = 5,
    time_var = "Date")
  rsp$instantiate(task)

  expect_equal(rsp$iters, 15)
})

test_that("resampling iterations equals folds * repeats", {
  task = tsk("cookfarm")
  rsp = rsmp("repeated_sptcv_cstf",
    folds = 3, repeats = 5,
    time_var = "Date", space_var = "SOURCEID")
  rsp$instantiate(task)

  expect_equal(rsp$iters, 15)
})
