test_that("resampling iterations equals folds", {
  task = test_make_twoclass_task()
  rsp = rsmp("spcv_env", folds = 2, features = "p_1")
  rsp$instantiate(task)

  expect_equal(rsp$iters, 2)
})

test_that("feature p_1 seperates the observations in two folds of equal size", {
  set.seed(1)

  task = test_make_twoclass_task()
  rsp = rsmp("spcv_env", folds = 2, features = "p_1")
  rsp$instantiate(task)

  expect_equal(rsp$test_set(1), 19:36)
  expect_equal(rsp$train_set(1), 1:18)
  expect_equal(rsp$test_set(2), 1:18)
  expect_equal(rsp$train_set(2), 19:36)
})

test_that("non-numeric feature throws an error", {
  task = test_make_twoclass_task()
  rsp = rsmp("spcv_env", folds = 2, features = "p_2")

  expect_error(rsp$instantiate(task))
})

test_that("non-existing feature throws an error", {
  task = test_make_twoclass_task()
  rsp = rsmp("spcv_env", folds = 2, features = "p_3")

  expect_error(rsp$instantiate(task))
})
