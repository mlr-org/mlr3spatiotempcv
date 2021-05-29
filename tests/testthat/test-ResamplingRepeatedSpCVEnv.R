test_that("folds can be printed", {
  task = test_make_twoclass_task()
  rsp = rsmp("repeated_spcv_env")
  rsp$param_set$values = list(folds = 3, repeats = 5, features = "p_1")
  rsp$instantiate(task)

  expect_equal(rsp$folds(4:8), c(1, 2, 3, 1, 2))
})

test_that("reps can be printed", {
  task = test_make_twoclass_task()
  rsp = rsmp("repeated_spcv_env")
  rsp$param_set$values = list(folds = 3, repeats = 5, features = "p_1")
  rsp$instantiate(task)

  expect_equal(rsp$repeats(4:8), c(2, 2, 2, 3, 3))
})

test_that("resampling iterations equals folds * repeats", {
  task = test_make_twoclass_task()
  rsp = rsmp("repeated_spcv_env", folds = 2, repeats = 5, features = "p_1")
  rsp$instantiate(task)

  expect_equal(rsp$iters, 10)
})

test_that("feature p_1 seperates the observations in folds of equal size", {
  set.seed(1)

  task = test_make_twoclass_task()
  rsp = rsmp("repeated_spcv_env", folds = 2, repeats = 5, features = "p_1")
  rsp$instantiate(task)

  expect_equal(rsp$test_set(1), 19:36)
  expect_equal(rsp$train_set(1), 1:18)
  expect_equal(rsp$test_set(2), 1:18)
  expect_equal(rsp$train_set(2), 19:36)
})

test_that("non-numeric feature throws an error", {
  task = test_make_twoclass_task(features = c("numeric", "factor"))
  rsp = rsmp("repeated_spcv_env", folds = 2, repeats = 5, features = "p_2")

  expect_error(rsp$instantiate(task))
})

test_that("non-existing feature throws an error", {
  task = test_make_twoclass_task()
  rsp = rsmp("repeated_spcv_env", folds = 2, repeats = 5, features = "p_3")

  expect_error(rsp$instantiate(task))
})
