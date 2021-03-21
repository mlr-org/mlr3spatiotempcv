test_that("resampling iterations equals number of observations (two-class response)", {
  task = test_make_twoclass_task()
  rsp = rsmp("spcv_buffer", theRange = 1)
  rsp$instantiate(task)

  expect_equal(rsp$iters, 36)
})

test_that("resampling iterations equals number of observations (multi-class response)", {
  task = test_make_multiclass()
  rsp = rsmp("spcv_buffer", theRange = 1)
  rsp$instantiate(task)

  expect_equal(rsp$iters, 36)
})

test_that("resampling iterations equals number of observations (continuous response)", {
  task = test_make_regr_task()
  rsp = rsmp("spcv_buffer", theRange = 1)
  rsp$instantiate(task)

  expect_equal(rsp$iters, 36)
})

test_that("buffered observations are excludes in train set", {
  task = test_make_twoclass_task()
  rsp = rsmp("spcv_buffer", theRange = 1, spDataType = "PA")
  rsp$instantiate(task)

  expect_true(all(!c(1, 2, 7) %in% rsp$train_set(1)))
  expect_true(all(!c(8, 2, 7, 9, 14) %in% rsp$train_set(8)))
})

test_that("test sets only include positive observations", {
  task = test_make_twoclass_task()
  rsp = rsmp("spcv_buffer", theRange = 1, spDataType = "PB", addBG = FALSE)
  rsp$instantiate(task)

  expect_equal(rsp$iters, 18)
  expect_equal(mlr3misc::map_int(1:18, rsp$test_set), 1:18)
})

test_that("test sets include background observations", {
  task = test_make_twoclass_task()
  rsp = rsmp("spcv_buffer", theRange = 1, spDataType = "PB", addBG = TRUE)
  rsp$instantiate(task)

  expect_equal(rsp$test_set(13), c(19, 13))
  expect_equal(rsp$test_set(14), c(20, 14))
  expect_equal(rsp$test_set(15), c(21, 15))
  expect_equal(rsp$test_set(16), c(22, 16))
  expect_equal(rsp$test_set(17), c(23, 17))
  expect_equal(rsp$test_set(18), c(24, 18))
})

test_that("spDataType = 'PA' and addBG = TRUE throws an error", {
  task = test_make_twoclass_task()
  rsp = rsmp("spcv_buffer", theRange = 1, spDataType = "PA", addBG = TRUE)

  expect_error(rsp$instantiate(task))
})

test_that("spDataType = 'PB' and continuous response throws an error", {
  task = test_make_regr_task()
  rsp = rsmp("spcv_buffer", theRange = 1, spDataType = "PB")

  expect_error(rsp$instantiate(task))
})

test_that("spDataType = 'PB' and multi-class response throws an error", {
  task = test_make_regr_task()
  rsp = rsmp("spcv_buffer", theRange = 1, spDataType = "PB")

  expect_error(rsp$instantiate(task))
})

test_that("grouping throws errors when 'groups' is set", {
  task = test_make_twoclass_task(group = TRUE)
  rsp = rsmp("spcv_buffer", theRange = 1)

  expect_error(
    rsp$instantiate(task),
    "Grouping is not supported for spatial resampling methods")
})
