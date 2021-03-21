test_that("resampling with twoclass response is correctly instantiated", {
  task = test_make_twoclass_task()
  rsp = rsmp("spcv_buffer", theRange = 1)
  rsp$instantiate(task)

  expect_equal(rsp$iters, 36)
  expect_list(rsp$instance)
})

test_that("resampling with multiclass response is correctly instantiated", {
  task = test_make_multiclass()
  rsp = rsmp("spcv_buffer", theRange = 1)
  rsp$instantiate(task)

  expect_equal(rsp$iters, 36)
  expect_list(rsp$instance)
})

test_that("resampling with continuous response is correctly instantiated", {
  task = test_make_regr_task()
  rsp = rsmp("spcv_buffer", theRange = 1)
  rsp$instantiate(task)

  expect_equal(rsp$iters, 36)
  expect_list(rsp$instance)
})

test_that("buffer is corretly applied", {
  task = test_make_regr_task()
  rcv = rsmp("spcv_buffer", theRange = 1)
  rcv$instantiate(task)

  expect_true(all(!c(1, 2, 7) %in% rcv$train_set(1)))
  expect_true(all(!c(8, 2, 7, 9, 14) %in% rcv$train_set(8)))
})

test_that("spDataType = 'PA' and addBG = TRUE throws an error", {
  task = test_make_regr_task()
  rsp = rsmp("spcv_buffer", theRange = 1, spDataType = "PA", addBG = TRUE)
  expect_error(rsp$instantiate(task))
})

test_that("spDataType = 'PB' and regression task throws and error", {
  task = test_make_regr_task()
  rsp = rsmp("spcv_buffer", theRange = 1, spDataType = "PB")
  expect_error(rsp$instantiate(task))
})

test_that("spDataType = 'PB' and multi-class task throws and error", {
  task = test_make_multiclass()
  rsp = rsmp("spcv_buffer", theRange = 1, spDataType = "PB")
  expect_error(rsp$instantiate(task))
})
