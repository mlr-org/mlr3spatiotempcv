test_that("check that diplodia example task works", {
  task = tsk("diplodia")
  rsp = rsmp("spcv_coords", folds = 2)
  rsp$instantiate(task)

  expect_equal(rsp$iters, 2)
})

test_that("check that ecuador example task works", {
  task = tsk("ecuador")
  rsp = rsmp("spcv_coords", folds = 2)
  rsp$instantiate(task)

  expect_equal(rsp$iters, 2)
})

test_that("check that cookfarm example task works", {
  task = tsk("cookfarm_mlr3")
  rsp = rsmp("spcv_coords", folds = 2)
  rsp$instantiate(task)

  expect_equal(rsp$iters, 2)
})
