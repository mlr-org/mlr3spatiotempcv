test_that("folds can be printed", {
  rsp = rsmp("repeated-spcv-env")
  rsp$param_set$values = list(folds = 3, repeats = 5)
  rsp$instantiate(task)

  expect_equal(rsp$folds(4:8), c(4, 5, 1, 2, 3))
})

test_that("reps can be printed", {
  rsp = rsmp("repeated-spcv-env")
  rsp$param_set$values = list(folds = 3, repeats = 5)
  rsp$instantiate(task)

  expect_equal(rsp$repeats(4:8), c(2, 2, 2, 3, 3))
})
