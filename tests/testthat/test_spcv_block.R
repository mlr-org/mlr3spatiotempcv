context("spcv-block")

rsp = mlr_resamplings$get("spcv-block")
task = mlr_tasks$get("ecuador")

test_that("cols and rows are ignored when range is given", {
  rsp = mlr_resamplings$get("spcv-block")
  rsp$param_set$values = list(cols = 2, rows = 3, range = 100, folds = 4)
  expect_warning(rsp$instantiate(task), "Cols and rows are ignored. Range is used to generated blocks")
})
