context("spcv-env")

rsp = mlr_resamplings$get("spcv-env")
task = mlr_tasks$get("diplodia")

test_that("cols and rows are ignored when range is given", {
  rsp = mlr_resamplings$get("spcv-env")
  rsp$param_set$values = list(features = "lithology", folds = 4)
  expect_warning(rsp$instantiate(task), "Cols and rows are ignored. Range is used to generated blocks")
})
