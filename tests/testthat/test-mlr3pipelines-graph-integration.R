test_that("spcv methods work in a Graph Learner", {
  skip_if_not_installed("mlr3pipelines")
  skip_if_not_installed("blockCV")
  skip_if_not_installed("raster")
  skip_on_cran() # "Cannot reproduce winbuilder failure"

  # - spcv_buffer is missing due to runtime reasons
  # - spcv_block is missing due to runtime reasons
  rsmps = rsmps(c("spcv_coords", "spcv_env"), folds = 2)
  rsmps = append(rsmps, rsmp("spcv_block", folds = 2, rows = 2, cols = 2))

  # for some reason the test errors if we dont attach the full namespace here
  library(mlr3spatiotempcv)
  task = tsk("ecuador")

  out = mlr3misc::map_lgl(rsmps, function(x) {
    test_graph_learner(task, x)
  })

  expect_true(all(out))
})
