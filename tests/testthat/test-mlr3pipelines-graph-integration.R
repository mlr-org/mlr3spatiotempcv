test_that("spcv methods work in a Graph Learner", {
  skip("until mlr3filters >= 0.5.1 in on CRAN - including as_task assertion fix")
  skip_if_not_installed("mlr3pipelines")
  skip_if_not_installed("blockCV")

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

test_that("sptcv methods work in a Graph Learner", {
  skip_if_not_installed("mlr3pipelines")

  skip_on_cran()
  skip_on_os("mac")
  skip_if_not_installed("skmeans")
  rsmps = rsmps(c("sptcv_cstf"), folds = 2)

  out = mlr3misc::map_lgl(rsmps, function(x) {
    test_graph_learner(task_cluto, x,
      time_var = "Date",
      learner = "regr.featureless",
      measure = "regr.mse"
    )
  })

  expect_true(all(out))
})
