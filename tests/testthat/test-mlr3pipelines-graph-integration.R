test_that("spcv methods work in a Graph Learner", {

  # - spcv_buffer is missing due to runtime reasons
  # - spcv_block is missing due to runtime reasons
  rsmps = rsmps(c("spcv_coords", "spcv_env"), folds = 2)
  rsmps = append(rsmps, rsmp("spcv_block", folds = 2, rows = 2, cols = 2))

  out = mlr3misc::map_lgl(rsmps, function(x) {
    test_graph_learner(tsk("ecuador"), x)
  })

  expect_true(all(out))
})

test_that("sptcv methods work in a Graph Learner", {
  skip_on_cran()
  skip_on_os("mac")
  rsmps = rsmps(c("sptcv_cstf", "sptcv_cluto"), folds = 2, time_var = "Date")

  out = mlr3misc::map_lgl(rsmps, function(x) {
    test_graph_learner(tsk("cookfarm"), x,
      learner = "regr.featureless",
      measure = "regr.mse")
  })

  expect_true(all(out))
})
