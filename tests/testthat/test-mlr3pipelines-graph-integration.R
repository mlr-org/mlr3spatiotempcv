test_that("spcv methods work in a Graph Learner", {

  # Info: spcv_buffer is missing due to runtime reasons
  rsmps = rsmps(c("spcv_coords", "spcv_env"), folds = 2)
  rsmps = append(rsmps, rsmp("spcv_block", folds = 2, rows = 2, cols = 2))

  out = mlr3misc::map_lgl(rsmps, function(x) {
    test_graph_learner(tsk("ecuador"), x)
  })

  expect_true(all(out))
})

test_that("sptcv methods work in a Graph Learner", {
  rsmps = rsmps(c("sptcv_cstf", "sptcv_cluto"), folds = 2, time_var = "Date")

  out = mlr3misc::map_lgl(rsmps, function(x) {
    test_graph_learner(tsk("cookfarm"), x)
  })

  expect_true(all(out))
})
