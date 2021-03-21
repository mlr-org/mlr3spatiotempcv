test_that("coordinates can be used as features", {
  task = test_make_regr_task(coords_as_features = TRUE)

  expect_names(c("x", "y"), subset.of = task$feature_names)
})

test_that("printing works", {
  task = test_make_regr_task()

  expect_output(print(task))
  expect_data_table(task$coordinates())
  expect_output(print(task$truth()))
})
