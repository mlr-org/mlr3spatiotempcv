context("TaskRegrST")

test_that("coordinates can be used as features", {
  task = TEST_MAKE_REGR(coords_as_features = TRUE)

  expect_names(c("x", "y"), subset.of = task$feature_names)
})

test_that("printing works", {
  task = TEST_MAKE_REGR()

  expect_output(print(task))
  expect_data_table(task$coordinates())
  expect_output(print(task$truth()))
})
