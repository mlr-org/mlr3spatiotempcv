test_that("coordinates can be used as features", {
  task = test_make_twoclass_task(coords_as_features = TRUE)

  expect_names(c("x", "y"), subset.of = task$feature_names)
})

test_that("printing works", {
  task = test_make_twoclass_task()

  expect_output(print(task))
  expect_data_table(task$coordinates())
  expect_output(print(task$truth()))
})

test_that("Supplying a non-spatio temporal task gives descriptive error message", {
  expect_error(
    rsmp("spcv_coords")$instantiate(tsk("boston_housing")),
    "Assertion on 'task' failed: Must inherit from class 'TaskClassifST'/'TaskRegrST'") # nolint
})

test_that("coordinates can be used as features", {
  task = test_make_twoclass_task(coords_as_features = TRUE)

  expect_names(c("x", "y"), subset.of = task$feature_names)
})

test_that("sf objects can be used for task creation", {
  task = test_make_sf_twoclass_task()

  expect_output(print(task))
  expect_data_table(task$coordinates())
  expect_output(print(task$truth()))
})

test_that("sf objects can be used for task creation", {
  task = test_make_sf_regr_task()

  expect_output(print(task))
  expect_data_table(task$coordinates())
  expect_output(print(task$truth()))
})
