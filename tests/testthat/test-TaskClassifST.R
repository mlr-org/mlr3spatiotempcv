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
    "Must inherit from class 'TaskClassifST', 'TaskRegrST' or a 'Task' with 'DataBackendVector'") # nolint
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

# https://github.com/mlr-org/mlr3spatiotempcv/issues/151
test_that("tasks created from sf objects do not duplicate rows", {
  data = test_make_sf_twoclass_df()

  task = TaskClassifST$new(
    id = "sp_regression",
    backend = data,
    target = "response",
    extra_args = list(
      coordinate_names = c("x", "y"),
      crs = "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs",
      coords_as_features = FALSE)
  )

  expect_equal(nrow(task$data()), nrow(data))
})

test_that("extra_args contains default arguments", {
  data("ecuador", package = "mlr3spatiotempcv")
  task = TaskClassifST$new(
    id = "sp_regression",
    backend = ecuador,
    target = "slides",
    extra_args = list(
      coordinate_names = c("x", "y"))
  )

  expect_equal(task$extra_args, list(coords_as_features = FALSE, crs = NA, coordinate_names = c("x", "y")))
})
