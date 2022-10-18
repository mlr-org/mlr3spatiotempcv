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

# https://github.com/mlr-org/mlr3spatiotempcv/issues/151
test_that("tasks created from sf objects do not duplicate rows", {
  data = test_make_sp()
  data$p_1 = c(rep("A", 18), rep("B", 18))
  data$response = rnorm(36)

  task = TaskRegrST$new(
    id = "sp_regression",
    backend = data,
    target = "response",
    coordinate_names = c("x", "y"),
    crs = "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs",
    coords_as_features = FALSE
  )

  expect_equal(nrow(task$data()), nrow(data))
})

test_that("extra_args contains default arguments", {
  data = test_make_sp()
  data$p_1 = c(rep("A", 18), rep("B", 18))
  data$response = rnorm(36)

  task = TaskRegrST$new(
    id = "sp_regression",
    backend = data,
    target = "response",
    coordinate_names = c("x", "y")
  )

  expect_equal(task$extra_args, list(
    crs = NA_character_,
    coordinate_names = c("x", "y"), coords_as_features = FALSE
  ))
})

test_that("Task creation works via constructor and sf object", {
  skip_if_not_installed("sf")
  data = test_make_sp()
  data$p_1 = c(rep("A", 18), rep("B", 18))
  data$response = rnorm(36)

  data_sf = sf::st_as_sf(data, coords = c("x", "y"))

  # create mlr3 task via constructor
  expect_error(TaskRegrST$new("ecuador_sf",
    backend = data_sf,
    target = "response"
  ), "Creating tasks from `sf` objects is not supported anymore")
})
