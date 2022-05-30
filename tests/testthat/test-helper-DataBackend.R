test_that("`get_coordinates()` works with DataBackendVector", {

  skip_if_not_installed("mlr3spatial")
  requireNamespace("mlr3spatial", quietly = TRUE)
  task_1 = tsk("ecuador")

  sf = sf::st_as_sf(ecuador, coords = c("x", "y"),
    crs = "+proj=utm +zone=17 +south +datum=WGS84 +units=m +no_defs")
  backend = as_data_backend(sf)
  task_vec = TaskClassif$new(id = "test", backend = backend, target = "slides")

  expect_equal(get_coordinates(task_1)$x, get_coordinates(task_vec)$X)
  expect_equal(get_coordinates(task_1)$y, get_coordinates(task_vec)$Y)
})

test_that("`get_crs()` works with DataBackendVector", {
  skip_if_not_installed("mlr3spatial")
  requireNamespace("mlr3spatial", quietly = TRUE)

  task_1 = tsk("ecuador")

  sf = sf::st_as_sf(ecuador, coords = c("x", "y"),
    crs = "+proj=utm +zone=17 +south +datum=WGS84 +units=m +no_defs")
  backend = as_data_backend(sf)
  task_vec = TaskClassif$new(id = "test", backend = backend, target = "slides")

  expect_equal(get_crs(task_1), get_crs(task_vec)$input)
})

test_that("`assert_spatial_task()` works", {

  skip_if_not_installed("mlr3spatial")
  requireNamespace("mlr3spatial", quietly = TRUE)

  sf = sf::st_as_sf(ecuador, coords = c("x", "y"),
    crs = "+proj=utm +zone=17 +south +datum=WGS84 +units=m +no_defs")
  backend = as_data_backend(sf)
  task_vec = TaskClassif$new(id = "test", backend = backend, target = "slides")

  assert_spatial_task(task_vec)
})

test_that("`get_coordinates()` works with DataBackendRaster", {

  skip_if_not_installed(c("mlr3spatial", "terra"))
  requireNamespace("mlr3spatial", quietly = TRUE)
  ras = mlr3spatial::demo_stack_spatraster()
  terra::crs(ras) = "EPSG:4326" # dummy
  value = data.table(ID = c(0, 1), y = c("negative", "positive"))
  terra::set.cats(ras, layer = "y", value = value)
  backend = as_data_backend(ras)
  task_ras = TaskClassif$new(id = "y", backend = backend, target = "y")

  coord_names = get_coordinate_names(task_ras)
  expect_equal(coord_names, c("x", "y"))

  expect_silent(get_coordinates(task_ras)[[coord_names[1]]])
  expect_silent(get_coordinates(task_ras)[[coord_names[2]]])

  expect_length(get_crs(task_ras), 5)
  expect_length(get_crs(task_ras, pretty = FALSE), 1)
})

test_that("`get_crs()` works with DataBackendRaster", {
  skip_if_not_installed(c("mlr3spatial", "terra"))
  requireNamespace("mlr3spatial", quietly = TRUE)
  ras = mlr3spatial::demo_stack_spatraster()
  terra::crs(ras) = "EPSG:4326" # dummy
  value = data.table(ID = c(0, 1), y = c("negative", "positive"))
  terra::set.cats(ras, layer = "y", value = value)
  backend = as_data_backend(ras)
  task_ras = TaskClassif$new(id = "y", backend = backend, target = "y")

  expect_length(get_crs(task_ras), 5)
  expect_length(get_crs(task_ras, pretty = FALSE), 1)
})
