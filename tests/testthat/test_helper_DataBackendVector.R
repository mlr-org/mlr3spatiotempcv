test_that("get_coordinates function works", {
  task_1 = tsk("ecuador")

  sf = sf::st_as_sf(ecuador, coords = c("x", "y"), crs = "+proj=utm +zone=17 +south +datum=WGS84 +units=m +no_defs")
  backend = as_data_backend(sf)
  task_2 = TaskClassif$new(id = "test", backend = backend, target = "slides")

  expect_equal(get_coordinates(task_1)$x, get_coordinates(task_2)$X)
  expect_equal(get_coordinates(task_1)$y, get_coordinates(task_2)$Y)
})

test_that("get_crs function works", {
  task_1 = tsk("ecuador")

  sf = sf::st_as_sf(ecuador, coords = c("x", "y"), crs = "+proj=utm +zone=17 +south +datum=WGS84 +units=m +no_defs")
  backend = as_data_backend(sf)
  task_2 = TaskClassif$new(id = "test", backend = backend, target = "slides")

  expect_equal(get_crs(task_1), get_crs(task_2)$input)
})

test_that("assert_spatial_task function works", {
  task_1 = tsk("ecuador")

  assert_spatial_task(task_1)

  sf = sf::st_as_sf(ecuador, coords = c("x", "y"), crs = "+proj=utm +zone=17 +south +datum=WGS84 +units=m +no_defs")
  backend = as_data_backend(sf)
  task_2 = TaskClassif$new(id = "test", backend = backend, target = "slides")

  assert_spatial_task(task_2)
})
