test_that("mlr3spatiotempcv indices are the same as CAST knndm: modeldomain", {
  skip_if_not_installed("CAST")

  task = tsk("ecuador")
  points = sf::st_as_sf(task$coordinates(), crs = task$crs, coords = c("x", "y"))
  modeldomain = sf::st_as_sfc(sf::st_bbox(points))

  set.seed(42)
  rsmp = rsmp("spcv_knndm", modeldomain = modeldomain)
  suppressMessages(suppressWarnings(rsmp$instantiate(task)))

  set.seed(42)
  testknndm = suppressMessages(suppressWarnings(CAST::knndm(
    tpoints = points,
    modeldomain = modeldomain
  )))

  expect_equal(rsmp$instance$train, testknndm$indx_train)
  expect_equal(rsmp$instance$test, testknndm$indx_test)
})

test_that("mlr3spatiotempcv indices are the same as CAST knndm: predpoints", {
  skip_if_not_installed("CAST")

  simarea = list(matrix(c(0, 0, 0, 100, 100, 100, 100, 0, 0, 0),
    ncol = 2, byrow = TRUE))
  simarea = sf::st_polygon(simarea)
  train_points = sf::st_sample(simarea, 1000, type = "random")
  train_points = sf::st_as_sf(train_points)
  train_points$target = as.factor(sample(c("TRUE", "FALSE"), 1000, replace = TRUE))
  pred_points = sf::st_sample(simarea, 1000, type = "regular")

  task = mlr3spatial::as_task_classif_st(sf::st_as_sf(train_points),
    "target", positive = "TRUE")

  set.seed(42)
  rsmp = rsmp("spcv_knndm", predpoints = pred_points)
  suppressMessages(suppressWarnings(rsmp$instantiate(task)))

  set.seed(42)
  testknndm = suppressMessages(suppressWarnings(CAST::knndm(
    tpoints = train_points,
    predpoints = pred_points
  )))

  expect_equal(rsmp$instance$train, testknndm$indx_train)
  expect_equal(rsmp$instance$test, testknndm$indx_test)
})
