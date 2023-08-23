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

test_that("folds can be printed", {
  skip_if_not_installed("CAST")
  task = mlr3spatial::as_task_classif_st(sf::st_as_sf(train_points),
    "target", positive = "TRUE")
  rsp = rsmp("repeated_spcv_knndm", folds = 3, repeats = 5, ppoints = pred_points)
  suppressMessages(suppressWarnings(rsp$instantiate(task)))

  expect_equal(rsp$folds(3:6), c(3, 1, 2, 3))
})

test_that("reps can be printed", {
  skip_if_not_installed("CAST")
  task = mlr3spatial::as_task_classif_st(sf::st_as_sf(train_points),
    "target", positive = "TRUE")
  rsp = rsmp("repeated_spcv_knndm", folds = 3, repeats = 5, ppoints = pred_points)
  suppressMessages(suppressWarnings(rsp$instantiate(task)))

  expect_equal(rsp$repeats(4:8), c(2, 2, 2, 3, 3))
})

test_that("resampling iterations equals folds * repeats", {
  skip_if_not_installed("CAST")
  task = mlr3spatial::as_task_classif_st(sf::st_as_sf(train_points),
    "target", positive = "TRUE")
  rsp = rsmp("repeated_spcv_knndm", folds = 3, repeats = 5, ppoints = pred_points)
  suppressMessages(suppressWarnings(rsp$instantiate(task)))

  expect_equal(rsp$iters, 15)
})
