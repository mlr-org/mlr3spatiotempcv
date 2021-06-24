# for graph learner tests
loadNamespace("mlr3pipelines")
loadNamespace("mlr3")

# Create 6x6 point grid with 1m distance between points
test_make_sp = function() {
  coordinates = expand.grid(315172:315177, 5690670:5690675)
  names(coordinates) = c("x", "y")
  coordinates
}

# regr tasks -------------------------------------------------------------------

# Create regression task
test_make_regr_task = function(coords_as_features = FALSE) {
  data = test_make_sp()
  data$p_1 = c(rep("A", 18), rep("B", 18))
  data$response = rnorm(36)

  TaskRegrST$new(
    id = "sp_regression",
    backend = data,
    target = "response",
    extra_args = list(
      coordinate_names = c("x", "y"),
      crs = "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs",
      coords_as_features = coords_as_features)
  )
}

# Create regression task
# similar to test_make_regr_task(), just to check if 'sf' objects work
# as intended
test_make_sf_regr_task = function(coords_as_features = FALSE) {
  data = test_make_sp()
  data$p_1 = c(rep("A", 18), rep("B", 18))
  data$response = rnorm(36)

  data_sf = sf::st_as_sf(data, coords = c("x", "y"), crs = "epsg:4326")

  TaskRegrST$new(
    id = "sf_regr",
    backend = data_sf,
    target = "response",
    extra_args = list(
      coordinate_names = c("x", "y"),
      crs = "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs",
      coords_as_features = FALSE)
  )
}

# classif tasks ----------------------------------------------------------------

# Create twoclass task
test_make_twoclass_task = function(group = FALSE, coords_as_features = FALSE,
  features = "numeric") {

  data = test_make_sp()
  if ("numeric" %in% features) {
    data$p_1 = c(rnorm(18, 0), rnorm(18, 10))
  }
  if ("factor" %in% features) {
    data$p_2 = as.factor(c(rep("lvl_1", 18), rep("lvl_2", 18)))
  }
  data$response = as.factor(c(rep("A", 18), rep("B", 18)))

  if (group) {
    data$group = rep_len(letters[1:10], 36)
  }

  task = TaskClassifST$new(
    id = "sp_twoclass",
    backend = data,
    target = "response",
    extra_args = list(
      coordinate_names = c("x", "y"),
      crs = "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs",
      positive = "A",
      coords_as_features = coords_as_features)
  )

  if (group) {
    task$col_roles$group = "group"
  }
  task
}

# Create twoclass sf task
# similar to test_make_twoclass_task(), just to check if 'sf' objects work
# as intended
test_make_sf_twoclass_task = function(group = FALSE, coords_as_features = FALSE, features = "numeric") {

  data = test_make_sp()
  if ("numeric" %in% features) {
    data$p_1 = c(rnorm(18, 0), rnorm(18, 10))
  }
  if ("factor" %in% features) {
    data$p_2 = as.factor(c(rep("lvl_1", 18), rep("lvl_2", 18)))
  }
  data$response = as.factor(c(rep("A", 18), rep("B", 18)))

  if (group) {
    data$group = rep_len(letters[1:10], 36)
  }

  data_sf = sf::st_as_sf(data, coords = c("x", "y"), crs = "epsg:4326")

  task = TaskClassifST$new(
    id = "sf_twoclass",
    backend = data_sf,
    target = "response",
    extra_args = list(
      positive = "A",
      coords_as_features = coords_as_features)
  )

  if (group) {
    task$col_roles$group = "group"
  }
  task
}

# Create twoclass sf task
test_make_sf_twoclass_df = function(group = FALSE,
  coords_as_features = FALSE, features = "numeric") {

  data = test_make_sp()
  if ("numeric" %in% features) {
    data$p_1 = c(rnorm(18, 0), rnorm(18, 10))
  }
  if ("factor" %in% features) {
    data$p_2 = as.factor(c(rep("lvl_1", 18), rep("lvl_2", 18)))
  }
  data$response = as.factor(c(rep("A", 18), rep("B", 18)))

  if (group) {
    data$group = rep_len(letters[1:10], 36)
  }

  data_sf = sf::st_as_sf(data, coords = c("x", "y"), crs = "epsg:4326")

  return(data_sf)
}

# sf DF to use directly with {blockCV} functions
test_make_blockCV_test_df = function() {

  set.seed(123)
  x = runif(5000, -80.4, -74)
  y = runif(5000, 39.6, 41)

  data = data.frame(
    spp = "test",
    label = factor(round(runif(length(x), 0, 1))),
    x = x,
    y = y)

  data_sf = sf::st_as_sf(data,
    coords = c("x", "y"),
    crs = "EPSG:4326")

  return(data_sf)

}

# mlr3 task to compare mlr3spatiotempcv results with {blockCV} results
test_make_blockCV_test_task = function() {
  data = test_make_blockCV_test_df()

  task = TaskClassifST$new(
    id = "test",
    backend = data,
    target = "label",
    positive = "1")
  return(task)

}

# multiclass tasks -------------------------------------------------------------
#
# Create multiclass task
test_make_multiclass = function() {
  data = test_make_sp()
  data$p_1 = rnorm(36)
  data$response = as.factor(c(rep("A", 9), rep("B", 9), rep("C", 9), rep("D", 9)))

  TaskClassifST$new(
    id = "sp_multiclass",
    backend = data,
    target = "response",
    extra_args = list(
      coordinate_names = c("x", "y"),
      crs = "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs",
      coords_as_features = FALSE)
  )
}

# mlr3pipelines Graph learner --------------------------------------------------

test_graph_learner = function(task, resampling, learner = "classif.featureless",
  measure = "classif.ce") {

  lgr::get_logger("mlr3")$set_threshold("warn")
  lgr::get_logger("bbotk")$set_threshold("warn")

  lrn = lrn(
    learner
  )
  po_lrn = mlr3pipelines::po("learner", lrn)

  # Create feature filter based on variable importance
  po_filter = mlr3pipelines::po("filter",
    filter = mlr3filters::flt("importance", learner = lrn))

  # Create process (new learner) for filtering the task
  grph = mlr3pipelines::Graph$new()$add_pipeop(po_filter)$add_pipeop(po_lrn)$add_edge("importance", learner) # nolint
  glrn = mlr3pipelines::GraphLearner$new(grph)

  # Create filter parameters
  param_set = paradox::ParamSet$new(
    params = list(paradox::ParamDbl$new("importance.filter.frac",
      lower = 0.1, upper = 1))
  )

  # Create filtering instance
  instance = mlr3tuning::TuningInstanceSingleCrit$new(
    task = task,
    learner = glrn,
    resampling = resampling,
    measure = msr(measure),
    search_space = param_set,
    terminator = mlr3tuning::trm("none")
  )

  # Create tuner
  tuner = mlr3tuning::tnr("grid_search", resolution = 2)
  tuner$optimize(instance)

  return(TRUE)
}

# vdiffr::expect_doppelganger = function(title, fig, path = NULL, ...) {
#   testthat::skip_if_not_installed("vdiffr")
#   vdiffr::expect_doppelganger(title = title, fig = fig, path = path, ...)
# }
