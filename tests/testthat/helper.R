# Create 6x6 point grid with 1m distance between points
test_make_sp = function() {
  coordinates = expand.grid(315172:315177, 5690670:5690675)
  names(coordinates) = c("x", "y")
  coordinates
}

# Create regression task
test_make_regr = function(coords_as_features = FALSE) {
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

# Create twoclass task
test_make_twoclass = function(group = FALSE, coords_as_features = FALSE, features = "numeric") {

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

test_graph_learner = function(task, resampling) {

  lgr::get_logger("mlr3")$set_threshold("warn")
  lgr::get_logger("bbotk")$set_threshold("warn")

  # This example uses the ranger package to do model and perform feature filtering
  # In order to do this, pipeops need to be used
  lrn <- lrn(
    "classif.featureless"
  )
  po_lrn <- mlr3pipelines::po("learner", lrn)

  # Create feature filter based on variable importance
  po_filter <- mlr3pipelines::po("filter",
    filter = mlr3filters::flt("importance", learner = lrn))

  # Create process (new learner) for filtering the task
  glrn <- mlr3pipelines::GraphLearner$new(po_filter %>>% po_lrn)
  glrn$predict_type <- "prob"

  # Create filter parameters
  param_set <- paradox::ParamSet$new(
    params = list(paradox::ParamDbl$new("importance.filter.frac",
      lower = 0.1, upper = 1))
  )

  # Create filtering instance
  instance <- mlr3tuning::TuningInstanceSingleCrit$new(
    task = task,
    learner = glrn,
    resampling = resampling,
    measure = msr("classif.ce"),
    search_space = param_set,
    terminator = mlr3tuning::trm("none")
  )

  # Create tuner
  tuner <- mlr3tuning::tnr("grid_search", resolution = 2)
  tuner$optimize(instance)

  return(TRUE)
}
