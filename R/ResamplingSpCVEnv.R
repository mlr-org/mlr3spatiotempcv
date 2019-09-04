#' @title Environmental Block Cross Validation Resampling
#'
#' @format [R6::R6Class] inheriting from [Resampling].
#' @import mlr3
#'
#' @description Environmental Block Cross Validation. This strategy uses k-means
#' clustering to specify blocks of smilar environmental conditions. Only numeric
#' features can be used. The `features` used for building blocks can be
#' specified in the `param_set`. By default, all numeric features are used.
#' @section Fields: See [Resampling].
#'
#' @section Methods: See [Resampling].
#'
#' @references Valavi R, Elith J, Lahoz-Monfort JJ, Guillera-Arroita G. blockCV:
#' An r package for generating spatially or environmentally separated folds for
#' k-fold cross-validation of species distribution models. Methods Ecol Evol.
#' 2019; 10:225–232. https://doi.org/10.1111/2041-210X.13107
#'
#' @export
#' @examples
#' library(mlr3)
#' task = tsk("ecuador")
#'
#' # Instantiate Resampling
#' rcv = rsmp("spcv-env")
#' rcv$param_set$values = list(folds = 4)
#' rcv$instantiate(task)
#'
#' # Individual sets:
#' rcv$train_set(1)
#' rcv$test_set(1)
#' intersect(rcv$train_set(1), rcv$test_set(1))
#'
#' # Internal storage:
#' rcv$instance
ResamplingSpCVEnv = R6Class("ResamplingSpCVEnv",
  inherit = mlr3::Resampling,
  public = list(
    initialize = function(id = "spcv-env", param_vals = list(folds = 10L)) {
      super$initialize(
        id = id,
        param_set = ParamSet$new(params = list(
          ParamUty$new("stratify", default = NULL),
          ParamInt$new("folds", lower = 1L, tags = "required"),
          ParamUty$new("features")

        )),
        param_vals = param_vals
      )
    },
    instantiate = function(task) {

      assert_task(task)

      # Set values to default if missing
      if (is.null(self$param_set$values$rows)) {
        self$param_set$values$rows = self$param_set$default[["rows"]]
      }
      if (is.null(self$param_set$values$cols)) {
        self$param_set$values$cols = self$param_set$default[["cols"]]
      }
      if (is.null(self$param_set$values$features)) {
        self$param_set$values$features = task$feature_names
      }

      groups = task$groups
      stratify = self$param_set$values$stratify

      if (length(stratify) == 0L || isFALSE(stratify)) {
        if (is.null(groups)) {
          # Remove non-numeric features, target and coordinates
          columns = task$col_info[! id %in% c(task$target_names, "x", "y") & type == "numeric"]

          # Check for selected features that are not in task
          diff = setdiff(self$param_set$values$features, columns[, id])
          if (length(diff) > 0) {
            stop(sprintf("'spcv-env' requires numeric features for clustering. Feature '%s' is either non-numeric or does not exist in the data",
              diff))
          }
          columns = columns[id %in% self$param_set$values$features]
          columns = columns[, id]

          data = task$data()[, columns, with = FALSE]

          instance = private$.sample(task$row_ids, data)
        } else {
          stopf("Grouping is not supported for spatial resampling methods.")
        }
      } else {
        if (!is.null(groups)) {
          stopf("Grouping is not supported for spatial resampling methods")
        }
        stopf("Stratification is not supported for spatial resampling methods.")
      }

      self$instance = instance
      self$task_hash = task$hash
      invisible(self)
    }
  ),

  active = list(
    iters = function() {
      self$param_set$values$folds
    }
  ),

  private = list(
    .sample = function(ids, data) {
      inds = kmeans(data, centers = self$param_set$values$folds)

      data.table(
        row_id = ids,
        fold = inds$cluster,
        key = "fold"
      )
    },

    .get_train = function(i) {
      self$instance[!list(i), "row_id", on = "fold"][[1L]]
    },

    .get_test = function(i) {
      self$instance[list(i), "row_id", on = "fold"][[1L]]
    },

    deep_clone = function(name, value) {
      if (name == "instance") copy(value) else value
    }
  )
)
