#' @title Environmental Block Cross Validation Resampling
#'
#' @format [R6::R6Class] inheriting from [Resampling].
#' @import mlr3
#'
#' @description
#' Environmental Block Cross Validation.
#'
#' @section Fields:
#' See [Resampling].
#'
#' @section Methods:
#' See [Resampling].
#'
#' @references
#' Valavi R, Elith J, Lahoz-Monfort JJ, Guillera-Arroita G. blockCV: An r package for generating spatially or environmentally separated folds for k-fold cross-validation of species distribution models. Methods Ecol Evol. 2019; 10:225–232. https://doi.org/10.1111/2041-210X.13107
#'
#' @export
#' @examples
#' task <- mlr3::mlr_tasks$get("ecuador")
#'
#' # Instantiate Resampling
#' rcv <- mlr3::mlr_resamplings$get("spcv-env")
#' rcv$param_set$values <- list(folds = 4))
#' rcv$instantiate(task)
#'
#' # Individual sets:
#' rcv$train_set(1)
#' rcv$test_set(1)
#' intersect(rcv$train_set(1), rcv$test_set(1))
#'
#' # Internal storage:
#' rcv$instance
ResamplingSpCVEnv <- R6Class("ResamplingSpCVEnv",
  inherit = mlr3::Resampling,
  public = list(
    initialize = function(id = "spcv-env", param_vals = list(folds = 10L)) {
      super$initialize(
        id = id,
        param_set = ParamSet$new(params = list(
          ParamUty$new("stratify", default = NULL),
          ParamInt$new("folds", lower = 1L, tags = "required")
        )),
        param_vals = param_vals
      )
    },
    instantiate = function(task) {
      assert_task(task)

      # Set values to default if missing
      if (is.null(self$param_set$values$rows)) {
        self$param_set$values$rows <- self$param_set$default[["rows"]]
      }
      if (is.null(self$param_set$values$cols)) {
        self$param_set$values$cols <- self$param_set$default[["cols"]]
      }

      groups <- task$groups
      stratify <- self$param_set$values$stratify

      if (length(stratify) == 0L || isFALSE(stratify)) {
        if (is.null(groups)) {
          # Remove non-mnumeric features, target and coordinates
          columns <- task$col_info
          columns <- columns[id != task$target_names]
          columns <- columns[type == "numeric"]
          columns <- columns[!id %in% c("x", "y")]
          columns <- columns[, id]
          data <- task$data()[, columns, with = FALSE]

          instance <- private$.sample(task$row_ids, data)
        } else {
          stopf("Grouping is not supported for spatial resampling methods.", call. = FALSE)
        }
      } else {
        if (!is.null(groups)) {
          stopf("Grouping is not supported for spatial resampling methods", call. = FALSE)
        }
        stopf("Stratification is not supported for spatial resampling methods.", call. = FALSE)
      }

      self$instance <- instance
      self$task_hash <- task$hash
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
      inds <- kmeans(data, centers = self$param_set$values$folds)

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

    .combine = function(instances) {
      rbindlist(instances, use.names = TRUE)
    },

    deep_clone = function(name, value) {
      if (name == "instance") copy(value) else value
    }
  )
)
