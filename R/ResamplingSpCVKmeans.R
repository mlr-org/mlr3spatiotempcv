#' @title Spatial Cross Validation Resampling
#'
#' @format [R6::R6Class] inheriting from [Resampling].
#' @import mlr3
#'
#' @description
#' Spatial Cross validation following the "k-means" approach after Brenning 2012.
#'
#' @section Fields:
#' See [Resampling].
#'
#' @section Methods:
#' See [Resampling].
#'
#' @references
#' Brenning, A. (2012). Spatial cross-validation and bootstrap for the
#' assessment of prediction rules in remote sensing: The R package sperrorest.
#' 2012 IEEE International Geoscience and Remote Sensing Symposium.
#' https://doi.org/10.1109/igarss.2012.6352393
#'
#' @export
#' @examples
#' task <- mlr3::mlr_tasks$get("ecuador")
#'
#' # Instantiate Resampling
#' rcv <- mlr3::mlr_resamplings$get("spcv-kmeans")
#' rcv$param_set$values <- list(folds = 3)
#' rcv$instantiate(task)
#'
#' # Individual sets:
#' rcv$train_set(1)
#' rcv$test_set(1)
#' intersect(rcv$train_set(1), rcv$test_set(1))
#'
#' # Internal storage:
#' rcv$instance # table
ResamplingSpCVKmeans <- R6Class("ResamplingSpCVKmeans",
  inherit = mlr3::Resampling,
  public = list(
    initialize = function(id = "spcv-kmeans", param_vals = list(folds = 10L)) {
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
      groups <- task$groups


      stratify <- self$param_set$values$stratify

      if (length(stratify) == 0L || isFALSE(stratify)) {
        if (is.null(groups)) {
          instance <- private$.sample(task$row_ids, task$coordinates())
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
    .sample = function(ids, coords) {
      # Align coords and row_id order
      coords$row_id <- as.character(1:nrow(coords))
      setorder(coords, row_id)
      coords = coords[,c("x", "y")]

      inds <- kmeans(coords, centers = self$param_set$values$folds)

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
