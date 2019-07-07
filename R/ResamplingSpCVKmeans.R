#' @title Spatial Cross Validation Resampling
#'
#' @format [R6::R6Class] inheriting from [Resampling].
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
#' # Create a task with 10 observations
#' task = mlr_tasks$get("ecuador")
#' task$filter(1:10)
#'
#' # Instantiate Resampling
#' rcv = mlr_resamplings$get("spcv-kmeans")
#' rcv$param_set$values = list(folds = 3)
#' rcv$instantiate(task)
#'
#' # Individual sets:
#' rcv$train_set(1)
#' rcv$test_set(1)
#' intersect(rcv$train_set(1), rcv$test_set(1))
#'
#' # Internal storage:
#' rcv$instance # table
ResamplingSpCVKmeans = R6Class("ResamplingSpCVKmeans", inherit = mlr3::Resampling,
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
      groups = task$groups

      if (!is.null(task$col_roles$coordinates)) {
        task$set_col_role(c("x", "y"), "feature")
        coords = as.data.table(task$data(cols = c("x", "y")))
        task$set_col_role(c("x", "y"), "coordinates")
      }

      stratify = self$param_set$values$stratify
      if (length(stratify) == 0L || isFALSE(stratify)) {
        if (is.null(groups)) {
          instance = private$.sample(task$row_ids, coords)
        } else {
          private$.groups = groups
          instance = private$.sample(unique(groups$group, coords))
        }
      } else {
        if (!is.null(groups)) {
          stopf("Cannot combine stratification with grouping")
        }
        instances = stratify(task, stratify)
        instance = private$.combine(lapply(instances$..row_id, function(x) private$.sample(x, coords)))
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
    .sample = function(ids, coords) {

      inds = kmeans(coords, centers = self$param_set$values$folds)
      inds = factor(inds$cluster)

      # uses resulting factor levels from kmeans clustering to set up a list of
      # length x (x = folds) with row indices of the data referring to which fold
      # each observation is assigned to
      ids = lapply(levels(inds), function(x, spl)
        which(spl == x), spl = inds)

      data.table(
        row_id = ids,
        fold = seq_along0(ids) %% self$param_set$values$folds + 1L,
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
