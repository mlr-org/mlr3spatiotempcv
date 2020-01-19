#' @title Environmental Block Cross Validation Resampling
#'
#' @import mlr3
#'
#' @description
#' Environmental Block Cross Validation. This strategy uses k-means clustering
#' to specify blocks of similar environmental conditions. Only numeric features
#' can be used. The `features` used for building blocks can be specified in the
#' `param_set`. By default, all numeric features are used.
#'
#' @references
#' \cite{mlr3spatiotempcv}{valavi2018}
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
ResamplingSpCVEnv = R6Class("ResamplingSpCVEnv", inherit = mlr3::Resampling,

  public = list(
    #' @description
    #' Create an "Environmental Block" resampling instance.
    #' @param id `character(1)`\cr
    #'   Identifier for the resampling strategy.
    initialize = function(id = "spcv-env") {
      ps = ParamSet$new(params = list(
        ParamInt$new("folds", lower = 1L, tags = "required"),
        ParamUty$new("features")
      ))
      ps$values = list(folds = 10L)
      super$initialize(
        id = id,
        param_set = ps
      )
    },

    #' @description
    #'  Materializes fixed training and test splits for a given task.
    #' @param task [Task]\cr
    #'  A task to instantiate.
    instantiate = function(task) {

      assert_task(task)
      pv = self$param_set$values

      # Set values to default if missing
      if (is.null(pv$rows)) {
        pv$rows = self$param_set$default[["rows"]]
      }
      if (is.null(pv$cols)) {
        pv$cols = self$param_set$default[["cols"]]
      }
      if (is.null(pv$features)) {
        pv$features = task$feature_names
      }

      groups = task$groups

      if (!is.null(groups)) {
        stopf("Grouping is not supported for spatial resampling methods")
      }

      # Remove non-numeric features, target and coordinates
      columns = task$col_info[!id %in%
        c(task$target_names, "x", "y")][type == "numeric"]

      # Check for selected features that are not in task
      diff = setdiff(pv$features, columns[, id])
      if (length(diff) > 0) {
        stopf("'spcv-env' requires numeric features for clustering.
              Feature '%s' is either non-numeric or does not exist in the data.",
          diff, wrap = TRUE)
      }
      columns = columns[id %in% pv$features]
      columns = columns[, id]

      data = task$data()[, columns, with = FALSE]

      instance = private$.sample(task$row_ids, data)

      self$instance = instance
      self$task_hash = task$hash
      invisible(self)
    }
  ),

  active = list(
    #' @field iters `integer(1)`\cr
    #'   Returns the number of resampling iterations, depending on the
    #'   values stored in the `param_set`.
    iters = function() {
      as.integer(self$param_set$values$folds)
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
    # private get funs for train and test which are used by
    # Resampling$.get_set()
    .get_train = function(i) {
      self$instance[!list(i), "row_id", on = "fold"][[1L]]
    },

    .get_test = function(i) {
      self$instance[list(i), "row_id", on = "fold"][[1L]]
    }
  )
)
