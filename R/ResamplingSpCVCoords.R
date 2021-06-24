#' @title (sperrorest) Coordinate-based k-means clustering
#'
#' @template rox_spcv_coords
#'
#' @references
#' `r format_bib("brenning2012")`
#'
#' @export
#' @examples
#' library(mlr3)
#' task = tsk("ecuador")
#'
#' # Instantiate Resampling
#' rcv = rsmp("spcv_coords", folds = 5)
#' rcv$instantiate(task)
#'
#' # Individual sets:
#' rcv$train_set(1)
#' rcv$test_set(1)
#' # check that no obs are in both sets
#' intersect(rcv$train_set(1), rcv$test_set(1)) # good!
#'
#' # Internal storage:
#' rcv$instance # table
ResamplingSpCVCoords = R6Class("ResamplingSpCVCoords",
  inherit = mlr3::Resampling,

  public = list(
    #' @description
    #' Create an "coordinate-based" repeated resampling instance.
    #'
    #' For a list of available arguments, please see [sperrorest::partition_cv].
    #' @param id `character(1)`\cr
    #'   Identifier for the resampling strategy.
    initialize = function(id = "spcv_coords") {
      ps = ParamSet$new(params = list(
        ParamInt$new("folds", lower = 1L, default = 10L, tags = "required")
      ))
      ps$values = list(folds = 10L)
      super$initialize(
        id = id,
        param_set = ps,
        man = "mlr3spatiotempcv::mlr_resamplings_spcv_coords"
      )
    },

    #' @description
    #'  Materializes fixed training and test splits for a given task.
    #' @param task [Task]\cr
    #'  A task to instantiate.
    instantiate = function(task) {

      mlr3::assert_task(task)
      checkmate::assert_multi_class(task, c("TaskClassifST", "TaskRegrST"))
      groups = task$groups


      if (!is.null(groups)) {
        stopf("Grouping is not supported for spatial resampling methods")
      }

      instance = private$.sample(task$row_ids, task$coordinates())

      self$instance = instance
      self$task_hash = task$hash
      self$task_nrow = task$nrow
      invisible(self)
    }
  ),
  active = list(
    #' @field iters `integer(1)`\cr
    #'   Returns the number of resampling iterations, depending on the
    #'   values stored in the `param_set`.
    iters = function() {
      self$param_set$values$folds
    }
  ),
  private = list(
    .sample = function(ids, coords) {
      inds = kmeans(coords, centers = self$param_set$values$folds)

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
