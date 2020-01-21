#' @title Spatial Buffer Cross Validation Resampling
#'
#' @import mlr3
#'
#' @description Spatial Buffer Cross validation implemented by the `blockCV`
#' package.
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
#' rcv = rsmp("spcv-buffer", range = 1000)
#' rcv$instantiate(task)
#'
#' # Individual sets:
#' rcv$train_set(1)
#' rcv$test_set(1)
#' intersect(rcv$train_set(1), rcv$test_set(1))
#'
#' # Internal storage:
#' rcv$instance
ResamplingSpCVBuffer = R6Class("ResamplingSpCVBuffer",
  inherit = mlr3::Resampling,

  public = list(
    #' @description
    #' Create an "Environmental Block" resampling instance.
    #' @param id `character(1)`\cr
    #'   Identifier for the resampling strategy.
    initialize = function(id = "spcv-buffer") {
      ps = ParamSet$new(params = list(
        ParamInt$new("range", lower = 1L, tags = "required")
      ))
      ps$values = list(range = 100)
      super$initialize(
        id = id,
        param_set = ps
      )
      require_namespaces(c("blockCV", "sf"))
    },

    #' @description
    #'  Materializes fixed training and test splits for a given task.
    #' @param task [Task]\cr
    #'  A task to instantiate.
    instantiate = function(task) {

      assert_task(task)

      groups = task$groups


      if (!is.null(groups)) {
        stopf("Grouping is not supported for spatial resampling methods")
      }

      instance = private$.sample(task$row_ids, task$coordinates(), task$crs)

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
      as.integer(length(self$instance$fold))
    }
  ),

  private = list(
    .sample = function(ids, coords, crs) {

      require_namespaces(c("blockCV", "sf"))

      points = sf::st_as_sf(coords,
        coords = c("x", "y"),
        crs = crs)

      inds = blockCV::buffering(
        speciesData = points,
        theRange = self$param_set$values$range,
        progress = FALSE
      )

      inds = map(inds$folds, function(x) {
        set = map(x, function(y) {
          ids[y]
        })
        names(set) = c("train", "test")
        set
      })
      test_inds = map_int(inds, function(x) as.integer(x[["test"]]))

      data.table(
        row_id = seq(1:length(test_inds)),
        fold = test_inds,
        key = "fold"
      )
    },

    .get_train = function(i) {
      self$instance[!list(i), "row_id", on = "fold"][[1L]]
    },

    .get_test = function(i) {
      self$instance[list(i), "row_id", on = "fold"][[1L]]
    }
  )
)
