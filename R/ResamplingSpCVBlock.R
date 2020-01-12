#' @title Spatial Block Cross Validation Resampling
#'
#' @import mlr3
#'
#' @description Spatial Block Cross validation implemented by the `blockCV`
#' package.
#'
#' @references
#' \cite{mlr3spatiotempcv}{valavi2018}
#' @export
#' @examples
#' library(mlr3)
#' task = tsk("ecuador")
#'
#' # Instantiate Resampling
#' rcv = rsmp("spcv-block")
#' rcv$instantiate(task)
#'
#' # Individual sets:
#' rcv$train_set(1)
#' rcv$test_set(1)
#' intersect(rcv$train_set(1), rcv$test_set(1))
#'
#' # Internal storage:
#' rcv$instance
ResamplingSpCVBlock = R6Class("ResamplingSpCVBlock",
  inherit = mlr3::Resampling,

  public = list(
    #' @description
    #' Create an "Environmental Block" resampling instance.
    #' @param id `character(1)`\cr
    #'   Identifier for the resampling strategy.
    initialize = function(id = "spcv-block") {
      ps = ParamSet$new(params = list(
        ParamUty$new("stratify", default = NULL),
        ParamInt$new("folds", lower = 1L, tags = "required"),
        ParamInt$new("rows", lower = 1L, default = 4),
        ParamInt$new("cols", lower = 1L, default = 4),
        ParamInt$new("range", lower = 1L),
        ParamFct$new("selection", levels = c("random", "systematic",
          "checkerboard"), default = "random")
      ))
      ps$values = list(folds = 10L)
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
      pv = self$param_set$values

      # Check combination
      if (!is.null(pv$range) && (!is.null(pv$rows) || !is.null(pv$cols))) {
        warning("Cols and rows are ignored. Range is used to generated blocks.")
      }

      # Set values to default if missing
      if (is.null(pv$rows) & is.null(pv$range)) {
        self$param_set$values$rows = self$param_set$default[["rows"]]
      }
      if (is.null(pv$cols) & is.null(pv$range)) {
        self$param_set$values$cols = self$param_set$default[["cols"]]
      }
      if (is.null(self$param_set$selection)) {
        self$param_set$values$selection = self$param_set$default[["selection"]]
      }

      # Check for valid combinations of rows, cols and folds
      if ((self$param_set$values$rows * self$param_set$values$cols) < self$param_set$values$folds) {
        stopf("'nrow' * 'ncol' needs to be larger than 'folds'.")
      }

      if (!is.null(self$param_set$values$rows) && !is.null(self$param_set$values$cols)) {
        warning("Hyperparameters 'rows' and 'cols' not set. Using the default value of '4' set by 'mlr3spatiotempcv' for both which results in a grid of 16. You might want to set these values yourself during resampling construction.")
      }

      groups = task$groups
      stratify = self$param_set$values$stratify

      if (length(stratify) == 0L || isFALSE(stratify)) {
        if (is.null(groups)) {
          instance = private$.sample(task$row_ids, task$coordinates())
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
    #' @field iters `integer(1)`\cr
    #'   Returns the number of resampling iterations, depending on the
    #'   values stored in the `param_set`.
    iters = function() {
      as.integer(self$param_set$values$folds)
    }
  ),

  private = list(
    .sample = function(ids, coords) {
      points = sf::st_as_sf(coords, coords = c("x", "y"))

      # Suppress print message, warning crs and package load
      capture.output(inds <- suppressMessages(suppressWarnings(blockCV::spatialBlock(
        speciesData = points,
        theRange = self$param_set$values$range,
        rows = self$param_set$values$rows,
        cols = self$param_set$values$cols,
        k = self$param_set$values$folds,
        selection = self$param_set$values$selection,
        showBlocks = FALSE,
        progress = FALSE))))

      data.table(
        row_id = ids,
        fold = inds$foldID,
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
