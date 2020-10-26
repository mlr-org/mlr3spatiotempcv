#' @title Spatial Block Cross Validation Resampling
#'
#' @template rox_spcv_block
#'
#' @references
#' `r tools::toRd(bibentries["valavi2018"])`
#'
#' @export
#' @examples
#' library(mlr3)
#' task = tsk("ecuador")
#'
#' # Instantiate Resampling
#' rcv = rsmp("spcv_block", range = 1000)
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
    initialize = function(id = "spcv_block") {
      ps = ParamSet$new(params = list(
        ParamInt$new("folds", lower = 1L, default = 10L, tags = "required"),
        ParamInt$new("rows", lower = 1L),
        ParamInt$new("cols", lower = 1L),
        ParamInt$new("range", lower = 1L),
        ParamFct$new("selection", levels = c(
          "random", "systematic",
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
      checkmate::assert_multi_class(task, c("TaskClassifST", "TaskRegrST"))
      pv = self$param_set$values

      if (!is.null(pv$range)) {
      } else if (is.null(pv$range) && is.null(pv$rows) | is.null(pv$cols)) {
        stopf("Either 'range' or 'cols' & 'rows' need to be set.")
      }

      if (!is.null(pv$range) && (!is.null(pv$rows) | !is.null(pv$cols))) { # nocov start
        cli::cli_alert_warning("{.field spcv_block}: Arguments
          {.code cols} and {.code rows} will be ignored. {.code range} is used
          to generated blocks.", wrap = TRUE)
      } # nocov end

      # Set values to default if missing
      if (is.null(pv$rows) & is.null(pv$range)) {
        self$param_set$values$rows = self$param_set$default[["rows"]] # nocov
      }
      if (is.null(pv$cols) & is.null(pv$range)) {
        self$param_set$values$cols = self$param_set$default[["cols"]] # nocov
      }
      if (is.null(self$param_set$selection)) {
        self$param_set$values$selection = self$param_set$default[["selection"]]
      }

      # Check for valid combinations of rows, cols and folds
      if (!is.null(self$param_set$values$row) &&
        !is.null(self$param_set$values$cols)) {
        if ((self$param_set$values$rows * self$param_set$values$cols) <
          self$param_set$values$folds) {
          stopf("'nrow' * 'ncol' needs to be larger than 'folds'.") # nocov
        }
      }

      groups = task$groups

      if (!is.null(groups)) {
        stopf("Grouping is not supported for spatial resampling methods.")
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
      as.integer(self$param_set$values$folds)
    }
  ),

  private = list(
    .sample = function(ids, coords) {
      points = sf::st_as_sf(coords, coords = c("x", "y"))

      # Suppress print message, warning crs and package load
      # Note: Do not replace the assignment operator here.
      capture.output(inds <- suppressMessages(suppressWarnings(
        blockCV::spatialBlock(
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
