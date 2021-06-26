#' @title (blockCV) Spatial block resampling
#'
#' @template rox_spcv_block
#'
#' @references
#' `r format_bib("valavi2018")`
#'
#' @importFrom utils capture.output
#'
#' @export
#' @examples
#' if (mlr3misc::require_namespaces(c("sf", "blockCV"), quietly = TRUE)) {
#'   library(mlr3)
#'   task = tsk("ecuador")
#'
#'   # Instantiate Resampling
#'   rcv = rsmp("spcv_block", range = 1000L)
#'   rcv$instantiate(task)
#'
#'   # Individual sets:
#'   rcv$train_set(1)
#'   rcv$test_set(1)
#'   intersect(rcv$train_set(1), rcv$test_set(1))
#'
#'   # Internal storage:
#'   rcv$instance
#' }
ResamplingSpCVBlock = R6Class("ResamplingSpCVBlock",
  inherit = mlr3::Resampling,

  public = list(

    #' @field blocks `sf | list of sf objects`\cr
    #'  Polygons (`sf` objects) as returned by \pkg{blockCV} which grouped
    #'  observations into partitions.
    blocks = NULL,

    #' @description
    #' Create an "spatial block" resampling instance.
    #'
    #' For a list of available arguments, please see
    #' [blockCV::spatialBlock()].
    #' @param id `character(1)`\cr
    #'   Identifier for the resampling strategy.
    initialize = function(id = "spcv_block") {
      ps = ParamSet$new(params = list(
        ParamInt$new("folds", lower = 1L, default = 10L, tags = "required"),
        ParamInt$new("rows", lower = 1L),
        ParamInt$new("cols", lower = 1L),
        ParamInt$new("range"),
        ParamFct$new("selection", levels = c(
          "random", "systematic",
          "checkerboard"), default = "random"),
        ParamUty$new("rasterLayer",
          default = NULL,
          custom_check = function(x) {
            checkmate::check_class(x, "RasterLayer",
              null.ok = TRUE)
          }
        )
      ))
      ps$values = list(folds = 10L)
      super$initialize(
        id = id,
        param_set = ps,
        man = "mlr3spatiotempcv::mlr_resamplings_spcv_block"
      )
      mlr3misc::require_namespaces(c("blockCV", "sf"))
    },

    #' @description
    #'  Materializes fixed training and test splits for a given task.
    #' @param task [Task]\cr
    #'  A task to instantiate.
    instantiate = function(task) {

      mlr3::assert_task(task)
      checkmate::assert_multi_class(task, c("TaskClassifST", "TaskRegrST"))
      pv = self$param_set$values

      if (!is.null(pv$range)) {
      } else if (is.null(pv$range) && is.null(pv$rows) | is.null(pv$cols)) {
        stopf("Either 'range' or 'cols' & 'rows' need to be set.")
      }

      if (!is.null(pv$range) && (!is.null(pv$rows) | !is.null(pv$cols))) { # nocov start
        messagef("'spcv_block': Arguments 'cols' and 'rows' will be ignored.
                 'range' is used to generate blocks.", wrap = TRUE)
      } # nocov end

      # Set values to default if missing
      if (is.null(pv$rows) & is.null(pv$range)) {
        self$param_set$values$rows = self$param_set$default[["rows"]] # nocov
      }
      if (is.null(pv$cols) & is.null(pv$range)) {
        self$param_set$values$cols = self$param_set$default[["cols"]] # nocov
      }
      if (is.null(self$param_set$values$selection)) {
        self$param_set$values$selection = self$param_set$default[["selection"]]
      }
      if (is.null(self$param_set$values$rasterLayer)) {
        self$param_set$values$rasterLayer = self$param_set$default[["rasterLayer"]]
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
      instance = private$.sample(
        task$row_ids,
        task$coordinates(),
        task$extra_args$crs
      )

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
    .sample = function(ids, coords, crs) {

      points = sf::st_as_sf(coords,
        coords = colnames(coords),
        crs = crs
      )
      # Suppress print message, warning crs and package load
      # Note: Do not replace the assignment operator here.
      capture.output(inds <- suppressMessages((
        blockCV::spatialBlock(
          speciesData = points,
          theRange = self$param_set$values$range,
          rows = self$param_set$values$rows,
          cols = self$param_set$values$cols,
          k = self$param_set$values$folds,
          rasterLayer = self$param_set$values$rasterLayer,
          selection = self$param_set$values$selection,
          showBlocks = FALSE,
          progress = FALSE,
          verbose = FALSE))))

      # Warning: In st_point_on_surface.sfc(sf::st_zm(x)) :
      # st_point_on_surface may not give correct results for
      # longitude/latitude data
      blocks_sf = suppressWarnings(sf::st_as_sf(inds$blocks))

      self$blocks = blocks_sf

      # to be able to plot the spatial block later in autoplot(),
      # we need to return them here
      data.table(
        row_id = ids,
        fold = inds$foldID
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
