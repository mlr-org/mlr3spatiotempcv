#' @title (blockCV) Spatial block resampling
#'
#' @template rox_spcv_block
#' @name mlr_resamplings_spcv_block
#'
#' @references
#' `r format_bib("valavi2018")`
#'
#' @export
#' @examples
#' \donttest{
#' if (mlr3misc::require_namespaces(c("sf", "blockCV"), quietly = TRUE)) {
#'   library(mlr3)
#'   task = tsk("ecuador")
#'
#'   # Instantiate Resampling
#'   rcv = rsmp("spcv_block", range = 3000L, folds = 3)
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
    #' [blockCV::cv_spatial()].
    #' @param id `character(1)`\cr
    #'   Identifier for the resampling strategy.
    initialize = function(id = "spcv_block") {
      ps = ps(
        folds = p_int(lower = 1L, default = 10L, tags = "required"),
        rows = p_int(lower = 1L),
        cols = p_int(lower = 1L),
        ParamInt$new("range"),
        ParamInt$new("seed"),
        hexagon = p_lgl(default = FALSE),
        selection = p_fct(levels = c(
          "random", "systematic",
          "checkerboard"), default = "random"),
        ParamUty$new("rasterLayer",
          default = NULL,
          custom_check = function(x) {
            checkmate::check_class(x, "SpatRaster",
              null.ok = TRUE)
          }
        )
      )
      ps$values = list(folds = 10L)
      super$initialize(
        id = id,
        param_set = ps,
        label = "Spatial block resampling",
        man = "mlr3spatiotempcv::mlr_resamplings_spcv_block"
      )
    },

    #' @description
    #'  Materializes fixed training and test splits for a given task.
    #' @param task [Task]\cr
    #'  A task to instantiate.
    instantiate = function(task) {
      mlr3misc::require_namespaces(c("blockCV", "sf"))

      mlr3::assert_task(task)
      assert_spatial_task(task)
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
      if (is.null(pv$hexagon)) {
        self$param_set$values$hexagon = self$param_set$default[["hexagon"]]
      }

      # Check for valid combinations of rows, cols and folds
      if (!is.null(self$param_set$values$row) &&
        !is.null(self$param_set$values$cols)) {
        if ((self$param_set$values$rows * self$param_set$values$cols) <
          self$param_set$values$folds) {
          stopf("'nrow' * 'ncol' needs to be larger than 'folds'.") # nocov
        }
      }
      # checkerboard option only allows for two folds
      if (self$param_set$values$selection == "checkerboard" &&
        pv$folds > 2) {
        mlr3misc::stopf("'selection = checkerboard' only allows for two folds.
          Setting argument 'folds' to a value larger than would result in an empty test set.",
          wrap = TRUE)
      }

      groups = task$groups

      if (!is.null(groups)) {
        stopf("Grouping is not supported for spatial resampling methods.")
      }
      instance = private$.sample(
        task$row_ids,
        task$coordinates(),
        task$crs,
        self$param_set$values$seed
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
    .sample = function(ids, coords, crs, seed) {

      points = sf::st_as_sf(coords,
        coords = colnames(coords),
        crs = crs
      )
      # Suppress print message, warning crs and package load
      # Note: Do not replace the assignment operator here.
      inds = blockCV::cv_spatial(
        x = points,
        size = self$param_set$values$range,
        rows_cols = c(self$param_set$values$rows, self$param_set$values$cols),
        k = self$param_set$values$folds,
        r = self$param_set$values$rasterLayer,
        selection = self$param_set$values$selection,
        hexagon = self$param_set$values$hexagon,
        plot = FALSE,
        progress = FALSE,
        report = FALSE,
        verbose = FALSE,
        seed = seed)

      # Warning: In st_point_on_surface.sfc(sf::st_zm(x)) :
      # st_point_on_surface may not give correct results for
      # longitude/latitude data
      blocks_sf = suppressWarnings(sf::st_as_sf(inds$blocks))

      self$blocks = blocks_sf

      # to be able to plot the spatial block later in autoplot(),
      # we need to return them here
      data.table(
        row_id = ids,
        fold = inds$folds_ids
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

#' @include aaa.R
resamplings[["spcv_block"]] = ResamplingSpCVBlock
