#' @title Repeated Spatial Cross Validation Resampling
#'
#' @import mlr3
#'
#' @description Spatial Block Cross validation implemented by the `blockCV`
#' package.
#'
#' @references
#' \cite{mlr3spatiotempcv}{valavi2018}
#'
#' @details
#'
#' By default [blockCV::spatialBlock()] does not allow the creation of multiple
#' repetitions. `mlr3spatiotempcv` adds support for this when using the `range`
#' argument for fold creation. When supplying a vector of `length(repeats)` for
#' argument `range`, these different settings will be used to create folds which
#' differ among the repetitions.
#'
#' Multiple repetitions are not possible when using the "row & cols" approach
#' because the created folds will always be the same.
#'
#' @export
#' @examples
#' library(mlr3)
#' task = tsk("diplodia")
#'
#' # Instantiate Resampling
#' rrcv = rsmp("repeated-spcv-block",
#'   folds = 3, repeats = 2,
#'   range = c(1000, 1500))
#' rrcv$instantiate(task)
#'
#' # Individual sets:
#' rrcv$iters
#' rrcv$folds(1:6)
#' rrcv$repeats(1:6)
#'
#' # Individual sets:
#' rrcv$train_set(1)
#' rrcv$test_set(1)
#' intersect(rrcv$train_set(1), rrcv$test_set(1))
#'
#' # Internal storage:
#' rrcv$instance # table
ResamplingRepeatedSpCVBlock = R6Class("ResamplingRepeatedSpCVBlock",
  inherit = mlr3::Resampling,

  public = list(
    #' @description
    #' Create an "coordinate-based" repeated resampling instance.
    #' @param id `character(1)`\cr
    #'   Identifier for the resampling strategy.
    initialize = function(id = "repeated-spcv-block") {
      ps = ParamSet$new(params = list(
        ParamInt$new("folds", lower = 1L, default = 10L, tags = "required"),
        ParamInt$new("repeats", lower = 1, default = 1L, tags = "required"),
        ParamInt$new("rows", lower = 1L),
        ParamInt$new("cols", lower = 1L),
        ParamUty$new("range"),
        ParamFct$new("selection", levels = c(
          "random", "systematic",
          "checkerboard"), default = "random")
      ))

      ps$values = list(folds = 10L, repeats = 1L)
      super$initialize(
        id = id,
        param_set = ps,
        man = "mlr3spatiotempcv::mlr_resamplings_repeated_spcv_block"
      )

    },

    #' @description Translates iteration numbers to fold number.
    #' @param iters `integer()`\cr
    #'   Iteration number.
    folds = function(iters) {
      iters = assert_integerish(iters, any.missing = FALSE, coerce = TRUE)
      ((iters - 1L) %% as.integer(self$param_set$values$repeats)) + 1L
    },

    #' @description Translates iteration numbers to repetition number.
    #' @param iters `integer()`\cr
    #'   Iteration number.
    repeats = function(iters) {
      iters = assert_integerish(iters, any.missing = FALSE, coerce = TRUE)
      ((iters - 1L) %/% as.integer(self$param_set$values$folds)) + 1L
    },

    #' @description
    #'  Materializes fixed training and test splits for a given task.
    #' @param task [Task]\cr
    #'  A task to instantiate.
    instantiate = function(task) {

      assert_task(task)
      pv = self$param_set$values
      assert_numeric(pv$repeats, min.len = 1)

      if (!is.null(pv$range)) {
        # We check if repeats == length(range) because for each repetition a
        # different range is needed to create different folds
        if (!(length(pv$range) == pv$repeats)) {
          stopf("When doing repeated block resampling using 'range', argument
              'repeats' needs to be the same length as 'range'.", wrap = TRUE)
        }
      } else if (is.null(pv$range) && is.null(pv$rows) | is.null(pv$cols)) {
        stopf("Either 'range' or 'cols' & 'rows' need to be set.")
      }

      if (!is.null(pv$range) && (!is.null(pv$rows) | !is.null(pv$cols))) {
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
      if (!is.null(self$param_set$values$row) &&
        !is.null(self$param_set$values$cols)) {
        if ((self$param_set$values$rows * self$param_set$values$cols) <
          self$param_set$values$folds) {
          stopf("'nrow' * 'ncol' needs to be larger than 'folds'.")
        }
      }

      groups = task$groups

      if (!is.null(groups)) {
        stopf("Grouping is not supported for spatial resampling methods.")
      }
      instance = private$.sample(task$row_ids, task$coordinates())

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
      pv = self$param_set$values
      as.integer(pv$repeats) * as.integer(pv$folds)
    }
  ),

  private = list(
    .sample = function(ids, coords) {
      pv = self$param_set$values
      folds = as.integer(pv$folds)

      create_blocks = function(coords, range) {
        points = sf::st_as_sf(coords, coords = c("x", "y"))

        # Suppress print message, warning crs and package load
        capture.output(inds <- suppressMessages(suppressWarnings(
          blockCV::spatialBlock(
            speciesData = points,
            theRange = range,
            rows = self$param_set$values$rows,
            cols = self$param_set$values$cols,
            k = self$param_set$values$folds,
            selection = self$param_set$values$selection,
            showBlocks = FALSE,
            progress = FALSE)$foldID)))
        return(inds)
      }

      map_dtr(seq_len(pv$repeats), function(i) {
        data.table(
          row_id = ids, rep = i,
          fold = create_blocks(coords, self$param_set$values$range[i])
        )
      })
    },

    .get_train = function(i) {
      i = as.integer(i) - 1L
      folds = as.integer(self$param_set$values$folds)
      rep = i %/% folds + 1L
      fold = i %% folds + 1L
      ii = data.table(rep = rep, fold = seq_len(folds)[-fold])
      self$instance[ii, "row_id", on = names(ii), nomatch = 0L][[1L]]
    },

    .get_test = function(i) {
      i = as.integer(i) - 1L
      folds = as.integer(self$param_set$values$folds)
      rep = i %/% folds + 1L
      fold = i %% folds + 1L
      ii = data.table(rep = rep, fold = fold)
      self$instance[ii, "row_id", on = names(ii), nomatch = 0L][[1L]]
    }
  )
)
