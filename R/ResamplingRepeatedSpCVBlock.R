#' @title (blockCV) Repeated spatial block resampling
#'
#' @template rox_spcv_block
#'
#' @references
#' `r format_bib("valavi2018")`
#'
#' @export
#' @examples
#' if (mlr3misc::require_namespaces(c("sf", "blockCV"), quietly = TRUE)) {
#'   library(mlr3)
#'   task = tsk("diplodia")
#'
#'   # Instantiate Resampling
#'   rrcv = rsmp("repeated_spcv_block",
#'     folds = 3, repeats = 2,
#'     range = c(5000L, 10000L))
#'   rrcv$instantiate(task)
#'
#'   # Individual sets:
#'   rrcv$iters
#'   rrcv$folds(1:6)
#'   rrcv$repeats(1:6)
#'
#'   # Individual sets:
#'   rrcv$train_set(1)
#'   rrcv$test_set(1)
#'   intersect(rrcv$train_set(1), rrcv$test_set(1))
#'
#'   # Internal storage:
#'   rrcv$instance # table
#' }
ResamplingRepeatedSpCVBlock = R6Class("ResamplingRepeatedSpCVBlock",
  inherit = mlr3::Resampling,
  public = list(

    #' @field blocks `sf | list of sf objects`\cr
    #'  Polygons (`sf` objects) as returned by \pkg{blockCV} which grouped
    #'  observations into partitions.
    blocks = NULL,

    #' @description
    #' Create an "spatial block" repeated resampling instance.
    #'
    #' For a list of available arguments, please see [blockCV::spatialBlock].
    #' @param id `character(1)`\cr
    #'   Identifier for the resampling strategy.
    initialize = function(id = "repeated_spcv_block") {
      ps = ParamSet$new(params = list(
        ParamInt$new("folds", lower = 1L, default = 10L, tags = "required"),
        ParamInt$new("repeats", lower = 1, default = 1L, tags = "required"),
        ParamInt$new("rows", lower = 1L),
        ParamInt$new("cols", lower = 1L),
        ParamUty$new("range",
          custom_check = function(x) checkmate::assert_integer(x)),
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
      ((iters - 1L) %% as.integer(self$param_set$values$folds)) + 1L
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

      mlr3::assert_task(task)
      checkmate::assert_multi_class(task, c("TaskClassifST", "TaskRegrST"))
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
        # nocov start
        warningf("'repeated_spcv_block: Arguments
          {.code cols} and {.code rows} will be ignored. {.code range} is used
          to generated blocks.", wrap = TRUE)
        # nocov end
      }

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
        # nocov start
        if ((self$param_set$values$rows * self$param_set$values$cols) <
          self$param_set$values$folds) {
          stopf("'nrow' * 'ncol' needs to be larger than 'folds'.")
        }
        # nocov end
      }

      groups = task$groups

      if (!is.null(groups)) {
        stopf("Grouping is not supported for spatial resampling methods.") # nocov # nolint
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
      pv = self$param_set$values
      as.integer(pv$repeats) * as.integer(pv$folds)
    }
  ),
  private = list(
    .sample = function(ids, coords, crs) {

      # since blockCV >= 2.1.4 and sf >= 1.0
      requireNamespace("rgdal", quietly = TRUE)

      pv = self$param_set$values

      create_blocks = function(coords, range) {
        points = sf::st_as_sf(coords,
          coords = colnames(coords))

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
            verbose = FALSE,
            progress = FALSE))))
        return(inds)
      }

      inds = mlr3misc::map(seq_len(pv$repeats), function(i) {
        inds = create_blocks(coords, self$param_set$values$range[i])
        blocks = sf::st_as_sf(inds$blocks)

        return(list(resampling = data.table(
          row_id = ids,
          fold = inds$foldID,
          rep = i
        ), blocks = blocks))
      })

      # extract blocks for each repetition
      blocks = mlr3misc::map(inds, function(i) {
        i$blocks
      })

      # assign blocks to field to be able to access them in autoplot()
      self$blocks = blocks

      # extract and row bind indices
      map_dtr(inds, function(i) {
        i$resampling
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
