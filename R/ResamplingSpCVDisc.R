#' @title (sperrorest) Spatial "disc" resampling
#'
#' @template rox_spcv_disc
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
#' rcv = rsmp("spcv_disc", folds = 3L, radius = 200L, buffer = 200L)
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
ResamplingSpCVDisc = R6Class("ResamplingSpCVDisc",
  inherit = mlr3::Resampling,

  public = list(
    #' @description
    #' Create a "Spatial 'Disc' resampling" resampling instance.
    #'
    #' For a list of available arguments, please see
    #' [sperrorest::partition_disc].
    #' @param id `character(1)`\cr
    #'   Identifier for the resampling strategy.
    initialize = function(id = "spcv_disc") {
      ps = ParamSet$new(params = list(
        ParamInt$new("folds", lower = 1L, default = 10L, tags = "required"),
        ParamInt$new("radius",
          lower = 0L, tags = "required",
          special_vals = list(0L)),
        ParamInt$new("buffer",
          lower = 0L, default = NULL,
          special_vals = list(NULL)),
        ParamUty$new("prob",
          default = NULL),
        ParamLgl$new("replace", default = FALSE)
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

      mlr3::assert_task(task)
      checkmate::assert_multi_class(task, c("TaskClassifST", "TaskRegrST"))
      groups = task$groups

      # Set values to default if missing
      mlr3misc::map(
        c("buffer", "radius", "prob", "replace"),
        function(x) private$.set_default_param_values(x)
      )

      if (!is.null(groups)) {
        stopf("Grouping is not supported for spatial resampling methods")
      }

      private$.sample(task$row_ids, task$coordinates())

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

      index = sample.int(nrow(coords),
        size = self$param_set$values$folds,
        replace = self$param_set$values$replace,
        prob = self$param_set$values$prob
      )

      # we need to set a custom index starting at 1 because the index from
      # sperrorest does not start at 1 and does not increase by 1
      # the index is required for assigning the train/test sets to their
      # respective folds
      mlr3_index = 1

      for (i in index) {
        if (!is.null(self$param_set$values$buffer) |
          self$param_set$values$radius >= 0) {
          di = sqrt((coords[[1]] - as.numeric(coords[i, 1]))^2 + # nolint
            (coords[[2]] - as.numeric(coords[i, 2]))^2) # nolint
        }
        train_sel = numeric()
        if (self$param_set$values$radius >= 0) {
          # leave-disc-out with buffer:
          test_sel = which(di <= self$param_set$values$radius)
          train_sel = which(di > (self$param_set$values$radius + self$param_set$values$buffer))
        } else {
          # leave-one-out with buffer:
          test_sel = i
          if (is.null(self$param_set$values$buffer)) {
            train_sel = seq_len(nrow(coords))[-i] # nocov
          } else {
            train_sel = which(di > self$param_set$values$buffer)
          }
        }
        if (length(train_sel) == 0) {
          warningf(
            "Empty training set in 'partition_disc': 'buffer'
            and/or 'radius' too large?",
            wrap = TRUE
          )
        }

        # similar result structure as in sptcv_cstf
        self$instance$test[[mlr3_index]] = test_sel
        self$instance$train[[mlr3_index]] = train_sel

        mlr3_index = mlr3_index + 1
      }

      invisible(self)
    },
    .set_default_param_values = function(param) {
      if (is.null(self$param_set$values[[param]])) {
        self$param_set$values[[param]] = self$param_set$default[[param]]
      }
    },

    # private get funs for train and test which are used by
    # Resampling$.get_set()
    .get_train = function(i) {
      self$instance$train[[i]]
    },
    .get_test = function(i) {
      self$instance$test[[i]]
    }
  )
)
