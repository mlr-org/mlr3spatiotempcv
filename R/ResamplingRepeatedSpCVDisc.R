#' @title (sperrorest) Repeated spatial "disc" resampling
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
#' rrcv = rsmp("repeated_spcv_disc",
#'   folds = 3L, repeats = 2,
#'   radius = 200L, buffer = 200L)
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
ResamplingRepeatedSpCVDisc = R6Class("ResamplingRepeatedSpCVDisc",
  inherit = mlr3::Resampling,
  public = list(
    #' @description
    #' Create a "Spatial 'Disc' resampling" resampling instance.
    #'
    #' For a list of available arguments, please see
    #' [sperrorest::partition_disc].
    #' @param id `character(1)`\cr
    #'   Identifier for the resampling strategy.
    initialize = function(id = "repeated_spcv_disc") {
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
        ParamLgl$new("replace", default = FALSE),
        ParamInt$new("repeats", lower = 1, default = 1L, tags = "required")
      ))
      ps$values = list(folds = 10L, repeats = 1)
      super$initialize(
        id = id,
        param_set = ps,
        man = "mlr3spatiotempcv::mlr_resamplings_repeated_spcv_disc"
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
      groups = task$groups
      if (!is.null(groups)) {
        stopf("Grouping is not supported for spatial resampling methods.") # nocov
      }

      # Set values to default if missing
      mlr3misc::map(
        c("buffer", "radius", "prob", "replace"),
        function(x) private$.set_default_param_values(x)
      )

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
      pv = self$param_set$values
      as.integer(pv$repeats) * as.integer(pv$folds)
    }
  ),
  private = list(
    .sample = function(ids, coords) {
      reps = self$param_set$values$repeats
      # declare empty list so the for-loop can write to its fields
      self$instance = vector("list", length = reps)

      # k = self$param_set$values$folds

      for (rep in seq_len(reps)) {
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
          self$instance[[rep]]$test[[mlr3_index]] = test_sel
          self$instance[[rep]]$train[[mlr3_index]] = train_sel

          mlr3_index = mlr3_index + 1
        }
        invisible(self)
      }
    },
    .set_default_param_values = function(param) {
      if (is.null(self$param_set$values[[param]])) {
        self$param_set$values[[param]] = self$param_set$default[[param]]
      }
    },
    .get_train = function(i) {
      i = as.integer(i) - 1L
      folds = as.integer(self$param_set$values$folds)
      rep = i %/% folds + 1L
      fold = i %% folds + 1L
      self$instance[[rep]]$train[[fold]]
    },
    .get_test = function(i) {
      i = as.integer(i) - 1L
      folds = as.integer(self$param_set$values$folds)
      rep = i %/% folds + 1L
      fold = i %% folds + 1L
      self$instance[[rep]]$test[[fold]]
    }
  )
)
