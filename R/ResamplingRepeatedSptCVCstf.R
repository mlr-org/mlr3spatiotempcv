#' @title (CAST) Repeated "leave-location-and-time-out" resampling
#'
#' @template rox_sptcv_cstf
#'
#' @references
#' `r format_bib("zhao2002")`
#'
#' @export
#' @examples
#' library(mlr3)
#' library(mlr3spatiotempcv)
#' task = tsk("cookfarm")
#'
#' # Instantiate Resampling
#' rrcv = rsmp("repeated_sptcv_cstf", folds = 3, repeats = 5, time_var = "Date")
#' rrcv$instantiate(task)
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
ResamplingRepeatedSptCVCstf = R6Class("ResamplingRepeatedSptCVCstf",
  inherit = mlr3::Resampling,
  public = list(

    #' @description
    #' Create a "Spacetime Folds" resampling instance.
    #'
    #' For a list of available arguments, please see
    #' [CAST::CreateSpacetimeFolds].
    #' @param id `character(1)`\cr
    #'   Identifier for the resampling strategy.
    initialize = function(id = "repeated_sptcv_cstf") {
      ps = ParamSet$new(params = list(
        ParamInt$new("folds", lower = 1L, default = 10L, tags = "required"),
        ParamInt$new("repeats", lower = 1, default = 1L, tags = "required"),
        ParamUty$new("space_var", custom_check = function(x) check_character(x, len = 1)),
        ParamUty$new("time_var", custom_check = function(x) check_character(x, len = 1)),
        ParamUty$new("class", custom_check = function(x) check_character(x, len = 1))
      ))
      ps$values = list(folds = 10L, repeats = 1)

      super$initialize(
        id = id,
        param_set = ps,
        man = "mlr3spatiotempcv::mlr_resamplings_repeated_sptcv_cstf"
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
    #'   A task to instantiate.
    instantiate = function(task) {

      pv = self$param_set$values

      mlr3::assert_task(task)
      checkmate::assert_multi_class(task, c("TaskClassifST", "TaskRegrST"))
      checkmate::assert_subset(pv$time_var,
        choices = task$feature_names,
        empty.ok = TRUE)
      checkmate::assert_subset(pv$space_var,
        choices = task$feature_names,
        empty.ok = TRUE)
      groups = task$groups

      if (!is.null(groups)) {
        stopf("Grouping is not supported for spatial resampling methods") # nocov
      }

      private$.sample(task)

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
    .sample = function(task) {
      pv = self$param_set$values

      # declare empty list so the for-loop can write to its fields
      self$instance = vector("list", length = pv$repeats)

      for (rep in seq_len(pv$repeats)) {
        sptfolds = sample_cstf(
          self = self, task, pv$space_var, pv$time_var,
          pv$class, pv$folds, task$data())

        # combine space and time folds
        for (i in 1:pv$folds) {
          if (!is.null(pv$time_var) & !is.null(sptfolds$space_var)) {
            self$instance[[rep]]$test[[i]] = which(sptfolds$data[[sptfolds$space_var]] %in%
              sptfolds$spacefolds[[i]] &
              sptfolds$data[[pv$time_var]] %in% sptfolds$timefolds[[i]])
            self$instance[[rep]]$train[[i]] = which(!sptfolds$data[[sptfolds$space_var]] %in%
              sptfolds$spacefolds[[i]] &
              sptfolds$data[[pv$time_var]] %in% sptfolds$timefolds[[i]])
          } else if (is.null(pv$time_var) & !is.null(sptfolds$space_var)) {
            self$instance[[rep]]$test[[i]] = which(sptfolds$data[[sptfolds$space_var]] %in%
              sptfolds$spacefolds[[i]])
            self$instance[[rep]]$train[[i]] = which(!sptfolds$data[[sptfolds$space_var]] %in%
              sptfolds$spacefolds[[i]])
          } else if (!is.null(pv$time_var) & is.null(sptfolds$space_var)) {
            self$instance[[rep]]$test[[i]] = which(sptfolds$data[[pv$time_var]] %in%
              sptfolds$timefolds[[i]])
            self$instance[[rep]]$train[[i]] = which(!sptfolds$data[[pv$time_var]] %in%
              sptfolds$timefolds[[i]])
          }
        }
      }
      invisible(self)
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
