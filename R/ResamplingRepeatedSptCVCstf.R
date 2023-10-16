#' @title (CAST) Repeated spatiotemporal "leave-location-and-time-out" resampling
#'
#' @template rox_sptcv_cstf
#' @name mlr_resamplings_repeated_sptcv_cstf
#'
#' @section Parameters:
#'
#' * `repeats` (`integer(1)`)\cr
#'   Number of repeats.
#'
#' @references
#' `r format_bib("zhao2002")`
#'
#' @export
#' @examples
#' \dontrun{
#' library(mlr3)
#' task = tsk("cookfarm_mlr3")
#' task$set_col_roles("SOURCEID", roles = "space")
#' task$set_col_roles("Date", roles = "time")
#'
#' # Instantiate Resampling
#' rcv = rsmp("repeated_sptcv_cstf", folds = 5, repeats = 2)
#' rcv$instantiate(task)
#'
#' ### Individual sets:
#' # rcv$train_set(1)
#' # rcv$test_set(1)
#' # check that no obs are in both sets
#' intersect(rcv$train_set(1), rcv$test_set(1)) # good!
#'
#' # Internal storage:
#' # rcv$instance # table
#' }
ResamplingRepeatedSptCVCstf = R6Class("ResamplingRepeatedSptCVCstf",
  inherit = mlr3::Resampling,
  public = list(

    #' @description
    #' Create a "Spacetime Folds" resampling instance.
    #' @param id `character(1)`\cr
    #'   Identifier for the resampling strategy.
    initialize = function(id = "repeated_sptcv_cstf") {
      ps = ParamSet$new(params = list(
        ParamInt$new("folds", lower = 1L, default = 3L, tags = "required"),
        ParamInt$new("repeats", lower = 1, default = 10L, tags = "required"),
        ParamLgl$new("stratify", default = FALSE)
      ))
      ps$values = list(folds = 3L, repeats = 10L, stratify = FALSE)

      super$initialize(
        id = id,
        param_set = ps,
        label = "Repeated spatiotemporal 'Leave-location-and-time-out' resampling",
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
      task = assert_task(task)
      strata = task$strata
      groups = task$groups

      if (!is.null(groups)) {
        stopf("Grouping is not supported for spatial resampling methods.")
      }

      if (!is.null(strata)) {
        stopf("Stratified sampling is not supported for spatial resampling methods.")
      }

      if (!length(task$col_roles$space) && !length(task$col_roles$time)) {
        stopf("%s has no column role 'space' or 'time'.", format(task))
      }

      self$instance = private$.sample(task)

      self$task_hash = task$hash
      self$task_nrow = task$nrow
      invisible(self)
    }
  ),
  active = list(

    #' @field iters `integer(1)`\cr
    #'   Returns the number of resampling iterations, depending on the
    #'   values stored in the `param_set`.
    iters = function(rhs) {
      assert_ro_binding(rhs)
      pv = self$param_set$values
      as.integer(pv$repeats) * as.integer(pv$folds)
    }
  ),
  private = list(
    .sample = function(task) {
      pv = self$param_set$values
      map(seq_len(pv$repeats), function(i) sample_cast(task, pv$stratify, pv$folds))
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
