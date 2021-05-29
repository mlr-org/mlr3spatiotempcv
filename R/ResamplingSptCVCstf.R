#' @title (CAST) "Leave-location-and-time-out" resampling
#'
#' @template rox_sptcv_cstf
#'
#' @references
#' `r format_bib("meyer2018")`
#'
#' @export
#' @examples
#' library(mlr3)
#' task = tsk("cookfarm")
#'
#' # Instantiate Resampling
#' rcv = rsmp("sptcv_cstf",
#'   folds = 5,
#'   time_var = "Date", space_var = "SOURCEID")
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
ResamplingSptCVCstf = R6Class("ResamplingSptCVCstf",
  inherit = mlr3::Resampling,
  public = list(

    #' @description
    #' Create a "Spacetime Folds" resampling instance.
    #'
    #' For a list of available arguments, please see
    #' [CAST::CreateSpacetimeFolds].
    #' @param id `character(1)`\cr
    #'   Identifier for the resampling strategy.
    initialize = function(id = "sptcv_cstf") {
      ps = ParamSet$new(params = list(
        ParamInt$new("folds", lower = 1L, default = 10L, tags = "required"),
        ParamUty$new("space_var", custom_check = function(x) check_character(x, len = 1)),
        ParamUty$new("time_var", custom_check = function(x) check_character(x, len = 1)),
        ParamUty$new("class", custom_check = function(x) check_character(x, len = 1))
      ))
      ps$values = list(folds = 10L)

      super$initialize(
        id = id,
        param_set = ps,
        man = "mlr3spatiotempcv::mlr_resamplings_sptcv_cstf"
      )
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
        stopf("Grouping is not supported for spatial resampling methods") # nocov # nolint
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
      self$param_set$values$folds
    }
  ),
  private = list(
    .sample = function(task) {
      pv = self$param_set$values

      sptfolds = sample_cstf(
        self = self, task, pv$space_var, pv$time_var,
        pv$class, pv$folds, task$data()
      )

      # combine space and time folds
      for (i in 1:pv$folds) {
        if (!is.null(pv$time_var) & !is.null(sptfolds$space_var)) {
          self$instance$test[[i]] = which(sptfolds$data[[sptfolds$space_var]] %in%
            sptfolds$spacefolds[[i]] &
            sptfolds$data[[pv$time_var]] %in% sptfolds$timefolds[[i]])
          self$instance$train[[i]] = which(!sptfolds$data[[sptfolds$space_var]] %in%
            sptfolds$spacefolds[[i]] &
            !sptfolds$data[[pv$time_var]] %in% sptfolds$timefolds[[i]])
        } else if (is.null(pv$time_var) & !is.null(sptfolds$space_var)) {
          self$instance$test[[i]] = which(sptfolds$data[[sptfolds$space_var]] %in%
            sptfolds$spacefolds[[i]])
          self$instance$train[[i]] = which(!sptfolds$data[[sptfolds$space_var]] %in%
            sptfolds$spacefolds[[i]])
        } else if (!is.null(pv$time_var) & is.null(sptfolds$space_var)) {
          self$instance$test[[i]] = which(sptfolds$data[[pv$time_var]] %in%
            sptfolds$timefolds[[i]])
          self$instance$train[[i]] = which(!sptfolds$data[[pv$time_var]] %in%
            sptfolds$timefolds[[i]])
        }
      }
      invisible(self)
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
