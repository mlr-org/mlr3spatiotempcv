#' @title Create Spatio-Temporal Folds Using Predefined Groups
#'
#' @template rox_sptcv_cstf
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

    #' @field space_var `character(1)`\cr
    #'   Column name identifying the spatial units.
    space_var = NULL,

    #' @field time_var `character(1)`\cr
    #'  Column name identifying the temporal units.
    time_var = NULL,

    #' @field class `character(1)`\cr
    #'  Column name identifying a class unit (e.g. land cover).
    class = NULL,

    #' @description
    #' Create a "Spacetime Folds" resampling instance.
    #' @param id `character(1)`\cr
    #'   Identifier for the resampling strategy.
    #' @param space_var `character(1)`\cr
    #'   Column name identifying the spatial units.
    #' @param time_var `character(1)`\cr
    #'  Column name identifying the temporal units.
    #' @param class `character(1)`\cr
    #'  Column name identifying a class unit (e.g. land cover).
    initialize = function(id = "sptcv_cstf",
      space_var = NULL,
      time_var = NULL,
      class = NULL) {
      ps = ParamSet$new(params = list(
        ParamInt$new("folds", lower = 1L, default = 10L, tags = "required")
      ))
      ps$values = list(folds = 10L)

      self$space_var = space_var
      self$time_var = time_var
      self$class = class

      super$initialize(
        id = id,
        param_set = ps,
        man = "mlr3spatiotempcv::mlr_resamplings_SptCVCstf"
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
    #'   A task to instantiate.
    #' @param space_var `[character]`\cr
    #'   Column name identifying the spatial units.
    #' @param time_var `[character]`\cr
    #'   Column name identifying the temporal units.
    #' @param class `[character]`\cr
    #'   Column name identifying a class unit (e.g. land cover).
    instantiate = function(task) {

      assert_task(task)
      checkmate::assert_multi_class(task, c("TaskClassifST", "TaskRegrST"))
      checkmate::assert_character(self$time_var)
      groups = task$groups

      if (!is.null(groups)) {
        stopf("Grouping is not supported for spatial resampling methods")
      }

      private$.sample(task, self$space_var, self$time_var, self$class)

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
    .sample = function(task, space_var, time_var, class) {
      k = self$param_set$values$folds
      data = task$data()

      sptfolds = sample_cstf(
        self = self, task, space_var, time_var,
        class, k, data)

      # combine space and time folds
      for (i in 1:k) {
        if (!is.null(time_var) & !is.null(sptfolds$space_var)) {
          self$instance$test[[i]] = which(sptfolds$data[[sptfolds$space_var]] %in%
            sptfolds$spacefolds[[i]] &
            sptfolds$data[[time_var]] %in% sptfolds$timefolds[[i]])
          self$instance$train[[i]] = which(!sptfolds$data[[sptfolds$space_var]] %in%
            sptfolds$spacefolds[[i]] &
            sptfolds$data[[time_var]] %in% sptfolds$timefolds[[i]])
        } else if (is.null(time_var) & !is.null(sptfolds$space_var)) {
          self$instance$test[[i]] = which(sptfolds$data[[sptfolds$space_var]] %in%
            sptfolds$spacefolds[[i]])
          self$instance$train[[i]] = which(!sptfolds$data[[sptfolds$space_var]] %in%
            sptfolds$spacefolds[[i]])
        } else if (!is.null(time_var) & is.null(sptfolds$space_var)) {
          self$instance$test[[i]] = which(sptfolds$data[[time_var]] %in%
            sptfolds$timefolds[[i]])
          self$instance$train[[i]] = which(!sptfolds$data[[time_var]] %in%
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
