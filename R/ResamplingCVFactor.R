#' @title (sperrorest) Factor-based partitioning
#'
#' @template rox_cv_factor
#'
#' @references
#' `r format_bib("brenning2012")`
#'
#' @export
#' @examples
#' library(mlr3)
#' task = tsk("cookfarm")
#'
#' # Instantiate Resampling
#' rcv = rsmp("cv_factor", factor = "TAXSUSDA")
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
ResamplingCVFactor = R6Class("ResamplingCVFactor",
  inherit = mlr3::Resampling,

  public = list(
    #' @description
    #' Create an "coordinate-based" repeated resampling instance.
    #'
    #' For a list of available arguments, please see [sperrorest::partition_factor].
    #' @param id `character(1)`\cr
    #'   Identifier for the resampling strategy.
    initialize = function(id = "cv_factor") {
      ps = ParamSet$new(params = list(
        ParamUty$new("factor", custom_check = function(x) {
          checkmate::check_character(x, len = 1)
        }, tags = "required"),
        ParamLgl$new("factor_as_feature", default = FALSE)
      ))
      ps$values = list(factor_as_feature = FALSE)
      super$initialize(
        id = id,
        param_set = ps,
        man = "mlr3spatiotempcv::mlr_resamplings_cv_factor"
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

      if (!is.null(groups)) {
        stopf("Grouping is not supported for spatial resampling methods")
      }

      levels = droplevels(as.factor(task$data()[[self$param_set$values$factor]])
      )
      # drop grouping variable from columns used as features
      if (!self$param_set$values$factor_as_feature) {
        task$col_roles$feature = task$col_roles$feature[-which(task$col_roles$feature == self$param_set$values$factor)]
      }

      instance = private$.sample(task$row_ids, levels)

      self$instance = instance
      self$task_hash = task$hash
      self$task_nrow = task$nrow
      invisible(self)
    }),
  active = list(
    #' @field iters `integer(1)`\cr
    #'   Returns the number of resampling iterations, depending on the
    #'   values stored in the `param_set`.
    iters = function() {
      length(unique(self$instance$fold))
    }),
  private = list(
    .sample = function(ids, levels) {
      data.table(
        row_id = ids,
        fold = as.integer(levels),
        key = "fold"
      )
    },

    # private get funs for train and test which are used by
    # Resampling$.get_set()
    .get_train = function(i) {
      self$instance[!list(i), "row_id", on = "fold"][[1L]]
    },
    .get_test = function(i) {
      self$instance[list(i), "row_id", on = "fold"][[1L]]
    })
)
