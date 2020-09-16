#' @title Spatio-Temporal Cluster Resampling
#'
#' @import mlr3
#' @template rox_sptcv_cluto
#'
#' @export
#' @examples
#' \dontrun{
#' library(mlr3)
#' library(mlr3spatiotempcv)
#' task = tsk("cookfarm")
#'
#' # Instantiate Resampling
#' rcv = rsmp("sptcv_cluto", folds = 5)
#' rcv$instantiate(task, "Date")
#'
#' # Individual sets:
#' rcv$train_set(1)
#' rcv$test_set(1)
#' # check that no obs are in both sets
#' intersect(rcv$train_set(1), rcv$test_set(1)) # good!
#'
#' # Internal storage:
#' rcv$instance # table
#' }
ResamplingSptCVCluto = R6Class("ResamplingSptCVCluto",
  inherit = mlr3::Resampling,

  public = list(
    #' @description
    #' FIXME.
    #' @param id `character(1)`\cr
    #'   Identifier for the resampling strategy.
    initialize = function(id = "sptcv_cluto") {
      ps = ParamSet$new(params = list(
        ParamInt$new("folds", lower = 1L, default = 10L, tags = "required")
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
    #' @param time_var [character]\cr
    #'  The name of the variable which represents the time dimension.
    #'  Must be of type numeric.
    #' @param clmethod [character]\cr
    #'   Name of the clustering method to use within `vcluster`.
    #'   See Details for more information.
    #' @param cluto_parameters [character]\cr
    #'   Additional parameters to pass to `vcluster`.
    #'   Must be given as a single character string, e.g.
    #'   `"param1='value1'param2='value2'"`.
    #'   See the CLUTO documentation for a full list of supported parameters.
    #' @param verbose [logical]\cr
    #'   Whether to show `vcluster` progress and summary output.
    instantiate = function(task, time_var, clmethod = "direct",
      cluto_parameters = NULL, verbose = TRUE) {

      requireNamespace("skmeans", quietly = TRUE)

      assert_task(task)
      groups = task$groups

      if (!is.null(groups)) {
        stopf("Grouping is not supported for spatial resampling methods")
      }

      time = as.POSIXct(task$data()[[time_var]])
      # time in seconds since 1/1/1970
      time_num = as.numeric(time)

      data_matrix = data.matrix(data.frame(task$coordinates(), time_num))
      colnames(data_matrix) = c("x", "y", "z")

      instance = private$.sample(
        task$row_ids, data_matrix, clmethod,
        cluto_parameters, verbose)

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
      self$param_set$values$folds
    }
  ),

  private = list(
    .sample = function(ids, data_matrix, clmethod, cluto_parameters, verbose) {
      vcluster_loc = check_cluto_path()

      if (is.null(cluto_parameters)) {
        control_cluto = sprintf('-clmethod="%s"', clmethod)
      } else {
        control_cluto = sprintf('-clmethod="%s""%s"', clmethod, cluto_parameters)
      }

      inds = skmeans::skmeans(data_matrix,
        k = self$param_set$values$folds,
        method = "CLUTO",
        control = list(
          vcluster = vcluster_loc,
          verbose = verbose,
          control = control_cluto)
      )

      data.table(
        row_id = ids,
        fold = inds$cluster,
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
    }
  )
)
