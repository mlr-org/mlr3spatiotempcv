#' @title (skmeans) Spatiotemporal clustering resampling
#'
#' @template rox_sptcv_cluto
#'
#' @references
#' `r format_bib("zhao2002")`
#'
#' @export
#' @examples
#' \dontrun{
#' if (mlr3misc::require_namespaces("skmeans", quietly = TRUE)) {
#'   library(mlr3)
#'   library(mlr3spatiotempcv)
#'   task = tsk("cookfarm")
#'
#'   # Instantiate Resampling
#'   rcv = rsmp("sptcv_cluto", folds = 5, time_var = "Date")
#'   rcv$instantiate(task)
#'
#'   # Individual sets:
#'   rcv$train_set(1)
#'   rcv$test_set(1)
#'   # check that no obs are in both sets
#'   intersect(rcv$train_set(1), rcv$test_set(1)) # good!
#'
#'   # Internal storage:
#'   rcv$instance # table
#' }
#' }
ResamplingSptCVCluto = R6Class("ResamplingSptCVCluto",
  inherit = mlr3::Resampling,
  public = list(

    #' @field time_var [character]\cr
    #'  The name of the variable which represents the time dimension.
    #'  Must be of type numeric.
    time_var = NULL,

    #' @field clmethod [character]\cr
    #'   Name of the clustering method to use within `vcluster`.
    #'   See Details for more information.
    clmethod = NULL,

    #' @field cluto_parameters [character]\cr
    #'   Additional parameters to pass to `vcluster`.
    #'   Must be given as a single character string, e.g.
    #'   `"param1='value1'param2='value2'"`.
    #'   See the CLUTO documentation for a full list of supported parameters.
    cluto_parameters = NULL,

    #' @field verbose [logical]\cr
    #'   Whether to show `vcluster` progress and summary output.
    verbose = NULL,

    #' @description
    #' Create an repeated resampling instance using the CLUTO algorithm.
    #' @param id `character(1)`\cr
    #'   Identifier for the resampling strategy.
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
    initialize = function(id = "sptcv_cluto",
      time_var = NULL,
      clmethod = "direct",
      cluto_parameters = NULL,
      verbose = TRUE) {

      ps = ParamSet$new(params = list(
        ParamInt$new("folds", lower = 1L, default = 10L, tags = "required")
      ))
      ps$values = list(folds = 10L)

      self$time_var = time_var
      self$clmethod = clmethod
      self$cluto_parameters = cluto_parameters
      self$verbose = verbose

      super$initialize(
        id = id,
        param_set = ps,
        man = "mlr3spatiotempcv::mlr_resamplings_sptcv_cluto"
      )
    },

    #' @description
    #'  Materializes fixed training and test splits for a given task.
    #' @param task [Task]\cr
    #'  A task to instantiate.
    instantiate = function(task) {

      mlr3misc::require_namespaces("skmeans", quietly = TRUE)

      mlr3::assert_task(task)
      checkmate::assert_multi_class(task, c("TaskClassifST", "TaskRegrST"))
      checkmate::assert_subset(self$time_var, choices = task$feature_names)
      groups = task$groups

      if (!is.null(groups)) {
        stopf("Grouping is not supported for spatial resampling methods") # nocov # nolint
      }

      if (!is.null(self$time_var)) {

        time = as.POSIXct(task$data()[[self$time_var]])
        # time in seconds since 1/1/1970
        time_num = as.numeric(time)

        data_matrix = data.matrix(data.frame(task$coordinates(), time_num))
        colnames(data_matrix) = c("x", "y", "z")
      } else {
        data_matrix = data.matrix(data.frame(task$coordinates()))

        colnames(data_matrix) = c("x", "y")
      }

      instance = private$.sample(
        task$row_ids, data_matrix, self$clmethod,
        self$cluto_parameters, self$verbose)

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
        control_cluto = sprintf('-clmethod="%s""%s"', clmethod, cluto_parameters) # nocov # nolint
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
