#' @title (skmeans) Repeated spatiotemporal clustering resampling
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
#'   rrcv = rsmp("repeated_sptcv_cluto", folds = 3, repeats = 5)
#'   rrcv$instantiate(task, time_var = "Date")
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
#' }
ResamplingRepeatedSptCVCluto = R6Class("ResamplingRepeatedSptCVCluto",
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
    initialize = function(id = "repeated_sptcv_cluto",
      time_var = NULL,
      clmethod = "direct",
      cluto_parameters = NULL,
      verbose = TRUE) {

      ps = ParamSet$new(params = list(
        ParamInt$new("folds", lower = 1L, default = 10L, tags = "required"),
        ParamInt$new("repeats", lower = 1, default = 1L, tags = "required")
      ))
      ps$values = list(folds = 10L, repeats = 1)

      self$time_var = time_var
      self$clmethod = clmethod
      self$cluto_parameters = cluto_parameters
      self$verbose = verbose

      super$initialize(
        id = id,
        param_set = ps,
        man = "mlr3spatiotempcv::mlr_resamplings_repeated_sptcv_cluto"
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
    instantiate = function(task) {

      mlr3misc::require_namespaces("skmeans", quietly = TRUE)

      mlr3::assert_task(task)
      checkmate::assert_multi_class(task, c("TaskClassifST", "TaskRegrST"))
      checkmate::assert_subset(self$time_var, choices = task$feature_names)
      groups = task$groups

      if (!is.null(groups)) {
        stopf("Grouping is not supported for spatial resampling methods") # nocov
      }

      time = as.POSIXct(task$data()[[self$time_var]])
      # time in seconds since 1/1/1970
      time_num = as.numeric(time)

      data_matrix = data.matrix(data.frame(task$coordinates(), time_num))
      colnames(data_matrix) = c("x", "y", "z")

      instance = private$.sample(
        task$row_ids, data_matrix, self$clmethod, self$cluto_parameters,
        self$verbose
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
    .sample = function(ids, data_matrix, clmethod, cluto_parameters, verbose) {
      vcluster_loc = check_cluto_path()

      pv = self$param_set$values
      folds = as.integer(pv$folds)

      if (is.null(cluto_parameters)) {
        control_cluto = sprintf('-clmethod="%s"', clmethod)
      } else {
        control_cluto = sprintf('-clmethod="%s""%s"', clmethod, cluto_parameters) # nocov nolint
      }

      mlr3misc::map_dtr(seq_len(pv$repeats), function(i) {
        data.table(
          row_id = ids, rep = i,
          fold = skmeans::skmeans(data_matrix,
            k = folds,
            method = "CLUTO",
            control = list(
              vcluster = vcluster_loc,
              verbose = verbose,
              control = paste(control_cluto, sprintf("-seed='%s'", i))
            )
          )$cluster
        )
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
