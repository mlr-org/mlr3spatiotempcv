#' @title (CAST) Spatiotemporal "Leave-location-and-time-out" resampling
#'
#' @template rox_sptcv_cstf
#' @name mlr_resamplings_sptcv_cstf
#'
#' @references
#' `r format_bib("meyer2018")`
#'
#' @export
#' @examples
#' library(mlr3)
#' task = tsk("cookfarm_mlr3")
#' task$set_col_roles("SOURCEID", roles = "space")
#' task$set_col_roles("Date", roles = "time")
#'
#' # Instantiate Resampling
#' rcv = rsmp("sptcv_cstf", folds = 5)
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
        ParamInt$new("folds", lower = 1L, default = 3L, tags = "required"),
        ParamLgl$new("stratify", default = FALSE)
      ))
      ps$values = list(folds = 3L, stratify = FALSE)

      super$initialize(
        id = id,
        param_set = ps,
        label = "Spatiotemporal 'Leave-location-and-time-out' resampling",
        man = "mlr3spatiotempcv::mlr_resamplings_sptcv_cstf"
      )
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
      self$param_set$values$folds
    }
  ),
  private = list(
    .sample = function(task) {
      pv = self$param_set$values
      sample_cast(task, pv$stratify, pv$folds)
    },

    .get_train = function(i) {
      self$instance$train[[i]]
    },
    .get_test = function(i) {
      self$instance$test[[i]]
    }
  )
)

sample_cast = function(task, stratify = FALSE, folds) {

  if (length(task$col_roles$time) && length(task$col_roles$space)) {
    lg$info(sprintf("Using column roles 'space' (var. '%s') and 'time' (var. '%s') for partitioning", task$col_roles$space, task$col_roles$time))

  } else if (length(task$col_roles$time)) {
    lg$info(sprintf("Using column role 'time' (var. '%s') for partitioning", task$col_roles$time))
  }
  else if (length(task$col_roles$space)) {
    lg$info(sprintf("Using column role 'space' (var. '%s') for partitioning", task$col_roles$space))
  }

  target = if (stratify) task$target_names else NULL
  space = task$col_roles$space
  time = task$col_roles$time
  data = task$data(cols = c(target, space, time))

  if (length(space)) {
    # group observations by space
    group_space = unique(data, by = space)

    if (nrow(group_space) < folds) {
      stop("The number of folds is higher than the number of spatial units.")
    }

    # assign fold to each group
    # optionally stratify by target
    group_space[, fold_space := shuffle(seq_len0(.N) %% folds + 1), by = target]
    # add fold to all observations in group
    instance_space = merge(data, group_space, by = space, sort = FALSE)
    # add row id
    instance_space[, row_id := .I]
    # extract folds
    train_space = map(seq_len(folds), function(i) instance_space[!list(i), row_id, on = "fold_space"])
    test_space = map(seq_len(folds), function(i) instance_space[list(i), row_id, on = "fold_space"])
  }

  if (length(time)) {
    # group observations by time
    group_time = unique(data, by = time)

    if (nrow(group_time) < folds) {
      stop("The number of folds is higher than the number of temporal units.")
    }

    # assign fold to each group
    group_time[, fold_time := shuffle(seq_len0(.N) %% folds + 1)]
    # add fold to all observations in group
    instance_time = merge(data, group_time, by = time, sort = FALSE)
    # add row id
    instance_time[, row_id := .I]
    # extract folds
    train_time = map(seq_len(folds), function(i) instance_time[!list(i), row_id, on = "fold_time"])
    test_time = map(seq_len(folds), function(i) instance_time[list(i), row_id, on = "fold_time"])
  }

  # combine space and time folds
  train = if (length(space) && length(time)) pmap(list(train_space, train_time), function(x, y) intersect(x, y)) else if (length(space)) train_space else train_time
  test = if (length(space) && length(time)) pmap(list(test_space, test_time), function(x, y) intersect(x, y)) else if (length(space)) test_space else test_time
  list(train = train, test = test)
}
