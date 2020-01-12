#' @title Repeated Spatial Cross Validation Resampling
#'
#' @import mlr3
#'
#' @description Spatial Cross validation following the "k-means" approach after
#' Brenning 2012.
#'
#' @references
#' \cite{mlr3spatiotempcv}{brenning2012}
#'
#' @export
#' @examples
#' library(mlr3)
#' task = tsk("diplodia")
#'
#' # Instantiate Resampling
#' rrcv = rsmp("repeated-spcv-coords")
#' rrcv$param_set$values = list(folds = 3, repeats = 5)
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
ResamplingRepeatedSpCVCoords = R6Class("ResamplingRepeatedSpCVCoords",
  inherit = mlr3::Resampling,

  public = list(
    #' @description
    #' Create an "coordinate-based" repeated resampling instance.
    #' @param id `character(1)`\cr
    #'   Identifier for the resampling strategy.
    initialize = function(id = "repeated-spcv-coords") {
      ps = ParamSet$new(params = list(
        ParamInt$new("repeats", lower = 1),
        ParamInt$new("folds", lower = 1L, tags = "required")
      ))
      ps$values = list(folds = 10L)
      super$initialize(
        id = id,
        param_set = ps,
        man = "mlr3spatiotempcv::mlr_resamplings_repeated_spcvcoords"
      )

    },

    folds = function(iters) {
      iters = assert_integerish(iters, any.missing = FALSE, coerce = TRUE)
      ((iters - 1L) %% as.integer(self$param_set$values$repeats)) + 1L
    },

    repeats = function(iters) {
      iters = assert_integerish(iters, any.missing = FALSE, coerce = TRUE)
      ((iters - 1L) %/% as.integer(self$param_set$values$folds)) + 1L
    },

    #' @description
    #'  Materializes fixed training and test splits for a given task.
    #' @param task [Task]\cr
    #'  A task to instantiate.
    instantiate = function(task) {

      assert_task(task)
      groups = task$groups


      stratify = self$param_set$values$stratify

      if (length(stratify) == 0L || isFALSE(stratify)) {
        if (is.null(groups)) {
          instance = private$.sample(task$row_ids, task$coordinates())
        } else {
          stopf("Grouping is not supported for spatial resampling methods.")
        }
      } else {
        if (!is.null(groups)) {
          stopf("Grouping is not supported for spatial resampling methods")
        }
        stopf("Stratification is not supported for spatial resampling methods.")
      }

      self$instance = instance
      self$task_hash = task$hash
      invisible(self)
    }
  ),

  active = list(
    iters = function() {
      pv = self$param_set$values
      as.integer(pv$repeats) * as.integer(pv$folds)
    }
  ),

  private = list(
    .sample = function(ids, coords) {
      pv = self$param_set$values
      folds = as.integer(pv$folds)

      map_dtr(seq_len(pv$repeats), function(i) {
        data.table(row_id = ids, rep = i,
          fold = kmeans(coords, centers = folds)$cluster
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
    },

    .combine = function(instances) {
      rbindlist(instances, use.names = TRUE)
    }
  )
)
