#' @title (blockCV) Repeated "environmental blocking" resampling
#'
#' @template rox_spcv_env
#'
#' @references
#' `r format_bib("valavi2018")`
#'
#' @export
#' @examples
#' if (mlr3misc::require_namespaces(c("sf", "blockCV"), quietly = TRUE)) {
#'   library(mlr3)
#'   task = tsk("ecuador")
#'
#'   # Instantiate Resampling
#'   rrcv = rsmp("repeated_spcv_env", folds = 4, repeats = 2)
#'   rrcv$instantiate(task)
#'
#'   # Individual sets:
#'   rrcv$train_set(1)
#'   rrcv$test_set(1)
#'   intersect(rrcv$train_set(1), rrcv$test_set(1))
#'
#'   # Internal storage:
#'   rrcv$instance
#' }
ResamplingRepeatedSpCVEnv = R6Class("ResamplingRepeatedSpCVEnv",
  inherit = mlr3::Resampling,
  public = list(
    #' @description
    #' Create an "Environmental Block" repeated resampling instance.
    #'
    #' For a list of available arguments, please see [blockCV::envBlock].
    #' @param id `character(1)`\cr
    #'   Identifier for the resampling strategy.
    initialize = function(id = "repeated_spcv_env") {
      ps = ParamSet$new(params = list(
        ParamInt$new("folds", lower = 1L, default = 10L, tags = "required"),
        ParamInt$new("repeats", lower = 1, default = 10L, tags = "required"),
        ParamUty$new("features")
      ))
      ps$values = list(folds = 10L, repeats = 1)
      super$initialize(
        id = id,
        param_set = ps,
        man = "mlr3spatiotempcv::mlr_resamplings_repeated_spcv_env"
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
    instantiate = function(task) {

      mlr3::assert_task(task)
      checkmate::assert_multi_class(task, c("TaskClassifST", "TaskRegrST"))
      pv = self$param_set$values

      # Set values to default if missing
      if (is.null(pv$features)) {
        pv$features = task$feature_names
      }

      # Remove non-numeric features, target and coordinates
      columns = task$col_info[!id %in%
        c(task$target_names, "x", "y")][type == "numeric"]

      # Check for selected features that are not in task
      diff = setdiff(pv$features, columns[, id])
      if (length(diff) > 0) {
        warningf("Feature '%s' is either non-numeric or does not exist in data.")
        stopf("Method 'spcv_env' requires numeric features for clustering.")
      }
      columns = columns[id %in% pv$features]
      columns = columns[, id]

      data = task$data()[, columns, with = FALSE]

      instance = private$.sample(task$row_ids, data)

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
    .sample = function(ids, data) {
      pv = self$param_set$values
      folds = as.integer(pv$folds)

      mlr3misc::map_dtr(seq_len(pv$repeats), function(i) {
        data.table(
          row_id = ids, rep = i,
          fold = kmeans(data, centers = folds)$cluster
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
