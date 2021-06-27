#' @title (blockCV) Spatial buffering resampling
#'
#' @template rox_spcv_buffer
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
#'   rcv = rsmp("spcv_buffer", theRange = 10000)
#'   rcv$instantiate(task)
#'
#'   # Individual sets:
#'   rcv$train_set(1)
#'   rcv$test_set(1)
#'   intersect(rcv$train_set(1), rcv$test_set(1))
#'
#'   # Internal storage:
#'   # rcv$instance
#' }
ResamplingSpCVBuffer = R6Class("ResamplingSpCVBuffer",
  inherit = mlr3::Resampling,

  public = list(
    #' @description
    #' Create an "Environmental Block" resampling instance.
    #'
    #' For a list of available arguments, please see
    #' [blockCV::buffering()].
    #' @param id `character(1)`\cr
    #'   Identifier for the resampling strategy.
    initialize = function(id = "spcv_buffer") {
      ps = ParamSet$new(params = list(
        ParamInt$new("theRange", lower = 1L, tags = "required"),
        ParamFct$new("spDataType", default = "PA", levels = c("PA", "PB")),
        ParamLgl$new("addBG", default = TRUE)
      ))

      super$initialize(
        id = id,
        param_set = ps,
        man = "mlr3spatiotempcv::mlr_resamplings_spcv_buffer"
      )
      mlr3misc::require_namespaces(c("blockCV", "sf"))
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

      instance = private$.sample(
        task$row_ids,
        task$data()[[task$target_names]],
        task$coordinates(),
        task$extra_args$positive,
        task$extra_args$crs,
        task$properties)

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
      as.integer(length(self$instance))
    }
  ),
  private = list(
    .sample = function(ids, response, coords, positive, crs, properties) {

      mlr3misc::require_namespaces(c("blockCV", "sf"))

      pars = self$param_set$get_values()

      if (!isTRUE("twoclass" %in% properties) && isTRUE(pars$spDataType == "PB")) {
        stopf("spDataType = 'PB' should only be used with two-class response.")
      }

      if (!is.null(pars$addBG) && isTRUE(pars$spDataType == "PA")) {
        stopf("Parameter addBG should only be used with spDataType = 'PB'.")
      }

      # Recode response to 0/1 for twoclass
      if ("twoclass" %in% properties) {
        response = ifelse(response == positive, 1, 0)
        pars$species = "response"
      }

      data = sf::st_as_sf(cbind(response, coords),
        coords = colnames(coords),
        crs = crs
      )

      inds = invoke(blockCV::buffering,
        speciesData = data,
        progress = FALSE,
        .args = pars)

      # if addBG = TRUE, the test set can contain more than one element
      if (!is.null(pars$addBG)) {
        mlr3misc::map(inds$folds, function(x) {
          set = mlr3misc::map(x, function(y) {
            ids[y]
          })
          names(set) = c("train", "test")
          set
        })
      } else {
        train_list = mlr3misc::map(inds$folds, function(x) {
          x[[1]]
        })
        train_list = set_names(
          train_list,
          sprintf("train_fold_%s", seq_along(train_list)))
      }
    },
    .get_train = function(i) {
      if (!is.null(self$param_set$values$addBG)) {
        self$instance[[i]]$train # nocov
      } else {
        self$instance[[i]]
      }
    },
    .get_test = function(i) {
      if (!is.null(self$param_set$values$addBG)) {
        self$instance[[i]]$test
      } else {
        i
      }
    }
  )
)
