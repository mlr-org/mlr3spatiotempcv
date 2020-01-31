#' @title Spatial Buffer Cross Validation Resampling
#'
#' @import mlr3
#'
#' @description Spatial Buffer Cross validation implemented by the `blockCV`
#' package.
#'
#' @note
#' However, the default settings allow to conduct a leave-one-out cross
#' validation for two-class, multi-class and continuous response data, where
#' each observation is one test set. For each test, all observations outside the
#' buffer around the test observation are included in the training set.
#'
#' The parameter `spDataType = PB` and `addBG` are designed for
#' presence-background data in species distribution modelling. If `spDataType =
#' PB`, test sets are only created for each presence observation
#' (`task\$postive`). The option `addBG = TRUE` adds the background data inside
#' the buffer to the corresponding test sets. For each test set, all
#' observations outside the buffer around the test observation are included in
#' the training set.
#'
#' @references
#' \cite{mlr3spatiotempcv}{valavi2018}
#'
#' @export
#' @examples
#' library(mlr3)
#' task = tsk("ecuador")
#'
#' # Instantiate Resampling
#' rcv = rsmp("spcv-buffer", theRange = 1000)
#' rcv$instantiate(task)
#'
#' # Individual sets:
#' rcv$train_set(1)
#' rcv$test_set(1)
#' intersect(rcv$train_set(1), rcv$test_set(1))
#'
#' # Internal storage:
#' rcv$instance
ResamplingSpCVBuffer = R6Class("ResamplingSpCVBuffer",
  inherit = mlr3::Resampling,

  public = list(
    #' @description
    #' Create an "Environmental Block" resampling instance.
    #' @param id `character(1)`\cr
    #'   Identifier for the resampling strategy.
    initialize = function(id = "spcv-buffer") {
      ps = ParamSet$new(params = list(
        ParamInt$new("theRange", lower = 1L, tags = "required"),
        ParamFct$new("spDataType", default = "PA", levels = c("PA", "PB")),
        ParamLgl$new("addBG", default = TRUE)
      ))

      super$initialize(
        id = id,
        param_set = ps
      )
      require_namespaces(c("blockCV", "sf"))
    },

    #' @description
    #'  Materializes fixed training and test splits for a given task.
    #' @param task [Task]\cr
    #'  A task to instantiate.
    instantiate = function(task) {

      assert_task(task)

      groups = task$groups

      if (!is.null(groups)) {
        stopf("Grouping is not supported for spatial resampling methods")
      }

      instance = private$.sample(task$row_ids,
        task$data()[[task$target_names]],
        task$coordinates(),
        task$positive,
        task$crs,
        task$properties)

      self$instance = instance
      self$task_hash = task$hash
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

      require_namespaces(c("blockCV", "sf"))

      pars = self$param_set$get_values()

      if (!isTRUE("twoclass" %in% properties) && isTRUE(pars$spDataType == "PB")) {
        stopf("spDataType = 'PB' should only be used with two-class response")
      }

      if (!is.null(pars$addBG) && isTRUE(pars$spDataType == "PA")) {
        stopf("Parameter addBG should only be used with spDataType = 'PB'")
      }

      # Recode response to 0/1 for twoclass
      if (isTRUE("twoclass" %in% properties)) {
        response = ifelse(response == positive, 1, 0)
        pars$species = "response"
      }

      data = sf::st_as_sf(cbind(response, coords),
        coords = c("x", "y"),
        crs = crs)

      inds = invoke(blockCV::buffering,
        speciesData = data,
        progress = FALSE,
        .args = pars)

      map(inds$folds, function(x) {
        set = map(x, function(y) {
          ids[y]
        })
        names(set) = c("train", "test")
        set
      })
    },

    .get_train = function(i) {
      self$instance[[i]]$train
    },

    .get_test = function(i) {
      self$instance[[i]]$test
    }
  )
)
