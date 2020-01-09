#' @title Spatial Buffer Cross Validation Resampling
#'
#' @import mlr3
#'
#' @description Spatial Buffer Cross validation implemented by the `blockCV`
#' package.
#'
#' @references Valavi R, Elith J, Lahoz-Monfort JJ, Guillera-Arroita G. blockCV:
#' An r package for generating spatially or environmentally separated folds for
#' k-fold cross-validation of species distribution models. Methods Ecol Evol.
#' 2019; 10:225–232. https://doi.org/10.1111/2041-210X.13107
#'
#' @export
#' @examples
#' library(mlr3)
#' task = tsk("ecuador")
#'
#' # Instantiate Resampling
#' rcv = rsmp("spcv-buffer")
#' rcv$param_set$values = list(range = 1000)
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
        ParamUty$new("stratify", default = NULL),
        ParamInt$new("range", lower = 1L, tags = "required")
      ))
      ps$values = list(range = 100)
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
      stratify = self$param_set$values$stratify

      if (length(stratify) == 0L || isFALSE(stratify)) {
        if (is.null(groups)) {
          instance = private$.sample(task$row_ids, task$coordinates(), task$crs)
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
    #' @field iters `integer(1)`\cr
    #'   Returns the number of resampling iterations, depending on the
    #'   values stored in the `param_set`.
    iters = function() {
      as.integer(length(self$instance$fold))
    }
  ),

  private = list(
    .sample = function(ids, coords, crs) {

      require_namespaces(c("blockCV", "sf"))

      points = sf::st_as_sf(coords,
        coords = c("x", "y"),
        crs = crs)

      inds = blockCV::buffering(
        speciesData = points,
        theRange = self$param_set$values$range,
        progress = FALSE
      )

      inds = map(inds$folds, function(x) {
        set = map(x, function(y) {
          ids[y]
        })
        names(set) = c("train", "test")
        set
      })
      test_inds = map_int(inds, function(x) as.integer(x[["test"]]))

      data.table(
        row_id = as.character(seq(1:length(test_inds))),
        fold = test_inds,
        key = "fold"
      )
    },

    .get_train = function(i) {
      self$instance[!list(i), "row_id", on = "fold"][[1L]]
    },

    .get_test = function(i) {
      self$instance[list(i), "row_id", on = "fold"][[1L]]
    }
  )
)
