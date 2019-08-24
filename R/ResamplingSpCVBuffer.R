#' @title Spatial Buffer Cross Validation Resampling
#'
#' @format [R6::R6Class] inheriting from [Resampling].
#' @import mlr3
#'
#' @description
#' Spatial Buffer Cross validation implemented by the `blockCV` package.
#'
#' @section Fields:
#' See [Resampling].
#'
#' @section Methods:
#' See [Resampling].
#'
#' @references
#' Valavi R, Elith J, Lahoz-Monfort JJ, Guillera-Arroita G. blockCV: An r package for generating spatially or environmentally separated folds for k-fold cross-validation of species distribution models. Methods Ecol Evol. 2019; 10:225–232. https://doi.org/10.1111/2041-210X.13107
#'
#' @export
#' @examples
#' task <- mlr3::mlr_tasks$get("ecuador")
#'
#' # Instantiate Resampling
#' rcv <- mlr3::mlr_resamplings$get("spcv-buffer")
#' rcv$param_set$values <- list(range = 1000)
#' rcv$instantiate(task)
#'
#' # Individual sets:
#' rcv$train_set(1)
#' rcv$test_set(1)
#' intersect(rcv$train_set(1), rcv$test_set(1))
#'
#' # Internal storage:
#' rcv$instance
ResamplingSpCVBuffer <- R6Class("ResamplingSpCVBuffer",
  inherit = mlr3::Resampling,
  public = list(
    initialize = function(id = "spcv-block", param_vals = list(range = 100)) {
      super$initialize(
        id = id,
        param_set = ParamSet$new(params = list(
          ParamUty$new("stratify", default = NULL),
          ParamInt$new("range", lower = 1L, tags = "required")
        )),
        param_vals = param_vals
      )
    },
    instantiate = function(task) {

      assert_task(task)

      groups <- task$groups
      stratify <- self$param_set$values$stratify

      if (length(stratify) == 0L || isFALSE(stratify)) {
        if (is.null(groups)) {
          instance <- private$.sample(task$row_ids, task$coordinates(), task$crs)
        } else {
          stopf("Grouping is not supported for spatial resampling methods.", call. = FALSE)
        }
      } else {
        if (!is.null(groups)) {
          stopf("Grouping is not supported for spatial resampling methods", call. = FALSE)
        }
        stopf("Stratification is not supported for spatial resampling methods.", call. = FALSE)
      }

      self$instance <- instance
      self$task_hash <- task$hash
      invisible(self)
    }
  ),

  active = list(
    iters = function() {
      length(self$instance)
    }
  ),

  private = list(
    .sample = function(ids, coords, crs) {
      points <- sf::st_as_sf(coords,
        coords = c("x", "y"),
        crs = crs)

      inds <- blockCV::buffering(
        speciesData = points,
        theRange = self$param_set$values$range,
        progress = FALSE
      )

      map(inds$folds, function(x) {
        set = map(x, function(y) {
          ids[y]
        })
        names(set) <- c("train", "test")
        set
      })
    },

    .get_train = function(i) {
      self$instance[[i]]$train
    },

    .get_test = function(i) {
      self$instance[[i]]$test
    },

    .combine = function(instances) {
      rbindlist(instances, use.names = TRUE)
    },

    deep_clone = function(name, value) {
      if (name == "instance") copy(value) else value
    }
  )
)
