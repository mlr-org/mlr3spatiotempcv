#' @title Spatial Block Cross Validation Resampling
#'
#' @format [R6::R6Class] inheriting from [Resampling].
#' @import mlr3
#'
#' @description
#' Spatial Block Cross validation implemented by the `blockCV` package.
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
#' rcv <- mlr3::mlr_resamplings$get("spcv-block")
#' rcv$param_set$values <- list(folds = 4)
#' rcv$instantiate(task)
#'
#' # Individual sets:
#' rcv$train_set(1)
#' rcv$test_set(1)
#' intersect(rcv$train_set(1), rcv$test_set(1))
#'
#' # Internal storage:
#' rcv$instance
ResamplingSpCVBlock <- R6Class("ResamplingSpCVBlock",
  inherit = mlr3::Resampling,
  public = list(
    initialize = function(id = "spcv-block", param_vals = list(folds = 10L)) {
      super$initialize(
        id = id,
        param_set = ParamSet$new(params = list(
          ParamUty$new("stratify", default = NULL),
          ParamInt$new("folds", lower = 1L, tags = "required"),
          ParamInt$new("rows", lower = 1L, default = 2),
          ParamInt$new("cols", lower = 1L, default = 2)
        )),
        param_vals = param_vals
      )
    },
    instantiate = function(task) {
      assert_task(task)

      # Set values to default if missing
      if (is.null(self$param_set$values$rows)) {
        self$param_set$values$rows = self$param_set$default[["rows"]]
      }
      if (is.null(self$param_set$values$cols)) {
        self$param_set$values$cols = self$param_set$default[["cols"]]
      }

      groups <- task$groups
      stratify <- self$param_set$values$stratify

      if (length(stratify) == 0L || isFALSE(stratify)) {
        if (is.null(groups)) {
          instance <- private$.sample(task$row_ids, task$coordinates())
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
      self$param_set$values$folds
    }
  ),

  private = list(
    .sample = function(ids, coords) {
      points <- sf::st_as_sf(cbind(ids, coords), coords = c("x", "y"))

      # Supress warning about missing crs
      inds <- suppressWarnings(blockCV::spatialBlock(
        speciesData = points,
        rows = self$param_set$values$rows,
        cols = self$param_set$values$cols,
        k = self$param_set$values$folds,
        showBlocks = FALSE,
        progress = FALSE,
      ))
      inds <- map(inds$folds, function(x) x[[2]])

      # Match row_ids with folds
      fold_id <- map(1:nrow(points), function(x) {
        which(sapply(inds, function(y) as.numeric(x) %in% y))
      })

      data.table(
        row_id = ids,
        fold = unlist(fold_id),
        key = "fold"
      )
    },

    .get_train = function(i) {
      self$instance[!list(i), "row_id", on = "fold"][[1L]]
    },

    .get_test = function(i) {
      self$instance[list(i), "row_id", on = "fold"][[1L]]
    },

    .combine = function(instances) {
      rbindlist(instances, use.names = TRUE)
    },

    deep_clone = function(name, value) {
      if (name == "instance") copy(value) else value
    }
  )
)
