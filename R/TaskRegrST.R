#' @title SpatioTemporal Regression Task
#'
#' @import data.table
#' @import mlr3
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Task]/[TaskSupervised]/[TaskRegr].
#'
#' @description
#' This task specializes [Task] and [TaskSupervised] for spatio-temporal regression problems.
#' The target column is assumed to be a factor.
#' The `task_type` is set to `"classif"` and `"spatiotemporal"`.
#'
#' A spatial example task is available via `mlr_tasks$get("ecuador")`.
#' During initialization, coordinates need to be passed.
#' By default, coordinates are not used as features.
#' This can be changed by setting `coords_as_features = TRUE`.
#'
#' @section Construction:
#' ```
#' t = TaskRegrST$new(id, backend, target, coordinates,
#'   coords_as_features = FALSE)
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier for the task.
#'
#' * `backend` :: [DataBackend]\cr
#'   Either a [DataBackend], or any object which is convertible to a DataBackend with `as_data_backend()`.
#'   E.g., a `data.frame()` will be converted to a [DataBackendDataTable].
#'
#' * `target` :: `character(1)`\cr
#'   Name of the target column.
#'
#' * `coordinates` :: [data.frame]\cr
#'   A [data.frame] with `ncol(2)` containing longitude and latitude information.
#'
#' * `coords_as_features` :: `logical(1)`\cr
#'   Whether the coordinates should also be used as features. Default is `FALSE`.
#'
#' * `crs` :: `character(1)`\cr
#'   Coordinates reference system
#'
#' @section Fields:
#' All methods from [TaskSupervised] and [TaskRegr], and additionally:
#'
#' * `coordinates` :: [data.frame]\cr
#'   Stores the coordinates passed during construction.
#'
#' @section Methods:
#' See [TaskSupervised] and [TaskRegr].
#'
#' @family Task
#' @seealso Example spatial regression task: [`ecuador`][mlr_tasks_ecuador].
#' @export
#' @examples
#'
#' # possible properties:
#' mlr3::mlr_reflections$task_properties$regr
TaskRegrST <- R6::R6Class("TaskRegrST",
  inherit = TaskRegr,
  public = list(
    crs = NULL,

    initialize = function(id, backend, target, coordinates, coords_as_features = FALSE, crs = NULL) {
      assert_string(target)
      super$initialize(id = id, backend = backend, target = target)
      self$crs = checkmate::assert_character(crs, null.ok = TRUE)

      # check coordinates
      assert_names(self$backend$colnames, must.include = coordinates)
      for (coord in coordinates) {
        assert_numeric(self$data(cols = coord)[[1L]], any.missing = FALSE)
      }

      type = self$col_info[id == target]$type
      if (type %nin% c("integer", "numeric")) {
        stopf("Target column '%s' must be numeric", target)
      }

      # mark columns as coordinates and
      # check if coordinates should be included as features
      self$col_roles$coordinates = coordinates
      if (isFALSE(coords_as_features)) {
        self$col_roles$feature = setdiff(self$col_roles$feature, coordinates)
      }
    },

    truth = function(row_ids = NULL) {
      super$truth(row_ids)[[1L]]
    },

    coordinates = function(row_ids = NULL) {
      if (is.null(row_ids)) {
        # Return coords in task$data order
        row_ids <- self$row_ids
      }
      self$backend$data(rows = row_ids, cols = self$coordinate_names)
    },
    print = function(...) {
      .task_print(self)
    }
  ),

  active = list(
    coordinate_names = function() {
      self$col_roles$coordinates
    }
  )
)

.task_print = function(self) {

  catf("%s (%i x %i)", format(self), self$nrow, self$ncol)
  catf(str_indent("Target:", self$target_names))
  catf(str_indent("Properties:", self$properties))
  catf(str_indent("Coordinates", self$coordinate_names))

  types = self$feature_types
  if (nrow(types)) {
    catf("Features (%i):", nrow(types))
    types = types[, list(N = .N, feats = str_collapse(get("id"), n = 100L)), by = "type"][, "type" := translate_types(get("type"))]
    setorderv(types, "N", order = -1L)
    pmap(types, function(type, N, feats) catf(str_indent(sprintf("* %s (%i):", type, N), feats)))
  }

  if (length(self$col_roles$order)) {
    catf(str_indent("Order by:", self$col_roles$order))
  }
  if ("groups" %in% self$properties) {
    catf(str_indent("Groups:", self$col_roles$group))
  }
  if ("weights" %in% self$properties) {
    catf(str_indent("Weights:", self$col_roles$weight))
  }

}
