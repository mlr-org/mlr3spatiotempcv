#' @title SpatioTemporal Classification Task
#'
#' @import data.table
#' @import mlr3
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from
#'   [Task]/[TaskSupervised]/[TaskClassif].
#'
#' @description This task specializes [Task] and [TaskSupervised] for
#' spatio-temporal classification problems. The target column is assumed to be a
#' factor. The `task_type` is set to `"classif"` and `"spatiotemporal"`.
#'
#' A spatial example task is available via `mlr_tasks$get("ecuador")`. During
#' initialization, coordinates need to be passed. By default, coordinates are
#' not used as features. This can be changed by setting `coords_as_features =
#' TRUE`.
#'
#' @family Task
#' @seealso Example spatial classification task: [`ecuador`][mlr_tasks_ecuador].
#' @export
#' @examples
#' data = data.table::as.data.table(readRDS(system.file("extdata",
#'   "ecuador.rda", package = "mlr3spatiotempcv")))
#' task = TaskClassifST$new("ecuador", backend = data, target = "slides",
#'   positive = "TRUE", coordinates = c("x", "y"))
#'
#' task$task_type
#' task$formula()
#' task$truth()
#' task$class_names
#' task$positive
#' task$negative
#' task$coordinates()
#' task$coordinate_names
#'
#' # possible properties:
#' mlr3::mlr_reflections$task_properties$classif
TaskClassifST = R6::R6Class("TaskClassifST", inherit = TaskClassif,

  public = list(
    crs = NULL,

    #' @description
    #' Create a new spatiotemp resampling instance
    #' @param coordinates `character(2)`\cr
    #'   Name of the coordinates in the dataset.
    #' @param crs `character(1)`\cr
    #'   Coordinates reference system
    #' @param coords_as_features `logical(1)`\cr Whether the coordinates
    #'   should also be used as features. Default is `FALSE`.
    #' @inheritParams TaskClassif
    initialize = function(id, backend, target, coordinates,
      coords_as_features = FALSE, positive = NULL, crs = NULL,
      coordinate_names = NULL) {

      assert_string(target)
      super$initialize(id = id, backend = backend, target = target,
        positive = positive)

      info = self$col_info[id == target]
      levels = info$levels[[1L]]

      if (info$type %nin% c("factor", "character")) {
        stopf("Target column '%s' must be a factor or character", target)
      }
      if (length(levels) < 2L) {
        stopf("Target column '%s' must have at least two levels", target)
      }

      self$properties = union(self$properties, if (length(levels) == 2L) "twoclass" else "multiclass")
      if (!is.null(positive)) {
        self$positive = positive
      }

      self$crs = checkmate::assert_character(crs, null.ok = TRUE)

      # check coordinates
      assert_names(self$backend$colnames, must.include = coordinates)
      for (coord in coordinates) {
        assert_numeric(self$data(cols = coord)[[1L]], any.missing = FALSE)
      }

      # mark columns as coordinates and
      # check if coordinates should be included as features
      self$col_roles$coordinates = coordinates
      if (isFALSE(coords_as_features)) {
        self$col_roles$feature = setdiff(self$col_roles$feature, coordinates)
      }

      if (is.null(self$coordinate_names)) {
        self$coordinate_names = self$col_roles$coordinates
      }
    },

    #' @description
    #' Return the coordinates of the task
    #' @param row_ids ???
    coordinates = function(row_ids = NULL) {
      if (is.null(row_ids)) {
        # Return coords in task$data order
        row_ids = self$row_ids
      }
      self$backend$data(rows = row_ids, cols = self$coordinate_names)
    },

    #' @field coordinate_names [character]\cr
    #'   The variables names of the coordinates.
    coordinate_names = NULL
  )

  # active = list(
  #   coordinate_names = function() {
  #     self$col_roles$coordinates
  #   }
  # )
)
