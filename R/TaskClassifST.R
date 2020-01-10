#' @title SpatioTemporal Classification Task
#'
#' @import data.table
#' @import mlr3
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
#'   positive = "TRUE", coordinate_names = c("x", "y"))
#'
#' task$task_type
#' task$formula()
#' task$class_names
#' task$positive
#' task$negative
#' task$coordinates()
#' task$coordinate_names
TaskClassifST = R6::R6Class("TaskClassifST", inherit = TaskClassif,

  public = list(

    #' @description
    #' Create a new spatiotemporal resampling instance.
    #' @param id `character(1)`\cr
    #'   Identifier for the task.
    #' @param backend [DataBackend]\cr
    #'   Either a [DataBackend], or any object which is convertible to a
    #'   DataBackend with `as_data_backend()`. E.g., a `data.frame()` will be
    #'   converted to a [DataBackendDataTable].
    #' @param target `character(1)`\cr
    #'   Name of the target column.
    #' @param positive `character(1)`\cr
    #'   Only for binary classification: Name of the positive class. The levels
    #'   of the target columns are reordered accordingly, so that the first
    #'   element of `$class_names` is the positive class, and the second element
    #'   is the negative class.
    #' @param crs `character(1)`\cr
    #'   Coordinates reference system
    #' @param coords_as_features `logical(1)`\cr
    #'   Whether the coordinates should also be used as features.
    #'   Default is `FALSE`.
    #' @param coordinate_names `character(2)`\cr
    #'   The variables names of the coordinates in the data.
    initialize = function(id, backend, target, positive = NULL,
      coords_as_features = FALSE, crs = NA, coordinate_names = NA) {

      self$coordinate_names = coordinate_names
      self$crs = crs

      assert_string(target)
      super$initialize(id = id, backend = backend, target = target,
        positive = positive)
      self$crs = checkmate::assert_character(crs, null.ok = TRUE)

      info = self$col_info[id == target]
      levels = info$levels[[1L]]

      if (info$type %nin% c("factor", "character")) {
        stopf("Target column '%s' must be a factor or character", target)
      }
      if (length(levels) < 2L) {
        stopf("Target column '%s' must have at least two levels", target)
      }

      self$properties = union(self$properties,
        if (length(levels) == 2L) "twoclass" else "multiclass")
      if (!is.null(positive)) {
        self$positive = positive
      }

      # check coordinates
      assert_names(self$backend$colnames, must.include = coordinate_names)
      for (coord in coordinate_names) {
        assert_numeric(self$data(cols = coord)[[1L]], any.missing = FALSE)
      }

      # mark columns as coordinates and check if coordinates should be included
      # as features
      self$col_roles$coordinates = coordinate_names
      if (isFALSE(coords_as_features)) {
        self$col_roles$feature = setdiff(self$col_roles$feature,
          coordinate_names)
      }
    },

    #' @description
    #' Return the coordinates of the task
    #' @param rows Row IDs. Can be used to subset the returned coordinates.
    coordinates = function(rows = NULL) {
      if (is.null(rows)) {
        # Return coords in task$data order
        rows = self$row_ids
      }
      self$backend$data(rows = rows, cols = self$coordinate_names)
    },

    #' @description
    #' Print the task.
    #' @param ... Arguments passed to the `$print()` method of the superclass.
    print = function(...) {
      super$print(...)
      cat("* Coordinates:\n")
      print(self$coordinates())
    },

    #' @field coordinate_names [character]\cr
    #' The variables names of the coordinates in the data.
    coordinate_names = NULL,

    #' @field crs [character]\cr
    #' The `proj4string` of the coordinate system.
    crs = NULL
  )
)
