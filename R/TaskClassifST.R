#' @title SpatioTemporal Classification Task
#'
#' @import mlr3
#'
#' @description This task specializes [Task] and [TaskSupervised] for
#' spatiotemporal classification problems. The target column is assumed to be a
#' factor. The `task_type` is set to `"classif"` and `"spatiotemporal"`.
#'
#' A spatial example task is available via `tsk("ecuador")`, a spatiotemporal
#' one via `tsk("cookfarm")`.
#'
#' The coordinate reference system passed during initialization must match the
#' one which was used during data creation, otherwise offsets of multiple meters
#' may occur. By default, coordinates are not used as features. This can be
#' changed by setting `extra_args$coords_as_features = TRUE`.
#'
#' @family Task
#' @export
#' @examples
#' data = mlr3::as_data_backend(ecuador)
#' task = TaskClassifST$new("ecuador",
#'   backend = data, target = "slides",
#'   positive = "TRUE", extra_args = list(coordinate_names = c("x", "y"))
#' )
#'
#' task$task_type
#' task$formula()
#' task$class_names
#' task$positive
#' task$negative
#' task$coordinates()
#' task$coordinate_names
TaskClassifST = R6::R6Class("TaskClassifST",
  inherit = TaskClassif,

  public = list(

    #' @description
    #' Create a new spatiotemporal resampling Task
    #' @param id `[character(1)]`\cr
    #'   Identifier for the task.
    #' @param backend [DataBackend]\cr
    #'   Either a [DataBackend], or any object which is convertible to a
    #'   DataBackend with `as_data_backend()`. E.g., a `data.frame()` will be
    #'   converted to a [DataBackendDataTable].
    #' @param target `[character(1)]`\cr
    #'   Name of the target column.
    #' @param positive `[character(1)]`\cr
    #'   Only for binary classification: Name of the positive class.
    #'   The levels of the target columns are reordered accordingly, so that the
    #'   first element of `$class_names` is the positive class, and the second
    #'   element is the negative class.
    #' @template rox_param_extra_args
    initialize = function(id, backend, target, positive = NULL,
      extra_args = list(
        coords_as_features = FALSE, crs = NA,
        coordinate_names = NA)) {

      assert_string(target)
      super$initialize(
        id = id, backend = backend, target = target,
        positive = positive, extra_args = extra_args)

      self$extra_args$coordinate_names = extra_args$coordinate_names
      self$extra_args$crs = checkmate::assert_character(extra_args$crs, null.ok = TRUE)

      info = self$col_info[id == target]
      levels = info$levels[[1L]]

      if (info$type %nin% c("factor", "character")) {
        stopf("Target column '%s' must be a factor or character", target) # nocov
      }
      if (length(levels) < 2L) {
        stopf("Target column '%s' must have at least two levels", target) # nocov
      }

      self$properties = union(
        self$properties,
        if (length(levels) == 2L) "twoclass" else "multiclass")
      if (!is.null(positive)) {
        self$positive = positive
      }

      # check coordinates
      assert_names(self$backend$colnames, must.include = extra_args$coordinate_names)
      for (coord in extra_args$coordinate_names) {
        assert_numeric(self$data(cols = coord)[[1L]], any.missing = FALSE)
      }

      # mark columns as coordinates and check if coordinates should be included
      # as features
      self$col_roles$coordinates = extra_args$coordinate_names
      if (isFALSE(extra_args$coords_as_features)) {
        self$col_roles$feature = setdiff(
          self$col_roles$feature,
          extra_args$coordinate_names)
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
      self$backend$data(rows = rows, cols = self$extra_args$coordinate_names)
    },

    #' @description
    #' Print the task.
    #' @param ... Arguments passed to the `$print()` method of the superclass.
    print = function(...) {
      super$print(...)
      cat("* Coordinates:\n")
      print(self$coordinates())
    },

    #' @field extra_args (named `list()`)\cr
    #' Additional task arguments set during construction.
    #' Required for [convert_task()].
    extra_args = NULL
  )
)
