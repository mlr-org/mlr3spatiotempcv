#' @title Crate a Spatiotemporal Classification Task
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
#' if (mlr3misc::require_namespaces(c("sf", "blockCV"), quietly = TRUE)) {
#'
#'   data = mlr3::as_data_backend(ecuador)
#'   task = TaskClassifST$new("ecuador",
#'     backend = data, target = "slides",
#'     positive = "TRUE", extra_args = list(coordinate_names = c("x", "y"))
#'   )
#'
#'   # passing objects of class 'sf' is also supported
#'   data_sf = sf::st_as_sf(ecuador, coords = c("x", "y"))
#'   task = TaskClassifST$new("ecuador_sf",
#'     backend = data_sf, target = "slides", positive = "TRUE"
#'   )
#'
#'   task$task_type
#'   task$formula()
#'   task$class_names
#'   task$positive
#'   task$negative
#'   task$coordinates()
#'   task$coordinate_names
#' }
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

      # support for 'sf' tasks
      if (inherits(backend, "sf")) {
        extra_args$crs = sf::st_crs(backend)$input
        coordinates = sf::st_coordinates(backend)
        # ensure a point feature has been passed
        checkmate::assert_character(as.character(sf::st_geometry_type(backend, by_geometry = FALSE)), fixed = "POINT") # nolint
        backend = sf::st_set_geometry(backend, NULL)
        backend = cbind(backend, coordinates)
        extra_args$coordinate_names = colnames(coordinates)
      }

      super$initialize(
        id = id, backend = backend, target = target,
        positive = positive, extra_args = extra_args)

      self$extra_args$coordinate_names = extra_args$coordinate_names

      info = self$col_info[id == target]
      levels = info$levels[[1L]]

      if (info$type %nin% c("factor", "character", "ordered")) {
        stopf("Target column '%s' must be a factor, character or ordered.", target) # nocov
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
