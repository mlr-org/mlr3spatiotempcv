#' @title Create a Spatiotemporal Regression Task
#'
#' @description
#' This task specializes [Task] and [TaskSupervised] for spatiotemporal
#' classification problems.
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
TaskRegrST = R6::R6Class("TaskRegrST",
  inherit = TaskRegr,
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
    #' @template rox_param_extra_args
    initialize = function(id, backend, target,
      extra_args = list(
        coords_as_features = FALSE, crs = NA,
        coordinate_names = NA)) {

      # support for 'sf' tasks

      if (inherits(backend, "sf")) {
        extra_args$crs = sf::st_crs(backend)$input
        coordinates = sf::st_coordinates(backend)
        # ensure a point feature has been passed
        checkmate::assert_character(as.character(sf::st_geometry_type(backend, by_geometry = FALSE)), fixed = "POINT") # nolint
        backend = sf::st_set_geometry(backend, NULL)
        backend = merge(backend, coordinates)
        extra_args$coordinate_names = colnames(coordinates)
      }

      self$extra_args$coordinate_names = extra_args$coordinate_names
      self$extra_args$crs = extra_args$crs

      assert_string(target)
      super$initialize(
        id = id, backend = backend, target = target,
        extra_args = extra_args)

      type = self$col_info[id == target]$type
      if (type %nin% c("integer", "numeric")) {
        stopf("Target column '%s' must be numeric", target) # nocov
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
