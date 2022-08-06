#' @title Crate a Spatiotemporal Classification Task
#'
#' @description This task specializes [Task] and [TaskSupervised] for
#' spatiotemporal classification problems. The target column is assumed to be a
#' factor. The `task_type` is set to `"classif"` and `"spatiotemporal"`.
#'
#' A spatial example task is available via `tsk("ecuador")`, a spatiotemporal
#' one via `tsk("cookfarm_mlr3")`.
#'
#' The coordinate reference system passed during initialization must match the
#' one which was used during data creation, otherwise offsets of multiple meters
#' may occur. By default, coordinates are not used as features. This can be
#' changed by setting `coords_as_features = TRUE`.
#'
#' @template rox_param_id
#' @template rox_param_backend
#' @template rox_param_target
#' @template rox_param_positive
#' @template rox_param_label
#' @template rox_param_coords_as_features
#' @template rox_param_crs
#' @template rox_param_coordinate_names
#' @template rox_param_extra_args
#'
#' @family Task
#' @export
#' @examples
#' if (mlr3misc::require_namespaces(c("sf", "blockCV"), quietly = TRUE)) {
#'   task = as_task_classif_st(ecuador,
#'     target = "slides",
#'     positive = "TRUE", coordinate_names = c("x", "y")
#'   )
#'
#'   # passing objects of class 'sf' is also supported
#'   data_sf = sf::st_as_sf(ecuador, coords = c("x", "y"))
#'   task = as_task_classif_st(data_sf, target = "slides", positive = "TRUE")
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
    initialize = function(id, backend, target, positive = NULL,
      label = NA_character_, coordinate_names, crs = NA_character_,
      coords_as_features = FALSE, extra_args = list()) {
      if (inherits(backend, "sf")) {
        stopf("Creating a task from an sf objects is not supported anymore. Use `as_task_classif_st()` to convert an sf objects into a task.")
      }

      super$initialize(
        id = id, backend = backend, target = target,
        positive = positive, extra_args = extra_args
      )

      self$crs = crs
      self$coordinate_names = coordinate_names
      walk(coordinate_names, function(x) {
        assert_numeric(self$backend$head(1)[[x]], .var.name = x)
      })

      if (packageVersion("mlr3") > "0.13.4") {
        # adjust classif task
        self$task_type = "classif_st"
        new_col_roles = named_list(setdiff(
          mlr_reflections$task_col_roles[["classif_st"]],
          names(private$.col_roles)), character(0))
        private$.col_roles = insert_named(private$.col_roles, new_col_roles)
      }

      # add coordinates as features
      self$coords_as_features = assert_flag(coords_as_features)
    },

    #' @description
    #' Returns coordinates of observations.
    #'
    #' @param row_ids (`integer()`)\cr
    #'   Vector of rows indices as subset of `task$row_ids`.
    #'
    #' @return [data.table::data.table()]
    coordinates = function(row_ids = NULL) {
      if (is.null(row_ids)) row_ids = self$row_ids
      self$backend$data(rows = row_ids, cols = self$coordinate_names)
    },

    #' @description
    #' Print the task.
    #' @param ... Arguments passed to the `$print()` method of the superclass.
    print = function(...) {
      super$print(...)
      cat("* Coordinates:\n")
      print(self$coordinates(), nrows = 10)
      if (length(self$col_roles$time) && length(self$col_roles$space)) {
        catn(c(
          "* Column roles:",
          sprintf("  - Time: %s", self$col_roles$time),
          sprintf("  - Space: %s", self$col_roles$space)
        ))
      } else if (length(self$col_roles$time)) {
        catn(c(
          "* Column roles:",
          sprintf("  - Time: %s", self$col_roles$time)
        ))
      } else if (length(self$col_roles$space)) {
        catn(c(
          "* Column roles:",
          sprintf("  - Space: %s", self$col_roles$space)
        ))
      }
    }
  ),
  active = list(

    #' @field crs (`character(1)`)\cr
    #'   Returns coordinate reference system of task.
    crs = function(rhs) {
      if (missing(rhs)) {
        return(self$extra_args$crs)
      }
      self$extra_args$crs = rhs
    },

    #' @field coordinate_names (`character()`)\cr
    #'   Coordinate names.
    coordinate_names = function(rhs) {
      if (missing(rhs)) {
        return(self$extra_args$coordinate_names)
      }
      self$extra_args$coordinate_names = assert_character(rhs,
        len = 2,
        all.missing = FALSE, any.missing = FALSE
      )
    },

    #' @field coords_as_features (`logical(1)`)\cr
    #'   If `TRUE`, coordinates are used as features.
    coords_as_features = function(rhs) {
      if (missing(rhs)) {
        return(self$extra_args$coords_as_features)
      }

      self$extra_args$coords_as_features = assert_flag(rhs)
      if (rhs) {
        self$set_col_roles(self$coordinate_names, add_to = "coordinate")
      } else {
        self$set_col_roles(self$coordinate_names, roles = "coordinate")
      }
    }
  )
)
