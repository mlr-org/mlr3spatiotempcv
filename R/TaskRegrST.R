#' @title Create a Spatiotemporal Regression Task
#'
#' @description
#' This task specializes [Task] and [TaskSupervised] for spatiotemporal
#' classification problems.
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
#' @template rox_param_label
#' @template rox_param_coords_as_features
#' @template rox_param_crs
#' @template rox_param_coordinate_names
#' @template rox_param_extra_args
#'
#' @family Task
#' @export
TaskRegrST = R6::R6Class("TaskRegrST",
  inherit = TaskRegr,
  public = list(

    #' @description
    #' Create a new spatiotemporal resampling Task
    initialize = function(id, backend, target, label = NA_character_,
      coordinate_names, crs = NA_character_, coords_as_features = FALSE,
      extra_args = list()) {
      super$initialize(
        id = id, backend = backend, target = target,
        extra_args = extra_args
      )
      self$extra_args$coords_as_features = assert_flag(coords_as_features)
      self$extra_args$crs = crs
      self$coordinate_names = coordinate_names
      walk(coordinate_names, function(x) {
        assert_numeric(self$backend$head(1)[[x]], .var.name = x)
      })

      new_col_roles = named_list(
        setdiff(
          mlr_reflections$task_col_roles[["regr_st"]],
          names(private$.col_roles)
        ), character(0)
      )
      private$.col_roles = insert_named(private$.col_roles, new_col_roles)

      # add coordinates as features
      if (coords_as_features) {
        self$set_col_roles(self$coordinate_names, add_to = "coordinate")
      } else {
        self$set_col_roles(self$coordinate_names, roles = "coordinate")
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
      print(self$coordinates(), nrows = 10)
      if (length(self$col_roles$time) && length(self$col_roles$space)) {
        cat(
          "* Column roles:\n- Time:", self$col_roles$time,
          "\n- Space:", self$col_roles$space, "\n"
        )
      } else if (length(self$col_roles$time)) {
        cat("* Column roles:\n- Time:", self$col_roles$time, "\n")
      } else if (length(self$col_roles$space)) {
        cat("* Column roles:\n- Space:", self$col_roles$space, "\n")
      }
    }
  ),
  active = list(

    #' @field crs (`character(1)`)\cr
    #'   Coordinate reference system.
    crs = function(rhs) {
      if (missing(rhs)) {
        return(self$extra_args$crs)
      }
      # FIXME:
      # self$extra_args$crs = assert_string(rhs, na.ok = TRUE)
    },

    #' @field coordinate_names (`character()`)\cr
    #'   Coordinate names.
    coordinate_names = function(rhs) {
      if (missing(rhs)) {
        return(self$extra_args$coordinate_names)
      }
      self$extra_args$coordinate_names = assert_character(rhs, len = 2,
        all.missing = FALSE, any.missing = FALSE)
    }
  )
)
