#' @title Convert to a Spatiotemporal Regression Task
#'
#' @description
#' Convert object to a [TaskRegrST].
#'
#' This is a S3 generic, specialized for at least the following objects:
#'
#' 1. [TaskRegrST]: Ensure the identity.
#' 1. [data.frame()] and [DataBackend]: Provides an alternative to the
#'    constructor of [TaskRegrST].
#' 1. [sf::sf]: Extracts spatial meta data before construction.
#' 1. [TaskClassif]: Calls [convert_task()].
#'
#' @param x (any)\cr
#'   Object to convert.
#' @template rox_param_target
#' @param id (`character(1)`)\cr
#'   Id for the new task.
#'   Defaults to the (deparsed and substituted) name of the data argument.
#' @template rox_param_coords_as_features
#' @template rox_param_crs
#' @template rox_param_coordinate_names
#' @template rox_param_label
#' @param ... (any)\cr
#'   Additional arguments.
#'
#' @return [TaskRegrST]
#' @examples
#' if (mlr3misc::require_namespaces(c("sf"), quietly = TRUE)) {
#'   library("mlr3")
#'   data("cookfarm_mlr3", package = "mlr3spatiotempcv")
#'
#'   # data.frame
#'   as_task_regr_st(cookfarm_mlr3, target = "PHIHOX",
#'     coords_as_features = FALSE, crs = 26911,
#'     coordinate_names = c("x", "y"))
#'
#'   # sf
#'   cookfarm_sf = sf::st_as_sf(cookfarm_mlr3, coords = c("x", "y"), crs = 26911)
#'   as_task_regr_st(cookfarm_sf, target = "PHIHOX")
#' }
#' @export
as_task_regr_st = function(x, ...) {
  UseMethod("as_task_regr_st")
}

#' @rdname as_task_regr_st
#' @inheritParams mlr3::as_task
#' @export as_task_regr_st.TaskRegrST
#' @exportS3Method
as_task_regr_st.TaskRegrST = function(x, clone = FALSE, ...) { # nolint
  if (clone) x$clone() else x
}

#' @rdname as_task_regr_st
#' @export as_task_regr_st.data.frame
#' @exportS3Method
as_task_regr_st.data.frame = function(x, target, id = deparse(substitute(x)),
  coordinate_names, crs = NA_character_, coords_as_features = FALSE,
  label = NA_character_, ...) {
  ii = which(map_lgl(keep(x, is.double), anyInfinite))
  if (length(ii)) {
    warningf("Detected columns with unsupported Inf values in data: %s",
      str_collapse(names(ii)))
  }
  TaskRegrST$new(id = id, backend = x, target = target,
    coords_as_features = coords_as_features, crs = crs,
    coordinate_names = coordinate_names, label = label)
}

#' @template rox_param_positive
#' @rdname as_task_regr_st
#' @export as_task_regr_st.DataBackend
#' @exportS3Method
as_task_regr_st.DataBackend = function(x, target, id = deparse(substitute(x)),
  positive = NULL, coordinate_names, crs, coords_as_features = FALSE,
  label = NA_character_, ...) {
  TaskRegrST$new(id = id, backend = x, target = target,
    coords_as_features = coords_as_features, crs = crs,
    coordinate_names = coordinate_names, label = label)
}

#' @rdname as_task_regr_st
#' @export as_task_regr_st.sf
#' @exportS3Method
as_task_regr_st.sf = function(x, target = NULL, id = deparse(substitute(x)),
  coords_as_features = FALSE, label = NA_character_, ...) {
  id = as.character(id)
  geometries = as.character(unique(sf::st_geometry_type(x)))
  if (!test_names(geometries, identical.to = "POINT")) {
    stop("Simple feature may not contain geometries of type '%s'",
      str_collapse(setdiff(geometries, "POINT")))
  }

  # extract spatial meta data
  crs = sf::st_crs(x)$input
  coordinates = as.data.frame(sf::st_coordinates(x))
  coordinate_names = colnames(coordinates)

  # convert sf to data.frame
  x[[attr(x, "sf_column")]] = NULL
  attr(x, "sf_column") = NULL
  x = as.data.frame(x)

  # add coordinates
  x = cbind(x, coordinates)

  as_task_regr_st(x, target = target, id = id,
    coords_as_features = coords_as_features, crs = crs,
    coordinate_names = coordinate_names, label = label)
}

#' @inheritParams mlr3::convert_task
#' @rdname as_task_regr_st
#' @export as_task_regr_st.TaskClassifST
#' @exportS3Method
as_task_regr_st.TaskClassifST = function(x, target = NULL,
  drop_original_target = FALSE, drop_levels = TRUE, ...) {
  convert_task(intask = x, target = target, new_type = "regr_st",
    drop_original_target = FALSE, drop_levels = TRUE)
}
