#' @title Convert to a Spatiotemporal Classification Task
#'
#' @description
#' Convert an object to a [TaskClassifST].
#' This is a S3 generic for the following objects:
#'
#' 1. [TaskClassifST]: Ensure the identity.
#' 1. [data.frame()] and [mlr3::DataBackend]: Provides an alternative to the
#'    constructor of [TaskClassifST].
#' 1. [sf::sf]: Extracts spatial meta data before construction.
#' 1. [mlr3::TaskRegr]: Calls [mlr3::convert_task()].
#'
#' @param x (any)\cr
#'   Object to convert.
#' @template rox_param_target
#' @param id (`character(1)`)\cr
#'   Id for the new task.
#'   Defaults to the (deparsed and substituted) name of the data argument.
#' @template rox_param_positive
#' @template rox_param_coords_as_features
#' @template rox_param_crs
#' @template rox_param_coordinate_names
#' @template rox_param_label
#' @param ... (any)\cr
#'   Additional arguments.
#'
#' @return [TaskClassifST].
#' @export
#' @examples
#' if (mlr3misc::require_namespaces(c("sf"), quietly = TRUE)) {
#'   library("mlr3")
#'   data("ecuador", package = "mlr3spatiotempcv")
#'
#'   # data.frame
#'   as_task_classif_st(ecuador, target = "slides", positive = "TRUE",
#'     coords_as_features = FALSE,
#'     crs = "+proj=utm +zone=17 +south +datum=WGS84 +units=m +no_defs",
#'     coordinate_names = c("x", "y"))
#'
#'   # sf
#'   ecuador_sf = sf::st_as_sf(ecuador, coords = c("x", "y"), crs = 32717)
#'   as_task_classif_st(ecuador_sf, target = "slides", positive = "TRUE")
#' }
as_task_classif_st = function(x, ...) {
  UseMethod("as_task_classif_st")
}

#' @rdname as_task_classif_st
#' @inheritParams mlr3::as_task
#' @export as_task_classif_st.TaskClassifST
#' @exportS3Method
as_task_classif_st.TaskClassifST = function(x, clone = FALSE, ...) { # nolint
  if (clone) x$clone() else x
}

#' @rdname as_task_classif_st
#' @export as_task_classif_st.data.frame
#' @exportS3Method
as_task_classif_st.data.frame = function(x, target, id = deparse(substitute(x)),
  positive = NULL, coordinate_names, crs = NA_character_,
  coords_as_features = FALSE, label = NA_character_, ...) {
  ii = which(map_lgl(keep(x, is.double), anyInfinite))
  if (length(ii)) {
    warningf("Detected columns with unsupported Inf values in data: %s",
      str_collapse(names(ii)))
  }
  TaskClassifST$new(id = id, backend = x, target = target, positive = positive,
    coords_as_features = coords_as_features, crs = crs,
    coordinate_names = coordinate_names, label = label)
}

#' @rdname as_task_classif_st
#' @export as_task_classif_st.DataBackend
#' @exportS3Method
as_task_classif_st.DataBackend = function(x, target,
  id = deparse(substitute(x)), positive = NULL, coordinate_names, crs,
  coords_as_features = FALSE, label = NA_character_, ...) {
  TaskClassifST$new(id = id, backend = x, target = target, positive = positive,
    coords_as_features = coords_as_features, crs = crs,
    coordinate_names = coordinate_names, label = label)
}

#' @rdname as_task_classif_st
#' @export as_task_classif_st.sf
#' @exportS3Method
as_task_classif_st.sf = function(x, target = NULL, id = deparse(substitute(x)),
  positive = NULL, coords_as_features = FALSE, label = NA_character_, ...) {
  id = as.character(id)

  if (inherits(x, "sf")) {
    # extract spatial meta data
    crs = sf::st_crs(x)$input
    coordinates = as.data.frame(sf::st_coordinates(x))
    coordinate_names = colnames(coordinates)

    x = format_sf(x)
  }

  as_task_classif_st(x, target = target, id = id, positive = positive,
    coords_as_features = coords_as_features, crs = crs,
    coordinate_names = coordinate_names, label = label)
}

#' @rdname as_task_regr_st
#' @export as_task_regr_st.TaskClassifST
#' @exportS3Method
as_task_regr_st.TaskClassifST = function(x, target = NULL,
  drop_original_target = FALSE, drop_levels = TRUE, ...) {
  convert_task(intask = x, target = target, new_type = "regr_st",
    drop_original_target = FALSE, drop_levels = TRUE)
}
