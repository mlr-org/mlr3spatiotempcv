#' @title Convert to a Spatiotemporal Classification Task
#'
#' @description
#' Convert an object to a [TaskClassifST].
#' This is a S3 generic for the following objects:
#'
#' 1. [TaskClassifST]: ensure the identity
#' 2. [data.frame()] and [DataBackend]: provides an alternative to the
#' constructor of [TaskClassifST].
#' 2. [sf::sf].
#'
#' @inheritParams mlr3::as_task_classif
#' @param  crs `[character(1)]`\cr
#'     Coordinate reference system. Either a PROJ string or an
#'     [EPSG](https://epsg.io/) code.
#' @param coords_as_features `[logical(1)]`\cr
#'     Whether the coordinates should also be used as features.
#' @param coordinate_names (`character()`)\cr
#'     The variables names of the coordinates in the data.
#'
#' @return [TaskClassifST].
#' @export
#' @examples
#' library("mlr3")
#' data("ecuador", package = "mlr3spatiotempcv")
#'
#' # data.frame
#' as_task_classif_st(ecuador, target = "slides", positive = "TRUE",
#'   coords_as_features = FALSE,
#'   crs = "+proj=utm +zone=17 +south +datum=WGS84 +units=m +no_defs",
#'   coordinate_names = c("x", "y"))
#'
#' # sf
#' ecuador_sf = sf::st_as_sf(ecuador, coords = c("x", "y"), crs = 32717)
#' as_task_classif_st(ecuador_sf, target = "slides", positive = "TRUE")
#'
#' # TaskClassifST
#' task = tsk("ecuador")
#' as_task_classif_st(task)
as_task_classif_st = function(x, ...) {
  UseMethod("as_task_classif_st", x)
}

#' @rdname as_task_classif_st
#' @export
as_task_classif_st.TaskClassifST = function(x, clone = FALSE, ...) { # nolint
  if (clone) x$clone() else x
}

#' @rdname as_task_classif_st
#' @export
as_task_classif_st.data.frame = function(x, target = NULL, id = deparse(substitute(x)),
  positive = NULL, crs = NA, coords_as_features = FALSE, coordinate_names = NA, ...) {
  TaskClassifST$new(id = id, backend = x, target = target, positive = positive,
    extra_args = list(coordinate_names = coordinate_names, coords_as_features = coords_as_features,
      crs = crs))
}

#' @rdname as_task_classif_st
#' @export
as_task_classif_st.DataBackend = function(x, target = NULL, id = deparse(substitute(x)),
  positive = NULL, crs = NA, coords_as_features = FALSE, coordinate_names = c("x", "y"), ...) {
  TaskClassifST$new(id = id, backend = x, target = target, positive = positive,
    extra_args = list(coordinate_names = coordinate_names, coords_as_features = coords_as_features,
      crs = crs))
}

#' @rdname as_task_classif_st
#' @export
as_task_classif_st.sf = function(x, target = NULL, id = deparse(substitute(x)), positive = NULL,
  coords_as_features = FALSE, crs = sf::st_crs(x)$input, coordinate_names = colnames(sf::st_coordinates(x)), ...) {

  eval(id)
  eval(crs)
  eval(coordinate_names)

  # coordinate_names = colnames(sf::st_coordinates(x))
  coordinates = sf::st_coordinates(x)
  # ensure a point feature has been passed
  checkmate::assert_character(as.character(sf::st_geometry_type(x, by_geometry = FALSE)), fixed = "POINT") # nolint
  x = sf::st_set_geometry(x, NULL)
  x = cbind(x, coordinates)

  TaskClassifST$new(id = id, backend = x, target = target, positive = positive,
    extra_args = list(coords_as_features = coords_as_features, coordinate_names = coordinate_names,
      crs = crs))
}

#' @inherit mlr3::as_task_classif title return
#' @inheritParams mlr3::as_task_classif
#' @description
#' Convert object to a [TaskClassif].
#' This is a S3 generic for [TaskClassifST].
#' @seealso [mlr3::as_task_classif()]
as_task_classif = function(x, ...) {
  UseMethod("as_task_classif", x)
}

#' @rdname as_task_classif
#' @inheritParams mlr3::convert_task
#' @export
as_task_classif.TaskClassifST = function(x) {
  TaskClassif$new(id = x$id, backend = x$backend, target = x$target_names, positive = x$positive)
}

#' @rdname as_task_classif_st
#' @export
as_task_classif_st.DataBackendSf = function(x, target = NULL, id = deparse(substitute(x)),
  positive = NULL, crs = sf::st_crs(x$coordinates)$input, coords_as_features = FALSE, ...) {

  eval(id)
  eval(crs)

  coordinates = sf::st_coordinates(x$coordinates)
  # ensure a point feature has been passed
  checkmate::assert_character(as.character(sf::st_geometry_type(x$coordinates, by_geometry = FALSE)), fixed = "POINT") # nolint

  TaskClassifST$new(id = id, backend = x, target = target, positive = positive,
    extra_args = list(coords_as_features = coords_as_features, crs = crs))
}

#' @rdname as_task_classif_st
#' @export
as_task_classif_st.DataBackendSpatRaster = function(x, target = NULL, id = deparse(substitute(x)),
  positive = NULL, crs = terra::crs(x$stack),
  coords_as_features = FALSE, coordinate_names = c("x", "y"), ...) {

  TaskClassifST$new(id = id, backend = x, target = target, positive = positive,
    extra_args = list(coordinate_names = coordinate_names,
      coords_as_features = coords_as_features, crs = crs))
}
