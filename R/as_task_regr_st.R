#' @title Convert to a Spatiotemporal Regression Task
#'
#' @description
#' Convert an object to a [TaskRegrST].
#' This is a S3 generic for the following objects:
#'
#' 1. [TaskRegrST]: ensure the identity
#' 2. [data.frame()] and [DataBackend]: provides an alternative to the
#' constructor of [TaskRegrST].
#' 2. [sf::sf].
#'
#' @inheritParams mlr3::as_task_regr
#' @param  crs `[character(1)]`\cr
#'     Coordinate reference system. Either a PROJ string or an
#'     [EPSG](https://epsg.io/) code.
#' @param coords_as_features `[logical(1)]`\cr
#'     Whether the coordinates should also be used as features.
#' @param coordinate_names (`character()`)\cr
#'     The variables names of the coordinates in the data.
#'
#' @return [TaskRegrST].
#' @export
#' @examples
#' library("mlr3")
#' data("cookfarm_sample", package = "mlr3spatiotempcv")
#'
#' # data.frame
#' as_task_regr_st(cookfarm_sample, target = "PHIHOX",
#'   coords_as_features = FALSE,
#'   crs = 26911,
#'   coordinate_names = c("x", "y"))
#'
#' # sf
#' cookfarm_sf = sf::st_as_sf(cookfarm_sample, coords = c("x", "y"), crs = 26911)
#' as_task_regr_st(cookfarm_sf, target = "PHIHOX")
#'
#' # TaskRegrST
#' task = tsk("cookfarm")
#' as_task_regr_st(task)
as_task_regr_st = function(x, ...) {
  UseMethod("as_task_regr_st", x)
}

#' @rdname as_task_regr_st
#' @export
as_task_regr_st.TaskRegrST = function(x, clone = FALSE, ...) { # nolint
  if (clone) x$clone() else x
}

#' @rdname as_task_regr_st
#' @export
as_task_regr_st.data.frame = function(x, target = NULL, id = deparse(substitute(x)),
  crs = NA, coords_as_features = FALSE, coordinate_names = NA, ...) {
  TaskRegrST$new(id = id, backend = x, target = target,
    extra_args = list(coordinate_names = coordinate_names, coords_as_features = coords_as_features,
      crs = crs))
}

#' @rdname as_task_regr_st
#' @export
as_task_regr_st.DataBackend = function(x, target = NULL, id = deparse(substitute(x)),
  crs = NA, coords_as_features = FALSE, coordinate_names = c("x", "y"), ...) {
  TaskRegrST$new(id = id, backend = x, target = target,
    extra_args = list(coordinate_names = coordinate_names, coords_as_features = coords_as_features,
      crs = crs))
}

#' @rdname as_task_regr_st
#' @export
as_task_regr_st.sf = function(x, target = NULL, id = deparse(substitute(x)),
  coords_as_features = FALSE, ...) {
  TaskRegrST$new(id = id, backend = x, target = target,
    extra_args = list(coords_as_features = coords_as_features))
}

#' @inherit mlr3::as_task_regr title return
#' @inheritParams mlr3::as_task_regr
#' @description
#' Convert object to a [TaskRegr].
#' This is a S3 generic for [TaskRegrST].
#' @seealso [mlr3::as_task_regr()]
as_task_regr = function(x, ...) {
  UseMethod("as_task_regr", x)
}

#' @rdname as_task_regr
#' @inheritParams mlr3::convert_task
#' @export
as_task_regr.TaskRegrST = function(x) {
  TaskRegr$new(id = x$id, backend = x$backend, target = x$target_names)
}
