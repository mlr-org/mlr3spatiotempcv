#' @title Convert to a SpatioTemporal Classification Task
#'
#' @description
#' Convert object to a [TaskClassifST].
#' This is a S3 generic, specialized for at least the following objects:
#'
#' 1. [TaskClassifST]: ensure the identity
#' 2. [data.frame()] and [DataBackend]: provides an alternative to the constructor of [TaskClassifST].
#' 2. [sf::sf].
#'
#' @inheritParams mlr3::as_task_classif
#' @param coordinate_names (`character()`)\cr 
#'   Name of the coordinate columns. Defaults to `c("x", "y")`.
#'
#' @return [TaskClassifST].
#' @export
#' @examples
#' data("ecuador", package = "mlr3spatiotempcv")
#' as_task_classif_st(ecuador, target = "slides", positive = "TRUE")
#'
#' ecuador_sf = sf::st_as_sf(ecuador, coords = c("x", "y"), crs = 4326)
#' as_task_classif_st(ecuador_sf, target = "slides", positive = "TRUE")
as_task_classif_st = function(x, ...) {
  UseMethod("as_task_classif_st")
}


#' @rdname as_task_classif_st
#' @export
as_task_classif_st.TaskClassifST = function(x, clone = FALSE, ...) { # nolint
  if (clone) x$clone() else x
}


#' @rdname as_task_classif_st
#' @export
as_task_classif_st.data.frame = function(x, target = NULL, id = deparse(substitute(x)), positive = NULL, coordinate_names = c("x", "y"), ...) { # nolint
  TaskClassifST$new(id = id, backend = x, target = target, positive = positive,
    extra_args = list(coordinate_names = coordinate_names))
}


#' @rdname as_task_classif_st
#' @export
as_task_classif_st.DataBackend = function(x, target = NULL, id = deparse(substitute(x)), positive = NULL, coordinate_names = c("x", "y"), ...) { # nolint
  TaskClassifST$new(id = id, backend = x, target = target, positive = positive,
    extra_args = list(coordinate_names = coordinate_names))
}

#' @rdname as_task_classif_st
#' @export
as_task_classif_st.sf = function(x, target = NULL, id = deparse(substitute(x)), positive = NULL, ...) { # nolint
  TaskClassifST$new(id = id, backend = x, target = target, positive = positive)
}
