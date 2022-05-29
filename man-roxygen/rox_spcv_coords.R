#' @description
#' Splits data by clustering in the coordinate space.
#' See the upstream implementation at `sperrorest::partition_kmeans()` and
#' Brenning (2012) for further information.
#'
#' @section Parameters:
#' * `folds` (`integer(1)`)\cr
#'   Number of folds.
#'
#' @details
#' Universal partitioning method that splits the data in the coordinate space.
#' Useful for spatially homogeneous datasets that cannot be split well with
#' rectangular approaches like `ResamplingSpCVBlock`.
#'
