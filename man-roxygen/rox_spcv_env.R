#' @description
#' Splits data by clustering in the feature space.
#' See the upstream implementation at `blockCV::cv_cluster()` and
#' Valavi et al. (2018) for further information.
#'
#' @section Parameters:
#' * `folds` (`integer(1)`)\cr
#'   Number of folds.
#' * `features` (`character()`)\cr
#'   The features to use for clustering.
#'
#' @details
#' Useful when the dataset is supposed to be split on environmental information
#' which is present in features.
#' The method allows for a combination of multiple features for clustering.
#'
#' The input of raster images directly as in `blockCV::cv_cluster()` is not
#' supported. See \CRANpkg{mlr3spatial} and its raster DataBackends for such
#' support in \CRANpkg{mlr3}.
