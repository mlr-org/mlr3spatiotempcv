#' @description
#' Spatial partitioning using circular test areas of one of more observations.
#' Optionally, a buffer around the test area can be used to exclude observations.
#' See the upstream implementation at `sperrorest::partition_disc()` and
#' Brenning (2012) for further information.
#'
#' @section Parameters:
#' * `folds` (`integer(1)`)\cr
#'   Number of folds.
#' * `radius` (`numeric(1)`)\cr
#'   Radius of test area disc.
#' * `buffer` (`integer(1)`)\cr
#'   Radius around test area disc which is excluded from training or test set.
#' * `prob` (`integer(1)`)\cr
#'   Optional argument passed down to `sample()`.
#' * `replace` (`logical(1)`)\cr
#'   Optional argument passed down to `sample()`. Sample with or without
#'   replacement.
