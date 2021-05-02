#' @inherit blockCV::spatialBlock description details
#'
#' @section {mlr3spatiotempcv} notes:
#'
#' By default [blockCV::spatialBlock()] does not allow the creation of multiple
#' repetitions. `mlr3spatiotempcv` adds support for this when using the `range`
#' argument for fold creation. When supplying a vector of `length(repeats)` for
#' argument `range`, these different settings will be used to create folds which
#' differ among the repetitions.
#'
#' Multiple repetitions are not possible when using the "row & cols" approach
#' because the created folds will always be the same.
#'
#' The 'Description' and 'Details' fields are inherited from the respective
#' upstream function.
#'
#' For a list of available arguments, please see [blockCV::spatialBlock].
