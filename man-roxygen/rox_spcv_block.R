#' @inherit blockCV::cv_spatial description details
#'
#' @section {mlr3spatiotempcv} notes:
#'
#' By default [blockCV::cv_spatial()] does not allow the creation of multiple
#' repetitions. `mlr3spatiotempcv` adds support for this when using the `size`
#' argument for fold creation. When supplying a vector of `length(repeats)` for
#' argument `size`, these different settings will be used to create folds which
#' differ among the repetitions.
#'
#' Multiple repetitions are not possible when using the "row & cols" approach
#' because the created folds will always be the same.
#'
#' The 'Description' and 'Details' fields are inherited from the respective
#' upstream function.
#'
#' For a list of available arguments, please see [blockCV::cv_spatial].
#'
#' `blockCV` >= 3.0.0 changed the argument names of the implementation. For backward compatibility, `mlr3spatiotempcv` is still using the old ones.
#' Here's a list which shows the mapping between `blockCV` < 3.0.0 and `blockCV` >= 3.0.0:
#' - `range` -> `size`
#' - `rasterLayer` -> `r`
#' - `speciesData` -> `points`
#' - `showBlocks` -> `plot`
#' - `cols` and `rows` -> `rows_cols`
#'
#' The default of argument `hexagon` is different in `mlr3spatiotempcv` (`FALSE` instead of `TRUE`) to create square blocks instead of hexagonal blocks by default.
