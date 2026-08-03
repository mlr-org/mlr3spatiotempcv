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
#'
#' @section Fold balancing:
#'
#' [blockCV::cv_spatial()] assigns blocks to folds by drawing `iteration`
#' random candidate assignments and keeping the most balanced one.
#' All balancing parameters only take effect for `selection = "random"`.
#'
#' By default the balancing objective is the total number of observations per
#' fold, which is what `mlr3spatiotempcv` has always used.
#' Setting `balance_on_target = TRUE` passes the target of the task to argument
#' `column` of [blockCV::cv_spatial()], which balances the response classes
#' (or the quantile bins of a continuous response) across the folds instead.
#' This is opt-in to keep the default fold assignment unchanged.
#'
#' @section Parameters:
#'
#' * `balance` (`logical(1)`)\cr
#'   Search `iteration` random block-to-fold assignments for a balanced one.
#'   If `FALSE`, a single random assignment is used.
#'   Default: `TRUE`.
#' * `iteration` (`integer(1)`)\cr
#'   Number of candidate assignments evaluated during the balancing search.
#'   Default: `100`.
#' * `balance_on_target` (`logical(1)`)\cr
#'   Balance the classes (or quantile bins) of the task target across folds
#'   instead of the plain number of observations.
#'   Default: `FALSE`.
#' * `num_bins` (`integer(1)`)\cr
#'   Number of quantile bins a continuous target is stratified into before
#'   balancing. Only relevant with `balance_on_target = TRUE`.
#'   Set to `NULL` to treat every unique value as its own class.
#'   Default: `4`.
#' * `presence_bg` (`logical(1)`)\cr
#'   Treat the target as presence-background data, i.e. balance only the
#'   presence records so that the abundant background points cannot dominate
#'   the objective. Requires `balance_on_target = TRUE`.
#'   For a two-class [TaskClassifST] the positive class of the task is used as
#'   the presence class, a numeric target must consist of `0`s and `1`s.
#'   Default: `FALSE`.
