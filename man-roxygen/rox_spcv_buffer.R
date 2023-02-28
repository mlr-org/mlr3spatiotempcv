#' @inherit blockCV::cv_buffer description details
#'
#' @section {mlr3spatiotempcv} notes:
#'
#' The 'Description' and 'Details' fields are inherited from the respective
#' upstream function.
#' For a list of available arguments, please see [blockCV::cv_buffer].
#'
#' `blockCV` >= 3.0.0 changed the argument names of the implementation. For backward compatibility, `mlr3spatiotempcv` is still using the old ones.
#' Here's a list which shows the mapping between `blockCV` < 3.0.0 and `blockCV` >= 3.0.0:
#' - `theRange` -> `size`
#' - `addBG` -> `add_bg`
#' - `spDataType` (character vector) -> `presence_bg` (boolean)
#'
#' @seealso ResamplingSpCVDisc
