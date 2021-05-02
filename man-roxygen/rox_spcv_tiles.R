#' @inherit sperrorest::partition_tiles description note
#'
#' @section {mlr3spatiotempcv} notes:
#'
#' The 'Description' and 'Note' fields are inherited from the respective
#' upstream function.
#'
#' For a list of available arguments, please see [sperrorest::partition_tiles].
#'
#' This method is similar to [ResamplingSpCVBlock].
#'
#' @note Default parameter settings may change in future releases. This
#'   function, especially the rotation and shifting part of it and the algorithm
#'   for cleaning up small tiles is still a bit experimental. Use with caution.
#'   For non-zero offsets (`offset!='none')`), the number of tiles may actually
#'   be greater than `nsplit[1]*nsplit[2]` because of fractional tiles lurking
#'   into the study region. `reassign=TRUE` with suitable thresholds is
#'   therefore recommended for non-zero (including random) offsets.
#'
#' @seealso ResamplingSpCVBlock
