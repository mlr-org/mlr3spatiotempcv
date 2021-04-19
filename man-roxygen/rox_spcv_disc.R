#' @inherit sperrorest::partition_disc description details note references
#'
#' @note Test area discs are centered at (random) samples, not at general random
#'   locations. Test area discs may (and likely will) overlap independently of
#'   the value of `replace`. `replace` only controls the replacement of the
#'   center point of discs when drawing center points from the samples.
#'
#' @section {mlr3spatiotempcv} notes:
#'
#' The 'Description' and 'Note' fields are inherited from the respective
#' upstream function.
#'
#' For a list of available arguments, please see [sperrorest::partition_disc].
#'
#' This method is similar to [ResamplingSpCVBuffer].
#'
#' @seealso ResamplingSpCVBuffer
