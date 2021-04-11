#'
#' @description `partition_disc` partitions the sample into training and tests
#'   set by selecting circular test areas (possibly surrounded by an exclusion
#'   buffer) and using the remaining samples as training samples
#'   (leave-one-disc-out cross-validation).
#'
#'   This method is similar to [ResamplingSpCVBuffer].
#'
#' @note Test area discs are centered at (random) samples, not at general random
#'   locations. Test area discs may (and likely will) overlap independently of
#'   the value of `replace`. `replace` only controls the replacement of the
#'   center point of discs when drawing center points from the samples.
#'
#' @seealso ResamplingSpCVBuffer
