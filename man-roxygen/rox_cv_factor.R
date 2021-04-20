#' @inherit sperrorest::partition_factor description details
#'
#' @section {mlr3spatiotempcv} notes:
#'
#' Argument factor should refer to a variable name within the data.
#' Passing a custom vector with length `nrow(data)` as in \pkg{sperrorest} is
#' not supported.
#'
#' By default the supplied variable is not used as a feature during modeling.
#' If you want to include it as a feature, set `factor_as_feature = TRUE` during
#' creation of the resampling instance.
#'
#' This method does not have a repeated variant as the partitions are fixed and
#' there is no possibility for variation.
#' If you want to have more flexibility with groups in your data, consider
#' using the `groups` column role as described in [mlr3::Task].
#' Note that this is currently not supported in \pkg{mlr3spatiotempcv}.
#'
#' The 'Description' and 'Details' fields are inherited from the respective
#' upstream function.
#'
#' For a list of available arguments, please see [sperrorest::partition_factor].
#'
#' @seealso ResamplingSptCVCstf
