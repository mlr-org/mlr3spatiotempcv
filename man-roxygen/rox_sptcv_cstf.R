#' @inherit CAST::CreateSpacetimeFolds description details note
#'
#' @note Standard k-fold cross-validation can lead to considerable
#'   misinterpretation in spatial-temporal modelling tasks. This function can be
#'   used to prepare a Leave-Location-Out, Leave-Time-Out or
#'   Leave-Location-and-Time-Out cross-validation as target-oriented validation
#'   strategies for spatial-temporal prediction tasks. See Meyer et al. (2018)
#'   for further information.
#'
#' @section {mlr3spatiotempcv} notes:
#'
#' The 'Description', 'Details' and 'Note' fields are inherited from the
#' respective upstream function.
#'
#' For a list of available arguments, please see [CAST::CreateSpacetimeFolds].
