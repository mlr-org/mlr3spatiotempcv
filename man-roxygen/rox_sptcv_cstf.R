#' @description
#' Splits data using Leave-Location-Out (LLO), Leave-Time-Out (LTO) and Leave-Location-and-Time-Out (LLTO) Cross-Validation.
#' See `CreateSpacetimeFolds()` from package \CRANpkg{CAST} and Meyer et al. (2018) for further information.
#'
#' @section Parameters:
#' * `folds` (`integer(1)`)\cr
#'   Number of folds.
#' * `stratify`\cr
#'   If `TRUE`, stratify on the target column.
#'
#' @details
#' LLO predicts on unknown locations i.e. complete locations are left out in the training sets.
#' The `"space"` role in `Task$col_roles` identifies spatial units.
#' If `stratify` is `TRUE`, the target distribution is similar in each fold.
#' This is useful for land cover classification when the observations are polygons.
#' In this case, LLO with stratification should be used to hold back complete polygons and have a similar target distribution in each fold.
#' LTO leaves out complete temporal units which are identified by the `"time"`  role in `Task$col_roles`.
#' LLTO leaves out spatial and temporal units.
#' See the examples.
