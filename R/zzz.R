#' @rawNamespace import(data.table, except = transpose)
#' @import paradox
#' @import mlr3misc
#' @import checkmate
#' @import mlr3
#' @import ggplot2
#' @importFrom R6 R6Class
"_PACKAGE"

.onLoad <- function(libname, pkgname) {

  # add 'coordinates' as col roles
  tmp <- mlr_reflections$task_col_roles$regr
  mlr_reflections$task_col_roles <- list(
    regr = c(tmp, "coordinates"),
    classif = c(tmp, "coordinates")
  )

  # add spatial task
  mlr_tasks$add("ecuador", load_task_ecuador)

  # add spatial resampling methods
  mlr_resamplings$add("spcv-block", ResamplingSpCVBlock)
  mlr_resamplings$add("spcv-buffer", ResamplingSpCVBuffer)
  mlr_resamplings$add("spcv-coords", ResamplingSpCVCoords)
  mlr_resamplings$add("spcv-env", ResamplingSpCVEnv)
}
