#' @rawNamespace import(data.table, except = transpose)
#' @import paradox
#' @import mlr3misc
#' @importFrom R6 R6Class
#' @importFrom mlr3 Task Resampling mlr_reflections mlr_tasks mlr_resamplings
"_PACKAGE"

.onLoad = function(libname, pkgname) {
  # add 'coordinates' as col roles
  tmp = c("feature", "target", "label", "order", "groups", "weights", "coordinates")
  mlr_reflections$task_col_roles = list(
    regr = tmp,
    classif = tmp
  )

  # add spatial task
  mlr_tasks$add("ecuador", load_task_ecuador)

  # add spatial resampling methods
  mlr_resamplings$add("spcv-kmeans", ResamplingSpCVKmeans)
}
