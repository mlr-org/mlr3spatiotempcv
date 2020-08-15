#' @rawNamespace import(data.table, except = transpose)
#' @import paradox
#' @import mlr3misc
#' @import checkmate
#' @import mlr3
#' @import ggplot2
#' @importFrom R6 R6Class
#' @importFrom utils globalVariables
"_PACKAGE"

register_mlr3 = function() {
  # reflections ----------------------------------------------------------------
  x = utils::getFromNamespace("mlr_reflections", ns = "mlr3")

  if (!grepl("ST", x$task_types[, "task"])) {
    x = utils::getFromNamespace("mlr_reflections", ns = "mlr3")

    x$task_types = setkeyv(rbind(x$task_types, rowwise_table(
      ~type, ~package, ~task, ~learner, ~prediction, ~measure,
      "regr", "mlr3spatiotempcv", "TaskRegrST", "LearnerRegr", "PredictionRegr",
      "MeasureRegr",

      "classif", "mlr3spatiotempcv", "TaskClassifST", "LearnerClassif",
      "PredictionClassif", "MeasureClassif"
    )), "type")

    x$task_col_roles$regr = c(
      "feature", "target", "label", "order", "group",
      "weight", "coordinates")
    x$task_col_roles$classif = c(
      "feature", "target", "label", "order", "group",
      "weight", "coordinates")

    # tasks --------------------------------------------------------------------

    x = utils::getFromNamespace("mlr_tasks", ns = "mlr3")

    mlr_tasks$add("ecuador", load_task_ecuador)
    mlr_tasks$add("diplodia", load_task_diplodia)
    mlr_tasks$add("cookfarm", load_task_cookfarm)

    # resampling methods ---------------------------------------------------------

    x = utils::getFromNamespace("mlr_resamplings", ns = "mlr3")
    mlr_resamplings$add("spcv-block", ResamplingSpCVBlock)
    mlr_resamplings$add("spcv-buffer", ResamplingSpCVBuffer)
    mlr_resamplings$add("spcv-cstf", ResamplingSptCVCstf)
    mlr_resamplings$add("spcv-cluto", ResamplingSptCVCluto)
    mlr_resamplings$add("spcv-coords", ResamplingSpCVCoords)
    mlr_resamplings$add("spcv-env", ResamplingSpCVEnv)

    mlr_resamplings$add("repeated-spcv-coords", ResamplingRepeatedSpCVCoords)
    mlr_resamplings$add("repeated-spcv-env", ResamplingRepeatedSpCVEnv)
    mlr_resamplings$add("repeated-spcv-block", ResamplingRepeatedSpCVBlock)
    mlr_resamplings$add("repeated-spcv-cluto", ResamplingRepeatedSptCVCluto)
  }

  utils::globalVariables(c(
    "row_id", "cookfarm", "ecuador", "diplodia",
    "resampling", "task"))

}

.onLoad = function(libname, pkgname) { # nolint
  register_mlr3()
  setHook(packageEvent("mlr3", "onLoad"), function(...) register_mlr3(),
    action = "append")
}
