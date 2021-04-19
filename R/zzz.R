#' @rawNamespace import(data.table, except = transpose)
#' @importFrom R6 R6Class
#' @importFrom mlr3 TaskClassif TaskRegr Resampling as_data_backend assert_task rsmp tsk rsmps lrn msr mlr_resamplings
#' @import mlr3misc
#' @import checkmate
#' @import paradox
#' @import ggplot2
#' @importFrom utils globalVariables
#' @section Main resources:
#' * Book on mlr3: \url{https://mlr3book.mlr-org.com}
#' * mlr3book section about spatiotemporal data: \url{https://mlr3book.mlr-org.com/spatiotemporal.html}
#' * package vignettes: \url{https://mlr3spatiotempcv.mlr-org.com/dev/articles/}
#'
#' ## Miscellaneous \pkg{mlr3} content
#' * Use cases and examples: \url{https://mlr3gallery.mlr-org.com}
#' * More classification and regression tasks: \CRANpkg{mlr3data}
#' * Connector to [OpenML](https://www.openml.org): \CRANpkg{mlr3oml}
#' * More classification and regression learners: \CRANpkg{mlr3learners}
#' * Even more learners: \url{https://github.com/mlr-org/mlr3extralearners}
#' * Preprocessing and machine learning pipelines: \CRANpkg{mlr3pipelines}
#' * Tuning of hyperparameters: \CRANpkg{mlr3tuning}
#' * Visualizations for many \pkg{mlr3} objects: \CRANpkg{mlr3viz}
#' * Survival analysis and probabilistic regression: \CRANpkg{mlr3proba}
#' * Cluster analysis: \CRANpkg{mlr3cluster}
#' * Feature selection filters: \CRANpkg{mlr3filters}
#' * Feature selection wrappers: \CRANpkg{mlr3fselect}
#' * Interface to real (out-of-memory) data bases: \CRANpkg{mlr3db}
#' * Performance measures as plain functions: \CRANpkg{mlr3measures}
#' * Parallelization framework: \CRANpkg{future}
#' * Progress bars: \CRANpkg{progressr}
#' @references
#' `r format_bib("schratz2019")`
#'
#' `r format_bib("valavi2018")`
#'
#' `r format_bib("meyer2018")`
#'
#' `r format_bib("zhao2002")`
"_PACKAGE"

utils::globalVariables(c(
  "row_id", "cookfarm_sample", "ecuador", "diplodia",
  "resampling", "task", "indicator", "fold", "id", "type"))

register_mlr3 = function() { # nocov start
  # reflections ----------------------------------------------------------------

  x = utils::getFromNamespace("mlr_reflections", ns = "mlr3")

  x$task_types = setkeyv(rbind(x$task_types, rowwise_table(
    ~type, ~package, ~task, ~learner, ~prediction, ~measure,
    "regr", "mlr3spatiotempcv", "TaskRegrST", "LearnerRegr", "PredictionRegr",
    "MeasureRegr",

    "classif", "mlr3spatiotempcv", "TaskClassifST", "LearnerClassif",
    "PredictionClassif", "MeasureClassif"
  )), "type")

  # append "coordinates" to col_roles
  x$task_col_roles$classif_st = append(x$task_col_roles$classif, "coordinates")
  x$task_col_roles$regr_st = append(x$task_col_roles$regr, "coordinates")

  # tasks --------------------------------------------------------------------

  mlr_tasks = utils::getFromNamespace("mlr_tasks", ns = "mlr3")

  mlr_tasks$add("ecuador", load_task_ecuador)
  mlr_tasks$add("diplodia", load_task_diplodia)
  mlr_tasks$add("cookfarm", load_task_cookfarm)

  # resampling methods ---------------------------------------------------------

  mlr_resamplings = utils::getFromNamespace("mlr_resamplings", ns = "mlr3")
  mlr_resamplings$add("spcv_block", ResamplingSpCVBlock)
  mlr_resamplings$add("spcv_buffer", ResamplingSpCVBuffer)
  mlr_resamplings$add("spcv_disc", ResamplingSpCVDisc)
  mlr_resamplings$add("spcv_tiles", ResamplingSpCVTiles)
  mlr_resamplings$add("sptcv_cstf", ResamplingSptCVCstf)
  mlr_resamplings$add("sptcv_cluto", ResamplingSptCVCluto)
  mlr_resamplings$add("spcv_coords", ResamplingSpCVCoords)
  mlr_resamplings$add("spcv_env", ResamplingSpCVEnv)

  mlr_resamplings$add("repeated_spcv_coords", ResamplingRepeatedSpCVCoords)
  mlr_resamplings$add("repeated_spcv_env", ResamplingRepeatedSpCVEnv)
  mlr_resamplings$add("repeated_spcv_block", ResamplingRepeatedSpCVBlock)
  mlr_resamplings$add("repeated_spcv_disc", ResamplingRepeatedSpCVDisc)
  mlr_resamplings$add("repeated_spcv_tiles", ResamplingRepeatedSpCVTiles)
  mlr_resamplings$add("repeated_sptcv_cluto", ResamplingRepeatedSptCVCluto)
  mlr_resamplings$add("repeated_sptcv_cstf", ResamplingRepeatedSptCVCstf)
}

.onLoad = function(libname, pkgname) { # nolint
  register_mlr3()
  setHook(packageEvent("mlr3", "onLoad"), function(...) register_mlr3(),
    action = "append")
}

.onUnload = function(libpath) { # nolint
  event = packageEvent("mlr3", "onLoad")
  hooks = getHook(event)
  pkgname = vapply(hooks, function(x) environment(x)$pkgname, NA_character_)
  setHook(event, hooks[pkgname != "mlr3spatiotempcv"], action = "replace")
}

leanify_package() # nocov end
