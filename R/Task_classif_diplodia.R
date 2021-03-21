#' @title Diplodia Classification Task
#'
#' @docType data
#' @usage data(diplodia)
#' @name mlr_tasks_diplodia
#' @format [R6::R6Class] inheriting from [TaskClassif].
#'
#' @section Usage:
#' ```
#' mlr_tasks$get("diplodia")
#' tsk("diplodia")
#' ```
#'
#' @description Data set created by Patrick Schratz, University of Jena
#'   (Germany) and Eugenia Iturritxa, NEIKER, Vitoria-Gasteiz (Spain). This
#'   dataset should be cited as Schratz et al. (2019) (see reference below). The
#'   publication also contains additional information on data collection. The
#'   data set provided here shows infections of trees by the pathogen _Diplodia
#'   Sapinea_ in the Basque Country in Spain. Predictors are environmental
#'   variables like temperature, precipitation, soil and more.
#'
#' @references
#' `r format_bib("schratz2019")`
#'
#' @template seealso_task
"diplodia"

load_task_diplodia = function(id = "diplodia") {
  b = mlr3::as_data_backend(diplodia)
  b$hash = "_mlr3_tasks_diplodia_"
  task = TaskClassifST$new(
    id = "diplodia", b,
    target = "diplo01", positive = "1",
    extra_args = list(
      coordinate_names = c("x", "y"), coords_as_features = FALSE,
      crs = "+proj=utm +zone=30 +south +datum=WGS84 +units=m +no_defs"))
  return(task)
}
