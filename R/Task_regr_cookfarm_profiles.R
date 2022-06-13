#' @title Cookfarm Profiles Regression Task
#'
#' @docType data
#' @usage data(cookfarm_mlr3)
#' @name mlr_tasks_cookfarm
#' @format [R6::R6Class] inheriting from [TaskRegr].
#'
#' @section Usage:
#' ```
#' mlr_tasks$get("cookfarm")
#' tsk("cookfarm_mlr3")
#' ```
#'
#' @description
#' The R.J. Cook Agronomy Farm (cookfarm) is a Long-Term Agroecosystem Research
#' Site operated by Washington State University, located near Pullman,
#' Washington, USA. Contains spatio-temporal (3D+T) measurements of three soil
#' properties and a number of spatial and temporal regression covariates.
#'
#' Here, only the "Profiles" dataset is used from the collection.
#' The `Date` column was appended from the `readings` dataset.
#' In addition coordinates were appended to the task as variables `"x"` and `"y"`.
#'
#' The dataset was borrowed and adapted from package GSIF which was on archived on
#' CRAN in 2021-03.
#'
#' @section Column roles:
#' The task has set column roles "space" and "time" for variables `"Date"` and
#'  `"SOURCEID"`, respectively.
#' These are used by certain methods during partitioning, e.g.,
#'  `mlr_resamplings_sptcv_cstf` with variant "Leave-location-and-time-out".
#' If only one of space or time should left out, the column roles must be
#' adjusted by the user!
#'
#' @references
#' Gasch, C.K., Hengl, T., Gräler, B., Meyer, H., Magney, T., Brown, D.J., 2015.
#' Spatio-temporal interpolation of soil water, temperature, and electrical
#' conductivity in 3D+T: the Cook Agronomy Farm data set. Spatial Statistics,
#' 14, pp.70–90.
#'
#' Gasch, C.K., D.J. Brown, E.S. Brooks, M. Yourek, M. Poggio, D.R. Cobos, C.S.
#' Campbell, 2016? Retroactive calibration of soil moisture sensors using a
#' two-step, soil-specific correction. Submitted to Vadose Zone Journal.
#'
#' Gasch, C.K., D.J. Brown, C.S. Campbell, D.R. Cobos, E.S. Brooks, M. Chahal,
#' M. Poggio, 2016? A field-scale sensor network data set for monitoring and
#' modeling the spatial and temporal variation of soil moisture in a dryland
#' agricultural field. Submitted to Water Resources Research.
#' @template seealso_task
"cookfarm_mlr3"

load_task_cookfarm = function(id = "cookfarm_mlr3") {
  b = mlr3::as_data_backend(cookfarm_mlr3)
  b$hash = "_mlr3_tasks_cookfarm_"
  task = TaskRegrST$new(
    id = "cookfarm", b, target = "PHIHOX",
    coordinate_names = c("x", "y"), coords_as_features = FALSE,
    crs = 26911)
  task$set_col_roles("Date", roles = "time")
  task$set_col_roles("SOURCEID", roles = "space")
  return(task)
}
