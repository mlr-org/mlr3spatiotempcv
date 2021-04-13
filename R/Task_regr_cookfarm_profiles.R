#' @title Cookfarm Profiles Regression Task
#'
#' @docType data
#' @usage data(cookfarm_sample)
#' @name mlr_tasks_cookfarm
#' @format [R6::R6Class] inheriting from [TaskRegr].
#'
#' @section Usage:
#' ```
#' mlr_tasks$get("cookfarm")
#' tsk("cookfarm")
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
#' 500 random samples were drawn from the complete sample.
#'
#' The dataset was borrowed and adapted from package GSIF which was on archived on
#' CRAN in 2021-03.
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
"cookfarm_sample"

load_task_cookfarm = function(id = "cookfarm") {
  b = mlr3::as_data_backend(cookfarm_sample)
  b$hash = "_mlr3_tasks_cookfarm_"
  task = TaskRegrST$new(
    id = "cookfarm", b, target = "PHIHOX",
    extra_args = list(
      coordinate_names = c("x", "y"), coords_as_features = FALSE,
      crs = 26911))
  return(task)
}
