#' @title Diplodia Classification Task
#'
#' @name mlr_tasks_diplodia
#' @format [R6::R6Class] inheriting from [TaskClassif].
#' @importFrom mlr3 as_data_backend
#'
#' @section Usage: ``` mlr_tasks$get("diplodia") ```
#'
#' @description Data set created by Patrick Schratz, University of Jena
#'   (Germany) and Eugenia Iturritxa, NEIKER, Vitoria-Gasteiz (Spain). This
#'   dataset should be cited as Schratz et al. (2019) (see reference below). The
#'   publication also contains additional information on data collection. The
#'   data set provided here shows infections of trees by the pathogen _Diplodia
#'   Sapinea_ in the Basque Country in Spain. Predictors are environmental
#'   variables like temperature, precipitation, soil and more.
#'
#' @references Schratz, P., Muenchow, J., Iturritxa, E., Richter, J., &
#'   Brenning, A. (2019). Hyperparameter tuning and performance assessment of
#'   statistical and machine-learning algorithms using spatial data. Ecological
#'   Modelling, 406, 109–120. https://doi.org/10/gf34bd
NULL

load_task_diplodia = function(id = "diplodia") {
  b = as_data_backend(readRDS(system.file("extdata", "diplodia.rda",
    package = "mlr3spatiotemporal")))
  b$hash = "_mlr3_tasks_diplodia_"
  task = TaskClassifST$new(id, b, target = "diplo01", positive = "1",
    coordinates = c("x", "y"), coords_as_features = FALSE,
    crs = "+proj=utm +zone=30 +south +datum=WGS84 +units=m +no_defs")
}
