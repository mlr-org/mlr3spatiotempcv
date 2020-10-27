#' @title Ecuador Classification Task
#'
#' @docType data
#' @usage data(ecuador)
#' @name mlr_tasks_ecuador
#' @format [R6::R6Class] inheriting from [TaskClassif].
#'
#' @section Usage:
#' ```
#' mlr_tasks$get("ecuador")
#' tsk("ecuador")
#' ```
#'
#' @description
#' Data set created by Jannes Muenchow, University of Erlangen-Nuernberg,
#' Germany. This dataset should be cited as Muenchow et al. (2012) (see reference
#' below). The publication also contains additional information on data
#' collection and the geomorphology of the area. The data set provided here is (a
#' subset of) the one from the 'natural' part of the RBSF area and corresponds
#' to landslide distribution in the year 2000.
#'
#' @references Muenchow, J., Brenning, A., Richter, M., 2012. Geomorphic process
#'   rates of landslides along a humidity gradient in the tropical Andes.
#'   Geomorphology, 139-140: 271-284.
#' @template seealso_task
"ecuador"

load_task_ecuador = function(id = "ecuador") {
  b = mlr3::as_data_backend(ecuador)
  b$hash = "_mlr3_tasks_ecuador_"
  task = TaskClassifST$new(
    id = "ecuador", b, target = "slides", positive = "TRUE",
    extra_args = list(
      coordinate_names = c("x", "y"), coords_as_features = FALSE,
      crs = "+proj=utm +zone=17 +south +datum=WGS84 +units=m +no_defs"))
  return(task)
}
