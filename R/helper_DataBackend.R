#' Get CRS from Task or Backend
#' @description
#'   Returns the CRS of the given Task or Backend.
#' @inheritParams mlr3::assert_task
#' @param pretty `[logical]`\cr
#'   If `TRUE`, returns the pretty coordinate information instead of raw CRS
#'   string.
#' @keywords internal
get_crs = function(task, pretty = TRUE) {
  if (checkmate::test_class(task$backend, "DataBackendVector")) {
    sf::st_crs(task$backend$geometry)
  } else if (checkmate::test_class(task$backend, "DataBackendRaster")) {
    terra::crs(task$backend$stack[[1]], describe = pretty)
  } else {
    task$crs
  }
}

#' Get coordinates from Task or Backend
#' @description
#'   Returns the coordinates of the given Task or Backend.
#' @inheritParams mlr3::assert_task
#' @keywords internal
get_coordinates = function(task) {
  if (checkmate::test_class(task$backend, "DataBackendVector")) {
    as.data.table(sf::st_coordinates(task$backend$geometry))
  } else if (checkmate::test_class(task$backend, "DataBackendRaster")) {
    as.data.table(terra::crds(task$backend$stack[[1]]))
  } else {
    task$coordinates()
  }
}

#' Get coordinates names from Task or Backend
#' @description
#'   Returns the coordinates names of the given Task or Backend.
#' @inheritParams mlr3::assert_task
#' @keywords internal
get_coordinate_names = function(task) {
  if (checkmate::test_class(task$backend, "DataBackendVector")) {
    names(as.data.table(sf::st_coordinates(task$backend$geometry)))
  } else if (checkmate::test_class(task$backend, "DataBackendRaster")) {
    names(as.data.table(terra::crds(task$backend$stack[[1]])))
  } else {
    task$coordinate_names
  }
}

#' Check spatial task
#' @description
#'   Assertion helper for spatial mlr3 tasks.
#' @inheritParams mlr3::assert_task
#' @keywords internal
assert_spatial_task = function(task) {
  if (!checkmate::test_multi_class(task$backend, c("DataBackendVector", "DataBackendRaster")) &&
    !checkmate::test_multi_class(task, c("TaskClassifST", "TaskRegrST"))) {
    stopf("Assertion on 'task' failed: Must inherit from class 'TaskClassifST', 'TaskRegrST' or a 'Task' with 'DataBackendVector' or 'DataBackendRaster'.") # nolint
  }
}
