#' Get CRS from Task or Backend
#' @description
#' @inheritParams mlr3::assert_task
#' @param pretty `[logical]`\cr
#'   If `TRUE`, returns the pretty coordinate information instead of raw CRS
#'   string.
get_crs = function(task, pretty = TRUE) {
  if (checkmate::test_class(task$backend, "DataBackendVector")) {
    sf::st_crs(task$backend$geometry)
  } else if (checkmate::test_class(task$backend, "DataBackendRaster")) {
    terra::crs(task$backend$stack[[1]], describe = pretty)
  } else {
    task$crs
  }
}

get_coordinates = function(task) {
  if (checkmate::test_class(task$backend, "DataBackendVector")) {
    as.data.table(sf::st_coordinates(task$backend$geometry))
  } else if (checkmate::test_class(task$backend, "DataBackendRaster")) {
    as.data.table(terra::crds(task$backend$stack[[1]]))
  } else {
    task$coordinates()
  }
}

get_coordinate_names = function(task) {
  if (checkmate::test_class(task$backend, "DataBackendVector")) {
    names(as.data.table(sf::st_coordinates(task$backend$geometry)))
  } else if (checkmate::test_class(task$backend, "DataBackendRaster")) {
    names(as.data.table(terra::crds(task$backend$stack[[1]])))
  } else {
    task$coordinate_names
  }
}

assert_spatial_task = function(task) {
  if (!checkmate::test_multi_class(task$backend, c("DataBackendVector", "DataBackendRaster")) &&
    !checkmate::test_multi_class(task, c("TaskClassifST", "TaskRegrST"))) {
    stopf("Assertion on 'task' failed: Must inherit from class 'TaskClassifST', 'TaskRegrST' or a 'Task' with 'DataBackendVector' or 'DataBackendRaster'.") # nolint
  }
}
