get_crs = function(task) {
  if (checkmate::test_class(task$backend, "DataBackendVector")) {
    sf::st_crs(task$backend$geometry)
  } else {
    task$extra_args$crs
  }
}

get_coordinates = function(task) {
  if (checkmate::test_class(task$backend, "DataBackendVector")) {
    as.data.table(sf::st_coordinates(task$backend$geometry))
  } else {
    task$coordinates()
  }
}

assert_spatial_task = function(task) {
  if (!checkmate::test_class(task$backend, "DataBackendVector") &&
    !checkmate::test_multi_class(task, c("TaskClassifST", "TaskRegrST"))) {
    stopf("Assertion on 'task' failed: Must inherit from class 'TaskClassifST', 'TaskRegrST' or a 'Task' with 'DataBackendVector'.") # nolint
  }
}
