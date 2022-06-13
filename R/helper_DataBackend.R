#' Check spatial task
#' @description
#'   Assertion helper for spatial mlr3 tasks.
#' @inheritParams mlr3::assert_task
#' @keywords internal
assert_spatial_task = function(task) {
  if (!checkmate::test_multi_class(task, c("TaskClassifST", "TaskRegrST"))) {
    stopf("Assertion on 'task' failed: Must inherit from class 'TaskClassifST' or 'TaskRegrST'.") # nolint
  }
}
