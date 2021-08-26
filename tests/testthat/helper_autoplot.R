# for tsk()
loadNamespace("mlr3")

prepare_autoplot = function(resampling_type, task = NULL, ...) {
  if (is.null(task)) {
    task = test_make_twoclass_task()
  }
  rsp = rsmp(resampling_type, ...)$instantiate(task)

  return(list(rsp = rsp, task = task))
}
