# reducing redundant code parts
prepare_autoplot_cstf = function(task) {

  data = task$data()
  data$row_id = task$row_ids
  data$indicator = 0
  coords = task$coordinates()
  coords$row_id = task$row_ids

  for (i in seq_len(resampling$iters)) {

    row_id = resampling$instance$test[[i]]

    data$indicator[data$row_id %in% row_id] = i
  }
  data$Date = as.Date(data$Date)
  # merge the coords for the 3D plot
  data_coords = merge(data, coords, by = "row_id")
  return(data_coords)
}

# some checks
assert_autoplot = function(object, fold_id, task) {

  # instantiate if not yet done
  if (!object$is_instantiated) {
    object = object$instantiate(task)
  }

  if (!is.null(fold_id)) {
    if (length(fold_id) > object$iters) {
      stopf("More folds specified than stored in resampling.")
    }
    if (length(fold_id) == 1 && fold_id > object$iters) {
      stopf("Specified a fold id which exceeds the total number of folds.")
    }
    if (any(fold_id > object$iters)) {
      stopf("Specified a fold id which exceeds the total number of folds.")
    }
  }

  return(object)
}

reorder_levels = function(object) {

  # 'fold' needs to be a factor, otherwise `show.legend = "points" has no
  # effect

  object$indicator = as.factor(as.character(object$indicator))

  # reorder factor levels so that "train" comes first
  object$indicator = ordered(object$indicator, levels = c("Train", "Test"))

  return(object)

}
