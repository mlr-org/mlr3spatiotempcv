# reducing redundant code parts
prepare_autoplot_cstf = function(task, resampling) {

  data = task$data()
  data$row_id = task$row_ids
  data$indicator = ""
  coords = task$coordinates()
  coords$row_id = task$row_ids

  if (grepl("Repeated", class(resampling)[1])) {
    n_iters = resampling$iters / resampling$repeats(resampling$iters)
  } else {
    n_iters = resampling$iters
  }

  for (i in seq_len(n_iters)) {

    row_id_test = resampling$instance$test[[i]]
    row_id_train = resampling$instance$train[[i]]

    data$test[data$row_id %in% row_id_test] = i
    data$train[data$row_id %in% row_id_train] = i
  }

  data$Date = as.Date(data$Date)
  # merge the coords for the 3D plot
  data_coords = merge(data, coords, by = "row_id")
  return(data_coords)
}

# some checks on the resampling object
assert_autoplot = function(object, fold_id, task) {

  # instantiate if not yet done
  if (!object$is_instantiated) {
    object = object$instantiate(task) # nocov
  }

  if (!is.null(fold_id)) {
    if (length(fold_id) > object$iters) {
      stopf("More folds specified than stored in resampling.") # nocov
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

  if ("Omitted" %in% levels(object$indicator)) {
    # reorder factor levels so that "train" comes first
    object$indicator = ordered(object$indicator,
      levels = c("Train", "Test", "Omitted"))
  } else {

    # reorder factor levels so that "train" comes first
    object$indicator = ordered(object$indicator, levels = c("Train", "Test"))
  }
  return(object)
}
