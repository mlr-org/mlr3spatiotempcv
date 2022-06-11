# helper for resampling instances with a list structure
# (default is a data.table with `row_id` and `fold` columns)
# applies to:
# - SpCVDisc
# - SpCVCstf
# - SpCVTiles
format_resampling_list = function(task, resampling) {
  data = task$data()
  data$row_id = task$row_ids
  data$indicator = ""
  coords = get_coordinates(task)
  coords$row_id = task$row_ids

  if (any(grepl("ResamplingRepeated", class(resampling)))) {
    n_iters = resampling$iters / resampling$repeats(resampling$iters)
  } else {
    n_iters = resampling$iters
  }

  for (i in seq_len(n_iters)) {
    if (any(grepl("ResamplingSpCVBuffer", class(resampling)))) {
      row_id_train = coords[row_id %in% resampling$train_set(n_iters)][["row_id"]]
      row_id_test = coords[row_id %in% resampling$test_set(n_iters)][["row_id"]]
    } else {
      row_id_test = resampling$instance$test[[i]]
      row_id_train = resampling$instance$train[[i]]
    }

    data$test[data$row_id %in% row_id_test] = i
    data$train[data$row_id %in% row_id_train] = i
  }

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
      levels = c("Train", "Test", "Omitted")
    )
  } else {

    # reorder factor levels so that "train" comes first
    object$indicator = ordered(object$indicator, levels = c("Train", "Test"))
  }
  return(object)
}

#' Stratified random sampling
#' @param data (`data.table`)\cr
#'   Data
#' @param col (`character(1)`)\cr
#'   Column to stratify on.
#' @param n (`integer`)\cr
#'   Sample size per group
#' @keywords internal
strat_sample_folds = function(data, col, n) {
  assertClass(data, "data.table")
  assert_int(n)

  # col = substitute2(col)

  if (!is.null(n)) {
    assert_integer(n)
    if (n > min(setDT(data)[, .N, keyby = col][, N])) {
      lg$error(sprintf("The minimum sample per fold group must be less or equal to the number of observations in the smallest fold group (%s).", min(setDT(data)[, .N, keyby = col][, N])))
      stopf()
    }
    data = data[, .SD[sample(x = .N, size = n)],
      by = col
    ]
  }
  return(data)
}
