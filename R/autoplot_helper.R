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
