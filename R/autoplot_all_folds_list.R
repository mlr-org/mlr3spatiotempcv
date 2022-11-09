autoplot_all_folds_list = function(task, resampling, sample_fold_n, fold_id, repeats_id) {
  data_coords = format_resampling_list(task, resampling)

  # extract test ids from lists
  row_ids_test = data.table::rbindlist(
    lapply(resampling$instance$test, as.data.table),
    idcol = "fold")
  setnames(row_ids_test, c("fold", "row_id"))

  test_folds = merge(data_coords, row_ids_test, by = "row_id", all = TRUE)

  # take stratified random sample from folds
  if (!is.null(sample_fold_n)) {
    test_folds = strat_sample_folds(test_folds, "test", sample_fold_n)
  }

  sf_df = sf::st_as_sf(test_folds,
    coords = task$coordinate_names,
    crs = task$crs)

  # only keep test ids
  sf_df = stats::na.omit(sf_df, cols = "fold")

  # order fold ids
  sf_df = sf_df[order(sf_df$fold, decreasing = FALSE), ]
  sf_df$fold = as.factor(as.character(sf_df$fold))
  sf_df$fold = factor(sf_df$fold, levels = unique(as.character(sf_df$fold)))

  # for all non-repeated rsmp cases
  if (is.null(repeats_id)) {
    repeats_id = 1 # nocov
  }

  plot = ggplot() +
    geom_sf(
      data = sf_df["fold"], show.legend = "point", linewidth = 0.5,
      aes(color = fold)
    ) +
    ggsci::scale_color_ucscgb() +
    labs(color = sprintf("Partition #, Rep %s", repeats_id))
  return(plot)
}
