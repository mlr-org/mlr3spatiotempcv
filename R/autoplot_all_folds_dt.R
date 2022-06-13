autoplot_all_folds_dt = function(task, resampling, sample_fold_n, fold_id,
  repeats_id, plot_as_grid, show_omitted = FALSE, show_blocks = FALSE, show_labels = FALSE, ...) {
  # take stratified random sample from folds
  if (!is.null(sample_fold_n)) {
    resampling = strat_sample_folds(resampling, "fold", sample_fold_n)
  }

  sf_df = sf::st_as_sf(resampling,
    coords = task$coordinate_names,
    crs = task$crs)

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
      data = sf_df["fold"], show.legend = "point",
      aes(color = fold),
      ...
    ) +
    ggsci::scale_color_ucscgb() +
    labs(color = sprintf("Partition #, Rep %s", repeats_id))
  return(plot)
}
