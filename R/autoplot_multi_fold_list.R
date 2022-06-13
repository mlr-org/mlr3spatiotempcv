autoplot_multi_fold_list = function(task, resampling, sample_fold_n, fold_id,
  repeats_id, plot_as_grid = TRUE, show_omitted = FALSE, ...) {

  plot_list = mlr3misc::map(fold_id, function(.x) {

    data_coords = format_resampling_list(task, resampling)

    data_coords$indicator = NA_character_

    if (any(grepl("ResamplingSpCVBuffer", class(resampling)))) {
      spcv_buffer = TRUE
    } else {
      spcv_buffer = FALSE
    }

    if (spcv_buffer) {
      row_id_train = resampling$train_set(.x)
      row_id_test = resampling$test_set(.x)
    } else {
      row_id_test = resampling$instance$test[[.x]]
      row_id_train = resampling$instance$train[[.x]]
    }

    data_coords[list(row_id_train), "indicator" := "Train", on = "row_id"]
    data_coords[list(row_id_test), "indicator" := "Test", on = "row_id"]

    # take stratified random sample from folds
    if (!is.null(sample_fold_n)) {
      data_coords = strat_sample_folds(data_coords, "test", sample_fold_n)
    }

    # should omitted points be shown?
    if (show_omitted && nrow(data_coords[is.na(indicator)]) > 0) {
      data_coords[is.na(get("indicator")), "indicator" := "Omitted"]

      sf_df = sf::st_as_sf(data_coords,
        coords = get_coordinate_names(task),
        crs = get_crs(task))
      sf_df = reorder_levels(sf_df)

      plot = ggplot() +
        geom_sf(data = sf_df, aes(color = indicator), ...) +
        scale_color_manual(values = c(
          "Omitted" = "grey",
          "Train" = "#0072B5",
          "Test" = "#E18727"
        )) +
        {
          if (spcv_buffer) {
            labs(color = "Set", title = sprintf(
              "Fold %s", .x))
          } else {
            labs(color = "Set", title = sprintf(
              "Fold %s, Repetition %s", .x,
              repeats_id))
          }
        } +
        theme(
          plot.title = ggtext::element_textbox(
            size = 10,
            color = "black", fill = "#ebebeb", box.color = "black",
            height = unit(0.33, "inch"), width = unit(1, "npc"),
            linetype = 1, r = unit(5, "pt"),
            valign = 0.5, halign = 0.5,
            padding = margin(2, 2, 2, 2), margin = margin(3, 3, 3, 3))
        )
    } else {
      data_coords = data_coords[!is.na(get("indicator")), , ]

      sf_df = sf::st_as_sf(data_coords,
        coords = get_coordinate_names(task),
        crs = get_crs(task))
      sf_df = reorder_levels(sf_df)

      plot = ggplot() +
        geom_sf(data = sf_df, aes(color = indicator), ...) +
        scale_color_manual(values = c(
          "Train" = "#0072B5",
          "Test" = "#E18727"
        )) +
        {
          if (spcv_buffer) {
            labs(color = "Set", title = sprintf(
              "Fold %s", .x))
          } else {
            labs(color = "Set", title = sprintf(
              "Fold %s, Repetition %s", .x,
              repeats_id))
          }
        } +
        theme(
          plot.title = ggtext::element_textbox(
            size = 10,
            color = "black", fill = "#ebebeb", box.color = "black",
            height = unit(0.33, "inch"), width = unit(1, "npc"),
            linetype = 1, r = unit(5, "pt"),
            valign = 0.5, halign = 0.5,
            padding = margin(2, 2, 2, 2), margin = margin(3, 3, 3, 3))
        )
    }
    return(invisible(plot))
  })

  if (length(fold_id) == 1) {
    return(plot_list[[1]])
  }

  # Return a plot grid via patchwork?
  if (!plot_as_grid) {
    return(invisible(plot_list))
  } else {
    # for repeated cv we also print out the rep number
    if (is.null(repeats_id)) {
      repeats_id = 1 # nocov
    }

    plot_list_pw = patchwork::wrap_plots(plot_list) +
      patchwork::plot_layout(guides = "collect")
    return(plot_list_pw)
  }
}
