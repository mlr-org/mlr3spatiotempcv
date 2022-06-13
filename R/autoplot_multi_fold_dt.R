
#' Autoplot helper
#' @param resampling Actual resampling object (needed for spcv_block with
#'   "show_blocks = TRUE")
#' @param resampling_mod Modified resampling object (normal data.table)
#' @keywords internal
autoplot_multi_fold_dt = function(task, resampling, resampling_mod,
  sample_fold_n, fold_id, repeats_id, plot_as_grid = FALSE,
  show_omitted = FALSE, show_blocks = FALSE, show_labels = FALSE, ...) {

  plot_list = mlr3misc::map(fold_id, function(.x) {

    dt = resampling_mod
    dt$indicator = NA_character_
    dt[, indicator := ifelse(fold == .x, "Test", "Train")]

    # take stratified random sample from folds
    if (!is.null(sample_fold_n)) {
      dt = strat_sample_folds(dt, "fold", sample_fold_n)
    }

    sf_df = sf::st_as_sf(dt,
      coords = get_coordinate_names(task),
      crs = get_crs(task))

    sf_df = reorder_levels(sf_df)

    if (show_blocks) {
      checkmate:: assert_multi_class(resampling,
        c("ResamplingSpCVBlock", "ResamplingRepeatedSpCVBlock"))
      if (any(grepl("ResamplingRepeated", class(resampling)))) {
        resampling_mod = resampling_mod[rep == repeats_id, ]
        blocks = resampling$blocks[[repeats_id]]
      } else {
        blocks = resampling$blocks
      }
      blocks = sf::st_set_crs(blocks, sf::st_crs(sf_df))

      plot = ggplot() +
        geom_sf(data = sf_df, aes(color = indicator), ...) +
        geom_sf(
          data = blocks, color = "black", alpha = 0,
          size = 0.7) +
        scale_color_manual(values = c(
          "Train" = "#0072B5",
          "Test" = "#E18727"
        )) +
        labs(color = "Set", title = sprintf(
          "Fold %s, Repetition %s", .x,
          repeats_id)) +
        theme(
          plot.title = ggtext::element_textbox(
            size = 10,
            color = "black", fill = "#ebebeb", box.color = "black",
            height = unit(0.33, "inch"), width = unit(1, "npc"),
            linetype = 1, r = unit(5, "pt"),
            valign = 0.5, halign = 0.5,
            padding = margin(2, 2, 2, 2), margin = margin(3, 3, 3, 3))
        )

      if (show_labels) {
        plot = plot +
          geom_sf_label(
            data = blocks, color = "black",
            label = blocks$fold,
            size = 2, label.padding = unit(0.1, "lines"),
            fun.geometry = function(x) {
              # Warning: In st_point_on_surface.sfc(sf::st_zm(x)) :
              # st_point_on_surface may not give correct results for
              # longitude/latitude data
              suppressWarnings(sf::st_point_on_surface(sf::st_zm(x)))
            }
          )
      }
    } else {

      plot = ggplot() +
        geom_sf(data = sf_df, aes(color = indicator), ...) +
        scale_color_manual(values = c(
          "Train" = "#0072B5",
          "Test" = "#E18727"
        )) +
        labs(color = "Set", title = sprintf(
          "Fold %s, Repetition %s", .x,
          repeats_id)) +
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
    return(plot)
  })

  if (length(fold_id) == 1) {
    return(plot_list[[1]])
  }

  if (!plot_as_grid) {
    return(invisible(plot_list))
  } else {
    # for repeated cv we also print out the rep number
    if (is.null(repeats_id)) {
      repeats_id = 1 # nocov
    }

    plot_list_pw = patchwork::wrap_plots(plot_list) +
      patchwork::plot_layout(guides = "collect")
    return(invisible(plot_list_pw))
  }
}
