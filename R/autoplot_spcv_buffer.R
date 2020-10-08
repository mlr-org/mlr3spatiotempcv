#' @rdname autoplot_spatial_resampling
#' @export
#' @examples
#' ##########
#' # SpCVBuffer
#' ##########
#' \dontrun{
#' library(mlr3)
#' task = tsk("ecuador")
#' resampling = rsmp("spcv_buffer", theRange = 1000)
#' resampling$instantiate(task)
#'
#' autoplot(resampling, task, 1)
#' autoplot(resampling, task, c(1, 2))
#' }
autoplot.ResamplingSpCVBuffer = function( # nolint
  object,
  task,
  fold_id = NULL,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  crs = NULL,
  ...) {

  require_namespaces(c("sf", "patchwork", "ggtext"))
  resampling = object

  resampling = assert_autoplot(resampling, fold_id, task)

  # add the row_ids of the task to the coordinates
  coords = task$coordinates()
  coords$row_id = task$row_ids

  if (is.null(fold_id)) {
    cli::cli_alert_danger("Please provide a fold ID.")
    stopf("Plotting all folds of a LOOCV instance is not supported")
  }

  plot_list = mlr3misc::map(fold_id, function(.x) {

    coords_train = coords[row_id %in% resampling$train_set(.x)]
    coords_test = coords[row_id %in% resampling$test_set(.x)]

    coords_train$indicator = "Train"
    coords_test$indicator = "Test"

    table = rbind(coords_train, coords_test)

    # set fallback crs if missing
    if (is.null(crs)) {
      # use 4326 (WGS84) as fallback
      crs = 4326
      cli::cli_alert_info("CRS not set, transforming to WGS84 (EPSG: 4326).")
    }
    # transform to selected crs
    sf_df = sf::st_transform(
      sf::st_as_sf(table, coords = c("x", "y"), crs = task$extra_args$crs),
      crs = crs)

    sf_df$indicator = as.factor(as.character(sf_df$indicator))

    # reorder factor levels so that "train" comes first
    sf_df$indicator = ordered(sf_df$indicator, levels = c("Train", "Test"))

    ggplot() +
      geom_sf(data = sf_df, aes(color = .data[["indicator"]])) +
      scale_color_manual(values = c(
        "Train" = train_color,
        "Test" = test_color
      )) +
      labs(color = "Set", title = sprintf("Fold %s", .x)) +
      theme(plot.title = ggtext::element_textbox(
        size = 10,
        color = "black", fill = "#ebebeb", box.color = "black",
        height = unit(0.33, "inch"), width = unit(1, "npc"),
        linetype = 1, r = unit(5, "pt"),
        valign = 0.5, halign = 0.5,
        padding = margin(2, 2, 2, 2), margin = margin(3, 3, 3, 3)
      )
      )

  })

  # is a grid requested?
  if (!plot_as_grid) {
    return(plot_list)
  } else {
    plot_list_pw = patchwork::wrap_plots(plot_list) +
      patchwork::plot_layout(guides = "collect")
    return(plot_list_pw)
  }
}

#' @rdname autoplot_spatial_resampling
#' @export
plot.ResamplingSpCVBuffer = function(x, ...) {
  print(autoplot(x, ...))
}
