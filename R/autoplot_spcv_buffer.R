#' @rdname autoplot_spatial_resampling
#' @export
#' @examples
#' ##########
#' # SpCVBuffer
#' ##########
#' \dontrun{
#' library(mlr3)
#' resampling = rsmp("spcv-buffer", theRange = 1000)
#' resampling$instantiate(task)
#'
#' autoplot(resampling, task, 1)
#' autoplot(resampling, task, c(1, 2, 3, 4))
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

  require_namespaces(c("sf", "cowplot"))
  resampling = object

  resampling = assert_autoplot(resampling, fold_id, task)

  # add the row_ids of the task to the coordinates
  coords = task$coordinates()
  coords$row_id = task$row_ids

  if (is.null(fold_id)) {
    stopf("Plotting all folds of a LOOCV instance is not supported.
          Please provide a fold ID.", wrap = TRUE) # nolint
  }

  indicator = NULL

  plot_list = list()
  for (i in fold_id) {
    coords_train = coords[row_id %in% resampling$instance[[i]]$train]
    coords_test = coords[row_id %in% resampling$instance[[i]]$test]

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
      sf::st_as_sf(table, coords = c("x", "y"), crs = task$crs),
      crs = crs)

    sf_df$indicator = as.factor(as.character(sf_df$indicator))

    # reorder factor levels so that "train" comes first
    sf_df$indicator = ordered(sf_df$indicator, levels = c("Train", "Test"))

    plot_list[[length(plot_list) + 1]] =
      ggplot() +
      geom_sf(data = sf_df, aes(color = .data[["indicator"]])) +
      scale_color_manual(values = c(
        "Train" = train_color,
        "Test" = test_color
      )) +
      labs(color = "Set") +
      theme(legend.position = "none")
  }

  # is a grid requested?
  if (!plot_as_grid) {
    return(plot_list)
  } else {
    plots = do.call(cowplot::plot_grid, list(
      plotlist = plot_list,
      labels = sprintf("Fold %s", fold_id)
    ))

    # Extract legend standalone, we only want one legend in the grid
    coords_train = coords[row_id %in% resampling$instance[[1]]$train]
    coords_test = coords[row_id %in% resampling$instance[[1]]$test]

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
      sf::st_as_sf(table, coords = c("x", "y"), crs = task$crs),
      crs = crs)

    # 'fold' needs to be a factor, otherwise `show.legend = "points" has no
    # effect
    sf_df$indicator = as.factor(as.character(sf_df$indicator))

    # reorder factor levels so that "train" comes first
    sf_df$indicator = ordered(sf_df$indicator, levels = c("Train", "Test"))

    legend = cowplot::get_legend(ggplot() +
      geom_sf(
        data = sf_df, aes(color = indicator),
        show.legend = "point"
      ) +
      scale_color_manual(values = c(
        "Train" = "#0072B5",
        "Test" = "#E18727"
      )) +
      labs(color = "") +
      theme(legend.position = "bottom"))

    # Plot
    cowplot::plot_grid(plots, legend, ncol = 1, rel_heights = c(1, .1))
  }
}

#' @rdname autoplot_spatial_resampling
#' @export
plot.ResamplingSpCVBuffer = function(x, ...) {
  print(autoplot(x, ...))
}
