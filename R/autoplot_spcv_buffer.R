#' @title Visualization Functions for SpCV Buffer Methods.
#'
#' @description Generic S3 `plot()` and `autoplot()` (ggplot2) methods to
#'   visualize mlr3 spatiotemporal resampling objects.
#'
#' @name autoplot.ResamplingSpCVBuffer
#' @inheritParams autoplot.ResamplingSpCVBlock
#' @param object `[Resampling]`\cr
#'   mlr3 spatial resampling object of class [ResamplingSpCVBuffer].
#' @param x `[Resampling]`\cr
#'   mlr3 spatial resampling object of class [ResamplingSpCVBuffer].
#' @export
#' @seealso
#'   - mlr3book chapter on on ["Spatiotemporal Visualization"](https://mlr3book.mlr-org.com/spatiotemporal.html#vis-spt-partitions).
#'   - [autoplot.ResamplingSpCVBlock()]
#'   - [autoplot.ResamplingSpCVCoords()]
#'   - [autoplot.ResamplingSpCVEnv()]
#'   - [autoplot.ResamplingCV()]
#'   - [autoplot.ResamplingSptCVCstf()]
#'   - [autoplot.ResamplingSptCVCluto()]
#' @examples
#' \donttest{
#' if (mlr3misc::require_namespaces(c("sf", "blockCV"), quietly = TRUE)) {
#'   library(mlr3)
#'   library(mlr3spatiotempcv)
#'   task = tsk("ecuador")
#'   resampling = rsmp("spcv_buffer", theRange = 1000)
#'   resampling$instantiate(task)
#'
#'   ## single fold
#'   autoplot(resampling, task, fold_id = 1) +
#'     ggplot2::scale_x_continuous(breaks = seq(-79.085, -79.055, 0.01))
#'
#'   ## multiple folds
#'   autoplot(resampling, task, fold_id = c(1, 2)) *
#'     ggplot2::scale_x_continuous(breaks = seq(-79.085, -79.055, 0.01))
#' }
#' }
autoplot.ResamplingSpCVBuffer = function( # nolint
  object,
  task,
  fold_id = NULL,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  ...) {

  mlr3misc::require_namespaces(c("sf", "patchwork", "ggtext"))
  resampling = object

  resampling = assert_autoplot(resampling, fold_id, task)

  # add the row_ids of the task to the coordinates
  coords = task$coordinates()
  coords$row_id = task$row_ids

  if (is.null(fold_id)) {
    stopf("Please provide a fold ID.
      Plotting all folds of a LOOCV instance is not supported", wrap = TRUE)
  }

  plot_list = mlr3misc::map(fold_id, function(.x) {

    coords_train = coords[row_id %in% resampling$train_set(.x)]
    coords_test = coords[row_id %in% resampling$test_set(.x)]

    coords_train$indicator = "Train"
    coords_test$indicator = "Test"

    table = rbind(coords_train, coords_test)

    sf_df = sf::st_as_sf(table,
      coords = task$extra_args$coordinate_names,
      crs = task$extra_args$crs)

    sf_df$indicator = as.factor(as.character(sf_df$indicator))

    # reorder factor levels so that "train" comes first
    sf_df$indicator = ordered(sf_df$indicator, levels = c("Train", "Test"))

    plot = ggplot() +
      geom_sf(data = sf_df, aes(color = .data[["indicator"]]), ...) +
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
      ))
    return(invisible(plot))

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

#' @rdname autoplot.ResamplingSpCVBuffer
#' @export
plot.ResamplingSpCVBuffer = function(x, ...) {
  print(autoplot(x, ...)) # nocov
}
