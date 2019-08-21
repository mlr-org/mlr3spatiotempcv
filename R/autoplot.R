#' @title Plot for Spatial Resampling
#'
#' @description Generates plots for [mlr3spatiotemporal::ResamplingSpCVBlock].
#'
#' @import ggplot2
#' @param object [mlr3spatiotemporal::ResamplingSpCVBlock]
#' @param task [mlr3spatiotemporal::TaskClassifST]
#' @param fold_id [numeric()]\cr For which fold ids should training and test
#'   sets be visualized?
#' @param grid (`logical(1)`)\cr Should a gridded plot using
#'   `cowplot::plot_grid()` be created instead of returning a list with all
#'   ggplot2 objects? Default is `TRUE`. Only applies if `fold_id` is set.
#' @param ... Currently not used
#'
#' @details By default a plot is returned; if `fold_id` is set, a gridded plot
#' is created. If `grid = FALSE`, only a list of ggplot2 objects is returned.
#' This gives the option to align the single plots manually.
#'
#' @return [ggplot2::ggplot()] object.
#' @export
#' @examples
#' library(mlr3)
#' library(ggplot2)
#' task = mlr3::mlr_tasks$get("ecuador")
#' resampling = ResamplingSpCVBlock$new()
#' resampling$param_set$values = list(folds = 4)
#' resampling$instantiate(task)
#' autoplot(resampling, task)
#' autoplot(resampling, task, fold_id = 1)
#' autoplot(resampling, task, fold_id = c(1, 2, 3, 4))
#' # return only a list of ggplot2 objects
#' plot_list = autoplot(resampling, task, fold_id = c(1, 2, 3, 4), grid = FALSE)
autoplot.ResamplingSpCVBlock = function(object, task, fold_id = NULL, grid = TRUE,
                                        ...) {

  coords = task$coordinates()
  coords$row_id = task$row_ids

  # attach the coordinates to the resampling indices
  coords_resamp = merge(coords, object$instance, by = "row_id")

  # plot train and test of a specific fold?
  if (!is.null(fold_id)) {
    if (length(fold_id) > object$iters) {
      stop("More folds specified than stored in resampling")
    }
    # Multiplot with train and test set
    plot_list = list()
    for (i in fold_id) {
      table = copy(coords_resamp)

      # suppress undefined global variables note
      indicator = NULL
      fold = NULL

      table[, indicator := ifelse(fold == i, "Test", "Train")]

      sf_df = sf::st_as_sf(table, coords = c("x", "y"), crs = task$crs)
      sf_df$indicator = as.factor(as.character(sf_df$indicator))

      plot_list[[length(plot_list) + 1]] =
        ggplot() +
        geom_sf(data = sf_df, aes(color = indicator)) +
        scale_color_viridis_d() +
        labs(color = "Set") +
        theme(legend.position = "none")
    }
    plots = do.call(cowplot::plot_grid, list(plotlist = plot_list,
                                             labels = sprintf("Fold %s", seq(1, length(plot_list)))))

    # is a grid requested?
    if (isTRUE(grid)) {
    # Extract legend standalone, we only want one legend in the grid
    coords_resamp[, indicator := ifelse(fold == 1, "Test", "Train")]

    sf_df = sf::st_as_sf(coords_resamp, coords = c("x", "y"), crs = task$crs)
    # 'fold' needs to be a factor, otherwise `show.legend = "points" has no effect
    sf_df$indicator = as.factor(as.character(sf_df$indicator))

    legend = cowplot::get_legend(ggplot() +
      geom_sf(data = sf_df, aes(color = indicator),
                       show.legend = "point") +
      scale_color_viridis_d() +
      labs(color = "Set") +
      theme(legend.position = "bottom"))

    # Plot
    cowplot::plot_grid(plots, legend, ncol = 1, rel_heights = c(1, .1))
    }
    else {
      # return plots as list
      return(plots)
    }
  } else {
    # Create one plot with all (test)-folds
    sf_df = sf::st_as_sf(coords_resamp, coords = c("x", "y"), crs = task$crs)
    sf_df$fold = as.factor(as.character(sf_df$fold))

    ggplot() +
      geom_sf(data = sf_df, show.legend = "point",
                       aes(color = fold)) +
      scale_color_viridis_d() +
      labs(color = "Test Set, Fold #")

  }
}

#' @title Plot for Spatial Resampling
#'
#' @description
#' Generates plots for [mlr3spatiotemporal::ResamplingSpCVBuffer].
#'
#' @param object [mlr3spatiotemporal::ResamplingSpCVBuffer].
#' @param task [mlr3spatiotemporal::TaskClassifST]
#' @param fold_id [numeric()]
#' @param ... Currently not used
#'
#' @return [ggplot2::ggplot()] object.
#' @export
#' @examples
#' library(mlr3)
#' library(ggplot2)
#' task = mlr3::mlr_tasks$get("ecuador")
#' resampling = ResamplingSpCVBuffer$new()
#' resampling$param_set$values = list(range = 1000)
#' resampling$instantiate(task)
#' autoplot(resampling, task, 1)
autoplot.ResamplingSpCVBuffer = function(object, task, fold_id, ...) {

  coords = task$coordinates()
  coords$row_id = task$row_ids

  coords_train = coords[coords$row_id %in% object$instance[[fold_id]]$train]
  coords_test = coords[coords$row_id %in% object$instance[[fold_id]]$test]

  coords_train$indicator = "train"
  coords_test$indicator = "test"

  coords_df = rbind(coords_train, coords_test)

  sf_df = sf::st_as_sf(coords_df, coords = c("x", "y"))

  # suppress undefined global variables note
  indicator = NULL

  ggplot() +
    geom_sf(data = sf_df, aes(color = indicator)) +
    scale_color_viridis_d()
}

#' @title Plot for Spatial Resampling
#'
#' @description
#' Generates plots for [mlr3spatiotemporal::ResamplingSpCVEnv].
#'
#' @param object [mlr3spatiotemporal::ResamplingSpCVEnv]
#' @param task [mlr3spatiotemporal::TaskClassifST]
#' @param fold_id [numeric()]
#' @param ... Currently not used
#'
#' @return [ggplot2::ggplot()] object.
#' @export
#' @examples
#' library(mlr3)
#' library(ggplot2)
#' task = mlr3::mlr_tasks$get("ecuador")
#' resampling = ResamplingSpCVEnv$new()
#' resampling$param_set$values = list(folds = 4, features = c("dem"))
#' resampling$instantiate(task)
#' autoplot(resampling, task)
#' autoplot(resampling, task, 1)
#' autoplot(resampling, task, c(1, 2, 3, 4))
autoplot.ResamplingSpCVEnv = function(object, task, fold_id = NULL, ...) {

  coords = task$coordinates()
  coords$row_id = task$row_ids

  coords_resamp = merge(coords, object$instance, by = "row_id")

  # suppress undefined global variables note
  indicator = NULL
  fold = NULL

  if (!is.null(fold_id)) {
    if (length(fold_id) > object$iters) {
      stop("More folds specified than stored in resampling")
    }
    # Multiplot with train and test set
    plot_list = list()
    for (i in fold_id) {
      table = copy(coords_resamp)
      table[, indicator := ifelse(fold == i, "Test", "Train")]

      sf_df = sf::st_as_sf(table, coords = c("x", "y"), crs = task$crs)
      sf_df$indicator = as.factor(as.character(sf_df$indicator))

      plot_list[[length(plot_list) + 1]] =
        ggplot() +
        geom_sf(data = sf_df, aes(color = indicator)) +
        scale_color_viridis_d() +
        labs(color = "Set") +
        theme(legend.position = "none")
    }
    plots = do.call(cowplot::plot_grid, plot_list)

    # Get legend
    coords_resamp[, indicator := ifelse(fold == 1, "Test", "Train")]

    sf_df = sf::st_as_sf(coords_resamp, coords = c("x", "y"), crs = task$crs)
    sf_df$indicator = as.factor(as.character(sf_df$indicator))


    legend = cowplot::get_legend(ggplot() +
      geom_sf(data = sf_df, aes(color = indicator)) +
      scale_color_viridis_d() +
      labs(color = "Set") +
      theme(legend.position = "bottom"))

    # Plot
    cowplot::plot_grid(plots, legend, ncol = 1, rel_heights = c(1, .1))
  } else {
    # Plot with folds
    sf_df = sf::st_as_sf(coords_resamp, coords = c("x", "y"), crs = task$crs)
    sf_df$fold = as.factor(as.character(sf_df$fold))

    ggplot() +
      geom_sf(data = sf_df, aes(color = fold)) +
      scale_color_viridis_d() +
      labs(color = "Fold")
  }
}

#' @title Plot for Spatial Resampling
#'
#' @description
#' Generates plots for [mlr3spatiotemporal::ResamplingSpCVCoords].
#'
#' @param object [mlr3spatiotemporal::ResamplingSpCVCoords]
#' @param task [mlr3spatiotemporal::TaskClassifST]
#' @param fold_id [numeric()]
#' @param ... Currently not used
#'
#' @return [ggplot2::ggplot()] object.
#' @export
#' @examples
#' library(mlr3)
#' library(ggplot2)
#' task = mlr3::mlr_tasks$get("ecuador")
#' resampling = ResamplingSpCVCoords$new()
#' resampling$param_set$values = list(folds = 4)
#' resampling$instantiate(task)
#' autoplot(resampling, task)
#' autoplot(resampling, task, 1)
#' autoplot(resampling, task, c(1, 2, 3, 4))
autoplot.ResamplingSpCVCoords = function(object, task, fold_id = NULL, ...) {

  coords = task$coordinates()
  coords$row_id = task$row_ids

  coords_resamp = merge(coords, object$instance, by = "row_id")

  # suppress undefined global variables note
  indicator = NULL
  fold = NULL

  if (!is.null(fold_id)) {
    if (length(fold_id) > object$iters) {
      stop("More folds specified than stored in resampling")
    }
    # Multiplot with train and test set
    plot_list = list()
    for (i in fold_id) {
      table = copy(coords_resamp)
      table[, indicator := ifelse(fold == i, "Test", "Train")]

      sf_df = sf::st_as_sf(table, coords = c("x", "y"), crs = task$crs)
      sf_df$indicator = as.factor(as.character(sf_df$indicator))

      plot_list[[length(plot_list) + 1]] =
        ggplot() +
        geom_sf(data = sf_df, aes(color = indicator)) +
        scale_color_viridis_d() +
        labs(color = "Set") +
        theme(legend.position = "none")
    }
    plots = do.call(cowplot::plot_grid, plot_list)

    # Get legend
    coords_resamp[, indicator := ifelse(fold == 1, "Test", "Train")]

    sf_df = sf::st_as_sf(coords_resamp, coords = c("x", "y"), crs = task$crs)
    sf_df$indicator = as.factor(as.character(sf_df$indicator))

    legend = cowplot::get_legend(ggplot() +
      geom_sf(data = sf_df, aes(color = indicator)) +
      scale_color_viridis_d() +
      labs(color = "Set") +
      theme(legend.position = "bottom"))

    # Plot
    cowplot::plot_grid(plots, legend, ncol = 1, rel_heights = c(1, .1))
  } else {
    # Plot with folds
    sf_df = sf::st_as_sf(coords_resamp, coords = c("x", "y"), crs = task$crs)
    sf_df$fold = as.factor(as.character(sf_df$fold))

    ggplot() +
      geom_sf(data = sf_df, aes(color = fold)) +
      scale_color_viridis_d() +
      labs(color = "Fold")
  }
}
