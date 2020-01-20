#' @title Visualization of spatial resampling.
#'
#' @description Generates plots for [ResamplingSpCVBlock], [ResamplingSpCVEnv],
#'   [ResamplingSpCVBuffer] and [ResamplingSpCVCoords].
#'
#' @importFrom data.table data.table rbindlist
#' @import ggplot2
#'
#' @param object [Resampling]\cr
#'   mlr3 spatial resampling object. One of class [ResamplingSpCVBuffer],
#'   [ResamplingSpCVBlock], [ResamplingSpCVCoords], [ResamplingSpCVEnv].
#' @param task [TaskClassifST]/[TaskRegrST]\cr
#'   mlr3 task object.
#' @param fold_id [numeric]\cr
#'   Fold IDs to plot.
#' @param repeats_id [numeric]\cr
#'   Repetition ID to plot.
#' @param grid (`logical(1)`)\cr Should a gridded plot using
#'   `cowplot::plot_grid()` be created? If `FALSE` only a list with all
#'   \CRANpkg{ggplot2} resamplings is returned. Default is `TRUE`. Only applies
#'   if a numeric vector is passed to argument `fold_id`.
#' @param train_color `character(1)`\cr
#'   The color to use for the training set observations.
#' @param test_color `character(1)`\cr
#'   The color to use for the test set observations.
#' @param ... Not used.
#'
#' @details By default a plot is returned; if `fold_id` is set, a gridded plot
#'   is created. If `grid = FALSE`, only a list of ggplot2 resamplings is
#'   returned. This gives the option to align the single plots manually.
#'
#'   When no single fold is selected, the [ggsci::scale_color_ucscgb()] palette
#'   is used to display all partitions. If you want to change the colors,
#'   call `<plot> + <color-palette>()`
#' @return [ggplot()] or list of ggplot2 objects.

#' @title Plot for Spatial Resampling
#'
#' @rdname autoplot_spatial_resampling

#' @export
#' @examples
#' \donttest{
#' #####
#' # SpCVBlock
#' #####
#' library(mlr3)
#' task = tsk("ecuador")
#' resampling = rsmp("spcv-block", range = 1000)
#' resampling$instantiate(task)
#'
#' ## Visualize all partitions
#' autoplot(resampling, task)
#'
#' ## Visualize the train/test split of a single fold
#' autoplot(resampling, task, fold_id = 1)
#'
#' ## Visualize train/test splits of multiple folds
#' autoplot(resampling, task, fold_id = c(1, 2, 3, 4))
#'
#' # return only a list of ggplot2 resamplings
#' plot_list = autoplot(resampling, task,
#'   fold_id = c(1, 2, 3, 4), grid = FALSE)
#' }
autoplot.ResamplingSpCVBlock = function(
  object,
  task,
  fold_id = NULL,
  grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  ...) {
  autoplot_spatial(resampling = object,
    task = task,
    fold_id = fold_id,
    grid = grid)
}

#' @title Plot for Spatial Resampling
#'
#' @rdname autoplot_spatial_resampling
#' @export
#' @examples
#' #####
#' # SpCVEnv
#' #####
#' resampling = rsmp("spcv-env", folds = 4, features = c("dem"))
#' resampling$instantiate(task)
#' autoplot(resampling, task)
#' autoplot(resampling, task, 1)
#' autoplot(resampling, task, c(1, 2, 3, 4))
autoplot.ResamplingSpCVEnv = function(
  object,
  task,
  fold_id = NULL,
  grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  ...) {
  autoplot_spatial(resampling = object,
    task = task,
    fold_id = fold_id,
    grid = grid)
}

#' @title Plot for Spatial Resampling
#'
#' @rdname autoplot_spatial_resampling
#' @export
#' @examples
#' #####
#' # SpCVBuffer
#' #####
#' resampling = rsmp("spcv-buffer", range = 1000)
#' resampling$instantiate(task)
#' autoplot(resampling, task, 1)
#' autoplot(resampling, task, c(1, 2, 3, 4))
autoplot.ResamplingSpCVBuffer = function(
  object,
  task,
  fold_id = NULL,
  grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  ...) {
  autoplot_spatial(resampling = object,
    task = task,
    fold_id = fold_id,
    grid = grid)

}

#' @title Plot for Spatial Resampling
#'
#' @rdname autoplot_spatial_resampling
#' @export
#' @examples
#' #####
#' # SpCVCoords
#' #####
#' resampling = rsmp("spcv-coords")
#' resampling$instantiate(task)
#' autoplot(resampling, task)
#' autoplot(resampling, task, 1)
#' autoplot(resampling, task, c(1, 2, 3, 4))
autoplot.ResamplingSpCVCoords = function(
  object,
  task,
  fold_id = NULL,
  grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  ...) {
  autoplot_spatial(resampling = object,
    task = task,
    fold_id = fold_id,
    grid = grid)
}

#' @title Plot for Repeated Spatial Resampling
#'
#' @rdname autoplot_spatial_resampling
#' @export
#' @examples
#' #####
#' # RepeatedSpCVCoords
#' #####
#' task = tsk("diplodia")
#' resampling = rsmp("repeated-spcv-coords", folds = 10, repeats = 2)
#' resampling$instantiate(task)
#' autoplot(resampling, task)
#' autoplot(resampling, task, 1)
#' autoplot(resampling, task, fold_id = 2, repeats_id = 2)
#' autoplot(resampling, task, c(1, 2, 3, 4))
autoplot.ResamplingRepeatedSpCVCoords = function(
  object,
  task,
  fold_id = NULL,
  repeats_id = 1,
  grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  ...) {
  autoplot_spatial(resampling = object,
    task = task,
    fold_id = fold_id,
    repeats_id = repeats_id,
    grid = grid)
}

#' @title Plot for Repeated Spatial Resampling
#'
#' @rdname autoplot_spatial_resampling
#' @export
#' @examples
#' #####
#' # RepeatedSpCVEnv
#' #####
#' task = tsk("ecuador")
#' resampling = rsmp("repeated-spcv-env", folds = 10, repeats = 2)
#' resampling$instantiate(task)
#' autoplot(resampling, task)
#' autoplot(resampling, task, 1)
#' autoplot(resampling, task, fold_id = 2, repeats_id = 2)
#' autoplot(resampling, task, c(1, 2, 3, 4))
autoplot.ResamplingRepeatedSpCVEnv = function(
  object,
  task,
  fold_id = NULL,
  repeats_id = 1,
  grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  ...) {
  autoplot_spatial(resampling = object,
    task = task,
    fold_id = fold_id,
    repeats_id = repeats_id,
    grid = grid)
}

#' @title Plot for Repeated Spatial Resampling
#'
#' @rdname autoplot_spatial_resampling
#' @export
#' @examples
#' #####
#' # RepeatedSpCVBlock
#' #####
#' task = tsk("ecuador")
#' resampling = rsmp("repeated-spcv-block", folds = 5, repeats = 2,
#'   range = c(1000, 1500))
#' resampling$instantiate(task)
#' autoplot(resampling, task)
#' autoplot(resampling, task, 1)
#' autoplot(resampling, task, fold_id = 2, repeats_id = 2)
#' autoplot(resampling, task, c(1, 2, 3, 4))
autoplot.ResamplingRepeatedSpCVBlock = function(
  object,
  task,
  fold_id = NULL,
  repeats_id = 1,
  grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  ...) {
  autoplot_spatial(resampling = object,
    task = task,
    fold_id = fold_id,
    repeats_id = repeats_id,
    grid = grid)
}

autoplot_spatial = function(
  resampling = NULL,
  task = NULL,
  fold_id = NULL,
  repeats_id = NULL,
  grid = NULL,
  train_color = NULL,
  test_color = NULL) {

  coords = task$coordinates()
  coords$row_id = task$row_ids
  require_namespaces(c("sf", "cowplot"))

  # instantiante if not yet done
  if (!resampling$is_instantiated) {
    resampling = resampling$instantiate(task)
  }

  coords_resamp = merge(coords, resampling$instance, by = "row_id")

  if (grepl("Repeated", class(resampling)[1])) {
    coords_resamp = coords_resamp[rep == repeats_id, ]
  }

  if (class(resampling)[1] == "ResamplingSpCVBuffer" && is.null(fold_id)) {
    stop("Plotting all folds of a LOOCV instance is not supported. Please provide a fold ID.") # nolint
  }

  # plot train and test of a specific fold?
  if (!is.null(fold_id)) {
    if (length(fold_id) > resampling$iters) {
      stop("More folds specified than stored in resampling.")
    }
    if (length(fold_id) == 1 && fold_id > resampling$iters) {
      stop("Specified a fold id which exceeds the total number of folds.")
    }
    if (any(fold_id > resampling$iters)) {
      stop("Specified a fold id which exceeds the total number of folds.")
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

      # reorder factor levels so that "train" comes first
      sf_df$indicator = ordered(sf_df$indicator, levels = c("Train", "Test"))

      plot_list[[length(plot_list) + 1]] =
        ggplot() +
        geom_sf(data = sf_df, aes(color = indicator)) +
        scale_color_manual(values = c("Train" = "#0072B5", "Test" = "#E18727")) +
        labs(color = "Set") +
        theme(legend.position = "none")
    }

    # is a grid requested?
    if (!grid) {
      return(plot_list)
    } else {
      # for repeated cv we also print out the rep number
      if (is.null(repeats_id)) {
        repeats_id = 1
      }
      plots = do.call(cowplot::plot_grid, list(plotlist = plot_list,
        labels = sprintf("Fold %s\nRep %s", fold_id,
          repeats_id)))

      # Extract legend standalone, we only want one legend in the grid
      coords_resamp[, indicator := ifelse(fold == 1, "Test", "Train")]

      sf_df = sf::st_as_sf(coords_resamp, coords = c("x", "y"), crs = task$crs)
      # 'fold' needs to be a factor, otherwise `show.legend = "points" has no
      # effect
      sf_df$indicator = as.factor(as.character(sf_df$indicator))

      # reorder factor levels so that "train" comes first
      sf_df$indicator = ordered(sf_df$indicator, levels = c("Train", "Test"))

      legend = cowplot::get_legend(ggplot() +
        geom_sf(data = sf_df, aes(color = indicator),
          show.legend = "point") +
        scale_color_manual(values = c("Train" = "#0072B5",
          "Test" = "#E18727")) +
        labs(color = "") +
        theme(legend.position = "bottom"))

      # Plot
      cowplot::plot_grid(plots, legend, ncol = 1, rel_heights = c(1, .1))
    }
  } else {
    # Create one plot with all (test)-folds
    sf_df = sf::st_as_sf(coords_resamp, coords = c("x", "y"), crs = task$crs)

    # order fold ids
    sf_df = sf_df[order(sf_df$fold, decreasing = FALSE), ]
    sf_df$fold = as.factor(as.character(sf_df$fold))
    sf_df$fold <- factor(sf_df$fold, levels = unique(as.character(sf_df$fold)))

    # for all non-repeated rsmp cases
    if (is.null(repeats_id)) {
      repeats_id = 1
    }
    ggplot() +
      geom_sf(data = sf_df, show.legend = "point",
        aes(color = fold)) +
      ggsci::scale_color_ucscgb() +
      labs(color = sprintf("Partition #, Rep %s", repeats_id))
  }
}
