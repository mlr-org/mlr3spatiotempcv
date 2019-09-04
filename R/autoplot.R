#' @title Visualization of spatial resampling.
#'
#' @description Generates plots for [ResamplingSpCVBlock], [ResamplingSpCVEnv],
#'   [ResamplingSpCVBuffer] and [ResamplingSpCVCoords].
#'
#' @import ggplot2
#' @param object [ResamplingSpCVBlock]\cr
#'   An instantianated spatial resampling.
#' @param task [TaskClassifST]\cr
#'   A spatial task.
#' @param fold_id [numeric()]\cr
#'   For which fold ids should training and test sets be visualized?
#' @param grid (`logical(1)`)\cr
#'   Should a gridded plot using `cowplot::plot_grid()` be created? If ´FALSE` only a
#'   list with all ggplot2 resamplings is returned. Default is `TRUE`. Only applies
#'   if a numeric vector is passed to argument `fold_id`.
#' @param ... Currently not used
#'
#' @details By default a plot is returned; if `fold_id` is set, a gridded plot
#'   is created. If `grid = FALSE`, only a list of ggplot2 resamplings is returned.
#'   This gives the option to align the single plots manually.
#'
#' @rdname autoplot_spatial_resampling
#' @return [ggplot()] resampling.
#' @export
#' @examples
#' #####
#' # SpCVBlock
#' #####
#' \dontrun{
#' library(mlr3)
#' task = tsk("ecuador")
#' resampling = rsmp("spcv-block")
#' resampling$param_set$values = list(folds = 4)
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
autoplot.ResamplingSpCVBlock = function(object, task, fold_id = NULL, grid = TRUE, ...) {
  autoplot_spatial(resampling = object, task = task, fold_id = fold_id,
    grid = grid)
}

#' @title Plot for Spatial Resampling
#'
#' @description
#' Generates plots for [mlr3spatiotemporal::ResamplingSpCVEnv].
#'
#' @inheritParams autoplot.ResamplingSpCVBlock
#'
#' @rdname autoplot_spatial_resampling
#' @return [ggplot()] resampling.
#' @export
#' @examples
#' #####
#' # SpCVEnv
#' #####
#' \dontrun{
#' resampling = rsmp("spcv-env")
#' resampling$param_set$values = list(folds = 4, features = c("dem"))
#' resampling$instantiate(task)
#' autoplot(resampling, task)
#' autoplot(resampling, task, 1)
#' autoplot(resampling, task, c(1, 2, 3, 4))
#' }
autoplot.ResamplingSpCVEnv = function(object, task, fold_id = NULL, grid = TRUE, ...) {
  autoplot_spatial(resampling = object, task = task, fold_id = fold_id,
    grid = grid)
}

#' @title Plot for Spatial Resampling
#'
#' @inheritParams autoplot.ResamplingSpCVBlock
#'
#' @rdname autoplot_spatial_resampling
#' @return [ggplot()] resampling.
#' @export
#' @examples
#' #####
#' # SpCVBuffer
#' #####
#' \dontrun{
#' resampling = rsmp("spcv-buffer")
#' resampling$param_set$values = list(range = 1000)
#' resampling$instantiate(task)
#' autoplot(resampling, task, 1)
#' autoplot(resampling, task, c(1, 2, 3, 4))
#' }
autoplot.ResamplingSpCVBuffer = function(object, task, fold_id = NULL, grid = TRUE, ...) {
  autoplot_spatial(resampling = object, task = task, fold_id = fold_id,
    grid = grid)

}

#' @title Plot for Spatial Resampling
#'
#' @description
#' Generates plots for [mlr3spatiotemporal::ResamplingSpCVCoords].
#'
#' @inheritParams autoplot.ResamplingSpCVBlock
#'
#' @rdname autoplot_spatial_resampling
#' @return [ggplot()] resampling.
#' @export
#' @examples
#' #####
#' # SpCVCoords
#' #####
#' \dontrun{
#' resampling = rsmp("spcv-coords")
#' resampling$instantiate(task)
#' autoplot(resampling, task)
#' autoplot(resampling, task, 1)
#' autoplot(resampling, task, c(1, 2, 3, 4))
#' }
autoplot.ResamplingSpCVCoords = function(object, task, fold_id = NULL, grid = TRUE, ...) {
  autoplot_spatial(resampling = object, task = task, fold_id = fold_id,
    grid = grid)
}

#' @title Plot for Spatial Resampling
#'
#' @description
#' Workhorse for autoplot.ResamplingSpCV*.
#'
#' @importFrom data.table data.table rbindlist
#' @importFrom mlr3misc imap
#' @import ggplot2
#'
#' @param resampling [ResamplingSpCVCoords]
#' @param task [TaskClassifST]
#' @param fold_id [numeric()]
#' @param ... Currently not used
#'
#' @return [ggplot()] resampling.
#' @export
#' @keywords internal
autoplot_spatial = function(resampling, task, fold_id = NULL, grid = TRUE) {
  coords = task$coordinates()
  coords$row_id = task$row_ids

  if (class(resampling)[1] == "ResamplingSpCVBuffer") {

    coords_resamp = imap(resampling$instance, function(x, y) {
      inds = data.table(row_id = resampling$instance[[y]]$train,
        fold = y,
        indicator = "Train")
      ind_test = data.table(row_id = resampling$instance[[y]]$test,
        fold = y,
        indicator = "Test")
      inds_comb = rbind(inds, ind_test)

      merge(coords, inds_comb, by = "row_id")
    })
    coords_resamp = rbindlist(coords_resamp)
  } else {
    # attach the coordinates to the resampling indices
    coords_resamp = merge(coords, resampling$instance, by = "row_id")
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

      if (!class(resampling)[1] == "ResamplingSpCVBuffer") {
        table[, indicator := ifelse(fold == i, "Test", "Train")]
      } else {
        # for ResamplingSpCVBuffer we already have a correct data.table
        # we only need to subset it to the desired fold(s)
        table = subset(table, fold %in% i)
      }

      sf_df = sf::st_as_sf(table, coords = c("x", "y"), crs = task$crs)
      sf_df$indicator = as.factor(as.character(sf_df$indicator))

      plot_list[[length(plot_list) + 1]] =
        ggplot() +
        geom_sf(data = sf_df, aes(color = indicator)) +
        scale_color_viridis_d() +
        labs(color = "Set") +
        theme(legend.position = "none")
    }

    # is a grid requested?
    if (!grid) {
      return(plot_list)
    } else {
      plots = do.call(cowplot::plot_grid, list(plotlist = plot_list,
        labels = sprintf("Fold %s", seq(1, length(plot_list)))))

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
  } else {
    # Create one plot with all (test)-folds
    sf_df = sf::st_as_sf(coords_resamp, coords = c("x", "y"), crs = task$crs)
    sf_df$fold = as.factor(as.character(sf_df$fold))

    ggplot() +
      geom_sf(data = sf_df, show.legend = "point",
        aes(color = fold)) +
      scale_color_viridis_d(breaks = sort(as.integer(as.character(sf_df$fold)))) +
      labs(color = "Test Set, Fold #")
  }
}
