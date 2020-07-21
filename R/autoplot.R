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
#'   fold_id = c(1, 2, 3, 4), grid = FALSE
#' )
autoplot.ResamplingSpCVBlock = function( # nolint
  object,
  task,
  fold_id = NULL,
  grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  ...) {
  autoplot_spatial(
    resampling = object,
    task = task,
    fold_id = fold_id,
    grid = grid
  )
}

#' @title Plot for Spatial Resampling
#'
#' @rdname autoplot_spatial_resampling
#' @export
#' @examples
#' #####
#' # SpCVEnv
#' #####
#' library(mlr3)
#' resampling = rsmp("spcv-env", folds = 4, features = c("dem"))
#' resampling$instantiate(task)
#' autoplot(resampling, task)
#' autoplot(resampling, task, 1)
#' # autoplot(resampling, task, c(1, 2, 3, 4))
autoplot.ResamplingSpCVEnv = function( # nolint
  object,
  task,
  fold_id = NULL,
  grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  ...) {
  autoplot_spatial(
    resampling = object,
    task = task,
    fold_id = fold_id,
    grid = grid
  )
}

#' @title Plot for Spatial Resampling
#'
#' @rdname autoplot_spatial_resampling
#' @export
#' @examples
#' #####
#' # SpCVBuffer
#' #####
#' library(mlr3)
#' resampling = rsmp("spcv-buffer", theRange = 1000)
#' resampling$instantiate(task)
#' autoplot(resampling, task, 1)
#' # autoplot(resampling, task, c(1, 2, 3, 4))
autoplot.ResamplingSpCVBuffer = function( # nolint
  object,
  task,
  fold_id = NULL,
  grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  ...) {

  resampling = object
  coords = task$coordinates()
  coords$row_id = task$row_ids
  require_namespaces(c("sf", "cowplot"))

  # instantiate if not yet done
  if (!resampling$is_instantiated) {
    resampling = resampling$instantiate(task)
  }

  if (is.null(fold_id)) {
    stopf("Plotting all folds of a LOOCV instance is not supported.
          Please provide a fold ID.", wrap = TRUE) # nolint
  }

  # plot train and test of a specific fold?
  if (length(fold_id) > resampling$iters) {
    stopf("More folds specified than stored in resampling.")
  }
  if (length(fold_id) == 1 && fold_id > resampling$iters) {
    stopf("Specified a fold id which exceeds the total number of folds.")
  }
  if (any(fold_id > resampling$iters)) {
    stopf("Specified a fold id which exceeds the total number of folds.")
  }

  indicator = NULL

  plot_list = list()
  for (i in fold_id) {
    coords_train = coords[row_id %in% resampling$instance[[i]]$train]
    coords_test = coords[row_id %in% resampling$instance[[i]]$test]

    coords_train$indicator = "Train"
    coords_test$indicator = "Test"

    table = rbind(coords_train, coords_test)

    sf_df = sf::st_as_sf(table, coords = c("x", "y"), crs = task$crs)
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
  if (!grid) {
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

    sf_df = sf::st_as_sf(table, coords = c("x", "y"), crs = task$crs)
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

#' @title Plot for Spatial Resampling
#'
#' @rdname autoplot_spatial_resampling
#' @export
#' @examples
#' #####
#' # SpCVCoords
#' #####
#' library(mlr3)
#' resampling = rsmp("spcv-coords")
#' resampling$instantiate(task)
#' autoplot(resampling, task)
#' autoplot(resampling, task, 1)
#' # autoplot(resampling, task, c(1, 2, 3, 4))
autoplot.ResamplingSpCVCoords = function( # nolint
  object,
  task,
  fold_id = NULL,
  grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  ...) {
  autoplot_spatial(
    resampling = object,
    task = task,
    fold_id = fold_id,
    grid = grid
  )
}

#' @title Plot for Spatio-temporal clustering
#'
#' @param tickformat_date `[character]`\cr
#'   Date format for z-axis.
#' @param crs `[character]`\cr
#'   EPSG code of the CRS for x and y axes.
#' @param nticks_y `[integer]`\cr
#'   Number of y axis breaks.
#' @param nticks_x `[integer]`\cr
#'   Number of x axis breaks.
#' @param point_size `[numeric]`\cr
#'   Point size of markers.
#' @param axis_label_fontsize `[integer]`\cr
#'   Font size of axis labels.
#' @rdname autoplot_spatial_resampling
#' @export
#' @examples
#' #####
#' # SptCVCluto
#' #####
#' if (Sys.info()[["sysname"]] != "Darwin") {
#'   library(mlr3)
#'   library(mlr3spatiotempcv)
#'   task_st = tsk("cookfarm")
#'   resampling = rsmp("spcv-cluto", folds = 5)
#'   resampling$instantiate(task_st, "Date")
#'
#'   # plot
#'   # resampling = readRDS("inst/spt-plotting-data.rda")
#'   autoplot(resampling, task_st)
#'   autoplot(resampling, task_st, 1)
#'   autoplot(resampling, task_st, c(1, 2, 3, 4))
#' }
autoplot.ResamplingSptCVCluto = function( # nolint
  object,
  task,
  fold_id = NULL,
  grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  tickformat_date = "%Y-%m",
  crs = 4326,
  nticks_x = 3,
  nticks_y = 3,
  point_size = 25,
  axis_label_fontsize = 11,
  ...) {

  autoplot_spatiotemp(
    resampling = object,
    task = task,
    fold_id = fold_id,
    grid = grid,
    tickformat_date = tickformat_date,
    crs = crs,
    nticks_y = nticks_y,
    nticks_x = nticks_y,
    point_size = point_size,
    axis_label_fontsize = axis_label_fontsize
  )
}

#' @title Plot for Spatial Resampling
#'
#' @rdname autoplot_spatial_resampling
#' @export
#' @examples
#' #####
#' # Non-Spatial CV
#' #####
#' library(mlr3)
#' resampling = rsmp("cv")
#' resampling$instantiate(task)
#' autoplot(resampling, task)
#' autoplot(resampling, task, 1)
#' # autoplot(resampling, task, c(1, 2, 3, 4))
autoplot.ResamplingCV = function( # nolint
  object,
  task,
  fold_id = NULL,
  grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  ...) {
  autoplot_spatial(
    resampling = object,
    task = task,
    fold_id = fold_id,
    grid = grid
  )
}

#' @title Plot for Spatial Resampling
#'
#' @rdname autoplot_spatial_resampling
#' @export
#' @examples
#' #####
#' # Repeated Non-Spatial CV
#' #####
#' library(mlr3)
#' resampling = rsmp("repeated_cv", folds = 5, repeats = 2)
#' resampling$instantiate(task)
#' autoplot(resampling, task)
#' autoplot(resampling, task, 1)
#' autoplot(resampling, task, fold_id = 2, repeats_id = 2)
#' # autoplot(resampling, task, c(1, 2, 3, 4))
autoplot.ResamplingRepeatedCV = function( # nolint
  object,
  task,
  fold_id = NULL,
  repeats_id = 1,
  grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  ...) {
  autoplot_spatial(
    resampling = object,
    task = task,
    fold_id = fold_id,
    repeats_id = repeats_id,
    grid = grid
  )
}

#' @title Plot for Repeated Spatial Resampling
#'
#' @rdname autoplot_spatial_resampling
#' @export
#' @examples
#' #####
#' # RepeatedSpCVCoords
#' #####
#' library(mlr3)
#' task = tsk("diplodia")
#' resampling = rsmp("repeated-spcv-coords", folds = 10, repeats = 2)
#' resampling$instantiate(task)
#' autoplot(resampling, task)
#' autoplot(resampling, task, 1)
#' autoplot(resampling, task, fold_id = 2, repeats_id = 2)
#' # autoplot(resampling, task, c(1, 2, 3, 4))
autoplot.ResamplingRepeatedSpCVCoords = function( # nolint
  object,
  task,
  fold_id = NULL,
  repeats_id = 1,
  grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  ...) {
  autoplot_spatial(
    resampling = object,
    task = task,
    fold_id = fold_id,
    repeats_id = repeats_id,
    grid = grid
  )
}

#' @title Plot for Repeated Spatial Resampling
#'
#' @rdname autoplot_spatial_resampling
#' @export
#' @examples
#' #####
#' # RepeatedSpCVEnv
#' #####
#' library(mlr3)
#' task = tsk("ecuador")
#' resampling = rsmp("repeated-spcv-env", folds = 10, repeats = 2)
#' resampling$instantiate(task)
#' autoplot(resampling, task)
#' autoplot(resampling, task, 1)
#' autoplot(resampling, task, fold_id = 2, repeats_id = 2)
#' autoplot(resampling, task, c(1, 2, 3, 4))
autoplot.ResamplingRepeatedSpCVEnv = function( # nolint
  object,
  task,
  fold_id = NULL,
  repeats_id = 1,
  grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  ...) {
  autoplot_spatial(
    resampling = object,
    task = task,
    fold_id = fold_id,
    repeats_id = repeats_id,
    grid = grid
  )
}

#' @title Plot for Repeated Spatial Resampling
#'
#' @rdname autoplot_spatial_resampling
#' @export
#' @examples
#' #####
#' # RepeatedSpCVBlock
#' #####
#' library(mlr3)
#' task = tsk("ecuador")
#' resampling = rsmp("repeated-spcv-block",
#'   folds = 5, repeats = 2,
#'   range = c(1000, 1500)
#' )
#' resampling$instantiate(task)
#' autoplot(resampling, task)
#' autoplot(resampling, task, 1)
#' autoplot(resampling, task, fold_id = 2, repeats_id = 2)
#' autoplot(resampling, task, c(1, 2, 3, 4))
autoplot.ResamplingRepeatedSpCVBlock = function( # nolint
  object,
  task,
  fold_id = NULL,
  repeats_id = 1,
  grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  ...) {
  autoplot_spatial(
    resampling = object,
    task = task,
    fold_id = fold_id,
    repeats_id = repeats_id,
    grid = grid
  )
}



#' @title Plot for Spatio-temporal clustering
#'
#' @param tickformat_date `[character]`\cr
#'   Date format for z-axis.
#' @param crs `[character]`\cr
#'   EPSG code of the CRS for x and y axes.
#' @param nticks_y `[integer]`\cr
#'   Number of y axis breaks.
#' @param nticks_x `[integer]`\cr
#'   Number of x axis breaks.
#' @param point_size `[numeric]`\cr
#'   Point size of markers.
#' @param axis_label_fontsize `[integer]`\cr
#'   Font size of axis labels.
#' @rdname autoplot_spatial_resampling
#' @export
#' @examples
#' #####
#' # RepeatedSptCVCluto
#' #####
#' if (Sys.info()[["sysname"]] != "Darwin") {
#'   library(mlr3)
#'   library(mlr3spatiotempcv)
#'   task_st = tsk("cookfarm")
#'   resampling = rsmp("repeated-spcv-cluto", folds = 5, repeats = 2)
#'   resampling$instantiate(task_st, "Date")
#'
#'   # plot
#'   # resampling = readRDS("inst/spt-plotting-data.rda")
#'   autoplot(resampling, task_st)
#'   autoplot(resampling, task_st, 1, repeats_id = 2)
#'   autoplot(resampling, task_st, c(1, 2, 3, 4))
#' }
autoplot.ResamplingRepeatedSptCVCluto = function( # nolint
  object,
  task,
  fold_id = NULL,
  repeats_id = 1,
  grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  tickformat_date = "%Y-%m",
  crs = 4326,
  nticks_x = 3,
  nticks_y = 3,
  point_size = 25,
  axis_label_fontsize = 11,
  ...) {

  autoplot_spatiotemp(
    resampling = object,
    task = task,
    fold_id = fold_id,
    repeats_id = repeats_id,
    grid = grid,
    tickformat_date = tickformat_date,
    crs = crs,
    nticks_y = nticks_y,
    nticks_x = nticks_y,
    point_size = point_size,
    axis_label_fontsize = axis_label_fontsize
  )
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

  # instantiate if not yet done
  if (!resampling$is_instantiated) {
    resampling = resampling$instantiate(task)
  }

  coords_resamp = merge(coords, resampling$instance, by = "row_id")

  if (grepl("Repeated", class(resampling)[1])) {
    coords_resamp = coords_resamp[rep == repeats_id, ]
  }

  # plot train and test of a specific fold?
  if (!is.null(fold_id)) {
    if (length(fold_id) > resampling$iters) {
      stopf("More folds specified than stored in resampling.")
    }
    if (length(fold_id) == 1 && fold_id > resampling$iters) {
      stopf("Specified a fold id which exceeds the total number of folds.")
    }
    if (any(fold_id > resampling$iters)) {
      stopf("Specified a fold id which exceeds the total number of folds.")
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
        scale_color_manual(values = c(
          "Train" = "#0072B5",
          "Test" = "#E18727"
        )) +
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
      plots = do.call(cowplot::plot_grid, list(
        plotlist = plot_list,
        labels = sprintf(
          "Fold %s\nRep %s", fold_id,
          repeats_id
        )
      ))

      # Extract legend standalone, we only want one legend in the grid
      coords_resamp[, indicator := ifelse(fold == 1, "Test", "Train")]

      sf_df = sf::st_as_sf(coords_resamp, coords = c("x", "y"), crs = task$crs)
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
  } else {
    # Create one plot with all (test)-folds
    sf_df = sf::st_as_sf(coords_resamp, coords = c("x", "y"), crs = task$crs)

    # order fold ids
    sf_df = sf_df[order(sf_df$fold, decreasing = FALSE), ]
    sf_df$fold = as.factor(as.character(sf_df$fold))
    sf_df$fold = factor(sf_df$fold, levels = unique(as.character(sf_df$fold)))

    # for all non-repeated rsmp cases
    if (is.null(repeats_id)) {
      repeats_id = 1
    }
    ggplot() +
      geom_sf(
        data = sf_df, show.legend = "point",
        aes(color = fold)
      ) +
      ggsci::scale_color_ucscgb() +
      labs(color = sprintf("Partition #, Rep %s", repeats_id))
  }
}

autoplot_spatiotemp = function(
  resampling = NULL,
  task = NULL,
  fold_id = NULL,
  repeats_id = NULL,
  grid = NULL,
  train_color = NULL,
  test_color = NULL,
  tickformat_date = NULL,
  crs = NULL,
  nticks_x = NULL,
  nticks_y = NULL,
  point_size = NULL,
  axis_label_fontsize = NULL) {

  coords = task$coordinates()
  # add row_id for upcoming merge
  coords$row_id = task$row_ids
  data = task$data()
  data$row_id = task$row_ids
  require_namespaces(c("plotly"))

  # instantiate if not yet done
  if (!resampling$is_instantiated) {
    resampling = resampling$instantiate(task)
  }

  coords_resamp = merge(coords, resampling$instance, by = "row_id")
  task_resamp_ids = merge(data, coords_resamp, by = "row_id")

  if (!is.null(crs)) {
    require_namespaces(c("sf"))
    # transform coordinates to WGS84
    coords = sf::st_coordinates(
      sf::st_transform(
        sf::st_as_sf(task$coordinates(), coords = c("x", "y"), crs = task$crs),
        crs = 4326)
    )
    task_resamp_ids$x = coords[, 1]
    task_resamp_ids$y = coords[, 2]
  }

  if (grepl("Repeated", class(resampling)[1])) {
    task_resamp_ids = task_resamp_ids[rep == repeats_id, ]
  }

  # Create one plot with all (test)-folds
  task_resamp_ids = task_resamp_ids[order(task_resamp_ids$fold, decreasing = FALSE), ]
  task_resamp_ids$fold = as.factor(as.character(task_resamp_ids$fold))
  task_resamp_ids$fold = factor(task_resamp_ids$fold,
    levels = unique(as.character(task_resamp_ids$fold))
  )

  # for all non-repeated rsmp cases
  if (is.null(repeats_id)) {
    repeats_id = 1
  }

  # plot train and test of a specific fold?
  if (!is.null(fold_id)) {
    if (length(fold_id) > resampling$iters) {
      stopf("More folds specified than stored in resampling.")
    }
    if (length(fold_id) == 1 && fold_id > resampling$iters) {
      stopf("Specified a fold id which exceeds the total number of folds.")
    }
    if (any(fold_id > resampling$iters)) {
      stopf("Specified a fold id which exceeds the total number of folds.")
    }

    # suppress undefined global variables note
    indicator = NULL
    fold = NULL

    task_resamp_ids$indicator = as.factor(as.character(task_resamp_ids$indicator))

    # reorder factor levels so that "train" comes first
    task_resamp_ids$indicator = ordered(task_resamp_ids$indicator,
      levels = c("Train", "Test")
    )

    if (length(fold_id) == 1) {
      # suppress undefined global variables note
      indicator = NULL
      fold = NULL

      task_resamp_ids$indicator = as.factor(as.character(task_resamp_ids$indicator))
      task_resamp_ids[, indicator := ifelse(fold == fold_id, "Test", "Train")]
      plot_single_plotly = plotly::plot_ly(task_resamp_ids,
        x = ~x, y = ~y, z = ~Date,
        color = ~indicator, colors = c(
          "#0072B5", "#E18727"
        ),
        sizes = c(20, 100)
      )

      plot_single_plotly = plotly::add_markers(plot_single_plotly,
        marker = list(size = point_size))
      plot_single_plotly = plotly::layout(plot_single_plotly,
        title = sprintf("Partition #, Rep %s", repeats_id),
        autosize = TRUE,
        scene = list(
          xaxis = list(title = "Lat", nticks = nticks_x),
          yaxis = list(title = "Lon", nticks = nticks_y),
          zaxis = list(
            title = "Time",
            type = "date",
            tickformat = tickformat_date,
            tickfont = list(size = axis_label_fontsize)
          ),
          camera = list(eye = list(z = 1.50))
        )
      )
      print(plot_single_plotly)
      return(invisible(plot_single_plotly))
    } else {
      # Multiplot across multiple folds with train and test set
      plot_list = list()
      for (i in fold_id) {

        # suppress undefined global variables note
        indicator = NULL
        fold = NULL

        task_resamp_ids$indicator = as.factor(as.character(task_resamp_ids$indicator))
        task_resamp_ids[, indicator := ifelse(fold == i, "Test", "Train")]

        task_resamp_ids$Date = as.Date(task_resamp_ids$Date)

        # create plots for each fold
        plot_list[[length(plot_list) + 1]] = {
          pl = plotly::plot_ly(task_resamp_ids,
            x = ~x, y = ~y, z = ~Date,
            color = ~indicator, colors = c(
              "#0072B5", "#E18727"
            ),
            # this is needed for later when doing 3D subplots
            scene = paste0("scene", i),
            showlegend = ifelse(i == 1, TRUE, FALSE)
          )
          pl = plotly::add_markers(pl, marker = list(size = point_size))
          layout_args = list(pl,
            "title" = sprintf("Fold #%s", i),
            list(
              xaxis = list(
                title = "Lat",
                nticks = nticks_x,
                tickfont = list(size = axis_label_fontsize)),
              yaxis = list(
                title = "Lon",
                nticks = nticks_y,
                tickfont = list(size = axis_label_fontsize)),
              zaxis = list(
                title = "Time",
                type = "date",
                tickformat = tickformat_date,
                tickfont = list(size = axis_label_fontsize)
                # sets size of axis titles
                # titlefont = list(size = 5)
              ),
              camera = list(eye = list(z = 1.50))
            )
          )
          # -`p` is the name of the plotly object.
          # - title sets the title of the plot
          # - the "scene" name is dynamically generated and refers to the scene
          #   name in the `plot_ly()` call
          names(layout_args) = c(
            "p",
            "title",
            paste0("scene", i)
          )

          pl = mlr3misc::invoke(plotly::layout, .args = layout_args)
        }
      }
    }

    # is a grid requested?
    if (!grid) {
      return(plot_list)
    } else {
      cli::cli_alert_info("Unfortunately plotly does not support a dynamic
       arrangement of multiple subplots.
       See article 'Visualization of spatiotemporal clusters'
       ({.url https://mlr3spatiotempcv.mlr-org.com/articles/spatiotemp-viz}) for a
       manual workaround.
       Use the objects in the returned list to arrange your custom grid.",
        wrap = TRUE)

      return(invisible(plot_list))
    }
  } else {
    pl = plotly::plot_ly(task_resamp_ids,
      x = ~x, y = ~y, z = ~Date,
      color = ~fold, colors = ggsci::pal_ucscgb("default")(26),
      sizes = c(20, 100)
    )
    pl = plotly::add_markers(pl, marker = list(size = point_size))
    plotly::layout(pl,
      title = sprintf("Partition #, Rep %s", repeats_id),
      margin = list(
        l = 10,
        r = 10,
        t = 40,
        b = 10
      ),
      scene = list(
        xaxis = list(
          title = "Lat",
          nticks = nticks_x),
        yaxis = list(
          title = "Lon",
          nticks = nticks_y),
        zaxis = list(
          title = "Time",
          type = "date",
          tickformat = tickformat_date,
          tickfont = list(size = axis_label_fontsize)
        ),
        legend = list(
          margin = list(
            l = 5
          )
        ),
        camera = list(eye = list(z = 1.50))
      )
    )
  }
}
