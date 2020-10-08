# SpCV Block -------------------------------------------------------------------

#' @title Visualization of spatiotemporal resampling methods.
#'
#' @description Generates plots for [ResamplingSpCVBlock], [ResamplingSpCVEnv],
#'   [ResamplingSpCVBuffer], [ResamplingSpCVCoords], [ResamplingSptCVCstf and
#'   [ResamplingSptCVCluto].
#'
#' @importFrom data.table rbindlist
#' @import ggplot2
#'
#' @param object `[Resampling]`\cr
#'   mlr3 spatial resampling object. One of class [ResamplingSpCVBuffer],
#'   [ResamplingSpCVBlock], [ResamplingSpCVCoords], [ResamplingSpCVEnv].
#' @param task `[TaskClassifST]/[TaskRegrST]`\cr
#'   mlr3 task object.
#' @param fold_id `[numeric]`\cr
#'   Fold IDs to plot.
#' @param repeats_id `[numeric]`\cr
#'   Repetition ID to plot.
#' @param plot_as_grid `[logical(1)]`\cr
#'   Should a gridded plot using `cowplot::plot_grid()` be created? If `FALSE`
#'   only a list with all \CRANpkg{ggplot2} resamplings is returned. Default is
#'   `TRUE`. Only applies if a numeric vector is passed to argument `fold_id`.
#' @param train_color `[character(1)]`\cr
#'   The color to use for the training set observations.
#' @param test_color `[character(1)]`\cr
#'   The color to use for the test set observations.
#' @param ... Not used.
#'
#' @details
#' By default a plot is returned; if `fold_id` is set, a gridded plot is
#' created. If `plot_as_grid = FALSE`, a list of plot objects is returned.
#' This can be used to align the plots individually.
#'
#' When no single fold is selected, the [ggsci::scale_color_ucscgb()] palette
#' is used to display all partitions.
#' If you want to change the colors, call `<plot> + <color-palette>()`.
#' @return [ggplot()] or list of ggplot2 objects.
#' @name autoplot_spatial_resampling
#' @export
#' @examples
#' ##########
#' # SpCVBlock
#' ##########
#' \dontrun{
#' library(mlr3)
#' task = tsk("ecuador")
#' resampling = rsmp("spcv_block", range = 1000)
#' resampling$instantiate(task)
#'
#' ## Visualize all partitions
#' autoplot(resampling, task)
#'
#' ## Visualize the train/test split of a single fold
#' autoplot(resampling, task, fold_id = 1)
#'
#' ## Visualize train/test splits of multiple folds
#' autoplot(resampling, task, fold_id = c(1, 2))
#'
#' # list of ggplot2 resamplings
#' plot_list = autoplot(resampling, task,
#'   fold_id = c(1, 2), grid = FALSE)
#' }
autoplot.ResamplingSpCVBlock = function( # nolint
  object,
  task,
  fold_id = NULL,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  crs = NULL,
  ...) {

  ellip = list(...)

  autoplot_spatial(
    resampling = object,
    task = task,
    fold_id = fold_id,
    repeats_id = ellip$repeats_id,
    plot_as_grid = plot_as_grid,
    train_color = train_color,
    test_color = test_color,
    crs = crs
  )
}

#' @rdname autoplot_spatial_resampling
#' @export
autoplot.ResamplingRepeatedSpCVBlock = function( # nolint
  object,
  task,
  fold_id = NULL,
  repeats_id = NULL,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  crs = NULL,
  ...) {

  autoplot.ResamplingSpCVBlock(
    object = object,
    task = task,
    fold_id = fold_id,
    plot_as_grid = plot_as_grid,
    train_color = train_color,
    test_color = test_color,
    crs = crs,
    # ellipsis
    repeats_id = repeats_id
  )
}

#' @importFrom graphics plot
#' @param x `[Resampling]`\cr
#'   mlr3 spatial resampling object. One of class [ResamplingSpCVBuffer],
#'   [ResamplingSpCVBlock], [ResamplingSpCVCoords], [ResamplingSpCVEnv].
#' @rdname autoplot_spatial_resampling
#' @export
plot.ResamplingSpCVBlock = function(x, ...) {
  print(autoplot(x, ...))
}

#' @rdname autoplot_spatial_resampling
#' @export
plot.ResamplingRepeatedSpCVBlock = function(x, ...) {
  print(autoplot(x, ...))
}

# SpCV Env ---------------------------------------------------------------------

#' @title Plot for Spatial Resampling
#'
#' @rdname autoplot_spatial_resampling
#' @export
#' @examples
#' ##########
#' # SpCVEnv
#' ##########
#' \dontrun{
#' library(mlr3)
#' resampling = rsmp("spcv_env", folds = 4, features = "dem")
#' resampling$instantiate(task)
#'
#' autoplot(resampling, task)
#' autoplot(resampling, task, 1)
#' autoplot(resampling, task, c(1, 2))
#' }
autoplot.ResamplingSpCVEnv = function( # nolint
  object,
  task,
  fold_id = NULL,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  crs = NULL,
  ...) {

  ellip = list(...)

  autoplot_spatial(
    resampling = object,
    task = task,
    fold_id = fold_id,
    repeats_id = ellip$repeats_id,
    plot_as_grid = plot_as_grid,
    crs = crs
  )
}

#' @rdname autoplot_spatial_resampling
#' @export
autoplot.ResamplingRepeatedSpCVEnv = function( # nolint
  object,
  task,
  fold_id = NULL,
  repeats_id = NULL,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  crs = NULL,
  ...) {

  autoplot.ResamplingSpCVEnv(
    object = object,
    task = task,
    fold_id = fold_id,
    plot_as_grid = plot_as_grid,
    train_color = train_color,
    test_color = test_color,
    crs = crs,
    # ellipsis
    repeats_id = repeats_id
  )
}

#' @importFrom graphics plot
#' @rdname autoplot_spatial_resampling
#' @export
plot.ResamplingSpCVEnv = function(x, ...) {
  print(autoplot(x, ...))
}

#' @rdname autoplot_spatial_resampling
#' @export
plot.ResamplingRepeatedSpCVEnv = function(x, ...) {
  print(autoplot(x, ...))
}

# SpCV Coords-------------------------------------------------------------------

#' @title Plot for Spatial Resampling
#'
#' @rdname autoplot_spatial_resampling
#' @export
#' @examples
#' ##########
#' # SpCVCoords
#' ##########
#' \dontrun{
#' library(mlr3)
#' task = tsk("ecuador")
#' resampling = rsmp("spcv_coords")
#' resampling$instantiate(task)
#'
#' autoplot(resampling, task)
#' autoplot(resampling, task, 1)
#' autoplot(resampling, task, c(1, 2))
#' }
autoplot.ResamplingSpCVCoords = function( # nolint
  object,
  task,
  fold_id = NULL,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  crs = NULL,
  ...) {

  ellip = list(...)

  autoplot_spatial(
    resampling = object,
    task = task,
    fold_id = fold_id,
    repeats_id = ellip$repeats_id,
    plot_as_grid = plot_as_grid,
    crs = crs
  )
}

#' @rdname autoplot_spatial_resampling
#' @export
autoplot.ResamplingRepeatedSpCVCoords = function( # nolint
  object,
  task,
  fold_id = NULL,
  repeats_id = NULL,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  crs = NULL,
  ...) {

  autoplot.ResamplingSpCVCoords(
    object = object,
    task = task,
    fold_id = fold_id,
    plot_as_grid = plot_as_grid,
    train_color = train_color,
    test_color = test_color,
    crs = crs,
    # ellipsis
    repeats_id = repeats_id
  )
}

#' @importFrom graphics plot
#' @rdname autoplot_spatial_resampling
#' @export
plot.ResamplingSpCVCoords = function(x, ...) {
  print(autoplot(x, ...))
}

#' @rdname autoplot_spatial_resampling
#' @export
plot.ResamplingRepeatedSpCVCoords = function(x, ...) {
  print(autoplot(x, ...))
}

# SptCV Cluto ------------------------------------------------------------------

#' @title Plot for spatio-temporal clustering
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
#' ##########
#' # SptCVCluto
#' ##########
#' \dontrun{
#' library(mlr3)
#' library(mlr3spatiotempcv)
#' task_st = tsk("cookfarm")
#' resampling = rsmp("sptcv_cluto", folds = 5)
#' resampling$instantiate(task_st, "Date")
#'
#' # plot
#' autoplot(resampling, task_st)
#' autoplot(resampling, task_st, 1)
#' autoplot(resampling, task_st, c(1, 2))
#' }
autoplot.ResamplingSptCVCluto = function( # nolint
  object,
  task,
  fold_id = NULL,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  tickformat_date = "%Y-%m",
  crs = NULL,
  nticks_x = 3,
  nticks_y = 3,
  point_size = 3,
  axis_label_fontsize = 11,
  ...) {

  ellip = list(...)

  autoplot_spatiotemp(
    resampling = object,
    task = task,
    fold_id = fold_id,
    repeats_id = ellip$repeats_id,
    plot_as_grid = plot_as_grid,
    tickformat_date = tickformat_date,
    crs = crs,
    nticks_y = nticks_y,
    nticks_x = nticks_y,
    point_size = point_size,
    axis_label_fontsize = axis_label_fontsize
  )
}

#' @rdname autoplot_spatial_resampling
#' @export
autoplot.ResamplingRepeatedSptCVCluto = function( # nolint
  object,
  task,
  fold_id = NULL,
  repeats_id = NULL,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  crs = NULL,
  ...) {

  autoplot.ResamplingSptCVCluto(
    object = object,
    task = task,
    fold_id = fold_id,
    plot_as_grid = plot_as_grid,
    train_color = train_color,
    test_color = test_color,
    crs = crs,
    # ellipsis
    repeats_id = repeats_id
  )
}

#' @importFrom graphics plot
#' @rdname autoplot_spatial_resampling
#' @export
plot.ResamplingSptCVCluto = function(x, ...) {
  print(autoplot(x, ...))
}

#' @rdname autoplot_spatial_resampling
#' @export
plot.ResamplingRepeatedSptCVCluto = function(x, ...) {
  print(autoplot(x, ...))
}

# CV ---------------------------------------------------------------------------

#' @title Plot for Spatial Resampling
#'
#' @rdname autoplot_spatial_resampling
#' @export
#' @examples
#' ##########
#' # Non-Spatial CV
#' ##########
#' \dontrun{
#' library(mlr3)
#' resampling = rsmp("cv")
#' resampling$instantiate(task)
#'
#' autoplot(resampling, task)
#' autoplot(resampling, task, 1)
#' autoplot(resampling, task, c(1, 2))
#' }
autoplot.ResamplingCV = function( # nolint
  object,
  task,
  fold_id = NULL,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  crs = crs,
  ...) {

  ellip = list(...)

  autoplot_spatial(
    resampling = object,
    task = task,
    fold_id = fold_id,
    plot_as_grid = plot_as_grid,
    crs = crs,
    repeats_id = ellip$repeats_id
  )
}

#' @rdname autoplot_spatial_resampling
#' @export
autoplot.ResamplingRepeatedCV = function( # nolint
  object,
  task,
  fold_id = NULL,
  repeats_id = NULL,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  crs = NULL,
  ...) {

  autoplot.ResamplingCV(
    object = object,
    task = task,
    fold_id = fold_id,
    plot_as_grid = plot_as_grid,
    train_color = train_color,
    test_color = test_color,
    crs = crs,
    # ellipsis
    repeats_id = repeats_id
  )
}

#' @importFrom graphics plot
#' @rdname autoplot_spatial_resampling
#' @export
plot.ResamplingCV = function(x, ...) {
  print(autoplot(x, ...))
}

#' @rdname autoplot_spatial_resampling
#' @export
plot.ResamplingRepeatedCV = function(x, ...) {
  print(autoplot(x, ...))
}

# autoplot_spatial -------------------------------------------------------------

autoplot_spatial = function(
  resampling = NULL,
  task = NULL,
  fold_id = NULL,
  repeats_id = NULL,
  plot_as_grid = NULL,
  train_color = NULL,
  test_color = NULL,
  crs = NULL,
  ...) {

  require_namespaces(c("sf", "patchwork", "ggtext"))

  resampling = assert_autoplot(resampling, fold_id, task)

  if (is.null(repeats_id)) {
    repeats_id = 1
  }

  # add the row_ids of the task to the coordinates
  coords = task$coordinates()
  coords$row_id = task$row_ids

  coords_resamp = merge(coords, resampling$instance, by = "row_id")

  if (grepl("Repeated", class(resampling)[1])) {
    coords_resamp = coords_resamp[rep == repeats_id, ]
  }

  if (!is.null(fold_id)) {

    # Multiplot with train and test set for each fold --------------------------

    plot_list = mlr3misc::map(fold_id, function(.x) {

      dt = coords_resamp
      dt$indicator = rep("foo", nrow(dt))
      dt[, indicator := ifelse(fold == .x, "Test", "Train")]

      # set fallback crs if missing
      if (is.null(crs)) {
        # use 4326 (WGS84) as fallback
        crs = 4326
        cli::cli_alert_info("CRS not set, transforming to WGS84 (EPSG: 4326).")
      }
      # transform to selected crs
      sf_df = sf::st_transform(
        sf::st_as_sf(dt, coords = c("x", "y"), crs = task$extra_args$crs),
        crs = crs)

      sf_df = reorder_levels(sf_df)

      ggplot() +
        geom_sf(data = sf_df, aes(color = indicator)) +
        scale_color_manual(values = c(
          "Train" = "#0072B5",
          "Test" = "#E18727"
        )) +
        labs(color = "Set", title = sprintf("Fold %s, Repetition %s", .x, repeats_id)) +
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

    # Return a plot grid via patchwork? ----------------------------------------

    if (!plot_as_grid) {
      return(plot_list)
    } else {
      # for repeated cv we also print out the rep number
      if (is.null(repeats_id)) {
        repeats_id = 1
      }

      plot_list_pw = patchwork::wrap_plots(plot_list) +
        patchwork::plot_layout(guides = "collect")
      return(plot_list_pw)
    }
  } else {

    # Create one plot colored by all test folds --------------------------------

    # set fallback crs if missing
    if (is.null(crs)) {
      # use 4326 (WGS84) as fallback
      crs = 4326
      cli::cli_alert_info("CRS not set, transforming to WGS84 (EPSG: 4326).")
    }
    # transform to selected crs
    sf_df = sf::st_transform(
      sf::st_as_sf(coords_resamp, coords = c("x", "y"), crs = task$extra_args$crs),
      crs = crs)

    # order fold ids
    sf_df = sf_df[order(sf_df$fold, decreasing = FALSE), ]
    sf_df$fold = as.factor(as.character(sf_df$fold))
    sf_df$fold = factor(sf_df$fold, levels = unique(as.character(sf_df$fold)))

    # for all non-repeated rsmp cases
    if (is.null(repeats_id)) {
      repeats_id = 1
    }
    plot = ggplot() +
      geom_sf(
        data = sf_df, show.legend = "point",
        aes(color = fold)
      ) +
      ggsci::scale_color_ucscgb() +
      labs(color = sprintf("Partition #, Rep %s", repeats_id))
    return(plot)
  }
}

# autoplot_spatiotemp ----------------------------------------------------------

autoplot_spatiotemp = function(
  resampling = NULL,
  task = NULL,
  fold_id = NULL,
  repeats_id = NULL,
  plot_as_grid = NULL,
  train_color = NULL,
  test_color = NULL,
  tickformat_date = NULL,
  crs = NULL,
  nticks_x = NULL,
  nticks_y = NULL,
  point_size = NULL,
  axis_label_fontsize = NULL) {

  require_namespaces("plotly")
  resampling = assert_autoplot(resampling, fold_id, task)

  # add the row_ids of the task to the coordinates
  coords = task$coordinates()
  coords$row_id = task$row_ids

  data = task$data()
  data$row_id = task$row_ids

  coords_resamp = merge(coords, resampling$instance, by = "row_id")
  task_resamp_ids = merge(data, coords_resamp, by = "row_id")

  if (is.null(repeats_id)) {
    repeats_id = 1
  }

  # set some required defaults
  if (grepl("Repeated", class(resampling)[1])) {
    task_resamp_ids = task_resamp_ids[rep == repeats_id, ]
  }

  # set fallback crs if missing
  if (is.null(crs)) {
    require_namespaces("sf")
    cli::cli_alert_info("CRS not set, transforming to WGS84 (EPSG: 4326).")

    crs = 4326
  }
  # transform coordinates to selected crs
  coords = sf::st_coordinates(
    sf::st_transform(
      sf::st_as_sf(task$coordinates(), coords = c("x", "y"), crs = task$extra_args$crs),
      crs = crs)
  )
  task_resamp_ids$x = coords[, 1]
  task_resamp_ids$y = coords[, 2]

  # Create one plot with all (test)-folds
  task_resamp_ids = task_resamp_ids[order(task_resamp_ids$fold, decreasing = FALSE), ]
  task_resamp_ids$fold = as.factor(as.character(task_resamp_ids$fold))
  task_resamp_ids$fold = factor(task_resamp_ids$fold,
    levels = unique(as.character(task_resamp_ids$fold))
  )

  # plot train and test of a specific fold?
  if (!is.null(fold_id)) {

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


      task_resamp_ids$indicator = as.factor(as.character(task_resamp_ids$indicator))
      plot_list = mlr3misc::map(fold_id, function(.x) {

        dt = task_resamp_ids
        dt[, indicator := ifelse(fold == .x, "Test", "Train")]

        dt$Date = as.Date(dt$Date)

        pl = plotly::plot_ly(task_resamp_ids,
          x = ~x, y = ~y, z = ~Date,
          color = ~indicator, colors = c(
            "#0072B5", "#E18727"
          ),
          # this is needed for later when doing 3D subplots
          scene = paste0("scene", i),
          showlegend = ifelse(i == 1, TRUE, FALSE)
        )

        # create plots for each fold
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

      })

      return(plot_list)

      plot = list()
      for (i in fold_id) {

        # suppress undefined global variables note
        indicator = NULL
        fold = NULL

        task_resamp_ids[, indicator := ifelse(fold == i, "Test", "Train")]

        task_resamp_ids$Date = as.Date(task_resamp_ids$Date)

        # create plots for each fold
        plot[[length(plot) + 1]] = {
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
    if (!plot_as_grid) {
      return(plot)
    } else {
      cli::cli_alert_info("Unfortunately plotly does not support a dynamic
       arrangement of multiple subplots.
       See article 'Visualization of spatiotemporal clusters'
       ({.url https://mlr3spatiotempcv.mlr-org.com/articles/spatiotemp-viz}) for a
       manual workaround.
       Use the objects in the returned list to arrange your custom grid.",
        wrap = TRUE)

      return(invisible(plot))
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
  return(pl)
}
