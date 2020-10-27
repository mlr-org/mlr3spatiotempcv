#' @title Visualization Functions for SptCV Cstf Methods.
#'
#' @description Generic S3 `plot()` and `autoplot()` (ggplot2) methods.
#'
#' @name autoplot.ResamplingSptCVCstf
#' @inheritParams autoplot.ResamplingSpCVBlock
#'
#' @param object `[Resampling]`\cr
#'   mlr3 spatial resampling object of class [ResamplingSptCVCstf] or
#'   [ResamplingRepeatedSptCVCstf].
#' @param x `[Resampling]`\cr
#'   mlr3 spatial resampling object of class [ResamplingSptCVCstf] or
#'   [ResamplingRepeatedSptCVCstf].
#' @param tickformat_date `[character]`\cr
#'   Date format for z-axis.
#' @param crs `[character]`\cr
#'   EPSG code of the CRS for x and y axes.
#'   If not set, EPSG 4326 (WGS84) is used.
#' @param nticks_y `[integer]`\cr
#'   Number of y axis breaks.
#' @param nticks_x `[integer]`\cr
#'   Number of x axis breaks.
#' @param point_size `[numeric]`\cr
#'   Point size of markers.
#' @param axis_label_fontsize `[integer]`\cr
#'   Font size of axis labels.
#' @export
#' @examples
#' library(mlr3)
#' library(mlr3spatiotempcv)
#' task_st = tsk("cookfarm")
#' resampling = rsmp("sptcv_cstf", folds = 5, time_var = "Date")
#' resampling$instantiate(task_st)
#'
#' # plot
#' autoplot(resampling, task_st)
#' # autoplot(resampling, task_st, 1)
#' # autoplot(resampling, task_st, c(1, 2))
autoplot.ResamplingSptCVCstf = function( # nolint
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

  dots = list(...)

  resampling = object
  coords = task$coordinates()
  coords$row_id = task$row_ids
  require_namespaces(c("sf", "patchwork", "ggtext"))

  resampling = assert_autoplot(resampling, fold_id, task)

  if (is.null(dots$repeats_id)) {
    repeats_id = 1
  } else {
    repeats_id = dots$repeats_id
  }

  resampling_sub = resampling$clone()

  if (grepl("Repeated", class(resampling)[1])) {
    resampling_sub$instance = resampling_sub$instance[[repeats_id]]
  }

  if (!is.null(fold_id)) {

    if (length(fold_id) == 1) {
      ### only one fold
      data_coords = prepare_autoplot_cstf(task, resampling_sub)

      # suppress undefined global variables note
      indicator = NULL

      row_id = resampling$instance$test[[fold_id]]
      data_coords[, indicator := ifelse(indicator == fold_id, "Test", "Train")]

      # fold ID needs to a factor
      data_coords$fold = as.factor(data_coords$fold)

      plot_single_plotly = plotly::plot_ly(data_coords,
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

      ### Multiplot of multiple partitions with train and test set

      plot = mlr3misc::map(fold_id, function(.x) {

        coords_train = coords[row_id %in% resampling_sub$instance$train[[.x]]]
        coords_test = coords[row_id %in% resampling_sub$instance$test[[.x]]]

        coords_train$indicator = "Train"
        coords_test$indicator = "Test"

        table = rbind(coords_train, coords_test)
        data = task$data()
        data$row_id = task$row_ids
        table_data = merge(data, table, by = "row_id")

        table_data$Date = as.Date(table_data$Date)

        # create plots for each fold
        pl = plotly::plot_ly(table_data,
          x = ~x, y = ~y, z = ~Date,
          color = ~indicator, colors = c(
            "#0072B5", "#E18727"
          ),
          # this is needed for later when doing 3D subplots
          scene = paste0("scene", .x),
          showlegend = ifelse(.x == 1, TRUE, FALSE)
        )
        pl = plotly::add_markers(pl, marker = list(size = point_size))
        layout_args = list(pl,
          "title" = sprintf("Fold #%s", .x),
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
          paste0("scene", .x)
        )

        pl = mlr3misc::invoke(plotly::layout, .args = layout_args)

      })
    }

    # is a grid requested?
    if (!plot_as_grid) {
      return(plot)
    } else {
      messagef("Unfortunately plotly does not support a dynamic
       arrangement of multiple subplots.
       See article 'Visualization of spatiotemporal clusters'
       (https://mlr3spatiotempcv.mlr-org.com/articles/spatiotemp-viz) for a
       manual workaround.
       Use the objects in the returned list to arrange a custom grid.",
        wrap = TRUE)

      return(invisible(plot))
    }
  } else {

    ### all test sets in one plot, each with a different colour
    data_coords = prepare_autoplot_cstf(task, resampling_sub)

    # fold ID needs to a factor
    data_coords$indicator = as.factor(as.character(data_coords$indicator))

    pl = plotly::plot_ly(data_coords,
      x = ~x, y = ~y, z = ~Date,
      color = ~indicator, colors = ggsci::pal_ucscgb("default")(26),
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

#' @rdname autoplot.ResamplingSptCVCstf
#' @export
autoplot.ResamplingRepeatedSptCVCstf = function( # nolint
  object,
  task,
  fold_id = NULL,
  repeats_id = NULL,
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

  autoplot.ResamplingSptCVCstf(
    object = object,
    task = task,
    fold_id = fold_id,
    plot_as_grid = plot_as_grid,
    train_color = train_color,
    test_color = test_color,
    tickformat_date = tickformat_date,
    crs = crs,
    nticks_x = nticks_x,
    nticks_y = nticks_y,
    point_size = point_size,
    axis_label_fontsize = axis_label_fontsize,
    # ellipsis
    repeats_id = repeats_id
  )
}

#' @rdname autoplot.ResamplingSptCVCstf
#' @export
plot.ResamplingSptCVCstf = function(x, ...) {
  print(autoplot(x, ...))
}

#' @rdname autoplot.ResamplingSptCVCstf
#' @export
plot.ResamplingRepeatedSptCVCstf = function(x, ...) {
  print(autoplot(x, ...)) # nocov
}
