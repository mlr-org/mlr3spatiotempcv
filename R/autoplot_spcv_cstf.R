#' @title Visualization Functions for SptCV Cstf Methods.
#'
#' @description Generic S3 `plot()` and `autoplot()` (ggplot2) methods to
#'   visualize mlr3 spatiotemporal resampling objects.
#'
#' @details
#' This method requires to set argument `fold_id`.
#' No plot showing all folds in one plot can be created.
#' This is because the LLTO method does not make use of all observations but only
#' a subset of them (many observations are omitted).
#' Hence, train and test sets of one fold are not re-used in other folds as in
#' other methods and plotting these without a train/test indicator would be
#' misleading.
#'
#' @section 2D vs 3D plotting:
#' This method has both a 2D and a 3D plotting method.
#' The 2D method returns a \pkg{ggplot} with x and y axes representing the spatial
#' coordinates.
#' The 3D method uses \pkg{plotly} to create an interactive 3D plot.
#' Set `plot3D = TRUE` to use the 3D method.
#'
#' Note that spatiotemporal datasets usually suffer from overplotting in 2D
#' mode.
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
#' @param nticks_y `[integer]`\cr
#'   Number of y axis breaks.
#' @param nticks_x `[integer]`\cr
#'   Number of x axis breaks.
#' @param point_size `[numeric]`\cr
#'   Point size of markers.
#' @param axis_label_fontsize `[integer]`\cr
#'   Font size of axis labels.
#' @param static_image `[logical]`\cr
#'   Whether to create a static image from the plotly plot via `plotly::orca()`.
#'   This requires the `orca` utility to be available.
#'   See [https://github.com/plotly/orca](https://github.com/plotly/orca) for
#'   more information.
#'   When used, by default a file named `plot.png` is created in the current
#'   working directory.
#' @param show_omitted `[logical]`\cr
#'   Whether to show points not used in train or test set for the current fold.
#' @param plot3D `[logical]`\cr
#'   Whether to create a 2D image via \pkg{ggplot2} or a 3D plot via
#'   \pkg{plotly}.
#' @param plot_time_var `[character]`\cr
#'   The variable to use for the z-axis (time).
#'   Remove the column role `feature` for this variable to only use
#'   it for plotting.
#' @param ... Passed down to `plotly::orca()`. Only effective when
#' `static_image = TRUE`.
#' @export
#' @seealso
#'   - mlr3book chapter on ["Spatiotemporal Visualization"](https://mlr3book.mlr-org.com/chapters/chapter13/beyond_regression_and_classification.html#spatiotemp-cv)
#'   - Vignette [Spatiotemporal Visualization](https://mlr3spatiotempcv.mlr-org.com/articles/spatiotemp-viz.html).
#'   - [autoplot.ResamplingSpCVBlock()]
#'   - [autoplot.ResamplingSpCVBuffer()]
#'   - [autoplot.ResamplingSpCVCoords()]
#'   - [autoplot.ResamplingSpCVEnv()]
#'   - [autoplot.ResamplingCV()]
#' @examples
#' \donttest{
#' if (mlr3misc::require_namespaces(c("sf", "plotly"), quietly = TRUE)) {
#'   library(mlr3)
#'   library(mlr3spatiotempcv)
#'   task_st = tsk("cookfarm_mlr3")
#'   task_st$set_col_roles("SOURCEID", "space")
#'   task_st$set_col_roles("Date", "time")
#'   resampling = rsmp("sptcv_cstf", folds = 5)
#'   resampling$instantiate(task_st)
#'
#'   # with both `"space"` and `"time"` column roles set (LLTO), the omitted
#'   # observations per fold can be shown by setting `show_omitted = TRUE`
#'   autoplot(resampling, task_st, fold_id = 1, show_omitted = TRUE)
#' }
#' }
autoplot.ResamplingSptCVCstf = function( # nolint
  object,
  task,
  fold_id = NULL,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  repeats_id = NULL,
  tickformat_date = "%Y-%m",
  nticks_x = 3,
  nticks_y = 3,
  point_size = 3,
  axis_label_fontsize = 11,
  static_image = FALSE,
  show_omitted = FALSE,
  plot3D = NULL,
  plot_time_var = NULL,
  sample_fold_n = NULL,
  ...) {

  dots = list(...)

  resampling = object
  coords = task$coordinates()
  coords$row_id = task$row_ids
  mlr3misc::require_namespaces(c("sf", "patchwork", "ggtext"))

  resampling = assert_autoplot(resampling, fold_id, task)

  if (is.null(dots$repeats_id)) {
    repeats_id = 1
  } else {
    repeats_id = dots$repeats_id
  }

  resampling_sub = resampling$clone()

  if (any(grepl("ResamplingRepeated", class(resampling)))) {
    resampling_sub$instance = resampling_sub$instance[[repeats_id]]
  }

  # check if we are in a 2D or 3D scenario
  if (is.null(plot3D)) {
    if (length(task$col_roles$space) &&
      length(task$col_roles$time)) {
      plot3D = TRUE
    } else {
      plot3D = FALSE
    }
  }

  # 2D -------------------------------------------------------------------------

  if (!plot3D) {

    if (!is.null(fold_id)) {
      ### Multiplot of single folds with train and test ----------------------
      plot = autoplot_multi_fold_list(task, resampling_sub, sample_fold_n,
        fold_id, repeats_id, plot_as_grid, show_omitted)
      # }
    } else {
      ### One plot showing all test folds --------------------------------------
      plot = autoplot_all_folds_list(task, resampling_sub, sample_fold_n,
        fold_id, repeats_id)
    }
    return(plot)
  }

  # 3D -------------------------------------------------------------------------

  if (plot3D) {

    if (!is.null(fold_id)) {

      if (length(fold_id) == 1) {
        ### only one fold

        data_coords = format_resampling_list(task, resampling_sub)

        data_coords$indicator = NA_character_

        if (length(task$col_roles$time)) {
          data_coords$Date = as.Date(task$data(cols = task$col_roles$time)[[1]])
        } else {
          # if time col is not set, check for plot_time col role
          if (!is.null(plot_time_var)) {
            data_coords$Date = as.Date(task$data(cols = plot_time_var)[[1]])
          } else {
            lg$error("Neither 'time' or 'plot' column roles are set. At least one is required for 3D plotting. If the variable is only used for plotting purposes, please define argument 'plot_time_var' in `autoplot()` and remove the column role 'feature' for this variable.")
            stop()
          }
        }

        row_id_test = resampling_sub$instance$test[[fold_id]]
        row_id_train = resampling_sub$instance$train[[fold_id]]

        data_coords[list(row_id_train), "indicator" := "Train", on = "row_id"]
        data_coords[list(row_id_test), "indicator" := "Test", on = "row_id"]

        # take stratified random sample from folds
        if (!is.null(sample_fold_n)) {
          data_coords = strat_sample_folds(data_coords, "test", sample_fold_n)
        }

        if (show_omitted && length(is.na(data_coords$indicator)) > 0) {
          data_coords[is.na(get("indicator")), "indicator" := "Omitted"]

          plot_single_plotly = plotly::plot_ly(data_coords,
            x = ~x, y = ~y, z = ~Date,
            color = ~indicator, colors = c(
              "Omitted" = "grey",
              "Test" = "#E18727",
              "Train" = "#0072B5"
            ),
            sizes = c(20, 100)
          )
        } else {
          data_coords = data_coords[!is.na(get("indicator")), , ]

          plot_single_plotly = plotly::plot_ly(data_coords,
            x = ~x, y = ~y, z = ~Date,
            color = ~indicator, colors = c(
              "Test" = "#E18727",
              "Train" = "#0072B5"
            ),
            sizes = c(20, 100)
          )
        }

        plot_single_plotly = plotly::add_markers(plot_single_plotly,
          marker = list(size = point_size))
        plot_single_plotly = plotly::layout(plot_single_plotly,
          title = sprintf(
            "Fold %s, Repetition %s", fold_id,
            repeats_id),
          autosize = TRUE,
          scene = list(
            xaxis = list(title = "Lat", nticks = nticks_x,
              tickfont = list(size = axis_label_fontsize)),
            yaxis = list(title = "Lon", nticks = nticks_y,
              tickfont = list(size = axis_label_fontsize)),
            zaxis = list(
              title = "Time",
              type = "date",
              tickformat = tickformat_date,
              tickfont = list(size = axis_label_fontsize)
            ),
            camera = list(eye = list(z = 1.50))
          )
        )

        if (static_image) {
          plotly::orca(plot_single_plotly, ...)
        }

        return(plot_single_plotly)
      } else {

        ### Multiplot of multiple partitions with train and test set

        plot = mlr3misc::map(fold_id, function(.x) {

          data_coords = format_resampling_list(task, resampling_sub)

          data_coords$indicator = NA_character_

          if (length(task$col_roles$time)) {
            data_coords$Date = as.Date(task$data(cols = task$col_roles$time)[[1]])
          } else {
            # if time col is not set, check for plot_time col role
            if (!is.null(plot_time_var)) {
              data_coords$Date = as.Date(task$data(cols = plot_time_var)[[1]])
            } else {
              lg$error("Neither 'time' or 'plot' column roles are set. At least one is required for 3D plotting. If the variable is only used for plotting purposes, please define argument 'plot_time_var' in `autoplot()` and remove the column role 'feature' for this variable.")
              stop()
            }
          }

          # get test and train indices
          row_id_test = resampling_sub$instance$test[[.x]]
          row_id_train = resampling_sub$instance$train[[.x]]

          # assign test or train to columns matching the respective row ids
          data_coords[row_id %in% row_id_test, indicator := "Test"]
          data_coords[row_id %in% row_id_train, indicator := "Train"]

          data_coords$Date = as.Date(data_coords$Date)

          # take stratified random sample from folds
          if (!is.null(sample_fold_n)) {
            data_coords = strat_sample_folds(data_coords, "test", sample_fold_n)
          }

          if (show_omitted) {
            data_coords[is.na(get("indicator")), "indicator" := "Omitted"]

            pl = plotly::plot_ly(data_coords,
              x = ~x, y = ~y, z = ~Date,
              color = ~indicator, colors = c(
                "Omitted" = "grey",
                "Test" = "#E18727",
                "Train" = "#0072B5"
              ),
              #   # this is needed for later when doing 3D subplots
              scene = paste0("scene", .x),
              showlegend = ifelse(.x == 1, TRUE, FALSE)
            )
          } else {
            data_coords[is.na(get("indicator")), "indicator" := "Omitted"]
            pl = plotly::plot_ly(data_coords,
              x = ~x, y = ~y, z = ~Date,
              color = ~indicator, colors = c(
                "Test" = "#E18727",
                "Train" = "#0072B5"
              ),
              #   # this is needed for later when doing 3D subplots
              scene = paste0("scene", .x),
              showlegend = ifelse(.x == 1, TRUE, FALSE)
              # sizes = c(20, 100)
            )
          }

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

          plot = mlr3misc::invoke(plotly::layout, .args = layout_args)

        })
      }
    }

    else {
      lg$fatal("This method requires to set argument 'fold_id'. See ?autoplot.ResamplingSptCVCstf for more information.") # nolint
      stop()
    }

    # is a grid requested?
    if (!plot_as_grid) {
      if (static_image) {
        plotly::orca(plot, ...)
      }
      return(plot)
    } else {
      lg$warn("Unfortunately plotly does not support a dynamic arrangement of multiple subplots.
       See article 'Visualization of spatiotemporal clusters' (https://mlr3spatiotempcv.mlr-org.com/articles/spatiotemp-viz) for a manual workaround.
       Use the objects in the returned list to arrange a custom grid.")

      if (static_image) {
        plotly::orca(plot, ...)
      }
      return(plot)
    }
  }

}

#' @rdname autoplot.ResamplingSptCVCstf
#' @export
autoplot.ResamplingRepeatedSptCVCstf = function( # nolint
  object,
  task,
  fold_id = NULL,
  repeats_id = 1,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  tickformat_date = "%Y-%m",
  nticks_x = 3,
  nticks_y = 3,
  point_size = 3,
  axis_label_fontsize = 11,
  plot3D = NULL,
  plot_time_var = NULL,
  ...) {

  autoplot.ResamplingSptCVCstf(
    object = object,
    task = task,
    fold_id = fold_id,
    plot_as_grid = plot_as_grid,
    train_color = train_color,
    test_color = test_color,
    tickformat_date = tickformat_date,
    nticks_x = nticks_x,
    nticks_y = nticks_y,
    point_size = point_size,
    axis_label_fontsize = axis_label_fontsize,
    plot3D = plot3D, ,
    plot_time_var = plot_time_var,
    ...,
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
