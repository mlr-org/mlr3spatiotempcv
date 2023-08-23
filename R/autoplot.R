# SpCV Block -------------------------------------------------------------------

#' @title Visualization Functions for SpCV Block Methods.
#'
#' @description Generic S3 `plot()` and `autoplot()` (ggplot2) methods to
#'   visualize mlr3 spatiotemporal resampling objects.
#'
#' @import ggplot2
#'
#' @param object `[Resampling]`\cr
#'   mlr3 spatial resampling object of class [ResamplingSpCVBlock] or
#'   [ResamplingRepeatedSpCVBlock].
#' @param task `[TaskClassifST]/[TaskRegrST]`\cr
#'   mlr3 task object.
#' @param x `[Resampling]`\cr
#'   mlr3 spatial resampling object of class [ResamplingSpCVBlock] or
#'   [ResamplingRepeatedSpCVBlock].
#'   mlr3 task object.
#' @param fold_id `[numeric]`\cr
#'   Fold IDs to plot.
#' @param repeats_id `[numeric]`\cr
#'   Repetition ID to plot.
#' @param plot_as_grid `[logical(1)]`\cr
#'   Should a gridded plot using via \CRANpkg{patchwork} be created? If `FALSE`
#'   a list with of \CRANpkg{ggplot2} objects is returned.
#'   Only applies if a numeric vector is passed to argument `fold_id`.
#' @param train_color `[character(1)]`\cr
#'   The color to use for the training set observations.
#' @param test_color `[character(1)]`\cr
#'   The color to use for the test set observations.
#' @param show_blocks `[logical(1)]`\cr
#'   Whether to show an overlay of the spatial blocks polygons.
#' @param show_labels `[logical(1)]`\cr
#'   Whether to show an overlay of the spatial block IDs.
#' @param label_size `[numeric(1)]`\cr
#'   Label size of block labels. Only applies for `show_labels = TRUE`.
#' @param ... Passed to `geom_sf()`. Helpful for adjusting point sizes and
#'   shapes.
#' @param sample_fold_n `[integer]`\cr
#'   Number of points in a random sample stratified over partitions.
#'   This argument aims to keep file sizes of resulting plots reasonable and
#'   reduce overplotting in dense datasets.
#' @details
#' By default a plot is returned; if `fold_id` is set, a gridded plot is
#' created. If `plot_as_grid = FALSE`, a list of plot objects is returned.
#' This can be used to align the plots individually.
#'
#' When no single fold is selected, the [ggsci::scale_color_ucscgb()] palette
#' is used to display all partitions.
#' If you want to change the colors, call `<plot> + <color-palette>()`.
#' @return [ggplot()] or list of ggplot2 objects.
#' @name autoplot.ResamplingSpCVBlock
#' @seealso
#'   - mlr3book chapter on ["Spatiotemporal Visualization"](https://mlr3book.mlr-org.com/special.html#vis-spt-partitions)
#'   - [autoplot.ResamplingSpCVBuffer()]
#'   - [autoplot.ResamplingSpCVCoords()]
#'   - [autoplot.ResamplingSpCVEnv()]
#'   - [autoplot.ResamplingSpCVDisc()]
#'   - [autoplot.ResamplingSpCVTiles()]
#'   - [autoplot.ResamplingCV()]
#'   - [autoplot.ResamplingSptCVCstf()]
#' @export
#' @examples
#' \donttest{
#' if (mlr3misc::require_namespaces(c("sf", "blockCV"), quietly = TRUE)) {
#'   library(mlr3)
#'   library(mlr3spatiotempcv)
#'   task = tsk("ecuador")
#'   resampling = rsmp("spcv_block", range = 1000L)
#'   resampling$instantiate(task)
#'
#'   ## list of ggplot2 resamplings
#'   plot_list = autoplot(resampling, task,
#'     crs = 4326,
#'     fold_id = c(1, 2), plot_as_grid = FALSE)
#'
#'   ## Visualize all partitions
#'   autoplot(resampling, task) +
#'     ggplot2::scale_x_continuous(breaks = seq(-79.085, -79.055, 0.01))
#'
#'   ## Visualize the train/test split of a single fold
#'   autoplot(resampling, task, fold_id = 1) +
#'     ggplot2::scale_x_continuous(breaks = seq(-79.085, -79.055, 0.01))
#'
#'   ## Visualize train/test splits of multiple folds
#'   autoplot(resampling, task,
#'     fold_id = c(1, 2),
#'     show_blocks = TRUE) *
#'     ggplot2::scale_x_continuous(breaks = seq(-79.085, -79.055, 0.01))
#' }
#' }
autoplot.ResamplingSpCVBlock = function( # nolint
  object,
  task,
  fold_id = NULL,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  show_blocks = FALSE,
  show_labels = FALSE,
  sample_fold_n = NULL,
  label_size = 2,
  ...) {

  autoplot_spatial(
    resampling = object,
    task = task,
    fold_id = fold_id,
    plot_as_grid = plot_as_grid,
    train_color = train_color,
    test_color = test_color,
    show_blocks = show_blocks,
    show_labels = show_labels,
    sample_fold_n = sample_fold_n,
    label_size = label_size,
    ... = ...
  )
}

#' @rdname autoplot.ResamplingSpCVBlock
#' @export
autoplot.ResamplingRepeatedSpCVBlock = function( # nolint
  object,
  task,
  fold_id = NULL,
  repeats_id = 1,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  show_blocks = FALSE,
  show_labels = FALSE,
  sample_fold_n = NULL,
  label_size = 2,
  ...) {

  autoplot.ResamplingSpCVBlock(
    object = object,
    task = task,
    fold_id = fold_id,
    plot_as_grid = plot_as_grid,
    train_color = train_color,
    test_color = test_color,
    show_blocks = show_blocks,
    show_labels = show_labels,
    sample_fold_n = sample_fold_n,
    label_size = label_size,
    ... = ...,
    # ellipsis
    repeats_id = repeats_id
  )
}

#' @importFrom graphics plot
#' @param x `[Resampling]`\cr
#'   mlr3 spatial resampling object. One of class [ResamplingSpCVBuffer],
#'   [ResamplingSpCVBlock], [ResamplingSpCVCoords], [ResamplingSpCVEnv].
#' @rdname autoplot.ResamplingSpCVBlock
#' @export
plot.ResamplingSpCVBlock = function(x, ...) {
  print(autoplot(x, ...)) # nocov
}

#' @rdname autoplot.ResamplingSpCVBlock
#' @export
plot.ResamplingRepeatedSpCVBlock = function(x, ...) {
  print(autoplot(x, ...)) # nocov
}

# SpCV Env ---------------------------------------------------------------------

#' @title Visualization Functions for SpCV Env Methods.
#'
#' @description Generic S3 `plot()` and `autoplot()` (ggplot2) methods.
#'
#' @inheritParams autoplot.ResamplingSpCVBlock
#' @param object `[Resampling]`\cr
#'   mlr3 spatial resampling object of class [ResamplingSpCVEnv] or
#'   [ResamplingRepeatedSpCVEnv].
#' @param x `[Resampling]`\cr
#'   mlr3 spatial resampling object of class [ResamplingSpCVEnv] or
#'   [ResamplingRepeatedSpCVEnv].
#' @export
#' @seealso
#'   - mlr3book chapter on ["Spatiotemporal Visualization"](https://mlr3book.mlr-org.com/special.html#vis-spt-partitions)
#'   - [autoplot.ResamplingSpCVBlock()]
#'   - [autoplot.ResamplingSpCVBuffer()]
#'   - [autoplot.ResamplingSpCVCoords()]
#'   - [autoplot.ResamplingSpCVDisc()]
#'   - [autoplot.ResamplingSpCVTiles()]
#'   - [autoplot.ResamplingCV()]
#'   - [autoplot.ResamplingSptCVCstf()]
#' @examples
#' \donttest{
#' if (mlr3misc::require_namespaces(c("sf", "blockCV"), quietly = TRUE)) {
#'   library(mlr3)
#'   library(mlr3spatiotempcv)
#'   task = tsk("ecuador")
#'   resampling = rsmp("spcv_env", folds = 4, features = "dem")
#'   resampling$instantiate(task)
#'
#'   autoplot(resampling, task) +
#'     ggplot2::scale_x_continuous(breaks = seq(-79.085, -79.055, 0.01))
#'   autoplot(resampling, task, fold_id = 1)
#'   autoplot(resampling, task, fold_id = c(1, 2)) *
#'     ggplot2::scale_x_continuous(breaks = seq(-79.085, -79.055, 0.01))
#' }
#' }
autoplot.ResamplingSpCVEnv = function( # nolint
  object,
  task,
  fold_id = NULL,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  sample_fold_n = NULL,
  ...) {
  autoplot_spatial(
    resampling = object,
    task = task,
    fold_id = fold_id,
    plot_as_grid = plot_as_grid,
    sample_fold_n = sample_fold_n,
    ... = ...
  )
}

#' @rdname autoplot.ResamplingSpCVEnv
#' @export
autoplot.ResamplingRepeatedSpCVEnv = function( # nolint
  object,
  task,
  fold_id = NULL,
  repeats_id = 1,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  sample_fold_n = NULL,
  ...) {

  autoplot.ResamplingSpCVEnv(
    object = object,
    task = task,
    fold_id = fold_id,
    plot_as_grid = plot_as_grid,
    train_color = train_color,
    test_color = test_color,
    sample_fold_n = sample_fold_n,
    ... = ...,
    # ellipsis
    repeats_id = repeats_id
  )
}

#' @importFrom graphics plot
#' @rdname autoplot.ResamplingSpCVEnv
#' @export
plot.ResamplingSpCVEnv = function(x, ...) {
  print(autoplot(x, ...)) # nocov
}

#' @rdname autoplot.ResamplingSpCVEnv
#' @export
plot.ResamplingRepeatedSpCVEnv = function(x, ...) {
  print(autoplot(x, ...)) # nocov
}

# SpCV Coords ------------------------------------------------------------------

#' @title Visualization Functions for SpCV Coords Methods.
#'
#' @description Generic S3 `plot()` and `autoplot()` (ggplot2) methods.
#'
#' @inheritParams autoplot.ResamplingSpCVBlock
#' @name autoplot.ResamplingSpCVCoords
#'
#' @param object `[Resampling]`\cr
#'   mlr3 spatial resampling object of class [ResamplingSpCVCoords] or
#'   [ResamplingRepeatedSpCVCoords].
#' @param x `[Resampling]`\cr
#'   mlr3 spatial resampling object of class [ResamplingSpCVCoords] or
#'   [ResamplingRepeatedSpCVCoords].
#' @export
#' @seealso
#'   - mlr3book chapter on ["Spatiotemporal Visualization"](https://mlr3book.mlr-org.com/special.html#vis-spt-partitions)
#'   - [autoplot.ResamplingSpCVBlock()]
#'   - [autoplot.ResamplingSpCVBuffer()]
#'   - [autoplot.ResamplingSpCVEnv()]
#'   - [autoplot.ResamplingSpCVDisc()]
#'   - [autoplot.ResamplingSpCVTiles()]
#'   - [autoplot.ResamplingCV()]
#'   - [autoplot.ResamplingSptCVCstf()]
#' @examples
#' if (mlr3misc::require_namespaces(c("sf"), quietly = TRUE)) {
#'   library(mlr3)
#'   library(mlr3spatiotempcv)
#'   task = tsk("ecuador")
#'   resampling = rsmp("spcv_coords")
#'   resampling$instantiate(task)
#'
#'   autoplot(resampling, task) +
#'     ggplot2::scale_x_continuous(breaks = seq(-79.085, -79.055, 0.01))
#'   autoplot(resampling, task, fold_id = 1)
#'   autoplot(resampling, task, fold_id = c(1, 2)) *
#'     ggplot2::scale_x_continuous(breaks = seq(-79.085, -79.055, 0.01))
#' }
autoplot.ResamplingSpCVCoords = function( # nolint
  object,
  task,
  fold_id = NULL,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  sample_fold_n = NULL,
  ...) {
  autoplot_spatial(
    resampling = object,
    task = task,
    fold_id = fold_id,
    plot_as_grid = plot_as_grid,
    sample_fold_n = sample_fold_n,
    ... = ...
  )
}

#' @rdname autoplot.ResamplingSpCVCoords
#' @export
autoplot.ResamplingRepeatedSpCVCoords = function( # nolint
  object,
  task,
  fold_id = NULL,
  repeats_id = 1,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  sample_fold_n = NULL,
  ...) {

  autoplot.ResamplingSpCVCoords(
    object = object,
    task = task,
    fold_id = fold_id,
    plot_as_grid = plot_as_grid,
    train_color = train_color,
    test_color = test_color,
    sample_fold_n = sample_fold_n,
    ... = ...,
    # ellipsis
    repeats_id = repeats_id
  )
}

#' @importFrom graphics plot
#' @rdname autoplot.ResamplingSpCVCoords
#' @export
plot.ResamplingSpCVCoords = function(x, ...) {
  print(autoplot(x, ...))
}

#' @rdname autoplot.ResamplingSpCVCoords
#' @export
plot.ResamplingRepeatedSpCVCoords = function(x, ...) {
  print(autoplot(x, ...)) # nocov
}

# SpCVDisc ---------------------------------------------------------------------

#' @title Visualization Functions for SpCV Disc Method.
#'
#' @description Generic S3 `plot()` and `autoplot()` (ggplot2) methods to
#'   visualize mlr3 spatiotemporal resampling objects.
#'
#' @importFrom stats na.omit
#'
#' @details
#' This method requires to set argument `fold_id` and no plot containing all
#' partitions can be created. This is because the method does not make use of
#' all observations but only a subset of them (many observations are left out).
#' Hence, train and test sets of one fold are not re-used in other folds as in
#' other methods and plotting these without a train/test indicator would not
#' make sense.
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
#' @name autoplot.ResamplingSpCVDisc
#' @inheritParams autoplot.ResamplingSpCVBlock
#'
#' @param show_omitted `[logical]`\cr
#'   Whether to show points not used in train or test set for the current fold.
#' @export
#' @seealso
#'   - mlr3book chapter on ["Spatiotemporal Visualization"](https://mlr3book.mlr-org.com/special.html#vis-spt-partitions)
#'   - Vignette [Spatiotemporal Visualization](https://mlr3spatiotempcv.mlr-org.com/articles/spatiotemp-viz.html).
#'   - [autoplot.ResamplingSpCVBlock()]
#'   - [autoplot.ResamplingSpCVBuffer()]
#'   - [autoplot.ResamplingSpCVCoords()]
#'   - [autoplot.ResamplingSpCVTiles()]
#'   - [autoplot.ResamplingSpCVEnv()]
#'   - [autoplot.ResamplingCV()]
#' @examples
#' \donttest{
#' if (mlr3misc::require_namespaces("sf", quietly = TRUE)) {
#'   library(mlr3)
#'   library(mlr3spatiotempcv)
#'   task = tsk("ecuador")
#'   resampling = rsmp("spcv_disc",
#'     folds = 5, radius = 200L, buffer = 200L)
#'   resampling$instantiate(task)
#'
#'   autoplot(resampling, task,
#'     fold_id = 1,
#'     show_omitted = TRUE, size = 0.7) *
#'     ggplot2::scale_x_continuous(breaks = seq(-79.085, -79.055, 0.01))
#' }
#' }
autoplot.ResamplingSpCVDisc = function( # nolint
  object,
  task,
  fold_id = NULL,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  repeats_id = NULL,
  show_omitted = FALSE,
  sample_fold_n = NULL,
  ...) {

  resampling = object
  coords = task$coordinates()
  coords$row_id = task$row_ids
  mlr3misc::require_namespaces(c("sf", "patchwork", "ggtext"))

  resampling = assert_autoplot(resampling, fold_id, task)

  if (is.null(repeats_id)) {
    repeats_id = 1
  } else {
    repeats_id = repeats_id
    # otherwise it gets passed to geom_sf() down below
    # dots$repeats_id = NULL
  }

  resampling_sub = resampling$clone()

  if (any(grepl("ResamplingRepeated", class(resampling)))) {
    resampling_sub$instance = resampling_sub$instance[[repeats_id]]
  }

  if (!is.null(fold_id)) {
    ### Multiplot of single folds with train and test
    plot = autoplot_multi_fold_list(task, resampling_sub, sample_fold_n,
      fold_id, repeats_id, plot_as_grid, show_omitted)
    return(plot)
  } else {
    ### One plot showing all test folds
    plot = autoplot_all_folds_list(task, resampling_sub, sample_fold_n,
      fold_id, repeats_id)
    return(plot)
  }
}

#' @rdname autoplot.ResamplingSpCVDisc
#' @export
autoplot.ResamplingRepeatedSpCVDisc = function( # nolint
  object,
  task,
  fold_id = NULL,
  repeats_id = 1,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  show_omitted = FALSE,
  sample_fold_n = NULL,
  ...) {

  autoplot.ResamplingSpCVDisc(
    object = object,
    task = task,
    fold_id = fold_id,
    plot_as_grid = plot_as_grid,
    train_color = train_color,
    test_color = test_color,
    show_omitted = show_omitted,
    sample_fold_n = sample_fold_n,
    ... = ...,
    # ellipsis
    repeats_id = repeats_id
  )
}

#' @importFrom graphics plot
#' @rdname autoplot.ResamplingSpCVDisc
#' @export
plot.ResamplingSpCVDisc = function(x, ...) {
  print(autoplot(x, ...)) # nocov
}

#' @rdname autoplot.ResamplingSpCVDisc
#' @export
plot.ResamplingRepeatedSpCVDisc = function(x, ...) {
  print(autoplot(x, ...)) # nocov
}


# SpCVTiles --------------------------------------------------------------------

#' @title Visualization Functions for SpCV Tiles Method.
#'
#' @description Generic S3 `plot()` and `autoplot()` (ggplot2) methods to
#'   visualize mlr3 spatiotemporal resampling objects.
#'
#' @importFrom stats na.omit
#'
#' @details
#' Specific combinations of arguments of `"spcv_tiles"` remove some
#' observations, hence `show_omitted` has an effect in some cases.
#'
#' @name autoplot.ResamplingSpCVTiles
#' @inheritParams autoplot.ResamplingSpCVBlock
#'
#' @param show_omitted `[logical]`\cr
#'   Whether to show points not used in train or test set for the current fold.
#' @export
#' @seealso
#'   - mlr3book chapter on ["Spatiotemporal Visualization"](https://mlr3book.mlr-org.com/special.html#vis-spt-partitions)
#'   - Vignette [Spatiotemporal Visualization](https://mlr3spatiotempcv.mlr-org.com/articles/spatiotemp-viz.html).
#'   - [autoplot.ResamplingSpCVBlock()]
#'   - [autoplot.ResamplingSpCVBuffer()]
#'   - [autoplot.ResamplingSpCVCoords()]
#'   - [autoplot.ResamplingSpCVDisc()]
#'   - [autoplot.ResamplingSpCVEnv()]
#'   - [autoplot.ResamplingCV()]
#' @examples
#' \donttest{
#' if (mlr3misc::require_namespaces(c("sf", "sperrorest"), quietly = TRUE)) {
#'   library(mlr3)
#'   library(mlr3spatiotempcv)
#'   task = tsk("ecuador")
#'   resampling = rsmp("spcv_tiles",
#'     nsplit = c(4L, 3L), reassign = FALSE)
#'   resampling$instantiate(task)
#'
#'   autoplot(resampling, task,
#'     fold_id = 1,
#'     show_omitted = TRUE, size = 0.7) *
#'     ggplot2::scale_x_continuous(breaks = seq(-79.085, -79.055, 0.01))
#' }
#' }
autoplot.ResamplingSpCVTiles = function( # nolint
  object,
  task,
  fold_id = NULL,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  repeats_id = NULL,
  show_omitted = FALSE,
  sample_fold_n = NULL,
  ...) {

  resampling = object
  coords = task$coordinates()
  coords$row_id = task$row_ids
  mlr3misc::require_namespaces(c("sf", "patchwork", "ggtext"))

  resampling = assert_autoplot(resampling, fold_id, task)

  if (is.null(repeats_id)) {
    repeats_id = 1
  } else {
    repeats_id = repeats_id
    # otherwise it gets passed to geom_sf() down below
    # dots$repeats_id = NULL
  }

  resampling_sub = resampling$clone(deep = TRUE)

  if (any(grepl("ResamplingRepeated", class(resampling)))) {
    resampling_sub$instance = resampling_sub$instance[[repeats_id]]
  }

  if (!is.null(fold_id)) {
    ### Multiplot of single folds with train and test
    plot = autoplot_multi_fold_list(task, resampling_sub, sample_fold_n,
      fold_id, repeats_id, plot_as_grid, show_omitted)
    return(plot)
  } else {
    ### One plot showing all test folds
    plot = autoplot_all_folds_list(task, resampling_sub, sample_fold_n,
      fold_id, repeats_id)
    return(plot)
  }
}

#' @rdname autoplot.ResamplingSpCVTiles
#' @export
autoplot.ResamplingRepeatedSpCVTiles = function( # nolint
  object,
  task,
  fold_id = NULL,
  repeats_id = 1,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  show_omitted = FALSE,
  sample_fold_n = NULL,
  ...) {

  autoplot.ResamplingSpCVTiles(
    object = object,
    task = task,
    fold_id = fold_id,
    plot_as_grid = plot_as_grid,
    train_color = train_color,
    test_color = test_color,
    show_omitted = show_omitted,
    sample_fold_n = sample_fold_n,
    ... = ...,
    # ellipsis
    repeats_id = repeats_id
  )
}

#' @importFrom graphics plot
#' @rdname autoplot.ResamplingSpCVTiles
#' @export
plot.ResamplingSpCVTiles = function(x, ...) {
  print(autoplot(x, ...)) # nocov
}

#' @rdname autoplot.ResamplingSpCVTiles
#' @export
plot.ResamplingRepeatedSpCVTiles = function(x, ...) {
  print(autoplot(x, ...)) # nocov
}

# SpCVBuffer -------------------------------------------------------------------

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
#' @param show_omitted `[logical]`\cr
#'   Whether to show points not used in train or test set for the current fold.
#' @export
#' @seealso
#'   - mlr3book chapter on ["Spatiotemporal Visualization"](https://mlr3book.mlr-org.com/special.html#vis-spt-partitions)
#'   - [autoplot.ResamplingSpCVBlock()]
#'   - [autoplot.ResamplingSpCVCoords()]
#'   - [autoplot.ResamplingSpCVEnv()]
#'   - [autoplot.ResamplingCV()]
#'   - [autoplot.ResamplingSptCVCstf()]
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
  show_omitted = FALSE,
  ...) {

  mlr3misc::require_namespaces(c("sf", "patchwork", "ggtext"))
  resampling = object

  resampling = assert_autoplot(resampling, fold_id, task)
  resampling_sub = resampling$clone(deep = TRUE)

  # add the row_ids of the task to the coordinates
  coords = task$coordinates()
  coords$row_id = task$row_ids

  if (is.null(fold_id)) {
    stopf("Please provide a fold ID.
      Plotting all folds of a LOOCV instance is not supported", wrap = TRUE)
  }

  # Multiplot of single folds with train and test
  plot = autoplot_multi_fold_list(task, resampling_sub,
    sample_fold_n = NULL, show_omitted = show_omitted,
    fold_id, repeats_id = 1)
  return(plot)
}

#' @rdname autoplot.ResamplingSpCVBuffer
#' @export
plot.ResamplingSpCVBuffer = function(x, ...) {
  print(autoplot(x, ...)) # nocov
}

# SpCVKnndm ---------------------------------------------------------------------

#' @title Visualization Functions for SpCV knndm Method.
#'
#' @description Generic S3 `plot()` and `autoplot()` (ggplot2) methods to
#'   visualize mlr3 spatiotemporal resampling objects.
#'
#' @importFrom stats na.omit
#'
#' @details
#' This method requires to set argument `fold_id` and no plot containing all
#' partitions can be created. This is because the method does not make use of
#' all observations but only a subset of them (many observations are left out).
#' Hence, train and test sets of one fold are not re-used in other folds as in
#' other methods and plotting these without a train/test indicator would not
#' make sense.
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
#' @name autoplot.ResamplingSpCVKnndm
#' @inheritParams autoplot.ResamplingSpCVBlock
#'
#' @export
#' @seealso
#'   - mlr3book chapter on ["Spatiotemporal Visualization"](https://mlr3book.mlr-org.com/special.html#vis-spt-partitions)
#'   - Vignette [Spatiotemporal Visualization](https://mlr3spatiotempcv.mlr-org.com/articles/spatiotemp-viz.html).
#'   - [autoplot.ResamplingSpCVBlock()]
#'   - [autoplot.ResamplingSpCVBuffer()]
#'   - [autoplot.ResamplingSpCVCoords()]
#'   - [autoplot.ResamplingSpCVTiles()]
#'   - [autoplot.ResamplingSpCVEnv()]
#'   - [autoplot.ResamplingCV()]
#' @examples
#' \donttest{
#' if (mlr3misc::require_namespaces(c("CAST", "sf"), quietly = TRUE)) {
#'   library(mlr3)
#'   library(mlr3spatiotempcv)
#'   task = tsk("ecuador")
#'   points = sf::st_as_sf(task$coordinates(), crs = task$crs, coords = c("x", "y"))
#'   modeldomain = sf::st_as_sfc(sf::st_bbox(points))
#'
#'   resampling = rsmp("spcv_knndm",
#'     folds = 5, modeldomain = modeldomain)
#'   resampling$instantiate(task)
#'
#'   autoplot(resampling, task,
#'     fold_id = 1, size = 0.7) *
#'     ggplot2::scale_x_continuous(breaks = seq(-79.085, -79.055, 0.01))
#' }
#' }
autoplot.ResamplingSpCVKnndm = function( # nolint
  object,
  task,
  fold_id = NULL,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  repeats_id = NULL,
  sample_fold_n = NULL,
  ...) {

  resampling = object
  coords = task$coordinates()
  coords$row_id = task$row_ids
  mlr3misc::require_namespaces(c("sf", "patchwork", "ggtext"))

  resampling = assert_autoplot(resampling, fold_id, task)

  if (is.null(repeats_id)) {
    repeats_id = 1
  } else {
    repeats_id = repeats_id
    # otherwise it gets passed to geom_sf() down below
    # dots$repeats_id = NULL
  }

  resampling_sub = resampling$clone()

  if (any(grepl("ResamplingRepeated", class(resampling)))) {
    resampling_sub$instance = resampling_sub$instance[[repeats_id]]
  }

  if (!is.null(fold_id)) {
    ### Multiplot of single folds with train and test
    plot = autoplot_multi_fold_list(task, resampling_sub, sample_fold_n,
      fold_id, repeats_id, plot_as_grid)
    return(plot)
  } else {
    ### One plot showing all test folds
    plot = autoplot_all_folds_list(task, resampling_sub, sample_fold_n,
      fold_id, repeats_id)
    return(plot)
  }
}

#' @rdname autoplot.ResamplingSpCVKnndm
#' @export
autoplot.ResamplingRepeatedSpCVKnndm = function( # nolint
  object,
  task,
  fold_id = NULL,
  repeats_id = 1,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  sample_fold_n = NULL,
  ...) {

  autoplot.ResamplingSpCVKnndm(
    object = object,
    task = task,
    fold_id = fold_id,
    plot_as_grid = plot_as_grid,
    train_color = train_color,
    test_color = test_color,
    sample_fold_n = sample_fold_n,
    ... = ...,
    # ellipsis
    repeats_id = repeats_id
  )
}

#' @importFrom graphics plot
#' @rdname autoplot.ResamplingSpCVKnndm
#' @export
plot.ResamplingSpCVKnndm = function(x, ...) {
  print(autoplot(x, ...)) # nocov
}

#' @rdname autoplot.ResamplingSpCVKnndm
#' @export
plot.ResamplingRepeatedSpCVKnndm = function(x, ...) {
  print(autoplot(x, ...)) # nocov
}




# CV ---------------------------------------------------------------------------

#' @title Visualization Functions for Non-Spatial CV Methods.
#'
#' @description Generic S3 `plot()` and `autoplot()` (ggplot2) methods.
#'
#' @name autoplot.ResamplingCV
#' @param object `[Resampling]`\cr
#'   mlr3 spatial resampling object of class [ResamplingCV] or
#'   [ResamplingRepeatedCV].
#' @param x `[Resampling]`\cr
#'   mlr3 spatial resampling object of class [ResamplingCV] or
#'   [ResamplingRepeatedCV].
#' @inheritParams autoplot.ResamplingSpCVBlock
#' @export
#' @seealso
#'   - mlr3book chapter on ["Spatiotemporal Visualization"](https://mlr3book.mlr-org.com/special.html#vis-spt-partitions)
#'   - [autoplot.ResamplingSpCVBlock()]
#'   - [autoplot.ResamplingSpCVBuffer()]
#'   - [autoplot.ResamplingSpCVCoords()]
#'   - [autoplot.ResamplingSpCVEnv()]
#'   - [autoplot.ResamplingSpCVDisc()]
#'   - [autoplot.ResamplingSpCVTiles()]
#'   - [autoplot.ResamplingSptCVCstf()]
#' @examples
#' if (mlr3misc::require_namespaces(c("sf", "patchwork", "ggtext", "ggsci"), quietly = TRUE)) {
#'   library(mlr3)
#'   library(mlr3spatiotempcv)
#'   task = tsk("ecuador")
#'   resampling = rsmp("cv")
#'   resampling$instantiate(task)
#'
#'   autoplot(resampling, task) +
#'     ggplot2::scale_x_continuous(breaks = seq(-79.085, -79.055, 0.01))
#'   autoplot(resampling, task, fold_id = 1)
#'   autoplot(resampling, task, fold_id = c(1, 2)) *
#'     ggplot2::scale_x_continuous(breaks = seq(-79.085, -79.055, 0.01))
#' }
autoplot.ResamplingCV = function( # nolint
  object,
  task,
  fold_id = NULL,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  sample_fold_n = NULL,
  ...) {
  autoplot_spatial(
    resampling = object,
    task = task,
    fold_id = fold_id,
    plot_as_grid = plot_as_grid,
    sample_fold_n = sample_fold_n,
    ... = ...
  )
}

#' @rdname autoplot.ResamplingCV
#' @export
autoplot.ResamplingRepeatedCV = function( # nolint
  object,
  task,
  fold_id = NULL,
  repeats_id = 1,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  sample_fold_n = NULL,
  ...) {

  autoplot.ResamplingCV(
    object = object,
    task = task,
    fold_id = fold_id,
    plot_as_grid = plot_as_grid,
    train_color = train_color,
    test_color = test_color,
    sample_fold_n = sample_fold_n,
    ... = ...,
    # ellipsis
    repeats_id = repeats_id
  )
}

# Custom CV --------------------------------------------------------------------

#' @title Visualization Functions for Non-Spatial CV Methods.
#'
#' @description Generic S3 `plot()` and `autoplot()` (ggplot2) methods.
#'
#' @name autoplot.ResamplingCustomCV
#' @param object `[Resampling]`\cr
#'   mlr3 spatial resampling object of class [ResamplingCustomCV].
#' @param x `[Resampling]`\cr
#'   mlr3 spatial resampling object of class [ResamplingCustomCV].
#' @inheritParams autoplot.ResamplingSpCVBlock
#' @export
#' @seealso
#'   - mlr3book chapter on ["Spatiotemporal Visualization"](https://mlr3book.mlr-org.com/special.html#vis-spt-partitions)
#'   - [autoplot.ResamplingSpCVBlock()]
#'   - [autoplot.ResamplingSpCVBuffer()]
#'   - [autoplot.ResamplingSpCVCoords()]
#'   - [autoplot.ResamplingSpCVEnv()]
#'   - [autoplot.ResamplingSpCVDisc()]
#'   - [autoplot.ResamplingSpCVTiles()]
#'   - [autoplot.ResamplingCV()]
#'   - [autoplot.ResamplingSptCVCstf()]
#' @examples
#' if (mlr3misc::require_namespaces(c("sf", "patchwork"), quietly = TRUE)) {
#'   library(mlr3)
#'   library(mlr3spatiotempcv)
#'   task = tsk("ecuador")
#'   breaks = quantile(task$data()$dem, seq(0, 1, length = 6))
#'   zclass = cut(task$data()$dem, breaks, include.lowest = TRUE)
#'
#'   resampling = rsmp("custom_cv")
#'   resampling$instantiate(task, f = zclass)
#'
#'   autoplot(resampling, task) +
#'     ggplot2::scale_x_continuous(breaks = seq(-79.085, -79.055, 0.01))
#'   autoplot(resampling, task, fold_id = 1)
#'   autoplot(resampling, task, fold_id = c(1, 2)) *
#'     ggplot2::scale_x_continuous(breaks = seq(-79.085, -79.055, 0.01))
#' }
autoplot.ResamplingCustomCV = function( # nolint
  object,
  task,
  fold_id = NULL,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  sample_fold_n = NULL,
  ...) {
  autoplot_custom_cv(
    resampling = object,
    task = task,
    fold_id = fold_id,
    plot_as_grid = plot_as_grid,
    sample_fold_n = sample_fold_n,
    ... = ...
  )
}

#' @importFrom graphics plot
#' @rdname autoplot.ResamplingCV
#' @export
plot.ResamplingCV = function(x, ...) {
  print(autoplot(x, ...)) # nocov
}

#' @rdname autoplot.ResamplingCV
#' @export
plot.ResamplingRepeatedCV = function(x, ...) {
  print(autoplot(x, ...)) # nocov
}

#' @rdname autoplot.ResamplingCustomCV
#' @export
plot.ResamplingCustomCV = function(x, ...) {
  print(autoplot(x, ...)) # nocov
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
  show_blocks = FALSE,
  show_labels = FALSE,
  sample_fold_n = NULL,
  label_size = NULL,
  ...) {

  mlr3misc::require_namespaces(c("sf", "patchwork", "ggtext"))

  # we need to work with a clone, otherwise the object modifications
  # will create issues in future calls usings the same object
  rsmp_autopl = resampling$clone()

  rsmp_autopl = assert_autoplot(rsmp_autopl, fold_id, task)

  # add the row_ids of the task to the coordinates
  coords = task$coordinates()
  coords$row_id = task$row_ids

  if (is.null(repeats_id)) {
    repeats_id = 1
  }

  if (!is.null(task$groups)) {
    coords_resamp = merge(coords, task$groups, by = "row_id")
    rsmp_autopl$instance$row_id = as.character(rsmp_autopl$instance$row_id)
    rsmp_autopl$instance$group = rsmp_autopl$instance$row_id
    rsmp_autopl$instance$row_id = NULL
    coords_resamp$group = as.character(coords_resamp$group)
    coords_resamp = merge(coords_resamp, rsmp_autopl$instance, by = "group",
      all = TRUE)
    setorder(coords_resamp, "row_id")
    coords_resamp$group = NULL
  } else {
    coords_resamp = merge(coords, rsmp_autopl$instance, by = "row_id")
  }

  if (any(grepl("ResamplingRepeated", class(rsmp_autopl)))) {
    coords_resamp = coords_resamp[rep == repeats_id, ]
  }

  if (!is.null(fold_id)) {
    ### Multiplot of single folds with train and test --------------------------
    plot = autoplot_multi_fold_dt(task = task, resampling = rsmp_autopl,
      resampling_mod = coords_resamp,
      sample_fold_n = sample_fold_n, fold_id = fold_id,
      repeats_id = repeats_id, plot_as_grid = plot_as_grid,
      show_blocks = show_blocks, show_labels = show_labels,
      label_size = label_size, ...)
  } else {
    ### One plot showing all test folds ----------------------------------------
    plot = autoplot_all_folds_dt(task = task, resampling = coords_resamp,
      sample_fold_n = sample_fold_n, fold_id = fold_id,
      repeats_id = repeats_id, plot_as_grid = plot_as_grid,
      show_blocks = show_blocks, show_labels = show_labels, ...)
  }
  return(plot)
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
  nticks_x = NULL,
  nticks_y = NULL,
  point_size = NULL,
  axis_label_fontsize = NULL,
  sample_fold_n = NULL,
  ...) {

  mlr3misc::require_namespaces("plotly")
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
  if (any(grepl("ResamplingRepeated", class(resampling)))) {
    task_resamp_ids = task_resamp_ids[rep == repeats_id, ]
  }

  coords = sf::st_coordinates(sf::st_as_sf(task$coordinates(),
    coords = task$coordinate_names,
    crs = task$crs))
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

      # add time col role back to `task_resamp_ids` as its needed for plotting
      task_resamp_ids$Date = task$data(cols = task$col_roles$time)[[task$col_roles$time]]

      task_resamp_ids$indicator = as.factor(as.character(task_resamp_ids$indicator))
      task_resamp_ids[, indicator := ifelse(fold == fold_id, "Test", "Train")]

      # take stratified random sample from folds
      if (!is.null(sample_fold_n)) {
        task_resamp_ids = strat_sample_folds(task_resamp_ids, test, sample_fold_n)
      }

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

      task_resamp_ids$Date = task$data(cols = task$col_roles$time)[[task$col_roles$time]]

      task_resamp_ids$indicator = as.factor(as.character(task_resamp_ids$indicator))

      # take stratified random sample from folds
      if (!is.null(sample_fold_n)) {
        task_resamp_ids = strat_sample_folds(task_resamp_ids, test, sample_fold_n)
      }

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
          scene = paste0("scene", .x),
          showlegend = ifelse(.x == 1, TRUE, FALSE)
        )

        # create plots for each fold
        pl = plotly::plot_ly(task_resamp_ids,
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
      return(invisible(plot_list))
    } else {
      lg$warn("Unfortunately plotly does not support a dynamic arrangement of multiple subplots.
       See article 'Visualization of spatiotemporal clusters' (https://mlr3spatiotempcv.mlr-org.com/articles/spatiotemp-viz) for a manual workaround.
       Use the objects in the returned list to arrange a custom grid.")

      return(invisible(plot_list))
    }
  } else {
    # add time col role back to `task_resamp_ids` as its needed for plotting
    task_resamp_ids$Date = task$data(cols = task$col_roles$time)[[task$col_roles$time]]

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

# autoplot_custom_cv -----------------------------------------------------------

autoplot_custom_cv = function(
  resampling = NULL,
  task = NULL,
  fold_id = NULL,
  repeats_id = NULL,
  plot_as_grid = NULL,
  train_color = NULL,
  test_color = NULL,
  sample_fold_n = NULL,
  ...) {

  mlr3misc::require_namespaces(c("sf", "patchwork", "ggtext"))

  # we need to work with a clone, otherwise the object modifications
  # will create issues in future calls usings the same object
  rsmp_autopl = resampling$clone()

  rsmp_autopl = assert_autoplot(rsmp_autopl, fold_id, task)

  # add the row_ids of the task to the coordinates
  coords = task$coordinates()
  coords$row_id = task$row_ids

  # bring into correct format (complicated alternative to reshape::melt)
  rsmp_autopl$instance = data.table::rbindlist(
    lapply(rsmp_autopl$instance, as.data.table),
    idcol = "fold")
  setnames(rsmp_autopl$instance, c("fold", "row_id"))

  coords_resamp = merge(coords, rsmp_autopl$instance, by = "row_id")
  # we need integers and not characters for the upcoming map() calls
  coords_resamp$fold = as.integer(as.factor(coords_resamp$fold))

  if (is.null(repeats_id)) {
    repeats_id = 1
  }

  if (!is.null(fold_id)) {
    ### Multiplot of single folds with train and test --------------------------
    plot = autoplot_multi_fold_dt(task = task, resampling = rsmp_autopl,
      resampling_mod = coords_resamp, show_blocks = FALSE,
      sample_fold_n = sample_fold_n, fold_id = fold_id,
      repeats_id = repeats_id, plot_as_grid = plot_as_grid, ...)
  } else {
    ### One plot showing all test folds ----------------------------------------
    plot = autoplot_all_folds_dt(task = task, resampling = coords_resamp,
      sample_fold_n = sample_fold_n, fold_id = fold_id,
      repeats_id = repeats_id, plot_as_grid = plot_as_grid, ...)
  }
  return(plot)
}
