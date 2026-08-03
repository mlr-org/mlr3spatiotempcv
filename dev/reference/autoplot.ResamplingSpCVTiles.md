# Visualization Functions for SpCV Tiles Method.

Generic S3 [`plot()`](https://rdrr.io/r/graphics/plot.default.html) and
[`autoplot()`](https://mlr3spatiotempcv.mlr-org.com/dev/reference/autoplot.md)
(ggplot2) methods to visualize mlr3 spatiotemporal resampling objects.

## Usage

``` r
# S3 method for class 'ResamplingSpCVTiles'
autoplot(
  object,
  task,
  fold_id = NULL,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  repeats_id = NULL,
  show_omitted = FALSE,
  sample_fold_n = NULL,
  ...
)

# S3 method for class 'ResamplingRepeatedSpCVTiles'
autoplot(
  object,
  task,
  fold_id = NULL,
  repeats_id = 1,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  show_omitted = FALSE,
  sample_fold_n = NULL,
  ...
)

# S3 method for class 'ResamplingSpCVTiles'
plot(x, ...)

# S3 method for class 'ResamplingRepeatedSpCVTiles'
plot(x, ...)
```

## Arguments

- object:

  `[Resampling]`  
  mlr3 spatial resampling object of class
  [ResamplingSpCVBlock](https://mlr3spatiotempcv.mlr-org.com/dev/reference/mlr_resamplings_spcv_block.md)
  or
  [ResamplingRepeatedSpCVBlock](https://mlr3spatiotempcv.mlr-org.com/dev/reference/mlr_resamplings_repeated_spcv_block.md).

- task:

  `[TaskClassifST]/[TaskRegrST]`  
  mlr3 task object.

- fold_id:

  `[numeric]`  
  Fold IDs to plot.

- plot_as_grid:

  `[logical(1)]`  
  Should a gridded plot using via
  [patchwork](https://CRAN.R-project.org/package=patchwork) be created?
  If `FALSE` a list with of
  [ggplot2](https://CRAN.R-project.org/package=ggplot2) objects is
  returned. Only applies if a numeric vector is passed to argument
  `fold_id`.

- train_color:

  `[character(1)]`  
  The color to use for the training set observations.

- test_color:

  `[character(1)]`  
  The color to use for the test set observations.

- repeats_id:

  `[numeric]`  
  Repetition ID to plot.

- show_omitted:

  `[logical]`  
  Whether to show points not used in train or test set for the current
  fold.

- sample_fold_n:

  `[integer]`  
  Number of points in a random sample stratified over partitions. This
  argument aims to keep file sizes of resulting plots reasonable and
  reduce overplotting in dense datasets.

- ...:

  Passed to `geom_sf()`. Helpful for adjusting point sizes and shapes.

- x:

  `[Resampling]`  
  mlr3 spatial resampling object. One of class
  [ResamplingSpCVBuffer](https://mlr3spatiotempcv.mlr-org.com/dev/reference/mlr_resamplings_spcv_buffer.md),
  [ResamplingSpCVBlock](https://mlr3spatiotempcv.mlr-org.com/dev/reference/mlr_resamplings_spcv_block.md),
  [ResamplingSpCVCoords](https://mlr3spatiotempcv.mlr-org.com/dev/reference/mlr_resamplings_spcv_coords.md),
  [ResamplingSpCVEnv](https://mlr3spatiotempcv.mlr-org.com/dev/reference/mlr_resamplings_spcv_env.md).

## Details

Specific combinations of arguments of `"spcv_tiles"` remove some
observations, hence `show_omitted` has an effect in some cases.

## See also

- mlr3book chapter on ["Spatial
  Analysis"](https://mlr3book.mlr-org.com/chapters/chapter13/beyond_regression_and_classification.html#sec-spatiotemporal)

- Vignette [Spatiotemporal
  Visualization](https://mlr3spatiotempcv.mlr-org.com/articles/spatiotemp-viz.html).

- [`autoplot.ResamplingSpCVBlock()`](https://mlr3spatiotempcv.mlr-org.com/dev/reference/autoplot.ResamplingSpCVBlock.md)

- [`autoplot.ResamplingSpCVBuffer()`](https://mlr3spatiotempcv.mlr-org.com/dev/reference/autoplot.ResamplingSpCVBuffer.md)

- [`autoplot.ResamplingSpCVCoords()`](https://mlr3spatiotempcv.mlr-org.com/dev/reference/autoplot.ResamplingSpCVCoords.md)

- [`autoplot.ResamplingSpCVDisc()`](https://mlr3spatiotempcv.mlr-org.com/dev/reference/autoplot.ResamplingSpCVDisc.md)

- [`autoplot.ResamplingSpCVEnv()`](https://mlr3spatiotempcv.mlr-org.com/dev/reference/autoplot.ResamplingSpCVEnv.md)

- [`autoplot.ResamplingCV()`](https://mlr3spatiotempcv.mlr-org.com/dev/reference/autoplot.ResamplingCV.md)

## Examples

``` r
# \donttest{
if (mlr3misc::require_namespaces(c("sf", "sperrorest"), quietly = TRUE)) {
  library(mlr3)
  library(mlr3spatiotempcv)
  task = tsk("ecuador")
  resampling = rsmp("spcv_tiles",
    nsplit = c(4L, 3L), reassign = FALSE)
  resampling$instantiate(task)

  autoplot(resampling, task,
    fold_id = 1,
    show_omitted = TRUE, size = 0.7) *
    ggplot2::scale_x_continuous(breaks = seq(-79.085, -79.055, 0.01))
}

# }
```
