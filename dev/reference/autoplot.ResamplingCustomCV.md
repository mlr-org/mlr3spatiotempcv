# Visualization Functions for Non-Spatial CV Methods.

Generic S3 [`plot()`](https://rdrr.io/r/graphics/plot.default.html) and
[`autoplot()`](https://mlr3spatiotempcv.mlr-org.com/dev/reference/autoplot.md)
(ggplot2) methods.

## Usage

``` r
# S3 method for class 'ResamplingCustomCV'
autoplot(
  object,
  task,
  fold_id = NULL,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  sample_fold_n = NULL,
  ...
)

# S3 method for class 'ResamplingCustomCV'
plot(x, ...)
```

## Arguments

- object:

  `[Resampling]`  
  mlr3 spatial resampling object of class
  [mlr3::ResamplingCustomCV](https://mlr3.mlr-org.com/reference/mlr_resamplings_custom_cv.html).

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

- sample_fold_n:

  `[integer]`  
  Number of points in a random sample stratified over partitions. This
  argument aims to keep file sizes of resulting plots reasonable and
  reduce overplotting in dense datasets.

- ...:

  Passed to `geom_sf()`. Helpful for adjusting point sizes and shapes.

- x:

  `[Resampling]`  
  mlr3 spatial resampling object of class
  [mlr3::ResamplingCustomCV](https://mlr3.mlr-org.com/reference/mlr_resamplings_custom_cv.html).

## See also

- mlr3book chapter on ["Spatial
  Analysis"](https://mlr3book.mlr-org.com/chapters/chapter13/beyond_regression_and_classification.html#sec-spatiotemporal)

- [`autoplot.ResamplingSpCVBlock()`](https://mlr3spatiotempcv.mlr-org.com/dev/reference/autoplot.ResamplingSpCVBlock.md)

- [`autoplot.ResamplingSpCVBuffer()`](https://mlr3spatiotempcv.mlr-org.com/dev/reference/autoplot.ResamplingSpCVBuffer.md)

- [`autoplot.ResamplingSpCVCoords()`](https://mlr3spatiotempcv.mlr-org.com/dev/reference/autoplot.ResamplingSpCVCoords.md)

- [`autoplot.ResamplingSpCVEnv()`](https://mlr3spatiotempcv.mlr-org.com/dev/reference/autoplot.ResamplingSpCVEnv.md)

- [`autoplot.ResamplingSpCVDisc()`](https://mlr3spatiotempcv.mlr-org.com/dev/reference/autoplot.ResamplingSpCVDisc.md)

- [`autoplot.ResamplingSpCVTiles()`](https://mlr3spatiotempcv.mlr-org.com/dev/reference/autoplot.ResamplingSpCVTiles.md)

- [`autoplot.ResamplingCV()`](https://mlr3spatiotempcv.mlr-org.com/dev/reference/autoplot.ResamplingCV.md)

- [`autoplot.ResamplingSptCVCstf()`](https://mlr3spatiotempcv.mlr-org.com/dev/reference/autoplot.ResamplingSptCVCstf.md)

## Examples

``` r
if (mlr3misc::require_namespaces(c("sf", "patchwork"), quietly = TRUE)) {
  library(mlr3)
  library(mlr3spatiotempcv)
  task = tsk("ecuador")
  breaks = quantile(task$data()$dem, seq(0, 1, length = 6))
  zclass = cut(task$data()$dem, breaks, include.lowest = TRUE)

  resampling = rsmp("custom_cv")
  resampling$instantiate(task, f = zclass)

  autoplot(resampling, task) +
    ggplot2::scale_x_continuous(breaks = seq(-79.085, -79.055, 0.01))
  autoplot(resampling, task, fold_id = 1)
  autoplot(resampling, task, fold_id = c(1, 2)) *
    ggplot2::scale_x_continuous(breaks = seq(-79.085, -79.055, 0.01))
}
```
