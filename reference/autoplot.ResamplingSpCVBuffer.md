# Visualization Functions for SpCV Buffer Methods.

Generic S3 [`plot()`](https://rdrr.io/r/graphics/plot.default.html) and
[`autoplot()`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.md)
(ggplot2) methods to visualize mlr3 spatiotemporal resampling objects.

## Usage

``` r
# S3 method for class 'ResamplingSpCVBuffer'
autoplot(
  object,
  task,
  fold_id = NULL,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  show_omitted = FALSE,
  ...
)

# S3 method for class 'ResamplingSpCVBuffer'
plot(x, ...)
```

## Arguments

- object:

  `[Resampling]`  
  mlr3 spatial resampling object of class
  [ResamplingSpCVBuffer](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_resamplings_spcv_buffer.md).

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

- show_omitted:

  `[logical]`  
  Whether to show points not used in train or test set for the current
  fold.

- ...:

  Passed to `geom_sf()`. Helpful for adjusting point sizes and shapes.

- x:

  `[Resampling]`  
  mlr3 spatial resampling object of class
  [ResamplingSpCVBuffer](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_resamplings_spcv_buffer.md).

## See also

- mlr3book chapter on ["Spatial
  Analysis"](https://mlr3book.mlr-org.com/chapters/chapter13/beyond_regression_and_classification.html#sec-spatiotemporal)

- [`autoplot.ResamplingSpCVBlock()`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingSpCVBlock.md)

- [`autoplot.ResamplingSpCVCoords()`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingSpCVCoords.md)

- [`autoplot.ResamplingSpCVEnv()`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingSpCVEnv.md)

- [`autoplot.ResamplingCV()`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingCV.md)

- [`autoplot.ResamplingSptCVCstf()`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingSptCVCstf.md)

## Examples

``` r
# \donttest{
if (mlr3misc::require_namespaces(c("sf", "blockCV"), quietly = TRUE)) {
  library(mlr3)
  library(mlr3spatiotempcv)
  task = tsk("ecuador")
  resampling = rsmp("spcv_buffer", theRange = 1000)
  resampling$instantiate(task)

  ## single fold
  autoplot(resampling, task, fold_id = 1) +
    ggplot2::scale_x_continuous(breaks = seq(-79.085, -79.055, 0.01))

  ## multiple folds
  autoplot(resampling, task, fold_id = c(1, 2)) *
    ggplot2::scale_x_continuous(breaks = seq(-79.085, -79.055, 0.01))
}

# }
```
