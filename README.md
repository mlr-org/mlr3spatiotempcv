
# mlr3spatiotempcv

Spatio-temporal resampling methods for mlr3.

<!-- badges: start -->

[![Build
Status](https://img.shields.io/travis/mlr-org/mlr3spatiotempcv/master?label=Linux&logo=travis&style=flat-square)](https://travis-ci.org/mlr-org/mlr3spatiotempcv)
[![CircleCI build
status](https://circleci.com/gh/mlr-org/mlr3spatiotempcv.svg?style=svg)](https://circleci.com/gh/mlr-org/mlr3spatiotempcv)
[![CRAN Status
Badge](https://www.r-pkg.org/badges/version-ago/mlr3spatiotempcv)](https://cran.r-project.org/package=mlr3spatiotempcv)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Coverage
status](https://codecov.io/gh/mlr-org/mlr3spatiotempcv/branch/master/graph/badge.svg)](https://codecov.io/github/mlr-org/mlr3spatiotempcv?branch=master)
<!-- badges: end -->

This package extends the [mlr3](https://github.com/mlr-org/mlr3) package
framework by spatiotemporal resampling and visualization methods.

## Resampling methods

Currently, the following ones are implemented:

| Literature             | Package                                                   | Reference     | Class Name           | mlr3 sugar            |
| ---------------------- | --------------------------------------------------------- | ------------- | -------------------- | --------------------- |
| Spatial Buffering      | [blockCV](https://github.com/rvalavi/blockCV)             | Valavi 2019   | `ResampleSpCVBuffer` | `rsmp("spcv-buffer")` |
| Spatial Blocking       | [blockCV](https://github.com/rvalavi/blockCV)             | Valavi 2019   | `ResampleSpCVBlock`  | `rsmp("spcv-block")`  |
| Spatial CV             | [sperrorest](https://github.com/giscience-fsu/sperrorest) | Brenning 2012 | `ResampleSpCVCoords` | `rsmp("spcv-coords")` |
| Environmental Blocking | [blockCV](https://github.com/rvalavi/blockCV)             | Valavi 2019   | `ResampleSpCVEnv`    | `rsmp("spcv-env")`    |

## Spatial tasks

| Name     | Code                        | Type    |
| -------- | --------------------------- | ------- |
| ecuador  | `mlr_tasks$get("ecuador")`  | Classif |
| diplodia | `mlr_tasks$get("diplodia")` | Classif |

## Visualization

Generic S3 function `autoplot()` for all implemented spatial resampling
methods.

### Visualization of all partitions

``` r
library(mlr3)
library(mlr3spatiotempcv)
library(ggplot2)

task = tsk("ecuador")
resampling = rsmp("spcv-coords", folds = 5)
resampling$instantiate(task)

autoplot(resampling, task)
```

![](man/figures/README-spcv-coords-all-partitions-1.png)<!-- -->

### Visualization of the first fold only

``` r
autoplot(resampling, task, fold_id = 1)
```

![](man/figures/README-spcv-coords-fold-1.png)<!-- -->

## More resources

For detailed information on how to use spatial resampling in {mlr3}
please read the section about [spatial analysis in the mlr3
book](https://mlr3book.mlr-org.com/spatial.html).

# References

<div id="refs" class="references hanging-indent">

<div id="ref-brenning2012">

Brenning, Alexander. 2012. “Spatial Cross-Validation and Bootstrap for
the Assessment of Prediction Rules in Remote Sensing: The R Package
sperrorest.” In *2012 IEEE International Geoscience and Remote Sensing
Symposium*. IEEE. <https://doi.org/10.1109/igarss.2012.6352393>.

</div>

<div id="ref-valavi2018">

Valavi, Roozbeh, Jane Elith, José J. Lahoz-Monfort, and Gurutzeta
Guillera-Arroita. 2018. “blockCV: an R package for generating spatially
or environmentally separated folds for k-fold cross-validation of
species distribution models,” June. <https://doi.org/10.1101/357798>.

</div>

</div>
