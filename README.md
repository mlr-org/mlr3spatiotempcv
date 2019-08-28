
# mlr3spatiotemporal

Spatio-temporal extensions for mlr3

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/mlr-org/mlr3spatiotemporal.svg?branch=master)](https://travis-ci.org/mlr-org/mlr3spatiotemporal)
[![CRAN
status](https://www.r-pkg.org/badges/version/mlr3spatiotemporal)](https://cran.r-project.org/package=mlr3spatiotemporal)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Coverage
status](https://codecov.io/gh/mlr-org/mlr3spatiotemporal/branch/master/graph/badge.svg)](https://codecov.io/github/mlr-org/mlr3spatiotemporal?branch=master)
<!-- badges: end -->

This package extends the [mlr3](https://github.com/mlr-org/mlr3) package
framework by spatiotemporal resampling and visualization methods.

## Resampling methods

Currently, the following ones are
implemented:

| Literature Name        | R Package                                                 | Reference     | mlr3spatiotemporal Name |
| ---------------------- | --------------------------------------------------------- | ------------- | ----------------------- |
| Spatial CV             | [sperrorest](https://github.com/giscience-fsu/sperrorest) | Brenning 2012 | `ResampleSpCVCoords`    |
| Spatial Blocking       | [blockCV](https://github.com/rvalavi/blockCV)             | Valavi 2019   | `ResampleSpCVBlock`     |
| Environmental Blocking | [blockCV](https://github.com/rvalavi/blockCV)             | Valavi 2019   | `ResampleSpCVEnv`       |
| Spatial Buffering      | [blockCV](https://github.com/rvalavi/blockCV)             | Valavi 2019   | `ResampleSpCVBuffer`    |

## Spatial tasks

  - Task “ecuador” -\> `mlr_tasks$get("ecuador")`

## Visualization methods

Generic S3 function `autoplot()` for all implemented spatial resampling
methods.

# References

<div id="refs" class="references">

<div id="ref-Brenning2012">

**Brenning**, A., 2012. “Spatial Cross-Validation and Bootstrap for the
Assessment of Prediction Rules in Remote Sensing: The R Package
Sperrorest.” In *2012 IEEE International Geoscience and Remote Sensing
Symposium*, 5372–5. <https://doi.org/10.1109/IGARSS.2012.6352393>.

</div>

<div id="ref-Valavi2019">

**Valavi** R, Elith J, Lahoz-Monfort JJ, Guillera-Arroita G. blockCV: An
r package for generating spatially or environmentally separated folds
for k-fold cross-validation of species distribution models. Methods Ecol
Evol. 2019; 10:225–232. doi.org/10.1111/2041-210X.13107

</div>

</div>
