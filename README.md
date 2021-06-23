# mlr3spatiotempcv

Package website: [release](https://mlr3spatiotempcv.mlr-org.com/) \| [dev](https://mlr3spatiotempcv.mlr-org.com/dev/)

Spatiotemporal resampling methods for mlr3.

<!-- badges: start -->

[![tic](https://github.com/mlr-org/mlr3spatiotempcv/workflows/tic/badge.svg?branch=main)](https://github.com/mlr-org/mlr3spatiotempcv/actions) [![CRAN Status](https://www.r-pkg.org/badges/version-ago/mlr3spatiotempcv)](https://cran.r-project.org/package=mlr3spatiotempcv) [![Coverage status](https://codecov.io/gh/mlr-org/mlr3spatiotempcv/branch/main/graph/badge.svg)](https://codecov.io/github/mlr-org/mlr3spatiotempcv?branch=main) [![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html) [![CodeFactor](https://www.codefactor.io/repository/github/mlr-org/mlr3spatiotempcv/badge)](https://www.codefactor.io/repository/github/mlr-org/mlr3spatiotempcv)

<!-- badges: end -->

This package extends the [mlr3](https://github.com/mlr-org/mlr3) package framework with spatiotemporal resampling and visualization methods.

If you prefer the [tidymodels](https://www.tidymodels.org/) ecosystem, have a look at the [{spatialsample}](https://spatialsample.tidymodels.org/index.html) package for spatial sampling methods.

# Installation

CRAN version

``` r
install.packages("mlr3spatiotempcv")
```

Development version

``` r
remotes::install_github("mlr-org/mlr3spatiotempcv")

# R Universe Repo
install.packages('mlr3spatiotempcv', mlrorg = 'https://mlr-org.r-universe.dev')
```

# Get Started

See the ["Get Started"](https://mlr3spatiotempcv.mlr-org.com/articles/mlr3spatiotempcv.html) vignette for a quick introduction.

For more detailed information including an usage example see the ["Spatiotemporal Analysis"](https://mlr3book.mlr-org.com/spatiotemporal.html) chapter in the mlr3book.

Article ["Spatiotemporal Visualization"](https://mlr3spatiotempcv.mlr-org.com/articles/spatiotemp-viz.html) shows how 3D subplots grids can be created.

# Citation

To cite the package in publications, use the output of `citation("mlr3spatiotempcv")`.

# Other spatiotemporal resampling packages

This list does not claim to be comprehensive.

| Name          | Language | Resources                                                                                                                                                                              |
| ------------- | -------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| blockCV       | R        | [Paper](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13107), [CRAN](https://cran.r-project.org/package=blockCV)                              |
| CAST          | R        | [Paper](https://www.sciencedirect.com/science/article/pii/S1364815217310976), [CRAN](https://cran.r-project.org/package=CAST) |
| ENMeval       | R        | [Paper](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.12261), [CRAN](https://cran.r-project.org/package=ENMeval)                              |
| spatialsample | R        | [CRAN](https://cran.r-project.org/package=spatialsample)                                                                                                               |
| sperrorest    | R        | [Paper](https://doi.org/10.1109%2Figarss.2012.6352393), [CRAN](https://cran.r-project.org/package=sperrorest)                                                          |
| Pyspatialml   | Python   | [GitHub](https://github.com/stevenpawley/Pyspatialml)                                                                                                                                  |
| spacv         | Python   | [GitHub](https://github.com/SamComber/spacv)                                                                                                                                           |
