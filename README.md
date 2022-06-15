# mlr3spatiotempcv

Package website: [release](https://mlr3spatiotempcv.mlr-org.com/) \| [dev](https://mlr3spatiotempcv.mlr-org.com/dev/)

Spatiotemporal resampling methods for mlr3.

<!-- badges: start -->

[![tic](https://github.com/mlr-org/mlr3spatiotempcv/workflows/tic/badge.svg?branch=main)](https://github.com/mlr-org/mlr3spatiotempcv/actions) [![CRAN Status](https://www.r-pkg.org/badges/version-ago/mlr3spatiotempcv)](https://cran.r-project.org/package=mlr3spatiotempcv) [![Coverage status](https://codecov.io/gh/mlr-org/mlr3spatiotempcv/branch/main/graph/badge.svg)](https://codecov.io/github/mlr-org/mlr3spatiotempcv?branch=main) [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html) [![CodeFactor](https://www.codefactor.io/repository/github/mlr-org/mlr3spatiotempcv/badge)](https://www.codefactor.io/repository/github/mlr-org/mlr3spatiotempcv)

<!-- badges: end -->

This package extends the [mlr3](https://github.com/mlr-org/mlr3) package framework with spatiotemporal resampling and visualization methods.

If you prefer the [tidymodels](https://www.tidymodels.org/) ecosystem, have a look at the [{spatialsample}](https://spatialsample.tidymodels.org/index.html) package for spatial sampling methods.

## Installation

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

## Get Started

See the ["Get Started"](https://mlr3spatiotempcv.mlr-org.com/articles/mlr3spatiotempcv.html) vignette for a quick introduction.

For more detailed information including an usage example see the ["Spatiotemporal Analysis"](https://mlr3book.mlr-org.com/08-special-spatiotemp.html) chapter in the mlr3book.

Article ["Spatiotemporal Visualization"](https://mlr3spatiotempcv.mlr-org.com/articles/spatiotemp-viz.html) shows how 3D subplots grids can be created.

## Citation

To cite the package in publications, use the output of `citation("mlr3spatiotempcv")`.

## Resources

- [Recorded talk about mlr3spatiotempcv and mlr3spatial at OpenDataScience Europe Conference 2021 in Wageningen, NL](https://av.tib.eu/media/55271)
- [List of scientific articles related to spatiotemporal modeling and/or spatial partitioning](https://pat-s.notion.site/Spatial-autocorrelation-in-modeling-b62e1bc904b546b9a489b171913a3551)
# Other spatiotemporal resampling packages

This list does not claim to be comprehensive.

(Disclaimer: Because CRAN does not like DOI URLs in their automated checks, direct linking to scientific articles is not possible...)

| Name          | Language | Resources                                                                                                                                                                              |
| ------------- | -------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| blockCV       | R        | [CRAN](https://cran.r-project.org/package=blockCV)                              |
| CAST          | R        | [Paper](https://www.sciencedirect.com/science/article/pii/S1364815217310976), [CRAN](https://cran.r-project.org/package=CAST) |
| ENMeval       | R        | [CRAN](https://cran.r-project.org/package=ENMeval)                              |
| spatialsample | R        | [CRAN](https://cran.r-project.org/package=spatialsample)                                                                                                               |
| sperrorest    | R        | [CRAN](https://cran.r-project.org/package=sperrorest)                                                          |
| Pyspatialml   | Python   | [GitHub](https://github.com/stevenpawley/Pyspatialml)                                                                                                                                  |
| spacv         | Python   | [GitHub](https://github.com/SamComber/spacv)                                                                                                                                           |
| Museo Toolbox         | Python   | [Paper](https://joss.theoj.org/papers/10.21105/joss.01978), [GitHub](https://github.com/nkarasiak/MuseoToolBox)                                                                                                                                           |

# FAQ

<details>
  <summary>Which resampling method should I use?</summary>
  <br>
    There is no single-best resampling method. It depends on your dataset characteristics and what your model should is about to predict on.
    The resampling scheme should reflect the final purpose of the model - this concept is called "target-oriented" resampling.
    For example, if the model was trained on multiple forest plots and its purpose is to predict something on unknown forest stands, the resampling structure should reflect this.
</details>

<details>
  <summary>Are there more resampling methods than the one {mlr3spatiotempcv} offers?</summary>
  <br>
    {mlr3spatiotempcv} aims to offer all resampling methods that exist in R.
    Though this does not mean that it covers all resampling methods.
    If there are some that you are missing, feel free to open an issue.
</details>

<details>
  <summary>How can I use the "blocking" concept of the old {mlr}?</summary>
  <br>
    This concept is now supported via the "column roles" concept available in {mlr3} [Task](https://mlr3.mlr-org.com/reference/Task.html) objects.
    See [this documentation](https://mlr3.mlr-org.com/reference/Resampling.html#grouping-blocking) for more information.
</details>

<details>
  <summary>For the methods that offer buffering, how can an appropriate value be chosen?</summary>
  <br>
  There is no easy answer to this question. Buffering train and test sets reduces the similarity between both.
  The degree of this reduction depends on the dataset itself and there is no general approach how to choosen an appropriate buffer size.
  Some studies used the distance at which the autocorrelation levels off.
  This buffer distance often removes quite a lot of observations and needs to be calculated first.
</details>
